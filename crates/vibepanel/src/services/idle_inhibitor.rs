//! IdleInhibitorService - Prevents system idle/sleep using D-Bus ScreenSaver API.
//!
//! This service uses the `org.freedesktop.ScreenSaver` D-Bus interface on the session bus,
//! which is the standard mechanism for GUI applications to inhibit idle on Wayland.
//! This is supported by hypridle, swayidle, and other idle daemons.
//!
//! ## Usage
//!
//! The service is a singleton that can be toggled on/off. When active,
//! it prevents the system from going idle or suspending.
//!
//! ## CLI Usage
//!
//! The `IdleInhibitorCli` struct provides a standalone idle inhibitor using
//! systemd-logind's D-Bus API. This doesn't require GTK and is suitable for
//! CLI commands like `vibepanel inhibit <command>`.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use gtk4::gio;
use gtk4::glib;
use gtk4::prelude::*;
use tracing::{debug, warn};

use super::callbacks::Callbacks;

/// Canonical snapshot of idle inhibitor state.
#[derive(Debug, Clone)]
pub struct IdleInhibitorSnapshot {
    /// Whether the inhibitor is currently active.
    pub active: bool,
    /// Whether the inhibitor is available.
    pub available: bool,
}

impl IdleInhibitorSnapshot {
    /// Create an initial snapshot.
    fn new() -> Self {
        Self {
            active: false,
            available: true,
        }
    }
}

/// Shared, process-wide idle inhibitor service.
///
/// Uses the `org.freedesktop.ScreenSaver` D-Bus interface to inhibit idle.
/// This is the standard mechanism used by Firefox, Steam, and other GUI apps,
/// and is supported by hypridle, swayidle, and other Wayland idle daemons.
pub struct IdleInhibitorService {
    /// Current snapshot of inhibitor state.
    snapshot: RefCell<IdleInhibitorSnapshot>,
    /// Registered callbacks for state changes.
    callbacks: Callbacks<IdleInhibitorSnapshot>,
    /// D-Bus inhibit cookie (for releasing the inhibitor).
    inhibit_cookie: Cell<u32>,
    /// D-Bus proxy for org.freedesktop.ScreenSaver.
    dbus_proxy: RefCell<Option<gio::DBusProxy>>,
}

impl IdleInhibitorService {
    /// Create a new IdleInhibitorService.
    fn new() -> Rc<Self> {
        let service = Rc::new(Self {
            snapshot: RefCell::new(IdleInhibitorSnapshot::new()),
            callbacks: Callbacks::new(),
            inhibit_cookie: Cell::new(0),
            dbus_proxy: RefCell::new(None),
        });

        // Initialize D-Bus proxy asynchronously
        service.init_dbus_proxy();

        service
    }

    /// Get the global IdleInhibitorService singleton.
    pub fn global() -> Rc<Self> {
        thread_local! {
            static INSTANCE: Rc<IdleInhibitorService> = IdleInhibitorService::new();
        }

        INSTANCE.with(|s| s.clone())
    }

    /// Initialize the D-Bus proxy for org.freedesktop.ScreenSaver.
    fn init_dbus_proxy(&self) {
        // Create proxy synchronously for simplicity - this is fast since it's just
        // setting up the proxy object, not making any D-Bus calls yet.
        match gio::DBusProxy::for_bus_sync(
            gio::BusType::Session,
            gio::DBusProxyFlags::NONE,
            None, // No interface info
            "org.freedesktop.ScreenSaver",
            "/org/freedesktop/ScreenSaver",
            "org.freedesktop.ScreenSaver",
            gio::Cancellable::NONE,
        ) {
            Ok(proxy) => {
                debug!("IdleInhibitorService: D-Bus proxy created for org.freedesktop.ScreenSaver");
                *self.dbus_proxy.borrow_mut() = Some(proxy);
            }
            Err(e) => {
                warn!(
                    "IdleInhibitorService: Failed to create D-Bus proxy: {}. \
                     Idle inhibition will not work. Is an idle daemon (hypridle, swayidle) running?",
                    e
                );
                // Mark as unavailable
                let mut snapshot = self.snapshot.borrow_mut();
                snapshot.available = false;
            }
        }
    }

    /// Register a callback to be invoked whenever the inhibitor state changes.
    pub fn connect<F>(&self, callback: F)
    where
        F: Fn(&IdleInhibitorSnapshot) + 'static,
    {
        self.callbacks.register(callback);

        // Immediately send current snapshot.
        let snapshot = self.snapshot.borrow().clone();
        self.callbacks.notify(&snapshot);
    }

    /// Return the current inhibitor snapshot.
    pub fn snapshot(&self) -> IdleInhibitorSnapshot {
        self.snapshot.borrow().clone()
    }

    /// Whether the inhibitor is currently active.
    #[allow(dead_code)] // API for potential CLI/external use
    pub fn active(&self) -> bool {
        self.snapshot.borrow().active
    }

    /// Whether the inhibitor is available.
    #[allow(dead_code)] // API for potential CLI/external use
    pub fn available(&self) -> bool {
        self.snapshot.borrow().available
    }

    /// Toggle the inhibitor state.
    #[allow(dead_code)] // API for potential CLI/external use
    pub fn toggle(&self) {
        let current = self.snapshot.borrow().active;
        self.set_active(!current);
    }

    /// Set the inhibitor state.
    pub fn set_active(&self, active: bool) {
        let current = self.snapshot.borrow().active;
        if current == active {
            return;
        }

        if active {
            self.enable_inhibitor();
        } else {
            self.disable_inhibitor();
        }
    }

    /// Stop the service and release any inhibitor.
    #[allow(dead_code)] // API for potential CLI/external use
    pub fn stop(&self) {
        if self.snapshot.borrow().active {
            self.disable_inhibitor();
        }
    }

    // Internal - D-Bus ScreenSaver API

    fn enable_inhibitor(&self) {
        let proxy_opt = self.dbus_proxy.borrow();
        let Some(proxy) = proxy_opt.as_ref() else {
            warn!("IdleInhibitorService: Cannot enable - no D-Bus proxy available");
            return;
        };

        // Call org.freedesktop.ScreenSaver.Inhibit(application_name, reason) -> cookie
        let args = ("vibepanel", "User requested idle inhibition").to_variant();

        match proxy.call_sync(
            "Inhibit",
            Some(&args),
            gio::DBusCallFlags::NONE,
            5000, // 5 second timeout
            gio::Cancellable::NONE,
        ) {
            Ok(result) => {
                // Result is (u,) - a tuple containing the cookie
                let cookie: u32 = result.child_get(0);
                if cookie == 0 {
                    warn!("IdleInhibitorService: Inhibit returned cookie=0, may not be working");
                }
                self.inhibit_cookie.set(cookie);
                debug!("IdleInhibitorService: Enabled (cookie={})", cookie);

                let mut snapshot = self.snapshot.borrow_mut();
                snapshot.active = true;
                let snapshot_clone = snapshot.clone();
                drop(snapshot);

                self.callbacks.notify(&snapshot_clone);
            }
            Err(e) => {
                warn!("IdleInhibitorService: Failed to call Inhibit: {}", e);
                // Don't update state since we failed
            }
        }
    }

    fn disable_inhibitor(&self) {
        let cookie = self.inhibit_cookie.get();
        if cookie == 0 {
            // No active inhibitor - but still update state for consistency
            let mut snapshot = self.snapshot.borrow_mut();
            if snapshot.active {
                snapshot.active = false;
                let snapshot_clone = snapshot.clone();
                drop(snapshot);
                self.callbacks.notify(&snapshot_clone);
            }
            return;
        }

        let proxy_opt = self.dbus_proxy.borrow();
        if let Some(proxy) = proxy_opt.as_ref() {
            // Call org.freedesktop.ScreenSaver.UnInhibit(cookie)
            let args = (cookie,).to_variant();

            match proxy.call_sync(
                "UnInhibit",
                Some(&args),
                gio::DBusCallFlags::NONE,
                5000,
                gio::Cancellable::NONE,
            ) {
                Ok(_) => {
                    debug!("IdleInhibitorService: Disabled (cookie={})", cookie);
                }
                Err(e) => {
                    warn!("IdleInhibitorService: Failed to call UnInhibit: {}", e);
                    // Continue anyway to update local state
                }
            }
        }

        self.inhibit_cookie.set(0);

        let mut snapshot = self.snapshot.borrow_mut();
        snapshot.active = false;
        let snapshot_clone = snapshot.clone();
        drop(snapshot);

        self.callbacks.notify(&snapshot_clone);
    }
}

impl Drop for IdleInhibitorService {
    fn drop(&mut self) {
        // Ensure we release the inhibitor when the service is dropped.
        let cookie = self.inhibit_cookie.get();
        if cookie != 0
            && let Some(proxy) = self.dbus_proxy.borrow().as_ref()
        {
            let args = (cookie,).to_variant();
            let _ = proxy.call_sync(
                "UnInhibit",
                Some(&args),
                gio::DBusCallFlags::NONE,
                1000,
                gio::Cancellable::NONE,
            );
        }
    }
}

// CLI interface - uses systemd-logind D-Bus API (no GTK required)

use gtk4::prelude::ToVariant;
use std::os::unix::io::OwnedFd;

/// CLI idle inhibitor using systemd-logind D-Bus API.
///
/// This creates an inhibit lock via org.freedesktop.login1.Manager.Inhibit.
/// The lock is held as long as the returned file descriptor is open.
///
/// Unlike the GTK-based service, this is designed for CLI usage where we
/// want to inhibit idle for the duration of a command (e.g., `vibepanel inhibit <command>`).
pub struct IdleInhibitorCli {
    /// The inhibit lock file descriptor. Dropping this releases the lock.
    _inhibit_fd: Option<OwnedFd>,
}

impl IdleInhibitorCli {
    /// Create a new idle inhibitor lock.
    ///
    /// Returns `None` if the inhibitor could not be acquired.
    pub fn new(reason: &str) -> Option<Self> {
        let fd = Self::acquire_inhibit_lock(reason)?;
        Some(Self {
            _inhibit_fd: Some(fd),
        })
    }

    /// Acquire an inhibit lock from systemd-logind.
    ///
    /// The lock prevents idle and sleep while the returned fd is open.
    fn acquire_inhibit_lock(reason: &str) -> Option<OwnedFd> {
        let connection = gio::bus_get_sync(gio::BusType::System, gio::Cancellable::NONE).ok()?;

        // Call org.freedesktop.login1.Manager.Inhibit
        // Arguments: (what, who, why, mode)
        // - what: colon-separated list of what to inhibit (idle:sleep)
        // - who: application name
        // - why: human-readable reason
        // - mode: "block" (hard block) or "delay" (delay for grace period)
        let args = (
            "idle:sleep", // what
            "vibepanel",  // who
            reason,       // why
            "block",      // mode
        );

        // Use call_with_unix_fd_list_sync to receive the file descriptor
        let result = connection.call_with_unix_fd_list_sync(
            Some("org.freedesktop.login1"),
            "/org/freedesktop/login1",
            "org.freedesktop.login1.Manager",
            "Inhibit",
            Some(&args.to_variant()),
            Some(glib::VariantTy::new("(h)").unwrap()),
            gio::DBusCallFlags::NONE,
            5000,
            gio::UnixFDList::NONE,
            gio::Cancellable::NONE,
        );

        match result {
            Ok((_reply, Some(fd_list))) => {
                // Get the fd from the list (index 0)
                match fd_list.get(0) {
                    Ok(owned_fd) => {
                        debug!(
                            "IdleInhibitorCli: Acquired inhibit lock (fd={:?})",
                            owned_fd
                        );
                        Some(owned_fd)
                    }
                    Err(e) => {
                        warn!("IdleInhibitorCli: Failed to get fd from list: {}", e);
                        None
                    }
                }
            }
            Ok((_reply, None)) => {
                warn!("IdleInhibitorCli: No fd list in reply");
                None
            }
            Err(e) => {
                warn!("IdleInhibitorCli: Failed to acquire inhibit lock: {}", e);
                None
            }
        }
    }
}

impl Drop for IdleInhibitorCli {
    fn drop(&mut self) {
        if self._inhibit_fd.is_some() {
            debug!("IdleInhibitorCli: Released inhibit lock");
        }
    }
}
