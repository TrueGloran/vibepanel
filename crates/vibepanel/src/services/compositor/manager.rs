//! CompositorManager - shared backend singleton for workspace and window title services.
//!
//! This module provides a centralized compositor backend instance that can be shared
//! across multiple services (WorkspaceService, WindowTitleService). This avoids the
//! problem of creating multiple backend instances that would duplicate IPC connections
//! and monitoring threads.
//!
//! # Architecture
//!
//! The CompositorManager receives updates from the backend thread via glib::idle_add_once(),
//! which schedules callbacks directly on the GTK main loop without polling. It maintains:
//! - A single backend instance
//! - Registered callbacks for workspace and window updates
//!
//! # Usage
//!
//! ```rust,ignore
//! let manager = CompositorManager::global();
//!
//! // Register for workspace updates
//! manager.register_workspace_callback(Arc::new(|snapshot| {
//!     // Handle workspace state change
//! }));
//! ```

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use gtk4::glib;
use tracing::{debug, info};
use vibepanel_core::config::WorkspaceConfig;

use super::{
    BackendKind, CompositorBackend, WindowCallback, WindowInfo, WindowOpenedCallback,
    WorkspaceCallback, WorkspaceMeta, WorkspaceSnapshot, factory,
};

/// Local callback type (not Send+Sync, runs on GTK main loop).
type LocalWorkspaceCallback = Rc<dyn Fn(&WorkspaceSnapshot)>;
type LocalWindowCallback = Rc<dyn Fn(&WindowInfo)>;
type LocalWindowOpenedCallback = Rc<dyn Fn(&WindowInfo)>;

// Thread-local singleton storage for CompositorManager
thread_local! {
    static COMPOSITOR_MANAGER: RefCell<Option<Rc<CompositorManager>>> = const { RefCell::new(None) };
}

/// Shared compositor manager that owns the backend and dispatches to multiple listeners.
///
/// This is a GTK main-thread singleton that wraps the compositor backend and
/// multiplexes its callbacks to multiple registered listeners.
pub struct CompositorManager {
    /// The compositor backend (owned).
    backend: RefCell<Option<Box<dyn CompositorBackend>>>,
    /// Registered workspace callbacks (local, not Send+Sync).
    workspace_callbacks: RefCell<Vec<LocalWorkspaceCallback>>,
    /// Registered window callbacks (local, not Send+Sync).
    window_callbacks: RefCell<Vec<LocalWindowCallback>>,
    /// Registered window-opened callbacks (local, not Send+Sync).
    window_opened_callbacks: RefCell<Vec<LocalWindowOpenedCallback>>,
    /// Last known workspace snapshot (for new listeners).
    last_workspace_snapshot: RefCell<Option<WorkspaceSnapshot>>,
    /// Last known window info (for new listeners).
    last_window_info: RefCell<Option<WindowInfo>>,
    /// Whether the backend has been started.
    started: RefCell<bool>,
}

impl CompositorManager {
    /// Create a new CompositorManager with the given workspace configuration.
    fn new(workspace_config: &WorkspaceConfig) -> Rc<Self> {
        let manager = Rc::new(Self {
            backend: RefCell::new(None),
            workspace_callbacks: RefCell::new(Vec::new()),
            window_callbacks: RefCell::new(Vec::new()),
            window_opened_callbacks: RefCell::new(Vec::new()),
            last_workspace_snapshot: RefCell::new(None),
            last_window_info: RefCell::new(None),
            started: RefCell::new(false),
        });

        // Initialize backend with config
        Self::init_backend(&manager, workspace_config);

        manager
    }

    /// Initialize the global CompositorManager singleton with workspace configuration.
    ///
    /// This must be called once from the GTK main thread before any calls to `global()`.
    /// Typically called during application startup after ConfigManager is initialized.
    pub fn init_global(workspace_config: &WorkspaceConfig) {
        COMPOSITOR_MANAGER.with(|cell| {
            let mut opt = cell.borrow_mut();
            if opt.is_some() {
                debug!("CompositorManager already initialized, skipping re-init");
                return;
            }
            *opt = Some(CompositorManager::new(workspace_config));
        });
    }

    /// Get the global CompositorManager singleton.
    ///
    /// This must be called from the GTK main thread.
    /// Panics if `init_global()` has not been called.
    pub fn global() -> Rc<Self> {
        COMPOSITOR_MANAGER.with(|cell| {
            cell.borrow()
                .clone()
                .expect("CompositorManager::global() called before init_global()")
        })
    }

    /// Register a callback for workspace state changes.
    ///
    /// The callback will be immediately invoked with the current state if available.
    pub fn register_workspace_callback<F>(&self, callback: F)
    where
        F: Fn(&WorkspaceSnapshot) + 'static,
    {
        let callback = Rc::new(callback);
        self.workspace_callbacks.borrow_mut().push(callback.clone());

        // Immediately send current state if available
        if let Some(ref snapshot) = *self.last_workspace_snapshot.borrow() {
            callback(snapshot);
        }
    }

    /// Register a callback for window focus changes.
    ///
    /// The callback will be immediately invoked with the current state if available.
    pub fn register_window_callback<F>(&self, callback: F)
    where
        F: Fn(&WindowInfo) + 'static,
    {
        let callback = Rc::new(callback);
        self.window_callbacks.borrow_mut().push(callback.clone());

        // Immediately send current state if available
        if let Some(ref info) = *self.last_window_info.borrow() {
            callback(info);
        }
    }

    /// Get the list of workspaces from the backend.
    pub fn list_workspaces(&self) -> Vec<WorkspaceMeta> {
        if let Some(ref backend) = *self.backend.borrow() {
            backend.list_workspaces()
        } else {
            Vec::new()
        }
    }

    /// Get the current workspace snapshot.
    pub fn get_workspace_snapshot(&self) -> WorkspaceSnapshot {
        if let Some(ref snapshot) = *self.last_workspace_snapshot.borrow() {
            snapshot.clone()
        } else if let Some(ref backend) = *self.backend.borrow() {
            backend.get_workspace_snapshot()
        } else {
            WorkspaceSnapshot::default()
        }
    }

    /// Get the current focused window info.
    pub fn get_focused_window(&self) -> Option<WindowInfo> {
        self.last_window_info.borrow().clone()
    }

    /// Switch to a workspace.
    pub fn switch_workspace(&self, workspace_id: i32) {
        if let Some(ref backend) = *self.backend.borrow() {
            backend.switch_workspace(workspace_id);
        }
    }

    /// Request the compositor to quit/exit.
    ///
    /// Used for logout functionality. Sends a quit command to the compositor
    /// via its native IPC.
    pub fn quit_compositor(&self) {
        if let Some(ref backend) = *self.backend.borrow() {
            backend.quit_compositor();
        }
    }

    /// Get the backend name (e.g., "Hyprland", "Niri", "MangoWC").
    pub fn backend_name(&self) -> &'static str {
        if let Some(ref backend) = *self.backend.borrow() {
            backend.name()
        } else {
            "unknown"
        }
    }

    /// Register a callback for window-opened events.
    ///
    /// This is only supported by some backends (currently Hyprland). The callback
    /// will be invoked on the GTK main thread when a new window is created.
    ///
    /// Used by Quick Settings to detect when external windows spawn (e.g., terminal
    /// from update card, VPN password dialog), which should trigger panel close.
    ///
    /// Note: Unlike workspace/window callbacks, this does not replay state - it only
    /// fires for new window creations after registration.
    pub fn register_window_opened_callback<F>(&self, callback: F)
    where
        F: Fn(&WindowInfo) + 'static,
    {
        let callback = Rc::new(callback);
        self.window_opened_callbacks.borrow_mut().push(callback);

        // Set up the backend callback if this is the first registration
        // and the backend supports it
        self.setup_window_opened_backend_callback();
    }

    /// Set up the backend window-opened callback if needed.
    ///
    /// This is called when a window-opened callback is registered. It creates
    /// a thread-safe wrapper that dispatches events to the GTK main loop.
    fn setup_window_opened_backend_callback(&self) {
        // Only set up if we have callbacks registered
        if self.window_opened_callbacks.borrow().is_empty() {
            return;
        }

        let Some(ref backend) = *self.backend.borrow() else {
            return;
        };

        // Create thread-safe callback that dispatches to main loop
        let on_window_opened: WindowOpenedCallback = Arc::new(move |window_info| {
            let window_info = window_info.clone();
            glib::idle_add_once(move || {
                CompositorManager::global().handle_window_opened(window_info);
            });
        });

        backend.set_window_opened_callback(Some(on_window_opened));
    }

    /// Handle a window-opened event from the backend.
    /// Called via glib::idle_add_once from the backend thread.
    fn handle_window_opened(&self, window_info: WindowInfo) {
        // Dispatch to all registered callbacks
        for cb in self.window_opened_callbacks.borrow().iter() {
            cb(&window_info);
        }
    }

    /// Handle a workspace update from the backend.
    /// Called via glib::idle_add_once from the backend thread.
    pub(crate) fn handle_workspace_update(&self, snapshot: WorkspaceSnapshot) {
        // Store for new listeners
        *self.last_workspace_snapshot.borrow_mut() = Some(snapshot.clone());

        // Dispatch to all registered callbacks
        for cb in self.workspace_callbacks.borrow().iter() {
            cb(&snapshot);
        }
    }

    /// Handle a window update from the backend.
    /// Called via glib::idle_add_once from the backend thread.
    pub(crate) fn handle_window_update(&self, window_info: WindowInfo) {
        // Store for new listeners
        *self.last_window_info.borrow_mut() = Some(window_info.clone());

        // Dispatch to all registered callbacks
        for cb in self.window_callbacks.borrow().iter() {
            cb(&window_info);
        }
    }

    /// Initialize the backend.
    fn init_backend(this: &Rc<Self>, workspace_config: &WorkspaceConfig) {
        // Parse backend kind from config
        let backend_kind = BackendKind::from_str(&workspace_config.backend);

        // Backends no longer filter by outputs - that's now handled at the widget level
        let backend = factory::create_backend(backend_kind, None);

        info!(
            "CompositorManager using backend: {} (config: {})",
            backend.name(),
            workspace_config.backend,
        );

        // Create thread-safe callbacks that use idle_add_once to schedule on main loop
        let on_workspace_update: WorkspaceCallback = Arc::new(move |snapshot| {
            glib::idle_add_once(move || {
                CompositorManager::global().handle_workspace_update(snapshot);
            });
        });

        let on_window_update: WindowCallback = Arc::new(move |window_info| {
            glib::idle_add_once(move || {
                CompositorManager::global().handle_window_update(window_info);
            });
        });

        // Start the backend first (which fetches initial state internally)
        backend.start(on_workspace_update, on_window_update);

        // Now store initial state - backend has fetched it during start()
        *this.last_workspace_snapshot.borrow_mut() = Some(backend.get_workspace_snapshot());
        *this.last_window_info.borrow_mut() = backend.get_focused_window();

        // Store backend
        *this.backend.borrow_mut() = Some(backend);
        *this.started.borrow_mut() = true;

        debug!("CompositorManager initialized");
    }
}

impl Drop for CompositorManager {
    fn drop(&mut self) {
        if let Some(ref backend) = *self.backend.borrow() {
            backend.stop();
        }
        debug!("CompositorManager dropped");
    }
}
