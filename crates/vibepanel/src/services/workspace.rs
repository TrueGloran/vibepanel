//! WorkspaceService - shared, event-driven workspace state via CompositorManager.
//!
//! This provides a GTK-friendly API for workspace state:
//! - Uses the shared CompositorManager singleton
//! - Provides snapshot-based state access
//! - Supports callback registration for reactive updates

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use tracing::debug;

use super::callbacks::Callbacks;
use super::compositor::{CompositorManager, WorkspaceMeta, WorkspaceSnapshot};

/// Enriched workspace object for widget consumption.
///
/// Combines static metadata with dynamic state for convenient widget rendering.
#[derive(Debug, Clone)]
pub struct Workspace {
    /// Unique workspace ID.
    pub id: i32,
    /// Display name for the workspace.
    pub name: String,
    /// Whether this is the currently active workspace.
    pub active: bool,
    /// Whether this workspace has windows.
    pub occupied: bool,
    /// Whether this workspace is marked urgent.
    pub urgent: bool,
    /// Number of windows on this workspace (if available from backend).
    pub window_count: Option<u32>,
    /// Output/monitor this workspace belongs to.
    /// - For Niri: always set (workspaces are per-monitor).
    /// - For MangoWC/Hyprland: always None (workspaces are global).
    #[allow(dead_code)] // Part of public API for future use
    pub output: Option<String>,
}

impl Workspace {
    /// Create a workspace from metadata using global state.
    fn from_meta(meta: &WorkspaceMeta, snapshot: &WorkspaceSnapshot) -> Self {
        Self {
            id: meta.id,
            name: meta.name.clone(),
            active: snapshot.active_workspace == Some(meta.id),
            occupied: snapshot.occupied_workspaces.contains(&meta.id),
            urgent: snapshot.urgent_workspaces.contains(&meta.id),
            window_count: snapshot.window_counts.get(&meta.id).copied(),
            output: meta.output.clone(),
        }
    }

    /// Create a workspace from metadata using per-output state.
    ///
    /// This uses the per-output window counts/occupied state instead of global,
    /// which is needed for multi-monitor setups where each bar should show
    /// the correct window count for its own output.
    fn from_meta_per_output(
        meta: &WorkspaceMeta,
        snapshot: &WorkspaceSnapshot,
        output: &str,
    ) -> Self {
        let per_output = snapshot.per_output.get(output);

        // Use per-output state if available, otherwise fall back to global
        let (active, occupied, window_count) = if let Some(state) = per_output {
            (
                state.active_workspace == Some(meta.id),
                state.occupied_workspaces.contains(&meta.id),
                state.window_counts.get(&meta.id).copied(),
            )
        } else {
            (
                snapshot.active_workspace == Some(meta.id),
                snapshot.occupied_workspaces.contains(&meta.id),
                snapshot.window_counts.get(&meta.id).copied(),
            )
        };

        Self {
            id: meta.id,
            name: meta.name.clone(),
            active,
            occupied,
            urgent: snapshot.urgent_workspaces.contains(&meta.id),
            window_count,
            output: meta.output.clone(),
        }
    }
}

/// Per-output workspace state for widget consumption.
///
/// Contains the workspace state specific to a single output/monitor,
/// with window counts and active state tailored to that output.
#[derive(Debug, Clone)]
pub struct PerOutputWorkspaces {
    /// Currently active workspace ID on this output.
    pub active_workspace: Option<i32>,
    /// Workspaces relevant to this output with per-output state.
    /// For MangoWC: all workspaces with per-output window counts.
    /// For Niri: only workspaces that belong to this output.
    pub workspaces: Vec<Workspace>,
}

/// Snapshot of workspace service state for callbacks.
///
/// This is a GTK-friendly view of the workspace state.
#[derive(Debug, Clone)]
pub struct WorkspaceServiceSnapshot {
    /// Currently active workspace ID.
    pub active_workspace: Option<i32>,
    /// Set of occupied workspace IDs.
    #[allow(dead_code)] // Part of public API for future use
    pub occupied_workspaces: HashSet<i32>,
    /// Window count per workspace (workspace_id -> count).
    #[allow(dead_code)] // Part of public API for future use
    pub window_counts: HashMap<i32, u32>,
    /// All workspaces with their current state.
    pub workspaces: Vec<Workspace>,
    /// Per-output workspace state for multi-monitor setups.
    /// Key is the output/monitor connector name (e.g., "eDP-1", "DP-1").
    pub per_output: HashMap<String, PerOutputWorkspaces>,
}

/// Shared, process-wide workspace service.
///
/// Provides reactive workspace state with GTK main loop integration.
/// Widgets should call `connect()` to receive updates when state changes.
pub struct WorkspaceService {
    /// Reference to the compositor manager.
    manager: Rc<CompositorManager>,
    /// Current workspace snapshot.
    snapshot: RefCell<WorkspaceSnapshot>,
    /// Static workspace metadata.
    workspaces: RefCell<Vec<WorkspaceMeta>>,
    /// Registered callbacks.
    callbacks: Callbacks<WorkspaceServiceSnapshot>,
    /// Whether the service has received at least one update.
    ready: RefCell<bool>,
}

impl WorkspaceService {
    fn new() -> Rc<Self> {
        // Get the shared compositor manager
        let manager = CompositorManager::global();

        // Get initial state from manager
        let initial_snapshot = manager.get_workspace_snapshot();
        let workspaces = manager.list_workspaces();

        let service = Rc::new(Self {
            manager,
            snapshot: RefCell::new(initial_snapshot),
            workspaces: RefCell::new(workspaces),
            callbacks: Callbacks::new(),
            ready: RefCell::new(true), // Ready immediately since manager handles startup
        });

        // Register with compositor manager
        Self::register_with_manager(&service);

        debug!("WorkspaceService initialized (using CompositorManager)");
        service
    }

    /// Get the global WorkspaceService singleton.
    pub fn global() -> Rc<Self> {
        thread_local! {
            static INSTANCE: Rc<WorkspaceService> = WorkspaceService::new();
        }

        INSTANCE.with(|s| s.clone())
    }

    /// Register a callback to be invoked when workspace state changes.
    /// The callback is always executed on the GLib main loop.
    pub fn connect<F>(&self, callback: F)
    where
        F: Fn(&WorkspaceServiceSnapshot) + 'static,
    {
        self.callbacks.register(callback);

        // Immediately send current state so widgets can render.
        if *self.ready.borrow() {
            let snapshot = self.build_snapshot();
            self.callbacks.notify(&snapshot);
        }
    }

    /// Request the compositor to switch to a workspace.
    pub fn switch_workspace(&self, workspace_id: i32) {
        self.manager.switch_workspace(workspace_id);
    }

    fn handle_update(&self, snapshot: WorkspaceSnapshot) {
        // Update stored snapshot
        *self.snapshot.borrow_mut() = snapshot;
        *self.ready.borrow_mut() = true;

        // Also refresh workspace list (in case of dynamic workspaces)
        *self.workspaces.borrow_mut() = self.manager.list_workspaces();

        // Build enriched snapshot and notify callbacks.
        let service_snapshot = self.build_snapshot();
        self.callbacks.notify(&service_snapshot);
    }

    fn register_with_manager(this: &Rc<Self>) {
        // Create callback that handles updates
        let service_weak = Rc::downgrade(this);
        this.manager.register_workspace_callback(move |snapshot| {
            if let Some(service) = service_weak.upgrade() {
                service.handle_update(snapshot.clone());
            }
        });
    }

    fn build_snapshot(&self) -> WorkspaceServiceSnapshot {
        let snapshot = self.snapshot.borrow();
        let workspaces_meta = self.workspaces.borrow();

        // Build global workspace list
        let workspaces: Vec<Workspace> = workspaces_meta
            .iter()
            .map(|meta| Workspace::from_meta(meta, &snapshot))
            .collect();

        // Build per-output workspace lists
        let mut per_output = HashMap::new();

        for (output_name, output_state) in &snapshot.per_output {
            // Filter workspaces for this output:
            // - For Niri: only include workspaces that belong to this output
            // - For MangoWC: include all workspaces (tags are global) but with per-output state
            let output_workspaces: Vec<Workspace> = workspaces_meta
                .iter()
                .filter(|meta| {
                    // Include if workspace is global (output is None) or belongs to this output
                    meta.output.is_none() || meta.output.as_ref() == Some(output_name)
                })
                .map(|meta| Workspace::from_meta_per_output(meta, &snapshot, output_name))
                .collect();

            per_output.insert(
                output_name.clone(),
                PerOutputWorkspaces {
                    active_workspace: output_state.active_workspace,
                    workspaces: output_workspaces,
                },
            );
        }

        WorkspaceServiceSnapshot {
            active_workspace: snapshot.active_workspace,
            occupied_workspaces: snapshot.occupied_workspaces.clone(),
            window_counts: snapshot.window_counts.clone(),
            workspaces,
            per_output,
        }
    }
}

impl Drop for WorkspaceService {
    fn drop(&mut self) {
        debug!("WorkspaceService dropped");
    }
}
