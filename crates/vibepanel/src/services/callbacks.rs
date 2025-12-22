//! Generic callback registry for service snapshot updates.
//!
//! This module provides `Callbacks<T>`, a reusable helper for the common
//! snapshot+callback pattern used across most services in the bar.
//!
//! ## Usage
//!
//! ```rust,ignore
//! pub struct MyService {
//!     snapshot: RefCell<MySnapshot>,
//!     callbacks: Callbacks<MySnapshot>,
//! }
//!
//! impl MyService {
//!     pub fn connect<F>(&self, callback: F)
//!     where
//!         F: Fn(&MySnapshot) + 'static,
//!     {
//!         self.callbacks.register(callback);
//!         // Immediately invoke with current snapshot
//!         self.callbacks.notify(&self.snapshot.borrow());
//!     }
//!
//!     fn on_state_change(&self) {
//!         self.callbacks.notify(&self.snapshot.borrow());
//!     }
//! }
//! ```

use std::cell::RefCell;
use std::rc::Rc;

/// Type alias for the callback storage to reduce complexity.
type CallbackList<T> = Vec<Rc<dyn Fn(&T)>>;

/// A registry of callbacks that receive snapshot updates.
///
/// This is the standard pattern used by services to notify widgets of state changes.
/// Callbacks are stored as `Rc<dyn Fn(&T)>` to allow cloning for async notification.
pub struct Callbacks<T> {
    inner: RefCell<CallbackList<T>>,
}

impl<T> Callbacks<T> {
    /// Create a new empty callback registry.
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Vec::new()),
        }
    }

    /// Register a callback to be invoked on snapshot updates.
    ///
    /// The callback is wrapped in `Rc` for efficient cloning during notification.
    pub fn register<F>(&self, callback: F)
    where
        F: Fn(&T) + 'static,
    {
        self.inner.borrow_mut().push(Rc::new(callback));
    }

    /// Notify all registered callbacks with the given snapshot.
    ///
    /// Callbacks are cloned before iteration to avoid holding the borrow
    /// during invocation, which prevents panics if callbacks re-enter the service.
    pub fn notify(&self, snapshot: &T) {
        let callbacks: Vec<_> = self.inner.borrow().iter().cloned().collect();
        for cb in callbacks {
            cb(snapshot);
        }
    }

    /// Returns true if no callbacks are registered.
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.inner.borrow().is_empty()
    }

    /// Returns the number of registered callbacks.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.inner.borrow().len()
    }
}

impl<T> Default for Callbacks<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[test]
    fn test_callbacks_register_and_notify() {
        let callbacks: Callbacks<i32> = Callbacks::new();
        let counter = Rc::new(Cell::new(0));

        let counter_clone = counter.clone();
        callbacks.register(move |value| {
            counter_clone.set(counter_clone.get() + *value);
        });

        callbacks.notify(&5);
        assert_eq!(counter.get(), 5);

        callbacks.notify(&3);
        assert_eq!(counter.get(), 8);
    }

    #[test]
    fn test_callbacks_multiple_listeners() {
        let callbacks: Callbacks<String> = Callbacks::new();
        let results = Rc::new(RefCell::new(Vec::new()));

        let results_clone = results.clone();
        callbacks.register(move |s| {
            results_clone.borrow_mut().push(format!("A:{}", s));
        });

        let results_clone = results.clone();
        callbacks.register(move |s| {
            results_clone.borrow_mut().push(format!("B:{}", s));
        });

        callbacks.notify(&"test".to_string());

        let collected: Vec<_> = results.borrow().clone();
        assert_eq!(collected, vec!["A:test", "B:test"]);
    }

    #[test]
    fn test_callbacks_empty() {
        let callbacks: Callbacks<()> = Callbacks::new();
        assert!(callbacks.is_empty());
        assert_eq!(callbacks.len(), 0);

        callbacks.register(|_| {});
        assert!(!callbacks.is_empty());
        assert_eq!(callbacks.len(), 1);
    }
}
