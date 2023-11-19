use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    time::Duration,
};

use std::time::Instant;

#[derive(Clone, Default)]
pub(crate) struct TimeoutTracker {
    data: Rc<RefCell<HashMap<u64, Instant>>>,
}

// Tokio is configured to use the current_thread runtime, so it is not unsafe to
// make this Send and Sync.
unsafe impl Send for TimeoutTracker {}
unsafe impl Sync for TimeoutTracker {}

impl TimeoutTracker {
    pub(crate) fn new() -> Self {
        Self {
            data: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub(crate) fn add(&self, id: u64) {
        let now = Instant::now();
        self.data.borrow_mut().insert(id, now);
    }

    pub(crate) fn remove_expired(&self, duration: Duration) -> HashSet<u64> {
        let now = Instant::now();
        let mut expired_ids = HashSet::new();

        self.data.borrow_mut().retain(|&id, instant| {
            if *instant <= now - duration {
                expired_ids.insert(id);
                false
            } else {
                true
            }
        });

        expired_ids
    }
}
