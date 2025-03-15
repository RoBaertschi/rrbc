pub mod unique_id;

pub trait ResultOk<T, E> {
    fn ok(self) -> Result<Option<T>, E>;
}

impl<T, E> ResultOk<T, E> for Option<Result<T, E>> {
    /// This flips a Option<Result<T, E>> to a Result<Option<T>, E>
    fn ok(self) -> Result<Option<T>, E> {
        self.map_or(Ok(None), |v| v.map(Some))
    }
}

pub trait ResultOkMap<T> {
    /// This maps a function that returns a result to a Result<Option<U>, E>
    fn ok_map<F, E, U>(self, f: F) -> Result<Option<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>;
}

impl<T> ResultOkMap<T> for Option<T> {
    fn ok_map<F, E, U>(self, f: F) -> Result<Option<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        self.map(f).ok()
    }
}

pub fn round_away_from_zero(step: i64, num: i64) -> i64 {
    if num > 0 {
        num + (step - (num % step))
    } else {
        num - (step - (num % step))
    }
}
