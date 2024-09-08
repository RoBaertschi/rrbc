use std::sync::atomic::AtomicU32;

pub fn temp() -> String {
    static ID: AtomicU32 = AtomicU32::new(0);
    format!(
        "tmp.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}
