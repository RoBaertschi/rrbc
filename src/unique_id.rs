use std::sync::atomic::AtomicU32;

pub fn temp_variable_name() -> String {
    static ID: AtomicU32 = AtomicU32::new(0);
    format!(
        "tmp.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_lable_name(action: &str) -> String {
    static ID: AtomicU32 = AtomicU32::new(0);
    format!(
        "{action}{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}
