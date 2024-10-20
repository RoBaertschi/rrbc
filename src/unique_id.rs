use std::sync::atomic::AtomicU64;

static ID: AtomicU64 = AtomicU64::new(0);

pub fn temp_variable_name() -> String {
    format!(
        "tmp.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_lable_name(action: &str) -> String {
    format!(
        "{action}{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_c_variable_name(variable_name: &str) -> String {
    format!(
        "{variable_name}.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}
