use std::sync::atomic::AtomicU64;

pub fn temp_variable_name() -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "tmp.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_label_name(action: &str) -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "tmp.{action}{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_c_variable_name(variable_name: &str) -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "{variable_name}.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_c_label_name(label_name: &str, function_name: &str) -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "{function_name}.{label_name}.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_loop_label(loop_type: &str) -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "loop_label.{loop_type}.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_switch_label() -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "switch_label.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}

pub fn temp_label(input: &str) -> String {
    static ID: AtomicU64 = AtomicU64::new(0);
    format!(
        "tmp.{input}.{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    )
}
