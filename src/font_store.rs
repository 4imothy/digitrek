// SPDX-License-Identifier: MIT

#[cfg(not(target_arch = "wasm32"))]
use native as platform;
use std::sync::{
    Mutex,
    atomic::{AtomicBool, Ordering},
};
#[cfg(target_arch = "wasm32")]
use web as platform;

pub const CUSTOM_FONT_NAME: &str = "custom_font";

static SETTINGS_OPEN: AtomicBool = AtomicBool::new(false);
static READY: Mutex<Option<Vec<u8>>> = Mutex::new(None);
static REJECTED: AtomicBool = AtomicBool::new(false);

pub fn set_settings_open(open: bool) {
    SETTINGS_OPEN.store(open, Ordering::Relaxed);
}

pub fn take_pending() -> Option<Vec<u8>> {
    READY.lock().unwrap().take()
}

pub fn take_rejected() -> bool {
    REJECTED.swap(false, Ordering::Relaxed)
}

fn set_pending(bytes: Vec<u8>) {
    *READY.lock().unwrap() = Some(bytes);
}

pub fn accept_drop(bytes: Vec<u8>) {
    if !SETTINGS_OPEN.load(Ordering::Relaxed) {
        return;
    }
    if ttf_parser::Face::parse(&bytes, 0).is_err() {
        REJECTED.store(true, Ordering::Relaxed);
        return;
    }
    platform::store_drop(bytes.clone());
    set_pending(bytes);
}

pub fn restore_default() {
    set_pending(include_bytes!("../assets/font.ttf").to_vec());
    platform::clear_stored();
}

pub fn init() {
    platform::init();
}

#[cfg(target_arch = "wasm32")]
mod web {
    use super::set_pending;
    use wasm_bindgen::{JsCast, JsValue, closure::Closure};
    use web_sys::{Event, IdbDatabase, IdbObjectStore, IdbRequest, IdbTransactionMode};

    const DB: &str = env!("CARGO_PKG_NAME");
    const STORE: &str = "blobs";
    const KEY: &str = super::CUSTOM_FONT_NAME;
    const VER: u32 = 1;

    fn req_result(e: &Event) -> JsValue {
        e.target()
            .unwrap()
            .dyn_into::<IdbRequest>()
            .unwrap()
            .result()
            .unwrap()
    }

    fn open_db(on_success: impl FnOnce(IdbDatabase) + 'static) {
        let window = web_sys::window().unwrap();
        let idb = window.indexed_db().unwrap().unwrap();
        let req = idb.open_with_u32(DB, VER).unwrap();

        let upgrade = Closure::<dyn FnMut(Event)>::new(|e: Event| {
            let db: IdbDatabase = req_result(&e).dyn_into().unwrap();
            db.create_object_store(STORE).ok();
        });
        req.set_onupgradeneeded(Some(upgrade.as_ref().unchecked_ref()));
        upgrade.forget();

        let mut slot = Some(on_success);
        let success = Closure::<dyn FnMut(Event)>::new(move |e: Event| {
            let db: IdbDatabase = req_result(&e).dyn_into().unwrap();
            if let Some(f) = slot.take() {
                f(db);
            }
        });
        req.set_onsuccess(Some(success.as_ref().unchecked_ref()));
        success.forget();
    }

    pub fn init() {
        open_db(|db| {
            let tx = db.transaction_with_str(STORE).unwrap();
            let store = tx.object_store(STORE).unwrap();
            let req = store.get(&JsValue::from_str(KEY)).unwrap();
            let cb = Closure::<dyn FnMut(Event)>::new(move |e: Event| {
                let result = req_result(&e);
                if !result.is_undefined() && !result.is_null() {
                    set_pending(js_sys::Uint8Array::new(&result).to_vec());
                }
            });
            req.set_onsuccess(Some(cb.as_ref().unchecked_ref()));
            cb.forget();
        });
    }

    fn with_store_rw(f: impl FnOnce(&IdbObjectStore) + 'static) {
        open_db(move |db| {
            let tx = db
                .transaction_with_str_and_mode(STORE, IdbTransactionMode::Readwrite)
                .unwrap();
            f(&tx.object_store(STORE).unwrap());
        });
    }

    pub fn store_drop(bytes: Vec<u8>) {
        with_store_rw(move |store| {
            let arr: JsValue = js_sys::Uint8Array::from(bytes.as_slice()).into();
            store.put_with_key(&arr, &JsValue::from_str(KEY)).unwrap();
        });
    }

    pub fn clear_stored() {
        with_store_rw(|store| {
            store.delete(&JsValue::from_str(KEY)).unwrap();
        });
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::set_pending;
    use std::path::PathBuf;

    fn font_path() -> PathBuf {
        let file = format!("{}.ttf", super::CUSTOM_FONT_NAME);
        directories::ProjectDirs::from(crate::APP_QUALIFIER, crate::APP_ORG, env!("CARGO_PKG_NAME"))
            .map(|d| d.data_dir().join(&file))
            .unwrap_or_else(|| PathBuf::from(&file))
    }

    pub fn init() {
        let path = font_path();
        if path.exists()
            && let Ok(bytes) = std::fs::read(&path)
        {
            set_pending(bytes);
        }
    }

    pub fn store_drop(bytes: Vec<u8>) {
        let dest = font_path();
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        std::fs::write(&dest, bytes).ok();
    }

    pub fn clear_stored() {
        std::fs::remove_file(font_path()).ok();
    }
}
