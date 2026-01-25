const CACHE_NAME = 'digitrek-v1';

const PRECACHE_ASSETS = [
    './',
    './index.html',
    './manifest.json',
    './favicon.svg',
    './digitrek.js',
    './digitrek_bg.wasm',
    './restart_audio_context.js',
    './assets/explosion.ogg',
    './assets/pop.ogg',
    './assets/projectile_launch.ogg',
    './assets/unmatched_keypress.ogg',
];

self.addEventListener('install', event => {
    event.waitUntil(
        caches.open(CACHE_NAME).then(cache => {
            return cache.addAll(PRECACHE_ASSETS);
        })
    );
    self.skipWaiting();
});

self.addEventListener('activate', event => {
    event.waitUntil(
        caches.keys().then(keys =>
            Promise.all(
                keys
                .filter(key => key !== CACHE_NAME)
                .map(key => caches.delete(key))
            )
        )
    );
    self.clients.claim();
});

self.addEventListener('fetch', event => {
    const url = new URL(event.request.url);

    if (url.origin !== self.location.origin) {
        return;
    }

    event.respondWith(
        caches.match(event.request).then(cached => {
            if (cached) {
                return cached;
            }

            return fetch(event.request).then(response => {
                if (
                    response &&
                    response.status === 200 &&
                    event.request.method === 'GET'
                ) {
                    const responseClone = response.clone();
                    caches.open(CACHE_NAME).then(cache => {
                        cache.put(event.request, responseClone);
                    });
                }

                return response;
            });
        })
    );
});

