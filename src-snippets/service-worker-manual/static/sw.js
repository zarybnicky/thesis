var CACHE_NAME = 'dependencies-cache';
var REQUIRED_FILES = ['index.html', 'all.js'];

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => cache.addAll(REQUIRED_FILES))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    self.clients.claim()
      .then(() => self.clients.matchAll())
      .then(clients => clients.forEach(
        client => client.postMessage("Ready for offline")))
  );
});

self.addEventListener('fetch', (event) => {
  var requestUrl = new URL(event.request.url);
  if (requestUrl.pathname === '/') {
    event.respondWith(caches.match('index.html'));
    return;
  }
  event.respondWith(caches.match(event.request).then(
    response => response || fetch(event.request)
  ));
});
