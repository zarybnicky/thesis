var FILE_CACHE_NAME = 'file-cache';
var DATA_CACHE_NAME = 'data-cache';
var REQUIRED_FILES = ['index.html', 'all.js'];

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(FILE_CACHE_NAME)
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
  event.respondWith(caches.match(event.request).then((response) => {
    if (response) {
      return response;
    }
    if (event.request.url.indexOf('https://hacker-news.firebaseio.com/') === 0) {
      return fetch(event.request).then(response => caches.open(DATA_CACHE_NAME).then((cache) => {
        cache.put(event.request.url, response.clone());
        return response;
      }));
    }
    return fetch(event.request);
  }));
});
