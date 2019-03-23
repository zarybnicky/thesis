var FILE_CACHE_NAME = 'dependencies-cache';
var DATA_CACHE_NAME = 'dependencies-cache';
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


self.addEventListener('fetch', function(e) {
  if (e.request.url.indexOf('https://hacker-news.firebaseio.com/') === 0) {
    e.respondWith(fetch(e.request).then(function(response) {
      return caches.open(DATA_CACHE_NAME).then(function(cache) {
        cache.put(e.request.url, response.clone());
        return response;
      });
    }));
  } else {
    e.respondWith(
      caches.match(e.request).then(function(response) {
        return response || fetch(e.request);
      })
    );
  }
});
