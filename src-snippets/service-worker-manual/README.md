# Simple offline-available application using a ServiceWorker

This snippet contains a simple GHCJS application that listens to messages from
the Service Worker that gets registered on that page. 

*Prerequisites*: nix

```
nix-build -A ghcjs.service-listener
cd result/bin/service-listener.jsexe/
php -S localhost:8000
# OR python -m SimpleHTTPServer (Python 2.7)
# OR python -m http.server      (Python 3.x)
# OR any other web server
```

When you first access the site, you'll see the text "Reflex script running!" -
which means the Reflex application has been loaded. After a while, you should
also see the text "Ready for offline", which means that the ServiceWorker has
been loaded, and that if you stop the server and reload the page, you should
still see the "Reflex script running!" text.

If you don't see the "Ready for offline" text, check that you haven't opened the
HTML file itself (ServiceWorkers don't work for file:// or other protocols), or
that your browser supports ServiceWorkers - if it doesn't, you shuold at least
see some errors in the browser console.
