# Integration tests for RealWorld client

To run integration tests for the RealWorld client, I have adapted tests from
https://github.com/anishkny/realworld-e2e-test/. To run them, start the
application either via Warp (`ghcid-here`), or run a static file server on port
3000, and then run `docker-compose up` in this directory.

```
cd src/tapaw-realworld-client/
ghcid-here
cd test/integration
docker-compose up
```
