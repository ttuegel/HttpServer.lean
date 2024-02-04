# HttpServer.lean

This is a prototype HTTP server written in Lean.
Actually, it probably doesn't deserve to be called an HTTP server.
It makes no attempt to follow the specification, but you can point curl at it.
It listens at `localhost:3000` for incoming connections,
receives HTTP requests and produces responses.
