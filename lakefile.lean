import Lake
open Lake DSL

require socket from git "https://github.com/hargoniX/socket.lean" @ "main"
require Http from git "https://github.com/JamesGallicchio/http" @ "main"

package «HttpServer» where
  -- add package configuration options here

@[default_target]
lean_exe «httpserver» where
  root := `HttpServer
