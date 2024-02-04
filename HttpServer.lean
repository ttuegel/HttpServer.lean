import Socket
import Http
import Parser
open Parser

abbrev Request := Http.Request Substring
abbrev Response := Http.Response Substring

-- Receive and parse a request from a connected socket.
-- The given socket should be in the "accepted" state.
def recvRequest (sock : Socket) : IO (Http.Parser.Error ⊕ Request) := do
  let bytes <- Socket.recv sock 4096
  let str := String.fromUTF8Unchecked bytes
  -- Stop parsing when we get to the body of the request.
  -- In other words, leave the request body as an uninterpreted string.
  let stop : Http.Parser Unit := pure ()
  let parsed := Parser.run (Http.Request.parse stop) str
  match parsed with
  | .ok body request => pure (Sum.inr { request with body := body })
  | .error e => pure (Sum.inl e)

def sendResponse (sock : Socket) (response : Response) : IO Unit := do
  let _ <- Socket.send sock response.toString.toUTF8
  pure ()

namespace Response

def ok : Response where
  version := Http.Version.HTTP_1_1
  status := Http.StatusCode.OK
  headers := Http.Headers.empty
  body := ""

def methodNotAllowed : Response where
  version := Http.Version.HTTP_1_1
  status := Http.StatusCode.METHOD_NOT_ALLOWED
  headers := Http.Headers.empty
  body := ""

def notFound : Response where
  version := Http.Version.HTTP_1_1
  status := Http.StatusCode.NOT_FOUND
  headers := Http.Headers.empty
  body := ""

end Response

def normalizePath (path : Http.URI.Path) : Http.URI.Path :=
  -- e.g. the path "/" is parsed to #[""]
  path.filter (λ s => s.length > 0)

instance : Lean.ToJson Http.Headers where
  toJson headers :=
    headers.toList.map (λ (name, values) => 
      ( name.toHeaderString
      , values.map Lean.Json.str |> List.toArray |> Lean.Json.arr
      )
    )
    |> Lean.Json.mkObj

instance : Lean.ToJson Request where
  toJson req := Lean.Json.mkObj
    [ ("version", Lean.Json.str req.version.toString)
    , ("url", Lean.Json.str req.url.toString)
    , ("method", Lean.Json.str req.method.toString)
    , ("headers", Lean.toJson req.headers)
    , ("body", Lean.Json.str req.body.toString)
    ]

def main : IO Unit := do
  let sock <- Socket.mk Socket.AddressFamily.inet Socket.Typ.stream
  Socket.bind sock (Socket.SockAddr4.v4 (Socket.IPv4Addr.mk 127 0 0 1) 3000)
  Socket.listen sock 32
  try repeat do
    let (sock', _) <- Socket.accept sock
    let request <- match (<- recvRequest sock') with
      | .inr request => pure request
      | .inl e => do 
        IO.println s!"Could not parse request: {e}"
        continue
    IO.println (request |> Lean.toJson |> Lean.Json.compress)
    let path := normalizePath request.url.path 
    let response :=
      match path with
        | #[] => match request.method with
          | .GET => Response.ok
          | _ => Response.methodNotAllowed
        | _ => Response.notFound
    sendResponse sock' response
    Socket.close sock'
  finally do
    Socket.close sock
