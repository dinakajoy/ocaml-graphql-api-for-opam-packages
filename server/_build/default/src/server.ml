(* open Lwt.Infix *)
open Graphql_lwt

type package = { id : int; name : string; version : string }

(* let packages = [
  { id: 1, name: "ocaml", version: "3.2.0" },
  { id: 2, name: "merlin", version: "1.4.2" },
  { id: 3, name: "dune", version: "1.2.0" },
  { id: 4, name: "ocaml-lsp", version: "2.0.0" },
]

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "packages"
    |> flatten
    |> filter_member "title"
    |> filter_string *)

let packages = [
  { id = 1; name = "ocaml"; version = "3.2.0" };
  { id = 2; name = "merlin"; version = "1.4.2" };
  { id = 3; name = "dune"; version = "1.2.0" };
  { id = 4; name = "ocaml-lsp"; version = "2.0.0" }
]

let package = Schema.(
  obj "package" ~fields:(fun _ ->
    [
      field "id"
        ~doc:"Unique package identifier"
        ~args:Arg.[]
        ~typ:(non_null int)
        ~resolve:(fun _ p -> p.id);
      field "name"
        ~doc:"Unique package name"
        ~args:Arg.[]
        ~typ:(non_null string)
        ~resolve:(fun _ p -> p.name);
      field "version"
        ~doc:"Package latest release version"
        ~args:Arg.[]
        ~typ:(non_null string)
        ~resolve:(fun _ p -> p.version);
    ]
  )
)

(* let rec consume_stream stream =
  Lwt.catch
    (fun () ->
      Lwt_stream.next stream >>= fun x ->
      let (Ok x | Error x) = x in
      Printf.eprintf "stream response: '%s'\n%!" (Yojson.Basic.to_string x);
      if Lwt_stream.is_closed stream then Lwt.return_unit
      else consume_stream stream)
    (function
      | Lwt_stream.Closed | Lwt_stream.Empty -> Lwt.return_unit
      | _ -> Lwt.return_unit)

let set_interval s f destroy =
  let rec set_interval_loop s f n =
    let timeout =
      Lwt_timeout.create s (fun () ->
          if n > 0 then (
            f ();
            set_interval_loop s f (n - 1) )
          else destroy ())
    in
    Lwt_timeout.start timeout
  in
  set_interval_loop s f 5 *)

let schema = Schema.(schema [
  io_field "packages"
    ~typ:(non_null (list (non_null package)))
    ~args:Arg.[]
    ~resolve:(fun _ () -> Lwt_result.return packages)
])


module Graphql_cohttp_lwt =
  Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO)
    (Cohttp_lwt.Body)

let () =
  let open Printf in
  let on_exn = function
    | Unix.Unix_error (error, func, arg) ->
        printf "Client connection error %s: %s(%S)" (Unix.error_message error)
          func arg
    | exn -> printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in
  let callback = Graphql_cohttp_lwt.make_callback (fun _req -> ()) schema in
  let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d/graphql\n%!" port;
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server |> Lwt_main.run
