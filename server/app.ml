open Graphql_lwt
open Lwt.Infix

type package =
  { id : string
  ; name : string
  ; version : string
  ; updatedAt : string
  }

let compare_by_names n1 n2 =
  let compare_names s1 s2 = compare s1 s2 in
  compare_names n1.name n2.name

let compare_by_dates n1 n2 =
  let date str = List.rev(Str.(split (regexp "-") str)) in
  let compare_dates s1 s2 = compare (date s1) (date s2) in
  compare_dates n1.updatedAt n2.updatedAt

let get_string key l =
  match List.assoc key l with `String s -> s | _ -> raise Not_found

let packages =
  let pkg_file = open_in "packages.json" in
  let json = Ezjsonm.value_from_channel pkg_file in
  close_in pkg_file;
  match json with
  | `A packages ->
    let add_package l = function
      | `O package ->
        let id1 = get_string "id" package in
        let name1 = get_string "name" package in
        let version1 = get_string "version" package in
        let updatedAt1 = get_string "updatedAt" package in
        { id = id1; name = name1; version = version1; updatedAt = updatedAt1 }
        :: l
      | _ ->
        l
    in
    (* List.fast_sort
      compare_by_dates
      (List.rev (List.fold_left add_package [] packages)) *)
    List.fast_sort compare_by_names (List.rev(List.fold_left
       add_package [] packages))
  | _ ->
    []

let package =
  Schema.(
    obj "package" ~fields:(fun _ ->
        [ field
            "id"
            ~doc:"Unique package identifier"
            ~args:Arg.[]
            ~typ:(non_null string)
            ~resolve:(fun _ p -> p.id)
        ; field
            "name"
            ~doc:"Unique package name"
            ~args:Arg.[]
            ~typ:(non_null string)
            ~resolve:(fun _ p -> p.name)
        ; field
            "version"
            ~doc:"Package latest release version"
            ~args:Arg.[]
            ~typ:(non_null string)
            ~resolve:(fun _ p -> p.version)
        ; field
            "updatedAt"
            ~doc:"Package latest update date"
            ~args:Arg.[]
            ~typ:(non_null string)
            ~resolve:(fun _ p -> p.updatedAt)
        ]))

let schema =
  Schema.(
    schema
      [ io_field
          "packages"
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
      printf
        "Client connection error %s: %s(%S)"
        (Unix.error_message error)
        func
        arg
    | exn ->
      printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in
  let callback a b c =
    Graphql_cohttp_lwt.make_callback (fun _req -> ()) schema a b c >>= fun r ->
    match r with
    | `Response (resp, t) ->
      let headers =
        Cohttp.Header.add_list
          resp.headers
          [ "Access-Control-Allow-Origin", "*"
          ; "Access-Control-Allow-Credentials", "true"
          ; "Access-Control-Allow-Methods", "GET,HEAD,OPTIONS,POST,PUT"
          ; ( "Access-Control-Allow-Headers"
            , "Access-Control-Allow-Headers, Origin,Accept, X-Requested-With, \
               Content-Type, Access-Control-Request-Method, \
               Access-Control-Request-Headers" )
          ]
      in
      let cors = { resp with headers; status = `OK } in
      Lwt.return (`Response (cors, t))
    | _ as r ->
      Lwt.return r
  in
  let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d/graphql\n%!" port;
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server |> Lwt_main.run
