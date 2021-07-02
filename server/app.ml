open Graphql_lwt
open Lwt.Infix
open Printf
module Graphql_cohttp_lwt =
  Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO)
    (Cohttp_lwt.Body)

type dependency = { packageName : string }

type package =
  { id : string
  ; name : string
  ; version : string
  ; updatedAt : string
  ; dependsOn : dependency list
  }

(* Sort packages in ascending order by name *)
  let compare_by_names n1 n2 =
  let compare_names s1 s2 = compare s1 s2 in
  compare_names n1.name n2.name

(* Sort packages in ascending order by date *)
  let compare_by_dates n1 n2 =
  let date str = List.rev Str.(split (regexp "-") str) in
  let compare_dates s1 s2 = compare (date s1) (date s2) in
  compare_dates n1.updatedAt n2.updatedAt

(* Filter packages and return packages with name that contains search query *)
  let starts_with s1 s2 =
  let len1 = String.length s1 in
  if len1 > String.length s2 then
    false
  else
    let s1 = String.lowercase_ascii s1 in
    let s2 = String.lowercase_ascii s2 in
    String.(equal (sub s2 0 (length s1)) s1)

(* Return package details that matches the specified package id *)
let isPackage s1 s2 =
  let len1 = String.length s1 in
  if len1 > String.length s2 then
    false
  else
    let s1 = String.lowercase_ascii s1 in
    let s2 = String.lowercase_ascii s2 in
    String.(equal s2 s1)

let dependency_of_json = function
  | `O [ ("packageName", `String packageName) ] ->
    Ok { packageName }
  | _ ->
    Error (`Msg "Not Working!!!")

let package_of_json = function
  | `O
      [ ("id", `String id)
      ; ("name", `String name)
      ; ("version", `String version)
      ; ("updatedAt", `String updatedAt)
      ; ("dependsOn", `A dependsOn)
      ] ->
    let dependsOn =
      try
        Ok (List.map (fun p -> Result.get_ok (dependency_of_json p)) dependsOn)
      with
      | _ ->
        Error (`Msg "Not Working!")
    in
    (match dependsOn with
    | Ok dependencies ->
      Ok { id; name; version; updatedAt; dependsOn = dependencies }
    | Error _ as e ->
      e)
  | _ ->
    Error (`Msg "Not Working!")

let getAllPackages pkgs =
  match pkgs with
  | `A packages ->
    (try
       Ok (List.map (fun p -> Result.get_ok (package_of_json p)) packages)
     with
    | _ ->
      Error (`Msg "Not Working!"))
  | _ ->
    Ok []

let readAllPackages () =
  let pkg_file = open_in "packages.json" in
  let json = Ezjsonm.value_from_channel pkg_file in
  close_in pkg_file;
  json

let get_string key l =
  match List.assoc key l with `String s -> s | _ -> raise Not_found

(* let getAllPackagesWithDependencies pkgs =
  match pkgs with |
   `A packages -> let add_package l = function | `O package -> let id1 =
   get_string "id" package in let name1 = get_string "name" package in let
   version1 = get_string "version" package in let updatedAt1 = get_string
   "updatedAt" package in { id = id1; name = name1; version = version1;
   updatedAt = updatedAt1 } :: l | _ -> l in List.fast_sort compare_by_names
   (List.rev (List.fold_left add_package [] packages)) | _ -> [] *)

let allPackages =
  let packages = readAllPackages () in
  let packages = getAllPackages packages in
  match packages with Ok packages -> packages | Error (`Msg m) -> failwith m

let dependency =
  Schema.(
    obj "package" ~fields:(fun _ ->
        [ field
            "packageName"
            ~doc:"All packages that a package is dependent on"
            ~args:Arg.[]
            ~typ:(non_null string)
            ~resolve:(fun _ p -> p.packageName)
        ]))

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
        ; field
            "dependsOn"
            ~doc:"Package dependencies"
            ~args:Arg.[]
            ~typ:(non_null (list (non_null dependency)))
            ~resolve:(fun _ p -> p.dependsOn)
        ]))

let schema =
  Schema.(
    schema
      [ io_field
          "allPackages"
          ~typ:(non_null (list (non_null package)))
          ~args:
            Arg.
              [ arg'
                  ~doc:
                    "Filter packages in asc by name or date or based on search \
                     query"
                  "filter"
                  ~typ:string
                  ~default:"sortByName"
              ]
          ~resolve:(fun _filter () filter ->
            if filter = "sortByDate" then
              let sortedDate = List.fast_sort compare_by_dates allPackages in
              Lwt_result.return sortedDate
            else if filter = "sortByName" then
              Lwt_result.return allPackages
            else
              let packagesList =
                List.filter
                  (fun package -> starts_with filter package.name)
                  allPackages
              in
              Lwt_result.return packagesList)
      ; field
          "package"
          ~typ:(non_null package)
          ~args:
            Arg.[ arg ~doc:"Get single package" "id" ~typ:(non_null string) ]
          ~resolve:(fun _id () id ->
            let packageData =
              List.find (fun package -> isPackage id package.id) allPackages
            in
            packageData)
      (* ; field
          "packages"
          ~typ:(non_null (list (non_null package)))
          ~args:
            Arg.
              [ arg
                  ~doc:
                    "Get all other package(s) that depends on defined\n package"
                  "name"
                  ~typ:(non_null string)
              ]
          ~resolve:(fun _name () name ->
            let packageDependants = getPackageDependants name in
            Lwt_result.return packageDependants) *)
      ])

let () =
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
