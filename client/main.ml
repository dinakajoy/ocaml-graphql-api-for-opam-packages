open Js_of_ocaml
open Brr
open Brr_io
open Fut.Syntax
module Html = Dom_html

let display_pkgs (id, name, version) =
  let tbody =
    Js.Opt.get
      (Html.document##getElementById (Js.string "opam_packages"))
      (fun () -> assert false)
  in
  let tr = Html.(createTr document) in
  let td1 = Html.(createTd document) in
  td1##.innerHTML := Js.string id;
  Dom.appendChild tr td1;
  let td2 = Html.(createTd document) in
  td2##.innerHTML := Js.string name;
  Dom.appendChild tr td2;
  let td3 = Html.(createTd document) in
  td3##.innerHTML := Js.string version;
  Dom.appendChild tr td3;
  Dom.appendChild tbody tr

let get_string key l =
  match List.assoc key l with `String s -> s | _ -> raise Not_found

let formatPackages packages =
  try
    let pkgs = Jstr.to_string (Json.encode packages) in
    let json = Ezjsonm.from_string pkgs in
    let json = Ezjsonm.find json [ "data"; "packages" ] in
    match json with
    | `A pkgs ->
      let add_pkg l = function
        | `O pkg ->
          let id = get_string "id" pkg in
          let name = get_string "name" pkg in
          let version = get_string "version" pkg in
          (id, name, version) :: l
        | _ ->
          l
      in
      List.iter display_pkgs (List.rev (List.fold_left add_pkg [] pkgs))
    | _ -> Console.log [Jstr.of_string pkgs]
  with
  | e -> Console.error [ Jstr.of_string ("Package Error" ^ Printexc.to_string e) ]

let package_query =
  {|
   {
    packages {
      id
      name
      version
    }
  }
|}

let query_json = Ezjsonm.value_to_string (`O ["query", `String  package_query])

let get_response_data response =
  let* data = Fetch.Body.json (Fetch.Response.as_body response) in
  match data with
  | Ok response ->
    formatPackages response;
    Fut.return ()
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return ()

let get_packages url =
  let init =
    Fetch.Request.init
      ~method':(Jstr.of_string "POST")
      ~body:(Fetch.Body.of_jstr (Jstr.of_string query_json))
      ~headers:(Fetch.Headers.of_assoc [Jstr.of_string "Content-Type", Jstr.of_string "application/json"])
      ()
  in
  let* result = Fetch.url ~init (Jstr.of_string url) in
  match result with
  | Ok response ->
    get_response_data response
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return ()

let start _ =
  let url = "http://localhost:8080/graphql" in
  Fut.await (get_packages url) ignore;
  Js._false

let _ = Html.window##.onload := Html.handler start
