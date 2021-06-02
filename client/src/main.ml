open Js_of_ocaml
open Js_of_ocaml_lwt
(* open Dom_html *)
module Html = Dom_html
open Ezjsonm
(* open Lwt
open Cohttp
open Cohttp_lwt_unix *)

(* let get_query = {|
  query {
    packages {
      id
      name
      version
    }
  }
|}

let get_packages query =
  let open Lwt.Infix in
  let headers =
    Cohttp.Header.of_list [ "Content-Type", "application/json"; "Accept", "application/json" ]
  in
  let uri = Uri.of_string("http://localhost:8080/graphql") in
  let body = `O [("query", `String query)] in
  let serialized_body = Ezjsonm.to_string body in
  Cohttp_lwt_unix.Client.post ~headers~body:(`String serialized_body) uri >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun pkg_data ->
  let json = Ezjsonm.from_string pkg_data in
  let json = Ezjsonm.find json [ "data"; "packages" ] in
    let id = Ezjsonm.get_string (find json [ "id" ]) in
    let name = Ezjsonm.get_string (find json [ "name" ]) in
    let name = String.capitalize_ascii name in
    let version = Ezjsonm.get_string (find json [ "version" ]) in *)

let start _ =
  (* Lwt_main.run get_repo_data get_query in *)
  let doc = Html.document in
  let body =
    Js.Opt.get (doc##getElementById (Js.string "opam_packages")) (fun () -> assert false)
  in
  let div = Html.createDiv doc in
    div##.id := Js.string "pkg_div";
  Dom.appendChild body div;
  Js._false

let _ = Html.window##.onload := Html.handler start
