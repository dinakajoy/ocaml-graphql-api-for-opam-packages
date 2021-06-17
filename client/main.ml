open Js_of_ocaml
open Brr
open Brr_io
open Fut.Syntax
module Html = Dom_html

type package =
  { id : string
  ; name : string
  ; version : string
  ; updatedAt : string
  }

let packages_holder = ref None

let compare_by_dates n1 n2 =
  let date str = List.rev Str.(split (regexp "-") str) in
  let compare_dates s1 s2 = compare (date s1) (date s2) in
  compare_dates n1.updatedAt n2.updatedAt

let remove_old_tbody () =
  let tf = Html.(createTfoot document) in
  tf##.id := Js.string "tfoot";
  let table =
    Js.Opt.get
      (Html.document##getElementById (Js.string "clear_tbody"))
      (fun () -> assert false)
  in
  let tbody =
    Js.Opt.get
      (Html.document##getElementById (Js.string "opam_packages"))
      (fun () -> assert false)
  in
  Dom.replaceChild table tf tbody

let create_new_tbody () =
  let table =
    Js.Opt.get
      (Html.document##getElementById (Js.string "clear_tbody"))
      (fun () -> assert false)
  in
  let tfoot =
    Js.Opt.get
      (Html.document##getElementById (Js.string "tfoot"))
      (fun () -> assert false)
  in
  let tbody = Html.(createTbody document) in
  tbody##.id := Js.string "opam_packages";
  Dom.replaceChild table tbody tfoot

let display_pkgs { id; name; version; updatedAt } =
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
  let td4 = Html.(createTd document) in
  td4##.innerHTML := Js.string updatedAt;
  Dom.appendChild tr td4;
  Dom.appendChild tbody tr

let get_string key l =
  match List.assoc key l with `String s -> s | _ -> raise Not_found

let formatPackages ?(filter = "sort_by_title") packages =
  match packages with
  | Some packages ->
    (try
       let pkgs = Jstr.to_string (Json.encode packages) in
       let json = Ezjsonm.from_string pkgs in
       let json = Ezjsonm.find json [ "data"; "packages" ] in
       match json with
       | `A pkgs ->
         let add_pkg l = function
           | `O pkg ->
             let id1 = get_string "id" pkg in
             let name1 = get_string "name" pkg in
             let version1 = get_string "version" pkg in
             let updatedAt1 = get_string "updatedAt" pkg in
             { id = id1
             ; name = name1
             ; version = version1
             ; updatedAt = updatedAt1
             }
             :: l
           | _ ->
             l
         in
         if filter = "sort_by_date" then
           List.iter
             display_pkgs
             (List.fast_sort
                compare_by_dates
                (List.rev (List.fold_left add_pkg [] pkgs)))
         else
           List.iter display_pkgs (List.rev (List.fold_left add_pkg [] pkgs))
       | _ ->
         Console.log [ Jstr.of_string pkgs ]
     with
    | e ->
      Console.error [ Jstr.of_string ("Package Error" ^ Printexc.to_string e) ])
  | None ->
    Console.error [ Jstr.of_string "There was an error" ]

let sort_by_title _ =
  remove_old_tbody ();
  create_new_tbody ();
  formatPackages ~filter:"sort_by_title" !packages_holder;
  Js._false

let sort_by_date _ =
  remove_old_tbody ();
  create_new_tbody ();
  formatPackages ~filter:"sort_by_date" !packages_holder;
  Js._false

let search_handler data packages =
  let search_data = Jstr.lowercased data in
  (* let search_data = data##toLowerCase in *)
  remove_old_tbody ();
  create_new_tbody ();
  match packages with
  | Some packages ->
    (try
       let pkgs = Jstr.to_string (Json.encode packages) in
       let json = Ezjsonm.from_string pkgs in
       let json = Ezjsonm.find json [ "data"; "packages" ] in
       match json with
       | `A pkgs ->
         let add_pkg l = function
           | `O pkg ->
             if
               Jstr.includes
                 ~sub:search_data
                 (Jstr.of_string (get_string "name" pkg))
             then
               let id1 = get_string "id" pkg in
               let name1 = get_string "name" pkg in
               let version1 = get_string "version" pkg in
               let updatedAt1 = get_string "updatedAt" pkg in
               { id = id1
               ; name = name1
               ; version = version1
               ; updatedAt = updatedAt1
               }
               :: l
             else
               l
           | _ ->
             l
         in
         List.iter display_pkgs (List.rev (List.fold_left add_pkg [] pkgs))
       | _ ->
         Console.log [ Jstr.of_string pkgs ]
     with
    | e ->
      Console.error [ Jstr.of_string ("Package Error" ^ Printexc.to_string e) ])
  | None ->
    Console.error [ Jstr.of_string "There was an error" ]

let get_packages_response_data response =
  let* data = Fetch.Body.json (Fetch.Response.as_body response) in
  match data with
  | Ok response ->
    packages_holder := Some response;
    Fut.return (Some response)
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return None

let get_packages url query =
  let init =
    Fetch.Request.init
      ~method':(Jstr.of_string "POST")
      ~body:(Fetch.Body.of_jstr (Jstr.of_string query))
      ~headers:
        (Fetch.Headers.of_assoc
           [ Jstr.of_string "Content-Type", Jstr.of_string "application/json" ])
      ()
  in
  let* result = Fetch.url ~init (Jstr.of_string url) in
  match result with
  | Ok response ->
    get_packages_response_data response
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return None

let package_query =
  let query =
    {|
    {
      packages() {
        id
        name
        version
        updatedAt
      }
    }
  |}
  in
  Ezjsonm.value_to_string (`O [ "query", `String query ])

(* G.set_timeout ~ms:3000 (fun () -> assert false) *)

let start _ =
  let url = "http://localhost:8080/graphql" in
  let result = get_packages url package_query in
  Fut.await result formatPackages;
  let sortByTitle =
    Js.Opt.get
      (Html.document##getElementById (Js.string "sort_by_title"))
      (fun () -> assert false)
  in
  sortByTitle##.onclick := Html.handler sort_by_title;
  let sortByDate =
    Js.Opt.get
      (Html.document##getElementById (Js.string "sort_by_date"))
      (fun () -> assert false)
  in
  sortByDate##.onclick := Html.handler sort_by_date;
  let searchFilter =
    Js.Opt.get
      (Html.document##getElementById (Js.string "filter"))
      (fun () -> assert false)
  in
  searchFilter##.onkeyup :=
    Html.handler (fun v ->
        Js.Opt.iter v##.target (fun t ->
            Js.Opt.iter (Dom_html.CoerceTo.input t) (fun t ->
                search_handler (Jstr.of_string(Js.to_string t##.value)) !packages_holder));
        (* Firebug.console##log t##.value *)
        Js._true);
  Js._false

let _ = Html.window##.onload := Html.handler start
