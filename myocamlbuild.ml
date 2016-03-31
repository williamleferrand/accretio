let type_dir = "type"
let server_dir = "server"
let client_dir = "client"
let client_exec = None

open Printf
open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

(* ocamlfind plugin ***********************************************************)

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x

let split_nl s = split s '\n'

let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s

(* this lists all supported packages *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")

(* this is supposed to list available syntaxes, but I don't know how to
   do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let dispatch_ocamlbuild =
  begin function
    | Before_options ->
      (* by using Before_options one let command line options have an
         higher priority *)
      (* on the contrary using After_options will guarantee to have the
         higher priority *)

      (* override default commands by ocamlfind ones *)
      Options.ocamlc     := ocamlfind & A"ocamlc";
      Options.ocamlopt   := ocamlfind & A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop"

    | After_rules ->

      (* When one link an OCaml library/binary/package, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      (* For each ocamlfind package one inject the -package option when
       	 * compiling, computing dependencies, generating documentation and
       	 * linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      end (find_packages ());

      (* Like -package but for extensions syntax. Morover -syntax is useless
       	 * when linking. *)
      List.iter begin fun syntax ->
        flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      end (find_syntaxes ());

      (* The default "thread" tag is not compatible with ocamlfind.
         Indeed, the default rules add the "threads.cma" or "threads.cmxa"
         options when using this tag. When using the "-linkpkg" option with
         ocamlfind, this module will then be added twice on the command line.

         To solve this, one approach is to add the "-thread" option when using
         the "threads" package using the previous plugin.
      *)
      flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
      flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
      flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]) ;

      (* stuffing the atgen rules here *)

      let tag_atdgen env patterns =
        List.iter (fun p -> tag_file (env p) (Tags.elements (Tags.of_list ["package(atdgen)"]))) patterns
      in

      rule "atdgen: .atd -> _t.ml*"
        ~prods:["%_t.ml";"%_t.mli"]
        ~dep:"%.atd"
        (begin fun env build ->
           let atdgen = "atdgen" in
           tag_atdgen env ["%_t.ml";"%_t.mli"];
           Cmd (S [A atdgen; A "-t"; P (env "%.atd")]);
         end) ;

      rule "atdgen: .atd -> _j.ml*"
        ~prods:["%_j.ml";"%_j.mli";]
        ~dep:"%.atd"
        (begin fun env build ->
           let atdgen = "atdgen" in
           tag_atdgen env ["%_j.ml"; "%_j.mli"];
           Cmd (S [A atdgen; A "-j"; A "-j-std"; P (env "%.atd")]);
         end) ;

      rule "atdgen: .atd -> _v.ml*"
        ~prods:["%_v.ml";"%_v.mli";]
        ~dep:"%.atd"
        (begin fun env build ->
           let atdgen = "atdgen" in
           tag_atdgen env ["%_v.ml";"%_v.mli";];
           Cmd (S [A atdgen; A "-v"; P (env "%.atd")]);
         end) ;

      ()

    | _ -> ()
  end


(* building ocsigen project with ocamlbuild ************************************)

let copy_with_header src prod =
  let contents = Pathname.read src in
  let header = "# 1 \"" ^ src ^ "\"\n" in
  Pack.Shell.mkdir_p (Filename.dirname prod);
  Echo ([header; contents], prod)

let copy_rule_with_header f name ?(deps=[]) src prod =
  rule name ~deps:(src :: deps) ~prod
    (fun env _ ->
       let prod = env prod in
       let src = env src in
       f env (Pathname.dirname prod) (Pathname.basename prod) prod;
       copy_with_header src prod
    )

let flag_infer file type_inferred =
  let file_tag = "file:" ^ file in
  let tags =
    [["ocaml"; "ocamldep"; file_tag];
     ["ocaml"; "compile"; file_tag];
     ["ocaml"; "infer_interface"; file_tag];
    ]
  in
  let f tags =
    flag tags (S [ A "-ppopt"; A "-type"; A "-ppopt"; P type_inferred])
  in
  List.iter f tags;
  flag ["ocaml"; "doc"; file_tag] (S [A "-ppopt"; A "-notype"])

let copy_rule_server =
  copy_rule_with_header
    (fun env dir name file ->
       let path = env "%(path)" in
       let type_inferred =
         Pathname.concat
           (Pathname.concat path type_dir)
           (Pathname.update_extension "inferred.mli" name)
       in
       tag_file file
         [ "pkg_eliom.server"; "pkg_eliom.syntax.server"; "thread";
           "pkg_js_of_ocaml.deriving.syntax" ; "pkg_deriving.syntax" ;
           "syntax_camlp4o" ];
       flag_infer file type_inferred;
       Pathname.define_context dir [ path ];
       Pathname.define_context path [ dir ])

let copy_rule_client =
  copy_rule_with_header
    (fun env dir name file ->
       let path = env "%(path)" in
       let type_inferred =
         Pathname.concat
           (Pathname.concat path type_dir)
           (Pathname.update_extension "inferred.mli" name)
       in
       tag_file file
         [ "pkg_eliom.client"; "pkg_eliom.syntax.client"; "thread";
           "syntax_camlp4o";
         ];
       flag_infer file type_inferred;
       Pathname.define_context dir [ path ];
       Pathname.define_context path [ dir ])


(* the types are inferred from the server code *)

let copy_rule_type =
  copy_rule_with_header
    (fun env dir name file ->
       let path = env "%(path)" in
       let server_dir = Pathname.concat path server_dir in
       let server_file = Pathname.concat server_dir name in
       tag_file file
         ("pkg_eliom.syntax.type" :: "thread" :: "syntax_camlp4o"
          :: Tags.elements (tags_of_pathname server_file)) ;
       Pathname.define_context dir [path; server_dir])

let js_rule () =
  let linker tags deps out =
    Cmd (S [A "js_of_eliom"; T tags;
            Command.atomize_paths deps; A "-o"; Px out])
  in
  rule "js_of_eliom: .cmo -> .js" ~dep:"%.cmo" ~prod:"%.js"
    (fun env ->
       Pack.Ocaml_compiler.link_gen
         "cmo" "cma" "cma" ["cmo"; "cmi"] linker
         (fun tags ->
            Tags.union
              (tags_of_pathname (env "%.js"))
              (tags++"ocaml"++"link"++"byte"++"jslink"++"js_of_eliom")
         )
         "%.cmo" "%.js"
         env)

let add_to_targets () =
  match client_exec with
  | None -> ()
  | Some x -> Options.targets @:= [x]

let dispatch_eliom hook =
  dispatch_ocamlbuild hook;
  match hook with
  | After_options ->
    add_to_targets ();
  | After_rules ->
    js_rule ();

    copy_rule_server "*.eliom -> **/_server/*.ml"
      ~deps:["%(path)/" ^ type_dir ^ "/%(file).inferred.mli"]
      "%(path)/%(file).eliom"
      ("%(path)/" ^ server_dir ^ "/%(file:<*>).ml");

    copy_rule_server "*.eliomi -> **/_server/*.mli"
      "%(path)/%(file).eliomi"
      ("%(path)/" ^ server_dir ^ "/%(file:<*>).mli");

    copy_rule_type "*.eliom -> **/_type/*.ml"
      "%(path)/%(file).eliom"
      ("%(path)/" ^ type_dir ^ "/%(file:<*>).ml");

    copy_rule_type "*.eliomi -> **/_type/*.mli"
      "%(path)/%(file).eliomi"
      ("%(path)/" ^ type_dir ^ "/%(file:<*>).mli");

    copy_rule_client "*.eliom -> **/_client/*.ml"
      ~deps:["%(path)/" ^ type_dir ^ "/%(file).inferred.mli"]
      "%(path)/%(file).eliom"
      ("%(path)/" ^ client_dir ^ "/%(file:<*>).ml");

    copy_rule_client "*.eliomi -> **/_client/*.mli"
      "%(path)/%(file).eliomi"
      ("%(path)/" ^ client_dir ^ "/%(file:<*>).mli");

    copy_rule_server "*.eliom -> _server/*.ml"
      ~deps:[type_dir ^ "/%(file).inferred.mli"]
      "%(file).eliom" (server_dir ^ "/%(file:<*>).ml");

    copy_rule_server "*.eliomi -> _server/*.mli"
      "%(file).eliomi" (server_dir ^ "/%(file:<*>).mli");

    copy_rule_type "*.eliom -> _type/*.ml"
      "%(file).eliom" (type_dir ^ "/%(file:<*>).ml");

    copy_rule_type "*.eliomi -> _type/*.mli"
      "%(file).eliomi" (type_dir ^ "/%(file:<*>).mli");

    copy_rule_client "*.eliom -> _client/*.ml"
      ~deps:[type_dir ^ "/%(file).inferred.mli"]
      "%(file).eliom" (client_dir ^ "/%(file:<*>).ml");

    copy_rule_client "*.eliomi -> _client/*.mli"
      "%(file).eliomi" (client_dir ^ "/%(file:<*>).mli");

  | _ -> ()


(* copy the graph files to the server, apply the syntax *)

let rules_copy_graph prefix folder =
  rule (sprintf "copy %s eliom files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/server/%.ml")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/server/%.ml")) ])))

let rules_copy_graph_client prefix folder =
  rule (sprintf "copy %s eliom files for prefix %s in the client" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/client/%.ml")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/client/%.ml")) ])))

let rules_copy_eliom prefix folder =
  rule (sprintf "copy %s eliom files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.eliom")
    ~prod:(prefix ^ "/%.eliom")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.eliom")) ;
                 P (env (prefix ^ "/%.eliom")) ])))

let rules_copy_ml_server prefix folder =
  rule (sprintf "copy %s ml files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/%.ml")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/%.ml")) ])))

let rules_copy_playbooks_server prefix folder =
  rule (sprintf "copy %s ml files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/%.ml")
    (fun env _ ->
       tag_file (env (prefix ^ "/%.ml")) [ "use_playbooks" ] ; (* is it needed? *)
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/%.ml")) ])))

let rules_copy_playbooks_client prefix folder =
  rule (sprintf "copy %s ml files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/%.ml")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/%.ml")) ])))

let rules_copy_ml_client prefix folder =
  rule (sprintf "copy %s ml files for prefix %s" folder prefix)
    ~dep:(folder ^ "/%.ml")
    ~prod:(prefix ^ "/%.ml")
    (fun env _ ->
       (Cmd (S [ A "cp" ;
                 P (env (folder ^ "/%.ml")) ;
                 P (env (prefix ^ "/%.ml")) ])))

(* build the version **********************************************************)

let make_VERSION env build =
  let use_git = try ignore(Unix.getenv "NOGIT"); false with _ -> true in
  if use_git && Ocamlbuild_pack.My_std.sys_file_exists "../.git"
  then Cmd (S [ A "git" ; A "log" ; A "-n1" ; A "--pretty=format:%h" ;
                Sh ">" ; P "VERSION" ])
  else
    begin
      print_endline "* do not generate VERSION";
      let _ = build [["share/VERSION"]] in
      Cmd (S [ A "cp"; P "share/VERSION" ; P "VERSION"])
    end

let make_VERSION_NUM env build =
  let use_git = try ignore(Unix.getenv "NOGIT"); false with _ -> true in
  if use_git && Ocamlbuild_pack.My_std.sys_file_exists "../.git"
  then
    Cmd (S [ A "git" ; A "log" ; A "--pretty=format:''" ; Sh "|" ; Sh "wc" ;
             A "-l" ; Sh "|" ; Sh "sed" ; A "s/ //g" ; Sh ">" ;
             P "VERSION_NUM" ])
  else
    begin
      print_endline "* do not generate VERSION_NUM";
      let _ = build [["share/VERSION_NUM"]] in
      Cmd (S [ A "cp"; P "share/VERSION_NUM" ; P "VERSION_NUM"])
    end

let make_version _ build =
  build [["VERSION";"VERSION_NUM"]];
  let version =
    try
      let chan = open_in "VERSION" in
      let version = input_line chan in
      if version == "" then "edge" else version
    with _ -> "edge" in
  let version_num =
    try
      let chan = open_in "VERSION_NUM" in
      let version_num = input_line chan in
      int_of_string version_num
    with _ -> 0 in
  let cmd =
    Printf.sprintf
      "{shared{ let version = %S\nlet version_num = %d }}"
      version version_num
  in
  Seq [
    Cmd (S [ A "mkdir"; A"-p"; P "common"]) ;
    Cmd (S [ A "echo"; Quote (Sh cmd); Sh ">"; P "app/version.eliom" ])
  ]


let dispatch_ys hook =
  dispatch_eliom hook;
  match hook with
  | After_options ->
    ()
  | After_rules ->

    rule "version.ml" ~prod:"app/version.eliom" ~deps:["VERSION"; "VERSION_NUM"] make_version;
    rule "VERSION" ~prod:"VERSION" make_VERSION;
    rule "VERSION_NUM" ~prod: "VERSION_NUM" make_VERSION_NUM ;

    let register_extension ?(before=[]) ?(after=[]) label =
      let use = "use_"^label in
      let library = "library/syntax/pa_" ^ label ^ ".cma" in
      dep [ "ocamldep"; use ] ([ library ]) ;
      dep [ "infer_interface"; use ] ([ library ]) ;
      flag [ "ocaml"; "compile"; use ] (S (before @ [ A "-ppopt"; A library ] @ after)) ;
      flag [ "ocaml"; "ocamldep"; use ] (S (before @ [ A "-ppopt"; A library ])) ;
      flag [ "ocaml"; "infer_interface"; use ] (S (before @ [ A "-ppopt"; A library ] @ after)) ;
    in

    register_extension "graph" ;

    register_extension
      ~before:[ A "-ppopt" ; Sh "`ocamlfind query type_conv`/pa_type_conv.cma" ] "graph_client" ;

    register_extension "operators" ;

    register_extension
      ~before:[ A "-ppopt" ; Sh "`ocamlfind query type_conv`/pa_type_conv.cma" ;
                A "-ppopt" ; Sh "`ocamlfind query deriving`/pa_deriving_common.cma" ;
                A "-ppopt" ; Sh "`ocamlfind query deriving`/pa_deriving_tc.cma" ;
                A "-ppopt" ; Sh "`ocamlfind query deriving-yojson`/syntax.cma" ; ]
      ~after:[ A "-ppopt" ; A "-export" ] "playbooks" ;

    (* copy the graph to the server *)
    rules_copy_graph "app" "graph" ;
    rules_copy_graph_client "app" "graph" ;

    (* copy the playbooks to the app *)
    rules_copy_playbooks_server "app/server" "playbooks" ;
    rules_copy_ml_server "app/server" "api" ;
    (* rules_copy_playbooks_client "app/client" "playbooks" ; *)

    (* copy the playbooks to the test *)
    rules_copy_playbooks_server "test" "playbooks" ;
    rules_copy_ml_server "test" "library/server" ;

    rules_copy_ml_server "app/server" "library/server" ;
    rules_copy_ml_server "app/server" "library/shared" ;
    rules_copy_ml_server "app/server" "library/syntax" ;

    rules_copy_ml_client "app/client" "library/client" ;
    rules_copy_ml_client "app/client" "library/shared" ;
    rules_copy_ml_client "app/client" "library/syntax" ;

  | _ -> ()

let _ =
  Ocamlbuild_plugin.dispatch dispatch_ys
