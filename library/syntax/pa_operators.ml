(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_operators"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) =
struct

  open Camlp4.PreCast
  include Syntax

  (* this is a duplicate, but I don't want to depend on pa_vertex outside of
     /graph *)

  let wrap_field _loc uid field =
    <:expr< (Ys_uid.to_string_padded $uid$) ^ "_" ^ $str:field$ >>

  EXTEND Gram

  expr: LAST
      [
        [

          (* get fields with a lwt argument*)
          "$$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "->" ; field = LIDENT ->
            let obj = "Object_"^obj in
            <:expr< Lwt.bind ($uid$) $uid:obj$.Store.$lid:"get_"^field$ >>

          (* get fields *)
          | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "->" ;
          field = LIDENT ->
            let obj = "Object_"^obj in
            <:expr< $uid:obj$.Store.$lid:"get_"^field$ $uid$ >>

          | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "->" ;
          "(" ; fields = LIST1 [ f = LIDENT -> f ] SEP "," ; ")" ->
          (* Printf.eprintf "hit rule 1\n"; flush stdout ; *)
          let obj = "Object_"^obj in

          (match fields with
          | [ field ] ->
            (* singleton case need to be handled separately *)
            <:expr< $uid:obj$.Store.$lid:"get_"^field$ $uid$ >>

          | _ as fields ->
          let keys = List.map (wrap_field _loc uid) fields in

          (* optimisation? we could use an iterator & skip the keys we're
             not interested in *)
          let idents = List.map (fun field -> <:expr< $lid:field$ >>) fields in
          let _, bindings =
            List.fold_left
              (fun (cnt, acc) field ->
                 (cnt + 1),
                 <:expr<
                    Lwt.bind
                      ($uid:obj$.Store.$lid:"raw_"^field$ $uid$ data_options.($int:(string_of_int cnt)$))
                      (fun [ $lid:field$ -> $acc$ ])
                 >>)
              (0,
               <:expr<
                 Lwt.return $tup:Ast.exCom_of_list idents$
               >>)
              fields
          in
          <:expr<
            Lwt.bind
             (Ys_persistency.Ocsipersist.Batch.get $uid:obj$.Store.db [| $list:keys$ |])
             (fun data_options ->
               (* now we need to remap this array to a tuple of results
                  which isn't super trivial to do in parallel *)
            $bindings$)
          >>)

        (* set a field. no need to batch as it is asynchrounous *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "=" ; e = expr LEVEL "top"  ->
          (* Printf.eprintf "hit rule 2\n"; flush stdout ; *)
          let obj = "Object_"^obj in
          <:expr<

            ($uid:obj$.Store.$lid:"set_"^field$ $uid$ $e$)

          >>

        (* patch a field lwt. no need to batch because we never patch several fields, but risk of deadlock *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "%%%" ; p = expr LEVEL "simple" ->
          (* Printf.eprintf "hit rule 3\n"; flush stdout ; *)
          let obj = "Object_"^obj in
          <:expr<

          $uid:obj$.Store.$lid:"patch_lwt_"^field$ $uid$ $p$

          >>

        (* patch a field. no need to batch because we never patch several fields *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "%%" ; p = expr LEVEL "simple" ->
          (* Printf.eprintf "hit rule 3\n"; flush stdout ; *)
          let obj = "Object_"^obj in
          <:expr<

          $uid:obj$.Store.$lid:"patch_"^field$ $uid$ $p$

          >>

        (* attach an edge *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "+=" ; p = expr LEVEL "simple" ->
          (* Printf.eprintf "hit rule 4\n"; flush stdout ; *)
          let obj = "Object_"^obj in
          <:expr<

          $uid:obj$.Store.$lid:"attach_to_"^field$ $uid$ $p$

          >>

        (* attach an edge, check unicity *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "+=!" ; p = expr LEVEL "simple" ->
          let obj = "Object_"^obj in
          <:expr<

          $uid:obj$.Store.$lid:"attach_to_"^field^"_check_unicity"$ $uid$ $p$

          >>

        (* detach an edge *)

        | "$" ; obj = LIDENT ; "(" ; uid = expr ; ")" ; "<-" ; field = LIDENT ;
          "-=" ; p = expr LEVEL "simple" ->
          (* Printf.eprintf "hit rule 5\n"; flush stdout ; *)
          let obj = "Object_"^obj in
          <:expr<

          $uid:obj$.Store.$lid:"detach_from_"^field$ $uid$ $p$

          >>

        (* the scala _ *)

        | "_" ; "." ; m = OPT [ m = UIDENT ; "." -> m ] ; lid = LIDENT ->
          (* Printf.eprintf "hit rule 6\n"; flush stdout ; *)
          match m with
            None -> <:expr< fun e -> e.$lid:lid$ >>
          | Some m -> <:expr< fun e -> e.$uid:m$.$lid:lid$ >>
        ]
      ];

 END


end


module M = Register.OCamlSyntaxExtension(Id)(Make)
