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
open PreCast
open Ast
open Syntax

open Automata

type playbook = Str_item of str_item | Graph of Automata.G.t

let chain = Gram.Entry.mk "chain"
let stage = Gram.Entry.mk "stage"
let graph = Gram.Entry.mk "graph"
let cron = Gram.Entry.mk "cron"

(* creation of the automata *)

let compile_automata chains crons =

  (* first we need to compile the options (ps: the code is dirty) *)

  let options = Hashtbl.create 0 in

  List.iter (fun (stage, _) -> Hashtbl.add options stage [ `Tickable ]) crons ;

  List.iter
    (fun ((label, op), _) ->
       Hashtbl.add
         options
         label
         (op @
            try
              Hashtbl.find options label
            with Not_found -> []))
    chains ;

  let get_options (label, _) =
    try
      Hashtbl.find options label
    with Not_found -> []
  in

  let update vertex =
    (fst vertex, get_options vertex)
  in
  (* then we fold over the graph & output the AST *)

  List.fold_left
    (fun acc (source, chain) ->
       snd
         (List.fold_left
            (fun (source, acc) (transition, destination) ->
               let source = update source in
               let destination = update destination in
               let acc = G.add_vertex acc source in
               let acc = G.add_vertex acc destination in
               destination, (G.add_edge_e acc (source, transition, destination)))
            (source, acc)
            chain))
    G.empty
    chains


let compile_crons _loc crons =
  let crons =
    List.fold_left
      (fun acc (label, crontab) ->
         let crontab = <:expr< Cron.crontab_of_string $crontab$ >> in
         <:expr< [ ($str:label$, $crontab$) :: $acc$ ] >>
      )
      <:expr< [] >>
      crons
  in
  <:str_item< value crontabs = $crons$ >>


EXTEND Gram

  stage:
    [
      [
        opts = OPT [ "*" -> `Tickable | "-" -> `Mailbox ] ; l = LIDENT ->
        let opts = match opts with
          | None -> []
          | Some `Tickable -> [ `Tickable ]
          | Some `Mailbox -> [ `Mailbox ]
        in
        l, opts
      ]
    ] ;

  chain:
    [
      [
        source = stage ; c = LIST1 [ "~>" ; edge = row_field ; "~>" ; dest = stage -> (edge, dest) ] ->
        (source, c)
      | source = stage ; c = LIST1 [ "<~" ; edge = row_field ; "<~" ; dest = stage -> (edge, dest) ]  ->
        (* now we need to flip that list *)
        let rec flip source acc =
          function
            [] -> assert false
          | [ edge, dest ] -> dest, ((edge, source) :: acc)
          | (edge, dest) :: nxt -> flip dest ((edge, source) :: acc) nxt
        in
        flip source [] c
      ]
    ] ;

  graph:
    [
      [
        chains = LIST1 [ chain ] -> chains
      ]
    ] ;

  cron:
    [
      [
        "CRON" ; stage = LIDENT ; crontab = expr -> (stage, crontab)
      ]
    ] ;

  str_item:
    [
      [
        "PLAYBOOK" ; chains = graph ; crons = LIST0 [ c = cron -> c ] ->
        Pa_type_conv.set_conv_path_if_not_set _loc ;

        let automata = compile_automata chains crons in
        let outbound_types = outbound_types _loc automata in
        let inbound_serializers = inbound_serializers _loc automata in
        let steps = steps _loc automata in
        let dispatch = dispatch _loc automata in

        let automata_description =
          let automata_serialized = graph_to_string automata in
          let triggers = triggers _loc automata in
          let mailables = mailables _loc automata in
          <:str_item<
                 value automata = $str:automata_serialized$ ;
                 value triggers = $triggers$ ;
                 value mailables = $mailables$ ;
          >>
        in

        let crons = compile_crons _loc crons in
        <:str_item<
             $outbound_types$ ;
             $inbound_serializers$ ;
             $steps$ ;
             $dispatch$ ;
             $automata_description$ ;
             $crons$ ;
        >>

      ]

    ];

  END
