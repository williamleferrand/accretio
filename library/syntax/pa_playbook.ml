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
let import = Gram.Entry.mk "import"
let parameters = Gram.Entry.mk "parameters"

let export = ref false


let _ =
  Camlp4.Options.add "-export" (Arg.Unit (fun _ -> export := true)) "export automata"

(* creation of the automata *)

let compile_automata _loc imports chains crons =

  (* first we need to compile the options (ps: the code is dirty) *)

  let options = Hashtbl.create 0 in
  let paths = Hashtbl.create 0 in

  List.iter (fun (stage, _) -> Hashtbl.add options stage [ Tickable ]) crons ;

  let append_options vertex =
    Hashtbl.add
      options
      (Vertex.stage vertex)
      ((Vertex.options vertex) @
         try
           Hashtbl.find options (Vertex.stage vertex)
         with Not_found -> [])
  in

  List.iter
    (fun (source, chain) ->
       append_options source ;
       List.iter
         (fun (_, edge) -> append_options edge)
         chain)
    chains ;

  let get_options label =
    try
      Hashtbl.find options label
    with Not_found -> []
  in

  let get_paths label =
    try
      Hashtbl.find paths label
    with Not_found -> []
  in

  let update vertex =
    append_options vertex ;
    Vertex.({ stage = Vertex.stage vertex ;
              options = get_options (Vertex.stage vertex) ;
              path = get_paths (Vertex.stage vertex) })
  in

  (* let's start by getting the imports *)

  let automata =
    List.fold_left
      (fun acc import ->
         let graph = Automata.load_automata _loc import in
         let acc =
           G.fold_vertex
             (fun vertex acc ->
                (match Vertex.path vertex with
                   [] -> Hashtbl.add paths (Vertex.stage vertex) [ import ]
                 | _ as path -> Hashtbl.add paths (Vertex.stage vertex) path) ;
                G.add_vertex acc (update vertex))
             graph
             acc
         in
         let acc =
           G.fold_edges_e
             (fun edge acc ->
                let edge = G.E.create (update (G.E.src edge)) (G.E.label edge) (update (G.E.dst edge)) in
                G.add_edge_e acc edge)
             graph
             acc
         in
         acc
      )
      G.empty
      imports
  in

  (* then we fold over the graph & output the AST *)

  let automata =
    List.fold_left
      (fun acc (stage, _) ->
         let options = get_options stage in
         G.add_vertex acc { Vertex.stage ; options ; path = [] })
      automata
      crons
  in

  let automata =
    List.fold_left
      (fun acc (source, chain) ->
         let acc = G.add_vertex acc (update source) in
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
      automata
      chains
  in
  automata


let compile_crons _loc crons =
  let crons =
    List.fold_left
      (fun acc (label, crontab) ->
         let crontab = <:expr< Cron.crontab_of_string $crontab$ >> in
         <:expr< [ ($str:label$, $crontab$) :: $acc$ ] >>)
      <:expr< [] >>
      crons
  in
  <:str_item< value crontabs = $crons$ >>


let compile_parameters _loc = function
    None -> <:str_item< value parameters = [] >>
  | Some parameters -> <:str_item< value parameters = $List.fold_left (fun acc (label, key) -> <:expr< [ ($str:label$, $str:key$) :: $acc$ ] >>) <:expr< [] >> parameters$ >>


EXTEND Gram

  stage:
    [
      [
        options = OPT [ "*" -> `Tickable | "-" -> `Mailbox ] ;
        stage = LIDENT ;
        message_strategies = OPT [ "<" ; message_strategies = LIST0 [ strategy = LIDENT -> strategy ] SEP "," ; ">" -> message_strategies ] ->

        let options = match options with
          | None -> []
          | Some `Tickable -> [ Tickable ]
          | Some `Mailbox -> [ Mailbox ]
        in

        let options = match message_strategies with
            None -> options
          | Some strats -> MessageStrategies strats :: options
        in

        { Vertex.stage ; options ; path = [] }
      ]
    ] ;

  import:
    [
      [
        "#" ; "import" ; playbook = LIDENT -> playbook
      ]
    ] ;

  chain:
    [
      [
        source = stage ; c = LIST0 [ "~>" ; edge = row_field ; "~>" ; dest = stage -> (edge, dest) ] ->
        (source, c)
      | source = stage ; c = LIST0 [ "<~" ; edge = row_field ; "<~" ; dest = stage -> (edge, dest) ]  ->
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

  parameters:
    [
      [
        "PARAMETERS" ; parameters = LIST0 [ "-" ; label = STRING ; "," ; key = STRING -> (label, key) ] ->
        parameters
      ]
    ] ;

  str_item:
    [
      [
        parameters = OPT parameters ; "COMPONENT" ; imports = LIST0 [ playbook = import -> playbook ] ; chains = graph ->
        Pa_type_conv.set_conv_path_if_not_set _loc ;
         if !export then
          begin
            let automata = compile_automata _loc imports chains [] in
            dump_automata _loc automata ;
            <:str_item<>>
          end
        else
          List.fold_left
            (fun acc import ->
               <:str_item<
                 $acc$ ;
                 open $uid:String.capitalize import$ ;
               >>)
            <:str_item< >>
            imports

        | parameters = OPT parameters ; "PLAYBOOK" ; imports = LIST0 [ playbook = import -> playbook ] ; chains = graph ; crons = LIST0 [ c = cron -> c ] ->
        Pa_type_conv.set_conv_path_if_not_set _loc ;

        (* it would be great if we could figure out if we are called by ocamldep or sth else .. *)

        if !export then
          begin
            let automata = compile_automata _loc imports chains crons in
            let outbound_types = outbound_types _loc automata in
            let inbound_serializers = inbound_serializers _loc automata in
            let steps = steps _loc automata in
            let dispatch = dispatch _loc automata in
            let dispatch_message_manually = dispatch_message_manually _loc automata in
            let dispatch_message_automatically = dispatch_message_automatically _loc automata in

            let automata_description =
              let automata_serialized = graph_to_string automata in
              let triggers = triggers _loc automata in
              let mailables = mailables _loc automata in
              let email_actions = email_actions _loc automata in
              <:str_item<
                 value automata = $str:automata_serialized$ ;
                 value triggers = $triggers$ ;
                 value mailables = $mailables$ ;
                 value email_actions = $email_actions$ ;
              >>
            in

            let parameters = compile_parameters _loc parameters in
            let crons = compile_crons _loc crons in

            dump_automata _loc automata ;
            print_automata _loc automata ;

            <:str_item<
              $parameters$ ;
              $outbound_types$ ;
              $inbound_serializers$ ;
              $steps$ ;
              $dispatch$ ;
              $dispatch_message_manually$ ;
              $dispatch_message_automatically$ ;
              $automata_description$ ;
              $crons$ ;

            >>
          end
        else
          List.fold_left
            (fun acc import ->
               <:str_item<
                 $acc$ ;
                 open $uid:String.capitalize import$ ;
               >>)
            <:str_item< open Cron >>
            imports
      ]
    ] ;

  END
