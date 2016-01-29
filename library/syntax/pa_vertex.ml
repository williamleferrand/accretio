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


open Printf

open Camlp4
open PreCast
open Ast

open Pa_type_conv
open Pa_tools
open Pa_options

let assoc key list =
  try
    List.assoc key list
  with Not_found -> `None

(* load function **************************************************************)

let wrap_field _loc field = (* you need to have uid in the environment *)
  <:expr< (Ys_uid.to_string_padded uid) ^ "_" ^ $str:field$ >>

let rec extract_keys_and_builder _loc cnt = function
   | <:ctyp< $tp1$; $tp2$ >> ->
     let keys1, binder1, builder1, cnt = extract_keys_and_builder _loc cnt tp1 in
     let keys2, binder2, builder2, cnt = extract_keys_and_builder _loc cnt tp2 in
     (keys1 @ keys2, binder1 @ binder2,
      <:rec_binding< $builder1$; $builder2$ >>,
      cnt)

   | <:ctyp< uid : $tp$ >> -> [], [], <:rec_binding< uid >>, cnt

   | <:ctyp< $lid:field$ : mutable $tp$ >> ->
     failwith "mutable fields are prohibited"

   | <:ctyp< $lid:field$ : $tp$ >> ->
     [ wrap_field _loc field ],
     [ <:expr< $lid:"raw_"^field$ uid fields.($int:(string_of_int cnt)$) >>, field ],
     <:rec_binding< $lid:field$ >>,
     (cnt + 1)

   | _ -> failwith "can't handle some awkward type"

(* generate aliases if they are defined ***************************************)

let generate_aliases _loc uniques aliases =
  let module_name = get_module_name _loc in
  List.map
    (function
      | Spatial field ->
        let index_name = "spatial_"^field in
        <:str_item<

          value $lid:index_name$ = Ys_aliases.Spatial.create () ;

          value $lid:"insert_"^field$ uid envelope =
            Ys_aliases.Spatial.insert $lid:index_name$ uid envelope ;

          value $lid:"search_by_"^field$ envelope =
            Ys_aliases.Spatial.search $lid:index_name$ envelope ;

        >>

      | PlainText field ->
        let plain_text_table = field in
        <:str_item<

          value $lid:plain_text_table$ =
            $str:module_name ^ "_" ^ plain_text_table$ ;

          value $lid:"insert_"^field$ uid content =
            Ys_aliases.PlainText.insert $lid:plain_text_table$ uid (String.lowercase content) (Ys_time.now ()) ;

          value $lid:"search_"^field$ query =
            Ys_aliases.PlainText.search $lid:plain_text_table$ query ;

        >>

      | PlainTextEdge (field, constr) ->
        let plain_text_table = field in
        <:str_item<

          value $lid:plain_text_table$ =
            $str:module_name ^ "_" ^ plain_text_table$ ;

          value $lid:"remove_"^field$ uid edges =
            Lwt_list.iter_p
              (fun (label, edge) ->
               Ys_aliases.PlainTextEdge.remove $lid:plain_text_table$ uid edge)
              edges ;

          value $lid:"insert_"^field$ uid edges =
            Lwt_list.iter_p
              (fun (label, edge) ->
                match $lid:constr$ label with
             [ None -> Lwt.return_unit
             | Some document -> Ys_aliases.PlainTextEdge.insert $lid:plain_text_table$ uid edge (String.lowercase document) ])
edges ;

          value $lid:"search_"^field$ host query =
            Ys_aliases.PlainTextEdge.search $lid:plain_text_table$ query host ;

        >>

      | String field when List.mem field uniques ->
        let db_name = "db_" ^ field in
        <:str_item<
          value $lid:db_name$ =
             let dir = Ys_config.get_string "db-root-dir" ^ "/" ^ $str:module_name$ ^ "_by_" ^ $str:field$ in
             Ys_persistency.Ocsipersist.open_db_blocking ~cache_size:(Ys_config.get_int "cache-size") dir ;

          value $lid:"set_"^field^"_by_string"$ uid key =
            Ys_persistency.Ocsipersist.put $lid:db_name$ (String.lowercase key) (Ys_uid.to_string uid) ;

          value $lid:"unset_"^field^"_by_string"$ key =
            Ys_persistency.Ocsipersist.delete $lid:db_name$ (String.lowercase key) ;

          value $lid:"find_by_"^field$ key =
             Lwt.bind
                (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                (fun
                   [ None -> Lwt.return_none
                   | Some uid -> Lwt.return (Some (Ys_uid.of_string uid)) ]) ;

          value $lid:"is_"^field^"_assigned_to_other"$ uid key =
            Lwt.bind
               (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
               (fun
                 [ Some uid' when Ys_uid.of_string uid' <> uid -> Lwt.return True
                 | _ -> Lwt.return False ]) ;

          value $lid:"is_"^field^"_assigned"$ key =
            Lwt.bind
               (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
               (fun
                 [ Some uid -> Lwt.return (Some (Ys_uid.of_string uid))
                 | _ -> Lwt.return_none ]) ;

        >>

   | StringFun (field, constr) when List.mem field uniques ->
        let db_name = "db_" ^ field in
        <:str_item<
          value $lid:db_name$ =
             let dir = Ys_config.get_string "db-root-dir" ^ "/" ^ $str:module_name$ ^ "_by_" ^ $str:field$ in
             Ys_persistency.Ocsipersist.open_db_blocking ~cache_size:(Ys_config.get_int "cache-size") dir ;

          value $lid:"set_"^field^"_by_string"$ uid key =
            Ys_persistency.Ocsipersist.put $lid:db_name$ (String.lowercase key) (Ys_uid.to_string uid) ;

          value $lid:"unset_"^field^"_by_string"$ key =
            Ys_persistency.Ocsipersist.delete $lid:db_name$ (String.lowercase key) ;

          value $lid:"find_by_"^field$ key =
             Lwt.bind
                (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                (fun
                   [ None -> Lwt.return_none
                   | Some uid -> Lwt.return (Some (Ys_uid.of_string uid)) ]) ;

          value $lid:"is_"^field^"_assigned_to_other"$ uid v =
            match $constr$ v with
            [ None -> Lwt.return False
            | Some key ->
               Lwt.bind
                 (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                 (fun
                   [ Some uid' when Ys_uid.of_string uid' <> uid -> Lwt.return True
                   | _ -> Lwt.return False ]) ] ;

          value $lid:"is_"^field^"_assigned"$ v =
            match $constr$ v with
            [ None -> Lwt.return_none
            | Some key ->
               Lwt.bind
                 (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                 (fun
                   [ Some uid -> Lwt.return (Some (Ys_uid.of_string uid))
                   | _ -> Lwt.return_none ]) ] ;

        >>

      | Strings field when List.mem field uniques ->
        let db_name = "db_" ^ field in
        <:str_item<
          value $lid:db_name$ =
             let dir = Ys_config.get_string "db-root-dir" ^ "/" ^ $str:module_name$ ^ "_by_" ^ $str:field$ in
             Ys_persistency.Ocsipersist.open_db_blocking ~cache_size:(Ys_config.get_int "cache-size") dir ;

          value $lid:"set_"^field^"_by_strings"$ uid keys =
            Lwt_list.iter_s
               (fun key -> Ys_persistency.Ocsipersist.put $lid:db_name$ (String.lowercase key) (Ys_uid.to_string uid))
               keys ;

          value $lid:"unset_"^field^"_by_strings"$ keys =
            Lwt_list.iter_s
               (fun key -> Ys_persistency.Ocsipersist.delete $lid:db_name$ (String.lowercase key))
               keys ;

          value $lid:"find_by_"^(String.sub field 0 (String.length field - 1))$ key =
            Lwt.bind
               (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
               (fun
                 [ None -> Lwt.return_none
                 | Some uid -> Lwt.return (Some (Ys_uid.of_string uid)) ]) ;

          (* we will call this function only to check if the alias already
             exists, so we'd better check them all *)

          value $lid:"is_"^field^"_assigned_to_other"$ uid keys =
            Lwt.bind
               (Lwt_list.map_p
                 (fun key -> Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                 keys)
               (fun bindings ->
                    Lwt.return (List.fold_left (fun acc -> fun [ Some uid' when Ys_uid.of_string uid' <> uid -> True | _ -> acc ]) False bindings)) ;


          value $lid:"is_"^field^"_assigned"$ keys =
            Lwt.bind
               (Lwt_list.map_p
                 (fun key -> Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                 keys)
               (fun bindings ->
                    Lwt.return (List.fold_left (fun acc -> fun [ Some uid -> Some (Ys_uid.of_string uid) | _ -> acc ]) None bindings)) ;

        >>

      | PlainTextAndString field when List.mem field uniques ->
        let plain_text_table = field in
        let db_name = "db_" ^ field in
        <:str_item<

          value $lid:plain_text_table$ =
            $str:module_name ^ "_" ^ plain_text_table$ ;

          value $lid:"insert_"^field$ uid content =
            Ys_aliases.PlainText.insert $lid:plain_text_table$ uid (String.lowercase content) (Ys_time.now ());

          value $lid:"search_"^field$ query =
            Ys_aliases.PlainText.search $lid:plain_text_table$ query ;

          value $lid:db_name$ =
             let dir = Ys_config.get_string "db-root-dir" ^ "/" ^ $str:module_name$ ^ "_by_" ^ $str:field$ in
             Ys_persistency.Ocsipersist.open_db_blocking ~cache_size:(Ys_config.get_int "cache-size") dir ;

          value $lid:"set_"^field^"_by_string"$ uid key =
            Ys_persistency.Ocsipersist.put $lid:db_name$ (String.lowercase key) (Ys_uid.to_string uid) ;

          value $lid:"unset_"^field^"_by_string"$ key =
            Ys_persistency.Ocsipersist.delete $lid:db_name$ (String.lowercase key) ;

          value $lid:"find_by_"^field$ key =
             Lwt.bind
                (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
                (fun
                   [ None -> Lwt.return_none
                   | Some uid -> Lwt.return (Some (Ys_uid.of_string uid)) ]) ;

          value $lid:"is_"^field^"_assigned_to_other"$ uid key =
            Lwt.bind
               (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
               (fun
                 [ Some uid' when Ys_uid.of_string uid' <> uid -> Lwt.return True
                 | _ -> Lwt.return False ]) ;


          value $lid:"is_"^field^"_assigned"$ key =
            Lwt.bind
               (Ys_persistency.Ocsipersist.get $lid:db_name$ (String.lowercase key))
               (fun
                 [ Some uid -> Lwt.return (Some (Ys_uid.of_string uid))
                 | _ -> Lwt.return_none ]) ;

        >>

      | String field -> failwith "string fields should be unique"
      | Strings field -> failwith "strings fields should be unique"
      | StringFun (field, _) -> failwith "stringfun fields should be unique"
      | _ -> failwith "the alias you requested hasn't been implemented")

    aliases


(* there is no need for lock when setting, as the put operation isn't blocking
   so there is no risk for race conditions *)

let rec generate_set_functions _loc aliases builders = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    <:str_item<
      $generate_set_functions _loc aliases builders tp1$;
      $generate_set_functions _loc aliases builders tp2$;
    >>

  | <:ctyp< uid : $tp$ >> -> <:str_item< >>

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : $tp$ >> ->
    let modules, lid = split_modules_ctyp field tp in
    <:str_item<
      value $lid:"set_"^field$ uid v =
        let size = $wrap_with_modules _loc (<:expr< $lid:("bin_size_"^lid)$ >>) modules$ v in
        let buf = Bin_prot.Common.create_buf size in
        let s = Bytes.create size in
        do {
          ignore ($wrap_with_modules _loc (<:expr< $lid:("bin_write_"^lid)$ >>) modules$ buf v ~pos:0) ;
          Bin_prot.Common.blit_buf_string buf s size ;
          Ys_persistency.Ocsipersist.put db $wrap_field _loc field$ s }
       >>

  | _ -> assert false


let rec generate_get_functions _loc required builders = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    <:str_item<
      $generate_get_functions _loc required builders tp1$;
      $generate_get_functions _loc required builders tp2$;
    >>

  | <:ctyp< uid : $tp$ >> -> <:str_item< >>

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : $tp$ >> ->
    let modules, lid = split_modules_ctyp field tp in
    <:str_item<
      (* we could factorize some code & call the raw_ functions, but then it
         breaks the (desirable) possibility to call some of the get__ inside
         some of the builders *)
      value rec $lid:"get_"^field$ uid =
         Lwt.bind
          (Ys_persistency.Ocsipersist.get db $wrap_field _loc field$)
          (fun
            [ None ->
                  $try
                     let builder = List.assoc field builders in
                     <:expr< $builder$ uid >>
                   with Not_found ->
                     match List.mem field required with
                        true ->
                         <:expr< let _ = Lwt_log.ign_info_f "field %s is missing for object %d" $str:field$ uid in raise (Required_field_is_missing (uid, $str:field$)) >>
                      | false ->
                        <:expr< Lwt.return $wrap_with_modules _loc (<:expr< $lid:"default_" ^ lid$ >>) modules$ >> $
             | Some data ->
                 let len = String.length data in
                 let buf = Bin_prot.Common.create_buf len in
                 do {
                   Bin_prot.Common.blit_string_buf data buf ~len;
                   Lwt.return ($ <:expr< $ wrap_with_modules _loc (<:expr< $lid:("bin_read_"^lid)$ >>) modules $ buf ~pos_ref:(ref 0) >> $) }
           ])

       >>

  | _ -> assert false


let rec generate_patch_functions _loc required alias_mapper uniques builders = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    <:str_item<
      $generate_patch_functions _loc required alias_mapper uniques builders tp1$;
      $generate_patch_functions _loc required alias_mapper uniques builders tp2$;
    >>

  | <:ctyp< uid : $tp$ >> -> <:str_item< >>

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : edges $t$ >> ->
    <:str_item<

      (* when patching a field, we expect some kind of lock. it's unclear how we
         could create one lock per customer without paying a large penalty, so
         as a first go, we create one lock per field. *)

    value $lid:"lock_"^field$ = Lwt_mutex.create () ;

    value $lid:"patch_"^field$ uid patch =
        Lwt_mutex.with_lock
          $lid:"lock_"^field$
          (fun () ->
            Lwt.bind
               ($lid:"get_"^field$ uid)
               (fun v_old ->
                   $match assoc field alias_mapper with
                     | `PlainTextEdge ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                           ($lid:"remove_"^field$ uid v_old)
                           (fun () ->
                             Lwt.bind
                               (Unsafe.$lid:"set_"^field$ uid v_new)
                               (fun () ->
                                  Lwt.bind
                                    ($lid:"insert_"^field$ uid v_new)
                                    (fun () -> Lwt.return v_new) ))  >>
                     | _ ->
                       <:expr< let v_new = patch v_old in
                               Lwt.bind
                                 (Unsafe.$lid:"set_"^field$ uid v_new)
                                 (fun () -> Lwt.return v_new) >> $)) ;

    value $lid:"patch_lwt_"^field$ uid patch =
        Lwt_mutex.with_lock
          $lid:"lock_"^field$
          (fun () ->
            Lwt.bind
               ($lid:"get_"^field$ uid)
               (fun v_old ->
                  Lwt.bind
                     (patch v_old)
                     (fun v_new ->
                        Lwt.bind
                          (Unsafe.$lid:"set_"^field$ uid v_new)
                          (fun () -> Lwt.return v_new )))) ;

     value $lid:"attach_to_"^field$ uid e2 =
                $lid:"patch_"^field$ uid (fun res -> [ e2 :: res ]) ;

     value $lid:"detach_from_"^field$ uid e2 =
                $lid:"patch_"^field$ uid (fun res -> Ys_uid.Edges.remove e2 res) ;

     value $lid:"attach_to_"^field^"_check_unicity"$ uid e2 =
                $lid:"patch_"^field$ uid (fun res -> match Ys_uid.Edges.mem (snd e2) res with [ True -> res | False -> [ e2 :: res ]]) ;

(*
      value $lid:"attach_to_"^field^"_check_unicity_all_keep"$ uid e2 =
        Lwt_mutex.with_lock
          $lid:"lock_"^field$
          (fun () ->
            Lwt.bind
               ($lid:"get_"^field$ uid)
               (fun v_old ->
                  do { Unsafe.$lid:"set_"^field$ uid (Ys_uid.Edges.add_unique_all_keep e2 v_old) ; Lwt.return_unit })) ; *)

    >>

  | <:ctyp< $lid:field$ : $tp$ >> ->

    <:str_item<

      (* cf comment above *)

      value $lid:"lock_"^field$ = Lwt_mutex.create () ;

      value $lid:"patch_lwt_"^field$ uid patch =
         $match assoc field alias_mapper with
           | `None ->
                <:expr< Lwt_mutex.with_lock
                          $lid:"lock_"^field$
                           (fun () ->
                                Lwt.bind
                                    ($lid:"get_"^field$ uid)
                                    (fun v_old ->
                                        Lwt.bind
                                            (patch v_old)
                                            (fun v_new ->
                                               Lwt.bind
                                                 (Unsafe.$lid:"set_"^field$ uid v_new)
                                                 (fun () -> Lwt.return v_new )))) >>
           | _ -> <:expr< () >> $ ;

      value $lid:"patch_"^field$ uid patch =
        Lwt_mutex.with_lock
          $lid:"lock_"^field$
          (fun () ->
             Lwt.bind
               ($lid:"get_"^field$ uid)
               (fun v_old ->
                   $match assoc field alias_mapper with
                     | `Spatial ->
                      <:expr<
                       let v_new = patch v_old in
                       do {
                        $lid:"insert_"^field$ uid v_new ;
                        Unsafe.$lid:"set_"^field$ uid v_new }
                      >>
                     | `String when List.mem field uniques ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"is_"^field^"_assigned_to_other"$ uid v_new)
                            (fun
                               [ True -> Lwt.return_false
                               | False ->
                                  Lwt.bind
                                    ($lid:"unset_"^field^"_by_string"$ v_old)
                                    (fun () ->
                                      Lwt.bind
                                        ($lid:"set_"^field^"_by_string"$ uid v_new)
                                        (fun () ->
                                          Lwt.bind
                                            (Unsafe.$lid:"set_"^field$ uid v_new)
                                            (fun () -> Lwt.return_true))) ]) >>
                     | `StringFun constr when List.mem field uniques ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"is_"^field^"_assigned_to_other"$ uid v_new)
                            (fun
                               [ True -> Lwt.return_false
                               | False ->
                                 do {
                                   match $constr$ v_old with
                                    [ Some v_old -> $lid:"unset_"^field^"_by_string"$ v_old
                                    | None -> () ] ;
                                   match $constr$ v_new with
                                    [ Some v_new -> $lid:"set_"^field^"_by_string"$ uid v_new
                                    | None -> () ] ;
                                  Lwt.bind
                                    (Unsafe.$lid:"set_"^field$ uid v_new)
                                    (fun () -> Lwt.return_true) } ]) >>
                     | `String ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"unset_"^field^"_by_string"$ v_old)
                            (fun () ->
                               Lwt.bind
                                 ($lid:"set_"^field^"_by_string"$ uid v_new)
                                 (fun () -> Unsafe.$lid:"set_"^field$ uid v_new)) >>
                     | `Strings when List.mem field uniques ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"is_"^field^"_assigned_to_other"$ uid v_new)
                            (fun
                               [ True -> Lwt.return_false
                               | False ->
                                  Lwt.bind
                                    ($lid:"unset_"^field^"_by_strings"$ v_old)
                                    (fun () ->
                                       Lwt.bind
                                         ($lid:"set_"^field^"_by_strings"$ uid v_new)
                                         (fun () ->
                                           Lwt.bind
                                            (Unsafe.$lid:"set_"^field$ uid v_new)
                                            (fun () -> Lwt.return_true))) ]) >>
                     | `Strings ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"unset_"^field^"_by_strings"$ v_old)
                            (fun () ->
                                Lwt.bind
                                  ($lid:"set_"^field^"_by_strings"$ uid v_new)
                                  (fun () -> Unsafe.$lid:"set_"^field$ uid v_new)) >>
                     | `PlainText ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                           (Unsafe.$lid:"set_"^field$ uid v_new)
                           (fun () -> $lid:"insert_"^field$ uid v_new) >>
                     | `PlainTextEdge ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                           ($lid:"remove_"^field$ uid v_old)
                           (fun () ->
                             do {
                              Lwt_log.ign_info_f "patching an edge with plaintextedges" ;
                              Lwt.bind
                                (Unsafe.$lid:"set_"^field$ uid v_new)
                                (fun () -> $lid:"insert_"^field$ uid v_new) })  >>

                     | `PlainTextAndString when List.mem field uniques ->
                       <:expr<
                         let v_new = patch v_old in
                         Lwt.bind
                            ($lid:"is_"^field^"_assigned_to_other"$ uid v_new)
                            (fun
                               [ True -> Lwt.return_false
                               | False ->
                                  Lwt.bind
                                    ($lid:"unset_"^field^"_by_string"$ v_old)
                                    (fun () ->
                                      Lwt.bind
                                         ($lid:"set_"^field^"_by_string"$ uid v_new)
                                         (fun () ->
                                            Lwt.bind
                                               (Unsafe.$lid:"set_"^field$ uid v_new)
                                               (fun () ->
                                                 Lwt.bind
                                                   ($lid:"insert_"^field$ uid v_new)
                                                   (fun () -> Lwt.return_true)))) ]) >>
                     | `PlainTextAndString ->
                       <:expr<
                          let v_new = patch v_old in
                          Lwt.bind
                             ($lid:"unset_"^field^"_by_string"$ v_old)
                             (fun () ->
                                Lwt.bind
                                  ($lid:"set_"^field^"_by_string"$ uid v_new)
                                  (fun () ->
                                     Lwt.bind
                                       (Unsafe.$lid:"set_"^field$ uid v_new)
                                       (fun () -> $lid:"insert_"^field$ uid v_new))) >>
                     | _ ->
                       <:expr< Unsafe.$lid:"set_"^field$ uid (patch v_old) >> $)) ;

       >>

  | _ -> assert false

let rec generate_raw_functions _loc required builders = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    <:str_item<
      $generate_raw_functions _loc required builders tp1$;
      $generate_raw_functions _loc required builders tp2$;
    >>

  | <:ctyp< uid : $tp$ >> -> <:str_item< >>

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : $tp$ >> ->
    let modules, lid = split_modules_ctyp field tp in
    <:str_item<
      value rec $lid:"raw_"^field$ uid data_option =
         match data_option with
            [ None ->
                  $try
                     let builder = List.assoc field builders in
                     <:expr< $builder$ uid >>
                   with Not_found ->
                      match List.mem field required with
                         | true ->
                           <:expr< let _ = Lwt_log.ign_info_f "field %s is missing for object %d" $str:field$ uid in raise (Required_field_is_missing (uid, $str:field$)) >>
                         | false ->
                           <:expr< Lwt.return $wrap_with_modules _loc (<:expr< $lid:"default_" ^ lid$ >>) modules$ >> $
             | Some data ->

                 let len = String.length data in
                 let buf = Bin_prot.Common.create_buf len in
                 do {
                   Bin_prot.Common.blit_string_buf data buf ~len;
                  Lwt.return ($ <:expr< $ wrap_with_modules _loc (<:expr< $lid:("bin_read_"^lid)$ >>) modules $ buf ~pos_ref:(ref 0) >> $) }
           ]
       >>

  | _ -> assert false

let rec generate_safe_setters _loc alias_mapper = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    <:str_item<
      $generate_safe_setters _loc alias_mapper tp1$;
      $generate_safe_setters _loc alias_mapper tp2$;
    >>

  | <:ctyp< uid : $tp$ >> -> <:str_item< >>

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : $tp$ >> ->
    (match assoc field alias_mapper with
    | `None ->
    <:str_item<
      value $lid:"set_"^field$ = Unsafe.$lid:"set_"^field$ ;
    >>
    | _ -> <:str_item< >>)

  | _ -> assert false

let rec extract_create_patterns _loc required alias_mapper builders = function
  | <:ctyp< $tp1$; $tp2$ >> ->
    let patterns1, setters_lwt1, fields1 = extract_create_patterns _loc required alias_mapper builders tp1 in
    let patterns2, setters_lwt2, fields2 = extract_create_patterns _loc required alias_mapper builders tp2 in
    (patterns1 @ patterns2), (setters_lwt1 @ setters_lwt2), (fields1 @ fields2)

  | <:ctyp< uid : $tp$ >> -> ([], [], [ <:rec_binding< uid >> ])
  | <:ctyp< created_on : $tp$ >> -> ([], [ <:expr< Unsafe.set_created_on uid created_on >> ], [ <:rec_binding< created_on >> ])

  | <:ctyp< $lid:field$ : mutable $tp$ >> ->
    failwith "mutable fields are prohibited"

  | <:ctyp< $lid:field$ : $tp$ >> when List.mem field required ->
    let setter_lwt =
      match assoc field alias_mapper with
      | `Spatial ->
        [ <:expr<
            Lwt.bind
               (Unsafe.$lid:"set_"^field$ uid $lid:field$)
               (fun () -> $lid:"insert_"^field$ uid $lid:field$)

          >> ]
      | `String ->
        [ <:expr<
            Lwt.bind
              (Unsafe.$lid:"set_"^field$ uid $lid:field$)
              (fun () -> $lid:"set_"^field^"_by_string"$ uid $lid:field$)
          >> ]
      | `StringFun constr ->
       [ <:expr<
            Lwt.bind
               (Unsafe.$lid:"set_"^field$ uid $lid:field$)
               (fun () ->
                   match $constr$ $lid:field$ with
                   [ Some v_new -> $lid:"set_"^field^"_by_string"$ uid v_new
                   | None -> Lwt.return_unit ])
         >> ]
      | `Strings ->
        [ <:expr<
            Lwt.bind
                (Unsafe.$lid:"set_"^field$ uid $lid:field$)
                (fun () -> $lid:"set_"^field^"_by_strings"$ uid $lid:field$)
          >> ]
      | `PlainText ->
        [ <:expr<
            Lwt.bind
              (Unsafe.$lid:"set_"^field$ uid $lid:field$)
              (fun () -> $lid:"insert_"^field$ uid $lid:field$)
          >> ]
      | `PlainTextAndString ->
        [ <:expr<
            Lwt_list.join
             [ Unsafe.$lid:"set_"^field$ uid $lid:field$ ;
               $lid:"set_"^field^"_by_string"$ uid $lid:field$ ;
               $lid:"insert_"^field$ uid $lid:field$ ] >>
         ]
        | _ -> [ <:expr< Unsafe.$lid:"set_"^field$ uid $lid:field$ >> ]
    in
      [ <:patt< ~ $lid:field$  >> ],
      setter_lwt,
      [ <:rec_binding< $lid:field$ >> ]

 | <:ctyp< $lid:field$ : $tp$ >> ->
    let modules, lid = split_modules_ctyp field tp in
    let setter_lwt =
      match assoc field alias_mapper with
      | `Spatial ->
        [ <:expr<
            match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v ->
              Lwt.bind
                 (Unsafe.$lid:"set_"^field$ uid v)
                 (fun () -> $lid:"insert_"^field$ uid v) ]
           >> ]
      | `StringFun constr ->
        [ <:expr<
            match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v ->
              Lwt.bind
                  (Unsafe.$lid:"set_"^field$ uid v)
                  (fun () ->
                     match $constr$ v with
                      [ Some v -> $lid:"set_"^field^"_by_string"$ uid v
                      | None -> Lwt.return_unit ])
            ] >> ]
      | `String ->
        [ <:expr<
            match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v ->
            Lwt.bind
              (Unsafe.$lid:"set_"^field$ uid v)
              (fun () -> $lid:"set_"^field^"_by_string"$ uid v) ]
           >> ]
      | `PlainText ->
        [ <:expr<
           match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v ->
              Lwt.bind
                (Unsafe.$lid:"set_"^field$ uid v)
                (fun () -> $lid:"insert_"^field$ uid v)
            ]
        >> ]
      | `PlainTextAndString ->
        [ <:expr<
           match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v ->
            Lwt_list.join
              [ Unsafe.$lid:"set_"^field$ uid v ;
              $lid:"set_"^field^"_by_string"$ uid v ;
              $lid:"insert_"^field$ uid v
            ]] >>
        ]
      | _ ->
          [ <:expr<
            match $lid:field$ with
            [ None -> Lwt.return_unit
            | Some v -> Unsafe.$lid:"set_"^field$ uid v ] >> ]
    in

    [ <:patt< ? ($lid:field$) >> ],
    setter_lwt,
    [ <:rec_binding< $lid:field$ =
       match $lid:field$ with
       [ None -> $wrap_with_modules _loc (<:expr< $lid:"default_" ^ lid$ >>) modules$
       | Some v -> v ] >> ]

  | _ -> assert false


let generate_store _loc type_name tp uniques aliases required builders recursive =
  let load_keys, load_binders, load_builder, _ = extract_keys_and_builder _loc 0 tp in
  let alias_mapper =
    List.map
      (function
        | Spatial field -> (field, `Spatial)
        | String field -> (field, `String)
        | StringFun (field, constr) -> (field, `StringFun constr)
        | PlainTextEdge (field, constr) -> (field, `PlainTextEdge)
        | Strings field -> (field, `Strings)
        | PlainText field -> (field, `PlainText)
        | PlainTextAndString field -> (field, `PlainText)
        | StringAutocomplete field -> (field, `StringAutocomplete))
      aliases
  in
  let aliases = generate_aliases _loc uniques aliases in
  let setters = generate_set_functions _loc alias_mapper builders tp in
  let raw = generate_raw_functions _loc required builders tp in
  let getters = generate_get_functions _loc required builders tp in
  let patchers = generate_patch_functions _loc required alias_mapper uniques builders tp in

  let safe_setters = generate_safe_setters _loc alias_mapper tp in

  let create_patterns, create_setters_lwt, create_fields = extract_create_patterns _loc required alias_mapper builders tp in

  let module_name = get_module_name _loc in
  <:str_item<

    module Store =
    struct

    type key = Ys_uid.uid ;
    type $lid:"value"$ = t ;

    exception Required_field_is_missing of (uid * string) ;

    value db =
        let dir = Ys_config.get_string "db-root-dir" ^ "/" ^ $str:module_name$ in
        Ys_persistency.Ocsipersist.open_db_blocking ~cache_size:(Ys_config.get_int "cache-size") dir ;

    value uid_current = ref None ;
    value uid_mutex = Lwt_mutex.create () ;

    value generate_uid () =
     Lwt_mutex.with_lock
        uid_mutex
        (fun () ->
          Lwt.bind
             (match uid_current.val with
                [ Some uid -> Lwt.return uid
                | None ->
                   Lwt.bind
                     (Ys_persistency.Ocsipersist.max_key db)
                     (fun
                        [ None -> Lwt.return 0
                        | Some uid -> Lwt.return uid ]) ])
             (fun cid ->
                 do { uid_current.val := Some (cid + 1) ;
                      Lwt.return (cid + 1) })) ;

    value iter_lwt f =
          Lwt.bind
             (Lwt.bind
               (Ys_persistency.Ocsipersist.max_key db)
               (fun
                   [ None -> Lwt.return 0
                  | Some max_key -> Lwt.return max_key ]))
          (fun high_uid ->
              let rec iter uid =
                 if uid > high_uid then
                    Lwt.return_unit
                 else
                    Lwt.bind (f uid) (fun () -> iter (uid + 1))
              in iter 1) ;

    value fold_flat_lwt f acc marker cardinal =
        Lwt.bind
           (match marker with
               [ None ->
                   Lwt.bind
                      (Ys_persistency.Ocsipersist.max_key db)
                      (fun
                         [ None -> Lwt.return 0
                         | Some max_key -> Lwt.return max_key ])
                | Some key -> Lwt.return key ])
           (fun high_uid ->
        let rec aux acc uid remaining =
          if (uid < 1) then
             Lwt.return (acc, None)
          else if remaining = 0 then
             Lwt.return (acc, Some uid)
          else
             Lwt.bind
               (f acc uid)
               (fun [ None -> aux acc (uid - 1) remaining
                    | Some acc -> aux acc (uid - 1) (remaining - 1) ])
        in
        aux acc high_uid cardinal) ;

    (* we need to set up aliases early on so that methods are available
       in the setters *)

    $list:aliases$ ;
    $getters$ ;
    $raw$;
    module Unsafe =
    struct
       $setters$ ;
     exception Not_found_unsafe;
     value get uid =
        Lwt.bind
          (Ys_persistency.Ocsipersist.Batch.get db [| $list:load_keys$ |])
          (fun fields ->
            $List.fold_left
               (fun acc (s, f) -> <:expr< Lwt.bind ($s$) (fun $lid:f$ -> $acc$) >>)
               <:expr< Lwt.return { $load_builder$ } >>
               load_binders$) ;

    end ;

    $patchers$ ;
    $safe_setters$;

    value create =
         $List.fold_left
           (fun acc patt -> <:expr< fun $patt$ -> $acc$ >>)
           (<:expr< fun () ->
               $List.fold_left
                 (fun acc unique ->
                   match List.mem unique required with
                   | false ->
                    <:expr<
                     match $lid:unique$ with
                        [ None -> $acc$
                        | Some $lid:"__"^unique$ ->
                            Lwt.bind
                               ($lid:"is_"^unique^"_assigned"$ $lid:"__"^unique$)
                               (fun
                                  [ Some obj -> Lwt.return (`Object_already_exists ($str:unique$, obj))
                                  | None -> $acc$ ]) ]
                    >>
                   | true ->
                    <:expr<
                       Lwt.bind
                        ($lid:"is_"^unique^"_assigned"$ $lid:unique$)
                        (fun
                          [ Some uid -> Lwt.return (`Object_already_exists ($str:unique$, uid))
                          | None -> $acc$ ])
                    >>)
               <:expr<
                 (* there is no lock here, because there is no reason why anyone
                    would be hitting this uid before we return *)
                 Lwt.bind
                   (generate_uid ())
                   (fun uid ->
                 let created_on = Ys_time.now () in

                 (* leveldb calls are asynchronous and hopefully non-blocking, so we
                 can probably avoid using detach & call the setters directly *)

                 (* here we need to apply the recursive fields *)
                 (* the whole code here is POS, would need overhaul at some point *)
                 $List.fold_left
                    (fun acc field ->
                      <:expr<
                        Lwt.bind
                         ($lid:field$ uid)
                         (fun
                            [ Some $lid:field$ -> $acc$
                            | None -> Lwt.return (`Couldnt_create_recursive_field $str:field$) ])
                      >>)
                    (List.fold_left
                      (fun acc s -> <:expr< Lwt.bind ($s$) (fun () -> $acc$) >>)
                      <:expr< do { $list:([ <:expr< let v = { $list:create_fields$ } in Lwt.return (`Object_created v) >> ])$ } >>
                      create_setters_lwt)
                    recursive$)
              >>
              uniques$ >>)
           create_patterns$ ;


    end

  >>




let generate type_name options _loc tp =

  let (aliases, required, uniques, builders, recursive) =
    let opt_to_list = function None -> [] | Some l -> l in
    match options with
      None -> [],[],[],[],[]
    | Some (aliases, required, uniques, builders, recursive) ->
      (opt_to_list aliases, opt_to_list required, opt_to_list uniques,
       opt_to_list builders, opt_to_list recursive)
  in

  <:str_item<

    open Bin_prot.Read;

    $generate_store _loc type_name tp uniques aliases required builders recursive$ ;

  >>


let inject_bin_io_modules_for_edges _loc =
  function
  | <:ctyp< { $list:fields$ } >> ->
    let rec inspect =
      function
      | <:ctyp< $t1$; $t2$ >> ->
        <:str_item<
          $inspect t1$ ;
          $inspect t2$
        >>
      | <:ctyp< $lid:label$ : edges $t$ >> ->

        let tp = TyDcl(_loc, "t", [], <:ctyp<edges $t$>>, []) in
        let bin_write  = Pa_bin_prot2.Generate_bin_write.bin_write false tp  in
        let bin_read   = Pa_bin_prot2.Generate_bin_read.bin_read false tp in
        let type_class = Pa_bin_prot2.Generate_tp_class.bin_tp_class false tp in

        <:str_item<
          module $uid:"Edges_"^label$ =
          struct
             value default_t = [];
             $bin_write$;
             $bin_read$;
             $type_class$;
          end
        >>

      | _ -> <:str_item< >>
    in
    inspect fields

  | _ -> <:str_item< >>


let () =
  add_generator_with_arg
    "vertex"
    options
    (fun options _ -> function
       | TyDcl (_loc, type_name, tps, rhs, _cl) ->
         let generate_nil0 _loc = <:str_item< >> in
         let generate_nil _loc _ = <:str_item< >> in
         let generate_nil2 _loc _ _ = <:str_item< >> in

         <:str_item<
           $inject_bin_io_modules_for_edges _loc rhs$;

          $Gen.switch_tp_def
           ~record:(generate type_name options)
           ~alias:generate_nil
           ~sum:generate_nil
           ~variants:generate_nil
           ~mani:generate_nil2
           ~nil:generate_nil0
           rhs$;

         >>
       | _ -> failwith "type not supported")
