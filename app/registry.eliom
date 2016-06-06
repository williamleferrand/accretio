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


(* this module owns the runtimes of the playbooks *)

open Printf

open Lwt
open Lwt_log

open Ys_uid

let playbooks : (uid, (module Api.PLAYBOOK)) Hashtbl.t = Hashtbl.create 0

let get uid =
  try
    Hashtbl.find playbooks uid
  with Not_found ->
    Lwt_log.ign_info_f "playbook %d is missing" uid ;
    raise Not_found

let mem uid =
  Hashtbl.mem playbooks uid

let find_or_create_author email =
  match_lwt Object_member.Store.find_by_email email with
  | Some uid -> return uid
  | None ->
    match_lwt Object_member.Store.create
                ~preferred_email:email
                ~emails:[ email ]
                () with
    | `Object_created member -> return member.Object_member.uid
    | `Object_already_exists (_, uid) -> return uid

let find_or_create_playbook playbook =
  let module Playbook = (val playbook : Api.PLAYBOOK) in
  (* let digest = Digest.to_hex (Digest.string Playbook.automata) in *)
  let hash = sprintf "%d-%s" Playbook.version Playbook.name (* digest *) in
  let parameters =
    List.map
        (fun (label, key) ->
           Object_playbook.({ label ; key }))
        Playbook.parameters
  in
 let properties =
    List.map
        (fun (property_name, property_value) ->
           Object_playbook.({ property_name ; property_value }))
        Playbook.properties
  in
  match_lwt Object_playbook.Store.find_by_hash hash with
  | Some uid ->
    ign_info_f "registering existing playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash uid ;
    (* let's update a few fields *)
    lwt _ = $playbook(uid)<-parameters %% (fun _ -> parameters) in
    lwt _ = $playbook(uid)<-properties %% (fun _ -> properties) in
    lwt _ = $playbook(uid)<-name %% (fun _ -> Playbook.name) in
    lwt _ = $playbook(uid)<-description %% (fun _ -> Playbook.description) in
    lwt _ = $playbook(uid)<-tags %% (fun _ -> Playbook.tags) in
    return uid
  | None ->

    lwt owner = find_or_create_author Playbook.author in

    match_lwt Object_thread.Store.create
                ~owner
                ~subject:Playbook.name
                () with
    | `Object_created thread ->
      match_lwt Object_playbook.Store.create
                  ~owner
                  ~name:Playbook.name
                  ~description:Playbook.description
                  ~scope:Ys_scope.Private
                  ~tags:Playbook.tags
                  ~parameters
                  ~properties
                  ~hash
                  ~thread:thread.Object_thread.uid
                () with
      | `Object_created playbook ->
        ign_info_f "registering recreated playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash playbook.Object_playbook.uid ;
        lwt _ = $thread(thread.Object_thread.uid)<-context +=! (`Playbook, playbook.Object_playbook.uid) in
        return playbook.Object_playbook.uid
      | `Object_already_exists (_, uid) ->
        ign_info_f "we might have created an orphan thread, %d" thread.Object_thread.uid ;
        ign_info_f "registering playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash uid ;
        return uid

let register playbook =
  try_lwt
    lwt uid = find_or_create_playbook playbook in
    Hashtbl.replace playbooks uid playbook ;
    return_unit
  with exn -> ign_error_f ~exn "couldn't register playbook" ; return_unit

(*

let load_playbook file =
  try
    Lwt_log.ign_info_f "loading module %s" file ;
    (* Dynlink.prohibit [ "Vault" ] ; *)
    Dynlink.allow_unsafe_modules true ;
    Dynlink.loadfile file ;
    Lwt_log.ign_info_f "module %s loaded" file
  with
    Dynlink.Error error -> Lwt_log.ign_error_f "dynlink error: %s" (Dynlink.error_message error)
  | _ as exn -> Lwt_log.ign_error_f ~exn "dynlink error"

*)

let start () =
  lwt _ = register (module Demo) in
  lwt _ = register (module Breadmaking) in
  lwt _ = register (module Serc_weekly) in
  lwt _ = register (module Children_circle) in
  lwt _ = register (module Dinners_with_friends) in
  lwt _ = register (module Accretio_grooming) in
  lwt _ = register (module Accretio_sanity) in
  lwt _ = register (module Monthly_bbq) in
  lwt _ = register (module Flying_club) in
  lwt _ = register (module Field_trips) in
  lwt _ = register (module Coop_babysitting) in
  lwt _ = register (module Ecd_at_home_preschool) in
  lwt _ = register (module Diy_velo_bambou) in
  lwt _ = register (module Mandarin_circle_time) in
  lwt _ = register (module Children_schoolbus) in
  lwt _ = register (module Children_schoolbus_group) in
  lwt _ = register (module Children_schoolbus_transportation) in
  (* lwt _ = register (module Children_schoolbus_planner) in *)
  lwt _ = register (module Bakery) in
  (* lwt _ = register (module Bay_tours) in *)
  lwt _ = register (module Fieldtrips) in
  return_unit
