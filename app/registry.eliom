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
  let digest = Digest.to_hex (Digest.string Playbook.automata) in
  let hash = sprintf "%d-%s" Playbook.version Playbook.name (* digest *) in
  match_lwt Object_playbook.Store.find_by_hash hash with
  | Some uid ->
    ign_info_f "registering playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash uid ;
    return uid
  | None ->
    lwt owner = find_or_create_author Playbook.author in
    match_lwt Object_playbook.Store.create
                ~owner
                ~name:Playbook.name
                ~description:Playbook.description
                ~hash
                () with
    | `Object_created playbook ->
      ign_info_f "registering playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash playbook.Object_playbook.uid ;
      return playbook.Object_playbook.uid
    | `Object_already_exists (_, uid) ->
      ign_info_f "registering playbook %s from author %s, hash is %s, uid is %d" Playbook.name Playbook.author hash uid ;
      return uid

let register playbook =
  try_lwt
    lwt uid = find_or_create_playbook playbook in
    Hashtbl.replace playbooks uid playbook ;
    return_unit
  with exn -> ign_error_f ~exn "couldn't register playbook" ; return_unit

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


let _ =
  register (module Bakers) ;
  register (module Demo) ;
  register (module Breadmaking) ;
  register (module Serc_weekly) ;
