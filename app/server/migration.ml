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


(* this is a super band aid module - it should just go away *)

open Lwt


let reset_all_boxes () =
  Lwt_log.ign_info_f "resetting all in/out boxes" ;
  Object_society.Store.fold_flat_lwt
    (fun _ uid ->
       $society(uid)<-inbox = [] ;
       $society(uid)<-outbox = [] ;
       return_none)
    ()
    None
    (-1)

let run () =
  lwt _ = reset_all_boxes () in
  (* reset_all_cohorts () ; *)
  (* lwt _ = relink_all_transitions () in
     lwt _ = inspect_parents () in
     lwt _ = insert_owner_in_parent_cohort () in
     lwt _ = archive_cohorts () in
     lwt _ = update_all_pools () in *)
  (* lwt _ = all_members_locked_are_ghosts () in *)
  (* lwt _ = touch_all_names () in *)
  (* lwt _ = recompute_followers_for_thoughts () in *)
  return_unit
