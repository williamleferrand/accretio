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
open Ys_uid


let reset_all_boxes () =
  Lwt_log.ign_info_f "resetting all in/out boxes" ;
  Object_society.Store.fold_flat_lwt
    (fun _ uid ->
       $society(uid)<-inbox = [] ;
       $society(uid)<-outbox = [] ;
       $society(uid)<-stack = [] ;
       $society(uid)<-history = [] ;
       Lwt_log.ign_info_f "society %d inboxes / outboxes reset" uid ;
       return_none)
    ()
    None
    (-1)

let reset_all_stacks () =
  Lwt_log.ign_info_f "resetting all stacks if they don't deserialize" ;
  Object_society.Store.fold_flat_lwt
    (fun _ uid ->
       Lwt_log.ign_info_f "checking society %d" uid ;
       lwt _ =
         try_lwt
           lwt stack = $society(uid)->stack in
           return_unit
         with _ ->
           Lwt_log.ign_info_f "resetting stack for society %d" uid ;
           $society(uid)<-stack = [] ; return_unit in
       lwt _ =
         try_lwt
           lwt sidecar = $society(uid)->sidecar in
           return_unit
         with _ ->
           Lwt_log.ign_info_f "resetting sidecar for society %d" uid ;
           $society(uid)<-sidecar = [] ; return_unit in
       return (Some ()))
    ()
    None
    (-1)

let create_playbook_threads () =
  Lwt_log.ign_info_f "create playbook threads" ;
  Object_playbook.Store.fold_flat_lwt
    (fun _ uid ->
       try_lwt
         lwt thread = $playbook(uid)->thread in
         return (Some ())
       with _ ->
         lwt owner, name = $playbook(uid)->(owner, name) in
         match_lwt Object_thread.Store.create
                     ~owner
                     ~subject:name
                     ~context:[ `Playbook, uid ]
                     () with
         | `Object_created thread ->
           $playbook(uid)<-thread = thread.Object_thread.uid ;
           return (Some ()))
      ()
      None
      (-1)

let reset_message_transport () =
  Lwt_log.ign_info_f "resetting message transport" ;
  Object_message.Store.fold_flat_lwt
    (fun _ uid ->
       try_lwt
         lwt _ = $message(uid)->transport in
         return (Some ())
       with _ ->
          $message(uid)<-transport = Object_message.NoTransport ;
          return (Some ()))
    ()
    None
    (-1)

let reattach_payments () =
  Lwt_log.ign_info_f "reattach_payments" ;
  Object_payment.Store.fold_flat_lwt
    (fun _ uid ->
      lwt society = $payment(uid)->society in
      lwt _ = $society(society)<-payments %% (fun payments ->
        let uids = set_of_edges payments in
        let uids = UidSet.add uid uids in
        List.map (fun uid -> (`Payment, uid)) (UidSet.elements uids))
      in
      return (Some ()))
    ()
    None
    (-1)

let load_all_and_check_for_errors () =
  Gc.compact () ;
  lwt _ =
    Object_member.Store.fold_flat_lwt
      (fun _ uid ->
         Lwt_log.ign_info_f "loading member %d" uid ;
         lwt obj = Object_member.Store.Unsafe.get uid in
         return (Some ()))
      ()
      None
      (-1)
  in
  lwt _ =
    Object_message.Store.fold_flat_lwt
      (fun _ uid ->
         Lwt_log.ign_info_f "loading message %d" uid ;
         lwt obj = Object_message.Store.Unsafe.get uid in
         return (Some ()))
      ()
      None
      (-1)
  in
  lwt _ =
    Object_playbook.Store.fold_flat_lwt
      (fun _ uid ->
         Lwt_log.ign_info_f "loading playbook %d" uid ;
         lwt obj = Object_playbook.Store.Unsafe.get uid in
         return (Some ()))
      ()
      None
      (-1)
  in
  lwt _ =
    Object_society.Store.fold_flat_lwt
      (fun _ uid ->
         Lwt_log.ign_info_f "loading society %d" uid ;
         lwt obj = Object_society.Store.Unsafe.get uid in
         return (Some ()))
      ()
      None
      (-1)
  in
  Gc.compact () ;
  Object_member.Store.compact () ;
  Object_message.Store.compact () ;
  Object_playbook.Store.compact () ;
  Object_society.Store.compact () ;
  Gc.compact () ;
  return_unit

let reset_society_tombstones () =
  Lwt_log.ign_info_f "resetting society tombstones" ;
  lwt _ =
    Object_society.Store.fold_flat_lwt
      (fun _ uid ->
         try_lwt
           lwt _ = $society(uid)->tombstones in
           return (Some ())
         with _ ->
         $society(uid)<-tombstones = [] ;
         return (Some ()))
      ()
      None
      (-1)
in
return_unit

let run () =
  lwt _ = create_playbook_threads () in
  lwt _ = reset_message_transport () in
  lwt _ = reset_all_stacks () in

  lwt _ = reattach_payments () in
  lwt _ = load_all_and_check_for_errors () in
  lwt _ = reset_society_tombstones () in
  (* lwt _ = reset_all_boxes () in *)
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
