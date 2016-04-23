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
       lwt _ = $society(uid)<-inbox = [] in
       lwt _ = $society(uid)<-outbox = [] in
       lwt _ = $society(uid)<-stack = [] in
       lwt _ = $society(uid)<-history = [] in
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
           lwt _ = $society(uid)<-stack = [] in
           return_unit in
       lwt _ =
         try_lwt
           lwt sidecar = $society(uid)->sidecar in
           return_unit
         with _ ->
           Lwt_log.ign_info_f "resetting sidecar for society %d" uid ;
           lwt _ = $society(uid)<-sidecar = [] in
           return_unit in
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
           lwt _ = $playbook(uid)<-thread = thread.Object_thread.uid in
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
          lwt _ = $message(uid)<-transport = Object_message.NoTransport in
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
         lwt _ = $society(uid)<-tombstones = [] in
         return (Some ()))
      ()
      None
      (-1)
in
return_unit


let move_to_ocsipersist () =
  Lwt_log.ign_info "moving all leveldb data to ocsipersist!" ;
  Lwt_list.iter_s
    (fun name ->
       let name = Ys_config.get_string "db-root-dir" ^ "/" ^ name in
       Ys_persistency.Siphon.from_leveldb_to_ocsipersist name)
    [
      "object_image" ;
      "object_member" ;
      "object_member_by_emails" ;
      "object_member_by_recovery_token" ;
      "object_member_by_shunts" ;
      "object_message" ;
      "object_message_by_reference" ;
      "object_payment" ;
      "object_payment_by_shortlink" ;
      "object_playbook" ;
      "object_playbook_by_hash" ;
      "object_society" ;
      "object_society_by_shortlink" ;
      "object_thread" ;
      "object_timer"
    ]

 let move_back_to_leveldb () =
  Lwt_log.ign_info "moving all the ocsipersist data back to leveldb" ;
  Lwt_list.iter_s
    (fun (db, name) ->
       let name = Ys_config.get_string "db-root-dir" ^ "/" ^ name in
       Ys_persistency.Siphon.from_ocsipersist_to_leveldb db name)
    [
      Object_image.Store.db, "object_image" ;
      Object_member.Store.db, "object_member" ;
      Object_member.Store.db_emails, "object_member_by_emails" ;
      Object_member.Store.db_recovery_token, "object_member_by_recovery_token" ;
      Object_member.Store.db_shunts, "object_member_by_shunts" ;
      Object_message.Store.db, "object_message" ;
      Object_message.Store.db_reference, "object_message_by_reference" ;
      Object_payment.Store.db, "object_payment" ;
      Object_payment.Store.db_shortlink, "object_payment_by_shortlink" ;
      Object_playbook.Store.db, "object_playbook" ;
      Object_playbook.Store.db_hash, "object_playbook_by_hash" ;
      Object_society.Store.db, "object_society" ;
      Object_society.Store.db_shortlink, "object_society_by_shortlink" ;
      Object_thread.Store.db, "object_thread" ;
      Object_timer.Store.db, "object_timer" ;
    ]

let reset_plaintext () =
  lwt _ = Object_image.Admin.reset_plaintext () in
  lwt _ = Object_member.Admin.reset_plaintext () in
  lwt _ = Object_message.Admin.reset_plaintext ()  in
  lwt _ = Object_payment.Admin.reset_plaintext ()  in
  lwt _ = Object_playbook.Admin.reset_plaintext ()  in
  lwt _ = Object_society.Admin.reset_plaintext ()  in
  lwt _ = Object_thread.Admin.reset_plaintext ()  in
  lwt _ = Object_timer.Admin.reset_plaintext ()  in
  lwt _ = Object_member.Admin.reset_plaintext ()  in
  return_unit

let run () =
  try_lwt
    (* lwt _ = relink_messages_from_followers  () in *)
    (* lwt _ = move_back_to_leveldb () in *)
    (* lwt _ = reset_plaintext () in *)
    return_unit
  with exn ->
    Lwt_log.ign_error_f ~exn "error caught while running the migrations" ;
    return_unit
