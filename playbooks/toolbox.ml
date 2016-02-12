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

open Lwt
open Api
open Ys_uid

open Eliom_content.Html5
open Eliom_content.Html5.D

(**
  * extracts emails from the body of a message and return the matching
  * members uids
  *
  *)

let extract_members_from_message context message =
  lwt content = context.get_message_content ~message in
  let members = Ys_email.get_all_emails content in
  lwt members =
    Lwt_list.fold_left_s
      (fun acc email ->
         match_lwt Object_member.Store.find_by_email email with
         | None -> return acc
         | Some uid -> return (UidSet.add uid acc))
      UidSet.empty
      members
  in
  return (UidSet.elements members)


(**
  * adds a "run-id" tag to emails and get it / extracts it
  * from all communications
  *
  *)

let key_run_id = "toolbox-run-id"

let new_run_id () = Ys_time.now ()

let run_id_from_message context message =
  match_lwt context.get_message_data ~message ~key:key_run_id with
    None ->
    context.log_error "couldn't extract run-id from message %d" message ;
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Couldn't extract run_id from message"
        ~content:[ pcdata "You might want to step in" ]
        ()
    in
    return_none
  | Some run_id -> return (Some (Int64.of_string run_id))

let data_run_id run_id =
  [ key_run_id, Int64.to_string run_id ]


(**
  * Format a list of members
  *
  *)

let ul_of_members context members =
  lwt members =
    Lwt_list.map_p
      (fun member ->
         lwt name, email = $member(member)->(name, preferred_email) in
         return (li [ pcdata email ; pcdata " (" ; pcdata name ; pcdata ")" ]))
      members
  in
  return (ul members)


(**
  * make sure that the emails are legit
  *
  *)

let key_acl = "toolbox-acl"
let data_supervisor l =
  (key_acl, "supervisor") :: l

let is_from_supervisor context message =
  match_lwt context.get_message_data ~message ~key:key_acl with
    Some "supervisor" -> return_true
  | _ -> return_false

(**
  * Some helpers around the emails
  *
  *)

let salutations member =
  match_lwt $member(member)->name with
    "" -> return (pcdata "Greetings")
  | _ as name -> return (pcdata ("Dear " ^ name ^ ","))
