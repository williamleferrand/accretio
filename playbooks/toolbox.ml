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
