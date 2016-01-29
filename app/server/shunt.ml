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

let doesnt_exist shunt =
  match_lwt Object_member.Store.find_by_shunt shunt with
  | None -> return_true
  | Some _ -> return_false

let create uid =
  match_lwt $member(uid)->state with
  | Object_member.Active ->
    (match_lwt Ys_random.random_string_unique doesnt_exist 16 with
       None ->
       Lwt_log.ign_error_f "couldn't create new shunt for member %d" uid ;
       return_none
     | Some shunt ->
       lwt _ = $member(uid)<-shunts %% (fun shunts -> shunt :: shunts) in
       return (Some shunt))
  | _ -> return_none

let url uid url =
  match_lwt create uid with
  | None -> return url
  | Some shunt -> return (url^"?shunt="^shunt)

let apply = function
  | None -> return_unit
  | Some options ->
    try_lwt
      let shunt = List.assoc "shunt" options in
      match_lwt Object_member.Store.find_by_shunt shunt with
      | None ->
        Lwt_log.ign_info_f "someone connected with shunt %s but there is no match" shunt ;
        return_unit
      | Some uid ->
        match_lwt $member(uid)->state with
        | Object_member.Active ->
          Lwt_log.ign_info_f "shunt %s maps to member %d" shunt uid ;
          lwt _ = $member(uid)<-shunts %% (List.filter (fun s -> s <> shunt)) in
          lwt _ = Object_member.Store.unset_shunts_by_strings [ shunt ] in
          lwt _ = Vault.set uid in
          return_unit
        | _ ->
          Lwt_log.ign_info_f "shunt %s maps to member %d but member isn't active" shunt uid ;
          return_unit
    with _ -> return_unit
