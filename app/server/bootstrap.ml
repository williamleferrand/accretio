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
open Ys_authentication

let run () =
  (* setup the logger *)
  let template = "$(date): $(level) $(section) $(loc-file): $(message)"in
  lwt _ = Ys_config.if_prod
    (fun () ->
       lwt logger = Lwt_log.file ~template ~file_name:(Ys_config.get_string "log-file") () in
       Lwt_log.default := logger ;
       return_unit)
    (fun () ->
       let logger = Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout () in
       Lwt_log.default := logger ;
       return_unit) in


  Lwt_log.append_rule "main" Lwt_log.Info ;
  (* Lwt_log.append_rule "*" Lwt_log.Info ; *)

  (* running all migrations *)
  lwt _ = Migration.run () in

  (* initializing some objects *)
  lwt _ =
    let email = "william@accret.io" and password = "cisco1234" in

    match Object_member.Store.check_uid 1 with
    | false ->
      Lwt_log.ign_info "original member doesn't exist" ;
      lwt salt = fresh_salt () in
      Object_member.Store.create
        ~preferred_email:email
        ~name:"William"
        ~emails:[ email ; "william@themipsfactory.com" ]
        ~rights:Object_member.Admin
        ~authentication:(Password (salt, hash salt password))
        ()
       >>= (function
           | `Object_already_exists _ -> return_unit
           | `Object_created obj -> Notify.send_welcome_message obj.Object_member.uid)
     | true ->
       Lwt_log.ign_info "original member does exist" ;
       $member(1)<-rights = Object_member.Admin ;
       lwt _ = $member(1)<-emails %% (fun _ -> [ email ; "warnegia@gmail.com" ; "william@themipsfactory.com" ]) in

       return_unit
  in

  Lwt_log.ign_info_f "let's find warnegia" ;

  (* reconnecting warnegia *)
  lwt _ =
    let warnegia = "warnegia@gmail.com" in
    match_lwt Object_member.Store.find_by_email warnegia with
      None ->
      Lwt_log.ign_info_f "reconnecting warnegia" ;
      lwt _ = $member(1)<-emails %% (fun emails -> warnegia :: emails) in
      return_unit
    | Some 1 ->
      Lwt_log.ign_info_f "warnegia exists" ;
      return_unit
    | Some uid ->
      Lwt_log.ign_info_f "reconnecting warnegia after disconnecting from %d" uid ;
      $member(uid)<-preferred_email = "accretio+warnegia@gmail.com" ;
      lwt _ = $member(uid)<-emails %% (fun _ -> [ "accretio+warnegia@gmail.com" ]) in
      lwt _ = $member(1)<-emails %% (fun emails -> warnegia :: emails) in
      return_unit
  in

  (* creating a dummy payment *)


  (* setting the timzeone *)
  (* CalendarLib.Time_Zone.change (CalendarLib.Time_Zone.Local) ; *)

(* activating the crons *)

  Executor.start_cron ()
