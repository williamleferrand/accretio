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

  lwt _ = Migration.run () in

  lwt _ = Registry.start () in
(*
   commenting that one out for now, it is causing some kind of fd leak
  let _ = ignore_result (Imap_endpoint.start ()) in
*)

  (* starting notify *)
  Notify.dequeue_batches () ;

  (* initializing some objects *)
  lwt _ =
    let email = "william@accret.io" and password = "cisco1234" in

    lwt salt = fresh_salt () in
    Object_member.Store.create
      ~preferred_email:email
      ~name:"William"
        ~emails:[ email ; "william@themipsfactory.com" ]
        ~rights:Object_member.Admin
        ~authentication:(Password (salt, hash salt password))
        ()
    >>= (function
        | `Object_already_exists _ ->
          Lwt_log.ign_info "looks like the root member already exists" ;
          return_unit
        | `Object_created obj ->
          Notify.send_welcome_message obj.Object_member.uid)
  in

  (* setting the timzeone *)
  CalendarLib.Time_Zone.change (CalendarLib.Time_Zone.Local) ;

  (* activating the crons *)
  ignore_result (Executor.start_cron ()) ;

  let rec use_sphinx () =
    (* Lwt_log.ign_info_f "patching name" ; *)
    lwt _ =
      try_lwt
        Logs.list_all_from_society 21 0L
      with _ -> return []
    in
    lwt _ = Lwt_unix.sleep 1.0 in
    use_sphinx ()
  in
  lwt _ = use_sphinx () in

  return_unit
