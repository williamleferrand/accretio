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
open Eliom_parameter
open Vault
open Sessions

let version = Printf.sprintf "%d_%s" Version.version_num Version.version

module App =
  Eliom_registration.App (struct
    let application_name = "mu_" ^ version
  end)

let register_page_no_param ~path ~extract_service () =
  ignore
    (App.register_service
       ~path
       ~get_params:(opt any)
       ~content_type:"text/html"
       (fun options pp ->
          lwt _ = Shunt.apply options in
          lwt session = Sessions.get () in
          lwt service = extract_service session in
          let pipe = Live.pipe in
          let mixpanel_id = Ys_config.get_string "mixpanel-id" in
          let fb_app_id = Ys_config.get_string Ys_config.fb_app_id in
          let stripe_publishable_key = Ys_config.get_string Ys_config.stripe_publishable_key in
          let url = (Ys_config.get_string "url-prefix")^"/"^(String.concat "/" path) in
          ignore {unit{
              Bootstrap.init %service %pipe %session %fb_app_id %stripe_publishable_key
            }} ;
          Lwt.return (Nutshell.common mixpanel_id version url)))

let register_page_no_param_with_title ~path ~extract_service () =
  ignore
    (App.register_service
       ~path
       ~get_params:(opt any)
       ~content_type:"text/html"
       (fun options pp ->
          lwt _ = Shunt.apply options in
          lwt session = Sessions.get () in
          lwt service, title, description = extract_service session in
          let pipe = Live.pipe in
          let mixpanel_id = Ys_config.get_string "mixpanel-id" in
          let fb_app_id = Ys_config.get_string Ys_config.fb_app_id in
          let stripe_publishable_key = Ys_config.get_string Ys_config.stripe_publishable_key in
          let url = (Ys_config.get_string "url-prefix")^"/"^(String.concat "/" path) in
          ignore {unit{
              Bootstrap.init %service %pipe %session %fb_app_id %stripe_publishable_key
            }} ;
          Lwt.return (Nutshell.common ?title ?description mixpanel_id version url)))

let register_page ~path ~get_params ~extract_service () =
  ignore
    (App.register_service
       ~path
       ~get_params:(suffix_prod get_params (opt any))
       ~content_type:"text/html"
       (fun (gp, options) pp ->
          lwt _ = Shunt.apply options in
          lwt session = Sessions.get () in
          let pipe = Live.pipe in
          lwt service = extract_service session gp in
          let mixpanel_id = Ys_config.get_string "mixpanel-id" in
          let fb_app_id = Ys_config.get_string Ys_config.fb_app_id in
          let stripe_publishable_key = Ys_config.get_string Ys_config.stripe_publishable_key in
          let to_and_from = Eliom_parameter.get_to_and_from get_params in
          let url = (Ys_config.get_string "url-prefix")^"/"^(String.concat "/" path) ^ "/" ^ (to_and_from.to_string (gp)) in
          ignore {unit{
              Bootstrap.init %service %pipe %session %fb_app_id %stripe_publishable_key
            }} ;
          Lwt.return (Nutshell.common mixpanel_id version url)))

let register_page_with_title ~path ~get_params ~extract_service () =
  ignore
    (App.register_service
       ~path
       ~get_params:((suffix_prod get_params (opt any)))
       ~content_type:"text/html"
       (fun (gp, options) pp ->
          lwt _ = Shunt.apply options in
          lwt session = Sessions.get () in
          let pipe = Live.pipe in
          lwt service, title, description = extract_service session gp in
          let mixpanel_id = Ys_config.get_string "mixpanel-id" in
          let fb_app_id = Ys_config.get_string Ys_config.fb_app_id in
          let stripe_publishable_key = Ys_config.get_string Ys_config.stripe_publishable_key in
          let to_and_from = Eliom_parameter.get_to_and_from get_params in
          let url = (Ys_config.get_string "url-prefix")^"/"^(String.concat "/" path) ^ "/" ^ (to_and_from.to_string (gp)) in
          ignore {unit{
              Bootstrap.init %service %pipe %session %fb_app_id %stripe_publishable_key
            }} ;
          Lwt.return (Nutshell.common ?title ?description mixpanel_id version url)))

let update_invite_code =
  function
  | None -> return_unit
  | Some invite_code -> Sessions.set_invite_code invite_code

let _ =
  let () = Ocsigen_config.set_maxrequestbodysizeinmemory 10000000 in
  ignore_result (Bootstrap.run ()) ;

  Lwt_log.ign_info "Starting Mu" ;

  register_page_no_param
    ~path:[ "" ]
    ~extract_service:(function
        | Anonymous -> return Service.Landing
        | Connected _ -> return Service.Landing)
    () ;
  (* new services *)
  register_page
    ~path:[ "admin" ; "graph" ; "member"]
    ~get_params:(opt (int "uid"))
    ~extract_service:(fun _ uid_option -> return (Service.AdminGraphMember uid_option))
    () ;
  register_page
    ~path:[ "admin" ; "graph" ; "thread"]
    ~get_params:(opt (int "uid"))
    ~extract_service:(fun _ uid_option -> return (Service.AdminGraphThread uid_option))
    () ;
  register_page_no_param
    ~path:[ "admin" ; "stats" ]
    ~extract_service:(fun _ -> return Service.AdminStats)
    () ;
  register_page_no_param
    ~path:[ "admin" ; "i18n" ]
    ~extract_service:(fun _ -> return Service.AdminI18n)
    () ;
  register_page_no_param
    ~path:[ "settings" ]
    ~extract_service:(fun _ -> return Service.Settings)
    () ;
  register_page_with_title
    ~path:[ "member" ]
    ~get_params:(int "uid")
    ~extract_service:(fun _ uid ->
        try_lwt
          lwt name = $member(uid)->name in
          return (Service.Member uid, Some name, None)
        with _ -> return (Service.Landing, None, None))
   () ;
  register_page
    ~path:[ "recover" ]
    ~get_params:(string "token")
    ~extract_service:(fun _ token -> return (Service.Recover token))
    () ;
  register_page
    ~path:[ "search" ]
    ~get_params:(opt (string "query"))
    ~extract_service:(fun _ query -> return (Service.Search query))
    () ;
  register_page_no_param
    ~path:[ "request_recovery" ]
    ~extract_service:(fun _ -> return Service.RequestRecovery)
    () ;
  register_page_no_param
    ~path:[ "feedback" ]
    ~extract_service:(fun _ -> return Service.Feedback)
    () ;
  register_page_no_param_with_title
    ~path:[ "schoolbus" ]
    ~extract_service:(fun _ -> return (Service.Schoolbus, Some "Preschool on wheels", Some "We pick up families at their doorstep and drive them to exciting activities all through the town"))
    () ;
  register_page
    ~path:[ "playbook" ]
    ~get_params:(int "uid")
    ~extract_service:(fun _ uid -> return (Service.Playbook uid))
    () ;
  register_page_with_title
    ~path:[ "society" ]
    ~get_params:(string "shortlink")
    ~extract_service:(fun _ shortlink ->
        match_lwt Object_society.Store.find_by_shortlink shortlink with
          | None ->
            (try_lwt
               let uid = int_of_string shortlink in
               lwt shortlink = $society(uid)->shortlink in
               return (Service.Society (shortlink, uid), None, None)
             with _ -> return (Service.Landing, None, None))
          | Some uid ->
            lwt name, description = $society(uid)->(name, description) in
            return (Service.Society (shortlink, uid), Some name, Some description))
    () ;
let manage path step =
  register_page_with_title
    ~path
    ~get_params:(string "shortlink")
    ~extract_service:(fun _ shortlink ->
        match_lwt Object_society.Store.find_by_shortlink shortlink with
          | None ->
            (try_lwt
               let uid = int_of_string shortlink in
               lwt shortlink = $society(uid)->shortlink in
               return (Service.Manage (shortlink, uid, step), None, None)
             with _ -> return (Service.Landing, None, None))
          | Some uid ->
            lwt name, description = $society(uid)->(name, description) in
            return (Service.Manage (shortlink, uid, step), Some name, Some description))
    () in

  manage [ "manage" ] Service.ManageHome ;
  manage [ "manage" ; "mailboxes" ] Service.ManageMailboxes ;
  manage [ "manage" ; "members" ] Service.ManageMembers ;
  manage [ "manage" ; "custom" ] Service.ManageCustom ;

  register_page_no_param
    ~path:[ "create" ]
    ~extract_service:(fun _ -> return Service.Create)
    () ;
  register_page_no_param
    ~path:[ "dashboard" ]
    ~extract_service:(function
        | Anonymous -> return Service.Landing
        | Connected _ -> return Service.Dashboard)
    () ;
  register_page_no_param
    ~path:[ "library" ]
    ~extract_service:(fun _ -> return Service.Library)
    () ;
  register_page
    ~path:[ "payment" ]
    ~get_params:(string "shortlink")
    ~extract_service:(fun _ shortlink ->
        match_lwt Object_payment.Store.find_by_shortlink shortlink with
        | None -> return Service.Landing
        | Some uid -> return (Service.Payment (shortlink, uid)))
    () ;

ignore
  (Eliom_registration.Streamlist.register_service
     ~path:[ "graph" ]
     ~get_params:(suffix_prod (int "uid") (opt any))
     ~content_type:"image/png"
     (fun (uid, options) _ ->
        let playbook = Registry.get uid in
        let module Playbook = (val playbook : Api.PLAYBOOK) in
        lwt result = Lwt_process.pmap ("dot", [| "dot" ; "-Tpng" |]) Playbook.automata in
        let result = Ocsigen_stream.of_string result in
       return ([ fun _ -> return result ], "")
    )) ;

Lwt_log.ign_info "Mu has started"
