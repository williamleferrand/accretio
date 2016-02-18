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


{shared{

open Lwt
open Vault

type join_reply =
    JoinSuccess of session_connected
  | JoinSuccessAndRedirect of session_connected * Service.service
  | JoinError

type login_reply =
    LoginSuccess of Vault.session_connected
  | LoginError
  | LoginErrorWithReason of string

}}

{server{

open Ys_authentication
open Object_member

(* we don't have a proper merge on the platform yet - so for users we will do
   it by hand on the invite only *)

let merge uid1 uid2 =
  return_unit

let create_and_join name email password =
  lwt salt = fresh_salt () in
  match_lwt Store.create
    ~preferred_email:email
    ~emails:[ email ]
    ~name
    ~authentication:(Password (salt, hash salt password))
    () with
  | `Object_created member ->
      (* lwt _ = Notify.send_welcome_message member.Object_member.uid in *)
      Lwt_log.ign_info_f "member created, uid is %s" (Ys_uid.to_string member.Object_member.uid) ;
      lwt session = Sessions.connect member.Object_member.uid in
      return (JoinSuccess session)

    | `Object_already_exists (reason, uid) ->
      Lwt_log.ign_info_f
        "create_and_join object already exists, reason is %s, uid is %s"
        reason
        (Ys_uid.to_string uid) ;
      lwt authentication = $member(uid)->authentication in
      match validate authentication password with
      | false ->
        Lwt_log.ign_error_f
          "can't join because an member with the same %s already exists" reason ;
        return JoinError
      | true ->
        (* this would happen if we have a UX fail, ie a member trying to log in
            using the join form *)
        lwt session = Sessions.connect uid in
        return (JoinSuccess session)


(* we need to plug some merging logic here *)
let update_and_join uid name email password =
  lwt salt = fresh_salt () in
  lwt _ = $member(uid)<-preferred_email = email in
  match_lwt $member(uid)<-emails %% (fun emails -> email :: List.filter (fun e -> e <> email) emails) with
  | false ->
    (* is it even possible ?? *)
    Lwt_log.ign_error_f "wops, updating a member with email %s that has already been assigned" email ;
    return JoinError
  | true ->
    lwt _ = $member(uid)<-name %% (fun _ -> name) in
    lwt _ = $member(uid)<-authentication = (Password(salt, hash salt password)) in
    lwt _ = $member(uid)<-state = Active in
    lwt session = Sessions.connect uid in
    return (JoinSuccess session)

let join (name, email, password) =
  Lwt_log.ign_info_f "join request %s %s" name email ;
  match_lwt Store.find_by_email email with
  | None -> create_and_join name email password
  | Some uid ->
    match_lwt $member(uid)->state with
      Ghost -> update_and_join uid name email password
    | _ -> return JoinError

let log_in (email, password) =
  Lwt_log.ign_info_f "login request %s" email ;
  match_lwt Store.find_by_email email with
  | None ->
    Lwt_log.ign_info_f "user not found: %s" email ;
    return LoginError
  | Some uid ->
    Lwt_log.ign_info_f "user found, uid is %s" (Ys_uid.to_string uid) ;
    match_lwt $member(uid)->(state, authentication) with
      Suspended reason, _ -> return (LoginErrorWithReason reason)
    | Archived, _ -> return (LoginErrorWithReason "Account archived, contact support")
    | Active, authentication when validate authentication password ->
      lwt session = Sessions.connect uid in
      return (LoginSuccess session)
    | _ -> return LoginError

let log_off () =
  Lwt_log.ign_info_f "log off request" ;
  Sessions.disconnect ()

let vault_join =
  server_function
    ~name:"vault_join"
    Json.t<string * string * string>
    join

let vault_log_in =
  server_function
    ~name:"vault_log_in"
    Json.t<string * string>
    log_in

let vault_log_off =
  server_function
    ~name:"vault_log_off"
    Json.t<unit>
    log_off

(* this is UNSAFE !!!!!! we need server side validation of the access_token *)
(* also, we could want to store either a fb_id or an access_token *)

let connect_with_facebook (fb_id, access_token, name, email) =
  lwt password = Ys_random.random_string 32 in
  match_lwt Store.find_by_email email with
  | None -> create_and_join name email password
  | Some uid ->
    match_lwt $member(uid)->state with
      Ghost -> update_and_join uid name email password
    | _ ->
      lwt session = Sessions.connect uid in
      return (JoinSuccess session)

let connect_with_facebook = server_function ~name:"authentication-fb" Json.t<string * string * string * string> connect_with_facebook

}}

{client{

open Lwt
open React

open Eliom_parameter
open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_dummy
open Sessions

(* let's define agora as an external service *)

let agora =
  Eliom_service.Unsafe.external_post_service
    ~prefix:""
    ~path:[ "" ]
    ~get_params:unit
    ~post_params:(string "email" ** (string "name" ** string "password"))
    ()

(* some helpers for the rpc's *)

let join = function
  | "", _, _ -> return (`Error "Please enter your name (pseudonyms are fine)")
  | _, "", _ -> return (`Error "Please enter a contact email")
  | _, _, p when String.length p < 8 ->
    return (`Error "Passwords should have at least 8 characters")
  | name, email, _ as info ->
    match_lwt %vault_join info with
    | JoinError -> return (`Error "Invalid Credentials")
    | JoinSuccessAndRedirect (session, service) ->
      Ys_mixpanel.alias session.member_uid ;
      Ys_mixpanel.set_people [ "$email", email ; "$name", name ] ;
      Ys_mixpanel.track "authentication-join-success-and-redirect" () ;
      Sessions.connect session ;
      return (`SuccessAndRedirect (session, service))
    | JoinSuccess session ->
      Ys_mixpanel.alias session.member_uid ;
      Ys_mixpanel.set_people [ "$email", email ; "$name", name ] ;
      Ys_mixpanel.track "authentication-join-success" () ;
      Sessions.connect session ;
      return (`Success session)

let log_in = function
  | "", _ -> return (`Error "Please enter your email")
  | _, p when String.length p < 8 ->
    return (`Error "Passwords should have at least 8 characters")
  | _ as info ->
    match_lwt %vault_log_in info with
    | LoginError ->
      Ys_mixpanel.track "authentication-log-in-error" () ;
      return (`Error "Invalid Credentials")
    | LoginErrorWithReason reason ->
      Ys_mixpanel.track "authentication-log-in-error-with-reason" ~params:[ "reason", reason ] () ;
      return (`Error reason)
    | LoginSuccess session ->
      Ys_mixpanel.track "authentication-log-in-success" () ;
      Ys_mixpanel.set_people [ "$name", session.member_name ] ;
      Sessions.connect session ;
      return (`Success session)

let log_off () =
  lwt _ = %vault_log_off () in
  Sessions.disconnect ();
  Ys_mixpanel.track "authentication-log-off" () ;
  return_unit

let log_off_no_redirect () =
  Ys_mixpanel.track "authentication-log-off-no-redirect" () ;
  Sessions.disconnect () ;
  return_unit

(* the landing connection form, we need that POST thing for 1password .. *)
(*
let connection_box_landing ?(guest=false) ?(cls=[]) () =

  let new_or_returning, update_new_or_returning = S.create `Returning in
  let email = input ~a:[ a_autocomplete `On ]  () in

  ignore (Lwt_js.yield () >>= fun _ -> Ys_dom.focus email ; return_unit);

  let new_toggle = input
      ~input_type:`Radio
      ~a:[ a_name "new_or_returning_landing";
           a_onclick (fun _ -> update_new_or_returning `New) ;
         ] () in

  let returning_toggle = input
      ~input_type:`Radio
      ~a:[ a_name "new_or_returning_landing";
           a_onclick (fun _ -> update_new_or_returning `Returning) ;
           a_checked `Checked
         ] () in

  let _ =
    S.map
      (function
        | `Returning ->
          Ys_dom.uncheck new_toggle ;
          Ys_dom.check returning_toggle
        | `New ->
          Ys_dom.uncheck returning_toggle ;
          Ys_dom.check new_toggle)
      new_or_returning
  in

  let new_or_returning_toggle =
    div ~a:[ a_class [ "authentication-toggle" ]] [
      span ~a:[ a_class [ "authentication-new-member" ];
                a_onclick (fun _ -> update_new_or_returning `New) ] [
        Raw.label [ pcdata "New member" ] ;
        new_toggle ;
      ] ;
      span ~a:[ a_class [ "authentication-returning-member"] ;
                a_onclick (fun _ -> update_new_or_returning `Returning) ] [
        returning_toggle ;
        Raw.label [ pcdata "Returning member" ]
      ]
    ] in

  let recovery_link () =
    Raw.a ~a:[ a_class [ "recovery-link" ] ;
               a_onclick (fun _ ->
                   Popup.close ();
                   Service.goto Service.RequestRecovery) ] [ pcdata "Password recovery" ]
  in

  let guest_link () =
    Raw.a ~a:[ a_class [ "guest-link" ] ;
               a_onclick (fun _ ->
                   Popup.close ();
                   Service.goto Service.Agora) ] [ pcdata "Continue as guest" ]
  in

  let new_user =
    post_form
      ~https:true
      ~service:agora
      (fun (email, (name, password)) ->
         [
           div ~a:[ a_class [ "authentication-field" ]] [
             Raw.label [ pcdata "Email" ] ;
             string_input  ~name:email ()
           ] ;
           div ~a:[ a_class [ "authentication-field";
                              "authentication-field-name" ]] [
             Raw.label [ pcdata_i18n "Name" ] ;
             string_input ~a:[ a_autocomplete `Off ]  ~name:name ()
           ] ;
           div ~a:[ a_class [ "authentication-field" ;
                              "authentication-field-password" ]] [
             Raw.label [ pcdata_i18n "Password"] ;
             string_input ~a:[ a_autocomplete `Off ] ~input_type:`Password ~name:password ()
           ] ;
           div ~a:[ a_class [ "authentication-control"; "clearfix" ]] [
             div ~a:[ a_class [ "left" ]] [
               recovery_link ()
             ] ;
             (if guest then
                div ~a:[ a_class [ "left" ]] [
                  guest_link ()
                ] else pcdata "") ;
             div ~a:[ a_class [ "right" ]] [
               string_input
                 ~input_type:`Submit
                 ~value:"Join"
                 ()
             ]
           ]
         ])
      ()

  in

  let returning_user =
    post_form
      ~https:true
      ~service:agora
      (fun (email, (name, password)) ->
         [
           string_input ~input_type:`Hidden ~name () ;
           div ~a:[ a_class [ "authentication-field" ]] [
             Raw.label [ pcdata "Email" ] ;
             string_input  ~name:email ()
           ] ;
           div ~a:[ a_class [ "authentication-field" ;
                              "authentication-field-password" ]] [
             Raw.label [ pcdata_i18n "Password"] ;
             string_input ~a:[ a_autocomplete `Off ] ~input_type:`Password ~name:password ()
           ] ;
           div ~a:[ a_class [ "authentication-control"; "clearfix" ]] [
             div ~a:[ a_class [ "left" ]] [
               recovery_link ()
             ] ;
             (if guest then
                div ~a:[ a_class [ "left" ]] [
                  guest_link ()
                ] else pcdata "") ;
             div ~a:[ a_class [ "right" ]] [
               string_input
                 ~input_type:`Submit
                 ~value:"Log in"
                 ()
             ]
           ]
         ])
      ()

  in

  div ~a:[ a_class [ "box"; "connection-box" ]] [
    h2 [ pcdata "Welcome in!" ] ;
    new_or_returning_toggle ;
    R.node
      (S.map
         (function
           | `New -> new_user
           | `Returning -> returning_user)
         new_or_returning) ;
  ]
*)

(* the very classic connection box ******************************************************)

let connection_box ~callback ?(cls=[]) () =

  let new_or_returning, update_new_or_returning = S.create `Returning in
  let email = input ~a:[ a_input_type `Text ; a_autocomplete `On ; a_placeholder "Email" ]  () in

  ignore (Lwt_js.yield () >>= fun _ -> Ys_dom.focus email ; return_unit);

  let new_toggle = input
      ~a:[ a_name "new_or_returning";
           a_onclick (fun _ -> update_new_or_returning `New) ;
           a_input_type `Radio
         ] () in

  let returning_toggle = input

      ~a:[ a_name "new_or_returning";
           a_onclick (fun _ -> update_new_or_returning `Returning) ;
           a_checked `Checked ;
           a_input_type `Radio
         ] () in

  let _ =
    S.map
      (function
        | `Returning ->
          Ys_dom.uncheck new_toggle ;
          Ys_dom.check returning_toggle
        | `New ->
          Ys_dom.uncheck returning_toggle ;
          Ys_dom.check new_toggle)
      new_or_returning
  in

  let new_or_returning_toggle =
    div ~a:[ a_class [ "authentication-toggle" ]] [
      span ~a:[ a_class [ "authentication-new-member" ];
                a_onclick (fun _ -> update_new_or_returning `New) ] [
        Raw.label [ pcdata "New" ] ;
        new_toggle ;
      ] ;
      span ~a:[ a_class [ "authentication-returning-member"] ;
                a_onclick (fun _ -> update_new_or_returning `Returning) ] [
        returning_toggle ;
        Raw.label [ pcdata "Returning" ]
      ]
    ] in

  let recovery_link () =
    Raw.a ~a:[ a_class [ "recovery-link" ] ;
               a_onclick (fun _ ->
                   Popup.close ();
                   Service.goto Service.RequestRecovery) ] [ pcdata "Recovery" ]
  in

  let connect_with_facebook () =
    let connect_with_facebook _ =
      lwt sess_opt = Ys_facebook.login ~perms:"email" in
      match sess_opt with
      | None ->
        Ys_mixpanel.track "authentication-fb-failed" () ;
        return_unit
      | Some session ->
        let fb_id =  Js.to_string session##userID in
        let access_token = Js.to_string session##accessToken in
        lwt me = Ys_facebook.get_user "me" in
        let name = me##name in
        let email = me##email in
        match_lwt %connect_with_facebook (fb_id, access_token, name, email) with
        | JoinError ->
          Ys_mixpanel.track "authentication-join-failure-facebook" () ;
          Help.error "Couldn't connect via Facebook"; return_unit
        | JoinSuccess session
        | JoinSuccessAndRedirect (session, _) ->
          Ys_mixpanel.alias session.member_uid ;
          Ys_mixpanel.set_people [ "$email", email ; "$name", name ] ;
          Ys_mixpanel.track "authentication-join-success" () ;
          Ys_mixpanel.track "authentication-join-success-via-facebook" ~params:[ "fb-id", fb_id ] () ;
          Sessions.connect session ;
          Help.silent () ; callback session
    in
    button

      ~a:[ a_onclick (fun _ -> ignore_result (connect_with_facebook ())) ;
           a_class [ "fb-connect" ]] [ pcdata "Connect with Facebook" ]
  in

  let new_user =
    let name = input ~a:[ a_autocomplete `On ; a_placeholder "Name" ]  () in
    let password = input ~a:[ a_autocomplete `On ; a_placeholder "Password" ; a_input_type `Password ] () in

    let join _ =
      let name = Ys_dom.get_value name in
      let email = Ys_dom.get_value email in
      let password = Ys_dom.get_value password in
      ignore_result
        (match_lwt join (name, email, password) with
         | `Error message -> Help.error message; return_unit
         | `Success session ->
           Help.silent ();
           callback session
         | `SuccessAndRedirect (session, service) ->
           Help.silent ();
           callback session)
    in

    Manip.Ev.onkeydown
      password
      (fun ev ->
         match ev##keyCode with
         | code when code = Keycode.return -> join (); true
         | _ -> true) ;


    let join_button =
      button
        ~a:[ a_onclick join ]

        [ pcdata_i18n "Join" ]
    in

    let cancel =
      button
        ~a:[ a_onclick (fun _ -> Popup.close ()) ]

          [ pcdata_i18n "Cancel" ]
    in

    div [
      div ~a:[ a_class [ "box-section" ]] [ name ] ;
      div ~a:[ a_class [ "box-section" ]] [ password ] ;
      div ~a:[ a_class [ "box-action" ; "clearfix" ]] [
        div ~a:[ a_class [ "left" ]] [ recovery_link () ] ;
        div ~a:[ a_class [ "right" ]] [
          cancel; join_button
        ]
      ]
    ] in

  let returning_user =
    let password = input ~a:[ a_autocomplete `On ; a_placeholder "Password" ; a_input_type `Password ] () in

    let log_in _ =
      ignore_result
        (log_in
          (Ys_dom.get_value email,
           Ys_dom.get_value password)
         >>= (function
             | `Error message -> Help.error message; return_unit
             | `Success session -> Help.silent (); callback session))
    in

    Manip.Ev.onkeydown
      password
      (fun ev ->
         match ev##keyCode with
         | code when code = Keycode.return -> log_in (); true
         | _ -> true) ;

    let log_in_button =
        button
          ~a:[ a_onclick log_in ]

          [ pcdata_i18n "Log in" ]
    in

    let cancel =
      button
        ~a:[ a_onclick (fun _ -> Popup.close ()) ]

        [ pcdata_i18n "Cancel" ]
    in

    div
      [
        div ~a:[ a_class [ "box-section" ]] [ password ] ;
        div ~a:[ a_class [ "box-action" ; "clearfix" ]] [
          div ~a:[ a_class [ "left" ]] [ recovery_link () ] ;
          div ~a:[ a_class [ "right" ]] [
            cancel; log_in_button
          ] ;
        ]
      ]
  in

  [
    (* div ~a:[ a_class [ "authentication-box-fb" ]] [
      connect_with_facebook ()
    ] ; *)

    div ~a:[ a_class [ "authentication-box" ]] [
      div ~a:[ a_class [ "authentication-field" ]] [
        email
      ] ;
      new_or_returning_toggle ;
      R.node
        (S.map
           (function
             | `New -> new_user
             | `Returning -> returning_user)
           new_or_returning) ;
    ]
  ]

let show_authentication_popup callback =
  Popup.show ~label:"log-in" ~cls:[ "box"; "connection-box" ]
    (connection_box
       ~callback:(fun session -> lwt _ = callback session in Popup.close_and_yield ()) ())

let if_connected ?mixpanel callback =
  match S.value Sessions.session with
  | Anonymous ->
    (match mixpanel with
       Some (label, params) -> Ys_mixpanel.track (label^"-attempt-disconnected") ~params ()
     | None -> Ys_mixpanel.track "if-connected-anonymous" ()) ;
    show_authentication_popup
      (fun session ->
         (match mixpanel with
            None -> ()
          | Some (label, params) -> Ys_mixpanel.track label ~params ()) ;
         callback session)
  | Connected session ->
    (match mixpanel with
       None -> ()
     | Some (label, params) -> Ys_mixpanel.track label ~params ()) ;
     ignore_result (callback session)

let get_session () =
  match S.value Sessions.session with
  | Connected session -> return session
  | Anonymous ->
    let condition = Lwt_condition.create () in
    show_authentication_popup (fun session -> Lwt_condition.signal condition session ; return_unit) ;
    Lwt_condition.wait condition

let log_off_popup _ =
 let log_off =
    button

      ~a:[ a_onclick
             (fun _ ->
                ignore_result (log_off () >>= fun _ -> Popup.close_and_yield ())) ]
      [ pcdata "Log off" ]
  in

  let cancel =
    button

      ~a:[ a_onclick (fun _ -> Popup.close ()) ]
      [ pcdata "Cancel" ]
  in

  Popup.show ~label:"log-off" ~cls:[ "box" ] [
    div ~a:[ a_class [ "box-section" ]] [
      pcdata "Do you really want to log off?"
    ] ;
    div ~a:[ a_class [ "box-action" ]] [
      cancel ; log_off
    ]
  ]

let connection_status_header () =
  S.map
    (function
      | Anonymous ->
          div ~a:[ a_class [ "left" ; "menu" ; "icon-login" ];
                   a_onclick (fun _ -> show_authentication_popup (fun _ -> Popup.close_and_yield ())) ] [] ;

      | Connected session ->
        Ys_mixpanel.identify session.member_uid ;


        (* let profile =
          match session.member_member.View_member.profile_picture with
            None ->
            div ~a:[ a_class [ "left"; "menu" ; "icon-user" ] ;
                     a_onclick (fun _ -> Service.goto (Service.Profile session.member_uid)) ] [] ;

          | Some image ->
            div ~a:[ a_class [ "left" ; "menu" ; "profile-picture" ] ;
                     a_onclick (fun _ -> Service.goto (Service.Profile session.member_uid)) ] [
              match image.View_image.content with
              | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
              | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()
            ]
        in

        profile *)

        div [
          div ~a:[ a_class [ "left" ; "menu" ; "icon-flow-cascade" ] ;
                   a_onclick (fun _ -> Service.goto Service.Dashboard) ] []
        ])
    Sessions.session

let connection_status_footer () =
  S.map
    (function
      | Anonymous -> pcdata ""
      | Connected session ->
        span [ pcdata " - " ;
               Raw.a ~a:[ a_onclick (fun _ -> ignore_result (log_off ())) ] [ pcdata "Log out" ]
             ])
    Sessions.session

}}
