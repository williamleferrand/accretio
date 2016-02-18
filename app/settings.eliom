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
open Sessions

type op =
  | Update_email_notify_batch_emails of bool deriving(Json)

}}

{server{

open Lwt_react
open Ys_authentication

let retrieve_settings () =
  Vault.protected_connected
    (fun session ->
       lwt settings = $member(session.member_uid)->settings in
       return (Some settings))

let retrieve_settings = server_function ~name:"settings-retrieve" Json.t<unit> retrieve_settings

let update_password (old_password, new_password) =
  Vault.protected_connected
    (fun session ->
       if (String.length new_password < 8) then return_none
       else
         lwt authentication = $member(session.Vault.member_uid)->authentication in
         match validate authentication old_password with
         | false -> return (Some false)
         | true ->
           lwt salt = fresh_salt () in
           lwt _ = $member(session.Vault.member_uid)<-authentication = (Password(salt, hash salt new_password)) in
           return (Some true))

let update_password = server_function ~name:"settings-update-password" Json.t<string * string> update_password

let update_name name =
  Vault.protected_connected
    (fun session ->
       lwt _ = $member(session.Vault.member_uid)<-name %% (fun _ -> name) in
       (* Live.notify (Live.Member session.Vault.member_uid) (Live.UpdateName name) ; *)
       return (Some ()))

let update_name = server_function ~name:"settings-update-name" Json.t<string> update_name

let update_batched choice =
  Vault.protected_connected
    (fun session ->
       lwt _ = $member(session.member_uid)<-settings %% (fun settings -> { settings with Object_member.email_notify_batch_emails = choice }) in
       return (Some choice))

let update_batched = server_function ~name:"settings-update-batched" Json.t<bool> update_batched

}}

{client{

open React
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_dummy
open Ys_react
open Service

let builder settings =

  let update_notifications =

    let batched, update_batched = S.create settings.Object_member.email_notify_batch_emails in

    let update_batched choice =
      Authentication.if_connected ~mixpanel:("settings-update-batched", [ "choice", string_of_bool choice ])
        (fun _ ->
           rpc %update_batched choice (fun choice -> Help.warning "Preferences saved" ; update_batched choice))
    in

    let all_toggle =
      let a = [ a_name "all_or_batched";
                a_onclick (fun _ -> update_batched false) ] in
      input
        ~a: ((a_input_type `Radio) :: (if not settings.Object_member.email_notify_batch_emails then a_checked `Checked :: a else a)) () in

    let returning_toggle =
      let a = [ a_name "all_or_batched";
                a_onclick (fun _ -> update_batched true) ] in
      input
        ~a:((a_input_type `Radio) :: (if settings.Object_member.email_notify_batch_emails then a_checked `Checked :: a else a)) () in

    div ~a:[ a_class [ "box" ]] [
      h2 [ pcdata "Email notifications" ] ;
      div ~a:[ a_class [ "box-section" ]] [
        all_toggle ; label [ pcdata "Receive one email by event (low volume)" ]
      ] ;
      div ~a:[ a_class [ "box-section" ]] [
        returning_toggle ; label [ pcdata "Receive daily digests" ]
      ] ;
    ]

  in

  let update_password =

    let old_password = input ~a:[ a_placeholder "Old password" ; a_input_type `Password ] () in
    let new_password = input ~a:[ a_placeholder "New password" ; a_input_type `Password ] () in

    let update _ =
      match Ys_dom.get_value old_password, Ys_dom.get_value new_password with
      | "", _ -> Help.warning "Please enter your current password"
      | _, new_password when String.length new_password < 8 ->
        Help.warning "Passwords must have a least 8 characters"
      | old_password_s, new_password_s ->
        Authentication.if_connected ~mixpanel:("settings-update-password", [])
          (fun _ ->
             rpc %update_password (old_password_s, new_password_s)
                 (function
                   | false ->
                     Ys_mixpanel.track "settings-update-password-mismatch" () ;
                     Help.warning "Please check that your current password is correct"
                   | true ->
                     Ys_dom.set_value new_password "" ;
                     Ys_dom.set_value old_password ""))
    in

    let update_button =
      button
          ~a:[ a_onclick update  ]

          [ pcdata "Update" ]
    in

    div ~a:[ a_class [ "box" ]] [
      h2 [ pcdata "Update password" ] ;
      div ~a:[ a_class [ "box-section" ]] [
        old_password
      ] ;
      div ~a:[ a_class [ "box-section" ]] [
        new_password ;
      ] ;
      div ~a:[ a_class [ "box-action" ]] [
        update_button
      ] ;
    ]

  in

  div ~a:[ a_class [ "settings" ]] [
    update_notifications ;
    update_password

  ]


let dom =
  Template.apply
    (fun () ->
       lwt session = Authentication.get_session () in
       %retrieve_settings ())
    (function
      | None -> div [ pcdata "Please connect first" ]
      | Some settings -> builder settings)


}}
