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

type recovery = AccountSuspended of string | AccountDisabled | EmailSent | ContactSupport

type recovery_result = Recovered of session_connected | InvalidToken

}}

{server{

let request_recovery email =
  try_lwt
    Object_member.Store.find_by_email email
    >>= function
    | None ->
      Lwt_log.ign_info_f "request_recovery for email %s, no member found" email ;
      return EmailSent
    | Some uid ->
      Lwt_log.ign_info_f "request_recovery for email %s, member found" email ;
      match_lwt $member(uid)->state with
      | Object_member.Suspended motive -> return (AccountSuspended motive)
      | Object_member.Ghost -> return ContactSupport
      | Object_member.Archived -> return AccountDisabled
      | Object_member.Active ->
        lwt token = Ys_random.random_string 48 in
        match_lwt $member(uid)<-recovery_token %% (fun _ -> token) with
        | false -> return ContactSupport
        | true ->
          lwt _ = $member(uid)<-recovery_token_expiration_timestamp = (Unix.time () +. Ys_config.get_float "recovery-token-timeout") in
          lwt _ = Notify.send_recovery_message uid token in
          return EmailSent
  with _ -> return EmailSent

let request_recovery = server_function ~name:"request_recovery" Json.t<string> request_recovery

open Ys_authentication

let recover (token, password) =
  try_lwt
    match token with
      "" -> return InvalidToken
    | _ as token ->
      Object_member.Store.find_by_recovery_token token
      >>= function
      | None -> return InvalidToken
      | Some uid ->
        match_lwt $member(uid)->(recovery_token, recovery_token_expiration_timestamp) with
        | (recovery_token, recovery_token_expiration_timestamp) when recovery_token = token
                                                                  && recovery_token_expiration_timestamp >= Unix.time () ->
          (lwt salt = fresh_salt () in
           lwt _ = $member(uid)<-authentication = Password(salt, hash salt password) in
           lwt _ = $member(uid)<-recovery_token_expiration_timestamp = 0.0 in
           lwt session = Sessions.connect uid in
           return (Recovered session))
        | _ -> return InvalidToken
  with _ -> return InvalidToken

let recover = server_function ~name:"recover" Json.t<string * string> recover

}}

{client{

open React
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_dummy
open Ys_react

let dom_request_recovery () =

  Lwt.ignore_result (Authentication.log_off_no_redirect ()) ;

  let state, update_state = S.create `AskingForEmail in

  S.map
    (function
      | `AskingForEmail ->

        let email = input  ~a:[ a_autocomplete `On ; a_placeholder "What is your email?" ] ~input_type:`Text () in

          let request_recovery _ =
            match Ys_dom.get_value email with
            | "" -> update_state `IncorrectInput
            | _ as email when not (Ys_email.is_valid email) -> update_state `IncorrectInput
            | _ as email ->
              Lwt.ignore_result
                (match_lwt %request_recovery email with
                 | AccountSuspended motive -> update_state (`AccountSuspended motive); return_unit
                 | AccountDisabled -> update_state `AccountDisabled; return_unit
                 | ContactSupport -> update_state `ContactSupport; return_unit
                 | EmailSent -> update_state `EmailSent; return_unit)
          in

          div ~a:[ a_class [ "recover"; "box" ]] [
            div ~a:[ a_class [ "box-section" ]] [
              email ;
            ] ;
            div ~a:[ a_class [ "box-action" ]] [
              button
                ~a:[ a_onclick request_recovery ]
                ~button_type:`Button
                [ pcdata "Recover" ]
            ]
          ]

        | `IncorrectInput -> div ~a:[ a_class [ "recover" ; "box" ]] [
            pcdata "Sorry, this is not a valid email"
          ]

        | `AccountSuspended motive -> div ~a:[ a_class [ "recover" ; "box" ]] [
            pcdata "This account has been suspended: " ;
            pcdata motive
          ]

        | `AccountDisabled -> div ~a:[ a_class [ "recover" ; "box" ]] [
            pcdata "This account has been disabled - please contact support at " ;
            Raw.a ~a:[ a_href "mailto:support@accret.io" ] [ pcdata "support@accret.io" ]
          ]

        | `ContactSupport -> div ~a:[ a_class [ "recover" ; "box" ]] [
            pcdata "Please contact support at support@accret.io"
          ]

        | `EmailSent -> div ~a:[ a_class [ "recover" ; "box" ]] [
            pcdata "Check your inbox for recovery instructions"
          ])
      state


let dom_recover token =

  Lwt.ignore_result (Authentication.log_off_no_redirect ()) ;

  let password1 = input  ~a:[ a_autocomplete `On ] ~input_type:`Password () in
  let password2 = input  ~a:[ a_autocomplete `On ] ~input_type:`Password () in

  Help.silent_on_keydown password1 ;
  Help.silent_on_keydown password2 ;

  let recover _ =

    match Ys_dom.get_value password1, Ys_dom.get_value password2 with
    | "", "" ->
      Help.warning "Please select a password"
    | password1, password2 when String.length password1 < 8 ->
      Help.warning "Password should have at least 8 characters"
    | password1, password2 when password1 <> password2 ->
      Help.warning "Both passwords should be identical"
    | password1, _ ->
      Lwt.ignore_result
        (%recover (token, password1)
         >>= function
           | Recovered session ->
             Sessions.connect session ;
             Service.goto Service.Landing ;
             return_unit
           | InvalidToken -> Help.error "This recovery link is invalid"; return_unit)
  in

  S.const
    (div ~a:[ a_class [ "recover" ]] [

        div ~a:[ a_class [ "box" ]] [
          h2 [ pcdata "Welcome back!" ] ;

          div ~a:[ a_class [ "box-section" ]] [
            h3 [ pcdata "Please pick up a new password" ] ;
            password1;
          ] ;

          div ~a:[ a_class [ "box-section" ]] [
            h3 [ pcdata "Please type it again" ] ;
            password2;
          ] ;

          div ~a:[ a_class [ "box-action" ]] [
            button
              ~a:[ a_onclick recover ]
              ~button_type:`Button
              [ pcdata "Recover" ]
          ] ;
        ]

      ])

}}
