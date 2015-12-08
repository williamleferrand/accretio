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

}}

{server{

let send_feedback (feedback, contact_info) =
  lwt session = Sessions.get () in
  Notify.send_feedback session feedback contact_info

let send_feedback = server_function ~name:"send-feedback" Json.t<string * string> send_feedback

}}

{client{

open React
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_dummy
open Ys_react
open Service

open React

let dom () =
  let state, update_state = S.create `Write_feedback in

  let feedback =
    Raw.textarea ~a:[ a_placeholder "How can we improve?" ] (pcdata "")
  in

  let contact_info = input ~a:[ a_placeholder "How can we get in touch?" ] ~input_type:`Text () in

  let send _ =
    match Ys_dom.get_value_textarea feedback, Ys_dom.get_value contact_info with
      "", _ -> Help.warning "Say something :-)"
    | feedback, contact_info ->
      Ys_mixpanel.track "feedback" ~params:[ "feedback", feedback ;
                                             "contact-info", contact_info ] () ;
      ignore_result
        (%send_feedback (feedback, contact_info)
         >>= fun _ -> update_state `Thanks; return_unit)
  in

  let send =
    button
      ~button_type:`Button
      ~a:[ a_onclick send ]
      [ pcdata "Send" ]
  in

  S.l2
    (fun state session ->
       match state with
       | `Write_feedback->
         div ~a:[ a_class [ "box" ]] [
           h2 [ pcdata "Send feedback" ] ;
           div ~a:[ a_class [ "box-section" ]] [
             feedback ;
           ] ;
           (match session with
              Anonymous ->
              div ~a:[ a_class [ "box-section" ]] [
                contact_info ;
              ]
            | Connected _ -> pcdata "") ;
           div ~a:[ a_class [ "box-action" ]] [
             send ;
           ]
         ]
       | `Thanks ->
         div ~a:[ a_class [ "box" ]] [
           h2 [ pcdata "Thanks" ] ;
           div ~a:[ a_class [ "box-section" ]] [
             pcdata "Stay in touch!" ;
           ]
         ])
    state
    Sessions.session

}}
