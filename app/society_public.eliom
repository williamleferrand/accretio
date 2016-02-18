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
open Ys_uid
open Vault
open View_society

}}

{server{

open Vault

let request_to_join (society, email, content) =
  Lwt_log.ign_info_f "request to join society %d from email %s" society email ;
  (* we assume here that the playbook on the other side has the core_join_request component loaded.
     if it doesn't, the message will still reach the inbox but won't be dispatched *)
  (* the stage name is hardcoded there, changing it in the playbook will break this feature *)
  lwt sender =
    match_lwt Object_member.Store.find_by_email email with
    | Some uid -> return uid
    | None ->
      match_lwt Object_member.Store.create
                  ~preferred_email:email
                  ~emails:[ email ]
                  ~name:""
                  ~state:Object_member.Ghost
                  () with
      | `Object_already_exists (_, uid) -> return uid
      | `Object_created member -> return member.Object_member.uid
  in
  match_lwt Object_message.Store.create
              ~society
              ~subject:"Request to join"
              ~transport:Object_message.NoTransport
              ~content
              ~origin:(Object_message.Member sender)
              ~destination:(Object_message.Stage "process_join_request")
              ~reference:(Object_message.create_reference content)
              () with
  | `Object_already_exists _ -> return_none
  | `Object_created message ->
    lwt _ = $member(sender)<-messages += (`Email, message.Object_message.uid) in
    lwt _ = Executor.stack_and_trigger_int society "process_join_request" message.Object_message.uid in
    return (Some ())

let request_to_join = server_function ~name:"society-public-request-to-join" Json.t<int * string * string> request_to_join

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_society

let dom view =

  let join =
    let email_input = input  ~a:[ a_input_type `Text ; a_placeholder "Email" ] () in
    let optional_message =
      Raw.textarea ~a:[ a_placeholder "Optional message for the group leader" ] (pcdata "")
    in
    let join _ =
      let email = Ys_dom.get_value email_input in
      match Ys_email.is_valid email with
        false -> Help.warning "Please enter a valid email"
      | true ->
        Ys_mixpanel.track "request-to-join" ~params:[ "society", Ys_uid.to_string view.uid ;
                                                      "email", email ;
                                                      "message", Ys_dom.get_value_textarea optional_message ] () ;
        detach_rpc
          %request_to_join
          (view.uid, email, Ys_dom.get_value_textarea optional_message)
          (fun _ ->
             Ys_dom.set_value email_input "" ;
             Ys_dom.set_value_textarea optional_message "" ;
             Help.warning "Request sent - you'll get an email soon!")
    in

    let join =
      button

        ~a:[ a_button_type `Button ; a_onclick join ]
        [ pcdata "Request to join" ]
    in

    div ~a:[ a_class [ "society-public-join" ]] [
      h2 [ pcdata "Request to join" ] ;
      div ~a:[ a_class [ "box" ]] [
        div ~a:[ a_class [ "box-section" ]] [ email_input ] ;
        div ~a:[ a_class [ "box-section" ]] [ optional_message ] ;
        div ~a:[ a_class [ "box-action" ]] [ join ] ;
      ]
    ]
  in

  let details =
    div ~a:[ a_class [ "society-public-details" ]] [
      h2 [ pcdata "Details" ] ;
      div ~a:[ a_class [ "leader" ]] [
        span ~a:[ a_class [ "icon-user" ]] [] ;
        pcdata "The supervisor for this group is " ; View_member.format view.supervisor  ;
      ] ;
      div ~a:[ a_class [ "playbook" ]] [
        span ~a:[ a_class [ "icon-lamp" ]] [] ;
        pcdata "This group follows the " ;
        Raw.a ~a:[ a_onclick (fun _ -> Service.goto (Service.Playbook view.playbook.View_playbook.uid)) ] [ pcdata view.playbook.View_playbook.name ] ;
        pcdata " playbook" ;
      ] ;
    ]
  in

  div ~a:[ a_class [ "society-public" ]] [

    h1 [ pcdata view.name ] ;

    div ~a:[ a_class [ "society-public-description" ]] [
      h2 [ pcdata "Purpose" ] ;
      div ~a:[ a_class [ "box" ]] [
        pcdata view.description ;
      ] ;
    ] ;

    join ;
    details ;
  ]

}}
