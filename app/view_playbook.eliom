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
open Sessions
open Ys_uid
open Vault

type scope = Public | Private

type parameter =
  {
    label : string ;
    key : string ;
  }

type property =
  {
    property_name : string ;
    property_value : string ;
  }

type t =
  {
    uid : uid ;
    owner : View_member.t ;
    name : string ;
    description : string ;
    hash : string ;
    scope : scope ;
    parameters : parameter list ;
    properties : property list ;
  }

}}

{server{

let to_view uid =
  lwt owner, name, description, hash, scope, parameters, properties = $playbook(uid)->(owner, name, description, hash, scope, parameters, properties) in
  lwt owner = View_member.to_view owner in
  let scope =
    match scope with
      Ys_scope.Public -> Public
    | Ys_scope.Private -> Private
  in
  let parameters =
    List.map
      (fun parameter -> { label = parameter.Object_playbook.label ; key = parameter.Object_playbook.key })
      parameters
  in
  let properties =
    List.map
      (fun property -> { property_name = property.Object_playbook.property_name ;
                         property_value = property.Object_playbook.property_value })
      properties
  in
  return {
    uid ;
    owner ;
    name ;
    description ;
    hash ;
    scope ;
    parameters ;
    properties ;
  }

let get_or_create email =
  match_lwt Object_member.Store.find_by_email email with
  | None ->
    (match_lwt Object_member.Store.create
                 ~preferred_email:email
                 ~emails:[ email ]
                 () with
      `Object_created obj ->
      return obj.Object_member.uid
    | `Object_already_exists (_, uid) ->
      return uid)
  | Some uid -> return uid

let keep_me_posted (uid, email) =
  Lwt_log.ign_info_f "keep me posted received for playbook %d, email is %s" uid email ;
  lwt follower = get_or_create email in
  lwt _ = $playbook(uid)<-followers +=! (`Follower, follower) in
  lwt _ = Notify.new_interest_for_playbook uid email in
  return (Some ())

let keep_me_posted = server_function ~name:"view-playbook-keep-me-posted" Json.t<int * string> keep_me_posted

let invite_all (uid, email, emails) =
  Lwt_log.ign_info_f "invite all received for playbook %d, from %s, emails are %s" uid email emails ;
  lwt host = get_or_create email in
  let guests = Ys_email.get_all_emails emails in
  lwt guests =
    Lwt_list.fold_left_s
      (fun acc email ->
         lwt uid = get_or_create email in
         return (UidSet.add uid acc))
      UidSet.empty
      guests
  in
  let guests = UidSet.elements guests in
  let invite = Object_playbook.({ host; guests }) in
  lwt _ = $playbook(uid)<-invites %% (fun invites -> invite :: invites) in
  return (Some ())

let invite_all = server_function ~name:"view-playbook-invite-all" Json.t<int * string * string> invite_all

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  let state, update_state = S.create `Closed in
  div ~a:[ a_class [ "playbook" ; "view-playbook" ] ;
          (* a_onclick (fun _ -> Service.goto (Service.Playbook view.uid)) *) ] [
    div ~a:[ a_class [ "name" ]] [ pcdata view.name ] ;
    div ~a:[ a_class [ "description" ]] [ pcdata view.description ] ;
    (match view.properties with
       [] -> pcdata ""
     | _ as properties ->
       div ~a:[ a_class [ "properties" ]] [
         ul (List.map (fun property -> li [ h3 [ pcdata property.property_name ] ;
                                            pcdata property.property_value ]) properties)
    ]) ;
    div ~a:[ a_class [ "actions" ]] [
      R.node
        (S.map
           (function
             | `Closed ->
               let interested =
                 button
                   ~a:[ a_button_type `Button ;
                        a_onclick (fun _ -> update_state `Interested) ]
                   [ pcdata "I'm interested in participating" ]
               in
               div ~a:[ a_class [ "interested" ]] [
                 interested
               ]
             | `Interested ->
               let email = input ~a:[ a_input_type `Text ; a_placeholder "Awesome - what is your email address?" ] () in
               let keep_me_posted _ =
                 match Ys_dom.get_value email with
                   "" -> Help.warning "Please enter a valid email address"
                 | _ as email ->
                   match Ys_email.is_valid email with
                   | false -> Help.warning "Please enter a valid email address"
                   | true ->
                   detach_rpc %keep_me_posted (view.uid, email) (fun _ -> update_state (`Invite email))
               in
               let keep_me_posted =
                 button
                   ~a:[ a_button_type `Button ;
                        a_onclick keep_me_posted ]
                   [ pcdata "Keep me posted" ]
               in
               div ~a:[ a_class [ "box" ]] [
                 div ~a:[ a_class [ "box-section" ]] [ email ] ;
                 div ~a:[ a_class [ "box-action" ]] [ keep_me_posted ] ;
               ]
             | `Invite email ->
               let invites =
                 Raw.textarea ~a:[ a_placeholder "Thanks. The more the merrier - do you want to invite people by email?" ] (pcdata "")
               in
               let close =
                 button
                   ~a:[ a_button_type `Button ;
                        a_onclick (fun _ -> update_state `Done) ]
                   [ pcdata "Close" ]
               in
               let send _ =
                 let emails = Ys_dom.get_value_textarea invites in
                 detach_rpc %invite_all (view.uid, email, emails) (fun _ -> update_state `Done)
               in
               let send =
                 button ~a:[ a_button_type `Button ;
                             a_onclick send ] [ pcdata "Send invites" ]
               in
               div ~a:[ a_class [ "box" ]] [
                 div ~a:[ a_class [ "box-section" ]] [ invites ] ;
                 div ~a:[ a_class [ "box-action" ]] [
                   close ;
                   send ;
                 ] ;
               ]
             | `Done ->
               div ~a:[ a_class [ "done" ]] [
                 pcdata "We will be in touch!"
               ])

           state
        )
    ] ;
  ]

}}
