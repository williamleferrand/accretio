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

type bundle = {
  uid : uid ;
}

}}

{server{

let retrieve uid =
  Lwt_log.ign_info_f "retrieving society %d" uid ;
  lwt leader = $society(uid)->leader in
  solve_acl
    ACLConnected
    (function
      | session when session.member_uid = leader || session.member_is_admin ->
        (* lwt view = retrieve uid in *)
        return (Some { uid })
      | _ -> return_none)
    (fun _ -> return_none)

let retrieve = server_function ~name:"manage-retrieve" Json.t<int> retrieve

let retrieve_members uid =
  protected_connected
    (fun _ ->
       (* todo, add acl here *)
       lwt members = $society(uid)->members in
       lwt members =
         Lwt_list.fold_left_s
           (fun acc -> function
              | `Member tags, uid ->
                lwt view = View_member.to_view uid in
                return ((view, tags) :: acc)
              | _ -> return acc)
           []
           members
       in
       return (Some members))

let retrieve_members = server_function ~name:"manage-retrieve-members" Json.t<int> retrieve_members

let retrieve_custom uid =
  protected_connected
    (fun _ ->
       lwt playbook, data = $society(uid)->(playbook, data) in
       lwt playbook = $playbook(playbook)->name in
       return (Some (playbook, data)))

let retrieve_custom = server_function ~name:"manage-retrieve-custom" Json.t<int> retrieve_custom
}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

(* this might not be the most efficient way ever, but let's stick to that for now *)

let menu shortlink uid =
  div ~a:[ a_class [ "manage-menu" ]]
    (List.map
       (fun (step, name) ->
          button
            ~a:[ a_button_type `Button ;
                 a_onclick (fun _ ->
                     Service.goto (Service.Manage (shortlink, uid, step))) ]
            [ pcdata name ])
       [
         Service.ManageHome, "Home" ;
         Service.ManageMailboxes, "Mailboxes" ;
         Service.ManageMembers, "Members" ;
         Service.ManageCustom, "Custom"
       ])

let builder_home shortlink =
  function
  | None -> pcdata ""
  | Some bundle ->
    div [
      menu shortlink bundle.uid
    ]

(* the members ****************************************************************)

let builder_members shortlink uid =
  function
  | None -> pcdata ""
  | Some members ->

    let members = RList.init members in

    let format_member (member, tags) =

      let tag_input = input ~a:[ a_input_type `Text ; a_placeholder "Comma separated tags" ; a_value (String.concat "," tags) ]  () in
      let tag_update _ =
        let tags = Ys_dom.get_value tag_input in
        let tags = Regexp.split (Regexp.regexp ",") tags in
        let tags = List.filter (fun s -> s <> "") tags in
        Authentication.if_connected
          (fun _ ->
             rpc %Society_leader.update_member_tags
                 (uid, member.View_member.uid, tags)
                 (RList.update members))
      in
      let tag_update =
        button
          ~a:[ a_button_type `Button ;
               a_onclick tag_update ]
          [ pcdata "Update tags" ]
      in

      let remove _ =
        Authentication.if_connected
          (fun _ ->
             rpc
             %Society_leader.remove_member
                 (uid, member.View_member.uid)
                 (RList.update members))
      in
      let remove =
        button
          ~a:[ a_button_type `Button ;
               a_onclick remove ]
          [ pcdata "Remove" ]
      in

      div ~a:[ a_class [ "member" ; "box" ; ]] [
        div ~a:[ a_class [ "box-section" ]] [
          pcdata member.View_member.name
        ] ;
        div ~a:[ a_class [ "box-section" ]] [
          pcdata member.View_member.email
        ] ;
        div ~a:[ a_class [ "box-section" ]] [
          tag_input ;
        ] ;
        div ~a:[ a_class [ "box-action" ]] [
          tag_update ;
          remove ;
        ] ;
      ]
    in

    let add_member =
      let input_email = input ~a:[ a_input_type `Text ; a_placeholder "Email" ]  () in
      let input_tags = input ~a:[ a_input_type `Text ; a_placeholder "Comma separated tags" ]  () in
      let create _ =
        match Ys_dom.get_value input_email with
          "" -> Help.warning "Please specify a valid email"
        | _ as emails ->
          let tags = Ys_dom.get_value input_tags in
          let tags : string list = Regexp.split (Regexp.regexp ",") tags in
          let tags = List.filter (fun s -> s <> "") tags in
          Authentication.if_connected
            (fun _ ->
               rpc
               %Society_leader.add_members
                   (uid, emails, tags)
                   (fun m ->
                      RList.update members m ;
                      Ys_dom.set_value input_email "" ;
                      Ys_dom.set_value input_tags ""))
      in
      let create =
        button
          ~a:[ a_button_type `Button ;
               a_onclick create ]
          [ pcdata "Create" ]
      in
      div ~a:[ a_class [ "add-member" ; "box" ]] [
        div ~a:[ a_class [ "box-section" ]] [
          input_email
        ] ;
        div ~a:[ a_class [ "box-section" ]] [
          input_tags
        ] ;
        div ~a:[ a_class [ "box-action" ]] [
          create
        ]
      ]
    in
    div ~a:[ a_class [ "manage" ; "manage-members" ]] [
      menu shortlink uid ;
      RList.map_in_div_hd ~a:[ a_class [ "members" ]] add_member format_member members ;
    ]

(* custom *********************************************************************)

let builder_custom shortlink uid = function
  | Some ("Children schoolbus", data) ->
    let data = RList.init data in
    div ~a:[ a_class [ "manage"; "manage-custom" ]] [
      menu shortlink uid ;
      div (Schoolbus_blocks.doms uid data ())
    ]
  | _ -> pcdata "no custom blocks"

(* the routing dom ************************************************************)

let dom shortlink uid =
  function
  | Service.ManageHome ->
    Template.apply %retrieve (builder_home shortlink) uid
  | Service.ManageMembers ->
    Template.apply %retrieve_members (builder_members shortlink uid) uid
  | Service.ManageCustom ->
    Template.apply %retrieve_custom (builder_custom shortlink uid) uid
  | _ -> S.const None

}}
