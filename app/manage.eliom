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

let retrieve_societies uid =
  protected_connected
    (fun _ ->
       lwt societies = $society(uid)->societies in
       lwt societies = Lwt_list.map_s (fun (_, uid) -> View_society.to_view uid) societies in
       return (Some societies))

let retrieve_societies = server_function ~name:"manage-retrieve-societies" Json.t<int> retrieve_societies
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
         Service.ManageCustom, "Custom" ;
         Service.ManageSocieties, "Societies"
       ])

let builder_home shortlink =
  function
  | None -> pcdata ""
  | Some bundle ->
    div ~a:[ a_class [ "manage" ]] [
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

(* societies ******************************************************************)

let builder_societies shortlink uid = function
    None -> pcdata ""
  | Some societies ->
    let societies = RList.init societies in
    let add_society =
      (* let playbook = input ~a:[ a_input_type `Text  ; a_placeholder "Playbook" ] () in *)
      let description = Raw.textarea ~a:[ a_placeholder "Description" ] (pcdata "") in

      let selection_playbook, update_selection_playbook = S.create (`String "") in
      let selection_society, update_selection_society = S.create (`String "") in

      let select_playbook = function
          `String name ->
          Help.warning (Printf.sprintf "setting playbook name %s" name) ;
          update_selection_playbook (`String name)
        | `Elt playbook ->
          Help.warning "setting a playbook" ;
          update_selection_playbook (`Elt playbook) ;
          (* we would need to reset the society here?? *)

      in

      let finalize_playbook, playbook =
        Autocomplete.create_playbook ~placeholder:"Playbook" View_playbook.format_autocomplete (fun i -> i) select_playbook
      in

      let select_society =
        function
        | `String name ->
          update_selection_society (`String name)
        | `Elt society ->
          update_selection_society (`Elt society) ;
          Ys_dom.set_value_textarea description society.View_society.description ;
          (* Ys_dom.set_value playbook society.View_society.playbook.View_playbook.name ; *)
      in

      let finalize_name, name =
        Autocomplete.create_society ~placeholder:"Name" View_society.format_autocomplete (fun i -> i) select_society
      in

      let _ =
        S.map
          (function
            | `String _ -> ()
            | `Elt society -> ())
          selection_society
      in

      List.iter
        (fun elt ->
           Ys_dom.onclick elt finalize_name ;
           Ys_dom.register_on_change
             elt
             (fun _ ->
                match S.value selection_society with
                  `Elt _ -> update_selection_society (`String "")
                | _ -> ()))
        [ description ] ;

      let create _ =
        match S.value selection_society with
        | `Elt society ->
          detach_rpc %Society_leader.link_society (uid, View_society.uid society)
              (fun society ->
                 finalize_playbook ~reset:true () ;
                 finalize_name ~reset:true () ;
                 Ys_dom.set_value_textarea description "" ;
                 RList.add societies society)
        | `String name ->
          (match S.value selection_playbook with
           | `String _ ->
             Help.warning "Please select a playbook from the autocomplete"
           | `Elt playbook ->
             (match name, Ys_dom.get_value_textarea description with
              | "", _ -> Help.warning "please give your society a name"
              | _, "" -> Help.warning "please give your society a description"
              | name_s, description_s ->
                detach_rpc %Society_leader.add_society (uid, View_playbook.uid playbook, name_s, description_s)
                    (fun society ->
                       finalize_playbook ~reset:true () ;
                       finalize_name ~reset:true () ;
                       Ys_dom.set_value_textarea description "" ;
                       RList.add societies society)))
      in

      let create =
        button
          ~a:[ a_button_type `Button ;
               a_onclick create ]
          [ pcdata "Create" ]
      in
      div ~a:[ a_class [ "add-society" ; "box" ]] [
        div ~a:[ a_class [ "box-section" ]] [ playbook ] ;
        div ~a:[ a_class [ "box-section" ]] [ name ] ;
        div ~a:[ a_class [ "box-section" ]] [ description ] ;
        div ~a:[ a_class [ "box-action" ]] [ create ] ;
      ]
    in
    let format_society society =
      div ~a:[ a_class [ "box" ]] [
        pcdata society.View_society.name
      ]
    in
    div ~a:[ a_class [ "manage" ; "manage-societies" ]] [
      menu shortlink uid ;
      RList.map_in_div_hd ~a:[ a_class [ "societies" ]] add_society format_society societies
    ]

(* the routing dom ************************************************************)

let dom shortlink uid =
  function
  | Service.ManageHome ->
    Template.apply %retrieve (builder_home shortlink) uid
  | Service.ManageMembers ->
    Template.apply %retrieve_members (builder_members shortlink uid) uid
  | Service.ManageCustom ->
    Template.apply %retrieve_custom (builder_custom shortlink uid) uid
  | Service.ManageSocieties ->
    Template.apply %retrieve_societies (builder_societies shortlink uid) uid
  | _ -> S.const None

}}
