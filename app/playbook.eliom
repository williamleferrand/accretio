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

type bundle =
  {
    playbook : View_playbook.t ;
    societies : View_society.t list ;
    automata : string ;
  }
}}

{server{

open Vault

let retrieve uid =
  lwt playbook = View_playbook.to_view uid in
  lwt societies = $playbook(uid)->societies in
  lwt societies =
    solve_acl
      ACLConnected
      (fun session ->
         Lwt_list.filter_p
           (fun (`Society, uid) ->
              return_true
           )
           societies)
      (fun () ->
         Lwt_list.filter_p
           (fun (`Society, uid) ->
              return_true
           )
           societies)
  in
  lwt societies =
    Lwt_list.map_p
      View_society.to_view
      (List.map snd societies) in
  let automata =
    let playbook = Registry.get uid in
    let module P = (val playbook : Api.PLAYBOOK) in
    P.automata
  in
  return
    {
      playbook ;
      societies ;
      automata ;
    }

let retrieve = server_function ~name:"playbook-retrieve" Json.t<int> retrieve

let _ =
  Ys_shortlink.register_checker Object_society.Store.find_by_shortlink

let create_society (playbook, name, description) : Ys_uid.uid option Lwt.t =
  protected_connected
    (fun session ->
       match_lwt Ys_shortlink.create () with
         None ->
         Lwt_log.ign_error_f "couldn't create shortlink for society %s, %s, playbook %d" name description playbook ;
         return_none
       | Some shortlink ->
       match_lwt Object_society.Store.create
                   ~shortlink
                   ~leader:session.member_uid
                   ~name
                   ~description
                   ~playbook
                   ~mode:Object_society.Sandbox
                   () with
       | `Object_created society ->
         let uid = society.Object_society.uid in
         lwt _ = $member(session.member_uid)<-societies +=! (`Society, uid) in
         lwt _ = $playbook(playbook)<-societies +=! (`Society, uid) in
         return (Some uid)
       | `Object_already_exists (_, uid) -> return (Some uid))

let create_society = server_function ~name:"playbook-create-society" Json.t<int * string * string> create_society

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_playbook

let builder view =
  let playbook = view.playbook in
  let societies = view.societies in
  (* todo : maybe we should pull the automata from the server & not carry the modules on the client side *)
  let graph = div ~a:[ a_class [ "playbook-automata" ]] [] in
  let svg = Ys_viz.render graph view.automata in

  let create_society =
    let name = input ~input_type:`Text ~a:[ a_placeholder "name" ] () in
    let description = Raw.textarea ~a:[ a_placeholder "description" ] (pcdata "") in
    let create _ =
      match Ys_dom.get_value name, Ys_dom.get_value_textarea description with
      "", _ -> Help.warning "Please specify a name"
      | _, "" -> Help.warning "Please add a description"
      | name, description ->
        Authentication.if_connected
          (fun _ ->
             rpc
             %create_society
                 (view.playbook.uid, name, description)
                 (fun uid -> Service.goto (Service.Society uid)))
    in
    let create =
      button
        ~button_type:`Button
        ~a:[ a_onclick create ]
        [ pcdata "Create" ]
    in
    div ~a:[ a_class [ "society" ]] [
      name ;
      description ;
      create ;
    ]
  in

  let societies =
    div ~a:[ a_class [ "societies" ]]
      (create_society :: List.map View_society.format view.societies)
  in

  div ~a:[ a_class [ "playbook" ]] [
    div ~a:[ a_class [ "playbook-description" ]] [
      pcdata playbook.View_playbook.description ;
    ] ;

    graph ;

    h2 [ pcdata "Societies" ] ;
    societies ;
  ]

let dom = Template.apply %retrieve builder

}}
