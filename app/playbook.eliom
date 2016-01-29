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
    thread : View_thread.t ;
    societies : View_society.t list ;
    automata : string ;
  }
}}

{server{

open Vault

let retrieve uid =
  lwt authorized =
    solve_acl
      ACLConnected
      (fun session ->
         match_lwt $playbook(uid)->(scope, owner) with
           Ys_scope.Public, _ -> return_true
         | _, owner when owner = session.member_uid -> return_true
         | _ -> return_false)
      (fun () ->
         match_lwt $playbook(uid)->scope with
           Ys_scope.Private-> return_false
         | Ys_scope.Public -> return_true)
  in
  match authorized with
    false -> return_none
  | true ->
    lwt playbook = View_playbook.to_view uid in
    lwt societies, thread = $playbook(uid)->(societies, thread) in
    lwt thread = View_thread.to_view thread in
    let automata =
      let playbook = Registry.get uid in
      let module P = (val playbook : Api.PLAYBOOK) in
      P.automata
    in
    return
      (Some {
          playbook ;
          thread ;
          societies = [] ;
          automata ;
        })

let retrieve = server_function ~name:"playbook-retrieve" Json.t<int> retrieve

let _ =
  Ys_shortlink.register_checker Object_society.Store.find_by_shortlink

let create_society (playbook, name, description, data) : (string * Ys_uid.uid) option Lwt.t =
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
                   ~data
                   () with
       | `Object_created society ->
         let uid = society.Object_society.uid in
         lwt _ = $member(session.member_uid)<-societies +=! (`Society, uid) in
         lwt _ = $playbook(playbook)<-societies +=! (`Society, uid) in
         return (Some (shortlink, uid))
       | `Object_already_exists (_, uid) -> return (Some (shortlink, uid)))

let create_society = server_function ~name:"playbook-create-society" Json.t<int * string * string * ((string * string) list)> create_society

let make_public playbook =
  protected_connected
    (fun session ->
       Lwt_log.ign_info_f "making playbook %d public" playbook ;
       lwt _ = $playbook(playbook)<-scope = Ys_scope.Public in
       return (Some View_playbook.Public))

let make_private playbook =
  protected_connected
    (fun session ->
       Lwt_log.ign_info_f "making playbook %d private" playbook ;
       lwt _ = $playbook(playbook)<-scope = Ys_scope.Private in
       return (Some View_playbook.Private))

let make_public = server_function ~name:"playbook-make-public" Json.t<int> make_public
let make_private = server_function ~name:"playbook-make-private" Json.t<int> make_private

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_playbook

let builder  = function
  | None ->
    div [
      pcdata "This playbook is private"
    ]
  | Some view ->
    let playbook = view.playbook in
    (* todo : maybe we should pull the automata from the server & not carry the modules on the client side *)
    let graph = div ~a:[ a_class [ "playbook-automata" ]] [] in
    let _ = Ys_viz.render graph view.automata in

    let create_society =
      let name = input ~input_type:`Text ~a:[ a_placeholder "name of your group" ] () in
      let description = Raw.textarea ~a:[ a_placeholder "brief description" ] (pcdata "") in
      let parameters =
        List.map
          (fun parameter ->
             let input = input ~input_type:`Text ~a:[ a_placeholder (String.lowercase parameter.label) ] () in
             input, (parameter.key, input))
          playbook.parameters
      in
      let inputs = List.map snd parameters in
      let parameters =
        div ~a:[ a_class [ "parameters" ]] (List.map fst parameters)
      in
      let create _ =
        match Ys_dom.get_value name, Ys_dom.get_value_textarea description with
          "", _ -> Help.warning "Please specify a name"
        | _, "" -> Help.warning "Please add a description"
        | name, description ->
          Authentication.if_connected ~mixpanel:("playbook-create-society", [ "name", name ; "description", description ])
            (fun _ ->
               let data = List.map (fun (label, input) -> label, Ys_dom.get_value input) inputs in
               rpc
               %create_society
                   (view.playbook.uid, name, description, data)
                   (fun (shortlink, uid) -> Service.goto (Service.Society (shortlink, uid))))
      in
      let create =
        button
          ~button_type:`Button
          ~a:[ a_onclick create ]
          [ pcdata "Create" ]
      in
      div ~a:[ a_class [ "create-society"  ]] [
        h2 [ pcdata "Start a new group driven by this playbook" ] ;
        div ~a:[ a_class [ "create-society-form" ; "box" ]] [
          name ;
          description ;
          parameters ;
          div ~a:[ a_class [ "box-action" ]] [
            create ;
          ]
        ]
      ]
    in

    let playbook_controls =
      R.node
        (S.map
           (function
             | Connected session when session.member_uid = playbook.owner.View_member.uid ->
               let scope, update_scope = S.create playbook.scope in
               let make_public _ =
                 Authentication.if_connected ~mixpanel:("playbook-make-public", [ "uid", Ys_uid.to_string playbook.uid ])
                   (fun _ -> rpc %make_public playbook.uid update_scope)
               in
               let make_private _ =
                 Authentication.if_connected ~mixpanel:("playbook-make-private", [ "uid", Ys_uid.to_string playbook.uid ])
                   (fun _ -> rpc %make_private playbook.uid update_scope)
               in
               div ~a:[ a_class [ "playbook-controls" ]] [
                 button
                   ~button_type:`Button
                   ~a:[ a_onclick (fun _ -> make_public ()) ;
                        R.a_class (S.map (function Public -> [ "active" ] | _ -> []) scope) ]
                   [ pcdata "Public" ] ;
                 button
                   ~button_type:`Button
                   ~a:[ a_onclick (fun _ -> make_private ()) ;
                        R.a_class (S.map (function Private -> [ "active" ] | _ -> []) scope) ]
                   [ pcdata "Private" ] ;
               ]
             | _ -> pcdata "")
           Sessions.session)
    in
    let thread =
      div ~a:[ a_class [ "thread" ]] [
        h2 [ pcdata "Discuss" ] ;
        Lambda_thread.format view.thread
      ]
    in
    div ~a:[ a_class [ "playbook" ]] [
      playbook_controls ;
      div ~a:[ a_class [ "playbook-name" ]] [
        pcdata playbook.View_playbook.name ;
      ] ;
      div ~a:[ a_class [ "playbook-description" ]] [
        pcdata playbook.View_playbook.description ;
      ] ;

      graph ;
      thread ;
      create_society ;

    ]

let dom = Template.apply %retrieve builder

}}
