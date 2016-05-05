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

}}

{server{

let get_playbooks_ () =
  solve_acl
    ACLConnected
    (fun session ->
       Lwt_log.ign_info_f "getting playbooks for member %d" session.member_uid ;
       lwt views, _ =
         Object_playbook.Store.fold_flat_lwt
           (fun views uid ->
              match Registry.mem uid with
                false -> return (Some views)
              | true ->
                Lwt_log.ign_info_f "inspecting playbook %d" uid ;
                match_lwt $playbook(uid)->(scope, owner) with
                  Ys_scope.Public, _ -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _, owner when owner = session.member_uid -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _, owner ->
                  Lwt_log.ign_info_f "skipping playbook owned by %d" owner ;
                  return (Some views))
             []
             None
             (-1)
         in
         return views
    )
    (fun () ->
       Lwt_log.ign_info_f "getting all public playbooks" ;
       lwt views, _ =
         Object_playbook.Store.fold_flat_lwt
           (fun views uid ->
              match Registry.mem uid with
                false -> return (Some views)
              | true ->
                match_lwt $playbook(uid)->scope with
                  Ys_scope.Public -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _ -> return (Some views))
          []
          None
          (-1)
      in
      return views)

let get_playbooks = server_function ~name:"dashboard-get-playbooks" Json.t<unit> get_playbooks_

let make_suggestion (email, suggestion) =
  lwt _ = Notify.make_suggestion email suggestion in
  return (Some ())

let make_suggestion = server_function ~name:"library-make-suggestion" Json.t<string * string> make_suggestion

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_society

let make_suggestion () =
  let state, update_state = S.create `Closed in
  div ~a:[ a_class [ "make-suggestion" ]] [
    R.node
      (S.map
         (function
           | `Closed ->
             div ~a:[ a_class [ "box" ; "closed" ]] [
               button
                 ~a:[ a_button_type `Button ;
                      a_onclick (fun _ -> update_state `Make_suggestion) ]
                 [ pcdata "Suggest an activity" ]
             ]
           | `Make_suggestion ->
             let suggestion = Raw.textarea ~a:[ a_placeholder "What do you have in mind?" ] (pcdata "") in
             let email = input ~a:[ a_input_type `Text ; a_placeholder "What is your email? (optional)" ] () in
             let make_suggestion _ =
               match Ys_dom.get_value_textarea suggestion with
                 "" -> Help.warning "Please make a suggestion"
               | _ as suggestion ->
                 let email = Ys_dom.get_value email in
                 detach_rpc %make_suggestion (email, suggestion) (fun _ -> update_state `Thanks)
             in
             let make_suggestion =
               button ~a:[ a_button_type `Button ;
                           a_onclick make_suggestion ]
                 [ pcdata "Suggest" ]
             in
             div ~a:[ a_class [ "box" ]] [
               div ~a:[ a_class [ "box-section" ]] [
                 suggestion
               ] ;
               div ~a:[ a_class [ "box-section" ]] [
                 email ;
               ] ;
               div ~a:[ a_class [ "box-action" ]] [
                 make_suggestion ;
               ]
             ]
           | `Thanks ->
             ignore_result (Lwt_js.sleep 2. >>= fun _ -> update_state `Closed ; return_unit) ;
             div [
               pcdata "Thanks!"
             ])
         state)
  ]

let builder playbooks =
  let playbooks = RList.init playbooks in

  let grid =
    Ys_grid.create
      ~a:[ a_class [ "playbooks" ; "clearfix" ]]
      ~a_col:[ a_class [ "library-column" ]]
      ~column_width:420
      ~content:(S.map (fun playbooks -> make_suggestion () :: List.map View_playbook.format playbooks) (RList.channel playbooks))
      ()
  in
  div ~a:[ a_class [ "library" ]] [
    h1 [ pcdata "Library" ] ;
    grid ;
  ]

let dom =
  Template.apply
    %get_playbooks
    builder

 }}
