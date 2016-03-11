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

let fetch_public_playbooks () =
  lwt playbooks = Library.get_playbooks_ () in
  return (Demo.automata, playbooks)

let fetch_public_playbooks =
  server_function ~name:"landing-fetch-popular-playbooks" Json.t<unit> fetch_public_playbooks

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let builder (demo, playbooks) =
  let graph_demo = div ~a:[ a_class [ "graph-demo" ]] [] in
  Ys_viz.render graph_demo demo ;

  let grid =
    Ys_grid.create
      ~a:[ a_class [ "playbooks" ; "clearfix" ]]
      ~a_col:[ a_class [ "library-column" ]]
      ~column_width:410
      ~content:(S.const (List.map View_playbook.format playbooks))
      ()
  in

  let toggle, content =
    Ys_toggle.create
      (fun _ _ _ ->
         div ~a:[ a_class [ "learn-more" ]] [

         h2 [ pcdata "How it works?" ] ;

         span [
           pcdata "Accretio is an open-source collection of recipes, called playbooks, that describe how a group can self-organize to achieve certain goals in a transparent and fair way." ; br () ;
           br () ;
           pcdata "Once a group decides to team up on recurring task, such as babysitting each others' children or taking turns at preparing meals, the platform schedules shifts and make sure that everyone periodically contributes to the group." ; br () ;
           br () ;
           pcdata "You can browse the list of existing playbooks and groups below. If you have an activity in mind that hasn't been implemented yet, " ;  Raw.a ~a:[ a_href "mailto:hi@accret.io" ] [ pcdata "drop us a line!" ] ; pcdata ". Alternatively, you can check out how accretio is implemented via our " ;  Raw.a ~a:[ a_target "_blank" ; a_href "https://github.com/accretio" ] [ pcdata "Github repository." ]
         ]
         ])
      (fun _ -> ())
  in
  let learn_more =
    button
      ~a:[ a_button_type `Button ;
           a_onclick (fun _ -> Ys_mixpanel.track "toggle-learn-more" () ; toggle ())
         ]
      [ pcdata "Learn more" ]
  in

  div ~a:[ a_class [ "landing" ]] [
    h1 [
      pcdata "Take turns at doing what you need"
    ] ;

    div ~a:[ a_class [ "landing-pitch" ; "clearfix" ]] [
      div ~a:[ a_class [ "pitch" ]] [
        div [ pcdata "Pick up an activity" ] ;
        div [ pcdata "Join an existing group or start your own" ] ;
        div [ pcdata "Let accretio transparently organize the shifts" ] ;
        learn_more ;
      ] ;
      graph_demo ;
    ] ;

    content ;

    div ~a:[ a_class [ "landing-popular" ]] [
      h2 [ pcdata "Search" ] ;
      Search.builder []
    ] ;

    div ~a:[ a_class [ "landing-popular" ]] [
      h2 [ pcdata "Popular activities" ] ;
      div ~a:[ a_class [ "library" ]] [ grid ] ;
    ]

  ]


let dom = Template.apply %fetch_public_playbooks builder


}}
