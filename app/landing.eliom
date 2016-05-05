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

let fetch_landing () =

  lwt playbooks = Library.get_playbooks_ () in
  lwt societies =
    Object_society.Store.search_tags "landing"
  in
  lwt societies = Lwt_list.map_s View_society.to_view societies in
  return (Demo.automata, societies, playbooks)

let fetch_landing =
  server_function ~name:"landing-fetch-landing" Json.t<unit> fetch_landing

let ask (email, question) =
  return (Some ())

let ask =
  server_function ~name:"landing-ask" Json.t<string * string> ask

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let builder (demo, societies, playbooks) =
  let graph_demo = div ~a:[ a_class [ "graph-demo" ]] [] in
  Ys_viz.render graph_demo demo ;

  let grid_societies =
    Ys_grid.create
      ~a:[ a_class [ "societies" ; "clearfix" ]]
      ~a_col:[ a_class [ "library-column" ]]
      ~column_width:420
      ~content:(S.const (List.map View_society.format societies))
      ()
  in

  let grid_playbooks =
    Ys_grid.create
      ~a:[ a_class [ "playbooks" ; "clearfix" ]]
      ~a_col:[ a_class [ "library-column" ]]
      ~column_width:420
      ~content:(S.const (Library.make_suggestion () :: List.map View_playbook.format playbooks))
      ()
  in

  let toggle, content =
    Ys_toggle.create
      (fun _ _ _ ->

         let question = Raw.textarea ~a:[ a_placeholder "What is your question?" ] (pcdata "") in
         let email = input ~a:[ a_input_type `Text ;
                                a_placeholder "Enter your email (optional)" ] () in

         let ask _ =
           match Ys_dom.get_value_textarea question with
             "" -> ()
           | _ as question_s ->
             let email_s = Ys_dom.get_value email in
             detach_rpc %ask (email_s, question_s) (fun _ ->
                 Ys_dom.set_value_textarea question "" ;
                 Ys_dom.set_value email "" ;
                 Help.warning "Thanks, we will be in touch!")
         in
         let ask =
           button
             ~a:[ a_button_type `Button ;
                  a_onclick ask ]
             [ pcdata "Ask" ]
         in

         let ask =
           div ~a:[ a_class [ "box" ]] [
             div ~a:[ a_class [ "box-section" ]] [
               question
             ] ;
             div ~a:[ a_class [ "box-section" ]] [
               email
             ] ;
             div ~a:[ a_class [ "box-action" ]] [
               ask
             ]
           ]
         in

         div ~a:[ a_class [ "learn-more" ]] [

           h2 [ pcdata "FAQ" ] ;
           div [
             h3 [ pcdata "What is accretio?" ] ;
             pcdata "Accretio is:" ;
             ul [
               li [ pcdata "an open-source collection of recipes, called playbooks, that describe how a group can self-organize to achieve certain goals in a transparent and fair fashion" ] ;
               li [ pcdata "an simple way to execute those recipes by communicating with the participants via email" ] ;
             ]
           ] ;
           div [
             h3 [ pcdata "How are the shifts organized?" ] ;
             pcdata "Accretio ensures that tasks are fairly split among the participants by rotating shifts. You can audit the playbooks yourself or analyse the activity's logs to verify it."
           ] ;
           div [
             h3 [ pcdata "What are the duties for each activities?" ] ;
             pcdata "For each activity, a brief outline of the duties are presented on the activity card."
           ] ;
           div [
             h3 [ pcdata "How to contribute?" ] ;
             pcdata "You can browse the list of existing playbooks and groups below. If you have an activity in mind that hasn't been implemented yet, " ;  Raw.a ~a:[ a_href "mailto:hi@accret.io" ] [ pcdata "drop us a line!" ] ; pcdata ". Alternatively, you can check out how accretio is implemented via our " ;  Raw.a ~a:[ a_target "_blank" ; a_href "https://github.com/accretio" ] [ pcdata "Github repository." ]
           ] ;
           div ~a:[ a_class [ "ask-a-question" ]] [
             h3 [ pcdata "Do you have a question?" ] ;
             Raw.a ~a:[ a_href "mailto:hi@accret.io" ] [ pcdata "We are here to help!" ]
           ] ;
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
      pcdata "Team up and take turns"
    ] ;

    div ~a:[ a_class [ "landing-pitch" ; "clearfix" ]] [
      div ~a:[ a_class [ "pitch" ]] [
        div [ pcdata "Pick up an activity" ] ;
        div [ pcdata "Join an existing group or start your own" ] ;
        div [ pcdata "Let accretio automatically organize the shifts" ] ;
        learn_more ;
      ] ;
      graph_demo ;
    ] ;

    content ;

   (* div ~a:[ a_class [ "landing-popular" ]] [
      h2 [ pcdata "Search" ] ;
      Search.builder []
    ] ; *)


    (match societies with
       [] -> pcdata ""
     | _ -> div ~a:[ a_class [ "landing-popular" ]] [
         h2 [ pcdata "Popular groups" ] ;
         div ~a:[ a_class [ "library" ]] [ grid_societies ]
       ]) ;

    div ~a:[ a_class [ "landing-popular" ]] [
      h2 [ pcdata "Popular playbooks" ] ;
      div ~a:[ a_class [ "library" ]] [ grid_playbooks ] ;
    ]

  ]

let dom = Template.apply %fetch_landing builder

}}
