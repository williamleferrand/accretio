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

  div ~a:[ a_class [ "landing" ]] [
    h1 [
      pcdata "Automate what makes you thrive"
    ] ;
    div ~a:[ a_class [ "landing-pitch" ; "clearfix" ]] [
      div ~a:[ a_class [ "pitch" ]] [
        pcdata "Use playbooks to describe, automate and share social processes"   ;
      ] ;
      graph_demo ;
    ] ;

    div ~a:[ a_class [ "landing-popular" ]] [
      h2 [ pcdata "Popular playbooks" ] ;
      div ~a:[ a_class [ "playbooks" ; "clearfix" ]]
        (List.map View_playbook.format playbooks)
    ]
  ]


let dom = Template.apply %fetch_public_playbooks builder


}}
