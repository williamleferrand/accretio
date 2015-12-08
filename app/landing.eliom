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

let popular_playbooks = [ 1 ]

let fetch_popular_playbooks () =
  Lwt_log.ign_info_f "fetch popular playbooks" ;
  lwt popular = Lwt_list.map_p View_playbook.to_view popular_playbooks in
  return (Demo.automata, popular)

let fetch_popular_playbooks =
  server_function ~name:"landing-fetch-popular-playbooks" Json.t<unit> fetch_popular_playbooks

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
    div ~a:[ a_class [ "landing-title" ]] [
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


let dom = Template.apply %fetch_popular_playbooks builder


}}
