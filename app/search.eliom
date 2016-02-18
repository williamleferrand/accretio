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

type result = Profile of View_member.t

}}

{server{

let search query : result list Lwt.t =
  Lwt_log.ign_info_f "running search for %s" query ;
  if query = "" then return []
  else
    begin
      lwt profiles = Object_member.Store.search_name query in
      lwt profiles =
        Lwt_list.map_p
          (fun uid -> lwt view = View_member.to_view uid in return (Profile view))
          profiles
      in
    end

let search = server_function ~name:"search-search" Json.t<string> search

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let builder ?(query="") results =

  let results = RList.init results in

  let search =
    let input =
      input
        
        ~a:[ a_placeholder "Search on Accretio" ;
             a_value query ] () in
    Ys_dom.delay_focus input ;
    Manip.Ev.onkeyup
      input
      (fun _ ->
         let query = Ys_dom.get_value input in
         Ys_mixpanel.track "search" ~params:[ "query", query ] () ;
         ignore_result (%search query >>= fun r -> RList.update results r ; Service.goto (Service.Search (Some query)) ; return_unit) ;
         true
      ) ;

    input ;
  in

  let format = function
    | Profile view ->

      let profile_picture =
          match view.View_member.profile_picture with
              None ->
              div ~a:[ a_class [ "leader-picture" ]] [
                span ~a:[ a_class [ "icon-user" ]] []
              ]

            | Some image ->
              div ~a:[ a_class [ "leader-picture" ]] [
                match image.View_image.content with
                | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
                | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()
              ]
      in


      div ~a:[ a_class [ "result-profile" ; "clearfix" ] ;
               a_onclick (fun _ -> Service.goto (Service.Profile view.View_member.uid)) ] [
        div ~a:[ a_class [ "member-profile" ]] [ profile_picture ] ;
        div ~a:[ a_class [ "member-name" ]] [
          pcdata view.View_member.name ;
        ]
      ]

  in

  div ~a:[ a_class [ "search" ]] [
    search ;
    RList.map_in_div ~a:[ a_class [ "results" ]] format results ;
  ]

let dom query () =
  Template.apply
  (fun () ->
    match query with
      None -> return []
    | Some query -> %search query)
  (builder ?query)
  ()

}}
