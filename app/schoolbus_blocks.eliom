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

open Children_schoolbus_types

}}

{server{

let update_profile (society, profile) =
  Lwt_log.ign_info_f "updating profile in society %d, %s" society profile ;
  let profile = Yojson_profile.from_string profile in
  let key = Printf.sprintf "profile-%d" profile.uid in
  protected_connected
    (fun _ ->
       lwt _ = $member(profile.uid)<-name %% (fun _ -> profile.name) in
       lwt _ = $society(society)<-data %% (fun data -> (key, Yojson_profile.to_string profile) :: List.remove_assoc key data) in
       lwt data = $society(society)->data in
       return (Some data))

let update_profile = server_function ~name:"schoolbus-block-update-profile" Json.t<int * string> update_profile

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

(* the profile manager ******************************************************************)

let profiles society data =
  let regex = Regexp.regexp "profile-[0-9]+" in

  let update_schema profile =
    let raw = Raw.textarea (pcdata profile) in
    let update_raw _ =
      try
        let profile = Ys_dom.get_value_textarea raw in
        let profile = Yojson_profile.from_string profile in
        let profile = Yojson_profile.to_string profile in
        detach_rpc %update_profile (society, profile) (RList.update data)
      with _ -> Help.warning "couldn't parse JSON"
    in
    let update_raw =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_raw ]
        [ pcdata "Update" ]
    in

    div ~a:[ a_class [ "box" ; "schoolbus-profile" ]] [
      h3 [ pcdata "The schema has changed, please migrate the JSON below" ] ;
      div ~a:[ a_class [ "box-section" ]] [
        raw ;
      ] ;
      div ~a:[ a_class [ "box-action" ]] [
        update_raw
      ]
    ]
  in

  let format_profile profile =
    (* name *)
    let name = Raw.input ~a:[ a_input_type `Text ; a_value profile.name ] () in
    let update_name _ =
      let profile = { profile with name = Ys_dom.get_value name } in
      detach_rpc %update_profile (society, Yojson_profile.to_string profile) (RList.update data)
    in
    let update_name =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_name ]
        [ pcdata "Update" ]
    in

    (* neighborhood *)
    let neighborhood = Raw.input ~a:[ a_input_type `Text ; a_value profile.neighborhood ] () in
    let update_neighborhood _ =
      let neighborhood = Ys_dom.get_value neighborhood in
      ignore_result
        (match_lwt Ys_googlemaps.geocode neighborhood with
           false -> Help.warning "couldn't geocode the address"; return_unit
         | true ->
           let profile = { profile with neighborhood } in
           rpc %update_profile (society, Yojson_profile.to_string profile) (RList.update data))
    in
    let update_neighborhood =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_neighborhood ]
        [ pcdata "Update" ]
    in

    (* children *)
    let format_child child =
      div [
       pcdata "Age: " ; pcdata child.age_string
      ]
    in

    (* raw *)
    let raw = Raw.textarea (pcdata (Yojson_profile.to_string profile)) in
    let update_raw _ =
      try
        let profile = Ys_dom.get_value_textarea raw in
        let profile = Yojson_profile.from_string profile in
        let profile = Yojson_profile.to_string profile in
        detach_rpc %update_profile (society, profile) (RList.update data)
      with _ -> Help.warning "couldn't parse JSON"
    in
    let update_raw =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_raw ]
        [ pcdata "Update" ]
    in

    (* groups *)
    let groups = Raw.input ~a:[ a_input_type `Text ; a_value (String.concat ", " profile.groups) ] () in
    let update_groups _ =
      let groups = Ys_dom.get_value groups in
      let groups = Regexp.split (Regexp.regexp "[\t ' ']*,[\t ' ']*") groups in
      let profile = { profile with groups } in
      detach_rpc %update_profile (society, Yojson_profile.to_string profile) (RList.update data)
    in
    let update_groups =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_groups ]
        [ pcdata "Update" ]
    in

    (* the form *)
    div ~a:[ a_class [ "box" ; "schoolbus-profile" ]] [
      h3 [ pcdata profile.name ; pcdata " (" ; pcdata (string_of_int profile.uid) ; pcdata ")" ] ;
      div ~a:[ a_class [ "box-section" ; "profile-name" ]] [
        h4 [ pcdata "Name" ] ;
        name ; update_name
      ] ;
      div ~a:[ a_class [ "box-section";  "profile-neighborhood" ]] [
        h4 [ pcdata "Pick up point" ] ;
        neighborhood ; update_neighborhood
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-children" ]] [
        h4 [ pcdata "Children" ];
        div (List.map format_child profile.children)
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-children" ]] [
        h4 [ pcdata "Groups" ];
        groups ; update_groups ;
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-raw" ]] [
        h4 [ pcdata "Raw" ];
        raw ; update_raw ;
      ] ;
    ]
  in

  div ~a:[ a_class [ "schoolbus-profiles" ]] [
    h2 [ pcdata "Profiles" ] ;
    RList.map_in_div
      (function (key, value) ->
        match Regexp.string_match regex key 0 with
          None -> pcdata ""
        | Some _ ->
          try
            let profile = Yojson_profile.from_string value in
            format_profile profile
          with exn ->
            update_schema value
      )
      data
  ]


(* scheduling the trips *****************************************************************)

let schedule_trip society =
  (* here we probably want to use tags *)
  div []

(* the doms *****************************************************************************)

let doms society data () =
  [
    profiles society data ;
    schedule_trip society ;
  ]

}}
