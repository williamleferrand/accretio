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

(* Ideally code here should only rely on the API signature as this is intended
   to be part of the playbook .. *)

{shared{

open Lwt
open Sessions
open Ys_uid
open Vault

open Children_schoolbus_types

}}

{server{

(* there are a lot of ways to mess up the link between profiles & societies
   membership .. *)

let update_profile (society, profile) =
  Lwt_log.ign_info_f "updating profile in society %d, %s" society profile ;
  let profile = Yojson_profile.from_string profile in
  let key = Printf.sprintf "profile-%d" profile.uid in
  protected_connected
    (fun _ ->
       lwt _ = $member(profile.uid)<-name %% (fun _ -> profile.name) in
       lwt _ = $society(society)<-data %%% (fun data ->
           lwt _ =
             match profile.children with
               [] -> return_unit
             | _ ->
               Lwt_list.iter_s
                 (fun group ->
                    lwt societies = Object_society.Store.search_societies society group in
                    Lwt_list.iter_p
                      (fun society ->
                         lwt members = $society(society)->members in
                         match Ys_uid.Edges.mem profile.uid members with
                           true -> return_unit
                         | false ->
                       lwt _ = $society(society)<-members += (`Member [ "active" ], profile.uid) in
                       (* Lwt_log.ign_info_f "calling new member in society %d for member %d" society profile.uid ;
                       Executor.stack_int society Api.Stages.new_member profile.uid *) return_unit)
                   societies)
                 profile.groups in
           return ((key, Yojson_profile.to_string profile) :: List.remove_assoc key data))
       in
       lwt data = $society(society)->data in
       return (Some data))

let create_profile (society, profile) =
  protected_connected
    (fun _ ->
       let profile = Yojson_profile.from_string profile in
       match profile.email with
         "" -> return_none
       | _ as email ->
         lwt uid =
           match_lwt Object_member.Store.find_by_email email with
           | Some uid -> return uid
           | None ->
             match_lwt Object_member.Store.create
                         ~preferred_email:email
                         ~emails:[ email ]
                         ~name:profile.name
                         () with
             | `Object_already_exists (_, uid) -> return uid
             | `Object_created member -> return member.Object_member.uid
         in
         let profile = { profile with uid } in
         let key = Printf.sprintf "profile-%d" profile.uid in
         lwt data = $society(society)->data in
         match List.exists (fun data -> fst data = key) data with
           | true -> return_none
           | false -> update_profile (society, Yojson_profile.to_string profile))

let update_profile = server_function ~name:"schoolbus-block-update-profile" Json.t<int * string> update_profile
let create_profile = server_function ~name:"schoolbus-block-create-profile" Json.t<int * string> create_profile

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
    let group, update_group = S.create None in

    let finalize_group, groups =
      Autocomplete.create_society ~placeholder:"Name" View_society.format_autocomplete (fun i -> i)
        (function
          | `Elt group -> update_group (Some group)
          | _ -> update_group None)
    in

    let update_groups _ =
      match S.value group with
        Some group ->
        let profile = { profile with groups = [ group.View_society.name ] } in
        detach_rpc %update_profile (society, Yojson_profile.to_string profile) (fun res -> finalize_group ~reset:true () ; RList.update data res)
      | _ ->
        ()
    in

    let update_groups =
      button
        ~a:[ a_button_type `Button ;
             a_onclick update_groups ]
        [ pcdata "Update" ]
    in

    let status =
      match profile.groups with
        [] -> "profile-not-assigned"
      | _ -> "profile-assigned"
    in

    (* the form *)
    div ~a:[ a_class [ "box" ; "schoolbus-profile" ; status  ]] [
      h3 [ pcdata profile.name ; pcdata " (" ; pcdata (string_of_int profile.uid) ; pcdata ")" ] ;
      div ~a:[ a_class [ "box-section" ; "profile-name" ]] [
        h4 [ pcdata "Name" ] ;
        name ; update_name
      ] ;
      div ~a:[ a_class [ "box-section";  "profile-neighborhood" ]] [
        h4 [ pcdata "Address" ] ;
        neighborhood ; update_neighborhood
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-children" ]] [
        h4 [ pcdata "Children" ];
        div (List.map format_child profile.children)
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-children" ]] [
        h4 [ pcdata "Group" ];
        div [
          ul (List.map (fun group -> li [ pcdata group ]) profile.groups) ;
        ] ;
        groups ; update_groups ;
      ] ;
      div ~a:[ a_class [ "box-section" ; "profile-raw" ]] [
        h4 [ pcdata "Raw" ];
        raw ; update_raw ;
      ] ;
    ]
  in

  (* add a profile *)

  let add_profile =

    let profile = Raw.textarea (pcdata (Yojson_profile.to_string (default_profile 0 "" ""))) in
    let add_profile _ =
      let profile = Ys_dom.get_value_textarea profile in
      try
        let profile = Yojson_profile.from_string profile in
        let profile = Yojson_profile.to_string profile in
        detach_rpc %create_profile (society, profile) (RList.update data)
      with _ ->
        Help.warning "Please provide a valid JSON"
    in

    let add_profile =
      button
        ~a:[ a_button_type `Button ;
             a_onclick add_profile ]
        [ pcdata "Add" ]
    in
    div ~a:[ a_class [ "schoolbus-profiles-add" ; "box" ]] [
      div ~a:[ a_class [ "box-section" ]] [
        profile ;
      ] ;
      div ~a:[ a_class [ "box-action" ]] [
        add_profile
      ] ;

    ]
  in

  div ~a:[ a_class [ "schoolbus-profiles" ]] [
    h2 [ pcdata "Profiles" ] ;
    add_profile ;
    RList.map_in_div_ordered
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

(* the doms *****************************************************************************)

let doms society data () =
  [
    profiles society data ;
  ]

}}
