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


open Eliom_content
open Html5
open D
open Lwt
open Js
open React

let attach ?(square=true) ?(max_width=300) ?(max_height=300) img =
  let options = Js.Unsafe.obj [||] in
  options##minWidth <- 50 ;
  options##minHeight <- 50 ;
  options##maxWidth <- max_width ;
  options##maxHeight <- max_height ;

  let plugins = Js.Unsafe.obj [||] in
  let plugin_crop = Js.Unsafe.obj [||] in
  plugin_crop##quickCropKey <- 47 ;
  plugin_crop##minWidth <- 30 ;
  plugin_crop##minHeight <- 30 ;
  if square then plugin_crop##ratio <- 1 ;

  let room = ref None in

  plugins##crop <- plugin_crop ;
  options##plugins <- plugins ;
  options##init <- Js.wrap_callback
      (fun _ ->
         match !room with
           None -> ()
         | Some room ->
           let plugin = room##getPlugin(Js.string "crop") in
           plugin##selectZone(170, 25, 300, 300)) ;
  let obj = jsnew (Js.Unsafe.variable "Darkroom") (To_dom.of_img img, options) in
  room := Some obj ;
  obj


let upload_box ?(square=true) ?(max_width=300) ?(max_height=300) ?(name="Select file") ?cl callback =
  let selected_file, update_selected_file = S.create None in
  let input = input ~a:[ a_id "images" ; a_accept [ "image/*" ] ] ~input_type:`File () in
  let label = Raw.label ~a:[ Raw.a_for "images" ; (match cl with None -> a_class [ "darkroom-label" ] | Some cl -> a_class [ cl ]) ] [ pcdata name ] in
  Manip.Ev.onchange
    input
    (fun ev ->
       Js.Optdef.case
         ((Html5.To_dom.of_input input)##files)
         (fun () -> update_selected_file None)
         (fun files ->
            Js.Opt.case
              files##item(0)
              (fun () -> update_selected_file None)
              (fun file ->
                 ignore_result
                   (lwt src = File.readAsDataURL file in
                    update_selected_file (Some (Js.to_string src)) ; return_unit))) ;
       true) ;
  R.node
    (S.map
       (function
         | None -> div ~a:[ a_class [ "darkroom" ]] [ input ; label ]
         | Some data ->

           let img = img ~alt:"" ~src:(uri_of_string (fun () -> data)) () in

           let darkroom = attach ~square ~max_width ~max_height img in

           let upload _ =
             let data = Js.to_string (darkroom##snapshotImage()) in
             (* forcing jpeg because for some reason png isn't working on my mac .. *)
             callback data (fun () -> update_selected_file None)
           in

           div ~a:[ a_class [ "darkroom-upload-box" ]] [
             img ;
             div ~a:[ a_class [ "upload-controls" ]] [
               button ~button_type:`Button ~a:[ a_onclick (fun _ -> update_selected_file None) ] [ pcdata "Select another file" ] ;
               button ~button_type:`Button ~a:[ a_onclick upload ] [ pcdata "Upload" ]
             ]
           ])
       selected_file)
