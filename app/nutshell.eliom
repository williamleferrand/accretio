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

open Eliom_content.Html5.D

}}

{server{

let description = "Automate what makes you thrive"

let common ?(title="Accretio") ?(description=description) mixpanel_id version url =
  let app_id = Ys_config.get_string (Ys_config.fb_app_id) in
  html (* ~a:[ a_manifest (uri_of_string (fun () -> "/manifest_" ^ version ^".manifest")) ] *)
    (head (Eliom_content.Html5.D.title (pcdata "Accretio")) [
        meta ~a:([a_charset "utf-8"; a_content "text/html"; a_http_equiv "Content-Type"]) () ;
        meta ~a:[ a_name "viewport"; a_content "initial-scale=1,maximum-scale=1,user-scalable=no,minimal-ui" ] () ;
        meta ~a:[ a_name "apple-mobile-web-app-capable"; a_content "yes" ] () ;
        meta ~a:[ a_name "mobile-web-app-capable"; a_content "yes" ] () ;

        meta ~a:[ a_property "og:description"; a_content description ] () ;
        meta ~a:[ a_property "og:type"; a_content "website" ] () ;
        meta ~a:[ a_property "og:url"; a_content url ] () ;
        meta ~a:[ a_property "fb:app_id"; a_content app_id ] () ;
        meta ~a:[ a_property "og:title"; a_content title ] () ;
        meta ~a:[ a_property "og:site_name"; a_content "https://accret.io" ] () ;
        link ~rel: [ `Other "apple-touch-icon" ] ~href:(uri_of_string (fun () -> "/logoaccretio_blue.png")) () ;

        Ys_googlemaps.script () ;
        script ~a:[ a_src (uri_of_string (fun () -> "/ie.js")) ] (pcdata "") ;
        script ~a:[ a_src (uri_of_string (fun () -> "/moment.min.js")) ] (pcdata "") ;
        script ~a:[ a_src (uri_of_string (fun () -> "/moment-timezone.min.js")) ] (pcdata "") ;

        (* script ~a:[ a_src (uri_of_string (fun () -> "/masonry.pkgd.min.js")) ] (pcdata "") ; *)
        (* script ~a:[ a_src (uri_of_string (fun () -> "/typeahead.bundle.js")) ] (pcdata "") ; *)

        script (pcdata "moment().format();") ;
        script ~a:[ a_src (uri_of_string (fun () -> "/Autolinker.min.js")) ] (pcdata "") ;

        (* script ~a:[ a_src (uri_of_string (fun () -> "/ZeroClipboard.js")) ] (pcdata "") ;
           script ~a:[ a_src (uri_of_string (fun () -> "/fabric.js")) ] (pcdata "") ;
           script ~a:[ a_src (uri_of_string (fun () -> "/darkroom.min.js")) ] (pcdata "") ;
           script ~a:[ a_src (uri_of_string (fun () -> "/countdown.min.js")) ] (pcdata "") ; *)

        script ~a:[ a_src (uri_of_string (fun () -> "/viz.js")) ] (pcdata "") ;
        script ~a:[ a_src (uri_of_string (fun () -> "/js-static_" ^ version ^ ".min.js")) ] (pcdata "") ;
        link ~rel:[ `Stylesheet ] ~href:(uri_of_string (fun () -> "/style_" ^ version ^ ".css")) ();
        link ~rel:[ `Stylesheet ] ~href:(uri_of_string (fun () -> "/fontello.css")) () ;
        Ys_mixpanel.tracker mixpanel_id
      ])
    (body [ div ~a:[ a_id "fb-root" ] []])

}}

{client{

open React

open Sessions
open Service

open Eliom_content.Html5

let load = S.const (div ~a:[ a_class [ "load" ]] [])

(* let lost = S.const (Some (div ~a:[ a_class [ "lost" ]] [])) *)

let wrap = S.map (fun content -> Some content)

let dispatch service : 'a option React.signal =
  match service with
  | AdminStats -> wrap (Admin.dom_stats ())
  | AdminGraphMember uid_option -> wrap (Admin.dom_graph_member uid_option)
  | AdminGraphThread uid_option -> wrap (Admin.dom_graph_thread uid_option)
  | AdminGraphBooking uid_option -> wrap (Admin.dom_graph_booking uid_option)
  | AdminGraphPayment uid_option -> wrap (Admin.dom_graph_payment uid_option)
  | AdminI18n -> wrap (Admin.dom_i18n ())
  | Playbook uid -> Playbook.dom uid
  | Dashboard -> Dashboard.dom ()
  | Library -> Library.dom ()
  | Create -> Create.dom ()
  | Landing -> Landing.dom ()
  | Society (_, uid) -> Society.dom uid
  | Manage (shortlink, uid, step) -> Manage.dom shortlink uid step
  | Payment (_, uid) -> Payment.dom uid
  | Settings -> Settings.dom ()
  | Search query -> Search.dom query
  | RequestRecovery -> wrap (Recover.dom_request_recovery ())
  | Schoolbus -> Schoolbus.dom ()
  | _ -> S.const None

let container: Html5_types.div_content_fun elt React.signal =
  S.Option.value ~default:(`Init load) (S.bind service dispatch)

(*  S.map dispatch service *)

open Help

let format_message message =
  match message with
  | Silent -> div []
  | Warning message ->
    let close_button =
      button
        ~a:[ a_onclick (fun _ -> update_message Silent) ]

        [ pcdata "Close" ]
    in
    div ~a:[ a_class [ "help"; "warning" ]]
      [
        pcdata message ;
        close_button
      ]
  | Error message ->
    let close_button =
      button
        ~a:[ a_onclick (fun _ -> update_message Silent) ]

        [ pcdata "Close" ]
    in
    div ~a:[ a_class [ "help"; "error" ]]
      [
        pcdata message ;
        close_button
      ]
  | Ask (message, continuation) ->
    let yes_button =
      button
        ~a:[ a_onclick (fun _ -> continuation true; update_message Silent) ]

        [ pcdata "Yes" ]
    in
    let no_button =
      button
        ~a:[ a_onclick (fun _ -> continuation false; update_message Silent) ]

        [ pcdata "No" ]
    in
    div ~a:[ a_class [ "help"; "ask" ]]
      [
        pcdata message ;
        yes_button ;
        no_button ;
      ]

open Vault

let body () =
  div ~a:[ a_class [ "nutshell" ]] [
    R.node (S.map format_message message) ;
    Header.dom () ;
    R.node (S.map (function None -> pcdata "" | Some popup -> popup) Popup.anchor) ;
    div ~a:[ a_class ["nutshell-inner" ]] [ R.node container ] ;
  ]

}}
