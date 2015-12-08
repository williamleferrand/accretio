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


(* we call this one dashboard.ml instead of map.ml to avoid name clashes *)


{shared{

open Lwt
open Vault
open Sessions

}}

{server{

let get_version () =
  return Mu.version

let get_version = server_function ~name:"get-version" Json.t<unit> get_version

}}

{client{

open React
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_dummy
open Ys_react
open Service

open React

let current_version = Printf.sprintf "%d_%s" Version.version_num Version.version
let max_errors = ref 3

(* todo : make it a little bit more user friendly? *)
let check_version ()=
  Js.wrap_callback
    (fun () ->
       ignore_result
         (try_lwt
            lwt version = %get_version () in
            if version <> current_version then
              begin
                Ys_log.info "current_version is %s, server version is %s, triggering update" current_version version ;
                Ys_mixpanel.track "updater-version-mismatch" ~params:[ "current-version", current_version ;
                                                                       "server-version", version ] () ;
                (Obj.magic (Dom_html.window##location))##reload(Js._true)
              end
            else
              max_errors := 3 ;
            return_unit
          with
            _ when !max_errors < 0 ->
            Ys_mixpanel.track "updater-max-errors" () ;
            (Obj.magic (Dom_html.window##location))##reload(Js._true)
          | _ -> Ys_log.info "couldn't check version"; decr max_errors ; return_unit))


let start () =
  ignore (Dom_html.window##setInterval (check_version (), 300000.0))

}}
