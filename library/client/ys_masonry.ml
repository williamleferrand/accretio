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


open Lwt
open Eliom_content

let attach ~gutter container =
  let masonry = Js.Unsafe.variable "Masonry" in
  let options = Js.Unsafe.obj [||] in
  options##gutter <- gutter ;
  options##transitionDuration <- 0;
  jsnew masonry (Html5.To_dom.of_div container, options)

let refresh = ref (fun () -> ())

let install elt =
  let masonery = attach ~gutter:10 elt in
  refresh := (fun () -> ignore_result (Lwt_js.yield () >>= fun _ -> ignore (masonery##layout()) ; return_unit)) ;
  !refresh ()
