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


open React
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_react

let get_width (): int = Js.Optdef.get (Dom_html.window##innerWidth) (fun _ -> 0)

let create ~a ~a_col ?head ~column_width ~content () =

  let width, update_width = S.create (get_width ()) in
  Dom_html.window##onresize <- Dom_html.handler (fun _ -> update_width (get_width ()); Js._true) ;

  let number_of_columns = S.map (fun width -> max ((width - 20) / column_width) 1) width in

  R.node
    (S.l2
       (fun content number_of_columns ->
          (* let width = number_of_columns * column_width in *)
          let cols = Array.make number_of_columns [] in
          let c = ref 0 in
          (match head with
             None -> ()
           | Some head -> cols.(!c) <- [ head ]; incr c) ;
          List.iter
            (fun elt ->
               let p = !c mod number_of_columns in
               cols.(p) <- elt :: cols.(p) ;
               incr c)
            content ;
          div ~a:(a_style (Printf.sprintf "width: %dpx" ((number_of_columns * column_width))) :: a) (List.map (fun col -> div ~a:a_col (List.rev col)) (Array.to_list cols)))
       content number_of_columns)
