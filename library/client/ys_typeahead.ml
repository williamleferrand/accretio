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


(* this can probably be refactored and made a little bit more elegant .. *)

open Lwt
open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let create ?(placeholder="") source extract format wrapper select =
  let suggestions = RListUnique.create ~extract () in
  let input = input ~a:[ a_placeholder placeholder ] ~input_type:`Text () in

  let grab ev =
    let prefix = Ys_dom.get_value input in
    match ev##keyCode, RListUnique.to_list suggestions with
    | code, [] when code = Keycode.space || code = Keycode.return ->
      let prefix = String.trim prefix in
      if (prefix <> "") then (select (`String prefix)) ;
      RListUnique.clear suggestions ;
      Ys_dom.set_value input "" ;
      true
    | _ ->
      source prefix (fun results -> RListUnique.replace suggestions results) ;
      true
  in

  let finalize () =
    match RListUnique.to_list suggestions with
    | [] ->
      let prefix = Ys_dom.get_value input in
      let prefix = String.trim prefix in
      if (prefix <> "") then (select (`String prefix)) ;
      RListUnique.clear suggestions ;
      Ys_dom.set_value input "" ;
    | _ -> ()
  in

  let select elt =
    RListUnique.clear suggestions ;
    Ys_dom.set_value input "" ;
    select (`Elt elt) ;
    Ys_dom.focus input ;
  in

  finalize, div ~a:[ a_class [ "typeahead" ]] [
    wrapper input ;
    R.node
      (S.map
         (function results ->

           let elts = List.map (fun v -> Some v, format v) results in

           let link (_, before) (elt, now) (_, after) other =
             Manip.Ev.onkeydown
               now
               (fun ev ->
                  match ev##keyCode with
                | code when code = Keycode.up ->
                  Ys_dom.focus before ;
                  false
                | code when code = Keycode.down ->
                  Ys_dom.focus after ;
                  false
                | _ -> match elt with
                    Some elt -> select elt ; false
                  | None -> true) ;
             match elt with
               None -> ()
             | Some elt ->
               Manip.Ev.onclick
                 now
                 (fun ev -> select elt ; false)
           in

           (match elts with
              [] -> Manip.Ev.onkeyup input grab
            | a :: [] ->
              link a (None, input) a grab ;
              link (None, input) a (None, input) (fun _ -> true)
            | a :: tl as l ->
             link (List.hd (List.rev tl)) (None, input) a grab ;
             let rec connect = function
               | a :: b :: c :: d ->
                 link a b c (fun _ -> true) ;
                 connect (b :: c :: d)
               | a :: b :: c ->
                 link a b (None, input) (fun _ -> true)
               | _ -> ()
             in
             connect ((None, input) :: l)) ;

           div ~a:[ a_class [ "autocomplete-suggestions" ]] (List.map snd elts))

         (RListUnique.channel suggestions))
  ]
