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

open Eliom_content.Html5
open Eliom_content.Html5.D
open Vault

}}

{client{

open React


let dom () =
  let regular =
    [
      Raw.img
        ~a:[ a_onclick (fun _ -> Service.goto Service.Landing) ;
             a_class [ "left"; "logo" ]] ~src:("/img/logoaccretio_white_bg.png")
        ~alt:"" () ;

      (* div ~a:[ a_onclick Loader.reset ; a_class [ "left"; "menu" ]] [] ; *)

      (* div ~a:[ a_onclick (fun _ -> Service.goto Service.Dashboard) ;
               a_class [ "left" ; "menu"; "icon-google-circles" ] ] [] ;

       div ~a:[ a_onclick (fun _ -> Service.goto (Service.Search None)) ;
               a_class [ "left" ; "menu"; "icon-search" ] ] [] ;

      *)

      (* R.node (Authentication.connection_status_header ()) ; *)

     (* div ~a:[ a_class [  "left" ; "menu" ; "icon-lifebuoy" ] ;
             a_onclick (fun _ -> Service.goto Service.Feedback) ] [] *)
    ]
  in

  R.node
    (S.map
       (function
         | Connected session when session.member_is_admin ->
           div ~a:[ a_class [ "header" ; "clearfix" ]]
             (regular @ [

                 div ~a:[ a_onclick (fun _ -> Service.goto Service.AdminStats) ;
                          a_class [ "left"; "menu"; "menu-admin" ]]
                   [
                     pcdata "Stats"
                   ] ;
                 div ~a:[ a_onclick (fun _ -> Service.goto (Service.AdminGraphMember None)) ;
                          a_class [ "left"; "menu"; "menu-admin" ]]
                   [
                     pcdata "Members"
                   ] ;
                 div ~a:[ a_onclick (fun _ -> Service.goto (Service.AdminGraphThread None)) ;
                          a_class [ "left"; "menu"; "menu-admin" ]]
                   [
                     pcdata "Threads"
                   ] ;
                 div ~a:[ a_onclick (fun _ -> Service.goto Service.AdminI18n) ;
                          a_class [ "left"; "menu"; "menu-admin" ]]
                   [
                     pcdata "I18n"
                   ] ;
               ])
         | _ ->  div ~a:[ a_class [ "header" ; "clearfix" ]] regular)
       Sessions.session)


}}
