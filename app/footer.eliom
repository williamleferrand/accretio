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
  footer ~a:[ a_class [ "footer" ]] [
    Raw.a ~a:[ a_href "mailto:hi@accret.io" ] [ pcdata "Contact" ] ;
    pcdata " - " ;
    Raw.a ~a:[ a_href "https://github.com/accretio" ] [ pcdata "Github" ] ;
    pcdata " - " ;
    Raw.a ~a:[ a_href "https://medium.com/@wleferrand" ] [ pcdata "Blog" ] ;
    R.node (Authentication.connection_status_footer ())
  ]

}}
