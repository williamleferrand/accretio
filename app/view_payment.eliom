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

type state = Pending | Failed of string | Paid

type currency = USD

type t =
  {
    uid : uid ;
    label : string ;
    society : View_society.t ;
    state : state ;
    amount : float ;
    currency : currency ;
  }

}}

{server{

let to_state =
  function
    Object_payment.Pending -> Pending
  | Object_payment.Failed reason -> Failed reason
  | Object_payment.Paid -> Paid

let to_currency =
  function
    Object_payment.USD -> USD

let to_view uid =
  lwt label, society, state, amount, currency = $payment(uid)->(label, society, state, amount, currency) in
  lwt society = View_society.to_view society in
  let state = to_state state in
  let currency = to_currency currency in
  return {
    uid ;
    label ;
    society ;
    state ;
    amount ;
    currency ;
  }

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  let state =
    match view.state with
    | Pending -> [ pcdata "Pending" ]
    | Failed msg -> [ pcdata "Failed: " ; pcdata msg ]
    | Paid -> [ pcdata "Paid" ]
  in
  let edit =
    button
      ~a:[ a_button_type `Button ;
           a_onclick (fun _ -> Service.goto (Service.AdminGraphPayment (Some view.uid))) ]
      [ pcdata "Edit" ]
  in
  div ~a:[ a_class [ "view-payment" ]] [
    div ~a:[ a_class [ "label" ]] [ pcdata view.label ] ;
    div ~a:[ a_class [ "amount" ]] [ pcdata (Printf.sprintf "$%.2f" view.amount) ] ;
    div ~a:[ a_class [ "state" ]] state ;
    div ~a:[ a_class [ "edit" ]] [ edit ] ;
  ]

}}
