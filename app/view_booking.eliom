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

type status = Pending | Confirmed of View_payment.t

type t =
  {
    uid : uid ;
    member : View_member.t ;
    count : int ;
    cost : float ;
    status : status ;
  }

}}

{server{

let to_view uid =
  lwt member, count, status, cost = $booking(uid)->(member, count, status, cost) in
  lwt member = View_member.to_view member in
  lwt status =
    match status with
      Object_booking.Pending -> return Pending
    | Object_booking.Confirmed uid ->
      lwt payment = View_payment.to_view uid in
      return (Confirmed payment)
  in
  return {
    uid ;
    member ;
    count ;
    cost ;
    status ;
  }

}}

{client{

open Printf
open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  let status =
    match view.status with
    | Pending -> [ pcdata "Pending" ]
    | Confirmed payment -> [ pcdata "Confirmed" ;
                             View_payment.format payment ]
  in
  div ~a:[ a_class [ "view-booking" ]] [
    div ~a:[ a_class [ "member" ]] [
      View_member.format view.member
    ] ;
    div ~a:[ a_class [ "count" ]] [
      pcdata (string_of_int view.count)
    ] ;
    div ~a:[ a_class [ "cost" ]] [
      pcdata (sprintf "%.2f" view.cost)
    ] ;
    div ~a:[ a_class [ "status" ]] status
  ]

}}
