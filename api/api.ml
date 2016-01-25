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

open Printf
open Lwt
open CalendarLib

open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid
open Ys_executor

exception CantDispatchEmail of int

type 'output context =
  {
    society : uid ;
    society_name : string ;
    society_description : string ;

    direct_link : string ;

    stage : string ;

    (* logging utilities *)

    log_info : 'a. ('a, unit, string, unit) Pervasives.format4 -> 'a ;
    log_warning : 'a. ?exn:exn -> ('a, unit, string, unit) Pervasives.format4 -> 'a ;
    log_error : 'a. ?exn:exn -> ('a, unit, string, unit) Pervasives.format4 -> 'a ;

    (* delayed transitions *)

    set_timer : ?label:string -> duration:Calendar.Period.t -> 'output -> unit Lwt.t ;
    cancel_timers : query:string -> unit Lwt.t ;

    (* messaging utilities *)
    message_member : member:uid -> ?attachments:Object_message.attachments -> ?data:(string * string) list -> subject:string -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t ;
    message_supervisor : subject:string -> ?attachments:Object_message.attachments -> ?data:(string * string) list -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t ;
    reply_to : message:uid -> ?data:(string * string) list -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t ;
    forward_to_supervisor : message:uid -> ?data:(string * string) list -> subject:string -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t ;

    (* getting talent & tagging people *)

    search_members : ?max:int -> query:string -> unit -> uid list Lwt.t ;
    tag_member : member:uid -> tags:string list -> unit Lwt.t ;
    untag_member : member:uid -> tags:string list -> unit Lwt.t ;
    check_tag_member : member:uid -> tag:string -> bool Lwt.t ;

    (* local database *)

    set : key:string -> value:string -> unit Lwt.t ;
    get : key:string -> string option Lwt.t ;
    delete : key:string -> unit Lwt.t ;

    (* messages primitives *)

    get_message_content : message:uid -> string Lwt.t ;
    get_message_sender : message:uid -> uid Lwt.t ;
    get_original_message : message:uid -> uid Lwt.t ;
    get_message_data : message:uid -> key:string -> string option Lwt.t ;

    (* member management *)

    add_member : member:uid -> unit Lwt.t ;
    remove_member : member:uid -> unit Lwt.t ;
    is_member : member:uid -> bool Lwt.t ;

    (* payments *)

    request_payment : member:uid -> label:string -> evidence:Object_message.attachments -> amount:float -> on_success:(uid -> 'output) -> on_failure:(uid -> 'output) -> uid option Lwt.t ;
    payment_direct_link : payment:uid -> string Lwt.t ;
    payment_amount : payment:uid -> float Lwt.t ;

  }

module type STAGE_SPECIFICS =
  sig

    type outbound
    val stage : string
    val outbound_dispatcher : int -> outbound -> call

  end

module type STAGE_CONTEXT_FACTORY = functor (S : STAGE_SPECIFICS) -> sig type outbound = S.outbound val context : outbound context end

module type PLAYBOOK =
sig

  val author : string
  val name : string
  val description : string

  val version : int

  (* the stepping *)

  val step : (module STAGE_CONTEXT_FACTORY) -> call -> call option Lwt.t
  val dispatch_message_automatically : uid -> string -> call option Lwt.t
  val dispatch_message_manually : uid -> string -> string -> call option

  (* some static content for the client *)

  val automata : string
  val crontabs : (string * Ys_cron.t) list (* stage * crontab *)

  (* it is much easier to generate these value in camlp4 land than in the other side *)

  val triggers : (([ `Unit | `Int | `Int64 | `Float | `String | `Raw ]) * string) list
  val mailables : string list

  val email_actions : (string * string list) list

  val parameters : (string * string) list

end
