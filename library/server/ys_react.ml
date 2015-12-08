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


open Lwt_react

module Down =
struct

  type 'a stateful =
      {throttling: float option;
       scope: Eliom_common.client_process_scope option;
       react: 'a E.t;
       name: string option;
       size: int option;}

  type 'a stateless = 'a Eliom_comet.Channel.t

  type 'a t' =
    | Stateful of 'a stateful
    | Stateless of 'a stateless

  type 'a t =
      {t : 'a t';
       react_down_mark: 'a t Eliom_common.wrapper;}

  let wrap_stateful
    {throttling=t; scope; react=e; name; size} =
    let ee =
      (match t with
        | None -> e
        | Some t -> E.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = E.to_stream ee in
    let channel = Eliom_comet.Channel.create ?scope ?name ?size stream in
    (channel,Eliom_common.make_unwrapper (Eliom_wrap.id_of_int Ys_bandaid.react_down_unwrap_id_int))

  let wrap_stateless channel =
    (channel,Eliom_common.make_unwrapper (Eliom_wrap.id_of_int Ys_bandaid.react_down_unwrap_id_int))

  let internal_wrap = function
    | { t = Stateful v } -> wrap_stateful v
    | { t = Stateless v } -> wrap_stateless v

  let react_down_mark () = Eliom_common.make_wrapper internal_wrap

  let stateful ?scope ?throttling ?name ?size (e : 'a E.t) =
    Stateful
      {throttling=throttling;
       scope;
       react=e;
       name=name;
       size=size;
      }

  let stateless ?throttling ?name ?size (e : 'a E.t) =
    let ee =
      (match throttling with
        | None -> e
        | Some t -> E.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = E.to_stream ee in
    Stateless (Eliom_comet.Channel.create ~scope:`Site ?name ?size stream)

  let of_react ?scope ?throttling ?name ?size (e : 'a E.t) =
    let t =
      match scope with
	| Some `Site -> stateless ?throttling ?name ?size e
	| None -> stateful ?throttling ?name ?size e
	| Some ((`Client_process n) as scope) ->
	  stateful ~scope ?throttling ?name ?size e
    in
    { t; react_down_mark=react_down_mark () }

end
