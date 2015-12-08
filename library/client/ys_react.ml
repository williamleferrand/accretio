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
open React
open Eliom_content.Html5
open Eliom_content.Html5.D


module RInt =
struct

  type t =
    {
      s : int S.t ;
      update : int -> unit ;
    }

  let create () =
    let s, update = S.create 0 in
    { s; update }

  let init i =
    let s, update = S.create i in
    { s; update }

  let incr t =
    t.update (S.value t.s + 1)

  let update t =
    t.update

  let decr t =
    t.update (S.value t.s - 1)

  let map t f =
    S.map f t.s

  let iter t f =
    (* I still don't have a clear understanding of what happens here, gc-wise *)
    ignore (map f t)

  let map_in_div ?(a=[]) f t =
    R.node (S.map (fun i -> div ~a [ f i ]) t.s)

  let channel t = t.s

  let to_pcdata t =
    R.node (S.map (fun i -> pcdata (string_of_int i)) t.s)

end


module RListUnique =
  struct

    type ('a, 'b) t =
      {
        s: 'a list S.t ;
        update : 'a list -> unit ;
        extract : 'a -> 'b ;
        keys : ('b, unit) Hashtbl.t ;
      }

    let create ~extract () =
      let s, update = S.create [] in
      {
        s ; update ; extract ; keys = Hashtbl.create 0
      }

    let init ~extract l =
      let s, update = S.create l in
      let keys = Hashtbl.create 0 in
      List.iter (fun e -> Hashtbl.add keys (extract e) ()) l ;
      {
        s ; update ; extract ; keys ;
      }

    let mem t key =
      Hashtbl.mem t.keys key

    let add t e =
      let key = t.extract e in
      if not (Hashtbl.mem t.keys key) then
        begin
          Hashtbl.add t.keys key () ;
          t.update (e :: S.value t.s)
        end

    let replace t l =
      Hashtbl.clear t.keys ;
      List.iter (fun e -> Hashtbl.add t.keys (t.extract e) ()) l ;
      t.update l

    let clear t =
      Hashtbl.clear t.keys ;
      t.update []

    let append t l =
      let l = List.filter (fun e -> not (Hashtbl.mem t.keys (t.extract e))) l in
      List.iter (fun e -> Hashtbl.add t.keys (t.extract e) ()) l ;
      t.update (l @ S.value t.s)

    let remove t key =
      Hashtbl.remove t.keys key ;
      t.update (List.filter (fun e -> t.extract e <> key) (S.value t.s))

    let map_l f t =
      S.map (List.map f) t.s

    let channel t = t.s

    let length t = List.length (S.value t.s)

    let fold_left_in_div ?(hook=(fun () -> ())) ?(a=[]) t f acc =
      R.node
        (S.map
           (function
             | [] -> pcdata ""
             | _ as l ->
               (* hook (); *)
               div ~a (List.fold_left f acc l))
           t.s)

    let map_in_div ?(a=[]) ?(empty_placeholder=[]) f t =
      R.node
        (S.map
           (function
               [] -> div ~a empty_placeholder
             | _ as l -> div ~a (List.map f l)) t.s)

  let map_in_div_head ?(a=[]) ?(empty_placeholder=[]) head f t =
      R.node
        (S.map
           (function
               [] -> div ~a empty_placeholder
             | _ as l -> div ~a (head :: List.map f l)) t.s)

    let map_in_div_ ?(a=[]) ?(empty_placeholder=[]) f t =
      R.node
        (S.map
           (function
               [] -> div ~a empty_placeholder
             | _ as l -> div ~a (List.map (f (mem t)) l)) t.s)

    let map_in_div_with_hook ?(a=[]) ?(empty_placeholder=[]) hook f t =
      R.node
        (S.map
           (function
               [] -> div ~a empty_placeholder
             | _ as l ->
               let d = div ~a (List.map f l) in
               (* hook d; *)
               d)
           t.s)

    let to_list t =
      S.value t.s

  end

module RList =
  struct

    type 'a t =
      {
        s : 'a list S.t ;
        update : 'a list -> unit ;
      }

    let create ?eq () =
      let s, update = S.create ?eq [] in
      { s; update }

    let init l =
      let s, update = S.create l in
      { s; update }

    let reset t =
      t.update []

    let is_empty_signal t =
      S.map (function [] -> true | _ -> false) t.s

    let channel t = t.s

    let add t e =
      t.update (e :: S.value t.s)

    let add_all t e =
      t.update (e @ S.value t.s)

    let add_all_unique noteq t e =
      t.update
        (List.fold_left
           (fun acc e -> e :: (List.filter (noteq e) acc))
           (S.value t.s)
           e)

    let append t e =
      t.update (e :: S.value t.s)

    let append_end t e =
      t.update (S.value t.s @ [ e ])

    let append_unique neq t e =
      t.update (e :: List.filter neq (S.value t.s))

    let append_all t e =
      t.update (S.value t.s @ e)

    let fill t l =
      t.update l

    let update t l =
      t.update l

    let remove t e =
      t.update (List.filter ((<>) e) (S.value t.s))

    let filter t f =
      t.update (List.filter f (S.value t.s))

    let insert cmp t e =
      let rec insert = function
        | [] -> [ e ]
        | t::q when cmp t e < 0 -> t :: insert q
        | t::q -> e :: t :: q
      in
      t.update (insert (S.value t.s))

    let map_in_div ?(a=[]) ?(empty_placeholder=[]) f t =
      R.node
        (S.map
           (function
               [] -> div ~a empty_placeholder
             | _ as l -> div ~a (List.map f l)) t.s)

    let fold_in_div ?(a=[]) f acc t =
      R.node (S.map (fun l -> div ~a (List.fold_left f acc l)) t.s)

    let map_in_div_hd ?(a=[]) hd f t =
      R.node (S.map (fun l -> div ~a (hd :: List.map f l)) t.s)

    let map_in_div_tl ?(a=[]) tl f t =
      R.node (S.map (fun l -> div ~a (List.map f l @ [ tl ])) t.s)

    let map_in_div_rev ?(a=[]) f t =
      R.node (S.map (fun l -> div ~a (List.fold_left (fun acc v -> f v :: acc) [] l)) t.s)

    let map_in_div2 ?(a=[]) f s t =
      R.node (S.l2 (fun l2 l -> div ~a (List.map (f l2) l)) s t.s)

    let is_empty t =
      match S.value t.s with
      | [] -> true
      | _ -> false

    let to_list t = S.value t.s
    let flatten f t =
      List.fold_left
        (fun acc e ->
           match f e with
           None -> acc
           | Some b -> b :: acc)
        []
        (S.value t.s)

    (* mappers, experimental, will change **)

    let iter_s_without_first_update f t =
      let is_start = ref true in
      Lwt_react.S.map_s
        (fun l -> match !is_start with
             true -> is_start := false; return_unit
           | false -> f l) t.s
      >>= fun r -> Lwt_react.S.keep r; return_unit

    let map_s f t = Lwt_react.S.map_s f t.s
    let iter_s f t = Lwt_react.S.map_s f t.s
      >>= fun r -> Lwt_react.S.keep r; return_unit

    let map f t =
      S.map f t.s

    let map_l f t =
      S.map (List.map f) t.s

  end

module R =
  struct

    let node_s s =
      let content, update_content = S.create (div []) in

      Lwt.ignore_result
        (s >>= fun s ->
         Lwt_react.S.map_s
           (fun c -> update_content c; return_unit)
           s);

      R.node content

    include R

  end

open Lwt_stream
open Lwt_react

module Down =
struct

  type 'a t = 'a Lwt_stream.result React.E.t

  let internal_unwrap ( channel, unwrapper ) =
    E.of_stream (Lwt_stream.map_exn channel)

  (* super unsafe ... *)
  let () =
    Eliom_unwrap.register_unwrapper
      (Eliom_unwrap.id_of_int Ys_bandaid.react_down_unwrap_id_int)
      internal_unwrap

end
