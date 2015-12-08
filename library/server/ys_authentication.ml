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
open Lwt_preemptive
open Bin_prot.Std
open Cryptokit

type t =
    | LOCKED
    | Password of string * string (* salt * hash *)
    | MFA with bin_io

let default_t = LOCKED

let fresh_salt () =
  Ys_random.random_string (Ys_config.get_int_with_default "salt-size" 128)

let private_key = Ys_config.get_string "private-key"

let hash salt password =
  let salted = password ^ salt in
  let hash = MAC.hmac_sha256 private_key in
  hash_string hash salted

let validate = function
  | LOCKED ->
    (fun _ -> false)
  | MFA ->
    (fun _ -> Lwt_log.ign_error "MFA validate not implemented" ; false)
  | Password (salt, hash_) ->
    (fun password -> hash_ = hash salt password)
