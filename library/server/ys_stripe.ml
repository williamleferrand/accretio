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

type 'a response = Ok of 'a | Error of exn deriving()

type error =
  {
    message: string option ;
    type_ : string option ;
    code : string option ;
    charge : string option ;
  } deriving(Yojson)

module ReplyCharges =
struct
  type t =
    {
      id : string ;
      created : int64 ;
      livemode : bool ;
      paid : bool ;
      status : string ;
      amount : int ;
      currency : string ;
      refunded : bool;
      failure_message : string option ;
    } deriving (Yojson)
end

module Sources =
struct

  type data =
    {
      last4 : string
    } deriving (Yojson)

  type t =
    {
      total_count : int ;
      has_more : bool ;
      url : string ;
      data : data list ;
    } deriving (Yojson)

end

module ReplyCreateCustomer =
struct
          (*
            {
            "object": "customer",
            "created": 1362537858,
            "id": "cus_1PMFlBUfhF3Sye",
            "livemode": false,
            "description": "payinguser@example.com",
            "active_card": null,
            "email": null,
            "delinquent": false,
            "subscription": null,
            "discount": null,
            "account_balance": 0
            } *)
  type t =
    {
      id : string ;
      sources : Sources.t ;
    } deriving (Yojson)
end

module ReplyRetrieveCustomer =
struct
  type card =
    {
      last4 : string ;
    } deriving (Yojson)

  type t =
    {
      active_card : card option ;
    } deriving (Yojson)
end

type currency = USD deriving (Yojson)

let string_of_currency = function USD -> "usd"


let charges ~customer ~amount ?(currency="usd") ~description () =
  let key = Ys_config.get_string "stripe-secret-key" in
  let headers = Ys_http.basic_auth key "" in
  Ys_http.post_string
    ~https:true
    ~host:"api.stripe.com"
    ~headers
    ~uri:"/v1/charges"
    ~content: [ "customer", customer ;
                "amount", (string_of_int amount) ;
                "currency", currency ;
	        "description", description ]
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  try_lwt
    let charge = ReplyCharges.Yojson_t.from_string reply in
    Lwt.return (Ok charge)
  with e -> Lwt.return (Error e)



let create_customer ~source ~email () =
  let key = Ys_config.get_string "stripe-secret-key" in
  let headers = Ys_http.basic_auth key "" in
  Ys_http.post_string
    ~https:true
    ~host:"api.stripe.com"
    ~headers
    ~uri:"/v1/customers"
    ~content: [ "source", source ; "email", email ]
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  try_lwt
    let customer = ReplyCreateCustomer.Yojson_t.from_string reply in
    Lwt.return (Ok customer)
  with e -> Lwt.return (Error e)

let update_customer ~source ~customer_id () =
  let key = Ys_config.get_string "stripe-secrey-key" in
  let headers = Ys_http.basic_auth key "" in
  Ys_http.post_string
    ~https:true
    ~host:"api.stripe.com"
    ~headers
    ~uri:("/v1/customers/"^customer_id)
    ~content: [ "source", source ]
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  try_lwt
    let customer = ReplyCreateCustomer.Yojson_t.from_string reply in
    Lwt.return (Ok customer)
  with e -> Lwt.return (Error e)

(*
let retrieve_customer ~customer_id =
  let key = Ys_config.get_string "stripe-key-private" in
  let headers = Ys_http.basic_auth key "" in
  Ys_http.get
    ~https:true
    ~host:"api.stripe.com"
    ~headers
    ~uri:("/v1/customers/"^customer_id)
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  let data = Yojson.Safe.from_string reply in
  try_lwt
    let customer = ReplyRetrieveCustomer.HJson_t.from_json data in
    Lwt.return (Ok customer)
  with e -> Lwt.return (Error e)

let charge_customer ~customer_id ~amount ~currency ~description =
  let key = Ys_config.get_string "stripe-key-private" in
  let headers = Ys_http.basic_auth key "" in
  Ys_http.post_string
    ~https:true
    ~host:"api.stripe.com"
    ~headers
    ~uri:"/v1/charges"
    ~content: [ "customer", customer_id ;
                "amount", (string_of_int (int_of_float (100.0 *. amount))) ;
                "currency", (string_of_currency currency) ;
	        "description", description ]
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  let data = Yojson.Safe.from_string reply in
  try_lwt
    let charge = ReplyCharges.HJson_t.from_json data in
    Lwt.return (Ok charge)
  with e -> Lwt.return (Error e)
*)
