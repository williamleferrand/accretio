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
open Js

class type request = object
  method number : js_string t writeonly_prop
  method cvc : js_string t writeonly_prop
  method exp_month : int writeonly_prop
  method exp_year : int writeonly_prop
  method name : js_string t writeonly_prop
  method address_line1 : js_string t writeonly_prop
  method address_line2 : js_string t writeonly_prop
  method address_city : js_string t writeonly_prop
  method address_state : js_string t writeonly_prop
  method address_zip : js_string t writeonly_prop
  method address_country : js_string t writeonly_prop
end

class type card = object
  method name : js_string t opt readonly_prop
  method address_line1 : js_string t opt readonly_prop
  method address_line2 : js_string t opt readonly_prop
  method address_city : js_string t opt readonly_prop
  method address_state: js_string t opt readonly_prop
  method address_zip : js_string t opt readonly_prop
  method address_country : js_string t opt readonly_prop
  method country : js_string t opt readonly_prop
  method exp_month_ : int readonly_prop
  method exp_year_ : int readonly_prop
  method last4 : js_string t opt readonly_prop
  method fingerprint : js_string t opt readonly_prop
  method object_ : js_string t opt readonly_prop
  method type_ : js_string t opt readonly_prop
end

class type error = object
  method message : js_string t readonly_prop
end

class type response = object
  method error : error t opt readonly_prop
  method id : js_string t readonly_prop (* "tok_u5dg20Gra", // String of token identifier, *)
  method card : card t readonly_prop (* { // Dictionary of the card used to create the token
                                        name: null,
                                        address_line1: "12 Main Street",
                                        address_line2: "Apt 42",
                                        address_city: "Palo Alto",
                                        address_state: "CA",
                                        address_zip: "94301",
                                        address_country: "US",
                                        country: "US",
                                        exp_month: 2,
                                        exp_year: 2012,
                                        last4: "4242",
                                        fingerprint: "BzXGiNioaEH4iECL",
                                        object: "card",
                                        type: "Visa"
                                        } *)
  method created : int readonly_prop (* 1360219143, // Integer of date token was created *)
  method currency : js_string t readonly_prop (*"usd", // String currency that the token was created in *)
  method livemode : bool t readonly_prop (* true, // Boolean of whether this token was created with a live or test API key *)
  method object_ : js_string t readonly_prop (* : "token", // String identifier of the type of object, always "token" *)
  method used : bool t readonly_prop (* used : false, *)
end

class type card_create = object
  method createToken : request t -> (int -> response t -> unit) -> js_string t meth
end

class type stripe = object
  method setPublishableKey : js_string t -> unit meth
  method card : card_create meth
end

let stripe () =
  Js.Unsafe.get (Dom_html.window) (Js.string "Stripe")

let load () =
  match Js.Optdef.test (stripe ()) with
    true -> ()
  | false ->
    let script = Dom_html.createScript Dom_html.document in
    script##src <- string "https://js.stripe.com/v2/" ;
    let head = Dom_html.document##head in
    Dom.appendChild head script

let rec exec f =
  let (res, w) = Lwt.task () in
  let rec test () =
    Js.Optdef.case
      (stripe ())
      (fun _ ->
         async (fun () -> Lwt_js.sleep 1.0 >>= fun _ -> test (); return ()))
      (fun stripe ->
         Lwt.wakeup w (f stripe)) in
  test () ;
  res

let set_publishable_key key =
  exec (fun stripe -> stripe##setPublishableKey (string key))

let create_token
    ~number
    ~exp_month
    ~exp_year
    ~cvc () =
  let request = Js.Unsafe.obj [||] in
  request##number <- string number ;
  request##exp_month_ <- string exp_month ;
  request##exp_year_ <- string exp_year ;
  request##cvc <- string cvc ;
  let res, w = Lwt.task () in
  let callback status response = Lwt.wakeup w (status, response) in
  ignore_result
    (exec
       (fun stripe -> let s = stripe##card##createToken (request, callback) in ())) ;
  res
