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


open Bin_prot.Std

type latlng = { lat: float ; lng: float } with bin_io

type t =
  {
    center: latlng;
    ne : latlng ;
    sw : latlng ;
    label : string ;
  } with bin_io

let to_tuple t =
  t.sw.lat, t.ne.lat, t.sw.lng, t.ne.lng


let world = {
  center = { lat = 0.0 ; lng = 0.0 } ;
  ne = { lat = 90.0 ; lng = 180.0 } ;
  sw = { lat = -. 90.0 ; lng = -. 180.0 } ;
  label = "world" ;
}
