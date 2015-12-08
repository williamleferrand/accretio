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

open Camlp4
open PreCast
open Ast

open Syntax

type label = string

type alias = Strings of label
           | String of label
           | StringFun of label * expr
           | StringAutocomplete of label
           | PlainText of label
           | PlainTextAndString of label
           | PlainTextEdge of label * string
           | Spatial of label

let alias = Gram.Entry.mk "alias"
let aliases = Gram.Entry.mk "aliases"
let uniques = Gram.Entry.mk "uniques"
let required = Gram.Entry.mk "required"
let builders = Gram.Entry.mk "builders"
let recursive = Gram.Entry.mk "recursive"
let options = Gram.Entry.mk "options"


EXTEND Gram

alias:
    [
      [
        "`"; "String"; label = LIDENT -> String label
      | "`"; "StringFun"; label = LIDENT ; constr = expr -> StringFun (label, constr)
      | "`"; "Strings"; label = LIDENT -> Strings label
      | "`"; "StringAutocomplete"; label = LIDENT -> StringAutocomplete label
      | "`"; "PlainTextAndString"; label = LIDENT -> PlainTextAndString label
      | "`"; "PlainTextEdge"; label = LIDENT ; constr = LIDENT -> PlainTextEdge (label, constr)
      | "`"; "PlainText"; label = LIDENT -> PlainText label
      | "`"; "Spatial"; label = LIDENT -> Spatial label
      ]
    ];

uniques:
  [
    [
      "uniques"; "=" ; "[" ;
      l = LIST1 [ u = LIDENT -> u ] SEP ";" ; "]"; OPT ";" -> l
    ]
  ];

aliases:
  [
    [
      "aliases"; "=" ; "[" ;
      l = LIST1 [ a = alias -> a ] SEP ";" ; "]"; OPT ";" -> l
    ]
  ];

required:
  [
    [
      "required"; "=" ; "[" ;
      l = LIST1 [ r = LIDENT -> r ] SEP ";" ; "]"; OPT ";" -> l
    ]
  ];

recursive:
  [
    [
      "recursive"; "=" ; "[" ;
      l = LIST1 [ r = LIDENT -> r ] SEP ";" ; "]"; OPT ";" -> l
    ]
  ];

builders:
  [
    [
      "builders"; "=" ; "[" ;
      l = LIST1 [ "("; r = LIDENT ; "," ;  builder = expr ; ")" -> (r, builder) ] SEP ";" ; "]"; OPT ";" -> l
    ]
  ];

options:
  [
     [  "{" ;
        a = OPT aliases ;
        r = OPT required ;
        u = OPT uniques ;
        b = OPT builders ;
        c = OPT recursive ;
        "}"  ->
        a, r, u, b, c
     ]
  ];

END
