(* 
 * This file is part of ocaml-ptree.
 *
 * OCaml-ptree is free software: you can redistribute it and/or 
 * modify it under the terms of the GNU Lesser General Public 
 * License as published by the Free Software Foundation.
 *
 * OCaml-ptree is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with ocaml-ptree. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012 Be Sport Inc 580 Howard St San Francisco CA USA
 *)

type 'a t = 'a Radix.t ref

let create () = ref (Radix.create ())

let fold prefix f acc ptree = fst(Radix.lookup !ptree prefix f acc)

let fold_with_max ~max prefix f acc ptree =
  let g (acc,n) v = match n with
    | i when i <= 0 -> (acc,0),true
    | 1 -> ((f acc v),0),true
    | n -> ((f acc v),n-1),false in
  let (res,_) = fold prefix g (acc,max) ptree in
  res

(* insert does not behave properly when inserting several time the same object *)
let insert ptree label value =
  ptree := Radix.bind (!ptree) label value

let rec remove ptree label value =
  ptree := Radix.remove (!ptree) label value

