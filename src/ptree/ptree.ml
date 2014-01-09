(*
 * This file is part of Ocaml-aliases.
 *
 * Ocaml-quadtree is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * Ocaml-quadtree is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Ocaml-aliases. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012 Be Sport
 *)

type 'a t = 'a Radix.t ref

let create () = ref Radix.empty

let clear r = r:=Radix.empty

let fold ptree prefix f acc = Radix.fold !ptree prefix f acc

let fold_with_max ptree ~max prefix f acc =
  Radix.fold_with_max !ptree ~max prefix f acc

let insert ptree label value =
  ptree := Radix.bind (!ptree) label value

let rec remove ptree label value =
  ptree := Radix.remove (!ptree) label value
