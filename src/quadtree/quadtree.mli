(*
 * This file is part of Ocaml-quadtree.
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
 * License along with Ocaml-quadtree. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012 Be Sport Inc 580 Howard St San Francisco CA USA
 *)

type instr =
  | Not_deeper of float
  | Stop
  | Continue

type 'a t

type rect = (float*float*float*float)

val init : ?rect:rect -> ?depth:int -> int -> 'a t

val clear : 'a t -> unit

val fold : 'a t -> rect -> ('b -> rect -> 'a -> 'b * instr) -> 'b -> 'b

val insert : 'a t -> float * float -> 'a -> unit

val insert_tailrec : 'a t -> float * float -> 'a -> unit

val remove : 'a t -> float * float -> 'a -> unit

val depth : 'a t -> int
