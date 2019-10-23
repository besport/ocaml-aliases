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

type 'a t
(** the type of quadtree *)

type rect = (float*float*float*float)
(** type of the bounds *)

val init : ?rect:rect -> ?depth:int -> int -> 'a t
(** [init cell_size] creates a quadtree with cells not bigger than [cell_size] elements.
    [rect] is use to set the initial bounds. (defaul:(-90., 90.,-180.,180.))
    [dept] is use to bound the depth of the quadtree. (default:15)
*)

val clear : 'a t -> unit
(** [clear t] clears the quadtree [t] *)

(** The type of instruction to control a fold on a quadtree
  - [Continue] keeps folding
  - [Stop] stop folding, returns the accumulator
  - [Not_deeper width] stop folding until the quadtree cell is larger than [width] *)
type instr =
  | Not_deeper of float
  | Stop
  | Continue

val fold : 'a t -> rect -> ('b -> rect -> 'a -> 'b * instr) -> 'b -> 'b
(** [fold t rect f a] fold over bindings inside [rect].
    Use [instr] to control the fold. *)

val insert : 'a t -> float * float -> 'a -> unit
(** [insert t (x,y) v] inserts the value [v] to the key (x,y). *)

val insert_tailrec : 'a t -> float * float -> 'a -> unit
(** Same has above, but tailrec. *)

val remove : 'a t -> float * float -> 'a -> unit
(** [remove t (x,y) v] remove the bind for keys [(x,y)] and value [v] *)

val depth : 'a t -> int
(** [depth t] returns the depth of the quadtree [t] *)
