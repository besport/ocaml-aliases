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

type 'a tree = N of 'a tree * 'a tree | C of int * ((int*'a) list)

type 'a t = {
  tree:'a tree ref;
  bound:int*int;
  cell_size:int;
}

let average d1 d2 = d1 + (( d2 - d1)/  2)

let init ?(cell_size=10) from_ to_ =
  let l =
    {
      tree=ref (C (0,[]));
      bound=(from_, to_);
      cell_size;
    } in
  l

let clear t =
  t.tree := C(0,[])

let insert tt d e =
  let cons l = (d,e)::l in
  let rec loop d1 d2 t = match t with
    | C (size,l) when size < tt.cell_size -> C (succ size,cons l)
    | C (_,l) ->
      let d3 = average d1 d2 in
      let l1,l2 = List.partition (fun (d,_) -> d < d3) l in
      let l1,l2 =
	      if d < d3
	      then (cons l1),l2
	      else l1,(cons l2) in
      N ((C (List.length l1,l1)),(C (List.length l2,l2)))
    | N(t1,t2) ->
      let d3 = average d1 d2 in
      if d < d3
      then N(loop d1 d3 t1,t2)
      else N(t1,loop d3 d2 t2)
  in
  let d1,d2 = tt.bound in
  tt.tree := loop d1 d2 !(tt.tree)

let remove tt d e =
  let rec loop d1 d2 t = match t with
    | C (size,l) -> let l = List.filter ((<>) (d,e)) l in
		    C (List.length l,l)
    | N(t1,t2) ->
      let d3 = average d1 d2 in
      if d < d3
      then N(loop d1 d3 t1,t2)
      else N(t1,loop d3 d2 t2)
  in
  let d1,d2 = tt.bound in
  tt.tree := loop d1 d2 !(tt.tree)

let fold t d1 d2 f acc =
  let rec next acc = function
    | [] -> acc
    | (b1,b2,t)::nxt -> loop b1 b2 acc nxt t
  and loop b1 b2 acc nxt t = match t with
    | C (_,l) ->
      let rec aux acc = function
	      | [] -> next acc nxt
	      | (d,e)::xs ->
	        let acc,stop = if b1 < d && d < b2 then f acc e else acc,false in
	        if stop then acc else aux acc xs
      in aux acc l
    | N(t1,t2)->
      let b3 = average d1 d2 in
      match d1 > b3 || d2 < b1, d1 > b2 || d2 < b3 with
	      | false, false -> loop b1 b3 acc ((b3,b2,t2)::nxt) t1
	      | false, _ -> loop b1 b3 acc nxt t1
	      | _, false -> loop b3 b2 acc nxt t2
	      | true,true -> next acc nxt
  in
  let b1,b2 = t.bound in
  loop b1 b2 acc [] !(t.tree)
