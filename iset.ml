(*
 * ISet - Intervalset sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(************************************************)
(**********  Zadanie Modyfikacja drzew ***********)
(*************************************************)
(********* Autor pracy: Grzegorz Gwóźdź **********)
(****** Code Reviewer: Wojciech Weremczuk ********)

type t =
  | Empty
  | Node of  t * (int * int) * t * int * int
(* lewe poddrzewa, przedział w wierzchołku, prawy wierzchołek, wysokość, liczba elementów w poddrzewie *)

(* wysokość drzewa *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*ile elementow w poddrzewie t*)
let how_many set =
  match set with
  | Empty -> 0
  | Node(_, _, _, _, ile) -> ile


(* dodawanie, widac kiedy przekroczymy max_int *)
  let addition (k1,k2) l r = 
    if k2 = max_int && k1 <= 1 then max_int
    else if k1 = min_int && k2 >= - 1 then max_int
    else if k1 = 0 && k1 = max_int - 1 then max_int
    else if k2 = 0 && k1 = min_int + 1 then max_int
    else if l = max_int || r = max_int then max_int
    else
      let ile_brakuje = max_int - l in
      if ile_brakuje <= r then max_int 
      else let ile_brakuje = ile_brakuje - r in
        if k1 < 0 && k2 > 0 then
         let ile_brak = max_int + k1 - 1 in
         if ile_brak <= k2 then max_int 
          else let ile = k2 - k1 + 1 in
            if ile_brakuje <= ile then max_int else l + r + ile
        else  let ile = k2 - k1 + 1 in
          if ile_brakuje <= ile then max_int else l + r + ile   

(* konstruktor drzewa *)
let make l (k1, k2) r = 
  Node (l, (k1, k2), r, max (height l) (height r) + 1, addition (k1,k2) (how_many l) (how_many r))

(* balansowanie drzewa jesli wysokosci sa inne*)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(*minimalny element*)
let rec min_elt = function
  | Node (Empty, (k1, k2), _, _, _) -> (k1, k2)
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* puste drzewo *)
let empty = Empty;;

(*czy drzewo puste*)
let is_empty x = 
  x = Empty


(*najdalszy przedział który pokrywa się z dodawanym w "lewo" *)
let rec go_left k1 k2 set maks_left=
  match set with
  | Empty -> (maks_left, empty)
  | Node (l, (q1, q2), r, _, _) ->
        if q1 <= k1 && k1 <= q2 || (q2 < max_int && k1 = q2 + 1 )
         then (q1, l)
        else if k1 < q1 then
          go_left k1 k2 l k1
        else let (maks_left, l1) = go_left k1 k2 r maks_left in
         (maks_left, bal l (q1,q2) l1)

(*najdalszy przedział który pokrywa się z dodawanym w "prawo" *)        
let rec go_right k1 k2 set maks_right=
  match set with
  | Empty -> (maks_right, empty)
  | Node (l, (q1, q2), r, _, _) ->
        if q1 <= k2 && k2 <= q2 || (k2 > min_int && k2 = q1 - 1)
          then (q2, r)
        else if k2 > q2 then
          go_right k1 k2 r k2
        else let (maks_right, r1) = go_right k1 k2 l maks_right in
          (maks_right, bal r1 (q1,q2) r) 

(*dodawanie do seta przedziału *)
let rec add (k1, k2) set = 
    match set with
    | Empty -> Node(Empty, (k1, k2), Empty, 1, addition (k1,k2) 0 0 )
    | Node (l, (q1, q2), r, _, _) -> 
      if q1 <= k1 && k2 <= q2 then set
      else if k2 < q1 then 
          if  k2 < q1-1 then
            let nl = add (k1, k2) l in
            bal nl (q1, q2) r
          else 
            let (maks_l, nl) = go_left k1 k2 l k1 in
            bal nl (maks_l, q2) r
      else if k1 > q2 then
          if k1 > q2 + 1 then 
            let nr = add (k1, k2) r in
            bal l (q1, q2) nr
          else 
            let (maks_r, nr) = go_right k1 k2 r k2 in 
            bal l (q1, maks_r) nr
      else if q1 <= k2 && k2 <= q2 then (* w lewo *)
      let (maks_left, lewe) = go_left k1 k2 l k1 in
      bal lewe (maks_left, q2) r 
      else if q1 <= k1 && k1 <= q2 then (* prawo *)
      let (maks_right, prawe) = go_right k1 k2 r k2 in
      bal l (q1, maks_right) prawe
      else (* oba na raz *) 
      let (maks_left, lewe) = go_left k1 k2 l k1 in
      let (maks_right, prawe) = go_right k1 k2 r k2 in
      bal lewe (maks_left, maks_right) prawe
     
(* usun minimalny element *)
let rec remove_min_elt = function
  | Node (Empty, (k1, k2), r, h, ile) ->  
    r
  | Node (l, k, r, _, _) ->    
    bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* zlączanie dwóch drzew *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(* rekurencyjne łączenie dwóch poddrzew i wierzchołka *)
let rec join l (v1, v2) r =
  match (l, r) with
    (Empty, _) -> add (v1, v2) r
  | (_, Empty) -> add (v1, v2) l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr (v1, v2) r) else
      if rh > lh + 2 then bal (join l (v1, v2) rl) rv rr else
      make l (v1, v2) r      

(* podział drzewa na poddrzewa o przedziałach mniejszych od x*)
(* określenie, czy x występuje w drzewie *)
(* oraz na poddrzewo o przedziałach większych od x *)
let split x set =
  let rec loop x = function
    |  Empty ->
        (Empty, false, Empty)
    | Node (l, (k1, k2), r, _, _) ->
        if k1 <= x && x <= k2 then
          if k1 = x && k2 = x then (l, true, r)
          else if k1 = x then (l, true, add (k1+1, k2) r)
          else if k2 = x then (add (k1, k2-1) l, true, r)
          else (add (k1, x-1) l, true, add (x+1, k2) r)
        else if x < k1 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl (k1, k2) r)
        else
          let (lr, pres, rr) = loop x r in (join l (k1, k2) lr, pres, rr)
  in
  loop x set

(*najdalszy przedział który nie pokrywa się z usuwanym w "lewo" *)
let rec go_left2 x y set = 
  match set with
  | Empty -> empty
  | Node(l, (k1, k2), r, _, _ ) ->
        if k1 <= x && x <= k2 then
            if x = k1 then l
            else add (k1, x-1) l
        else if x < k1 then
        go_left2 x y l 
        else bal l (k1, k2) (go_left2 x y r)

(*najdalszy przedział który nie pokrywa się z usuwanym w "prawo" *)
let rec go_right2 x y set =
  match set with
  | Empty -> empty
  | Node(l, (k1, k2), r, _, _ ) ->
        if k2 >= y && y >= k1 then
            if y = k2 then r
            else add (y+1, k2) r
        else if y > k2 then
        go_right2 x y r 
        else 
        bal (go_right2 x y l) (k1, k2) r

(* usunięcie przedzialu x, y z seta *)
let remove (x, y) set =
  let rec loop = function
    | Node (l, (k1, k2), r, _, _) ->
        if k1 <= x && y <= k2 then
          if k1 = x && k2 = y then merge l r
          else if k1 = x then bal l (y+1, k2) r
          else if k2 = y then bal l (k1, y-1) r
          else let lewe = add (k1, x-1) l 
               and prawe = add (y+1, k2) r in
               merge lewe prawe
        else if y < k1 then merge (loop l) (add (k1, k2) r)
        else if x > k2 then merge (add (k1, k2) l) (loop r) 
        else if k1 <= y && y <= k2 then
            let l1 = go_left2 x y l in
            if y = k2 then merge l1 r
            else join l1 (y+1, k2) r
        else if k1 <= x && x <= k2 then
           let r1 = go_right2 x y r in
           if x = k1 then merge l r1 
           else join l (k1, x-1) r1
        else
          let l1 = go_left2 x y l 
          and r1 = go_right2 x y r in
          merge l1 r1
    | Empty -> Empty in
      loop set

(*liczba elementow ponizej x*)
let below x set =
  let rec loop set =
  match set with
  | Empty -> 0
  | Node(l, (k1, k2), r, _, _) ->
    if k1 <= x && x <= k2 then addition (k1, x) (how_many l) 0
    else if x < k1 then loop l
    else addition (k1, k2) (how_many l) (loop r)
  in loop set


(* czy x jest między k1 a k2 *)
let inside k1 k2 x =
    if k1 <= x && x <= k2 then 0
    else if x < k1 then -1
    else 1

(* czy x występuje w drzewie *)
let mem x set =
  let rec loop set = 
    match set with
    | Node (l, (k1, k2), r, _, _) ->
        let c = inside k1 k2 x in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(* iteracja po elementach *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(* fold po elementach *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(* elementy seta *)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set