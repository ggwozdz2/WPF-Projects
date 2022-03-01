(*************************************************)
(*************** Zadanie Origami *****************)
(*************************************************)
(********* Autor pracy: Grzegorz Gwóźdź **********)
(******** Code Reviewerka: Emilia Dębicka ********)

open List;;
(*punkt na płaszczyźnie*)
type point = float * float

(*ile razy kartkę przebija igła*)
type kartka = point -> int

(*stala do porownywania wynikow*)
let stala = 0.0000000001

(*prostokątna kartka*)
let prostokat (x1, y1) (x2, y2) =
  fun (x, y) ->
    if x >= x1 && x <= x2 && y >= y1 && y <= y2
    then 1
    else 0;;

(*pomocnicza procedura do rownania okregu*)
let mnozenie m1 m2 =
  (m1 -. m2) *. (m1 -. m2);;

(*okrągła kartka*)
let kolko (x1, y1) r =
  fun (x, y) ->
    if sqrt(mnozenie x1 x +. mnozenie y1 y) <= (r +. stala)  
    then 1
    else 0;;

(*skalowanie współrzędnych, nie wpływa na iloczyn wektorowy*)
let recalc (x1, y1) (x2, y2) =
  (x1 /. 10., y1 /. 10., x2 /. 10., y2 /. 10.);;

(*iloczyn wektorowy*)
let iloczyn_wek p1 p2 =
  let (x1, y1, x2, y2) = recalc p1 p2 in
  let w1 = x1 *. y2 
  and w2 = x2 *. y1 in
    w1 -. w2;;

(*okreslenie po ktorej stronie znajduje sie punkt*)
let strona (x1, y1) (x2, y2) (p1, p2) =
  let wektor1 = (x2 -. x1, y2 -. y1)
  and wektor2 = (p1 -. x1, p2 -. y1) in
  let iloczyn = iloczyn_wek wektor1 wektor2 in
  if  iloczyn > 0.0 +. stala
    then 1
  else if iloczyn < 0.0 -. stala
    then -1
  else 0;;

(*postac kierunkowa prostej*)
(*wykonujemy tylko dla niepionowych i niepoziomych*)
let postac_kier (x1, y1) (x2, y2) =
  let a = (y2 -. y1) /. (x2 -. x1) in
  let b = y1 -. a *. x1 in
  (a, b);;

(*prosta prostopadla*)
(*wykonujemy tylko dla niepionowych i niepoziomych*)
let prostopadla a (x, y) =
  let a1 = (-1.) /. a in
  let b1 = y -. a1 *. x in
  (a1, b1);;

(*przeciecie dwoch prostych*)
let przeciecie (a1, b1) (a2, b2) =
  let x = (b2 -. b1) /. (a1 -. a2) in
  let y = a1 *. x +. b1 in
  (x, y);;

(*odbijanie punktu wzgledem prostej*)
let odbicie p1 p2 (x1, y1) =
  if fst p1 <> fst p2 && snd p1 <> snd p2 then
    let (a,b) = postac_kier p1 p2 in
    let (a1, b1) = prostopadla a (x1, y1) in
    let (x, y) = przeciecie (a1, b1) (a, b) in
    let x2 = 
      2. *. x -. x1 in
    let y2 =
      2. *. y -. y1 in
    (x2, y2)
  else if fst p1 = fst p2 then
    let odlegl = x1 -. fst p1 in
    (fst p1 -. odlegl, y1)
  else 
    let odlegl = y1 -. snd p1 in
    (x1, snd p1 -. odlegl);;

(*funkcja zginająca kartkę*)        
let zloz p1 p2 (k : kartka) =
  fun p ->
    let iloczyn = strona p1 p2 p in
    match iloczyn with
    | 1 -> k p + k (odbicie p1 p2 p)
    | 0 -> k p
    | _ -> 0;;

(*sekwencja złożeń kartki*)
let skladaj lista k =
  fold_left (fun a (x, y) -> zloz x y a) k lista;;