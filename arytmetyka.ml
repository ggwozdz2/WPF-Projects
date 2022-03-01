(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)
(********* Autor pracy: Grzegorz Gwóźdź *********)
(********* Code Reviewer: Filip Nogaj  **********)


(* zdefiniowanie czym jest wartosc*)
type wartosc = float *  float;;
(*w zadaniu wartości od a do b trzymamy jako parę (a,b); 
wartości (-inf, a)u(b, +inf) jako (b, a)*)

(* sprawdzamy czy przedzial jest pusty*)
let czy_przedzial_to_nan a = 
    if classify_float (fst a) == FP_nan then true
    else false;;

(* sprawdzamy czy jeden lub drugi przedzial jest pusty*)
let czy_przedzialy_to_nan a b = 
    if classify_float (fst a) == FP_nan || classify_float (fst b) == FP_nan then true
    else false;;

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)  
let wartosc_dokladnosc x p =
    if x >= 0.0  then  ((x -. (x *. p /. 100.0)), (x +. (x *. p /. 100.0)))
    else  ((x +. (x *. p /. 100.0)), (x -. (x *. p /. 100.0)));;

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y = (x, y);;                        

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x = (x, x);;

(* in_wartosc w x = x \in w *)
let in_wartosc w x = 
    if czy_przedzial_to_nan w then false 
        else if fst w <= snd w then
        if x >= fst w && x <= snd w then true 
        else false
    else
        if x >= fst w || x <= snd w then true
        else false;;

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc w = 
    if czy_przedzial_to_nan w then nan
    else if fst w <= snd w then fst w 
    else neg_infinity;;

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
let max_wartosc w = 
    if czy_przedzial_to_nan w then nan
    else if fst w <= snd w then snd w 
    else  infinity;; 

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc w = 
    if czy_przedzial_to_nan w then nan
    else if fst w <= snd w  then (fst w +. snd w) /. 2.0
    else nan;;
    
(*******************************************************)
(* Operacje arytmetyczne na niedokładnych wartościach. *)
let plus a b =  
    if czy_przedzialy_to_nan a b = true then (nan, nan)
    else if fst a <=snd a && fst b <=snd b then (*oba przedziały da się przedstawić jako przedział od x do y*)
        (fst a +. fst b, snd a +. snd b) 
    else if fst a > snd a && fst b > snd b(*oba przedziały da się przedstawić jako suma przedziałów od (-inf, x)u(y,inf) *)
        then (neg_infinity, infinity) 
    else if fst a <=snd a then (*przypadek że a jest przedziałem od x do y, a b sumą przedziałów*)
        if snd b +. snd a >= fst b +. fst a then (neg_infinity, infinity)
        else (fst b +. fst a, snd b +. snd a) 
    else (*przypadek że a jest sumą przedziałów od x do y a b przedziałem*)
        if snd a +. snd b >= fst a +. fst b then (neg_infinity, infinity)
        else (fst a +. fst b, snd a +. snd b);;


let minus a b =  
    if czy_przedzialy_to_nan a b  = true then (nan, nan)
    else if fst a <=snd a && fst b <=snd b then(*oba przedziały da się przedstawić jako przedział od x do y*)
        (fst a -. snd b, snd a -. fst b) 
    else if fst a > snd a && fst b > snd b then(*oba przedziały da się przedstawić jako suma przedziałów od (-inf, x)u(y,inf) *)
        (neg_infinity, infinity) 
    else if fst a <=snd a then (*przypadek że a jest przedziałem od x do y, a b sumą przedziałów*)
        if fst a -. snd b <= snd a -. fst b then (neg_infinity, infinity)
        else (fst a -. snd b, snd a -. fst b)
    else (*przypadek że a jest sumą przedziałów od x do y a b przedziałem*)
        if fst b -. snd a <= snd b -. fst a then (neg_infinity, infinity)
        else (fst a -. snd b, snd a -. fst b);;

(****************************************************)
(* pomocnicze funkcje używane w mnozeniu i dzieleniu*)

(* mnozenie jesli oba przedziały są postaci (x,y)*)
let mnozenie1 a b =
    let iloczyn1 = if fst a = 0.0 || fst b = 0.0 then 0.0 else fst a *. fst b in
    let iloczyn2 = if fst a = 0.0 || snd b = 0.0 then 0.0 else fst a *. snd b in
    let iloczyn3 = if snd a = 0.0 || fst b = 0.0 then 0.0 else snd a *. fst b in
    let iloczyn4 = if snd a = 0.0 || snd b = 0.0 then 0.0 else snd a *. snd b in                    
    let min_wart = min (min (min iloczyn1 iloczyn2) iloczyn3) iloczyn4  in         
    let max_wart = max (max (max iloczyn1 iloczyn2) iloczyn3) iloczyn4  in 
     (min_wart, max_wart);;

(*kiedy a jest postaci (x,y), a b jest suma przedzialow*)
let mnozenie2 a b = 
    if a = (0.0, 0.0) then (0.0, 0.0)
    else
    let iloczyn1 = if fst a = 0.0 || fst b = 0.0 then 0.0 else fst a *. fst b in
    let iloczyn2 = if fst a = 0.0 then 0.0 else fst a *. infinity in
    let iloczyn3 = if snd a = 0.0 || fst b = 0.0 then 0.0 else snd a *. fst b in
    let iloczyn4 = if snd a = 0.0  then 0.0 else snd a *. infinity in  
    let min1 = min (min (min iloczyn1 iloczyn2) iloczyn3) iloczyn4 in
    let max1 = max (max (max iloczyn1 iloczyn2) iloczyn3) iloczyn4 in
    let iloczyn1 = if fst a = 0.0  then 0.0 else fst a *. neg_infinity in
    let iloczyn2 = if fst a = 0.0 || snd b = 0.0 then 0.0 else fst a *. snd b in
    let iloczyn3 = if snd a = 0.0  then 0.0 else snd a *. neg_infinity in
    let iloczyn4 = if snd a = 0.0 || snd b = 0.0 then 0.0 else snd a *. snd b in  
    let min2 = min (min (min iloczyn1 iloczyn2) iloczyn3) iloczyn4 in 
    let max2 = max (max (max iloczyn1 iloczyn2) iloczyn3) iloczyn4 in
    (* w tym miejscu sprawdzam czy otrzymane przedzialy nie nachodzą na siebie i czy otrzymam (-inf,inf)*)
    if min1 = neg_infinity then 
        if max1 >= min2 then (neg_infinity, infinity) 
        else (min2, max1)
    else 
        if max2 >= min1 then (neg_infinity, infinity)
        else (min1, max2);;

(*kiedy a i b są sumą przedzialow*)
let mnozenie3 a b = 
    let przedzialpom1 = (neg_infinity, snd a) in
    let przedzialpom2 = (fst a, infinity) in
    let przedzial1 = mnozenie2 przedzialpom1 b in 
    let przedzial2 = mnozenie2 przedzialpom2 b in
    if przedzial1 = (neg_infinity, infinity) || przedzial2 = (neg_infinity, infinity) then (neg_infinity, infinity) 
    else
    let mini = max (snd przedzial1) (snd przedzial2) in 
    let maksi = min (fst przedzial1) (fst przedzial2) in 
    if mini >= maksi then (neg_infinity, infinity)
    else (maksi, mini);;

(****************************************************)

let razy a b = 
    if czy_przedzialy_to_nan a b = true then (nan, nan)
    else if fst a <=snd a && fst b <=snd b then mnozenie1 a b(*oba przedziały da się przedstawić jako przedział od x do y*)
    else if fst a <= snd a then mnozenie2 a b (* tylko przedział a da się przedstawił jako przedział od x do y*)
    else if fst b <= snd b then mnozenie2 b a (* tylko przedział b da się przedstawił jako przedział od x do y*)
    else mnozenie3 a b;; (* oba przedziały są sumą przedzialow*)
                             

let podzielic a b = 
    if czy_przedzialy_to_nan a b = true then (nan, nan)
    else if  b = (0.0, 0.0) then (nan, nan) (*dzielenie zero przez zero*) 
    else if fst b = 0.0 || snd b = 0.0 then(* zero jest na krańcach przedziału b*)
        if fst a <= snd a && fst b <= snd b  then
            if fst b = 0.0 then razy a (1. /. snd b, infinity)
            else  razy a (neg_infinity, 1. /. fst b)
        else if fst a <= snd a then 
            if snd b = 0.0 then razy a (neg_infinity, 1. /. fst b)
            else  razy a (1. /. snd b, infinity)
        else if fst b <= snd b then 
            if fst b = 0.0 then razy (1. /. snd b, infinity) a
            else  razy (neg_infinity, 1. /. fst b) a
        else 
            if snd b = 0.0 then razy a (neg_infinity, 1. /. fst b)
            else  razy a (1. /. snd b, infinity)
    else if in_wartosc b 0.0 = false then (*w przedziale b nie wystepuje 0*) 
         let bpom = (1. /. (snd b), 1. /. (fst b)) in
         razy bpom a
    else (* w przedziale b wystepuje 0*)
        let bpom = if b = (neg_infinity, infinity) then (neg_infinity, infinity)
        else (1. /. (snd b), 1. /. (fst b)) in
        razy a bpom

 (* TESTY: 


let a = (2.0, 4.0);;
let b = (1.0, 0.0);;
let c = podzielic a b;; (*-inf, 4*) 

let a = wartosc_od_do 2.0 3.0;;
let b = wartosc_od_do (-2.0) 7.0;;
let c = wartosc_od_do (-1.0) 1.0;;
let d = wartosc_od_do 2.0 2.0;;
let e = wartosc_od_do 9.0 17.0;;
let f = wartosc_od_do (-5.0) (-1.0);;
let g = wartosc_od_do 0.0 0.0;;
let h = wartosc_od_do (-100.0) 200.0;;
let i = wartosc_od_do 42. 69.

let w1 = razy b c;;
assert (in_wartosc w1 8.0 = false);;
let w2 = razy c d;;
let w3 = podzielic w1 w2;;
assert (max_wartosc w3= infinity);;
let w4 = plus g i;;
let w5 = minus g f;;
assert (in_wartosc w5 5.0 = true);;
let w6 = razy w1 w3;;
let w7 = plus c d;;
assert (sr_wartosc w7 = 2.0);;
let w8 = minus w5 d;;
assert (min_wartosc w8 = (-1.0));;
let w9 = podzielic w8 w6;;
let w10 = podzielic w6 w8;;
let w11 = razy w10 w1;;
let w12 = plus w11 w7;;
let w13 = razy e c;;
assert (in_wartosc w13 (-17.1) = false);;
let w14 = razy w13 w7;;
let w15 = podzielic w14 e;;
let w16 = podzielic d w15;;
assert (in_wartosc w16 0.34 = false);;
assert (in_wartosc w16 0.36 = true);;
assert (in_wartosc w16 (-0.34) = false);;
assert (in_wartosc w16 (-0.36) = true);;
let w17 = podzielic a w15;;
let w18 = razy w16 w17;;
let w19 = podzielic w17 w16;;
let w20 = podzielic w16 w17;;
let w21 = podzielic w5 w6;;
let w22 = razy w3 w4;;
let w23 = podzielic w17 a;;
assert (in_wartosc w23 (0.10) = false);;
assert (in_wartosc w23 (0.13) = true);;
let w24 = razy w23 b;;
let w25 = minus w18 w19;;
let w26 = plus w24 w25;;
assert (max_wartosc w26 = infinity);;
*) 