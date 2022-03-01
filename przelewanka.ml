(************************************************)
(************  Zadanie Przelewanka. *************)
(************************************************)
(********* Autor pracy: Grzegorz Gwóźdź *********)
(******* Code Reviewer:  *******)
open Queue;;
open Array;;
open List;;
open Hashtbl;;

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;

let przelej konfiguracja tablica = 
  let result = ref [] in 
  (*napelniamy szklanki*)
  for i = 0 to Array.length tablica - 1 do 
    if konfiguracja.(i) <> fst tablica.(i) then
      begin
        let pom = make (Array.length konfiguracja) 0 in
        for i = 0 to Array.length pom - 1 do
          pom.(i) <- konfiguracja.(i); 
        done;
        pom.(i) <- fst tablica.(i);
        result := pom :: !result;   
      end
  done; 
  (*wylewamy ze szklanek*)
  for i = 0 to Array.length tablica - 1 do 
    if konfiguracja.(i) <> 0 then 
      begin 
        let pom = make (Array.length konfiguracja) 0 in
        for i = 0 to Array.length pom - 1 do
          pom.(i) <- konfiguracja.(i); 
        done;
        pom.(i) <- 0;
        result := pom :: !result;        
      end
  done; 
  (*z jednej do drugiej*)
  for i = 0 to Array.length tablica - 1 do
    for j = 0 to Array.length tablica - 1 do
      if i <> j && konfiguracja.(i) <> 0 
            && konfiguracja.(j) <> fst tablica.(j) then
        let ilebrakuje = fst tablica.(j) - konfiguracja.(j) in
        begin 
          let pom = make (Array.length konfiguracja) 0 in
          for i = 0 to Array.length pom - 1 do
            pom.(i) <- konfiguracja.(i); 
          done;
          if ilebrakuje > pom.(i) then
            (pom.(j) <- pom.(j) + pom.(i);
             pom.(i) <- 0;) 
          else
            (pom.(j) <- fst tablica.(j);
             pom.(i) <- pom.(i) - ilebrakuje;);
          result := pom :: !result; 
        end

    done;
  done;     
  !result;;
      
let przelewanka tablica1 = 
  if tablica1 = [||] then 0 else
    let niezerowe = ref 0 in
    let niepuste = ref 0 in
    let gcd1 = ref 1
    and gcd2 = ref 1 in
    for i = 0 to Array.length tablica1 - 1 do
        if fst tablica1.(i) <> 0 then
            begin
                niezerowe := !niezerowe + 1;
                gcd1 := fst tablica1.(i);
            end;
        if snd tablica1.(i) <> 0 then
            begin
                niepuste := !niepuste + 1;
                gcd2 := snd tablica1.(i);
            end
    done;

    let pustepelne = ref false in
    
(*czy wszystkie szklanki nie maja byc puste*)
if !niezerowe = 0 || !niepuste = 0 then 0
else
    let tablica = make !niezerowe (0, 0) in
    let wynik = make !niezerowe 0 in
    let j = ref 0 in
    for i = 0 to Array.length tablica1 - 1 do
        if fst tablica1.(i) <> 0 then
            begin
                let (p, k) = tablica1.(i) in
                if p = k || k = 0 then
                    pustepelne := true;   
                tablica.(!j) <- tablica1.(i);
                wynik.(!j) <- snd tablica1.(i);
                gcd1 := gcd !gcd1 p;
                gcd2 := gcd !gcd2 k;
                j := !j + 1;
            end;
    done;
    (*dwa warunki do przelewania*)
    if !pustepelne = false || !gcd2 mod !gcd1 <> 0 then -1
    else
      let n = Array.length tablica in
      let pierwszy = (make n 0, 0) in
      let konfiguracje = Hashtbl.create n in
      let licznik = ref 1 in 
      Hashtbl.add konfiguracje (make n 0) !licznik ;
      let q = Queue.create () in 
      Queue.add pierwszy q;
      let result = ref (-1) in
      while  not (Queue.is_empty q) && !result = (-1) do
        let (p, odl) = Queue.take q in 
          let wyn = przelej p tablica in
          List.iter (fun e -> 
                        if Hashtbl.mem konfiguracje e = false then
                            begin 
                              if e = wynik then
                                  begin
                                    result := odl + 1;          
                                  end;
                              Queue.add (e, odl + 1) q;
                              licznik := !licznik + 1; 
                              Hashtbl.add konfiguracje e !licznik;
                            end
                      ) wyn;
      done;
      !result;;
