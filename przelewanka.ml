open Array;;

exception Impossible

let rec euk a b = 
    if b<>0 then euk b (a mod b) else a
;;

let nwd t = 
    let nwd = Array.fold_left (fun n (x,y) -> euk n x) 0 t
    in nwd
;;


let warunek_koncowy t = 
    (* pierwszym warunkiem jest sprawdzenie czy każdy wynik można otrzymać *)
    let get_nwd = nwd t in
    let check_nwd = if get_nwd = 0 then true 
    else Array.fold_left (fun check (x,y) -> (check && (y mod get_nwd = 0))) true t
    in 
    (* drugi warunek - optymalizacyjny - sprawdza czy któraś ze szklanek ma być pusta lub pełna *)
    let check_pusta_lub_pelna = Array.fold_left (fun check (x,y) -> (check || x=y || y=0)) false t
    in 
    (check_nwd && check_pusta_lub_pelna)
;;

let przelewanka t = 
    let wynik = ref (-1) in
    if warunek_koncowy t then (!wynik +1)
    else !wynik
;;