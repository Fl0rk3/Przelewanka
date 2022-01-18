(*
    Autor: Florian Ficek
    Code review: Maja Tkaczyk
*)

let rec euk a b = 
    if b<>0 then euk b (a mod b) else a
;;

let nwd t = 
    let nwd = Array.fold_left (fun n (x,y) -> euk n x) 0 t
    in nwd
;;

(* na początku sprawdzamy czy da się otrzymać stan końcowy szklanek *)
let ending_condition t = 
    (* pierwszym warunkiem jest sprawdzenie czy każdy wynik można otrzymać *)
    let get_nwd = nwd t in
    let check_nwd = if get_nwd = 0 then true 
    else Array.fold_left (fun check (x,y) -> (check && (y mod get_nwd = 0))) true t
    in 
    (* drugi warunek - optymalizacyjny - sprawdza czy któraś ze szklanek ma być pusta lub pełna *)
    let check_empty_or_full = Array.fold_left (fun check (x,y) -> (check || x=y || y=0)) false t
    in 
    (check_nwd && check_empty_or_full)
;;

let przelewanka tab = 
    let n = Array.length tab in
    if n = 0 then 0 else
    if not (ending_condition tab) then -1  else

    (* tworzenie potrzebnych do obliczenia zadania struktur:
        capacity -> pojemność szklanek
        goal -> finałowa zawartośc wody w szklankach
        visited -> mapa sprawdzonych decyzji
        start -> początkowy stan szklanek
     *)
    let result = ref (-1) in
    let capacity = Array.init n (fun i -> fst tab.(i)) in
    let goal = Array.init n (fun i -> snd tab.(i)) in
    let visited = Hashtbl.create 2137 in
    let start = Array.create n 0 in
    let queue = Queue.create () in
        let step (state, steps) = 
            if not (Hashtbl.mem visited state) then begin
                Queue.add (state, steps) queue;
                Hashtbl.add visited state ();
            end
        in
        step (start, 0);

        (* sprawdzanie wszystkich możliwości, przerwane gdy znajdzie rozwiązanie *)
        while not (Queue.is_empty queue) do
            let (this_state, steps) = Queue.pop queue in
                if this_state = goal then begin
                    result := steps;
                    Queue.clear queue;
                    end
                else begin
                    (* nalewanie wody do pełna do każdej szklanki *)
                    for i = 0 to n-1 do
                        let new_state = Array.copy this_state in
                        new_state.(i) <- capacity.(i);
                        step (new_state, steps+1);
                    done;

                    (* wylewanie wody z każdej szklanki *)
                    for i=0 to n-1 do
                        let new_state = Array.copy this_state in
                        new_state.(i) <- 0;
                        step (new_state, steps+1);
                    done;

                    (* prezelwanie wody między szklankami w każdy sposób *)
                    for i=0 to n-1 do
                        for j=0 to n-1 do
                            if i <> j && this_state.(i) <> 0 && this_state.(j) <> capacity.(j) then
                            let new_state = Array.copy this_state
                            and difference = min this_state.(i) (capacity.(j) - this_state.(j)) in
                            new_state.(i) <- new_state.(i) - difference;
                            new_state.(j) <- new_state.(j) + difference;
                            step (new_state, steps+1);
                        done;
                    done;
                end
            done;
        !result
;;