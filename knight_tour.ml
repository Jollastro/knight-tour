(* This is just a custom function to print out all the elements of the list. *)
let rec print_list = function 
      [] -> print_endline ""
    | (t1, t2)::c -> print_string "("; print_int t1 ; print_string ","; print_int t2; print_string ") "; print_list c
;;


(* L'ordinamento viene fatto sulle soluzioni parziali che vengono man mano generate partendo da una posizione iniziale.
   La generazione viene fatta utilizzando la funzione "successori" e la lista delle soluzioni parziali viene mantenuta ordinata seguendo il criterio della
   decrescenza, ovvero la soluzione parziale più corta viene mantenuta in prima posizione.
   In questo modo riduco la mole di calcoli e soprattutto favorisco percorsi che privilegiano i bordi anziché le zone centrali della scacchiera,
   così da evitare di creare zone inaccessibili *)

let salto_cavallo inizio n =

    (*Euristica Hill-Climbing Warnsdorff*) 

    (* La funzione "confronto" mi permette di confrontare le soluzioni parziali contenute nella lista e ordinarle secondo l'euristica di Warnsdorff.
       Il confronto viene fatto sulla lista dei successori delle mie soluzioni parziali.
       Le liste di successori che genero vengono filtrate prima di venire confrontate, in modo da evitare di inserirvi posizioni già visitate. *)
    let confronto p1 p2 = 
        let f1 = List.filter (fun x -> not(List.mem x p1)) (Successori.successori n (List.hd p1)) in
        let c1 = List.length(f1) in

        let f2 = List.filter (fun x -> not(List.mem x p2)) (Successori.successori n (List.hd p2)) in    
        let c2= List.length(f2) in

        if c1>c2 then 1
        else if c1=c2 then 0
        else -1 in

    (* La funzione "estendi" permette di incrementare le soluzioni parziali generando nuovi successori (filtrando le mosse già fatte) e
       inserendo le nuove posizioni generate alla testa delle soluzioni parziali di partenza. *)
    let estendi cammino =
        List.map (fun x -> x::cammino) (List.filter (fun x -> not(List.mem x cammino)) (Successori.successori n (List.hd cammino))) in

    (* La parte a destra di filter restituisce i successori. Ciascun successore viene filtrato con list filter,
       e genero una lista di elementi non presenti all'interno di cammino. Con list map gli n elementi della lista generati precedentemente da filter,
       generano n liste contenenti se stesso come testa e come coda il cammino. *)

    (* Nel caso di cammino=[(1,1)], otterremo estendi = [[(2,3);(1,1)];[(3,2);(1;1)]] *)


    (* La funzione "corretto" invece controlla se un cammino rappresenta la soluzione hamiltoniana nel grafo indotto dalle mosse del cavallo:
       esso deve toccare tutte le n^2 caselle della scacchiera.
       Se il cammino non è di dimensione n*n, allora estendo il cammino. *)
    let corretto cammino = (List.length cammino)=(n*n) in

    (* La funzione "aux" è quella che di fatto richiama le altre funzioni in modo ricorsivo utilizzando il pattern matching. *)
    let rec aux = function
          [] -> raise Not_found
        | t::c ->
            if corretto t then print_list (List.rev t)
            else aux ((List.sort confronto (estendi t))@c) in
    aux [[inizio]]
;;

salto_cavallo ((int_of_string Sys.argv.(1)), (int_of_string Sys.argv.(2))) (int_of_string Sys.argv.(3));;
