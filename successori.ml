(* Sollevo un'eccezione se vado fuori dalla scacchiera. *)
exception Fuori_Scacchiera;;

(* "successori" è una funzione che, partendo da una posizione definita dall'utente (una coppia di valori (x,y)), valuta tutte le mosse valide.
   Le mosse valide vengono valutate considerando lo spostamento che il cavallo può fare, ovvero la 'L' e controllando che questo non esca dalla scacchiera,
   la quale dimensione viene sempre definita dall'utente.
   Se esco dalla scacchiera sollevo l'eccezione "Fuori_Scacchiera".
   Quello che avviene in pratica è partire da una lista contenente le 8 funzioni di spostamento (gli 8 possibili salti del cavallo) e
   applicarle una alla volta alla posizione di partenza scorrendo la lista di funzioni iniziale tramite ricorsione.
   Per capire quando ho terminato la lista e per poter sfruttare la ricorsione utilizzo il pattern matching sulle liste *)

let successori n v =
    let mossa0 n (x,y) =
        if x+1<=n && y+2<=n then (x+1,y+2)
        else raise Fuori_Scacchiera in
    let mossa1 n (x,y) =
        if x-1>0 && y+2<=n then (x-1,y+2)
        else raise Fuori_Scacchiera in
    let mossa2 n (x,y) =
        if x+1<=n && y-2>0 then (x+1,y-2)
        else raise Fuori_Scacchiera in
    let mossa3 n (x,y) =
        if x-1>0 && y-2>0 then (x-1,y-2)
        else raise Fuori_Scacchiera in
    let mossa4 n (x,y) =
        if x+2<=n && y+1<=n then (x+2,y+1)
        else raise Fuori_Scacchiera in
    let mossa5 n (x,y) =
        if x-2>0 && y+1<=n then (x-2,y+1)
        else raise Fuori_Scacchiera in
    let mossa6 n (x,y) =
        if x+2<=n && y-1>0 then (x+2,y-1)
        else raise Fuori_Scacchiera in
    let mossa7 n (x,y) =
        if x-2>0 && y-1>0 then (x-2,y-1)
        else raise Fuori_Scacchiera in

    (* La funzione "aux" è la funzione che di fatto scorre la lista chiamando le altre funzioni sulla posizione di partenza scelta *)
    let rec aux = function
        |  [] -> []
        | t::c ->
            (* Nel caso di lista non vuota, la scorro ed applico le mosse in sequenza sulla posizione di partenza.
               Se non esco dalla scacchiera allora includo quella mossa nella lista risultante, altrimenti la scarto e procedo a valutare le mosse successive *)
            try t v::(aux c)
            with Fuori_Scacchiera -> aux c in
    (* Qui è dove realmente chiamo delle funzioni, prima stavo solo dichiarando delle sottofunzioni all'interno della funzione padre "successori" *)
    aux [mossa0 n; mossa1 n; mossa2 n; mossa3 n; mossa4 n; mossa5 n; mossa6 n; mossa7 n]
;;

(* Esempio di chiamate e debug per l'interprete live di OCaml *)
(* #trace successori;; *)
(* successori 5 (3,2);; *)
