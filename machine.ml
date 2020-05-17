type adress =
  | Reg of int
  | Mem of int
;;

type instruction =
  | INIT of adress * data (* Initialise la case adress du registre à data *)
  | LOAD of adress * adress (* charge de Mem vers Reg *)
  | SAVE of adress * adress (* sauvegarde de Reg vers Mem *)
  | IADD of adress * adress * adress (* addition d'entiers des 2e et 3e adress dans la 1ere *)
  | FADD of adress * adress * adress (* de même pour les float *)
  | INC of adress (* Incrémente un entier *)
  | IMULT of adress * adress * adress (* Multiplie deux entiers *)
  | FMULT of adress * adress * adress (* De même avec des float *)
  | ISUB of adress * adress * adress (* soustrait deux entiers *)
  | FSUB of adress * adress * adress (* de même pour deux float *)
  | IDIV of adress * adress * adress (* divise deux entiers *)
  | FDIV of adress * adress * adress (* de même pour deux float *)
  | IEQ of adress * adress * adress (* Met dans la 1ere si les deux entiers sont égaux*)
  | FEQ of adress * adress * adress (* De même avec des floats *)
  | ILT of adress * adress * adress (* Stocke si le premier int < le deuxième *)
  | FLT of adress * adress * adress (* De même avec des flaots *)
  | IIN of adress (* Lit un entier et le met dans le registre *)
  | FIN of adress (* De même pour un float *)
  | CIN of adress (* De même pour un char *)
  | IOUT of adress (* Sort un entier *)
  | FOUT of adress (* De même pour un float *)
  | COUT of adress (* De même pour un char *)
  | HALT (* signale à la machine qu'elle doit s'arrêter *)
  | JUMP of adress (* change le pointeur à une autre adresse mémoire *)
  | JUMPIF of adress * adress * adress (* Si le bool en 1ere adresse vrai, saute à la 2eme adresse. Sinon, saute à la 3eme *)
  | OR of adress * adress * adress (* stocke bool1 || bool2 *)
  | AND of adress * adress * adress (* De même avec && *)
  | NOT of adress * adress (* stocke not(bool) *)
;;

type data =
  | F of float
  | I of int
  | C of string (* sera du char dans le futur *)
  | B of bool
  | E (* ie. Empty *)
  | N of instruction
;;


type machine = {mutable mem : data array; mutable reg : data array; pointer : int ref};;


let run_instruction mem reg instruction running pointer =
  let n = Array.length mem in
  let m = Array.length reg in
  match instruction with
  | INIT (Reg r, d) -> if (r >= m) then failwith "Index error in INIT"
                            else
                              reg.(r) <- d
  | LOAD (Mem a, Reg b) -> if (a < n && b < m) then reg.(b) <- mem.(a) else failwith "Index error in LOAD"
  | SAVE (Reg a, Mem b) -> if (a < m && b < n) then mem.(b) <- reg.(a) else failwith "Index error in SAVE"
  | IADD (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in IADD"
                      else begin
                          match reg.(a), reg.(b) with
                          | I int1, I int2 -> reg.(r) <- I (int1 + int2)
                          | _, _ -> failwith "Type error in IADD"
                        end
  | FADD (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in FADD"
                      else begin
                          match reg.(a), reg.(b) with
                          | F float1, F float2 -> reg.(r) <- F (float1 +. float2)
                          | _, _ -> failwith "Type error in FADD"
                        end
  | INC (Reg a) -> if (a >= m) then failwith "Index error in INC"
               else begin
                   match reg.(a) with
                   | I int1 -> reg.(a) <- I (int1 + 1)
                   | _ -> failwith "Type error in INC"
                 end
  | ISUB (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in ISUB"
else begin
                          match reg.(a), reg.(b) with
                          | I int1, I int2 -> reg.(r) <- I (int1 - int2)
                          | _, _ -> failwith "Type error in ISUB"
                                    end
  | FSUB  (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in FSUB"
                      else begin
                          match reg.(a), reg.(b) with
                          | F float1, F float2 -> reg.(r) <- F (float1 -. float2)
                          | _, _ -> failwith "Type error in FSUB"
                                     end
  | IDIV (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in IDIV"
                      else begin
                          match reg.(a), reg.(b) with
                          | I int1, I int2 -> reg.(r) <- I (int1 / int2)
                          | _, _ -> failwith "Type error in IDIV"
                                    end
  | FDIV (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in FDIV"
                      else begin
                          match reg.(a), reg.(b) with
                          | F float1, F float2 -> reg.(r) <- F (float1 /. float2)
                          | _, _ -> failwith "Type error in FDIV"
                                    end
  | IMULT (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in IMULT"
                      else begin
                          match reg.(a), reg.(b) with
                          | I int1, I int2 -> reg.(r) <- I (int1 * int2)
                          | _, _ -> failwith "Type error in IMULT"
                                    end
  | FMULT (Reg r, Reg a, Reg b) -> if (a >= m || b >= m) then failwith "Index error in FMULT"
                      else begin
                          match reg.(a), reg.(b) with
                          | F float1, F float2 -> reg.(r) <- F (float1 *. float2)
                          | _, _ -> failwith "Type error in MULT"
                                    end
  | IIN (Reg r) -> if (r >= m) then failwith "Index error in IIN"
                   else begin
                       reg.(r) <- I (read_int ());
                     end
  | FIN (Reg r) -> if (r >= m) then failwith "Index error in FIN"
                   else begin
                       reg.(r) <- F (read_float ());
                     end
  | CIN (Reg r) -> if (r >= m) then failwith "Index error in IIN"
                   else begin
                       reg.(r) <- C (read_line ());
                     end
  | IOUT (Reg r) -> if (r >= m) then failwith "Index error in IOUT"
                   else begin
                        match reg.(r) with
                        | I int1 -> print_int int1
                        | _ -> failwith "Type error in IOUT"
                     end
  | FOUT (Reg r) -> if (r >= m) then failwith "Index error in FOUT"
                   else begin
                       match reg.(r) with
                        | F float1 -> print_float float1
                        | _ -> failwith "Type error in FOUT"
                     end
  | COUT (Reg r) -> if (r >= m) then failwith "Index error in COUT"
                   else begin
                       match reg.(r) with
                        | C string1 -> print_string string1
                        | _ -> failwith "Type error in COUT"
                      end
  | IEQ (Reg r, Reg a, Reg b) -> if (r >= m || a >= m || b >= m) then failwith "Index Error in IEQ"
                                 else begin
                                     match reg.(a), reg.(b) with
                                     | I int1, I int2 -> reg.(r) <- B (int1 = int2)
                                     | _, _ -> failwith "Type error in IEQ"
                                   end
  | FEQ  (Reg r, Reg a, Reg b) -> if (r >= m || a >= m || b >= m) then failwith "Index Error in FEQ"
                                 else begin
                                     match reg.(a), reg.(b) with
                                     | F float1, F float2 -> reg.(r) <- B (float1 = float2)
                                     | _, _ -> failwith "Type error in FEQ"
                                    end
  | ILT  (Reg r, Reg a, Reg b) -> if (r >= m || a >= m || b >= m) then failwith "Index Error in ILT"
                                 else begin
                                     match reg.(a), reg.(b) with
                                     | I int1, I int2 -> reg.(r) <- B (int1 < int2)
                                     | _, _ -> failwith "Type error in ILT"
                                   end
  | FLT  (Reg r, Reg a, Reg b) -> if (r >= m || a >= m || b >= m) then failwith "Index Error in FLT"
                                 else begin
                                     match reg.(a), reg.(b) with
                                     | F float1, F float2 -> reg.(r) <- B (float1 < float2)
                                     | _, _ -> failwith "Type error in FLT"
                                    end
  | JUMP (Mem r) -> pointer := r
  | JUMPIF (Reg r, Mem a, Mem b) -> if (a >= n || b >= n) then failwith "Index error in JUMPIF"
                                    else begin
                                      match reg.(r) with
                                      | B bool1 -> pointer := if bool1 then a else b
                                      | _ -> failwith "Type error in JUMPIF"
                                      end
  | OR (Reg r, Reg a, Reg b) -> if (a >= m || b >= m || r >= m) then failwith "Index error in OR"
                                    else begin
                                      match reg.(a), reg.(b) with
                                      | B bool1, B bool2 -> reg.(r) <- B (bool1 || bool2)
                                      | _, _ -> failwith "Type error in OR"
                                  end
  | AND (Reg r, Reg a, Reg b) -> if (a >= m || b >= m || r >= m) then failwith "Index error in AND"
                                    else begin
                                      match reg.(a), reg.(b) with
                                      | B bool1, B bool2 -> reg.(r) <- B (bool1 && bool2)
                                      | _, _ -> failwith "Type error in AND"
                                   end
  | NOT (Reg r, Reg a) -> if (a >= m || r >= m) then failwith "Index error in NOT"
                                    else begin
                                      match reg.(a) with
                                      | B bool1 -> reg.(r) <- B (not bool1)
                                      | _ -> failwith "Type error in OR"
                                      end
  | HALT -> print_string "Program halted succesfully"; running := false
  | _ -> print_int !pointer; failwith "Unknown instruction or mismatched types"
;;
  
let empty_machine () = {mem = Array.make 10 E; reg = Array.make 10 E; pointer = ref 0};;                       
let run_bis machine running =
  match machine.mem.(!(machine.pointer)) with
    | N instr -> run_instruction machine.mem machine.reg instr running machine.pointer;
    | _ -> ()
;;


let run (machine : machine) =
  let running = ref true in (* Change lorsque l'on rencontre une instruction HALT *)
  let old_pointer = ref !(machine.pointer) in
  while !running do
    run_bis machine running;
    if !old_pointer = !(machine.pointer) then machine.pointer := !(machine.pointer) + 1;
    old_pointer := !(machine.pointer);
  done
;;


(* Machine qui additionne deux entiers entrés *)
let machsimple = empty_machine ();; 

machsimple.mem <- [|
             N (IIN (Reg 0));
             N (IIN (Reg 1));
             N (IADD (Reg 2, Reg 1, Reg 0));
             N (IOUT (Reg 2));
             N HALT;
           |];;



(* machine qui calcule la factorielle d'un entier *)

let machFact = empty_machine ();;

machFact.mem <- [|
                 I 1;
                 N (IIN (Reg 0));
                 N (INC (Reg 0));
                 N (LOAD (Mem 0, Reg 1));
                 N (LOAD (Mem 0, Reg 2));
                 N (ILT (Reg 3, Reg 2, Reg 0));
                 N (JUMPIF (Reg 3, Mem 7, Mem 10));
                 N (IMULT (Reg 1, Reg 1, Reg 2));
                 N (INC (Reg 2));
                 N (JUMP (Mem 5));
                 N (IOUT (Reg 1));
                 N (HALT)
               |]
;;

run machFact;;

machFact;;

let val1 = read_int ();;

let read_int () =
  let str = read_line () in
  int_of_string (if str.[(String.length str) - 1] = ';' then String.sub str 0 (String.length str - 2) else str)
;;


(* COMPILATEUR *)
let strip line =
  let n = String.length line in
  let i = ref (n - 1) in
  while (!i >= 0) && (line.[!i] = '\n' || line.[!i] = ' ' || line.[!i] = ';') do
    i := !i - 1;
  done;
  let j = ref 0 in
  while (!j <= !i) && (line.[!j] = ';' || line.[!j] = ' ' || line.[!j] = '\n') do
    j := !j + 1;
  done;
  
  String.sub line !j (!i + 1 - !j)
;;



let slice program = (* Découpe le programme en une liste de lignes *)
  let res = ref [] in (* Liste des lignes *)
  let n = String.length program in
  let old_i = ref 0 in
  for i = 0 to (n - 1) do
    if program.[i] = ';' then begin
        res := (strip (String.sub program !old_i (i - !old_i))) :: !res;
        old_i := i;
      end
  done;
  List.rev !res
;;

slice "int i = 0; 
       float a = 0.342;
       while i < 0 do;
       isse; dsiqo; kl";;

let rec length liste =
  match liste with
  | [] -> 0
  | _ :: q -> 1 + length q
;;