let read_int () =
  let str = read_line () in
  int_of_string (if str.[(String.length str) - 1] = ';' then String.sub str 0 (String.length str - 2) else str)
;;

let rec length liste =
  match liste with
  | [] -> 0
  | _ :: q -> 1 + length q
;;


type adress =
  | Reg of int
  | Mem of int
  | E
;;

type instruction =
  | INIT of adress * data (* Initialise la case adress du registre à data *)
  | LOAD of adress * adress (* charge de Mem vers Reg *)
  | SAVE of adress * adress (* sauvegarde de Reg vers Mem *)
  | IADD of adress * adress * adress (* addition d'entiers des 2e et 3e adress dans la 1ere *)
  | FADD of adress * adress * adress (* de même pour les float *)
  | INC of adress (* Incrémente un entier *)
  | DEC of adress (* Décrémente un entier *)
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
  | PRINT of adress (* imprime une donnée de type qcq *)


and data =
  | F of float
  | I of int
  | C of string (* sera du char dans le futur *)
  | B of bool
  | E (* ie. Empty *)
  | N of instruction
  | A of adress (* Sert à faire des pointeurs -> listes chaînées *)
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
  | DEC (Reg a) -> if (a >= m) then failwith "Index error in INC"
               else begin
                   match reg.(a) with
                   | I int1 -> reg.(a) <- I (int1 - 1)
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
  | PRINT (Reg r) -> if r >= m then failwith "Index error in PRINT"
                     else
                       begin
                       match reg.(r) with
                       | I int1 -> print_int int1; print_newline ()
                       | F float1 -> print_float float1; print_newline ()
                       | C str1 -> print_string str1; print_newline ()
                       | B bool1 -> if bool1 then print_string "True" else print_string "False"; print_newline ()
                       | A (Reg r) -> print_string ("Pointer Reg " ^ string_of_int r); print_newline();
                       | A (Mem r) -> print_string ("Pointer Mem " ^ string_of_int r); print_newline();
                       | _ -> print_newline ()
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

(* run machFact;;*)

machFact;;


(* COMPILATEUR *)
let rec remove_comments prog =
  let n = String.length prog in
  let indiceD = ref (-1) in
  let indiceF = ref (-1) in
  for i = 0 to (n - 2) do
    if (!indiceD = -1) && (prog.[i] = '/') && (prog.[i + 1] = '*') then
      indiceD := i;
    if (!indiceF = -1) && (prog.[i] = '*') && (prog.[i + 1] = '/') then
      indiceF := i;
  done;
  if !indiceD != -1 then
    begin
      let progM = 
      if !indiceF != -1 then
          (String.sub prog 0 (!indiceD - 1)) ^ (String.sub prog (!indiceF + 2) (n - !indiceF - 2))
        else
          (String.sub prog 0 (!indiceD - 1))
      in
      remove_comments progM;
    end
  else
    prog
;;
                     
  
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

let remove_empty array =
  let n = Array.length array in
  let compteur = ref 0 in
  for i = 0 to (n - 1) do
    if array.(i) = [] then compteur := !compteur + 1;
  done;
  let res = Array.make (n - !compteur) array.(0) in
  let old_i = ref 0 in
  for i = 0 to (n - 1) do
    if array.(i) != [] then
      begin
        res.(!old_i) <- array.(i);
        old_i := !old_i + 1;
      end
  done;
  res
;;



let slice prog = (* Découpe le programme en une liste de lignes *)
  let res = ref [] in (* Liste des lignes *)
  let program = remove_comments prog in
  let n = String.length program in
  let old_i = ref 0 in
  for i = 0 to (n - 1) do
    if program.[i] = '\n' then begin
        res := (strip (String.sub program !old_i (i - !old_i))) :: !res;
        old_i := i;
      end
  done;
  if !old_i < n then
    res := (strip (String.sub program !old_i (n - !old_i))) :: !res;
  List.rev !res
;;

let slice_line line =
  let res = ref [] in
  let n = String.length line in
  let old_i = ref 0 in
  for i = 0 to (n - 1) do
    if line.[i] = ' ' then begin
        res := (strip (String.sub line !old_i (i - !old_i))) :: !res;
        old_i := i;
      end
  done;
  if !old_i < n then res := (strip (String.sub line !old_i (n  - !old_i))) :: !res;
  List.rev !res
;;

slice_line "int a = 0";;

let array_of_list list =
  let n = length list in
  let liste = ref list in
  let premier =
    match list with
    | [] -> failwith "Liste vide"
    | t :: _ -> t
  in
  let res = Array.make n premier in
  for i = 0 to (n - 1) do
    res.(i) <- match !liste with
               | [] -> failwith "Isse"
               | t :: q -> liste := q; t
  done;
  res
;;

              

let slice_double program = (* Découpe le programme en arrray de listes de mots *)
  let sliced = array_of_list (slice program) in
  let n = Array.length sliced in
  let res = Array.make n [] in
  for i = 0 to (n - 1) do
    res.(i) <- slice_line sliced.(i);
  done;
  remove_empty res
;;

              

slice_double "int isihdqi 0 
       float a 0.342
       while i < 0 do
       isse dsiqo kl";;




type dict =
  | Empty
  | Node of ((string * adress) * dict * dict)
;;

let empty_dict () = Empty;;

let rec add_to_dict dict value = (* val = (str * int) *)
  let (s, v) = value in
  match dict with
  | Empty -> Node (value, Empty, Empty)
  | Node ((s1, v1), fg, fd) when s1 = s -> failwith "variable already in dict"
  | Node ((s1, v1), fg, fd) when s < s1 -> Node ((s1, v1), add_to_dict fg value, fd)
  | Node ((s1, v1), fg, fd) -> Node ((s1, v1), fg, add_to_dict fd value)
;;

let rec is_in_dict dict str =
  match dict with
  | Empty -> false
  | Node ((s1, _), _, _) when s1 = str -> true
  | Node ((s1, _), fg, _) when str < s1 -> is_in_dict fg str
  | Node ((_, _), _, fd) -> is_in_dict fd str
;;

let rec get_adress dict str =
  match dict with
  | Empty -> failwith "variable name not in dict"
  | Node ((s1, v1), _, _) when s1 = str -> v1
  | Node ((s1, _), fg, _) when str < s1 -> get_adress fg str
  | Node ((_, _), _, fd) -> get_adress fd str
;;

let iso = empty_dict ();;

let iso = add_to_dict iso ("abc", Reg 0);;

is_in_dict iso "abc";;

type token =
  | AFFECT (* = *)
  | EQUAL (* == *)
  | DIFFERENT (* != *)
  | LEQ  (* <= *)
  | LQ (* < *)
  | GEQ (* >= *)
  | GQ (* > *)
  | PLUS (* + *)
  | MINUS (* - *)
  | MULT (* * *)
  | DIV (* / *)
  | AND (* && *)
  | OR (* || *)
  | NOT (* ! *)
  | EXP (* ** *)
  | PARO (* ( *)
  | PARC (* ) *)
  | WHILE (* while *)
  | ENDWHILE (* endwhile *)
  | FOR (* for *)
  | TO (* to *)
  | ENDFOR (* endfor *)
  | IF (* if *)
  | ENDIF (* endif *)
  | VARIABLE of string (* token correspondant à une variable dénotée par un string *)
  | INT
  | STRING
  | FLOAT
  | BOOL
  | CONSTTRUE
  | CONSTFALSE
  | CONSTINT of int
  | CONSTFLOAT of float
  | CONSTSTRING of string
  | FUNCTION of string
;;

let variable_name variable =
  match variable with
  | VARIABLE (stri) -> stri
  | _ -> failwith "Error, not a variable"
;;

let rec appartient liste elt =
  match liste with
  | [] -> false
  | t :: q -> (t = elt) || (appartient q elt)
;;


let check_number char =
  appartient ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] char
;;

let check_variable_char char =
  match char with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let checkVariable str = (* retourne si str represente correctement une variable *)
  let n = String.length str in
  let i = ref 0 in
  while !i < n && (check_variable_char str.[!i]) do
    i := !i + 1;
  done;
  !i = n
;;

       
let checkInt str = (* retourne si str représente un entier *)
  let n = String.length str in
  let i = ref 0 in
  while (!i < n) && (check_number str.[!i]) do
    i := !i + 1;
  done;
  !i = n
;;

let checkString str = (* retourne si str représente un string *)
  let n = String.length str in
  if n < 2 then false
  else (str.[0] = '\'') && (str.[n - 1] = '\'')
;;

let checkFloat str = (* retourne si str représente un float *)
  let n = String.length str in
  let trouvePoint = ref false in
  let resultat = ref true in
  for i = 0 to (n - 1) do
    if not (check_number str.[i]) then
      begin
        if (not !trouvePoint) && (str.[i] = '.') then trouvePoint := true
        else
          resultat := false
      end
  done;
  !resultat && !trouvePoint
;;    

let functionsList = [("print", PRINT E); ("inc", INC E); ("dec", DEC E);];; (* liste des noms de fonctions, avec ref vers l'instruction associée *)

let checkFunction str =
  let rec parcours liste str =
    match liste with
    | [] -> false
    | (a, b) :: q -> (a = str) || (parcours q str)
  in
  parcours functionsList str
;;


let string_to_token str =
  match str with
  | "=" -> AFFECT
  | "==" -> EQUAL
  | "!=" -> DIFFERENT
  | "<=" -> LEQ
  | "<" -> LQ
  | ">=" -> GEQ
  | ">" -> GQ
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> MULT
  | "/" -> DIV
  | "**" -> EXP
  | "&&" -> AND
  | "||" -> OR
  | "!" -> NOT
  | "(" -> PARO
  | ")" -> PARC
  | "while" -> WHILE
  | "endwhile" -> ENDWHILE
  | "for" -> FOR
  | "to" -> TO
  | "endfor" -> ENDFOR
  | "if" -> IF
  | "endif" -> ENDIF
  | "bool" -> BOOL
  | "int" -> INT
  | "float" -> FLOAT
  | "string" -> STRING
  | "true" -> CONSTTRUE
  | "false" -> CONSTFALSE
  | _ when checkInt str -> CONSTINT (int_of_string str)
  | _ when checkFloat str -> CONSTFLOAT (float_of_string str)
  | _ when checkString str -> CONSTSTRING (String.sub str 1 (String.length str - 2))
  | _ when checkFunction str -> FUNCTION str
  | _ when checkVariable str -> VARIABLE str
  | _ -> failwith ("Unrecognized token " ^ str)
;;

let rec tokenize_line line =
  match line with
  | [] -> []
  | str :: queue -> (string_to_token str) :: (tokenize_line queue)
;;


let tokenize_program prog =
  let sliced = slice_double prog in
  let n = Array.length sliced in
  let res = Array.make n [] in
  for i = 0 to (n - 1) do
    res.(i) <- tokenize_line sliced.(i);
  done;
  let res2 = Array.make n [| |] in
  for i = 0 to (n - 1) do
    res2.(i) <- Array.of_list res.(i);
  done;
  res2
;;

let opArray =
  [|AND; OR; LEQ; GEQ; LQ; GQ; PLUS; MINUS; MULT; DIV; EXP; EQUAL; NOT; PARO|];;


type variable = string;;
type constant = data;;

type syntaxTree =
  | TreeVariable of variable
  | TreeConstant of constant
  | TreeDUAL of token * syntaxTree * syntaxTree
  | TreeMONO of token * syntaxTree
  | Leaf
;;


let rec expToTree exp = (* Prend une liste de tokens symbolisant une expression et la transforme en l'arbre syntaxique associé *)
  let nExp = Array.length exp in
  let trouve = ref false in
  let parNb = ref 0 in (* stack de parentheses *)
  let nOP = Array.length opArray in
  let iOP = ref 0 in
  let iExp = ref 0 in
  let indiceTrouve = ref (-1) in
  if nExp = 0 then
    Leaf
  else
    begin
      if nExp = 1 then
        begin
          match exp.(0) with
          | VARIABLE str -> TreeVariable str
          | CONSTTRUE -> TreeConstant (B true)
          | CONSTFALSE -> TreeConstant (B false)
          | CONSTFLOAT fl -> TreeConstant (F fl)
          | CONSTSTRING str -> TreeConstant (C str)
          | CONSTINT cint -> TreeConstant (I cint)
          | _ -> failwith "Unrekognized token while parsing."
        end
      else
        begin
          while (!iOP < nOP) && (not !trouve) do (* teste l'existence de chaque token d'operatio, un par un, par ordre de priorite croissante *)
            iExp := 0;
            parNb := 0;
            while (!iExp < nExp) && (not !trouve) do
              match exp.(!iExp) with
              | PARO when opArray.(!iOP) = PARO -> begin
                  trouve := true;
                  indiceTrouve := !iExp;
                  iExp := !iExp + 1;
                end
              | PARO -> parNb := !parNb + 1; iExp := !iExp + 1;
              | PARC -> parNb := !parNb - 1; iExp := !iExp + 1;
              | _ when (!parNb = 0) && (not !trouve) && (exp.(!iExp) = opArray.(!iOP)) -> begin
                  trouve := true;
                  indiceTrouve := !iExp;
                  iExp := !iExp + 1;
                end
              | _ -> iExp := !iExp + 1; 
            done;
            iOP := !iOP + 1;
          done;
          if exp.(!indiceTrouve) = PARO then begin
              if !indiceTrouve > 0 then failwith "Error encountered while parsing.";
              expToTree (Array.sub exp 1 (nExp - 2));
            end
          else begin   
              if !indiceTrouve = (-1) then failwith "No rekognized token while parsing.";
              let l1 = expToTree (Array.sub exp 0 (!indiceTrouve)) in
              let l2 = expToTree (Array.sub exp (!indiceTrouve + 1) (nExp - !indiceTrouve - 1)) in
              match exp.(!indiceTrouve) with
              | PLUS | MINUS | DIV | MULT | EXP -> TreeDUAL (exp.(!indiceTrouve), l1, l2)
              | AND | OR -> TreeDUAL (exp.(!indiceTrouve), l1, l2)
              | LEQ | GEQ | LQ | GQ | EQUAL -> TreeDUAL (exp.(!indiceTrouve), l1, l2)
              | NOT -> TreeMONO (exp.(!indiceTrouve), l2)
              | _ -> failwith "Error encountered while parsing"
            end;
        end;
    end
;;



let machT = "int absql = 0
             float b = 1.2
             string abc = 'isse'
             print a /* issou
             inc a */
             print a
             dec a /* lel */
             print a
             print abc
             int c = ( a + a ) * a

             for int i = 0 to n do
             n = i
             endfor"
;;

let test = "a && ! ( c < b )";;


expToTree (tokenize_program test).(0);;

[| [|1; 2|]; [|1|] |];;

type naturalLangage = (* Temporaire *)
  | NaruralVARIABLE of variable
  | NaturalCONSTANT of data
                     
  | NatWHILE of naturalLangage * naturalLangage
  | NatFOR of naturalLangage * naturalLangage
  | NatIF of naturalLangage * naturalLangage
;;

type valType =
  | ValFloat
  | ValInt
  | ValString
  | ValBool
;;


type processedTree =
  | ProcessedAFFECT of valType * string * syntaxTree (* e.g. int a = ( 3 * b ) + 2 *)
  | ProcessedVALUE of string * syntaxTree (* e.g. a = ( 3 * b ) + 2 *)
  | ProcessedIF of syntaxTree * (processedTree array) (* le premier est la condition, le deuxième le then *)
  | ProcessedWHILE of syntaxTree * (processedTree array)
  | ProcessedFOR of processedTree * syntaxTree * (processedTree array) (* le premeir est l'affectation de la variable d'itération, le deuxième la limite et le troisième le bloc du for *)
;;


let rec process_program tokenized = (* prend en entree un programme tokenise *)
  let res = ref [] in (* liste des processed trees *)
  let n = Array.length tokenized in
  let i = ref 0 in
  while !i < n do
    let phraseCourante = tokenized.(!i) in
    if Array.length phraseCourante > 0 then
      begin
        match phraseCourante.(0) with
        | INT -> let name = match phraseCourante.(1) with
                   | VARIABLE str -> str
                   | _ -> failwith ("Error while processign line" ^ string_of_int !i)
                 in
                 res := ProcessedAFFECT (ValInt, name, expToTree (Array.sub phraseCourante 3 (Array.length phraseCourante - 3))) :: !res;
                 i := !i + 1;
        | FLOAT -> let name = match phraseCourante.(1) with
                   | VARIABLE str -> str
                   | _ -> failwith ("Error while processign line" ^ string_of_int !i)
                 in
                 res := ProcessedAFFECT (ValFloat, name, expToTree (Array.sub phraseCourante 3 (Array.length phraseCourante - 3))) :: !res;
                 i := !i + 1;
        | STRING -> let name = match phraseCourante.(1) with
                   | VARIABLE str -> str
                   | _ -> failwith ("Error while processign line" ^ string_of_int !i)
                 in
                 res := ProcessedAFFECT (ValString, name, expToTree (Array.sub phraseCourante 3 (Array.length phraseCourante - 3))) :: !res;
                 i := !i + 1;
        | BOOL -> let name = match phraseCourante.(1) with
                   | VARIABLE str -> str
                   | _ -> failwith ("Error while processign line" ^ string_of_int !i)
                 in
                 res := ProcessedAFFECT (ValInt, name, expToTree (Array.sub phraseCourante 3 (Array.length phraseCourante - 3))) :: !res;
                 i := !i + 1;
        | VARIABLE str -> res := ProcessedVALUE (str, expToTree (Array.sub phraseCourante 2 (Array.length phraseCourante - 2))) :: !res;
                          i := !i + 1;
        | IF ->
           let temp = Array.sub phraseCourante 1 (Array.length phraseCourante - 1) in
           let condition = expToTree(temp) in
           let compteur = ref 1 in
           let j = ref !i in
           let indiceTrouve = ref (-1) in
           while (!j < n) && (!compteur > 0) do
             if tokenized.(!j).(0) = IF then compteur := 1 + !compteur;
             if tokenized.(!j).(0) = ENDIF then
               begin
                 compteur := !compteur - 1;
                 if !compteur = 0 then indiceTrouve := !j;
               end;
             j := !j + 1;
           done;
           if !indiceTrouve = -1 then failwith ("Error : unmatched IF line " ^ string_of_int !i);
           let programTemp = Array.make (!indiceTrouve - !i - 1) tokenized.(0) in
           for k = 0 to (!indiceTrouve - !i - 2) do
             programTemp.(k) <- tokenized.(!i + k);
           done;
           res := ProcessedIF (condition, process_program programTemp) :: !res;
           i := !indiceTrouve + 1;
        | FOR ->
           let indiceTO = ref (-1) in
           let len = Array.length phraseCourante in
           for k = 0 to (len - 1) do
             if phraseCourante.(k) = TO then indiceTO := k;
           done;
           if !indiceTO = -1 then failwith ("Error : did not find matching DO line " ^ string_of_int !i);
           let initialisation = Array.sub phraseCourante 1 (!indiceTO - 1) in
           let limite = expToTree (Array.sub phraseCourante (!indiceTO + 1) (len - !indiceTO - 1)) in
           let compteur = ref 1 in
           let j = ref !i in
           let indiceTrouve = ref (-1) in
           while (!j < n) && (!compteur > 0) do
             if tokenized.(!j).(0) = FOR then compteur := 1 + !compteur;
             if tokenized.(!j).(0) = ENDFOR then
               begin
                 compteur := !compteur - 1;
                 if !compteur = 0 then indiceTrouve := !j;
               end;
             j := !j + 1;
           done;
           if !indiceTrouve = -1 then failwith ("Error : unmatched FOR line " ^ string_of_int !i);
           let programTemp = Array.make (!indiceTrouve - !i - 1) tokenized.(0) in
           for k = 0 to (!indiceTrouve - !i - 2) do
             programTemp.(k) <- tokenized.(!i + k);
           done;
           res := ProcessedFOR ((process_program [|initialisation;|]).(0), limite, process_program programTemp) :: !res;
           i := !indiceTrouve + 1;
        | WHILE ->
           let condition = expToTree (Array.sub phraseCourante 1 (Array.length phraseCourante - 1)) in
           let compteur = ref 1 in
           let j = ref !i in
           let indiceTrouve = ref (-1) in
           while (!j < n) && (!compteur > 0) do
             if tokenized.(!j).(0) = WHILE then compteur := 1 + !compteur;
             if tokenized.(!j).(0) = ENDWHILE then
               begin
                 compteur := !compteur - 1;
                 if !compteur = 0 then indiceTrouve := !j;
               end;
             j := !j + 1;
           done;
           if !indiceTrouve = -1 then failwith ("Error : unmatched WHILE line " ^ string_of_int !i);
           let programTemp = Array.make (!indiceTrouve - !i - 1) tokenized.(0) in
           for k = 0 to (!indiceTrouve - !i - 2) do
             programTemp.(k) <- tokenized.(!i + k);
           done;
           res := ProcessedWHILE (condition, process_program programTemp) :: !res;
           i := !indiceTrouve + 1;
        | _ -> failwith ("Unrecognized token, line " ^ string_of_int !i)
      end
    else
      i := !i + 1;
  done;
  Array.of_list (List.rev !res)
;;
        
                 
let test2 =
  "
int a = 0
if a == 0
endif
int c = 3
string d = 4
";;
 
let tokenized_test2 = tokenize_program test2;;

process_program tokenized_test2;;

tokenize_program machT;;
slice test;;
