(* Interpréteur Brainfuck *)
(* Il interprète une chaîne de caractères, 'commande' *)

let interpreter commande ascii=
  let maxDepth = 1000000000 in
  let depth = ref 0 in
  let longueur = String.length commande in
  let data = Array.make 30000 0 in
  let pointeur = ref 0 in
  let char = ref ' ' in
  let lecteur = ref 0 in
  while !lecteur < longueur && !depth < maxDepth do
    depth := !depth + 1;
    char := commande.[!lecteur];
    if !char = ',' then begin
      if ascii then
	data.(!pointeur) <- Char.code ((read_line()).[0])
      else
	data.(!pointeur) <- int_of_string (read_line());
    end;
    if !char = '+' then
      data.(!pointeur) <- data.(!pointeur) + 1;
    if !char = '-' then
      data.(!pointeur) <- data.(!pointeur) - 1;
    if !char = '<' then
      pointeur := !pointeur - 1;
    if !char = '>' then
      pointeur := !pointeur + 1;
    if !char = '.' then begin
      if ascii then
	print_char (Char.chr (data.(!pointeur)))
      else
	print_int data.(!pointeur);
    end;
    if !char = ']' && data.(!pointeur) != 0 then begin
      let compteur = ref 1 in
      lecteur := !lecteur - 1;
      while !lecteur > 0 && !compteur > 0 do
	if commande.[!lecteur] = '[' then
	  compteur := !compteur - 1;
	if commande.[!lecteur] = ']' then
	  compteur := !compteur + 1;
	lecteur := !lecteur - 1;
    done;
    if commande.[!lecteur + 1] != '[' then
      failwith "Erreur, pas de [ correspondant";
end;
    lecteur := !lecteur + 1;
done;
if !depth = maxDepth then failwith "Profondeur max atteinte.";
;;

interpreter ", > ,< [>[>+>+ << -] > [- < + >] << -] >>> ." false;;

interpreter ",.[-.]" false;;
