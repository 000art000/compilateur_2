type transition = 
{
     current_state: string;
     input_symbole: string;
     stack_head: string;
     next_state: string;
     new_stack_heads: string list

}

type automate =
{

  input_symbols: string list;
  states : string list;
  init_state : string;
  stack_symbols : string list;
  init_stack : string;
  transitions : (string,(string, transition list) Hashtbl.t) Hashtbl.t

}

let as_string3 a=
    let rec string_of_list =function
    |[]->""
    |[x]->x
    |x::l->x^", "^(string_of_list l)
    in
    let rec aux =function
    |[]->""
    |x::l->"push "^x^" "
    in
    let string_of_transition t=
      if t.new_stack_heads<>[] && (List.hd  t.new_stack_heads) =t.stack_head then
          "\t\t\t"^t.input_symbole^": "^(aux (List.tl  t.new_stack_heads))^
          (if t.current_state =t.next_state then
          "\n"
          else "change "^t.next_state^"\n" 
          )
      else
          "\t\t\t"^t.input_symbole^": pop "^(aux t.new_stack_heads)^
          (if t.current_state =t.next_state then
          "\n"
          else "change "^t.next_state^"\n" 
          )
    in
    let rec string_of_transitions tr=
      Hashtbl.fold (
        fun k x acc->"\t"^k^": begin\n\t   case top of\n"^( 
            Hashtbl.fold( 
                fun k2 x2 acc2 ->if x2<>[] then 
                    (match x2 with
                    |[t] when t.input_symbole=""->"\t\t"^k2^": pop\n"^acc2
                    |_->(
                "\t\t"^k2^": begin\n\t\t   case next of\n"^
                    (List.fold_left (fun acc3 e->
                      acc3^(string_of_transition e)  
                    ) "" x2)^"\t\t   end\n"^acc2)) else acc2
            ) x ""
        )^"\t   end\n" ^acc
      ) tr "" 
    in
    "input symbols: " ^ (string_of_list a.input_symbols) ^ 
    "\nstack symbols: " ^ (string_of_list a.stack_symbols) ^
    "\nstates: " ^ (string_of_list a.states) ^ 
    "\ninitial state: " ^(a.init_state) ^  
    "\ninitial stack:" ^ (a.init_stack) ^ 
    "\n\nprogram:\n   case state of\n" ^ 
    (string_of_transitions a.transitions)



let rec interprete mot transitions stack state=
  let rec string_of_list =function
    |[]->""
    |[x]->x
    |x::l->(string_of_list l)^x
  in
  let rec search_transition input_symbole=function
  |[]->None
  |trans::l->if (trans.input_symbole=input_symbole || trans.input_symbole="") then
            Some (trans.input_symbole,trans.next_state,trans.new_stack_heads)
            else search_transition input_symbole l

  in
  if stack=[] then
    if mot="" then "("^state^","^(string_of_list stack)^","^mot^")"
    else failwith "la pile est vide sans que l’entrée soit épuisée"
  else 
    if mot="" then 
      match search_transition "" (Hashtbl.find (Hashtbl.find transitions state) (List.hd stack)) with
      |Some (input_symbole,next_state,new_stack_heads)->
      "("^state^","^(string_of_list stack)^","^mot^")\n|-"^
      (interprete "" transitions ((List.rev new_stack_heads)@(List.tl stack)) next_state)
      |None->failwith "entrée est épuisée sans que la pile soit vide"
    else 
      match search_transition (String.sub mot 0 1) (Hashtbl.find (Hashtbl.find transitions state) (List.hd stack)) with
      |Some (input_symbole,next_state,new_stack_heads)->
        if input_symbole="" then 
        "("^state^","^(string_of_list stack)^","^mot^")\n|-"^
        (interprete mot transitions ((List.rev new_stack_heads)@(List.tl stack)) next_state)
        else  
        "("^state^","^(string_of_list stack)^","^mot^")\n|-"^
        (interprete (String.sub mot 1 ((String.length mot)-1)) transitions ((List.rev new_stack_heads)@(List.tl stack)) next_state)
      |None->failwith "il n’y a aucune transition qui s’applique"


let verifie_automate autom =
    let rec containt ele =function
    |[]->false
    |x::l -> if x=ele then true else containt ele l
  in
  let ver_deterministe transitions=
  (
    let rec remove_ele e=function
    |[]->[]
    |x::l when x=e->l
    |x::l-> x::(remove_ele e l)
    in
    let rec aux_ele e=function
    |[]->true
    |x::l-> if  (x.input_symbole <> e.input_symbole && not(x.input_symbole="" || e.input_symbole="")) then aux_ele e l else false
    in
    let rec aux_list l0=function 
    |[]->true
    |x::l-> if aux_ele x (remove_ele x l0) then aux_list l0 l else false
    in
    Hashtbl.fold (
      fun k x acc-> (Hashtbl.fold ( 
                        fun k2 x2 acc2 -> (aux_list x2 x2)  && acc2
                        ) x true) && acc 
      ) transitions true
  )
  in
  if not (containt autom.init_state autom.states ) || not (containt autom.init_stack autom.stack_symbols )  then
    false
  else 
  ver_deterministe autom.transitions

let interp_deter autom mot =
  if verifie_automate autom then
      interprete mot autom.transitions [autom.init_stack] autom.init_state
  else "l'automate ne satisfait pas les conditions"

let rec as_string a=
    let rec string_of_list =function
    |[]->""
    |x::l->x^" "^(string_of_list l)
    in
    let rec string_of_transitions tr=
      Hashtbl.fold (
        fun k x acc->
            Hashtbl.fold (
                fun k2 x2 acc2->
                  List.fold_left (
                     fun a t-> a^"( " ^ t.current_state ^"," ^t.input_symbole ^ "," ^ t.stack_head ^ "," ^ t.next_state ^ "," ^ (string_of_list t.new_stack_heads) ^ " )\n" 
                  ) "" x2  ^acc2
            ) x ""  ^acc
      ) tr "" 
    in
    "input_symbols:\n" ^ (string_of_list a.input_symbols) ^ "\nstates:\n" ^ (string_of_list a.states) ^ "\ninit_state:\n" ^
    (a.init_state) ^ "\nstack_symbols:\n" ^ (string_of_list a.stack_symbols) ^ "\ninit_stack:\n" ^ 
    (a.init_stack) ^ "\ntransitions:\n" ^ (string_of_transitions a.transitions) ^"\n"