%{
open Ast
%}

%token INPUT SYMBOLS STACK INITIAL TRANSITIONS EOF 
  POINT_VERGULE PARG_OUV PARG_FERM VERGULE DEUX_POINT STATE STATES SYMBOL
%token<string> LETTRE 

%start<Ast.automate> axiome

%%

  
axiome : a=automate EOF{a} 
automate : d=declarations t=transitions{
  let hashm tr=
    let rec transition_of_state_stack e s =function
    |[]->[]
    |x::l -> if x.current_state=e && x.stack_head=s then 
                x::(transition_of_state_stack e s l ) 
             else transition_of_state_stack e s l
    in 
    let h=Hashtbl.create (List.length d.states) in 
    List.iter (fun e -> 
      let h2=Hashtbl.create (List.length d.stack_symbols) in 
          List.iter (fun s -> 
              Hashtbl.add h2 s (transition_of_state_stack e s tr) ) d.stack_symbols ;
      Hashtbl.add h e h2 ) d.states ;
    h
    in
  {
  input_symbols= d.input_symbols;
  states = d.states;
  init_state = d.init_state;
  stack_symbols = d.stack_symbols;
  init_stack = d.init_stack;
  transitions = hashm t
}
}

declarations : is=inputsymbols ss=stacksymbols s=states i=initialstate ist=initialstack{
  {
  input_symbols= is;
  states = s;
  init_state = i;
  stack_symbols = ss;
  init_stack = ist;
  transitions = Hashtbl.create 1
}
}
inputsymbols : INPUT SYMBOLS DEUX_POINT l=separated_nonempty_list(VERGULE,LETTRE){l}
stacksymbols : STACK SYMBOLS DEUX_POINT l=separated_nonempty_list(VERGULE,LETTRE){l}
states : STATES DEUX_POINT l=separated_nonempty_list(VERGULE,LETTRE){l}
initialstate :  INITIAL STATE DEUX_POINT ltr=LETTRE{ltr}
initialstack : INITIAL STACK SYMBOL DEUX_POINT ltr=LETTRE{ltr}

transitions : TRANSITIONS DEUX_POINT l=list(transition){l}
transition : PARG_OUV e0=LETTRE VERGULE c=option(LETTRE) VERGULE sp0=LETTRE VERGULE e1=LETTRE VERGULE sp1=stack PARG_FERM
{
  {current_state=e0;
     input_symbole=(match c with Some x->x |None->"");
     stack_head=sp0;
     next_state=e1;
     new_stack_heads=sp1}
}
stack : l=separated_list(POINT_VERGULE,LETTRE) {l}

