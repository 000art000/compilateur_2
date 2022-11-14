%{
open Ast
%}

%token INPUT SYMBOLS STACK STATE STATES INITIAL PROGRAM OF TOP BEGIN END CHANGE POP NEXT REJECT CASE
  VERGULE DEUX_POINT EOF PUSH SYMBOL
%token<string> LETTRE 

%start<Ast.automate> axiome

%%

  
axiome : a=automate EOF{a} 

automate : d=declarations p=program{
                                    let rec state_fun t =function
                                    |[]->[]
                                    |state::l->
                                    if t.next_state="" then
                                    {
                                      current_state=state;
                                      input_symbole=t.input_symbole;
                                      stack_head=t.stack_head;
                                      next_state=state;
                                      new_stack_heads=t.new_stack_heads
                                    }::(state_fun t l)
                                    else 
                                    {
                                      current_state=state;
                                      input_symbole=t.input_symbole;
                                      stack_head=t.stack_head;
                                      next_state=t.next_state;
                                      new_stack_heads=t.new_stack_heads
                                    }::(state_fun t l)
                                    in

                                    let rec stack_fun t =function
                                    |[]->[]
                                    |stack::l->
                                    if t.new_stack_heads=[] && t.current_state=t.next_state then
                                    {
                                      current_state=t.current_state;
                                      input_symbole=t.input_symbole;
                                      stack_head=stack;
                                      next_state=t.next_state;
                                      new_stack_heads=t.new_stack_heads
                                    }::(stack_fun t l)
                                    else 
                                    {
                                      current_state=t.current_state;
                                      input_symbole=t.input_symbole;
                                      stack_head=stack;
                                      next_state=t.next_state;
                                      new_stack_heads=stack::t.new_stack_heads
                                    }::(stack_fun t l)
                                    in
                                    (*
                                    let rec next_fun t =function
                                    |[]->[{
                                      current_state=t.current_state;
                                      input_symbole="";
                                      stack_head=t.stack_head;
                                      next_state=t.next_state;
                                      new_stack_heads=t.new_stack_heads
                                    }]
                                    |next::l->
                                    {
                                      current_state=t.current_state;
                                      input_symbole=next;
                                      stack_head=t.stack_head;
                                      next_state=t.next_state;
                                      new_stack_heads=t.new_stack_heads
                                    }::(next_fun t l)
                                    in
                                    *)
                                    let rec stack_list st stacks=match st with
                                    |[]->[]
                                    |t::l->if t.stack_head="" then
                                    (stack_fun t stacks)::(stack_list l stacks)
                                    else
                                      if t.new_stack_heads=[] && t.current_state=t.next_state then
                                    [t]::(stack_list l stacks)
                                    else 
                                    [{
                                      current_state=t.current_state;
                                      input_symbole=t.input_symbole;
                                      stack_head=t.stack_head;
                                      next_state=t.next_state;
                                      new_stack_heads=t.stack_head::t.new_stack_heads
                                    }]::(stack_list l stacks)
                                    in(*
                                    let rec next_list st inputs=match st with
                                    |[]->[]
                                    |t::l->if t.input_symbole="" then (next_fun t inputs)::(next_list l inputs)
                                          else [t]::(next_list l inputs)
                                    in*)
                                    let rec transitions d =function
                                    |[]->[]
                                    |(lt,cr,is,sh,ns,nhs)::l->let t={
                                    current_state=cr;
                                    input_symbole=is;
                                    stack_head=sh;
                                    next_state=ns;
                                    new_stack_heads=nhs
                                    }in
                                    let t=
                                    (if t.current_state="" then
                                    state_fun t d.states 
                                    else if t.next_state="" then
                                            [{
                                    current_state=t.current_state;
                                    input_symbole=t.input_symbole;
                                    stack_head=t.stack_head;
                                    next_state=t.current_state;
                                    new_stack_heads=t.new_stack_heads
                                    }]
                                          else [t])
                                    in let t=stack_list t d.stack_symbols
                                    in (*let t=next_list (List.flatten t) d.input_symbols
                                    in*)
                                    (List.flatten t)@(transitions d l)
                                    in 
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
  transitions = hashm (transitions d p)
}
                                    }

declarations : is=inputsymbols ss=stacksymbols s=states i=initialstate ist=initialstack
{
  {
  input_symbols= is;
  states = s;
  init_state = i;
  stack_symbols = ss;
  init_stack = ist;
  transitions = Hashtbl.create 1
  }
}
inputsymbols : INPUT SYMBOLS DEUX_POINT l=separated_list(VERGULE,LETTRE){l}
stacksymbols : STACK SYMBOLS DEUX_POINT l=separated_nonempty_list(VERGULE,LETTRE){l}
states : STATES DEUX_POINT l=separated_nonempty_list(VERGULE,LETTRE){l}
initialstate :  INITIAL STATE DEUX_POINT ltr=LETTRE{ltr}
initialstack : INITIAL STACK SYMBOL DEUX_POINT ltr=LETTRE{ltr}

program : PROGRAM DEUX_POINT c=cases{c}

cases : CASE t=typee OF l=list(bloc){
                                  let rec aux typee=function
                                  |[]->[]
                                  |(lt,current_state,input_symbole,stack_head,next_state,new_stack_heads)::l->if typee="n" then
                                                                          (lt,current_state,lt,stack_head,next_state,new_stack_heads)::(aux typee l)
                                                                          else if typee="t" then
                                                                          (lt,current_state,input_symbole,lt,next_state,new_stack_heads)::(aux typee l)
                                                                          else 
                                                                          (lt,lt,input_symbole,stack_head,next_state,new_stack_heads)::(aux typee l)
                                  in
                                   aux t (List.flatten l)
                                       }

typee : NEXT{"n"} | TOP{"t"} | STATE{"s"}

bloc : l=LETTRE DEUX_POINT s=suit_bloc{let rec aux lettre =function 
                                      |[]->[]
                                      |(lt,current_state,input_symbole,stack_head,next_state,new_stack_heads)::ll->(lettre,current_state,input_symbole,stack_head,next_state,new_stack_heads)::(aux lettre ll)
                                      in aux l s
                                      }

suit_bloc : BEGIN c=cases END{c} | l=list(instr){l}

instr : POP{("","","","","",[])} | PUSH l=LETTRE{("","","","","",[l])} | CHANGE l=LETTRE{("","","","",l,[])} | REJECT{("","","","","",[])}
