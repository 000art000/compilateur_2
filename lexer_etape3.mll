{
open Parser_etape3
let keyword_table=Hashtbl.create 18;;
List.iter (fun (kwd ,tok) -> Hashtbl.add keyword_table kwd tok)
[
  ("input",INPUT);
  ("symbols",SYMBOLS);
  ("stack",STACK);
  ("state",STATE);
  ("states",STATES);
  ("initial",INITIAL);
  ("program",PROGRAM);
  ("of",OF);
  ("top",TOP);
  ("begin",BEGIN);
  ("end",END);
  ("change",CHANGE);
  ("pop",POP);
  ("push",PUSH);
  ("next",NEXT);
  ("reject",REJECT);
  ("case",CASE);
  ("symbol",SYMBOL)
]
}

let layout = [ ' ' '\t' '\n' ]
let digit = ['0'-'9']
let lettre = ['a'-'z''A'-'Z']
let lettre_ou_digit = lettre|digit


rule main = parse
  | layout* {main lexbuf} 
  | (lettre lettre+) as s{
     try Hashtbl.find keyword_table s
     with Not_found -> failwith "unexpected word" 
  }
  | lettre_ou_digit as c {LETTRE (String.make 1 c) }
  | ','  {VERGULE}
  | ':'  {DEUX_POINT}
  | eof			{ EOF }
  | _ 		{ failwith "unexpected character" }
