open Sys

let lexbuf = Lexing.from_channel stdin 

let ast1 () = Parser.axiome Lexer.main lexbuf 

let ast2 () = Parser_etape3.axiome Lexer_etape3.main lexbuf 

let usage () =
  print_string "-r: afficher l'automate\n";
  print_string "-i: interpreter un mot\n";
  print_string "-p: afficher l'automate dans la syntaxe de l'etape 3\n";
  print_string "-d: verifie si l'automate est deterministe\n";
  print_string "-id: interpreter un mot apres verification de l'automate\n";
  print_string "-R: afficher l'automate a partir d'un fichier de syntaxe d'etape 3\n";
  print_string "-I: interpreter un mot a partir d'un fichier de syntaxe d'etape 3\n";
  print_string "-P: afficher l'automate dans la syntaxe de l'etape 3 a partir d'un fichier de syntaxe d'etape 3\n";
  print_string "-D: verifie si l'automate est deterministe a partir d'un fichier de syntaxe d'etape 3\n";
  print_string "-ID: interpreter un mot apres verification de l'automate a partir d'un fichier de syntaxe d'etape 3\n";;

let main () =
  match Sys.argv with
  | [|_;"-r"|] ->let ast=ast1() in Printf.printf "%s" (Ast.as_string ast)
  | [|_;"-i"; mot|] ->let ast=ast1() in Printf.printf "%s" (Ast.interprete mot ast.transitions [ast.init_stack] ast.init_state )
  | [|_;"-p"|] ->let ast=ast1() in Printf.printf "%s" (Ast.as_string3 ast)
  | [|_;"-d"|] ->let ast=ast1() in if Ast.verifie_automate ast then Printf.printf "%s\n" "true" else Printf.printf "%s\n" "false"
  | [|_;"-id";mot|] ->let ast=ast1() in Printf.printf "%s" (Ast.interp_deter ast mot)
  | [|_;"-R"|] ->let ast=ast2() in Printf.printf "%s" (Ast.as_string ast)
  | [|_;"-I"; mot |] ->let ast=ast2() in Printf.printf "%s" (Ast.interprete mot ast.transitions [ast.init_stack] ast.init_state )
  | [|_;"-P"|] ->let ast=ast2() in Printf.printf "%s" (Ast.as_string3 ast)
  | [|_;"-D"|] ->let ast=ast2() in if Ast.verifie_automate ast then Printf.printf "%s\n" "true" else Printf.printf "%s\n" "false"
  | [|_;"-ID";mot|] ->let ast=ast2() in Printf.printf "%s" (Ast.interp_deter ast mot)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()