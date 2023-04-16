exception BadFileType

open Ast
  
let () = 
  let _ = 
    if Array.length Sys.argv <> 2 then
      (Format.printf "Usage: cps <file>\n";
       exit 0) in 
  let name = Sys.argv.(1) in
  let ind = String.rindex name '.' in
  let ext = String.sub name ind ((String.length name) - ind) in
  
  let file = open_in name in 
  let lexbuf = Lexing.from_channel file in 

  let tcode =
    if ext = ".ch" then 
      (* it's a target language file *)
      try Parser.t_exp Lexer.token lexbuf
      with Parsing.Parse_error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
        exit 1
	  
    else if ext = ".nc" then 
      (* it's a source language file. Parse it then translate it *)
      let scode = try Parser.s_exp Lexer.token lexbuf
      with Parsing.Parse_error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
        exit 1 in 
      
      Format.printf "@[";
      Format.printf "Expression:@\n  @[";
      Pprint.printSrcExp scode;
      Format.printf "@]@\n@\n";
      Format.printf "Translating the expression...@\n";
      Format.print_flush ();
      let t = Translate.cpsTranslate scode in
      Format.printf "@[";
      Format.printf "Translated Expression:@\n  @[";
      Pprint.printTargExp t;
      Format.printf "@]@\n@\n"; 
      TE_App (t, Eval.id_cont)
	
    else raise BadFileType
  in
  Format.printf "Evaluating the expression...@\n";
  Pprint.printTargExp tcode;
  Format.printf "@]@\n@\n"; 
  Format.print_flush ();  
  let v = Eval.eval tcode in 
  Format.printf "Result:@\n  @[";
  Pprint.printTargExp v;
  Format.printf "@]@\n";
  Format.printf "@]"
      
