See Assignment 3 for instructions.

Description of Files:
---------------------
  - ast.ml
      Defines the datatypes for the abstract syntax trees (ASTs).

  - eval.ml
      The interpreter for target language ASTs. 

  - main.ml
      The top level code that parses source language or target
      language files (depending on the file suffix). For source
      language files, it calls your cpsTranslate function to obtain a
      target language AST, and executes the target language AST. For
      target language files, it parses the file, and executes the
      target language AST.

  - lexer.mll
  - parser.mly
      A lexer and parser for source and target languages.

  - pprint.ml
      A pretty printer for the ASTs.

  - translate.ml
      CPS-style translation from the source language to the target
      language. You need to implement the cpsTranslate
      function in this file.    

  - server.ml
      Contains the implementation of server operations, such as echo,
      and double. Take a look in the file for other operations.

  - *.ch  
      Target language test programs
 
  - *.nc  
      Source language test programs
 
How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. Otherwise, if you are on a Windows machine, you can
  edit the make.bat file to point to your installation of Ocaml, and
  execute the make.bat file. If neither of these methods work, you
  should execute the commands listed in the make.bat file.

  Successful compilation will produce an executable file called "cps".

How to execute:
---------------

  Run the executable on a test program: "./cps target01.ch".

  This will parse the target language file target01.ch, build the AST,
  and execute it. You should see the output "Hello world".

  
  Now run the executable on a source language program: 
  "./cps source01.nc".  This will parse the source language file 
  source01.nc, build the AST, and then apply the function cpsTranslate
  to produce a target language AST. Assuming you have implemented this
  function, the target language AST will then be evaluated, and the
  result printed.

Notes:
------

  OCaml modes exist for emacs
  (http://caml.inria.fr/pub/docs/u3-ocaml/emacs/index.html) and vim
  (http://www.ocaml.info/vim/ftplugin/ocaml.vim).
