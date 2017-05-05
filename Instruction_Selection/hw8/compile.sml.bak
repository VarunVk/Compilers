(*
These lines are just place holders for automated testing
*)
val dir_inname = "../../../Parser/testcases/translation/"
val dir_outname = "/home/vijay094/ws/Compilers/Instruction_Selection/hw8/testing/my_outputs/"

signature COMPILE =
sig
  val compile: string -> string -> unit
  val compileAll: string -> unit
end

functor CompileFun() : COMPILE =
struct 
  structure Absyn = AbsynFun(structure Symbol = Symbol)
  structure TigerLrVals = TigerLrValsFun(
                             structure Token = LrParser.Token
                             structure A = Absyn
                             structure Symbol = Symbol)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  structure FindEscapes = FindEscapesFun(structure A = Absyn
                                         structure S = Symbol)
  structure Temp = TempFun(structure S = Symbol)
  structure IR = Tree(structure Temp = Temp)
  structure Regs = MipsRegsFun(structure T = Temp)
  structure Frame = MipsFrameFun(structure T = Temp
                                     structure Tree = IR
                                     structure MipsRegs = Regs)
  structure Canon = CanonFun(structure T = IR
                             structure Temp = Temp
                             structure Symbol = Symbol)
  structure Translate = TranslateFun(structure F = Frame
                                     structure T = Temp
                                     structure IR = IR
                                     structure Canon = Canon)
  structure Env = EnvFun(structure Symbol = Symbol
                         structure T = Temp
                         structure Tr = Translate)
  structure TigerSem = SemantFun(structure A = Absyn
                                 structure E = Env
                                 structure Symbol = Symbol
                                 structure Tr = Translate
                                 structure T = Temp)
  structure Assem = AssemFun(structure Temp = Temp)
  structure MipsCodeGen = MipsCodeGenFun(structure IR = IR
                                         structure A = Assem
                                         structure Temp = Temp
                                         structure MipsRegs = Regs
                                         structure Frame = Frame)

  fun emitproc out frag =
     if (Frame.StringFrag frag)
     then ( TextIO.output(out,"LABEL ");
            TextIO.output(out,((Frame.StringFragLab frag) ^ "\n"));
            TextIO.output(out,
                          String.translate (fn x => if x = #"\"" 
                                                    then "\\\"" 
                                                    else String.str x)  
                                           (Frame.StringFragStr frag) ^ 
                                                                  "\"\n\n")
          )
     else 
          let val instrs = MipsCodeGen.codegen (Frame.ProcFragBody frag)
              val format0 = Assem.format(Regs.tempmap (fn x => x))
          in TextIO.output(out,"LABEL ");
             TextIO.output(out,(Frame.ProcFragLab frag) ^ "\n");
             (app (fn i => TextIO.output(out,format0 i)) instrs) ;
             TextIO.output(out,"\n") 
          end 

  fun compile infilename outfilename =
      let 
        val _ = (ErrorMsg.reset(); ErrorMsg.fileName := infilename)
        val _ = (Translate.initfrags())
	val infile = TextIO.openIn infilename
	fun get _ = TextIO.input infile
	fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
        val _ = FindEscapes.findescapes(absyn)
        val _ = (TigerSem.transProg absyn)
        val outfile = TextIO.openOut outfilename
      in
        TextIO.closeIn infile;
	if (not (!ErrorMsg.anyErrors))
        then (app (emitproc outfile) (Translate.getResults()))
        else ();
	TextIO.closeOut outfile
      end handle LrParser.ParseError =>
              (TextIO.output(TextIO.stdErr,
                             "Parser cannot repair the program sufficiently \ 
                                \to parse it\n");
               ())

  exception FileError

  fun compileAll filename =
    let val infile = TextIO.openIn filename
        fun process_file(infile) =
          let val current_file = TextIO.inputLine(infile)
              val current_filename = 
                 case current_file of
                        NONE => (print ("error reading" ^ filename); 
                                 raise FileError) 
                      | SOME str1 => str1;
              val current_size = String.size(current_filename);
              val current_size = current_size - 1;
              val current_filename = 
                    String.substring(current_filename,0,current_size);
              val full_inname = dir_inname ^ current_filename
              val full_outname = dir_outname ^ current_filename
              val full_outname = full_outname ^ ".out";
              val temp = (print ("Compiling... " ^ 
                                   current_filename ^ ".....\n" ); 
                          compile full_inname full_outname)
          in
            if(TextIO.endOfStream(infile))
              then
                ()
              else
                process_file(infile)
                handle _ => ()
          end
    in
      if(TextIO.endOfStream(infile))
        then
          (print (filename ^ "is empty"))
        else
          process_file(infile)
    end


end


structure Compile = CompileFun()
