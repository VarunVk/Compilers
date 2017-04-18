signature PARSE =
sig
  val parse: string -> string -> unit
end

functor ParseFun(structure Symbol: SYMBOL) : PARSE=
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
  structure MipsRegs = MipsRegsFun(structure T = Temp)
  structure MipsFrame = MipsFrameFun(structure T = Temp
                                     structure MipsRegs=MipsRegs
                                       structure Tree = IR)
  structure Translate = TranslateFun(structure F = MipsFrame
                                     structure T = Temp
                                     structure IR = IR)
  structure Env = EnvFun(structure Symbol = Symbol
                         structure T = Temp
                         structure Tr = Translate)
  structure TigerSem = SemantFun(structure A = Absyn
                                 structure E = Env
                                 structure Symbol = Symbol
                                 structure Tr = Translate
                                 structure T = Temp)
  structure PrintAbsyn = PrintAbsynFun(structure A = Absyn
                                       structure Symbol = Symbol)
  structure PrintType = PrintTypeFun(structure E= Env
                                     structure Symbol = Symbol)
  structure PrintTree = PrintTreeFun(structure T = IR
                                     structure F = MipsFrame
                                     structure Temp = Temp)

  fun parse infilename outfilename =
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
        then PrintTree.printprog outfile (Translate.getResults())
        else ();
	TextIO.closeOut outfile
      end handle LrParser.ParseError => raise ErrorMsg.Error

end


structure Parse = ParseFun(structure Symbol = Symbol)
