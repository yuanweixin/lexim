import lexim
import strutils
import unittest
import std/sugar
import patty

variantp Token:
    Type
    Var
    Function
    Break
    Of
    End
    In
    Nil
    Let
    Do
    To
    For
    While
    Else
    Then
    If
    Array
    Assign
    Or
    And
    Ge
    Gt
    Le
    Lt
    Neq
    Eq
    Divide
    Times
    Minus
    Plus
    Dot
    Rbrace
    LBrace
    Rbrack
    Lbrack
    Rparen
    Lparen
    Semicolon
    Colon
    Comma
    String(s: string)
    Int(i: int)
    Id(id: string)
    Eof

type
    TigerlexState* = object
        strBody: string
        commentDepth: int

proc newLexState*(): TigerlexState =
    result = TigerlexState(strBody: "", commentDepth: 0)

genStringMatcher tigerTokenIter[TigerlexState, Token]:
    r"\n":
        discard
    r"\s":
        discard
    r"type":
        yield Type()
    r"var":
        yield Var()
    r"function":
        yield Function()
    r"break":
        yield Break()
    r"of":
        yield Of()
    r"end":
        yield End()
    r"in":
        yield In()
    r"nil":
        yield Nil()
    r"let":
        yield Let()
    r"do":
        yield Do()
    r"to":
        yield To()
    r"for":
        yield For()
    r"while":
        yield While()
    r"else":
        yield Else()
    r"then":
        yield Then()
    r"if":
        yield If()
    r"array":
        yield Array()
    r"\:=":
        yield Assign()
    r"\|":
        yield Or()
    r"&":
        yield And()
    r">=":
        yield Ge()
    r">":
        yield Gt()
    r"<=":
        yield Le()
    r"<":
        yield Lt()
    r"=":
        yield Eq()
    r"<>":
        yield Neq()
    r"/":
        yield Divide()
    r"\*":
        yield Times()
    r"-":
        yield Minus()
    r"\+":
        yield Plus()
    r"\.":
        yield Dot()
    r"\}":
        yield Rbrace()
    r"\{":
        yield Lbrace()
    r"\]":
        yield Rbrack()
    r"\[":
        yield Lbrack()
    r"\)":
        yield Rparen()
    r"\(":
        yield Lparen()
    r"\:":
        yield Colon()
    r";":
        yield Semicolon()
    r",":
        yield Comma()
    """["]""":
        beginState(string)
    string:
        """["]""":
            beginState(initial)
            yield String(lexState.strBody)
            lexState.strBody = ""
        r"\\t":
            lexState.strBody.add "\t"
        r"\\n":
            lexState.strBody.add "\n"
        """\\\"""":
            lexState.strBody.add "\""
        r"\\\\":
            lexState.strBody.add "\\"
        r"\\b":
            lexState.strBody.add "\b"
        r"\\r":
            lexState.strBody.add "\r"
        r"\\f":
            lexState.strBody.add "\f"
        r"\\[0-9]{3,3}":
            let i = parseInt(input.substr(oldPos+1, pos-1))
            lexState.strBody.add $chr(i)
        """\\(\t|\f|\n| )+\\""":
            discard
        r".":
            lexState.strBody.add input.substr(oldPos, pos-1)
    comment:
        r"/\*":
            inc lexState.commentDepth
        r"\*/":
            dec lexState.commentDepth
            if lexState.commentDepth == 0:
                beginState(initial)
        r".":
            discard
    r"/\*":
        inc lexState.commentDepth
        beginState(comment)
    r"[0-9]+":
        yield Int(parseInt(input.substr(oldPos, pos-1)))
    r"[a-zA-Z][a-zA-Z_0-9]*":
        yield Id(input.substr(oldPos, pos-1))
    "\0":
        yield Eof()
    r".":
        raise newException(Exception, "Unexpected character###" & input.substr(
                oldPos, pos-1) & "### at [" & $oldPos & "," & $(pos-1) & "]")

test "queens.tig":
    let expected = @[Let(), Var(), Id("N"), Assign(), Int(8), Type(), Id(
            "intArray"), Eq(), Array(), Of(), Id("int"), Var(), Id("row"),
            Assign(), Id("intArray"), Lbrack(), Id("N"), Rbrack(), Of(), Int(0),
            Var(), Id("col"), Assign(), Id("intArray"), Lbrack(), Id("N"),
            Rbrack(), Of(), Int(0), Var(), Id("diag1"), Assign(), Id(
            "intArray"), Lbrack(), Id("N"), Plus(), Id("N"), Minus(), Int(1),
            Rbrack(), Of(), Int(0), Var(), Id("diag2"), Assign(), Id(
            "intArray"), Lbrack(), Id("N"), Plus(), Id("N"), Minus(), Int(1),
            Rbrack(), Of(), Int(0), Function(), Id("printboard"), Lparen(),
            Rparen(), Eq(), Lparen(), For(), Id("i"), Assign(), Int(0), To(),
            Id("N"), Minus(), Int(1), Do(), Lparen(), For(), Id("j"), Assign(),
            Int(0), To(), Id("N"), Minus(), Int(1), Do(), Id("print"), Lparen(),
            If(), Id("col"), Lbrack(), Id("i"), Rbrack(), Eq(), Id("j"), Then(),
            String(" O"), Else(), String(" ."), Rparen(), Semicolon(), Id(
            "print"), Lparen(), String("\n"), Rparen(), Rparen(), Semicolon(),
            Id("print"), Lparen(), String("\n"), Rparen(), Rparen(), Function(),
            Id("try"), Lparen(), Id("c"), Colon(), Id("int"), Rparen(), Eq(),
            Lparen(), If(), Id("c"), Eq(), Id("N"), Then(), Id("printboard"),
            Lparen(), Rparen(), Else(), For(), Id("r"), Assign(), Int(0), To(),
            Id("N"), Minus(), Int(1), Do(), If(), Id("row"), Lbrack(), Id("r"),
            Rbrack(), Eq(), Int(0), And(), Id("diag1"), Lbrack(), Id("r"), Plus(),
            Id("c"), Rbrack(), Eq(), Int(0), And(), Id("diag2"), Lbrack(), Id(
            "r"), Plus(), Int(7), Minus(), Id("c"), Rbrack(), Eq(), Int(0),
            Then(), Lparen(), Id("row"), Lbrack(), Id("r"), Rbrack(), Assign(),
            Int(1), Semicolon(), Id("diag1"), Lbrack(), Id("r"), Plus(), Id(
            "c"), Rbrack(), Assign(), Int(1), Semicolon(), Id("diag2"), Lbrack(),
            Id("r"), Plus(), Int(7), Minus(), Id("c"), Rbrack(), Assign(), Int(
            1), Semicolon(), Id("col"), Lbrack(), Id("c"), Rbrack(), Assign(),
            Id("r"), Semicolon(), Id("try"), Lparen(), Id("c"), Plus(), Int(1),
            Rparen(), Semicolon(), Id("row"), Lbrack(), Id("r"), Rbrack(),
            Assign(), Int(0), Semicolon(), Id("diag1"), Lbrack(), Id("r"), Plus(),
            Id("c"), Rbrack(), Assign(), Int(0), Semicolon(), Id("diag2"),
            Lbrack(), Id("r"), Plus(), Int(7), Minus(), Id("c"), Rbrack(),
            Assign(), Int(0), Rparen(), Rparen(), In(), Id("try"), Lparen(),
            Int(0), Rparen(), End()]

    let input = readFile("tests/queens.tig")
    var lexState = newLexState()
    let actual = collect(newSeq):
        for token in tigerTokenIter(input, lexState): token
    doAssert expected == actual
