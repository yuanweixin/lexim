#
#
#    Lexim - The Lexer Generator for Nim
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  lexim/regexprs, lexim/nfa, macros, marshal
import std/tables
{.experimental: "caseStmtMacros".}
import fusion/matching
import std/sugar
import os 

export nfa
export regexprs

proc findMacro(name: string): PRegExpr {.used.} = nil

proc newRange(a, b: NimNode): NimNode {.compileTime.} =
  newCall(bindSym"..", a, b)

proc charSetLit(cc: set[char]): NimNode {.compileTime.} =
  const
    MaxChar = '\xFF'
  result = newNimNode(nnkCurly)
  var c1 = '\0'
  while true:
    if c1 in cc:
      var c2 = c1
      while c2 < MaxChar and succ(c2) in cc: c2 = succ(c2)
      if c1 == c2:
        result.add newLit(c1)
      elif c2 == succ(c1):
        result.add newLit(c1)
        result.add newLit(c2)
      else:
        result.add newRange(newLit(c1), newLit(c2))
      c1 = c2
    if c1 >= MaxChar: break
    inc c1

proc charAt(s: string; i: int): char {.inline.} =
  result = if i < s.len: s[i] else: '\0'

proc currChar(s, i: NimNode; isCString: bool): NimNode {.compileTime.} =
  result =
    if isCString:
      newTree(nnkBracketExpr, s, i)
    else:
      newCall(bindSym"charAt", s, i)

proc getCmp(s, i: NimNode; x: set[char]; isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"contains",  charSetLit(x), currChar(s, i, isCString))

proc getSpecial(s, i: NimNode; x: Alphabet; isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"==", currChar(s, i, isCString), newLit(x.val))

proc newVarStmt(name, typ, value: NimNode): NimNode {.compiletime.} =
  return newTree(nnkVarSection, newTree(nnkIdentDefs, name, typ, value))

proc nextState(i, state: NimNode; dest: int): NimNode {.compileTime.} =
  newStmtList(newCall(bindSym"inc", i), newAssignment(state, newLit(dest)))


const initialStartCondition = "initial"
type Rule = object 
    startCondition : string 
    regex : string 
    actions : NimNode 

proc replaceBeginState(curSc: NimNode, scStartStatesOffsetted: seq[int], scToIdx: var Table[string,int], state, n:NimNode) : NimNode = 
    case n 
    of StmtList[all @stmts]:
        let nodes = collect(newSeq):
            for stmt in stmts: replaceBeginState(curSc, scStartStatesOffsetted, scToIdx, state, stmt)
        return newStmtList(nodes)
    of ElifBranch([@cond, @stmtList]):
        return newTree(nnkElifBranch, cond, replaceBeginState(curSc, scStartStatesOffsetted, scToIdx, state, stmtList))
    of Else([@stmtList]):
        return newTree(nnkElse, replaceBeginState(curSc, scStartStatesOffsetted, scToIdx, state, stmtList))
    of IfStmt[all @branches]:
        # newIfStmt requires tuples, very annoying to use, so I will just do a nested match here. 
        let newBranches = collect(newSeq):
            for branch in branches: replaceBeginState(curSc, scStartStatesOffsetted, scToIdx, state, branch)
        return newTree(nnkIfStmt, newBranches)            
    of Call([Ident(strVal: "beginState"), @sc is (kind: in nnkStrKinds)]):
        if sc.strVal notin scToIdx:
          raise newException(Exception, "Unknown state: " & sc.strVal)
        let startState = scStartStatesOffsetted[scToIdx[sc.strVal]]
        result = quote do:
          `curSc` = `startState`
    of Call([Ident(strVal: "beginState"), Ident(strVal: @statename)]):
        if statename notin scToIdx:
          raise newException(Exception, "Unknown state: " & statename)
        let startState = scStartStatesOffsetted[scToIdx[statename]]
        result = quote do:
          `curSc` = `startState`
    else:
        return n 

proc genMatcher(state: NimNode, scStartStatesOffsetted: seq[int], scToIdx : var Table[string, int], curSc: NimNode, a: DFA; s, i: NimNode, rules: seq[Rule]; isCString: bool, labelOffset: var int, caseStmt: NimNode) {.compileTime.} =
  ## curSc: the genSym'ed variable track cur start cond's start state. 
  ## state: the genSym'ed variable for cur state inside the while loop. 
  for src in countup(1, a.stateCount):
    let rule = getRule(a, src)
    var ifStmt = newNimNode(nnkIfStmt)
    for dest in allDests(a, src):
      let (others, cs) = allTransitions(a, src, dest)
      if cs != {}:
        ifStmt.add newTree(nnkElifBranch,
                           getCmp(s, i, cs, isCString),
                           nextState(i, state, dest+labelOffset))
      for ot in others:
        if ot.kind == reChar:
          ifStmt.add newTree(nnkElifBranch,
                             getSpecial(s, i, ot, isCString),
                             nextState(i, state, dest+labelOffset))
        else:
          doAssert false, "not supported " & $ot.kind
    let actions = 
      if rule >= 1:
        let actions = replaceBeginState(curSc, scStartStatesOffsetted, scToIdx, state, rules[rule-1].actions)
        newStmtList(actions, newTree(nnkBreakStmt, newNimNode(nnkEmpty)))
      else:
        newTree(nnkBreakStmt, newNimNode(nnkEmpty))
    if ifStmt.len == 0:
      caseStmt.add newTree(nnkOfBranch, newLit(src+labelOffset), actions)
    else:
      ifStmt.add newTree(nnkElse, actions)
      caseStmt.add newTree(nnkOfBranch, newLit(src+labelOffset), ifStmt)
  labelOffset += a.stateCount 
  
proc tryGetRules(scToIdx: var Table[string,int], scToRules: var seq[seq[Rule]], n : NimNode) = 
    case n 
    of Call([Ident(strVal: @startCondition), StmtList([all @callList])]):
        for call in callList:
            case call
            of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
                if startCondition notin scToIdx:
                    scToRules.add @[]
                    scToIdx[startCondition] = scToIdx.len 
                let scIdx = scToIdx[startCondition]
                scToRules[scIdx].add Rule(startCondition: startCondition, regex:lit.strVal, actions: stmt)
            else:
                raise newException(Exception, "Unable to understand the start condition call with this AST repr " & astGenRepr call)
    of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
        let scIdx = scToIdx[initialStartCondition]
        scToRules[scIdx].add Rule(startCondition: initialStartCondition, regex:lit.strVal, actions:stmt)
    else:
        raise newException(Exception, "I do not understand the syntax of " & astGenRepr n)

template dslparse() {.dirty.} = 
  # dsl parsing 
  var scToRules : seq[seq[Rule]] 
  var scToIdx = initTable[string,int]() 
  # for sanity, initial start condition always gets the lower ordinal.
  scToIdx[initialStartCondition] = 0
  scToRules.add @[]

  case sections 
  of ArgList([StmtList([all @calls])]):
      for call in calls: 
          tryGetRules(scToIdx, scToRules, call)
  else:
      raise newException(Exception, "I do not understand the syntax " & astGenRepr sections)

  when defined(leximVerbose):
    for sc, idx in scToIdx:
      echo "sc=" & sc & ", idx=" & $idx
    for sc, rules in scToRules:
      echo "sc=" & $sc & ",rules=" & repr rules

template dfagen() {.dirty.} = 
  # dfa generation 
  var dfas : seq[DFA] 
  # we would have to generate a different version for cstring support. 
  for scIdx, rules in scToRules:
    when defined(leximSkipLexe):
      var bigRe: PRegExpr = nil
      var ruleIdx = 1
      for rule in rules:
        let rex = parseRegExpr(rule.regex, findMacro,
                                {reNoCaptures, reNoBackrefs})
        rex.rule = ruleIdx
        if bigRe.isNil: bigRe = rex
        else: bigRe = altExpr(bigRe, rex)
      var n: NFA
      var d, o: DFA
      regExprToNFA(bigRe, n)
      let alph = fullAlphabet(n)
      NFA_to_DFA(n, d, alph)
      optimizeDFA(d, o, alph)
      dfas.add o
    else:
      # use 'lexe.exe' helper program in order to speedup lexer generation
      var res: seq[string] = @[]
      for rule in rules:
        res.add rule.regex
      let data = $$res
      let lexeOut = staticExec("lexe", input=data, cache=data)
      when defined(leximVerbose):
        echo "lexe output (from regex construction):"
        echo lexeOut
      # for reasons unknown, can't catch exceptions from the call. it would
      # just crash the compiler. so, just rely on the previous lines to get 
      # the hint that something went wrong. 
      let o = to[DFA](lexeOut)
      dfas.add o

template codegen() {.dirty.} = 
  # because states for individual DFA fall in the range(1..numStates)
  # but we have multiple DFA, so keep running count of states added so far 
  # incremented once for each dfa processed. 
  var labelOffset = 0
  # pos, oldPos, input, lexState intentionally exposed to user action code 
  let pos = ident("pos")
  let oldPos = ident("oldPos")
  let input = ident("input")
  let lexState = ident("lexState")

  let curSc = genSym(nskVar, "curSc")  
  let state = genSym(nskVar, "state")

  var stateCnt = 0 
  var scStartStatesOffsetted: seq[int]
  for dfa in dfas:
    scStartStatesOffsetted.add stateCnt + dfa.startState
    stateCnt += dfa.stateCount
  when defined(leximVerbose):
    echo "scStartStatesOffsetted:"
    echo $scStartStatesOffsetted

  let initScStartState = dfas[0].startState
  let caseStmt = newNimNode(nnkCaseStmt)
  caseStmt.add state
  let isCstr = newLit(true) == isCString
  for sc, dfa in dfas:
    genMatcher(state, scStartStatesOffsetted, scToIdx, curSc, dfa, input, pos, scToRules[sc], isCstr, labelOffset, caseStmt)

  let inputStrType = if isCstr: ident("cstring") else: ident("string")
  # far less brain damaging to write than the eqv in AST 
  # this needs to be a closure iterator because otherwise it would inline, try
  # to generate identical goto labels in the same compilation unit and break
  # compilation
  result = quote do:
    iterator `procName`*(`input`: `inputStrType`, `lexState`: var `lexerStateTName`) : `tokenTName` {.closure.} = 
      var `state` {.goto.} : range[1..`stateCnt`] 
      var `curSc` : range[1..`stateCnt`] = `initScStartState`
      var `pos` = 0 
      while `pos` < `input`.len:
        let `oldPos` = `pos` 
        `state` = `curSc`     
        while true:
          `caseStmt`
  echo repr result

macro match(isCString: bool, lexerStateTName, tokenTName, procName: untyped, sections: varargs[untyped]): untyped =
  dslparse()
  dfagen()
  codegen()
  
macro genStringMatcher*(name, body : untyped) : untyped = 
  echo astGenRepr name
  case name:
  of BracketExpr([@procName is Ident(), @lexerStateT is Ident(), @tokenT is Ident()]):
    result = quote do:
      match(false, `lexerStateT`, `tokenT`, `procName`, `body`)
  else:
    raise newException(Exception, "Expected procName[<LexerStateType>, <TokenType>] but got " & repr name)

macro genCStringMatcher*(name, body : untyped) : untyped = 
  echo astGenRepr name
  case name:
  of BracketExpr([@procName is Ident(), @lexerStateT is Ident(), @tokenT is Ident()]):
    result = quote do:
      match(true, `lexerStateT`, `tokenT`, `procName`, `body`)
  else:
    raise newException(Exception, "Expected procName[<LexerStateType>, <TokenType>] but got " & repr name)