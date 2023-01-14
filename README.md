# lexim
Lexer generator for Nim. Based on the [implementation by Andreas Rumpf](https://github.com/Araq/lexim). 


I extended Araq's version with: 
* lexer states

  what flex calls "exclusive start conditions". There is always an "initial" state. Non-initial states are always exclusive. For my use cases of tokenizing strings and comments, this is enough. It would be pretty trivial to add support for "inclusive" rules, amounting to adding each inclusive rule to each exclusive state, before compiling the state's dfa. 

* dsl syntax 

  for specifying lexer states. 

* longest-matching rule: 

  the original version would discard inputs if you give it a pattern "a|aab|b" and input "aab", where it discards "aa" and matches "b". This doesn't make sense for a lexer in a programming language where you want it to succeed only if it was able to match the entire input. I extended the code so that it actually remembers the longest so-far matched position and backtracks to that when it runs out of possible matches. 

* jam state: 

  the original version would fail to terminate by never incrementing the current position and just dropping through the case statement in a while(true) loop. I changed it so that it would go to a jam state, which is hard coded to throw an exception. 

* iterator interface 

  in a parser context, the lexer provides a stream of tokens. thus, the generated code is a proc that returns an iterator. 

* global state support

  the generated iterator takes in a user defined state object, that is available to the user action code to be mutated. this can be used to implement, for example, nested comments and collecting together a string body. 

The implementation trades off code size for speed, by directly encoding a dfa and its actions as a series of goto's in while(true) case statement. 

# details 

Lexim implements fsa based scanner, using the classic Regex -> NFA -> DFA -> minimization. Regex parsing uses recursive descent. The NFA -> DFA with subset construction, DFA minimization using Hopcroft's algorithm. 

The library uses a good amount of metaprogramming to go from the dsl to the dfa tables directly embedded in code. The code generated is an iterator, that internally encodes the DFA transition tables as goto in a while loop, which is more efficient than simulating the DFA separately via table reads. Start conditions are translated into individual DFA's, and the DFA's are combined into 1 loop, with properly offsetted labels. Longest match is implemented by adding bookkeeping on the longest matching position and longest matched state's action label, which is jumped to when we fail to find a longer match. The jam state is the default jump target if there is no match found. 

No buffering scheme is given for the scanner. Since the toy compiler I write with this runs on modern desktops, memory is assumed to be abundant and it is just added complexity to throw in buffering. This eliminates incidental complexities such as needing to deal with with multi-line tokens (if using a line-oriented buffer), or dealing with the possibility that a single token can exceed your buffer size. 

Lexim uses a `lexe` helper exe. It is done to significantly speed up the compilation process by offloading the expensive DFA construction to a separate process instead of doing it in the compiler VM (not only is it slow, but for more complicated regex it generates the `interpretation requires too many iterations` error, which could be mitigated with compiler option `maxLoopIterationsVM:N` but it will still be significantly slower). The package is set up so that when you do `nimble install` it will compile `lexe` and put that into your nimble cache. 


# development

run `nimble install` if you are hacking this lib and need to test out changes to `lexe`. 

run `nimble runtests` to run the tests. 


# usage 

Basic usage (from [tests/ex1.nim](tests/ex1.nim)):

```nim

"""

import lexim

type State = object
  strBody: string
  commentDepth: int

type Token = object
  thing: int

genStringMatcher makeLexIter[State, Token]:
  "begin string":
    beginState(string)
  r"\d+":
    echo "an integer ", input.substr(oldPos, pos-1), "##"
  "else":
    echo "an ELSE"
  "elif":
    echo "an ELIF"
  "end": echo "an END"
  r"[a-zA-Z_]\w+":
    echo "an identifier ", input.substr(oldPos, pos-1), "##"
  r".":
    echo "something else ", input.substr(oldPos, pos-1), "##"
  string:
    "end string":
      echo "String(" & lexState.strBody & ")"
      lexState.strBody = ""
      beginState(initial)
    r".":
      lexState.strBody.add input.substr(oldPos, pos-1)

proc main =
  var input = "begin string hello world end stringthe 0909 else input elif elseo end"
  var state = State(strBody: "", commentDepth: 0)
  let tokenIter = makeLexIter(input)
  for thing in tokenIter(state):
    discard

main()
```

`genStringMatcher makeLexIter[State, Token]:` generates a public iterator, that takes a `var` (mutable) State object (which is accessible in the action code), yielding Token. 

`input` is a hard coded variable name for the input string being tokenized. 

`input.substr(oldPos, pos-1)` extracts the token string. `oldPos`, `pos` are hard coded variable names standing for start position of token and position after token, respectively. 

`beginState(string)` transitions to the `string` lexer state/start condition. 

`lexState` is the hard coded variable name bound to the passed in State object. 

the lexer state "initial" is the default and always available. In above example, because no lexer state was specified, all the rules fall into the initial state. 

See (tests/test_tiger.nim) for example of using the library to tokenize the tiger programming language. 

See (tests/test_counting_lines.nim) for example of implement line and column tracking. 

# Regex 

Note `[\n]` does not work because internally \n is treated as (\C\L|\L\C) and the \C\L part is a string, so can't really put that into a character class. You need to workaround it by rewriting your pattern. 

# Names accessible by user action code: 
* `input` is the input string. 
* `pos` is accessible in action code, it points to the index after the last matched token 
* `oldPos` is the first position of match 
* `beginState` can be used to transition state. It can take a string or identifier. 
* `lexState` is the name of the passed-in state object to the iterator. 

# Compile -d flag options
* `-d:leximVerbose` : this will cause it to dump out the generated nim code as well as some other diagnostic info 
* `-d:MaxelLabel=Val` :
There's a hard coded `MaxLabel` constant in `lexim/nfa.nim` which limits the total number of states you can have. It can be raised as needed by passing in the compiler flag. You need to raise it if you see error like this: 
  ```
  /home/vagrant/lexim/src/lexim.nim(315, 14) template/generic instantiation of `match` from here
  /home/vagrant/lexim/src/lexim.nim(118, 27) Error: illegal conversion from '2' to '[0..1]'
  ```

