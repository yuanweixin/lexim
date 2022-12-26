# lexim
Lexer generation and regex implementation for Nim. Based on the [implementation by Andreas Rumpf](https://github.com/Araq/lexim). 


I extended the original code with these features
* lexer states: what flex calls exclusive start conditions. There is always an "initial" state. Non-initial states are always exclusive. For my use cases of tokenizing strings and comments, this is enough. It would be pretty trivial to add support for "inclusive" rules, amounting to adding each inclusive rule to each exclusive state, before compiling the state's dfa. 
* dsl syntax for specifying lexer states. 
* longest-matching rule: the original version would discard inputs if you give it a pattern "a|aab|b" and input "aab", where it discards "aa" and matches "b". This doesn't make sense for a lexer in a programming language where you want it to succeed only if it was able to match the entire input. I extended the code so that it actually remembers the longest so-far matched position and backtracks to that when it runs out of possible matches. 
* jam state: the original version would fail to terminate by never incrementing the current position and just dropping through the case statement in a while(true) loop. I changed it so that it would go to a jam state, which is hard coded to throw an exception. 

The implementation trades off code size for speed, by directly encoding a dfa and its actions as a series of goto's in while(true) case statement. 

# details 

Lexim implements fsa based scanner, using the classic Regex -> NFA -> DFA -> minimization. Regex parsing uses recursive descent. The NFA -> DFA with subset construction, DFA minimization using Hopcroft's algorithm. The code generated is a computed goto that is embedded in the caller code, which is more efficient than simulating the DFA separately via table reads. 

No buffering scheme is given for the scanner. Since the toy compiler I write with this runs on modern desktops, memory is assumed to be abundant and it is just added complexity to throw in buffering. This eliminates incidental complexities such as needing to deal with with multi-line tokens (if using a line-oriented buffer), or dealing with the possibility that a single token can exceed your buffer size. 

Lexim optionally uses a `lexe` helper exe. It's not mandatory but will make the compilation process a LOT faster.  It speeds up the compilation process by offloading the expensive DFA construction to a separate process instead of doing it in the VM (for more complicated regex it generates the `interpretation requires too many iterations` error; fwiw it could be mitigated with compiler option `maxLoopIterationsVM:N` but that's ugly). The package is set up so that when you do `nimble install` it will compile `lexe` and put that into your nimble cache. 

# usage 
Please see [this](tests/ex1.nim) for a simple example. And [this](tests/test_tiger.nim) for example of using lexer states. 

Basic usage:

```nim

import lexim

type State = object 
  strBody : string 
  commentDepth : int 

type Token = object 
  thing : int 

genStringMatcher lex[State, Token]:
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
  var input = "begin string hello world end string the 0909 else input elif elseo end"
  var state = State(strBody:"", commentDepth:0)
  for thing in lex(input, state):
    discard

main()
```

`genStringMatcher lex[State, Token]:` generates a public iterator named `lex` that takes a `var` (mutable) State object (which is accessible in the action code), and the iterator yields Token's. 

Note: the lexer state "initial" is the default and always available. In above example, because no lexer state was specified, all the rules fall into the initial state. 

# Names accessible by user action code: 
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

