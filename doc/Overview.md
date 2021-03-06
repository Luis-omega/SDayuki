# SDayuki compiler overview


This is a high level description the compiler faces, currently it would be used
as a spec.
The types defined here can be just fictitious or could be implemented in a 
different way but behaviour must be the same as described.

## Syntax/Grammar

The language must be syntax oriented, that means that bidirectional type 
checking is a priority. This means that some times we would be out of the
traditional way in syntax matters to allow the language to be 
deterministically parsed and semantically clean.

## File Model

This part specifies how files are view from the compiler but this don't
mean that.

### Characters, lines, column, position and range.

#### Character.

We say that a **character** is a **Unicode code point**. 

#### Line.

A **line** is a list of characters that follow one of those :
  - Characters from the beginning of file until the first 
    occurrence of **\n** character or **EOF**.
  - Characters between two **\n** including the last **\n**
    that don't have another **\n** between them.
  - Characters between the last **\n** and the **EOF**.

The first case is the **First line** related to file and the third case is 
called the **Last line** related to file.

#### Column number.

A **column number** for a character inside a line is the number of 
characters from the beginning of the list, this means that the 
first character has column number 0.

#### Logical File.

A **logical file** is a list of lines.


#### Line Number.

A **line number** for a line in a logical file is the number of lines that
exists before the line. This means that first line has line number 0,
and last line has the greater line number inside a file.


#### Absolute position.

The **absolute position** for a character **c** with line number **X** 
and column number **Y** is the sum of all the number of characters inside
all lines with line number lower than **X** plus **Y** minus 1.
It just means the number of characters inside file that are before the 
character at line number **X** and column number **Y**.

#### Position.

Every character **c** inside a file **F** has then an associated 
position consisted in :

  - Absolute position of the character.
  - Line number of the character.
  - Column number of the character.


#### Range.

A **range** is isomorphic to the product of two positions **P1** and **P2** 
with the additional constraint that 
`P1.absolutePosition <= P2.absolutePosition`.
We say that **P1** is the **start position** and **P2** is the **end position**.



## Lexer and Indentation

Lexer must be as deterministic as possible, that means that we need a way to
proof that there can't be token collisions.

Lexer must be maintained simple, it's only responsibility is to convert 
plain text to Tokens. 

This means Lexer must have a signature like 

```
  lexer :: Text -> [RealToken]
```


### Tokens

There's two kind of tokens, tokens associated with real text inside a 
file and tokens that are generated by indentation handling or desugaring.
Both kind of tokens have a range.

So, tokens can looks like (but it could be implemented different)

```
data GeneratedTokenKind = 
  | IdentationStartToken
  | DesugaringToken

data RealTokenKind = 
    NewLineToken
  | ...

data RealToken = 
  RealToken {tokenRange :: Range, realTokenText:: Text, realTokenKind::Text}

data GeneratedToken =
  GeneratedToken {tokenRange :: Maybe Range, generatedTokenKind :: GeneratedTokenKind }

data TokenWrapper = 
  | RealTokenWrapper RealToken 
  | GeneratedTokenWrapper GeneratedTokenWrapper
```


### Indentation

We have a controversial decision here since we only allow spaces for indentation, all 
other characters at the beginning of a line are treat as non indentation. 

We mantain a Indentation stack to get track of current indentation level.

We have certain real tokens like **let** that insert a **indentation token** between the 
real tokens produced by lexer. 

A example : 

```
  let a =
         c 
      d = z
         in w
  somethingMore ... 
```

would produce the lexer output (with column number inside parentheses): 

```
  [
  LetToken(2), VariableToken(a,6), EqualToken(8), NewLineToken(9), 
  VariableToken(c,9), NewLineToken(10)
  VariableToken(d,6), EqualToken(8), VariableToken(z,10), NewLineToken(11)
  ,InToken(9), VariableToken(w,12), NewLineToken(13), ... 
  ]
```

And after processed it would looks like :


```
  [
  LetToken(2), LetIndentationStart(6), VariableToken(a,6), EqualToken(8) 
  ,VariableToken(c,9)
  ,LetIndentationSeparator(6), VariableToken(d,6), EqualToken(8), VariableToken(z,10)
  ,LetIndentationEnd(9), InToken(9), InIndentationStart(12),VariableToken(w,12),
  InIndentationEnd, ... 
  ]
```

In general them follow the structure (in ebnf format)

```
  keywordInitial someToken1(indentation>keywordInitial.indentation) 
  (
    someToken(indentation>SomeToken1.indentation)*
    NewLine 
    someToken(indentation=SomeToken1.indentation)
  )* 
  (
      matchingEndToken(indentation>=keywordInitial.indentation)
    | someToken(indentation<someToken1.indentation)
  )
```


