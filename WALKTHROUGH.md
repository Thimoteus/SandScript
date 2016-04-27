# Parsing

If you haven't already, install the `purescript-parsing` package:

`bower i --save purescript-parsing`

There are many parsing libraries in the Purescript ecosystem, but I've found that
the only one that works for our purposes is the above.

I'm also using `purescript-coercible` for type-safe coercions.

Let's start with the module's imports:

```purescript
import Prelude

import Data.Either (Either(..))
import Data.List as L
import Data.Coercible (coerce)
import Data.String (toCharArray, contains)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, try, sepBy, (<?>), sepEndBy)
import Text.Parsing.Parser.String (char, whiteSpace, oneOf, noneOf, satisfy, string)

import Control.Lazy (fix)
import Control.Alt ((<|>))
import Control.Apply ((*>))
```

Speaking of modules, you can choose to have all your code in one though I suggest
splitting it up into separate modules.

## Boilerplate

While the `purescript-parsing` library is good (after all, it's the only one that's
good enough for our purposes), it's lacking some basic combinators we'll need.

Let's define those now:

```purescript
infixr 5 L.Cons as :

type P a = Parser String a

many1 :: forall a. P a -> P (L.List a)
many1 p = do
  x <- p
  xs <- L.many p
  pure (x : xs)
```

`many1` takes a parser and matches one or more occurrences of it, putting the results
into a `List`.

```purescript
anyOf :: String -> P Char
anyOf s = satisfy \ c -> contains (coerce c) s
```

`anyOf` basically lets us treat `String`s as they are in Haskell, as `List`s of
`Char`s. The combinator we just defined takes in a string and matches any character
appearing in it. We'll use it as follows:

```purescript
anyLetter :: P Char
anyLetter = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

anyDigit :: P Char
anyDigit = anyOf "0123456789"

symbol :: P Char
symbol = anyOf "!#$%&|*+-/:<=>?@^_~"
```

## PSCi interlude

Now we'll write a function that takes a `String`, parses it, and tells us whether
it's matched a symbol or not:

```purescript
readExpr :: String -> String
readExpr input = case runParser input symbol of
                      Right _ -> "Found value"
                      Left err -> "No match: " <> show err
```

You can try this out in PSCi by importing the module and calling `readExpr`:

```
> readExpr "#"
"Found value"

> readExpr "nope"
"No match: ParseError { message: Character 'n' did not satisfy predicate, position: Position { line: 1, column: 2 } }"
```

## Incremental improvements

Let's change the definition of `parseExpr` so it can handle whitespace:

```purescript
readExpr input = case runParser input (whiteSpace *> symbol) of
```

The `(*>)` combinator takes a parser, matches it, ignores the result, then continues
with the next parser and returns its value.

Now you can try it in PSCi and feed it symbolic values prepended with whitespace or not, and it'll match.

## Defining an AST

Since we're building a programming language, we need to give it a grammar. I've
done this in a different module:

```purescript
import Data.List (List)

data WFF = Atom String
         | List (List WFF)
         | DotList (List WFF) WFF
         | Integer Int
         | String String
         | Bool Boolean
```

Here, "WFF" stands for "well-formed formula". They're mostly self-explanatory.
You can think of dotlists as nonempty lists where the last element is held
separately from the rest -- a so-called "witness" to nonemptiness.

## Parsing well-formed-formulas

Let's write a parser for variables first (corresponding to our atoms):

```purescript
parseAtom :: P WFF
parseAtom = do
  first <- anyLetter <|> symbol
  rest <- L.many $ anyLetter <|> anyDigit <|> symbol
  let atom = coerce $ first : rest
  pure case atom of
            "True" -> Bool true
            "False" -> Bool false
            _ -> Atom atom
```

The `(<|>)` combinator is the essence of the `Alt` typeclass: its intended interpretation
is sort of like a sequential or. That is, if the first argument somehow fails,
the result of the second is returned. To that end, I prefer to read it as "or".

This parser can be read as "Let the first character be any letter or symbol, and
the rest of the string be many occurrences of any letter, any digit, or any symbol.
Then if the result is the string 'True', return a `Bool` holding `true`; if it's
'False', return a `Bool` holding `false`, otherwise it's an `Atom`."

## FFI interlude

Since we need to somehow go from a `String` to an `Int`, the easiest way will be
to go via the FFI. In a Javascript file, put the following:

```javascript
// module <X>

exports.str2int = function (str) {
  return (str | 0);
}
```
where <X> is the name of the module you're working in.

Then in that module,

```purescript
foreign import str2int :: String -> Int
```

Note that any `String` which is malformed as a representation of an `Int` will
be mapped to `0`.

## Back to WFFs

Now we can write our parser for `Integer`s:

```purescript
parseInteger :: P WFF
parseInteger = Integer <$> do
  xs <- many1 anyDigit
  pure $ str2int $ coerce xs
```

and `String`s:

```purescript
parseString :: P WFF
parseString =
  String <<< coerce <$> between (char '"') (char '"') (L.many $ noneOf ['"'])
```

Note that we're not using `do` notation any more. Often we can write one-liners
that do what we want, and that sometimes makes things easier to work with since
we're not defining things that take up multiple LOCs.

To read this, we'll start with the part to the right of `<$>`: the `between`
combinator first takes a parser denoting the opener, then the closer, then
the parser to be matched. So we want many characters that aren't a double quote,
in between two double quote characters. The left of the `<$>` is merely function
composition of two functions. The direction of the arrowheads gives a hint as to
how it should be read: "first coerce, then apply String".

Finally, we have `<$>`: this connects the left-hand side (which is just a function)
and the right-hand side (which is a parser). All it does is apply the function to
the result of the parser.

Now we can write a parser that'll match a `String`, `Atom` or `Integer`:

```purescript
parseExpr :: P WFF
parseExpr = try parseAtom
        <|> try parseString
        <|> try parseInteger
        <?> "Expecting atom, string or integer"
```

and change our `readExpr` accordingly:

```purescript
readExpr input = case runParser input parseExpr of
```

Go ahead and try it in PSCi.

## Recursive parsing in a strict language

Unfortunately for us, strictness makes recursive parsing difficult. But we can
still do what we need.

The simplest definition will be for lists:

```purescript
parseList :: P WFF -> P WFF
parseList p = map List $ sepBy p whiteSpace
```

`map` is a prefix version of `<$>`. `sepBy` matches 0 or more occurrences of `p`
separated and optionally terminated by `whiteSpace`.

Note that the type definition takes in a parser as input. None of our previous
parsers have been of this form; this is because a `List` could itself contain
further `List`s, and so on ad infinitum. We'll get to how this all works after
we define the other parsers:

```purescript
parseDotList :: P WFF -> P WFF
parseDotList p = do
  head <- sepEndBy p whiteSpace
  char '.' *> whiteSpace
  tail <- p
  pure $ DotList head tail

parseQuoted :: P WFF -> P WFF
parseQuoted p = do
  string "'"
  x <- p
  pure $ List $ Atom "quote" : x : L.Nil
```

and rewrite `parseExpr` to take in these new parsers:

```purescript
parseExpr :: P WFF
parseExpr = fix \ p -> try parseAtom
                    <|> try parseString
                    <|> try parseInteger
                    <|> parseQuoted p
                    <|> between (char '(') (char ')') (parseList p)
                    <?> "Malformed input"
```

This is where the magic happens: `fix :: forall a. Lazy a => (a -> a) -> a`.
`fix` takes the least fixed point of a `Lazy`. Since Haskell is lazy by default,
this part isn't needed. But if we tried to do it the way they do it in Haskell-land,
we'd get an error about not being able to define something somewhere (or something).

Note that we pass the argument to our anonymous function to the recursively defined
parsers.

# Evaluation

Now we'll start adding the ability to evaluate wffs in our language.

Let's start by adding the ability to turn `WFF`s into `String`s:

```purescript
instance showWFF :: Show WFF where
  show (Atom n) = n
  show (String s) = show s
  show (Integer n) = show n
  show (Bool true) = "True"
  show (Bool _) = "False"
  show (List xs) = "(" <> unwordsList show xs <> ")"
  show (DotList xs x) = "(" <> unwordsList show xs <> " . " <> show x <> ")"

unwordsList :: (WFF -> String) -> List WFF -> String
unwordsList f = intercalate " " <<< map f
```

Here, we're giving an instance of the `Show` typeclass to our `WFF` type.
Basically, typeclasses define relationships between types, so a unary relationship
can be thought of as a predicate -- in our case, that a `WFF` can be `Show`n.

Note that in principle different instances can be given anywhere, so to prevent
this, typeclass instances are restricted to only be defined in the same module
where the class is defined, or where the type is.

For evaluation proper, I've used another module. Here are the imports:

```purescript
import Data.StrMap as Map
import Data.List as List
import Data.Foldable (foldl)
import Data.Maybe (maybe)
```

## Primitive operators

Let's add support for some basic arithmetic operations. We'll want a function
from wffs to themselves. For the nonrecursive ones, it's straightforward:

```purescript
eval :: WFF -> WFF
eval v@(String _) = v
eval n@(Integer _) = n
eval b@(Bool _) = b
eval (List (Atom "quote" : q : List.Nil)) = q
```

In Lisp, function application is done in a list where the head element is the
function and the tail are the arguments. So we need to somehow "reduce" `List`s
where the head is an `Atom` (denoting the name of the function):

```purescript
eval (List (Atom f : args)) = apply f $ eval <$> args
```

Here we reference an undefined `apply` function, which is just going to be our
own way of applying a function named by `f` to its arguments.

```purescript
apply :: String -> List.List WFF -> WFF
apply f args = maybe (Bool false) (_ $ args) $ Map.lookup f primitives
```

Again, we've referenced an undefined value, `primitives`. We'll store primitive
operations in a `StrMap`:

```purescript
data Op = Add | Sub | Mul | Div | Mod

primitives :: Map.StrMap (List.List WFF -> WFF)
primitives = Map.empty
           # Map.insert "+" (numericBinop Add)
           # Map.insert "-" (numericBinop Sub)
           # Map.insert "*" (numericBinop Mul)
           # Map.insert "/" (numericBinop Div)
           # Map.insert "mod" (numericBinop Mod)
```

In order to pattern match on which operation is being passed, we've defined a
custom type `Op`. That way we'll be able to, for example, multiply all arguments
in a list (and return the multiplicative identity 1 if the list is empty) or add
all the arguments (and return the additive identity if it's empty).


```purescript
numericBinop :: Op -> List.List WFF -> WFF
numericBinop Add = Integer <<< foldl add 0 <<< map unpackInt
numericBinop Mul = Integer <<< foldl mul 1 <<< map unpackInt
numericBinop Sub = Integer <<< numericFold sub 0
numericBinop Div = Integer <<< numericFold div 1
numericBinop Mod = Integer <<< numericFold mod 0
```

`numericFold` is what's doing the real work here. Basically it's a way to turn
a list (a, b, c, d) and an operator :) into a :) b :) c :) d, and some default
value if the list is otherwise empty:

```purescript
numericFold :: (Int -> Int -> Int) -> Int -> List.List WFF -> Int
numericFold op _ (x : xs) = foldl op (unpackInt x) $ unpackInt <$> xs
numericFold op i _ = i
```

And the final definition:
```purescript
unpackInt :: WFF -> Int
unpackInt (Integer n) = n
unpackInt _ = 0
```

## Testing

Let's define a module `Main` with the following imports, replacing the module
names in brackets:

```purescript
import <ParserModule> (read)
import <EvalModule> (eval)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

rep :: forall eff. String -> Eff ( console :: CONSOLE | eff ) Unit
rep = print <<< eval <<< read

main :: Eff ( console :: CONSOLE ) Unit
main = rep "(mod 30 9)"
```

and you can run it with e.g. `pulp run`.
