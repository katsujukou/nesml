module Test.Parsing.LexerSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Language.Nesml.Parsing.Errors (ParserErrorType(..), positionedError, rangedError)
import Language.Nesml.Parsing.Lexer (LexerState, StateCode(..), comment, lex, skipWhitespacesComments, whitespaces)
import Language.Nesml.Parsing.Monad (runParserM)
import Language.Nesml.Parsing.Types (Token(..))
import Test.Parsing.Utils (point, posHead, ranged, shouldBeParsedAs)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

init :: String -> LexerState
init = { pos: { ln: 1, col: 1 }, src: _, strbuf: "", code: Initial }

spec :: Spec Unit
spec = describe "Language.Nesml.Parsing.Lexer" do
  describe "whitespaces and comments" do
    it "should successfully read whitespaces" do
      st' /\ res <- liftEffect $ runParserM (init "  \tabc") whitespaces
      res `shouldEqual` (Right $ Just "  \t")
      st'.src `shouldEqual` "abc"
      st'.pos `shouldEqual` { ln: 1, col: 4 }

    it "should successfully read line feeds" do
      st' /\ res <- liftEffect $ runParserM (init "\n\r\nabc") whitespaces
      res `shouldEqual` (Right $ Just "\n\r\n")
      st'.src `shouldEqual` "abc"
      st'.pos `shouldEqual` { ln: 3, col: 1 }

    it "should successfully read single comment" do
      st' /\ res <- liftEffect $ runParserM (init "{* foo *} abc") comment
      res `shouldEqual` (Right $ Just "{* foo *}")
      st'.src `shouldEqual` " abc"
      st'.pos `shouldEqual` { ln: 1, col: 10 }

    it "should successfully read nested comment" do
      st' /\ res <- liftEffect $ runParserM (init "{* {* nested *} foo *} abc") comment
      res `shouldEqual` (Right $ Just "{* {* nested *} foo *}")
      st'.src `shouldEqual` " abc"
      st'.pos `shouldEqual` { ln: 1, col: 23 }

    it "should successfully read comment containing linefeed" do
      st' /\ res <- liftEffect $ runParserM (init "{* foo\nbar *} abc") comment
      res `shouldEqual` (Right $ Just "{* foo\nbar *}")
      st'.src `shouldEqual` " abc"
      st'.pos `shouldEqual` { ln: 2, col: 7 }

    it "should raise parse error when detect an isolated close comment" do
      st' /\ res <- liftEffect $ runParserM (init "*}") comment
      res `shouldEqual` (Left $ rangedError { start: { ln: 1, col: 1 }, end: { ln: 1, col: 3 } } UnexpectedCloseComment)
      st'.src `shouldEqual` ""
      st'.pos `shouldEqual` { ln: 1, col: 3 }

    it "should raise parse error when reach end-of-file while reading comment" do
      st' /\ res <- liftEffect $ runParserM (init "{* abc ") comment
      res `shouldEqual` (Left $ positionedError { ln: 1, col: 8 } MissingCloseComment)
      st'.src `shouldEqual` ""
      st'.pos `shouldEqual` { ln: 1, col: 8 }

    it "should successfully discard a sequence of whitespace charaters and comments" do
      let
        src = "{* foo *}\n  {* multiline \t{* nested comment *}\n*}  	\n    abc"
      st' /\ _ <- liftEffect $ runParserM (init src) skipWhitespacesComments
      st'.src `shouldEqual` "abc"
      st'.pos `shouldEqual` { ln: 4, col: 5 }

    it "should bring no changes when source has no leading comments and whitespaces." do
      let src = "abc"
      st' /\ _ <- liftEffect $ runParserM (init src) skipWhitespacesComments
      st'.src `shouldEqual` src
      st'.pos `shouldEqual` { ln: 1, col: 1 }

  describe "token parser" do
    it "should read out EOF from an empty text" do
      res <- liftEffect $ lex ""
      res `shouldEqual` [ Right $ ranged (point posHead) TokEOF ]

    it "should read out EOF from a text containing only commets and whitespaces" do
      res <- liftEffect $ lex "   {**}\t \n{* foo {* bar *}\r\n {* baz {* quz \n qus *} *} \r\n \r {* abc *} {*\t*} *}"
      res `shouldEqual` [ Right $ ranged (point { ln: 5, col: 20 }) TokEOF ]

    it "should successfully parse \"(\"" do
      "(" `shouldBeParsedAs` [ TokLeftParens ]

    it "should successfully parse \")\"" do
      ")" `shouldBeParsedAs` [ TokRightParens ]

    it "should successfully parse \"{\"" do
      "{" `shouldBeParsedAs` [ TokLeftBrace ]

    it "should successfully parse \"}\"" do
      "}" `shouldBeParsedAs` [ TokRightBrace ]

    it "should successfully parse \"[\"" do
      "[" `shouldBeParsedAs` [ TokLeftSquare ]

    it "should successfully parse \"]\"" do
      "]" `shouldBeParsedAs` [ TokRightSquare ]

    it "should successfully parse \",\"" do
      "," `shouldBeParsedAs` [ TokComma ]

    it "should successfully parse \";\"" do
      ";" `shouldBeParsedAs` [ TokSemicolon ]

    it "should successfully parse \"`\"" do
      "`" `shouldBeParsedAs` [ TokBacktick ]

    it "should successfully parse \"_\"" do
      "_" `shouldBeParsedAs` [ TokUnderscore ]

    it "should successfully parse operator" do
      "==" `shouldBeParsedAs` [ TokOperator [] false "==" ]
      "->" `shouldBeParsedAs` [ TokRightArrow ]
      "-" `shouldBeParsedAs` [ TokOperator [] false "-" ]
      "\\/" `shouldBeParsedAs` [ TokOperator [] false "\\/" ]

    it "should successfully parse prefix operator" do
      "(==)" `shouldBeParsedAs` [ TokOperator [] true "==" ]
      "(->)" `shouldBeParsedAs` [ TokRightArrow ]
      "(.)" `shouldBeParsedAs` [ TokOperator [] true "." ]

    it "should successfully parse unqualified identifier" do
      "x" `shouldBeParsedAs` [ TokLowerIdentifier [] "x" ]
      "x1" `shouldBeParsedAs` [ TokLowerIdentifier [] "x1" ]
      "x'" `shouldBeParsedAs` [ TokLowerIdentifier [] "x'" ]
      "_x" `shouldBeParsedAs` [ TokLowerIdentifier [] "_x" ]
      "_x1" `shouldBeParsedAs` [ TokLowerIdentifier [] "_x1" ]
      "_1" `shouldBeParsedAs` [ TokLowerIdentifier [] "_1" ]
      "__" `shouldBeParsedAs` [ TokLowerIdentifier [] "__" ]
      "_'" `shouldBeParsedAs` [ TokLowerIdentifier [] "_'" ]
      "X" `shouldBeParsedAs` [ TokUpperIdentifier [] "X" ]
      "X'" `shouldBeParsedAs` [ TokUpperIdentifier [] "X'" ]
      "X1" `shouldBeParsedAs` [ TokUpperIdentifier [] "X1" ]

    it "should successfully parse qualified identifier" do
      "Foo::x" `shouldBeParsedAs` [ TokLowerIdentifier [ "Foo" ] "x" ]
      "Foo::Bar::x" `shouldBeParsedAs` [ TokLowerIdentifier [ "Foo", "Bar" ] "x" ]
      "Foo::Bar::X" `shouldBeParsedAs` [ TokUpperIdentifier [ "Foo", "Bar" ] "X" ]

    it "should successfully parse qualified operator" do
      "Foo:::" `shouldBeParsedAs` [ TokOperator [ "Foo" ] false ":" ]

    it "should successfully parse qualified prefix operator" do
      "Foo::(:)" `shouldBeParsedAs` [ TokOperator [ "Foo" ] true ":" ]

    it "should successfully parse decimal integers" do
      "42" `shouldBeParsedAs` [ TokInteger "42" 42 ]
      "-42" `shouldBeParsedAs` [ TokInteger "-42" (-42) ]
      "+42" `shouldBeParsedAs` [ TokInteger "+42" 42 ]
      "0" `shouldBeParsedAs` [ TokInteger "0" 0 ]

    it "should successfully parse binary indeters" do
      "0b1111" `shouldBeParsedAs` [ TokInteger "0b1111" 15 ]

    it "should successfully parse hexadecimal indeters" do
      "0x1a" `shouldBeParsedAs` [ TokInteger "0x1a" 0x1a ]
      "0xaa" `shouldBeParsedAs` [ TokInteger "0xaa" 0xaa ]
