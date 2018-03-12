module Command.RenderSpec where

import Command.Render
import Data.Semigroup
import Data.String
import Test.Hspec
import Test.QuickCheck

instance Arbitrary CmdArgs where
  arbitrary = fmap cmdFromList arbitrary
  shrink = fmap cmdFromList . shrink . getRawArgs

typeclasses :: Spec
typeclasses = do
  describe "Typeclasses" $ do
    describe "Read" $ do
      describe "read" $ do
        it "can parse CmdArgs" $ do
          read
            "cmdFromList [\"-f\",\"--arg2\",\"--arg3\",\"--argWith\\\"Quotes\\\"\"]" `shouldBe`
            (arg "-f" <> arg "--arg2" <> arg "--arg3" <>
             arg "--argWith\"Quotes\"")
        context "when input is prefixed by cmdFromList" $ do
          it "can parse arbitrary string lists" $
            property $ \x -> read ("cmdFromList " ++ show x) == cmdFromList x
        it "is the inverse of show" $
          property $ \x -> (read . show) x == (x :: CmdArgs)
    describe "Semigroup" $ do
      describe "(<>)" $ do
        it "is associative" $
          property $ \a b c -> ((a :: CmdArgs) <> b) <> c == a <> (b <> c)
    describe "Monoid" $ do
      describe "mappend" $ do
        it "is equivalent to (<>)" $
          property $ \a b -> mappend (a :: CmdArgs) b == a <> b
      describe "mempty" $ do
        it "is the identity of (<>)" $
          property $ \x -> (x :: CmdArgs) <> mempty == x && mempty <> x == x
    describe "IsString" $ do
      describe "fromString" $ do
        it "converts a string to a single argument" $
          property $ \x -> fromString x == arg x

argumentBuilders :: Spec
argumentBuilders = do
  describe "Argument Builders" $ do
    describe "cmdFromList" $ do
      it "is the inverse of getRawArgs" $
        property $ \x -> cmdFromList (getRawArgs x) == x
    describe "arg" $ do
      it "wraps a string with no modification" $
        property $ \x -> getRawArgs (arg x) == [x]
    describe "shortFlag" $ do
      it "prepends a dash to a single character" $
        property $ \x -> getRawArgs (shortFlag x) == [['-', x]]
    describe "shortArg" $ do
      it "is a shortFlag followed by an arg" $
        property $ \a b -> getRawArgs (shortArg a b) == [['-', a], b]
    describe "longFlag" $ do
      it "prepends two dashes to a string" $
        property $ \x -> getRawArgs (longFlag x) == [('-' : '-' : x)]
    describe "longArg" $ do
      it "is a longFlag followed by an arg" $
        property $ \a b -> getRawArgs (longArg a b) == [('-' : '-' : a), b]

argumentRenderers :: Spec
argumentRenderers = do
  describe "Argument Renderers" $ do
    describe "getRawArgs" $ do
      it "is the inverse of cmdFromList" $
        property $ \xs -> getRawArgs (cmdFromList xs) == xs
    describe "renderShell" $ do
      context "when used with QuoteSingle" $ do
        it "escapes single quotes in arguments" $
          property $ \x ->
            renderShell QuoteSingle "cmd" (arg x) ==
            concat ["'cmd' '", escapeSingleQuotes x, "'"]
        it "escapes single quotes in command file path" $
          property $ \x ->
            renderShell QuoteSingle x mempty ==
            concat ["'", escapeSingleQuotes x, "'"]
        it "respects the original ordering of arguments" $
          property $ \xs ->
            renderShell QuoteSingle "cmd" (cmdFromList xs) ==
            "'cmd'" ++ concat [' ' : quoteSingle s | s <- xs]
      context "when used with QuoteDouble" $ do
        it "escapes double quotes, dollar signs, and backslashes in arguments" $
          property $ \x ->
            renderShell QuoteDouble "cmd" (arg x) ==
            concat ["\"cmd\" \"", escapeDoubleQuotes x, "\""]
        it
          "escapes double quotes, dollar signs, and backslashes in command file path" $
          property $ \x ->
            renderShell QuoteDouble x mempty ==
            concat ["\"", escapeDoubleQuotes x, "\""]
        it "respects the original ordering of arguments" $
          property $ \xs ->
            renderShell QuoteDouble "cmd" (cmdFromList xs) ==
            "\"cmd\"" ++ concat [' ' : quoteDouble x | x <- xs]
    describe "renderShellDefault" $ do
      it "is equivalent to renderShell QuoteSingle" $
        property $ \a b -> renderShellDefault a b == renderShell QuoteSingle a b

spec :: Spec
spec = parallel $ do
  typeclasses
  argumentBuilders
  argumentRenderers

escapeChars :: [(Char, String)] -> String -> String
escapeChars charMap =
  foldMap
    (\c ->
       case lookup c charMap of
         Nothing -> [c]
         Just replacement -> replacement)

escapeSingleQuotes :: String -> String
escapeSingleQuotes = escapeChars [('\'', ['\'', '"', '\'', '"', '\''])]

escapeDoubleQuotes :: String -> String
escapeDoubleQuotes =
  escapeChars [('"', ['\\', '"']), ('$', ['\\', '$']), ('\\', ['\\', '\\'])]

quoteSingle :: String -> String
quoteSingle s = concat ["'", escapeSingleQuotes s, "'"]

quoteDouble :: String -> String
quoteDouble s = concat ["\"", escapeDoubleQuotes s, "\""]
