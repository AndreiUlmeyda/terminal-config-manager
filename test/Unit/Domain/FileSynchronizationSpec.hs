module Unit.Domain.FileSynchronizationSpec where

import Data.Maybe ()
import Data.Text
  ( append,
    cons,
  )
import Domain.FileSynchronization
  ( extractValue,
    removeAfterAndIncluding,
    removeBeforeAndIncluding,
  )
import Infrastructure.Config
  ( Pattern (..),
    TargetValue (MkTargetValue),
  )
import Infrastructure.FileModification
  ( Content (MkContent),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "Finding the current value inside of some file content" $ do
    it "given completely empty input, should result in Nothing" $
      extractValue (MkPattern "") "" (MkContent "") `shouldBe` Nothing
    it "given a pattern and nothing else, should result in Nothing" $
      extractValue (MkPattern "some {{value}} pattern") "" (MkContent "") `shouldBe` Nothing
    it "given file content and nothing else, should result in Nothing" $
      extractValue (MkPattern "") "" (MkContent "some target pattern") `shouldBe` Nothing
    it "given a value marker and nothing else, should result in Nothing" $
      extractValue (MkPattern "") "{{value}}" (MkContent "") `shouldBe` Nothing
    it "given a pattern without text in front or after the value marker, should result in the entire content" $
      let valueMarker = "{{value}}"
          content = "some content"
       in extractValue (MkPattern valueMarker) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue content)
    it
      "given a pattern which matches at the start of the content, should result\
      \ in the rest of the content without the pattern before the value marker"
      $ let valueMarker = "{{value}}"
            content = cons 'a' "content"
            matchingPattern = cons 'a' valueMarker
         in extractValue (MkPattern matchingPattern) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue "content")
    it
      "given a pattern which matches a few characters into the content,should result\
      \ in the rest of the content without the pattern before the value marker"
      $ let valueMarker = "{{value}}"
            content = "irrelevant" `append` "o" `append` "content"
            matchingPattern = "o" `append` valueMarker
         in extractValue (MkPattern matchingPattern) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue "content")
    it
      "given a pattern which matches a few characters into the content,should result\
      \ in the rest of the content without the pattern before or after the value marker"
      $ let valueMarker = "{{value}}"
            content = "irrelevant" `append` "o" `append` "content" `append` "irrelevant"
            matchingPattern = "o" `append` valueMarker `append` "irrelevant"
         in extractValue (MkPattern matchingPattern) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue "content")
    it
      "given a pattern with nothing after the value marker, should result\
      \ in a match only up to and not including the next newline"
      $ let valueMarker = "{{value}}"
            content = "content" `append` "\n after newline"
            matchingPattern = valueMarker
         in extractValue (MkPattern matchingPattern) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue "content")
    it
      "given a pattern with nothing before the value marker, should result\
      \ in a backward match only up to and not including the nearest newline"
      $ let valueMarker = "{{value}}"
            content = "before newline\n" `append` "content" `append` "irrelevant"
            matchingPattern = valueMarker `append` "irrelevant"
         in extractValue (MkPattern matchingPattern) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue "content")

  describe "Remove a substring from a given string and everything before it" $ do
    it "given completely empty input, should result in the empty string" $
      removeBeforeAndIncluding "" "" `shouldBe` ""
    it "given a prefix, should remove it" $
      removeBeforeAndIncluding "a" "abc" `shouldBe` "bc"
    it "given a substring which is not a prefix, should remove it and everything before it" $
      removeBeforeAndIncluding "b" "abc" `shouldBe` "c"
  describe "Remove a substring from a given string and everything after it" $ do
    it "given completely empty input, should result in the empty string" $
      removeAfterAndIncluding "" "" `shouldBe` ""
    it "given a prefix, should remove everything" $
      removeAfterAndIncluding "a" "abc" `shouldBe` ""
    it "given a substring somewhere in the middle, should remove it and everything after it" $
      removeAfterAndIncluding "b" "abc" `shouldBe` "a"
