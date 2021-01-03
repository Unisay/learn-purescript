-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
module PrettyPrinter where

import Prelude
import Data.Array ((:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)

data Doc
  = Empty
  | Text String Doc
  | Line Int Doc

instance semigroupDoc :: Semigroup Doc where
  append = case _, _ of
    Empty, doc -> doc
    Text s x, y -> Text s (append x y)
    Line i x, y -> Line i (append x y)

nil :: Doc
nil = Empty

text :: String -> Doc
text s = Text s nil

line :: Doc
line = Line 0 nil

nest :: Int -> Doc -> Doc
nest i = case _ of
  Empty -> Empty
  Text s doc -> Text s (nest i doc)
  Line j doc -> Line (i + j) (nest i doc)

brackets :: Doc -> Doc
brackets d = text "[" <> d <> text "]"

layout :: Doc -> String
layout = case _ of
  Empty -> ""
  Text s x -> s <> layout x
  Line i x -> fromCharArray ('\n' : A.replicate i ' ') <> layout x

data Tree
  = Node String (Array Tree)

showTree :: Tree -> Doc
showTree (Node s ts) = text s <> showBracket ts

showBracket :: Array Tree -> Doc
showBracket = case _ of
  [] -> nil
  ts -> text " [" <> nest 2 (line <> showTrees ts) <> line <> text "]"

showTrees :: Array Tree -> Doc
showTrees arr = case A.uncons arr of
  Just { head: t, tail: ts }
    | A.null ts -> showTree t
  Just { head: t, tail: ts } -> showTree t <> text "," <> line <> showTrees ts
  Nothing -> nil

testTree :: Tree
testTree =
  Node "a"
    [ Node "b" [ Node "B" [], Node "B" [] ]
    , Node "c" []
    , Node "e" [ Node "f" [] ]
    ]

{-

Every document can be reduced to a normal form of text 
alternating with linebreaks nested to a given indentation:

text_s0 <> nest_i1 line <>
text_s1 <> nest_i2 line <>
text_s2 <> nest_i3 line <>
text_s3 <> nest_i4 line <>
······· <> nest_ik line <>
text_sk

For example:

-}
test2 :: Doc
test2 =
  text "bbbbb" <> text "["
    <> nest 2
        ( line <> text "ccc" <> text ","
            <> line
            <> text "dd"
        )
    <> line
    <> text "]"

{-

NF Reduction rules (laws):

  text (s <> t)      =     text s <> text t
  text ""            =     nil

  nest (i + j) x     =     nest i (nest j x)
  nest 0 x           =     x

  nest i (x <> y)    =     nest i x <> nest i y
  nest i nil         =     nil

  nest i (text s)    =     text s

-}
test2normal :: Doc
test2normal =
  text "bbbbb["
    <> (nest 2 line <> text "ccc,")
    <> (nest 2 line <> text "dd")
    <> (nest 0 line <> text "]")

{-

Laws that relate a document to its layout.

  layout (x <> y)       =  layout x <> layout y
  layout nil            =  ""
  
  layout (text s)       =  s
  layout (nest i line)  =  '\n' : copy i ' '

-}
