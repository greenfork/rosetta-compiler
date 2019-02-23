import unittest
import re, os, sequtils

import lexical_analyzer

proc rmWhitespace(str: string): string =
  str.replace(re" +", " ")

proc lex(str: string): string =
  str.tokenize.output

test "lexical analysis with inputs":
  let
    numberOfFiles = 15
    inputFiles: seq[string] = toSeq(0..<numberOfFiles).mapIt("input" & $it & ".txt")
    outputFiles: seq[string] = toSeq(0..<numberOfFiles).mapIt("lexed" & $it & ".txt")
  var
    inputText, outputText: string

  for i in 0..<numberOfFiles:
    inputText = readFile("examples" / inputFiles[i])
    outputText = readFile("examples" / outputFiles[i])

    echo "Checking input file " & $i & "..."
    check inputText.lex.rmWhitespace == outputText.rmWhitespace
