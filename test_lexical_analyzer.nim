import unittest
import typeinfo, streams, re, os, sequtils

import lexical_analyzer

proc rmWhitespace(str: string): string =
  str.replace(re" +", " ")

proc lex(str: string): string =
  str.tokenize.output

proc dumpToFile(name, str: string) =
  var f = newFileStream(name, fmWrite)
  f.write str
  f.close()

test "lexical analysis with inputs":
  let
    numberOfFiles = 15
    inputFiles: seq[string] = toSeq(0..<numberOfFiles).mapIt("input" & $it & ".txt")
    outputFiles: seq[string] = toSeq(0..<numberOfFiles).mapIt("lexed" & $it & ".txt")
  var
    inputStream, outputStream: Stream
    inputText, outputText: string

  for i in 0..<numberOfFiles:
    inputStream = openFileStream("examples" / inputFiles[i])
    outputStream = openFileStream("examples" / outputFiles[i])
    inputText = inputStream.readAll
    outputText = outputStream.readAll
    inputStream.close()
    outputStream.close()

    debugEcho "Checking input file " & $i & "..."
    check inputText.lex.rmWhitespace == outputText.rmWhitespace
