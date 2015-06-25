module Main where

import Log
import LogAnalysis (parse, whatWentWrong)

main :: IO ()
main = do
  testHW01
  testHW02

testHW01 :: IO ()
testHW01 = putStrLn "not implemented"

testHW02 :: IO ()
testHW02 = do
  msgs <- testParse parse 10 "./HW02/sample.log"
  print msgs

  errors <- testWhatWentWrong parse whatWentWrong "./HW02/error.log"
  putStrLn $ unlines $ "Error Log\n----------" : errors
