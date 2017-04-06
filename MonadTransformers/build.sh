#!/bin/bash
rm *.hi
rm *.o
reset
ghc --make MyState.hs 
ghc --make MyStateT.hs

