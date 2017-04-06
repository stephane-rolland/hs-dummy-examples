#!/bin/bash
rm Interact.hi
rm Interact
rm Interact.o
ghc --make Interact.hs
./Interact
