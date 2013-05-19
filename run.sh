#!/bin/bash
cd "$(dirname $0)/src"
runghc Main.hs $1 $2

