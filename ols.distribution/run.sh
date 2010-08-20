#!/bin/sh

# Ensure current working dir equals the dir of this script (thanks Wayoda)
cd "$(dirname -- "${0}")"
java -Dnl.lxtreme.ols.bundle.dir=plugins/ -cp "bin/*" nl.lxtreme.ols.runner.Runner
