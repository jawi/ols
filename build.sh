#!/bin/sh

mvn -Dpackage.osx.skip=true $@
cd ols.distribution/target
unzip ols-v*-full.zip plugins/
