#!/bin/sh

mvn -Dpackage.osx.skip=true $@
cd ols.distribution/target
unzip ols*-full.zip plugins/ -d ../../eclipse/run/
rm -rf ../../eclipse/run/felix-cache
