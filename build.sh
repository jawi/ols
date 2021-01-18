#!/bin/sh

mvn -Dpackage.osx.skip=true $@
cd ols.distribution/target
if [ -f ols*-full.zip ]; then
	unzip ols*-full.zip plugins/ -d ../../eclipse/run/
elif [ -f ols*-full.tar.gz ]; then
	tar zxf ols*-full.tar.gz -C ../../eclipse/run/ --strip-components=1 --wildcards '*/plugins/'
fi
rm -rf ../../eclipse/run/.fwcache
