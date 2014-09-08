#!/bin/sh

# Simple check to see whether the "magic" Java binary is available on our path;
java -version 1>/dev/null 2>&1
if [ "$?" -ne "0" ]; then
	echo It appears that Java is not installed on this computer. You
	echo should download and install the latest JDK.
	exit 1
fi

# cross-platform "readlink -f" function; taken and modified (clean ups and made 
# recursive) from <http://stackoverflow.com/questions/1055671>. 
canonical_readlink() {
	local targetFile=$1

	cd $(dirname "$targetFile")
	targetFile=$(basename "$targetFile")

	if [ -L "$targetFile" ]; then
    	canonical_readlink $(readlink "$targetFile")
	else
		echo "`pwd -P`/$targetFile"
	fi
}

# determine the location this script is run in
scriptname=$(canonical_readlink "$0")
basedir=$(dirname "$scriptname")

# all paths are used relatively from the base dir...
plugindir="$basedir/plugins/"
classpath="$basedir/bin/*"

# give the client roughly 1gigabyte of memory 
MEMSETTINGS=-Xmx1024m
java "$@" "$MEMSETTINGS" -Djna.nosys=true -Dnl.lxtreme.ols.bundle.dir="$plugindir" -DPlastic.defaultTheme=SkyBluer -cp "$classpath" nl.lxtreme.ols.runner.Runner
