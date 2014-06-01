# OLS client

[![Build Status](https://drone.io/github.com/jawi/ols/status.png)](https://drone.io/github.com/jawi/ols/latest)

## About

This is the public GIT repository for the OpenBench LogicSniffer client, also
kwown as OLS client, written by Jan Willem Janssen (JaWi).

For more information about this software and its hardware, see
[ols.lxtreme.nl](http://ols.lxtreme.nl)
and
[dangerousprototypes.com](http://dangerousprototypes.com/open-logic-sniffer).

## Compiling

In case you're interested in cloning this repository and compile it yourself,
you should do the following:

    $ git clone http://github.com/jawi/ols.git

For compiling the sources, you need to have at least a valid JDK (1.6+), Apache
Ant (1.8+) and optionally, a recent Gradle installed (1.12+).

On Unix-flavored machines, you can kick off a build from the project's root
directory by calling the included Gradle wrapper.

    $ ./gradlew build

On Windows, you can do something similar:

    C:\Source\ols> gradlew.bat build

Once the build is finished, you can create archives of the build artifacts by:

  $ cd build && ant init package


After this, you should find the latest binary ZIP or tarball in the "generated"
directory.

## Developing

To work on this project's code, you have to use Eclipse as development
environment, along with the [Bndtools plugin](http://bndtools.org/), which can
be installed from the Eclipse Marketplace.

Drop me a line if you want to contribute code to the OLS repository, if needed,
I can give you write-access to the GitHub or apply your patch.

  [J.W. Janssen](j dot w dot janssen at lxtreme.nl)

