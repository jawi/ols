@echo off

call mvn install:install-file -DgroupId=nl.lxtreme.ols -DartifactId=api -Dversion=1.0.4 -Dpackaging=jar -Dfile=lib/api-1.0.4.jar -Djavadoc=javadoc/api-1.0.4-javadoc.jar
call mvn install:install-file -DgroupId=nl.lxtreme.ols -DartifactId=util -Dversion=1.0.5 -Dpackaging=jar -Dfile=lib/util-1.0.5.jar -Djavadoc=javadoc/util-1.0.5-javadoc.jar
call mvn install:install-file -DgroupId=nl.lxtreme.ols -DartifactId=base -Dversion=1.0.0 -Dpackaging=jar -Dfile=lib/base-1.0.0.jar -Djavadoc=javadoc/base-1.0.0-javadoc.jar
call mvn install:install-file -DgroupId=nl.lxtreme.ols -DartifactId=test.util -Dversion=1.0.0 -Dpackaging=jar -Dfile=lib/test.util-1.0.0.jar -Djavadoc=javadoc/test.util-1.0.0-javadoc.jar
