@echo off

rem Check whether the "magic" Java binary is available...
java -version > NUL 2> NUL
if errorlevel 1 goto noJVM

rem For now, use the "console enabled" java for Windows...
java -Dnl.lxtreme.ols.bundle.dir=plugins/ -cp "bin/*" nl.lxtreme.ols.runner.Runner
goto end

:noJVM
echo It appears that Java is not installed on this computer. You
echo should download and install the latest JDK.
prompt
exit 1

:end
exit 0
