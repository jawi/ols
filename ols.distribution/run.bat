@echo off

rem Check whether the "magic" Java binary is available...
java -version > NUL 2> NUL
if errorlevel 1 goto noJVM

rem determine the location this script is run in...
set BASEDIR=%~dp0
rem all paths are used relatively from the base dir...
set PLUGINDIR=%BASEDIR%\plugins
set CLASSPATH=%BASEDIR%\bin\*
set MEMSETTINGS=-Xmx1024m

rem For now, use the "console enabled" java for Windows...
java %MEMSETTINGS% -Dnl.lxtreme.ols.bundle.dir="%PLUGINDIR%" -DPlastic.defaultTheme=SkyBluer -cp "%CLASSPATH%" nl.lxtreme.ols.runner.Runner
goto end

:noJVM
echo It appears that Java is not installed on this computer. You
echo should download and install the latest JDK.
prompt
exit 1

:end
exit 0
