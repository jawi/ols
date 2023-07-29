@echo off

:findJavaSimple
rem Simple check to see whether the "magic" Java binary is available on our path;
java -version > NUL 2> NUL
if errorlevel 1 goto findJavaAdvanced
goto setup

:findJavaAdvanced
rem See <https://github.com/jawi/ols/issues/140>
for /F "tokens=2*" %%A in ('REG QUERY "HKEY_LOCAL_MACHINE\SOFTWARE\Classes\TypeLib\{5852F5E0-8BF4-11D4-A245-0080C6F74284}\1.0\HELPDIR" /ve') do set JavaPath="%%B"
set path=%path%;%JavaPath%
java -version > NUL 2> NUL
if errorlevel 1 goto noJVM
goto setup

:noJVM
echo It appears that Java is not installed on this computer. You
echo should download and install the latest JRE.
pause
exit 1

:setup
set PLATFORMOPTS=-Dswing.defaultlaf=com.jgoodies.looks.plastic.PlasticXPLookAndFeel -DPlastic.defaultTheme=SkyBluer
rem determine the location this script is run in...
set BASEDIR=%~dp0
rem all paths are used relatively from the base dir...
set PLUGINDIR=%BASEDIR%\plugins
set CLASSPATH=.;%BASEDIR%\bin\*
rem give the client roughly 1gigabyte of memory
set MEMSETTINGS=-Xmx1024m
rem <https://github.com/jawi/ols/issues/125>
set SYSPROPS=-Djna.nosys=true
set JDK_JAVA_OPTIONS=--add-opens java.desktop/javax.swing.plaf.basic=ALL-UNNAMED

rem For now, use the "console enabled" java for Windows...
java %PLATFORMOPTS% %MEMSETTINGS% %SYSPROPS% -cp "%CLASSPATH%" nl.lxtreme.ols.runner.Runner -pluginDir="%PLUGINDIR%" %*

:end
exit 0
