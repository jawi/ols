#!/bin/sh

INPUT_DMG=$1
VOLUME_TITLE=$2

device=$(hdiutil attach -readwrite -noverify -owners off -noautoopen "${INPUT_DMG}" | egrep '^/dev/' | sed 1q | awk '{print $1}')

echo '
   tell application "Finder"
     tell disk "'${VOLUME_TITLE}'"
           open
           set current view of container window to icon view
           set toolbar visible of container window to false
           set statusbar visible of container window to false
           set the bounds of container window to {400, 100, 885, 430}
           set theViewOptions to the icon view options of container window
           set arrangement of theViewOptions to not arranged
           set icon size of theViewOptions to 96
           set background picture of theViewOptions to file ".background:'background.png'"
           make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
           set position of item "'${VOLUME_TITLE}'" of container window to {90, 165}
           set position of item "Applications" of container window to {395, 165}
           close
           open
           update without registering applications
     end tell
   end tell
' | osascript

SetFile -a -C /Volumes/${VOLUME_TITLE}

chmod -Rf go-w /Volumes/${VOLUME_TITLE}/*
sync
sync
hdiutil detach ${device}

###EOF###
