#!/bin/bash
#
# Script to create a disk image from an OSX App. Based on <http://stackoverflow.com/a/1513578/229140>

# Include current version & name...
. ./ols.version

src_dir=generated/osx
tmp_dmg=generated/ols-tmp.dmg
out_dmg=generated/ols-${ols_version}-full.dmg
mnt_pnt=/Volumes/${ols_shortName}

# Clean up stuff from earlier runs...
if [ -f ${tmp_dmg} ]; then
	rm -f ${tmp_dmg}
fi
if [ -f ${out_dmg} ]; then
	rm -f ${out_dmg}
fi

size=$((2 + $(du -mc "${src_dir}" | tail -1 | awk '{print $1}'))) 

echo "Creating temporary disk image of $size MB..."
hdiutil create  -srcfolder "${src_dir}" -volname "${ols_shortName}" -fs 'HFS+' -fsargs "-c c=64,a=16,e=16" -format UDRW -size "${size}M" ${tmp_dmg} -quiet

echo "Mounting temporary disk image & arranging stuff..."
device=$(hdiutil attach -readwrite -noverify -noautoopen "${tmp_dmg}" | egrep '^/dev/' | sed 1q | awk '{print $1}')

osascript <<EOF
tell application "Finder"
	repeat until exists disk "${ols_shortName}"
		delay 1
	end repeat
	tell disk "${ols_shortName}"
		open
		set current view of container window to icon view
		set toolbar visible of container window to false
		set statusbar visible of container window to false
		set the bounds of container window to {400, 100, 885, 430}
		set theViewOptions to the icon view options of container window
		set arrangement of theViewOptions to not arranged
		set icon size of theViewOptions to 96
		set background picture of theViewOptions to file ".background:background.png"
		-- make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
		do shell script "ln -s /Applications /Volumes/LogicSniffer/Applications"
		set position of item "${ols_shortName}" of container window to {90, 165}
		set position of item "Applications" of container window to {395, 165}
		close
		open
		update with necessity without registering applications
		update without registering applications
		delay 5
		close
	end tell
end tell
EOF

icon="resources/Package.icns"
tmp_icon="generated/icon-resource.r"

echo "read 'icns' (-16455) \"${icon}\";" > ${tmp_icon}
ifile="`printf \"${mnt_pnt}/Icon\r\"`"
Rez -o "${ifile}" ${tmp_icon} || exit $?
#rm -f ${tmp_icon}
SetFile -a C "${mnt_pnt}"
SetFile -a V "${ifile}"

sync && sync

if [ -d ${mnt_pnt} ]; then
	echo "Unmounting disk..."
	hdiutil detach "${device}" -quiet || hdiutil detach -force "${device}" -quiet

	if [ -d ${mnt_pnt} ]; then
		echo "Forcing unmount of disk..."
		diskutil unmount force ${mnt_pnt}
	fi
fi

# echo "Resizing temporary disk image..."
# hdiutil resize -shrinkonly -size min -nofinalgap "${tmp_dmg}"

echo "Converting temporary disk image to final disk image..."
hdiutil convert "${tmp_dmg}" -format UDZO -o "${out_dmg}" -quiet && rm -f "${tmp_dmg}"

echo "Internet-enabling final disk image..."
hdiutil internet-enable -yes ${out_dmg} -quiet

###EOF###
