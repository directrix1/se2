#!/bin/bash
rm -Rf rickrollframes
mkdir rickrollframes
ffmpeg -i rickroll.flv -r 25 -f image2 -s 160x130 rickrollframes/rickroll%05d.png
cd rickrollframes
export NUMFRAMES=`ls -1 | grep -c png`
echo '<pnga frames="'$NUMFRAMES'" plays="1">' >> test.xml
for i in *.png; do 
	echo '<image src="rickrollframes/'$i'" length="1/25"/>' >> test.xml
done
echo '</pnga>' >> test.xml

