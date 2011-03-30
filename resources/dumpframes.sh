#!/bin/bash
ffmpeg -i rickroll.flv -r 10 -f image2 -s 160x130 rickrollframes/rickroll%05d.png
