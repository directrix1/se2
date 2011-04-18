#!/usr/bin/env python
import sys

totaltime = 0
start = ""
end = ""
try:
    for a in sys.stdin.readlines():
        if "fix time:" in a:
            totaltime += int(a.split(":")[1].strip())
        elif "start time:" in a:
            start = a.split(":",1)[1].strip().upper()
        elif "end time:" in a:
            end = a.split(":",1)[1].strip().upper()
            sam = "AM" in start
            eam = "AM" in end
            start = start.replace("AM","").replace("PM","").strip().split(":")
            end = end.replace("AM","").replace("PM","").strip().split(":")
            start = int(start[0]) * 60 + int(start[1])
            end = int(end[0]) * 60 + int(end[1]) + \
                    (720 if sam!=eam else 0)
            totaltime+=end-start
finally:
    print totaltime
