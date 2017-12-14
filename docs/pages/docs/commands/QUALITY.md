---
title: QUALITY
tags: bad missing half the page
---

**QUALITY**

**quality** provides a collection of mesh quality measures  

**FORMAT:**

**quality** /[quality_type] / [quality_type options] 

Where quality-type can be aspect, pcc, volume, angle, quad, or taylor. Quality-type options depend on the quality-type. 

**quality** (no arguments) 

writes to screen and outx3dgen logfile giving volume and aspect ratio distribution information. Aspect ratios and element volumes are binned into 5 bins then totaled, min and max values are also reported. 

**quality**/aspect/[y] 

displays a count of the number of elements whose aspect ratio falls in each of 7 bins .  If y is specified create an attribute named aratio that will contain the value of the aspect ratio of each element. 

**quality**/edge_ratio/[y]

displays a count of the edge length minimum/edge length maximum in each of 7 bins. If y is specified create an attribute named eratio that will contain the value of the min/max edge ratio of each element.

**quality**/edge_min/[y]

displays a count of the minimum edge length in each of 7 bins. If y is specified create an attribute named edgemin that will contain the value of the min edge length of each element.

**quality**/edge_max/[y]

displays a count of the maximum edge length in each of 7 bins. If y is specified create an attribute named edgemax that will contain the value of the max edge length of each element.

**quality**/angle/gt OR lt/value]/ 

displays a count of the number of elements with a dihedral angle that is greater than or less than the supplied value. See also cmo/addatt/mo/ang_* commands for dihedral angle and solid angle calculations.

**quality**/pcc 

 
