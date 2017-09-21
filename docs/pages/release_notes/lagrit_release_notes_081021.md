---
title: 'LaGriT V2.004 Release Notes, 10/21/2008'
---


-------------------------------------
## Version 2.004 Release 09/12/08


-  resetpts bug fix, boundary\_components and extract\_surfmesh added features
-  resetpts had a bug for some element types
-  boundary\_components: added id\_numb boundary index number
-  extract\_surfmesh: added attributes idelem0, idelem1, idnode0 and removed attribute map

 
07/08/08:
---------

Timestamp:
07/08/08 07:34:40 (4 months ago)
Message:
Made changes so that filter and rmmat return with no action rather than
crash when passed an empty mesh object.


Timestamp:
07/08/08 07:32:28 (4 months ago)
Message:
Minor changes to documentation pages.


06/12/08:
---------

Timestamp:
06/12/08 13:35:01 (4 months ago)
Message:
Add options dump/att\_node and dump/att\_elem

Add options dump/att\_node and dump/att\_elem to output tables of either
node

attributes or element attributes to ascii files with header lines
beginning

with 
#. These are similar to dump/avs/file/mo/0 0 2 0 or
dump/avs/file/mo/0 0 0 2

except the addition of 
# signs to start lines. Now these files can be
read in

without editing using:

cmo / readatt / mo\_name / etc...


06/11/08:
---------

Timestamp:
06/11/08 16:08:01 (4 months ago)
Message:
Corrected bug: cmo\_exist returned error flag but check of error flag
was to wrong error flag variable.


Timestamp:
   06/11/08 16:00:12 (4 months ago)
   

Message:

   Added option of using 
# in addition to 
* for comment lines.

 

05/20/08:
---------

LaGriT V2.003 Release


Timestamp:
   05/20/08 14:42:40 (5 months ago)
   

Message:

   Compile and test V2.003 for platforms SGI-32, SUN, MAC, LINUX

    These include fixes to SGI compile errors in dumpavs.f filter.f

    refine\_tet\_add.f lagrit
*.h writinit.f and the Makefile in src

 
05/01/08:
---------

Timestamp:
05/01/08 12:57:58 (6 months ago)
Message:
Correct minor bugs. Test case now works.

sphere1.f had an incorrect attempt to use MO name before the name

had been obtained. Code would crash.

offsetsurf.f did not handle problems with non-triangle or line type MO.

Instead of kicking out, an attempt was made to compute sythetic normal

for a mesh object (such as quad) and this caused code to crash.


04/30/08:
---------

 

Message:

   Fixed error that occured when all output attributes were turned
    off.

    Fixed error that occured when all output attributes were turned
    off.

    Code tried to allocate a zero length array for xvalues( ).




04/08/08:
---------



   Updated and tested V2.002 apr 2008



   Updated and tested V2.002 apr 2008


Added 00README with instructions to run tests Saved old version V2.001

Timestamp:
  04/08/08 14:28: (7 months ago)
   

Message:

   Updated all reference/outx3dgen to new printatt output

    improved check\_test.py to compare numbers as numerical

    values instead of text string

    The new results are saved in result\_files/
*.txt



Timestamp:
   04/08/08 14:11:43 (7 months ago)
   

Message:

   Generalized version Makefile and dependencies

    Uses wildcards for .f .f90 and .c

    maintains object files in seperate directories

    for each platform and for debug and optimized

    use make help for list of options



Timestamp:
   04/08/08 14:08:58 (7 months ago)
   

Message:

   added options opt and debug as build choices

 

Timestamp:
   04/08/08 13:57:28 (7 months ago)
   

Message:

   initialize nremtet and npoints2 to 0

    modify output for Dudded points to indicate

    when there are no elements (for removal)



Timestamp:
   04/08/08 13:54:21 (7 months ago)
   

Message:

   initialize number\_elements and number\_nodes to zero

    to avoid junk values occuring when number should be 0



Timestamp:
   04/08/08 13:38:16 (7 months ago)


   

Message:

   corrected line 390 which was too long



Timestamp:
   04/08/08 13:36:37 (7 months ago)
   

Message:

   corrected change to printatt minmax output

    so that name has 18 characters, with total line of 80 chars



04/03/08:
---------

Timestamp:
   04/03/08 12:09:14 (7 months ago)
   

Message:

   for cmo\_setatt.f added error check for existing cmo

    for cmo/printatt/ minmax limited format to 80 characters

    and expanded name string size for 17 character names

 

Timestamp:
   04/03/08 09:49:17 (7 months ago)
   

Message:

   Add capability to read FEHM zone/zonn files.



01/30/08:
---------

Timestamp:
   01/30/08 10:25:28 (9 months ago)
   

Message:

   Corrected typo in screen output. Changed 'nnelements' to
    'nelements'

     Corrected typo in screen output. Changed 'nnelements' to
     'nelements'




01/25/08:
---------

Timestamp:
   01/25/08 14:42:43 (9 months ago)


   

Message:

   Changed some memory allocation from real(2) to int(1) for integer
    work arrays.

    Changed some memory allocation from real(2) to int(1) for integer
    work arrays. Running a large problem (&gt;10,000,000 nodes) was
    crashing at rmpoint / pset get pset\_name ) due to MALLOC failure.



01/22/08:
---------
Timestamp:
01/22/08 14::22 (9 months ago)
Message:
Changed name from 'program adrivgen' to 'program lagrit\_main'

Changed name from 'program adrivgen' to 'program lagrit\_main'



Timestamp:
   01/22/08 14:37:43 (9 months ago)
   

Message:

   Modified format of cmo/printatt/.../minmax to i18 and e18.9 instead
    of old 6 sig. figs.

    Modified format of cmo/printatt/.../minmax to i18 and e18.9 instead
    of old 6 significant figures.



01/17/08:
---------

Timestamp:
  01/17/08 08:03:46 (9 months ago)


Message:

   Modified header to FEHM STOR file output files.

    Modified header correcting spelling, changed X3D to LaGriT. No
    functional changes.

    Only test in lines one and two of STOR files has been changed.


