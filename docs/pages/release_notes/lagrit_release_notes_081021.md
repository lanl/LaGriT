---
title: 'LaGriT V2.004 Release Notes, 10/21/2008'
---

<div id="banner">

<div id="header">
<img height="300" width="300" src="lagrit_release_notes_081021_files/lagrit-logo.jpg">](https://ancho.lanl.gov/lagrit/){#logo

------------------------------------------------------------------------

<div id="metanav" class="nav">


<div id="main">

<div id="content" class="timeline">

10/21/08:
---------

### **[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/77%3Aa230c6aa1ef4)Version 2.004 Release**

09/12/08:
---------

Timestamp:
:   09/12/08 15:42:08 (1 month ago)

Message:

:   resetpts bug fix, boundary\_components and extract\_surfmesh added
    features

    resetpts had a bug for some element types

    boundary\_components: added id\_numb boundary index number

    extract\_surfmesh: added attributes idelem0, idelem1, idnode0 and
    removed attribute map

 Files: 

:   -   [lagrit.lanl.gov/docs/BOUNDARY\_C.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/BOUNDARY_C.md?rev=72%3A427e4b85dbdb "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/72%3A427e4b85dbdb#file0 "Show differences"))
    -   [lagrit.lanl.gov/docs/EXTRACT\_SURFMESH.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/EXTRACT_SURFMESH.md?rev=72%3A427e4b85dbdb "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/72%3A427e4b85dbdb#file1 "Show differences"))
    -   [src/boundary\_components.f](https://ancho.lanl.gov/lagrit/trac/browser/src/boundary_components.f?rev=72%3A427e4b85dbdb "Show entry in browser")
        [(modified)]{.comment
        ([5 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/72%3A427e4b85dbdb#file2 "Show differences"))
    -   [src/extract\_surfmesh.f](https://ancho.lanl.gov/lagrit/trac/browser/src/extract_surfmesh.f?rev=72%3A427e4b85dbdb "Show entry in browser")
        [(modified)]{.comment
        ([11 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/72%3A427e4b85dbdb#file3 "Show differences"))
    -   [src/resetpts.f](https://ancho.lanl.gov/lagrit/trac/browser/src/resetpts.f?rev=72%3A427e4b85dbdb "Show entry in browser")
        [(modified)]{.comment
        ([2 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/72%3A427e4b85dbdb#file4 "Show differences"))

<div id="legend">


07/08/08:
---------

Timestamp:
07/08/08 07:34:40 (4 months ago)
Message:
Made changes so that filter and rmmat return with no action rather than
crash when passed an empty mesh object.

Files:
-   [src/filter.f](https://ancho.lanl.gov/lagrit/trac/browser/src/filter.f?rev=71%3A5c8406e8de14 "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/71%3A5c8406e8de14#file0 "Show differences"))
-   [src/rmmat.f](https://ancho.lanl.gov/lagrit/trac/browser/src/rmmat.f?rev=71%3A5c8406e8de14 "Show entry in browser")
    [(modified)]{.comment
    ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/71%3A5c8406e8de14#file1 "Show differences"))
-   


Timestamp:
07/08/08 07:32:28 (4 months ago)
Message:
Minor changes to documentation pages.

Files:
-   [lagrit.lanl.gov/docs/CONNECT1.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/CONNECT1.md?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file0 "Show differences"))
-   [lagrit.lanl.gov/docs/OFFSETSURF.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/OFFSETSURF.md?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file1 "Show differences"))
-   [lagrit.lanl.gov/docs/QUALITY\_sliver\_cap\_needle\_wedge.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/QUALITY_sliver_cap_needle_wedge.md?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file2 "Show differences"))
-   [lagrit.lanl.gov/docs/REFINE.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/REFINE.md?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file3 "Show differences"))
-   [src/dumptecplot\_hybrid.f](https://ancho.lanl.gov/lagrit/trac/browser/src/dumptecplot_hybrid.f?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file4 "Show differences"))
-   [src/offsetsurf.f](https://ancho.lanl.gov/lagrit/trac/browser/src/offsetsurf.f?rev=70%3Acc165a78f4ee "Show entry in browser")
    [(modified)]{.comment
    ([9 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/70%3Acc165a78f4ee#file5 "Show differences"))

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

Files:
-   [src/dumpavs.f](https://ancho.lanl.gov/lagrit/trac/browser/src/dumpavs.f?rev=69%3A9cd5c898d5e4 "Show entry in browser")
    [(modified)]{.comment
    ([8 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/69%3A9cd5c898d5e4#file0 "Show differences"))
-   [src/writedump.f](https://ancho.lanl.gov/lagrit/trac/browser/src/writedump.f?rev=69%3A9cd5c898d5e4 "Show entry in browser")
    [(modified)]{.comment
    ([5 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/69%3A9cd5c898d5e4#file1 "Show differences"))

06/11/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/68%3Afb1f221839b1)

Timestamp:
06/11/08 16:08:01 (4 months ago)
Message:
Corrected bug: cmo\_exist returned error flag but check of error flag
was to wrong error flag variable.

Files:
-   [src/interp\_lg.f](https://ancho.lanl.gov/lagrit/trac/browser/src/interp_lg.f?rev=68%3Afb1f221839b1 "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/68%3Afb1f221839b1#file0 "Show differences"))


Timestamp:
:   06/11/08 16:00:12 (4 months ago)
:   

Message:

:   Added option of using 
# in addition to 
* for comment lines.

 Files: 

:   -   [src/control\_command\_lg.f](https://ancho.lanl.gov/lagrit/trac/browser/src/control_command_lg.f?rev=66%3A479e73cf7d3e "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/66%3A479e73cf7d3e#file0 "Show differences"))

-

05/20/08:
---------

LaGriT V2.003 Release


Timestamp:
:   05/20/08 14:42:40 (5 months ago)
:   

Message:

:   Compile and test V2.003 for platforms SGI-32, SUN, MAC, LINUX

    These include fixes to SGI compile errors in dumpavs.f filter.f

    refine\_tet\_add.f lagrit
*.h writinit.f and the Makefile in src

 Files: 

:   -   [bin/buildbin](https://ancho.lanl.gov/lagrit/trac/browser/bin/buildbin?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/buildbin "Show differences"))
    -   [bin/lagrit\_lin\_g](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_lin_g?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_lin_g "Show differences"))
    -   [bin/lagrit\_lin\_o](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_lin_o?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_lin_o "Show differences"))
    -   [bin/lagrit\_mac\_g](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_mac_g?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_mac_g "Show differences"))
    -   [bin/lagrit\_mac\_o](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_mac_o?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_mac_o "Show differences"))
    -   [bin/lagrit\_sun\_g](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_sun_g?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_sun_g "Show differences"))
    -   [bin/lagrit\_sun\_o](https://ancho.lanl.gov/lagrit/trac/browser/bin/lagrit_sun_o?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/bin/lagrit_sun_o "Show differences"))
    -   [lib/lagrit\_lin\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_g.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_lin_g.a "Show differences"))
    -   [lib/lagrit\_lin\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_o.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_lin_o.a "Show differences"))
    -   [lib/lagrit\_mac\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_g.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_mac_g.a "Show differences"))
    -   [lib/lagrit\_mac\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_o.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_mac_o.a "Show differences"))
    -   [lib/lagrit\_sun\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_g.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_sun_g.a "Show differences"))
    -   [lib/lagrit\_sun\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_o.a?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/lib/lagrit_sun_o.a "Show differences"))
    -   [test/level01/check\_test.py](https://ancho.lanl.gov/lagrit/trac/browser/test/level01/check_test.py?rev=65%3A684a123422c7 "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/65%3A684a123422c7/test/level01/check_test.py "Show differences"))

:   

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

Files:
-   [src/offsetsurf.f](https://ancho.lanl.gov/lagrit/trac/browser/src/offsetsurf.f?rev=48%3A1a8051c0661a "Show entry in browser")
    [(modified)]{.comment
    ([5 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/48%3A1a8051c0661a#file0 "Show differences"))
-   [src/sphere1.f](https://ancho.lanl.gov/lagrit/trac/browser/src/sphere1.f?rev=48%3A1a8051c0661a "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/48%3A1a8051c0661a#file1 "Show differences"))

04/30/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/47%3Aded381465e6b)

Timestamp:
:   04/30/08 12:33:36 (6 months ago)
:   

Message:

:   Fixed error that occured when all output attributes were turned
    off.

    Fixed error that occured when all output attributes were turned
    off.

    Code tried to allocate a zero length array for xvalues( ).

 Files: 

:   -   [src/dumptecplot\_hybrid.f](https://ancho.lanl.gov/lagrit/trac/browser/src/dumptecplot_hybrid.f?rev=47%3Aded381465e6b "Show entry in browser")
        [(modified)]{.comment
        ([10 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/47%3Aded381465e6b#file0 "Show differences")

<div id="legend">

### 


04/08/08:
---------

Timestamp:
:   04/08/08 12:50:28 (7 months ago)


:   

Message:

:   Updated and tested V2.002 apr 2008

 Files: 

:   -   [lib/lagrit\_lin\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_lin_g.a "Show differences"))
    -   [lib/lagrit\_lin\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_lin_o.a "Show differences"))
    -   [lib/lagrit\_mac\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_mac_g.a "Show differences"))
    -   [lib/lagrit\_mac\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_mac_o.a "Show differences"))
    -   [lib/lagrit\_sun\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_sun_g.a "Show differences"))
    -   [lib/lagrit\_sun\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_sun_o.a "Show differences"))




Timestamp:
:   04/08/08 12:50:28 (7 months ago)
:   

Message:

:   Updated and tested V2.002 apr 2008

 Files: 

:   -   [lib/lagrit\_lin\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_lin_g.a "Show differences"))
    -   [lib/lagrit\_lin\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_lin_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_lin_o.a "Show differences"))
    -   [lib/lagrit\_mac\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_mac_g.a "Show differences"))
    -   [lib/lagrit\_mac\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_mac_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_mac_o.a "Show differences"))
    -   [lib/lagrit\_sun\_g.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_g.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_sun_g.a "Show differences"))
    -   [lib/lagrit\_sun\_o.a](https://ancho.lanl.gov/lagrit/trac/browser/lib/lagrit_sun_o.a?rev=31%3A04bcb5deb3ea "Show entry in browser")
        [(modified)]{.comment ([view
        diffs](https://ancho.lanl.gov/lagrit/trac/changeset/31%3A04bcb5deb3ea/lib/lagrit_sun_o.a "Show differences"))

Added 00README with instructions to run tests Saved old version V2.001

Timestamp:
:   04/08/08 14:28: (7 months ago)
:   

Message:

:   Updated all reference/outx3dgen to new printatt output

    improved check\_test.py to compare numbers as numerical

    values instead of text string

    The new results are saved in result\_files/
*.txt

<!-- --

Timestamp:
:   04/08/08 14:11:43 (7 months ago)
:   

Message:

:   Generalized version Makefile and dependencies

    Uses wildcards for .f .f90 and .c

    maintains object files in seperate directories

    for each platform and for debug and optimized

    use make help for list of options

 Files: 

:   -   [src/Makefile](https://ancho.lanl.gov/lagrit/trac/browser/src/Makefile?rev=40%3A293993b117 "Show entry in browser")
        [(added)]{.comment
    -   [src/Makefile.depends](https://ancho.lanl.gov/lagrit/trac/browser/src/Makefile.depends?rev=40%3A293993b117 "Show entry in browser")

<!-- --

Timestamp:
:   04/08/08 14:08:58 (7 months ago)
:   

Message:

:   added options opt and debug as build choices

 Files: 

:   -   [src/buildbin](https://ancho.lanl.gov/lagrit/trac/browser/src/buildbin?rev=39%3A0c7fe060d0 "Show entry in browser")
        [(modified)]{.comment
        ([6 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/39%3A0c7fe060d0#file0 "Show differences"))

<!-- --

Timestamp:
:   04/08/08 13:57:28 (7 months ago)
:   

Message:

:   initialize nremtet and npoints2 to 0

    modify output for Dudded points to indicate

    when there are no elements (for removal)

 Files: 

:   -   [src/rmpoint.f](https://ancho.lanl.gov/lagrit/trac/browser/src/rmpoint.f?rev=37%3Ae47bed56f4 "Show entry in browser")
        [(modified)]{.comment
        ([4 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/37%3Ae47bed56f4#file0 "Show differences"))

<!-- --

Timestamp:
:   04/08/08 13:54:21 (7 months ago)
:   

Message:

:   initialize number\_elements and number\_nodes to zero

    to avoid junk values occuring when number should be 0

 Files: 

:   -   [src/cmo\_create\_cmo.f](https://ancho.lanl.gov/lagrit/trac/browser/src/cmo_create_cmo.f?rev=36%3A9367c60e5cb4 "Show entry in browser")
        [(modified)]{.comment
        ([4 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/36%3A9367c60e5cb4#file0 "Show differences"))

<!-- --

Timestamp:
:   04/08/08 13:38:16 (7 months ago)


:   

Message:

:   corrected line 390 which was too long

 Files: 

:   -   [src/read\_fehm\_zone.f](https://ancho.lanl.gov/lagrit/trac/browser/src/read_fehm_zone.f?rev=33%3Ac5c23d52f93c "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/33%3Ac5c23d52f93c#file0 "Show differences"))

<!-- --

Timestamp:
:   04/08/08 13:36:37 (7 months ago)
:   

Message:

:   corrected change to printatt minmax output

    so that name has 18 characters, with total line of 80 chars

 Files: 

:   -   [src/cmo\_setatt.f](https://ancho.lanl.gov/lagrit/trac/browser/src/cmo_setatt.f?rev=29%3A16b0a7f6385a "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/32%3Ad3b90dceed42#file0 "Show differences"))

-

04/03/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/29%3A16b0a7f6385a)

Timestamp:
:   04/03/08 12:09:14 (7 months ago)
:   

Message:

:   for cmo\_setatt.f added error check for existing cmo

    for cmo/printatt/ minmax limited format to 80 characters

    and expanded name string size for 17 character names

 Files: 

:   -   [src/cmo\_setatt.f](https://ancho.lanl.gov/lagrit/trac/browser/src/cmo_setatt.f?rev=29%3A16b0a7f6385a "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/29%3A16b0a7f6385a#file0 "Show differences"))


Timestamp:
:   04/03/08 09:49:17 (7 months ago)
:   

Message:

:   Add capability to read FEHM zone/zonn files.

 Files: 

:   -   [lagrit.lanl.gov/docs/READ.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/READ.md?rev=25%3A143a9305285d "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/25%3A143a9305285d#file0 "Show differences"))
    -   [lagrit.lanl.gov/docs/read\_fehm\_zone.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/read_fehm_zone.md?rev=25%3A143a9305285d "Show entry in browser")
        [(added)]{.comment
    -   [src/read\_fehm\_zone.f](https://ancho.lanl.gov/lagrit/trac/browser/src/read_fehm_zone.f?rev=25%3A143a9305285d "Show entry in browser")
        [(added)]{.comment
    -   [src/readdump.f](https://ancho.lanl.gov/lagrit/trac/browser/src/readdump.f?rev=25%3A143a9305285d "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/25%3A143a9305285d#file3 "Show differences"))

:   

01/30/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/24%3A7ea4eb8824e0)

Timestamp:
:   01/30/08 10:25:28 (9 months ago)
:   

Message:

:   Corrected typo in screen output. Changed 'nnelements' to
    'nelements'

     Corrected typo in screen output. Changed 'nnelements' to
     'nelements'

 Files: 

:   -   [src/cmo\_addatt.f](https://ancho.lanl.gov/lagrit/trac/browser/src/cmo_addatt.f?rev=24%3A7ea4eb8824e0 "Show entry in browser")
        [(modified)]{.comment
        ([7 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/24%3A7ea4eb8824e0#file0 "Show differences"))

<div id="legend">

### 



:   

01/25/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/21%3A0acfc0eddf)

Timestamp:
:   01/25/08 14:42:43 (9 months ago)


:   

Message:

:   Changed some memory allocation from real(2) to int(1) for integer
    work arrays.

    Changed some memory allocation from real(2) to int(1) for integer
    work arrays. Running a large problem (&gt;10,000,000 nodes) was
    crashing at rmpoint / pset get pset\_name ) due to MALLOC failure.

 Files: 

:   -   [src/rmpoint.f](https://ancho.lanl.gov/lagrit/trac/browser/src/rmpoint.f?rev=21%3A0acfc0eddf "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/21%3A0acfc0eddf#file0 "Show differences"))

:   

01/22/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/20%3A1d3957a02d1f)
Timestamp:
01/22/08 14::22 (9 months ago)
Message:
Changed name from 'program adrivgen' to 'program lagrit\_main'

Changed name from 'program adrivgen' to 'program lagrit\_main'

Files:
-   [lagrit.lanl.gov/docs/DEFINE.md](https://ancho.lanl.gov/lagrit/trac/browser/lagrit.lanl.gov/docs/DEFINE.md?rev=20%3A1d3957a02d1f "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/20%3A1d3957a02d1f#file0 "Show differences"))
-   [src/lagrit\_main.f](https://ancho.lanl.gov/lagrit/trac/browser/src/lagrit_main.f?rev=20%3A1d3957a02d1f "Show entry in browser")
    [(modified)]{.comment
    ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/20%3A1d3957a02d1f#file1 "Show differences"))


Timestamp:
:   01/22/08 14:37:43 (9 months ago)
:   

Message:

:   Modified format of cmo/printatt/.../minmax to i18 and e18.9 instead
    of old 6 sig. figs.

    Modified format of cmo/printatt/.../minmax to i18 and e18.9 instead
    of old 6 significant figures.

 Files: 

:   -   [src/cmo\_setatt.f](https://ancho.lanl.gov/lagrit/trac/browser/src/cmo_setatt.f?rev=19%3A0b384968dbf6 "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/19%3A0b384968dbf6#file0 "Show differences"))

-

01/17/08:
---------
[[]{.time](https://ancho.lanl.gov/lagrit/trac/changeset/18%3Aaed1a435b630)

Timestamp:
:   01/17/08 08:03:46 (9 months ago)


Message:

:   Modified header to FEHM STOR file output files.

    Modified header correcting spelling, changed X3D to LaGriT. No
    functional changes.

    Only test in lines one and two of STOR files has been changed.

 Files: 

:   -   [src/anothermatbld3d\_wrapper.f](https://ancho.lanl.gov/lagrit/trac/browser/src/anothermatbld3d_wrapper.f?rev=18%3Aaed1a435b630 "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/18%3Aaed1a435b630#file0 "Show differences"))
    -   [src/matbld2d\_stor.f](https://ancho.lanl.gov/lagrit/trac/browser/src/matbld2d_stor.f?rev=18%3Aaed1a435b630 "Show entry in browser")
        [(modified)]{.comment
        ([1 diff](https://ancho.lanl.gov/lagrit/trac/changeset/18%3Aaed1a435b630#file1 "Show differences"))
    -   [src/matbld3d\_stor.f](https://ancho.lanl.gov/lagrit/trac/browser/src/matbld3d_stor.f?rev=18%3Aaed1a435b630 "Show entry in browser")
        [(modified)]{.comment
        ([4 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/18%3Aaed1a435b630#file2 "Show differences"))
    -   [src/voronoi\_stor.f](https://ancho.lanl.gov/lagrit/trac/browser/src/voronoi_stor.f?rev=18%3Aaed1a435b630 "Show entry in browser")
        [(modified)]{.comment
        ([3 diffs](https://ancho.lanl.gov/lagrit/trac/changeset/18%3Aaed1a435b630#file3 "Show differences"))

:   


<div id="altlinks">


<div id="footer">

------------------------------------------------------------------------
[![Trac
Powered](lagrit_release_notes_081021_files/trac_logo_mini.png)"107"
"30"](http://trac.edgewall.org/){#tracpowered
Powered by [**Trac 0.10.4**](https://ancho.lanl.gov/lagrit/trac/about)

By [Edgewall Software](http://www.edgewall.org/).

Visit the Trac open source project at

<http://trac.edgewall.org/


