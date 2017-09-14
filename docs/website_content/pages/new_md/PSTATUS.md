---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: PSTATUS
---

** ** 

 **PSTATUS**

  Saves, removes, retrieves, or replaces a specified set of points,
  usually the last set of points defined by a generator command or the
  set of points defined by ifirst,ilast,istride. Note that point sets
  must be specified in sequence in order for this command to work
  properly.

**FORMAT:**

**pstatus** (Returns current point status counters)

**pstatus** **/save**/name/ifirst,ilast,

(Saves the point status numbers, ifirst,ilast,istride under name)

**pstatus** **/store**/name/ifirst,ilast

(Overwrites what was in name with ifirst,ilast,istride

**pstatus /delete**/name (Deletes values from name)

**pstatus** **/get**/name (Retrieves values from name)

 

