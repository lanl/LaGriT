---
title: setsize
tags: ok
---

 **SETSIZE**

  **setsize** will set the mesh object attributes
  xmin,xmax,ymin,ymax,zmin,zmax from the xic,yic,zic values of all
  'real' points (dudded and merged points will be ignored).  It also
  sets epsilon, epsilona and epsilonv as follows:  setsize is called
  internally by LaGriT commands that add nodes to the
  mesh;**[copypts](COPYPTS.md)**,**[createpts](createpts.md)**,
  **[regpnts](REGNPTS.md)**,
  **[recon](RECON.md)**,**[scale](SCALE.md)**,
  **[translate](TRANS.md)**

  epsilonv=abs(xmax-xmin)
*abs(ymax-ymin)
*abs(zmax-zmin)
*epsilonr
*1000.

  epsilona=((xmax-xmin)
*
*2+(ymax-ymin)
*
*2+(zmax-zmin)
*
*2)
*epsilonr
*1000.

  epsilonr is set at initialization time by:
 
     x2=one

     do i=1,1000
      x2=x2/two
      x1=one+x2
      if(x1.le.one) go to 11
     enddo

     11 epsilonr=x2*2.

where the values of 'one' and 'two' are obtained from the include file
'consts.h'.

The command [cmo/printatt](cmo/cmo_printatt.md) can be used to view
any of these cmo attributes:

e.g. mo/printatt/xmax

The programmer interface is the subroutine setsize (see Section e.7)

The variables epsilonl, epsilona and epsilonv are mesh object
attributes; hence they may be different for all meshes in a given run. 
epsilona, epsilonv and epsilon1 may be set by the user with the
**[cmo/setatt](cmo/cmo_setatt.md)** command.  They also will be
written to LaGriT dumps and subsequently will be read in at restart. The
variables in consts.h (epsilon and epsilonr) are machine and run
dependent. They are not written to LaGriT dumps and there is only one
copy per run.

**FORMAT:**

**setsize**

if epsilonv is very small epislonvis set to epsilona

epsilon1 is set by a call to set\_epsilon

epsilon1 is set to the square root of epsilona unless this number would
be too small in which case epsilon1 is to (( xmax-xmin) + (ymax-ymin) + (zmax-zmin)) * 1.e=8/3

Many LaGriT algorithms use epsilon1; for example, if a node falls on a
interface or boundary surface. It uses epsilonv to determine if a node
can be connected. Errors from **setpts** and **connect** may result if
inconsistant or wrong values of epsilons are used.
