*dk,intrp_element
      subroutine intrp_element(corder_value,corder_table,
     &                         ipxintrp,ipyintrp,ipzintrp,nump,
     &                         itype,
     &                         cmohex,
     &                         cfield,
     &                         ipvalue,
     &                         ierr2)
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE TAKES A SET OF POINTS IN X,Y,Z AND, FOR EACH
C         POINT, FINDS AN INTERPOLATED FIELD VALUE FOR SOME SCALAR
C         FIELD ON A HEXAHEDRAL GRID IN X,Y,Z.  THE METHOD IS TO
C         FIND WHICH ELEMENT IN LIST THE POINT LIES INSIDE AND THEN
C         LINEARLY INTERPOLATE IN THAT ELEMENT TO GET THE RESULTING
C         VALUE.  THE INTERPOLATION IS APPLIED TO EITHER THE FIELD
C         OR TO SOME FUNCTION OF THE FIELD, AS DETERMINED BY ITYPE.
C         IF A FUNCTION OF FIELD IS USED, THE RESULT IS FED INTO THE
C         INVERSE FUNCTION.
C
C         FORMAT:
C
C      INPUT ARGUMENTS -
C
C
C        cmotable - THE NAME OF THE CMO CONTAINING THE TABLE
C
C        ipxintrp - POINTER TO X-COORDINATES FOR INTERPOLATION POINTS
C
C        ipyintrp - POINTER TO Y-COORDINATES FOR INTERPOLATION POINTS
C
C        ipzintrp - POINTER TO Z-COORDINATES FOR INTERPOLATION POINTS
C
C        nump     - NUMBER OF POINTS TO BE INTERPOLATED
C
C        itype    - TYPE OF FUNCTION TO APPLY TO FIELD
C                   1 -- LINEAR
C                   2 -- LOG
C                   3 -- ASINH
C
C        cmohex   - NAME OF THE CMO CONTAINING THE TABLE
C
C        cfield   - NAME OF THE FIELD VALUE TO BE INTERPOLATED
C
C
C     OUTPUT ARGUMENTS -
C
C        ipvalue  - POINTER OF THE INTERPOLATED FIELD VALUES
C
C        ierr2    - ERROR INDICATOR, RETURNS PLACE IN LIST WHERE THERE
C                   WAS NO BOUNDING TET IN THE LIST FOR THIS POINT.
C                   RETURNS 0 IF NO TPROBLEMS, <0 IF SOME OTHER PROBLEM.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/intrp_element.f_a  $
CPVCS    
CPVCS       Rev 1.16   30 Sep 2004 09:17:14   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS
CPVCS       Rev 1.15   Thu Dec 18 10:24:52 1997   dcg
CPVCS    delclare ipxic,ipyic,ipzic as pointers
CPVCS
CPVCS       Rev 1.14   Thu May 29 11:25:18 1997   dcg
CPVCS    restore previous version
CPVCS    rev 1.13 fails in input.andrew test case
CPVCS
CPVCS       Rev 1.11   Mon Jul 29 15:43:18 1996   dcg
CPVCS    split off table_element, volume_element,tet,qud,tri
CPVCS
CPVCS       Rev 1.10   Wed Jul 24 17:32:56 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.9   Wed Jun 19 10:19:04 1996   het
CPVCS    If a ray gets lost, then restart it once by teleporting
CPVCS    close to the element that contains the target point.
CPVCS
CPVCS       Rev 1.8   Tue Apr 30 07:28:16 1996   het
CPVCS    Fix a error in the quad interpolation routine.
CPVCS
CPVCS       Rev 1.7   Tue Apr 02 02:24:06 1996   het
CPVCS    Correct an error in the volume_qud routine.
CPVCS
CPVCS       Rev 1.6   Fri Dec 22 14:13:08 1995   het
CPVCS    Correct a memory management error with ipath
CPVCS
CPVCS       Rev 1.5   12/05/95 08:25:42   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.4   11/16/95 17:02:18   het
CPVCS    Fix an error with the interpolation scheme
CPVCS
CPVCS       Rev 1.3   11/07/95 17:19:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.2   09/11/95 14:42:22   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.1   08/15/95 18:22:10   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.0   07/18/95 08:33:20   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:38   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C
      include "local_element.h"
C
      character*32 cmo, isubname
C
      pointer (ipxic_table, xic_table)
      pointer (ipyic_table, yic_table)
      pointer (ipzic_table, zic_table)
      real*8 xic_table(1000000), yic_table(1000000), zic_table(1000000)
C
      pointer (ipxic_value, xic_value)
      pointer (ipyic_value, yic_value)
      pointer (ipzic_value, zic_value)
      real*8 xic_value(1000000), yic_value(1000000), zic_value(1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipitflag, itflag)
      integer itflag(1000000)
C
      pointer (ipitpath, itpath)
      integer itpath(1000000)
C
      pointer (ipfield, field)
      real*8 field(1000000)
C
      real*8 volface(100)
C
      data iclrpath / 0 /
C
      data xsmall / 1.0d-06 /
C
C
C######################################################################
C
      integer itype, ierr2
      pointer (ipxintrp, xintrp)
      pointer (ipyintrp, yintrp)
      pointer (ipzintrp, zintrp)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(10000000),yic(10000000),zic(10000000)
      pointer (ipvalue, value)
      real*8 xintrp(nump),yintrp(nump),zintrp(nump),value(nump)
      character*(*) corder_value, corder_table, cmohex, cfield
C
      isubname='intrphex'
      cmo=cmohex
C     CHECK FOR VALID ITYPE
C
      if((itype.lt.1).or.(itype.gt.3)) then
        ierr2 = -1
        goto 9999
      endif
      ierr2 = 0
      tepsilon = 1.0d-10
C
      call get_epsilon('epsilonl', epsilonl)
C
      if(listflag.eq.0) then
        jmax = numtets
      else
        jmax = listlen
      endif
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierror)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierror)
      call cmo_get_info('faces_per_element',cmo,nefcmo,ilen,ityp,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,ilenxic,itypxic,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilenyic,itypyic,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilenzic,itypzic,ierror)
      call cmo_get_info(cfield,cmo,ipfield,ilenfield,itypfield,ierror)
C
      len1=icharlnf(corder_table)
      if(corder_table(1:2).eq.'xy') then
         ipxic_table=ipxic
         ipyic_table=ipyic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'z') then
            ipzic_table=ipzic
         endif
      elseif(corder_table(1:2).eq.'xz') then
         ipxic_table=ipxic
         ipyic_table=ipzic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'y') then
            ipzic_table=ipyic
         endif
      elseif(corder_table(1:2).eq.'yx') then
         ipxic_table=ipyic
         ipyic_table=ipxic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'z') then
            ipzic_table=ipzic
         endif
      elseif(corder_table(1:2).eq.'yz') then
         ipxic_table=ipyic
         ipyic_table=ipzic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'x') then
            ipzic_table=ipxic
         endif
      elseif(corder_table(1:2).eq.'zx') then
         ipxic_table=ipzic
         ipyic_table=ipxic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'y') then
            ipzic_table=ipyic
         endif
      elseif(corder_table(1:2).eq.'zy') then
         ipxic_table=ipzic
         ipyic_table=ipyic
         if(len1.eq.2) then
            length=nnodes
            call mmgetblk('ttemp',isubname,ipzic_table,length,2,icscode)
            do i=1,nnodes
               zic_table(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'x') then
            ipzic_table=ipxic
         endif
      endif
C
      len1=icharlnf(corder_value)
      if(corder_value(1:2).eq.'xy') then
         ipxic_value=ipxintrp
         ipyic_value=ipyintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_table(3:3).eq.'z') then
            ipzic_value=ipzintrp
         endif
      elseif(corder_value(1:2).eq.'xz') then
         ipxic_value=ipxintrp
         ipyic_value=ipzintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'y') then
            ipzic_value=ipyintrp
         endif
      elseif(corder_value(1:2).eq.'yx') then
         ipxic_value=ipyintrp
         ipyic_value=ipxintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'z') then
            ipzic_value=ipzintrp
         endif
      elseif(corder_value(1:2).eq.'yz') then
         ipxic_value=ipyintrp
         ipyic_value=ipzintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'x') then
            ipzic_value=ipxintrp
         endif
      elseif(corder_value(1:2).eq.'zx') then
         ipxic_value=ipzintrp
         ipyic_value=ipxintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'y') then
            ipzic_value=ipyintrp
         endif
      elseif(corder_value(1:2).eq.'zy') then
         ipxic_value=ipzintrp
         ipyic_value=ipyintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               zic_value(i)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'x') then
            ipzic_value=ipxintrp
         endif
      endif
C
      call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      length=nelements
      call mmgetblk('itflag',isubname,ipitflag,length,2,ierr)
      call mmgetblk('itpath',isubname,ipitpath,length,2,ierr)
C
C
      do ip=1,nump
         xa=xic_value(ip)
         ya=yic_value(ip)
         za=zic_value(ip)
C
      itnext=1
      iconvex=0
 200  continue
      do it=1,nelements
         itflag(it)=0
      enddo
      ilost=0
      irestart=0
      iflag=0
      ipath=0
      dowhile(iflag.eq.0.and.ipath.lt.nelements)
         ipath=ipath+1
         itpath(ipath)=itnext
         itflag(itnext)=1
         in_element=-1
         if(itettyp(itnext).eq.ifelmtri) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                        xa,ya,za,
     *                        in_element)
         elseif(itettyp(itnext).eq.ifelmqud) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            call inside_quad2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         xa,ya,za,
     *                         in_element)
         elseif(itettyp(itnext).eq.ifelmtet) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            call inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      xa,ya,za,
     *                      in_element)
         elseif(itettyp(itnext).eq.ifelmhex) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            i5=itet1(itetoff(itnext)+5)
            i6=itet1(itetoff(itnext)+6)
            i7=itet1(itetoff(itnext)+7)
            i8=itet1(itetoff(itnext)+8)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            x5=xic_table(i5)
            y5=yic_table(i5)
            z5=zic_table(i5)
            x6=xic_table(i6)
            y6=yic_table(i6)
            z6=zic_table(i6)
            x7=xic_table(i7)
            y7=yic_table(i7)
            z7=zic_table(i7)
            x8=xic_table(i8)
            y8=yic_table(i8)
            z8=zic_table(i8)
            call inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      xa,ya,za,
     *                      in_element)
         endif
         nen=nelmnen(itettyp(itnext))
         nef=nelmnef(itettyp(itnext))
         if(in_element.ge.0) then
            iflag=1
         else
            if(itettyp(itnext).eq.ifelmtri) then
               call volume_tri(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                         volelm)
            elseif(itettyp(itnext).eq.ifelmqud) then
               call volume_qud(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         volelm)
            elseif(itettyp(itnext).eq.ifelmtet) then
               call volume_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         volelm)
            else
               call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                         volelm)
            endif
            do i=1,nef
               if(itettyp(itnext).eq.ifelmtri .or.
     *            itettyp(itnext).eq.ifelmqud) then
                  if2=itet1(itetoff(itnext) +
     *                ielmface1(1,i,itettyp(itnext)))
                  if3=itet1(itetoff(itnext) +
     *                ielmface1(2,i,itettyp(itnext)))
                  if1=i1+i2+i3-if2-if3
                  x1=xic_table(if1)
                  y1=yic_table(if1)
                  z1=zic_table(if1)
                  x2=xic_table(if2)
                  y2=yic_table(if2)
                  z2=zic_table(if2)
                  x3=xic_table(if3)
                  y3=yic_table(if3)
                  z3=zic_table(if3)
                  dx= ((y3-y2)*(za-z2)-(ya-y2)*(z3-z2))
                  dy=-((x3-x2)*(za-z2)-(xa-x2)*(z3-z2))
                  dz= ((x3-x2)*(ya-y2)-(xa-x2)*(y3-y2))
                  dxt= ((y3-y2)*(z1-z2)-(y1-y2)*(z3-z2))
                  dyt=-((x3-x2)*(z1-z2)-(x1-x2)*(z3-z2))
                  dzt= ((x3-x2)*(y1-y2)-(x1-x2)*(y3-y2))
                  volface(i)=-(dxt*dx+dyt*dy+dzt*dz)
               elseif(itettyp(itnext).eq.ifelmtet) then
                  if2=itet1(itetoff(itnext) +
     *                ielmface1(1,i,itettyp(itnext)))
                  if3=itet1(itetoff(itnext) +
     *                ielmface1(2,i,itettyp(itnext)))
                  if4=itet1(itetoff(itnext) +
     *                ielmface1(3,i,itettyp(itnext)))
                  x2=xic_table(if2)
                  y2=yic_table(if2)
                  z2=zic_table(if2)
                  x3=xic_table(if3)
                  y3=yic_table(if3)
                  z3=zic_table(if3)
                  x4=xic_table(if4)
                  y4=yic_table(if4)
                  z4=zic_table(if4)
                  dx= ((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
                  dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
                  dz= ((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
                  volface(i)=+((x3-xa)*dx+(y3-ya)*dy+(z3-za)*dz)
               elseif(itettyp(itnext).eq.ifelmhex) then
                  if2=itet1(itetoff(itnext) +
     *                ielmface1(1,i,itettyp(itnext)))
                  if3=itet1(itetoff(itnext) +
     *                ielmface1(2,i,itettyp(itnext)))
                  if4=itet1(itetoff(itnext) +
     *                ielmface1(3,i,itettyp(itnext)))
                  x2=xic_table(if2)
                  y2=yic_table(if2)
                  z2=zic_table(if2)
                  x3=xic_table(if3)
                  y3=yic_table(if3)
                  z3=zic_table(if3)
                  x4=xic_table(if4)
                  y4=yic_table(if4)
                  z4=zic_table(if4)
                  dx= ((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
                  dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
                  dz= ((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
                  volface(i)=+((x3-xa)*dx+(y3-ya)*dy+(z3-za)*dz)
               endif
            enddo
            do j=1,nef
               index=ismax(nef,volface,1)
               if(volface(index).ge.-xsmall*max(xsmall,volelm)) then
                  if(jtet1(jtetoff(itnext)+index).eq.mbndry) then
                     iflag=2
                  elseif(jtet1(jtetoff(itnext)+index).gt.mbndry) then
                     ittest=1+
     *                    (jtet1(jtetoff(itnext)+index)-mbndry-1)/nefcmo
                     if(itflag(ittest).eq.0) then
                        itnext=ittest
                        goto 100
                     else
                        volface(index)=-1.0d+30
                     endif
                  else
                     ittest=1+(jtet1(jtetoff(itnext)+index)-1)/nefcmo
                     if(itflag(ittest).eq.0) then
                        itnext=ittest
                        goto 100
                     else
                        volface(index)=-1.0d+30
                     endif
                  endif
               endif
            enddo
         endif
         if(iflag.eq.0.and.itflag(itnext).ne.0) then
            do i=1,nef
               if(jtet1(jtetoff(itnext)+i).eq.mbndry) then
               elseif(jtet1(jtetoff(itnext)+i).gt.mbndry) then
                  ittest=1+
     *                   (jtet1(jtetoff(itnext)+i)-mbndry-1)/nefcmo
                  if(itflag(ittest).eq.0) then
                     itnext=ittest
                     goto 100
                  endif
               else
                  ittest=1+(jtet1(jtetoff(itnext)+i)-1)/nefcmo
                  if(itflag(ittest).eq.0) then
                     itnext=ittest
                     goto 100
                  endif
               endif
            enddo
C*****      iflag=3
            ilost=ilost+1
            irestart=itnext
            ipath=ipath+1
            itpath(ipath)=-irestart
            do it=1,nelements
               itflag(it)=0
            enddo
         endif
 100     continue
      enddo
C
      if(iflag.eq.0.and.ipath.ge.nelements) then
         iflag=2
         write(logmess,'(a,i10,3(1pe15.7))') 'Lost ray: ',ip,xa,ya,za
         call writloga('default',1,logmess,1,ierrwrt)
      endif
C
      if(iclrpath.gt.0) then
         do i=1,ipath
            itetclr(abs(itpath(i)))=iclrpath
         enddo
      endif
C
      if(iflag.eq.2) then
         if(iconvex.eq.0) then
            distmin=1.0d+100
            do it=1,nelements
               xavg=0.0d+00
               yavg=0.0d+00
               zavg=0.0d+00
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  xavg=xavg+xic_table(i1)
                  yavg=yavg+yic_table(i1)
                  zavg=zavg+zic_table(i1)
               enddo
               xavg=xavg/dble(nelmnen(itettyp(it)))
               yavg=yavg/dble(nelmnen(itettyp(it)))
               zavg=zavg/dble(nelmnen(itettyp(it)))
               dist=(xavg-xa)**2+
     *              (yavg-ya)**2+
     *              (zavg-za)**2
               if(dist.lt.distmin) then
                  itnext=it
                  distmin=dist
               endif
            enddo
            iconvex=iconvex+1
            goto 200
         else
            distmin=1.0d+100
            do i=1,nnodes
               dist=(xic_table(i)-xa)**2+
     *              (yic_table(i)-ya)**2+
     *              (zic_table(i)-za)**2
               if(dist.lt.distmin) then
                  iclose=i
                  distmin=dist
               endif
            enddo
            value(ip)=field(iclose)
         endif
      elseif(itettyp(itnext).eq.ifelmtri) then
         i1=itet1(itetoff(itnext)+1)
         i2=itet1(itetoff(itnext)+2)
         i3=itet1(itetoff(itnext)+3)
         x1=xic_table(i1)
         y1=yic_table(i1)
         z1=zic_table(i1)
         x2=xic_table(i2)
         y2=yic_table(i2)
         z2=zic_table(i2)
         x3=xic_table(i3)
         y3=yic_table(i3)
         z3=zic_table(i3)
         call volume_tri(xa,ya,za,x2,y2,z2,x3,y3,z3,
     *                   vol1)
         call volume_tri(xa,ya,za,x3,y3,z3,x1,y1,z1,
     *                   vol2)
         call volume_tri(xa,ya,za,x1,y1,z1,x2,y2,z2,
     *                   vol3)
C
         fv1 = xinterpolate(1,itype,field(i1))
         fv2 = xinterpolate(1,itype,field(i2))
         fv3 = xinterpolate(1,itype,field(i3))
C
         value(ip) = (fv1*vol1+fv2*vol2+fv3*vol3)/(vol1+vol2+vol3)
C
C        DO THE INVERSE FUNCTION OPERATION
         favg = (field(i1)+
     *           field(i2)+
     *           field(i3))/3.0d+00
         value(ip)=xinterpolate(2,itype,value(ip))
         if(itype.eq.2) value(ip)=sign(value(ip),favg)
C
      elseif(itettyp(itnext).eq.ifelmqud) then
         i1=itet1(itetoff(itnext)+1)
         i2=itet1(itetoff(itnext)+2)
         i3=itet1(itetoff(itnext)+3)
         i4=itet1(itetoff(itnext)+4)
         x1=xic_table(i1)
         y1=yic_table(i1)
         z1=zic_table(i1)
         x2=xic_table(i2)
         y2=yic_table(i2)
         z2=zic_table(i2)
         x3=xic_table(i3)
         y3=yic_table(i3)
         z3=zic_table(i3)
         x4=xic_table(i4)
         y4=yic_table(i4)
         z4=zic_table(i4)
         if(abs(x2-x1).lt.epsilonl) then
            x12=xa
            y12=ya
            z12=za
         else
            x12=xa
            y12=y1+(y2-y1)*(xa-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xa-x1)/(x2-x1)
         endif
         if(abs(y3-y2).lt.epsilonl) then
            x23=xa
            y23=ya
            z23=za
         else
            x23=x2+(x3-x2)*(ya-y2)/(y3-y2)
            y23=ya
            z23=z2+(z3-z2)*(ya-y2)/(y3-y2)
         endif
         if(abs(x4-x3).lt.epsilonl) then
            x34=xa
            y34=ya
            z34=za
         else
            x34=xa
            y34=y3+(y4-y3)*(xa-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xa-x3)/(x4-x3)
         endif
         if(abs(y4-y1).lt.epsilonl) then
            x41=xa
            y41=ya
            z41=za
         else
            x41=x1+(x4-x1)*(ya-y1)/(y4-y1)
            y41=ya
            z41=z1+(z4-z1)*(ya-y1)/(y4-y1)
         endif
C
         call volume_qud(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                   volqud)
C
        call volume_qud(x1 ,y1 ,z1 ,x12,y12,z12,xa ,ya ,za ,x41,y41,z41,
     *                  volqud3)
        call volume_qud(x2 ,y2 ,z2 ,x23,y23,z23,xa ,ya ,za ,x12,y12,z12,
     *                  volqud4)
        call volume_qud(x3 ,y3 ,z3 ,x34,y34,z34,xa ,ya ,za ,x23,y23,z23,
     *                  volqud1)
        call volume_qud(x4 ,y4 ,z4 ,x41,y41,z41,xa ,ya ,za ,x34,y34,z34,
     *                  volqud2)
C
         volqudb=volqud1+volqud2+volqud3+volqud4
C
C
C
C        IF WE GOT HERE, THE POINT IS INSIDE OR WITHIN TEPSILON
C        OF THE TET.  TRANSFORM THE FIELD VALUES BY THE DESIRED
C        FUNCTION
C
         fv1 = xinterpolate(1,itype,field(i1))
         fv2 = xinterpolate(1,itype,field(i2))
         fv3 = xinterpolate(1,itype,field(i3))
         fv4 = xinterpolate(1,itype,field(i4))
C
         value(ip) = (fv1*volqud1+fv2*volqud2+
     &                fv3*volqud3+fv4*volqud4) / volqudb
C
C        DO THE INVERSE FUNCTION OPERATION
         favg = (field(i1)+
     *           field(i2)+
     *           field(i3)+
     *           field(i4))/4.0d+00
         value(ip)=xinterpolate(2,itype,value(ip))
         if(itype.eq.2) value(ip)=sign(value(ip),favg)
      elseif(itettyp(itnext).eq.ifelmtet) then
         i1=itet1(itetoff(itnext)+1)
         i2=itet1(itetoff(itnext)+2)
         i3=itet1(itetoff(itnext)+3)
         i4=itet1(itetoff(itnext)+4)
C
C        DO THE INTERPOLATION:  INTERPOLATED VALUE = SUM OF VALUES AT NODES
C        MULTIPLIED BY RELATIVE VOLUME OF TET FORMED BY THE POINT AND
C        THE THREE VERTS OPPOSITE THE NODE
C
C        1,2,3,P
C
             d1x = xic_table(i2) - xic_table(i1)
             d1y = yic_table(i2) - yic_table(i1)
             d1z = zic_table(i2) - zic_table(i1)
             d2x = xic_table(i3) - xic_table(i1)
             d2y = yic_table(i3) - yic_table(i1)
             d2z = zic_table(i3) - zic_table(i1)
             d3x = xa - xic_table(i1)
             d3y = ya - yic_table(i1)
             d3z = za - zic_table(i1)
             vol4 = ( d3x*(d1z*d2y - d1y*d2z) +
     &                d3y*(d1x*d2z - d2x*d1z) +
     &                d3z*(d1y*d2x - d1x*d2y) )/6.0
C
C        1,4,2,P
C
             d1x = xic_table(i4) - xic_table(i1)
             d1y = yic_table(i4) - yic_table(i1)
             d1z = zic_table(i4) - zic_table(i1)
             d2x = xic_table(i2) - xic_table(i1)
             d2y = yic_table(i2) - yic_table(i1)
             d2z = zic_table(i2) - zic_table(i1)
 
             vol3 = ( d3x*(d1z*d2y - d1y*d2z) +
     &                d3y*(d1x*d2z - d2x*d1z) +
     &                d3z*(d1y*d2x - d1x*d2y) )/6.0
C
C        1,3,4,P
C
             d1x = xic_table(i3) - xic_table(i1)
             d1y = yic_table(i3) - yic_table(i1)
             d1z = zic_table(i3) - zic_table(i1)
             d2x = xic_table(i4) - xic_table(i1)
             d2y = yic_table(i4) - yic_table(i1)
             d2z = zic_table(i4) - zic_table(i1)
 
             vol2 = ( d3x*(d1z*d2y - d1y*d2z) +
     &                d3y*(d1x*d2z - d2x*d1z) +
     &                d3z*(d1y*d2x - d1x*d2y) )/6.0
C
C        3,2,4,P
C
             d1x = xic_table(i2) - xic_table(i3)
             d1y = yic_table(i2) - yic_table(i3)
             d1z = zic_table(i2) - zic_table(i3)
             d2x = xic_table(i4) - xic_table(i3)
             d2y = yic_table(i4) - yic_table(i3)
             d2z = zic_table(i4) - zic_table(i3)
             d3x = xa - xic_table(i3)
             d3y = ya - yic_table(i3)
             d3z = za - zic_table(i3)
 
             vol1 = ( d3x*(d1z*d2y - d1y*d2z) +
     &                d3y*(d1x*d2z - d2x*d1z) +
     &                d3z*(d1y*d2x - d1x*d2y) )/6.0
C
             fv1 = xinterpolate(1,itype,field(i1))
             fv2 = xinterpolate(1,itype,field(i2))
             fv3 = xinterpolate(1,itype,field(i3))
             fv4 = xinterpolate(1,itype,field(i4))
C
             value(ip) = (fv1*vol1+fv2*vol2+
     &                    fv3*vol3+fv4*vol4)/(vol1+vol2+vol3+vol4)
C
C        DO THE INVERSE FUNCTION OPERATION
             favg = (field(i1)+
     *               field(i2)+
     *               field(i3)+
     *               field(i4))/4.0d+00
             value(ip)=xinterpolate(2,itype,value(ip))
             if(itype.eq.2) value(ip)=sign(value(ip),favg)
C
C
      elseif(itettyp(itnext).eq.ifelmhex) then
         i1=itet1(itetoff(itnext)+1)
         i2=itet1(itetoff(itnext)+2)
         i3=itet1(itetoff(itnext)+3)
         i4=itet1(itetoff(itnext)+4)
         i5=itet1(itetoff(itnext)+5)
         i6=itet1(itetoff(itnext)+6)
         i7=itet1(itetoff(itnext)+7)
         i8=itet1(itetoff(itnext)+8)
         x1=xic_table(i1)
         y1=yic_table(i1)
         z1=zic_table(i1)
         x2=xic_table(i2)
         y2=yic_table(i2)
         z2=zic_table(i2)
         x3=xic_table(i3)
         y3=yic_table(i3)
         z3=zic_table(i3)
         x4=xic_table(i4)
         y4=yic_table(i4)
         z4=zic_table(i4)
         x5=xic_table(i5)
         y5=yic_table(i5)
         z5=zic_table(i5)
         x6=xic_table(i6)
         y6=yic_table(i6)
         z6=zic_table(i6)
         x7=xic_table(i7)
         y7=yic_table(i7)
         z7=zic_table(i7)
         x8=xic_table(i8)
         y8=yic_table(i8)
         z8=zic_table(i8)
C
         if(abs(x2-x1).lt.epsilonl) then
            x12=xa
            y12=y1+(y2-y1)*(xa-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xa-x1)/(x2-x1)
         else
            x12=xa
            y12=ya
            z12=za
         endif
         if(abs(y3-y2).lt.epsilonl) then
            x23=x2+(x3-x2)*(ya-y2)/(y3-y2)
            y23=ya
            z23=z2+(z3-z2)*(ya-y2)/(y3-y2)
         else
            x23=xa
            y23=ya
            z23=za
         endif
         if(abs(x4-x3).lt.epsilonl) then
            x34=xa
            y34=y3+(y4-y3)*(xa-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xa-x3)/(x4-x3)
         else
            x34=xa
            y34=ya
            z34=za
         endif
         if(abs(y4-y1).lt.epsilonl) then
            x41=x1+(x4-x1)*(ya-y1)/(y4-y1)
            y41=ya
            z41=z1+(z4-z1)*(ya-y1)/(y4-y1)
         else
            x41=xa
            y41=ya
            z41=za
         endif
C
         if(abs(x6-x5).lt.epsilonl) then
            x56=xa
            y56=y5+(y6-y5)*(xa-x5)/(x6-x5)
            z56=z5+(z6-z5)*(xa-x5)/(x6-x5)
         else
            x56=xa
            y56=ya
            z56=za
         endif
         if(abs(y7-y6).lt.epsilonl) then
            x67=x6+(x7-x6)*(ya-y6)/(y7-y6)
            y67=ya
            z67=z6+(z7-z6)*(ya-y6)/(y7-y6)
         else
            x67=xa
            y67=ya
            z67=za
         endif
         if(abs(x8-x7).lt.epsilonl) then
            x78=xa
            y78=y7+(y8-y7)*(xa-x7)/(x8-x7)
            z78=z7+(z8-z7)*(xa-x7)/(x8-x7)
         else
            x78=xa
            y78=ya
            z78=za
         endif
         if(abs(y8-y5).lt.epsilonl) then
            x85=x5+(x8-x5)*(ya-y5)/(y8-y5)
            y85=ya
            z85=z5+(z8-z5)*(ya-y5)/(y8-y5)
         else
            x85=xa
            y85=ya
            z85=za
         endif
C
         if(abs(z5-z1).lt.epsilonl) then
            x15=x1+(x5-x1)*(za-z1)/(z5-z1)
            y15=y1+(y5-y1)*(za-z1)/(z5-z1)
            z15=za
         else
            x15=xa
            y15=ya
            z15=za
         endif
         if(abs(z6-z2).lt.epsilonl) then
            x26=x2+(x6-x2)*(za-z2)/(z6-z2)
            y26=y2+(y6-y2)*(za-z2)/(z6-z2)
            z26=za
         else
            x26=xa
            y26=ya
            z26=za
         endif
         if(abs(z7-z3).lt.epsilonl) then
            x37=x3+(x7-x3)*(za-z3)/(z7-z3)
            y37=y3+(y7-y3)*(za-z3)/(z7-z3)
            z37=za
         else
            x37=xa
            y37=ya
            z37=za
         endif
         if(abs(z8-z4).lt.epsilonl) then
            x48=x4+(x8-x4)*(za-z4)/(z8-z4)
            y48=y4+(y8-y4)*(za-z4)/(z8-z4)
            z48=za
         else
            x48=xa
            y48=ya
            z48=za
         endif
C
         if(sqrt((x3-x1)**2+(y3-y1)**2).lt.epsilonl) then
            x13=xa
            y13=ya
            z13=z1+(z3-z1)*sqrt(xa**2+ya**2)/sqrt((x3-x1)**2+(y3-y1)**2)
         else
            x13=xa
            y13=ya
            z13=za
         endif
         if(sqrt((x6-x1)**2+(z6-z1)**2).lt.epsilonl) then
            x16=xa
            y16=y1+(y6-y1)*sqrt(xa**2+za**2)/sqrt((x6-x1)**2+(z6-z1)**2)
            z16=za
         else
            x16=xa
            y16=ya
            z16=za
         endif
         if(sqrt((y8-y1)**2+(z8-z1)**2).lt.epsilonl) then
            x18=x1+(x8-x1)*sqrt(ya**2+za**2)/sqrt((y8-y1)**2+(z8-z1)**2)
            y18=ya
            z18=za
         else
            x18=xa
            y18=ya
            z18=za
         endif
C
         if(sqrt((y7-y2)**2+(z7-z2)**2).lt.epsilonl) then
            x72=x2+(x7-x2)*sqrt(ya**2+za**2)/sqrt((y7-y2)**2+(z7-z2)**2)
            y72=ya
            z72=za
         else
            x72=xa
            y72=ya
            z72=za
         endif
         if(sqrt((x7-x4)**2+(z7-z4)**2).lt.epsilonl) then
            x74=xa
            y74=y4+(y7-y4)*sqrt(xa**2+za**2)/sqrt((x7-x4)**2+(z7-z4)**2)
            z74=za
         else
            x74=xa
            y74=ya
            z74=za
         endif
         if(sqrt((x7-x5)**2+(y7-y5)**2).lt.epsilonl) then
            x75=xa
            y75=ya
            z75=z5+(z7-z5)*sqrt(xa**2+ya**2)/sqrt((x7-x5)**2+(y7-y5)**2)
         else
            x75=xa
            y75=ya
            z75=za
         endif
C
         call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                   x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                   volhex)
C
        call volume_hex(x1 ,y1 ,z1 ,x12,y12,z12,x13,y13,z13,x41,y41,z41,
     *                  x15,y15,z15,x16,y16,z16,xa ,ya ,za ,x18,y18,z18,
     *                  volhex7)
        call volume_hex(x2 ,y2 ,z2 ,x23,y23,z23,x13,y13,z13,x12,y12,z12,
     *                  x26,y26,z26,x72,y72,z72,xa ,ya ,za ,x16,y16,z16,
     *                  volhex8)
        call volume_hex(x3 ,y3 ,z3 ,x34,y34,z34,x13,y13,z13,x23,y23,z23,
     *                  x37,y37,z37,x74,y74,z74,xa ,ya ,za ,x72,y72,z72,
     *                  volhex5)
        call volume_hex(x4 ,y4 ,z4 ,x41,y41,z41,x13,y13,z13,x34,y34,z34,
     *                  x48,y48,z48,x18,y18,z18,xa ,ya ,za ,x74,y74,z74,
     *                  volhex6)
        call volume_hex(x15,y15,z15,x16,y16,z16,xa ,ya ,za ,x18,y18,z18,
     *                  x5 ,y5 ,z5 ,x56,y56,z56,x75,y75,z75,x85,y85,z85,
     *                  volhex3)
        call volume_hex(x26,y26,z26,x72,y72,z72,xa ,ya ,za ,x16,y16,z16,
     *                  x6 ,y6 ,z6 ,x67,y67,z67,x75,y75,z75,x56,y56,z56,
     *                  volhex4)
        call volume_hex(x37,y37,z37,x74,y74,z74,xa ,ya ,za ,x72,y72,z72,
     *                  x7 ,y7 ,z7 ,x78,y78,z78,x75,y75,z75,x67,y67,z67,
     *                  volhex1)
        call volume_hex(x48,y48,z48,x18,y18,z18,xa ,ya ,za ,x74,y74,z74,
     *                  x8 ,y8 ,z8 ,x85,y85,z85,x75,y75,z75,x78,y78,z78,
     *                  volhex2)
C
         volhexb=volhex1+volhex2+volhex3+volhex4+
     *           volhex5+volhex6+volhex7+volhex8
C
C
C
C        IF WE GOT HERE, THE POINT IS INSIDE OR WITHIN TEPSILON
C        OF THE TET.  TRANSFORM THE FIELD VALUES BY THE DESIRED
C        FUNCTION
C
         ifound = 1
         fv1 = xinterpolate(1,itype,field(i1))
         fv2 = xinterpolate(1,itype,field(i2))
         fv3 = xinterpolate(1,itype,field(i3))
         fv4 = xinterpolate(1,itype,field(i4))
         fv5 = xinterpolate(1,itype,field(i5))
         fv6 = xinterpolate(1,itype,field(i6))
         fv7 = xinterpolate(1,itype,field(i7))
         fv8 = xinterpolate(1,itype,field(i8))
C
         value(ip) = (fv1*volhex1+fv2*volhex2+
     &                fv3*volhex3+fv4*volhex4+
     &                fv5*volhex5+fv6*volhex6+
     &                fv7*volhex7+fv8*volhex8) / volhexb
C
C        DO THE INVERSE FUNCTION OPERATION
         favg = (field(i1)+
     *           field(i2)+
     *           field(i3)+
     *           field(i4)+
     *           field(i5)+
     *           field(i6)+
     *           field(i7)+
     *           field(i8))/8.0d+00
         value(ip)=xinterpolate(2,itype,value(ip))
         if(itype.eq.2) value(ip)=sign(value(ip),favg)
      endif
C
      enddo
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
