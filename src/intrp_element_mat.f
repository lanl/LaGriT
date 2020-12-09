*dk,intrp_element
      subroutine intrp_element_mat(corder_value,corder_table,
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
C         POINT, FINDS A MATERIAL 
C         ON A HEXAHEDRAL GRID IN X,Y,Z.  THE METHOD IS TO
C         FIND WHICH ELEMENT IN LIST THE POINT LIES INSIDE AND THEN
C         GET THE MATERIAL NUMBER OF THAT ELEMENT
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
C        $Log: intrp_element_mat.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Wed Feb 04 10:11:00 1998   kmb
CPVCS    Removed commented out lines
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:52:18 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Tue Apr 08 12:56:52 1997   kmb
CPVCS    Initial revision.
C
CPVCS    Copied this subroutine from interp_element.f.  Modified
CPVCS     it to get material types.  K.Bower, 1/27/97
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
      pointer (ipivalue, ivalue)
      integer ivalue(1000000)

      pointer (ipitflag, itflag)
      integer itflag(1000000)
C
C
      pointer (ipfield, field)
C     real*8 field(1000000)
      integer field(1000000)

      integer ixpath
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
      pointer (ipvalue, value)


      integer value(nump)


      dimension xintrp(nump),yintrp(nump),zintrp(nump)
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
c
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      length=nelements
      call mmgetblk('itflag',isubname,ipitflag,length,2,ierr)
      call mmgetblk('ivalue',isubname,ipivalue,length,2,ierr)
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

         matmax=1
      do it=1,nelements
         itflag(it)=0

c         this is to get the maximum number of materials
         matmax=max(matmax,itetclr(it))
      enddo
      ilost=0
      irestart=0
      iflag=0
      ipath=0
      do  ixpath=1,nelements
         ipath=ipath+1
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
            ivalue(ip)=max(ivalue(ip), itetclr(ixpath))
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
               xavg=xavg/real(nelmnen(itettyp(it)))
               yavg=yavg/real(nelmnen(itettyp(it)))
               zavg=zavg/real(nelmnen(itettyp(it)))
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
            value(ip)=matmax+1
         endif

C     if iflag.ne.2 then
C
      else
         value(ip)=ivalue(itnext)
C
C
      endif
C
      enddo
      goto 9999
 9999 continue

      call mmrelprt(isubname,icscode)
      return
      end
