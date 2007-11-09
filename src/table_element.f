      subroutine table_element(cmohex,
     &                         ipxintrp,ipyintrp,ipzintrp,nump,
     &                         ipielement,
     &                         ierr2)
C
      implicit none
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
C         FIELD ON A  GRID IN X,Y,Z.  THE METHOD IS TO
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
C
C     OUTPUT ARGUMENTS -
C
C        ipielement - POINTER TO THE LIST OF OUTPUT ELEMENTS
C
C        ierr2    - ERROR INDICATOR, RETURNS PLACE IN LIST WHERE THERE
C                   WAS NO BOUNDING TET IN THE LIST FOR THIS POINT.
C                   RETURNS 0 IF NO TPROBLEMS, <0 IF SOME OTHER PROBLEM.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/table_element.f_a  $
CPVCS    
CPVCS       Rev 1.10   30 Jan 2002 10:28:00   dcg
CPVCS    remove duplicate declarations - put declarations in order
CPVCS
CPVCS       Rev 1.9   17 Dec 2001 12:50:42   dcg
CPVCS    implicit none plus clean up
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 17:02:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Apr 07 14:54:16 1997   het
CPVCS    Add the element to element interpoation algorithm.
CPVCS
CPVCS       Rev 1.6   Fri Jan 24 13:54:12 1997   het
CPVCS    Add the AMR arrays to the table lookup scheme to only look
CPVCS       at active elements.
CPVCS
CPVCS       Rev 1.5   Mon Dec 09 09:01:44 1996   het
CPVCS    Get the active set of elements.
CPVCS
CPVCS       Rev 1.4   Mon Dec 02 08:56:20 1996   het
CPVCS
CPVCS       Rev 1.3   Tue Nov 26 13:49:52 1996   het
CPVCS    Fix an error with calculating volumes of hybrid pri and pyr elements
CPVCS
CPVCS       Rev 1.2   Thu Nov 21 19:06:54 1996   het
CPVCS    Start rays at the element nearest the centroid of the points set.
CPVCS
CPVCS       Rev 1.1   Mon Nov 11 20:57:04 1996   het
CPVCS    Add pri and pyr element types.
CPVCS
CPVCS       Rev 1.0   Mon Jul 29 15:42:42 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
C
      include 'local_element.h'
      include 'consts.h'
C
      character*32 cmo, isubname
C
      pointer (ipitp1, itp1)
      integer itp1(*)
C
      pointer (ipxic_table, xic_table)
      pointer (ipyic_table, yic_table)
      pointer (ipzic_table, zic_table)
      real*8 xic_table(*), yic_table(*), zic_table(*)
C
      pointer (ipxic_value, xic_value)
      pointer (ipyic_value, yic_value)
      pointer (ipzic_value, zic_value)
      real*8 xic_value(*), yic_value(*), zic_value(*)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitflag, itflag)
      integer itflag(*)
C
      pointer (ipitpath, itpath)
      integer itpath(*)
C
      pointer (ipireal1, ireal1)
      integer ireal1(*)
C
      pointer (ipitetkid, itetkid)
      integer itetkid(*)
C
      pointer (ipitactive, itactive)
      integer itactive(*)
C
      real*8 volface(100)
C
      integer iclrpath
      data iclrpath / 0 /
C
      real*8 xsmall
      data xsmall / 1.0d-06 /
C
      character*32 coption
      data coption / '-active-' /
C
C
C######################################################################
C
      integer ierr2,nnodes,ilen,ityp,ierror,length,ierrdum,it,iclose,
     *  ittest,ismax,index,if4,if1,if3,if2,j,nef,nen,
     *  i1,i2,i3,i4,i5,i6,i7,i8,in_element,ipath,iflag,irestart,
     *  iconvex,itnext,ilost,ntactive,i,idum,icharlnf,mbndry,
     *  nefcmo,nelements,nump,icscode,ierr,itstart,ip,iprintcount
      real*8 dzt,dyt,dxt,dz,dy,dx,volelm,x1,x2,x3,x4,x5,x6,x7,x8,
     *  y1,y2,y3,y4,y5,y6,y7,y8,z1,z2,z3,z4,z5,z6,z7,z8,za,ya,xa,
     *  xavg,yavg,zavg,distmin,xavg_value,yavg_value,zavg_value,
     *  dist,tepsilon
      pointer (ipxintrp, xintrp)
      pointer (ipyintrp, yintrp)
      pointer (ipzintrp, zintrp)
      real*8 xintrp(nump),yintrp(nump),zintrp(nump)
      pointer (ipielement, ielement)
      integer ielement(nump)
      character*(*) cmohex
C
C
C######################################################################
C
C
      isubname='intrphex'
      cmo=cmohex
C
      ierr2 = 0
      tepsilon = 1.0d-10
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierror)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierror)
      call cmo_get_info('faces_per_element',cmo,nefcmo,ilen,ityp,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ierror)
C
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic_table,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic_table,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic_table,ilen,ityp,ierror)
C
      length=nnodes
      call mmgetblk("ireal1",isubname,ipireal1,length,1,icscode)
C
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('unpacktp', isubname)
C
      ipxic_value=ipxintrp
      ipyic_value=ipyintrp
      ipzic_value=ipzintrp
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
      call mmfindbk('itetkid',cmo,ipitetkid,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetkid',isubname,ipitetkid,length,1,icscode)
         do it=1,nelements
            itetkid(it)=0
         enddo
      endif
C
      length=nelements
      call mmgetblk('itactive',isubname,ipitactive,length,1,icscode)
      length=icharlnf(coption)
      if(coption(1:length).eq.'-all-') then
         ntactive=nelements
         do it=1,nelements
            itactive(it)=it
         enddo
      elseif(coption(1:length).eq.'-active-') then
         ntactive=0
         call get_active_elements(cmo,ntactive,ipitactive)
      endif
C
      length=nelements
      call mmgetblk('itflag',isubname,ipitflag,length,2,ierr)
      call mmgetblk('itpath',isubname,ipitpath,length,2,ierr)
C
      xavg_value=zero
      yavg_value=zero
      zavg_value=zero
      do i1=1,nump
         xavg_value=xavg_value+xic_value(i1)
         yavg_value=yavg_value+yic_value(i1)
         zavg_value=zavg_value+zic_value(i1)
      enddo
      xavg_value=xavg_value/nump
      yavg_value=yavg_value/nump
      zavg_value=zavg_value/nump
      itstart=1
      distmin=1.0d+100
      do idum=1,ntactive
         it=itactive(idum)
         xavg=zero
         yavg=zero
         zavg=zero
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            xavg=xavg+xic_table(i1)
            yavg=yavg+yic_table(i1)
            zavg=zavg+zic_table(i1)
         enddo
         xavg=xavg/real(nelmnen(itettyp(it)))
         yavg=yavg/real(nelmnen(itettyp(it)))
         zavg=zavg/real(nelmnen(itettyp(it)))
         dist=(xavg-xavg_value)**2+
     *        (yavg-yavg_value)**2+
     *        (zavg-zavg_value)**2
         if(dist.lt.distmin) then
            itstart=it
            distmin=dist
         endif
      enddo
C
      iprintcount=0
C
      do ip=1,nump
         xa=xic_value(ip)
         ya=yic_value(ip)
         za=zic_value(ip)
C
      itnext=itstart
      iconvex=0
 200  continue
      do it=1,nelements
         itflag(it)=0
      enddo
      ilost=0
      irestart=0
      iflag=0
      ipath=0
      dowhile(iflag.eq.0)
         if(ipath.ge.nelements) then
            iprintcount=iprintcount+1
            if(iprintcount.le.30) then
               write(logmess,'(a,a,i10)') 'To many paths: ',
     *                                    cmo(1:icharlnf(cmo)),
     *                                    ip
               call writloga('default',0,logmess,0,ierr)
            endif
            goto 110
         endif
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
         elseif(itettyp(itnext).eq.ifelmpyr) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            i5=itet1(itetoff(itnext)+5)
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
            call inside_pyr(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,
     *                      xa,ya,za,
     *                      in_element)
         elseif(itettyp(itnext).eq.ifelmpri) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            i5=itet1(itetoff(itnext)+5)
            i6=itet1(itetoff(itnext)+6)
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
            call inside_pri(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,
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
            elseif(itettyp(itnext).eq.ifelmpyr) then
               call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         x5,y5,z5,x5,y5,z5,x5,y5,z5,x5,y5,z5,
     *                         volelm)
            elseif(itettyp(itnext).eq.ifelmpri) then
               call volume_hex(x1,y1,z1,x2,y2,z2,x2,y2,z2,x3,y3,z3,
     *                         x4,y4,z4,x5,y5,z5,x5,y5,z5,x6,y6,z6,
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
                  x1=0.0
                  y1=0.0
                  z1=0.0
                  do j=1,nelmnen(itettyp(itnext))
                     if1=itet1(itetoff(itnext)+j)
                     x1=x1+xic_table(if1)
                     y1=y1+yic_table(if1)
                     z1=z1+zic_table(if1)
                  enddo
                  x1=x1/nelmnen(itettyp(itnext))
                  y1=y1/nelmnen(itettyp(itnext))
                  z1=z1/nelmnen(itettyp(itnext))
                  x2=xic_table(if2)
                  y2=yic_table(if2)
                  z2=zic_table(if2)
                  x3=xic_table(if3)
                  y3=yic_table(if3)
                  z3=zic_table(if3)
                  dx=  ((y3-y2)*(za-z2)-(ya-y2)*(z3-z2))
                  dy= -((x3-x2)*(za-z2)-(xa-x2)*(z3-z2))
                  dz=  ((x3-x2)*(ya-y2)-(xa-x2)*(y3-y2))
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
               elseif(itettyp(itnext).eq.ifelmpyr) then
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
               elseif(itettyp(itnext).eq.ifelmpri) then
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
                        dowhile(itetkid(ittest).ne.0)
                           itflag(ittest)=1
                           ittest=itetkid(ittest)
                        enddo
                        itnext=ittest
                        goto 100
                     else
                        volface(index)=-1.0d+30
                     endif
                  else
                     ittest=1+(jtet1(jtetoff(itnext)+index)-1)/nefcmo
                     if(itflag(ittest).eq.0) then
                        dowhile(itetkid(ittest).ne.0)
                           itflag(ittest)=1
                           ittest=itetkid(ittest)
                        enddo
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
                     dowhile(itetkid(ittest).ne.0)
                        itflag(ittest)=1
                        ittest=itetkid(ittest)
                     enddo
                     itnext=ittest
                     goto 100
                  endif
               else
                  ittest=1+(jtet1(jtetoff(itnext)+i)-1)/nefcmo
                  if(itflag(ittest).eq.0) then
                     dowhile(itetkid(ittest).ne.0)
                        itflag(ittest)=1
                        ittest=itetkid(ittest)
                     enddo
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
 110  continue
C
      if(iclrpath.gt.0) then
         do i=1,ipath
            itetclr(abs(itpath(i)))=iclrpath
         enddo
      endif
C
      if(iflag.eq.0) then
         ielement(ip)=0
      elseif(iflag.eq.2) then
         if(iconvex.eq.0) then
            distmin=1.0d+100
            do idum=1,ntactive
               it=itactive(idum)
               xavg=zero
               yavg=zero
               zavg=zero
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
               if(ireal1(i).eq.1) then
                  dist=(xic_table(i)-xa)**2+
     *                 (yic_table(i)-ya)**2+
     *                 (zic_table(i)-za)**2
                  if(dist.lt.distmin) then
                     iclose=i
                     distmin=dist
                  endif
               endif
            enddo
            ielement(ip)=-iclose
         endif
      else
         ielement(ip)=itnext
         itstart=itnext
      endif
C
      enddo
C
      if(iprintcount.gt.0) then
         write(logmess,'(a,i10)') 'Total lost paths: ',iprintcount
         call writloga('default',1,logmess,1,ierr)
      endif
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
