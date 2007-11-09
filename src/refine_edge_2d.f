*dk,refine2ed
      subroutine refine_edge_2d(cmo,ierr1)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/refine_edge_2d.f_a  $
CPVCS    
CPVCS       Rev 1.36   23 Jul 2007 09:31:50   gable
CPVCS    Fix error in checking in Rev 1.35 overwrote with geniee.f. Oops.
CPVCS    
CPVCS       Rev 1.34   08 Feb 2006 14:38:16   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.33   25 Mar 2002 09:53:34   dcg
CPVCS    reset ipointj at end
CPVCS    get rid of unused subroutines
CPVCS    
CPVCS       Rev 1.32   05 May 2000 15:31:18   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.31   Tue Mar 03 12:34:18 1998   dcg
CPVCS    fix number of arguments in x3d_error calls
CPVCS
CPVCS       Rev 1.30   Fri Oct 10 08:15:36 1997   gable
CPVCS    The consts.h file was not include. Things were working
CPVCS    fine on the SUN's since the constant being used was
CPVCS    zero, however, on the SGI this was causing problems.
CPVCS    consts.h is now included and zero is no longer passed
CPVCS    as an argument to int_edge.
CPVCS
CPVCS       Rev 1.29   Mon Apr 14 16:59:14 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.28   Sun Feb 23 10:40:38 1997   het
CPVCS    Define the xarea_ref variable with a data statement.
CPVCS
CPVCS       Rev 1.27   Fri Jan 24 14:29:10 1997   het
CPVCS    Add new children points if necessary.
CPVCS
CPVCS       Rev 1.26   Thu Jan 09 13:47:22 1997   gable
CPVCS    Fixed bug. xweight_source(1,nadd1) and xweight_source(2,nadd1)
CPVCS    were reversed. Case where refinement should have resulted
CPVCS    in perpendicular intersection of edge did not work. That
CPVCS    is now fixed.
CPVCS
CPVCS    Added some comments.
CPVCS
CPVCS       Rev 1.25   Tue Nov 12 13:10:38 1996   dcg
CPVCS    restore overwritten changes back to version 1.23
CPVCS    don't know what changes were in 1.24
CPVCS
CPVCS       Rev 1.23   Wed Oct 30 21:30:36 1996   het
CPVCS    Add an error flag to the refine_edge_2d call to
CPVCS       indicate that no edges where refined. Also use
CPVCS       parents to select edges for refinement.
CPVCS
CPVCS       Rev 1.22   Mon Oct 21 12:16:00 1996   het
CPVCS
CPVCS    Correct an error with an unitialized variable
CPVCS
CPVCS       Rev 1.21   Tue Jul 30 14:03:02 1996   dcg
CPVCS    put implicit before includes (for HP)
CPVCS
CPVCS       Rev 1.20   Wed Jul 24 17:34:18 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.19   Tue Apr 30 11:25:12 1996   dcg
CPVCS    replace literal in argument lists with consts variables
CPVCS
CPVCS       Rev 1.18   Wed Jan 24 06:08:58 1996   het
CPVCS    Change to value of xtrifac from 1/nef to 1/2
CPVCS
CPVCS       Rev 1.17   Wed Jan 03 10:08:02 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.16   11/07/95 17:24:24   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.15   11/07/95 11:26:50   het
CPVCS    Modify the 2D triangle refinement algorithms.
CPVCS
CPVCS       Rev 1.14   10/20/95 10:48:30   het
CPVCS    Fix iparent memory management error and add new refine options.
CPVCS
CPVCS       Rev 1.13   10/19/95 14:12:58   dcg
CPVCS    delete dimension statements not used in refine2db
CPVCS    remove obsolete commented out code
CPVCS
CPVCS       Rev 1.12   10/18/95 12:18:18   het
CPVCS    Fix a memory management error
CPVCS
CPVCS       Rev 1.11   09/29/95 09:13:50   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.10   08/15/95 18:23:36   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.9   07/14/95 10:14:08   het
CPVCS    Add new doping interpolate commands
CPVCS
CPVCS       Rev 1.8   06/27/95 16:37:20   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.7   06/21/95 13:20:48   het
CPVCS    Fix errors associated with point types of added nodes
CPVCS
CPVCS       Rev 1.6   06/08/95 03:55:46   het
CPVCS    Fix two memory management errors
CPVCS
CPVCS       Rev 1.5   06/05/95 10:36:32   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.4   05/nef0/95 07:51:14   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.3   05/26/95 13:16:38   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.2   05/24/95 15:42:10   het
CPVCS    Add another 2D triangle refinement option
CPVCS
CPVCS       Rev 1.1   03/28/95 12:34:50   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:54   pvcs
CPVCS    Original version.
C
       implicit real*8 (a-h,o-z)
      include 'consts.h'
      include "local_element.h"
C
      character*132 logmess
C
      character*(*) cmo
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer imt1(1000000), itp1(1000000), isn1(1000000)
      dimension xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      integer itet(3,1000000), jtet(3,1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipint1, int1)
      integer int1(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itflag(1000000),
     *        itetnn(3,1000000), itetnn1(3,1000000), itetnn2(3,1000000)
      pointer (ipiedge_tet, iedge_tet)
      pointer (ipiedge_face, iedge_face)
      pointer (ipiedge_edge, iedge_edge)
      integer iedge_tet(6*1000000), iedge_face(6*1000000),
     *        iedge_edge(6*1000000)
      pointer (ipxedge, xedge)
      real*8 xedge(6*1000000)
C
      pointer (ipint1add, int1add)
      integer int1add(1000000)
C
      parameter (nvalues=2)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(1000000), list_source(nvalues,1000000)
      real*8 xweight_source(nvalues,1000000)
C
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
      integer itriface0(3), itriface1(3,3)
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
C
      data xfactri / 0.25d+00 /
C*****data xfactri / 0.333333333333333d+00 /
C*****data xfactri / 0.5d+00 /
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
      ierr1=0
      nedge=0
      iedgeiter = 0
C
      isubname='refine_edge_2d'
C
C     ******************************************************************
C
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
C
C     ******************************************************************
C
C     Get the parents for each node.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("iparent",isubname,ipiparent,length,2,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
C     ******************************************************************
C
C     Do we have an interface:
C        int1() =  0 ==> not an interface point.
C        int1() =  1 ==> an interface point.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("int1",isubname,ipint1,length,2,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      length=3*ntets
      call mmgetblk('iedgetet',isubname,ipiedge_tet,length,2,icscode)
      call mmgetblk('iedgefac',isubname,ipiedge_face,length,2,icscode)
      call mmgetblk('iedgeedg',isubname,ipiedge_edge,length,2,icscode)
      call mmgetblk('xedge',isubname,ipxedge,length,2,icscode)
      length=ntets
      call mmgetblk('itflag',isubname,ipitflag,length,2,icscode)
      length=3*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,2,icscode)
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,2,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,2,icscode)
c
c     Find and flag all outside or interface noded
c
      do it=1,ntets
         do i=1,3
            itetnn(i,it)=itet(i,it)
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/nef
               itetnn2(i,it)=jtet(i,it)-mbndry-nef*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/nef
               itetnn2(i,it)=jtet(i,it)-nef*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
      nedge=0
      do it=1,ntets
         do i=1,3
            iflag=0
            if(itetnn1(i,it).le.0.or.itetnn1(i,it).gt.ntets) then
c
c              This is a boundary node
c
               iflag=1
            else
               jt=itetnn1(i,it)
               jf=itetnn2(i,it)
c
c              This is an interface node
c
               if(itetclr(it).ne.itetclr(jt)) iflag=2
            endif
            if(iflag.ne.0) then
               j1=itet(itriface1(3,i),it)
               j2=itet(itriface1(1,i),it)
               j3=itet(itriface1(2,i),it)
               xdot=(xic(j2)-xic(j1))*(xic(j3)-xic(j1)) +
     *              (yic(j2)-yic(j1))*(yic(j3)-yic(j1)) +
     *              (zic(j2)-zic(j1))*(zic(j3)-zic(j1))
               if(xdot.lt.-1.0e-10) then
c
c              We have an outside or interface edge that is
c              opposite a vertex angle greater than 90
c
                  xa=xic(j1)
                  ya=yic(j1)
                  za=zic(j1)
                  xfac=1.0
                  xb=xfac*(xic(j2)-xa)
                  yb=xfac*(yic(j2)-ya)
                  zb=xfac*(zic(j2)-za)
                  xd=xfac*(xic(j3)-xa)
                  yd=xfac*(yic(j3)-ya)
                  zd=xfac*(zic(j3)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv=xl+xa
                  yv=yl+ya
                  zv=zl+za
                  x23=0.5*(xic(j2)+xic(j3))
                  y23=0.5*(yic(j2)+yic(j3))
                  z23=0.5*(zic(j2)+zic(j3))
                  ds23=sqrt((xic(j3)-xic(j2))**2+
     *                      (yic(j3)-yic(j2))**2+
     *                      (zic(j3)-zic(j2))**2)
                  xarea=sqrt((xv-x23)**2+(yv-y23)**2+(zv-z23)**2)/ds23
                     if(nedge.gt.0) then
                        do l=1,nedge
                           lit=iedge_tet(l)
                           li=iedge_face(l)
                           l1=itet(itriface1(3,li),lit)
                           l2=itet(itriface1(1,li),lit)
                           l3=itet(itriface1(2,li),lit)
                           if((iparent(l2).eq.iparent(j2).and.
     *                         iparent(l3).eq.iparent(j3))
     *                         .or.
     *                        (iparent(l2).eq.iparent(j3).and.
     *                         iparent(l3).eq.iparent(j2))) then
C*****                              cx1=crosx1(j1,j2,j3)
C*****                              cy1=crosy1(j1,j2,j3)
C*****                              cz1=crosz1(j1,j2,j3)
C*****                              cx2=crosx1(l1,l2,l3)
C*****                              cy2=crosy1(l1,l2,l3)
C*****                              cz2=crosz1(l1,l2,l3)
C*****                              xdotc=cx1*cx2+cy1*cy2+cz1*cz2
C*****                              if(abs(1.0-xdotc).lt.1.0e-10) then
C*****                                 iedge_tet(l)=iedge_tet(nedge)
C*****                                 iedge_face(l)=iedge_face(nedge)
C*****                                 iedge_edge(l)=iedge_edge(nedge)
C*****                                 nedge=nedge-1
C*****                                 goto 5
C*****                              endif
                              xdotl=(xic(l2)-xic(l1))*(xic(l3)-xic(l1))+
     *                              (yic(l2)-yic(l1))*(yic(l3)-yic(l1))+
     *                              (zic(l2)-zic(l1))*(zic(l3)-zic(l1))
                              if(xdot.lt.xdotl) then
                                 iedge_tet(l)=it
                                 iedge_face(l)=i
                              endif
                              goto 5
                           endif
                        enddo
                     endif
                     j1=itet(itriface1(3,i),it)
                     j2=itet(itriface1(1,i),it)
                     j3=itet(itriface1(2,i),it)
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)-x1
                     y2=yic(j2)-y1
                     z2=zic(j2)-z1
                     x3=xic(j3)-x1
                     y3=yic(j3)-y1
                     z3=zic(j3)-z1
                     var1=zero
                     call int_edge(var1,var1,var1,
     *                             x2,y2,z2,
     *                             x3,y3,z3,
     *                             xint,yint,zint)
                     ds23=sqrt((x3-x2)**2+(y3-y2)**2+(z3-z2)**2)
                     ds2i=sqrt((xint-x2)**2+(yint-y2)**2+(zint-z2)**2)
                     ds3i=sqrt((xint-x3)**2+(yint-y3)**2+(zint-z3)**2)
c
c                    These three points are where the vertex that is being
c                    divided intersects perpenduclar to the opposite edge.
c
                     xint=xint+x1
                     yint=yint+y1
                     zint=zint+z1
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)
                     y2=yic(j2)
                     z2=zic(j2)
                     x3=xic(j3)
                     y3=yic(j3)
                     z3=zic(j3)
c
c                    Calculate the area of the original triangle and the two
c                    new triangles formed by the perpendicular intersection.
c
                     call volume_tri(x1,y1,z1,
     *                               x2,y2,z2,
     *                               x3,y3,z3,
     *                               voltri)
                     call volume_tri(x1,y1,z1,
     *                               x2,y2,z2,
     *                               xint,yint,zint,
     *                               voltri1)
                     call volume_tri(x1,y1,z1,
     *                               xint,yint,zint,
     *                               x3,y3,z3,
     *                               voltri2)
                     nedge=nedge+1
                     iedge_tet(nedge)=it
                     iedge_face(nedge)=i
c
c          If the area of the smaller of the two new triangles is less than
c          xfactri*original_area then bisect the edge, other wise uses the
c          perpendicular intersection. xfactri should be .le. 0.5.
c          xfactri = 0.5 will result in the edge always being bisected.
c
                     if(min(voltri1,voltri2).lt.xfactri*voltri) then
                        xedge(nedge)=min(voltri1,voltri2)/voltri
                     else
                        xedge(nedge)=0.0
                     endif
   5                 continue
                  endif
            endif
         enddo
      enddo
C
      if(nedge.le.0) then
         write(logmess,9000) cmo(1:icharlnf(cmo))
         call writloga('default',0,logmess,0,ierrwrt)
 9000    format("No edges for refine_edge_2d: ",a)
         ierr1=1
         goto 9999
      endif
C
C
      length=nedge
      call mmgetblk('int1add',isubname,ipint1add,length,2,icscode)
      do i=1,nedge
         int1add(i)=0
      enddo
C
      length=nedge
      call mmgetblk('list_sink',isubname,iplist_sink,length,2,icscode)
      length=nvalues*nedge
      call mmgetblk('list_source',isubname,iplist_source,length,2,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      nadd1=0
C
      iedgeiter=0
  10  continue
      iedgeiter=iedgeiter+1
      write(logmess,'(a,i10,i10)')
     *        'Edge iteration refine_edge_2d: ',iedgeiter,nedge
      call writloga('default',0,logmess,0,ierrw)
      do it=1,ntets
         itflag(it)=0
      enddo
      do it=1,ntets
         do i=1,3
            itetnn(i,it)=itet(i,it)
         enddo
         do i=1,3
            itetnn(i,it)=itet(i,it)
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/nef
               itetnn2(i,it)=jtet(i,it)-mbndry-nef*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/nef
               itetnn2(i,it)=jtet(i,it)-nef*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
      irefine=0
      npointsnew=npoints
      ntetsnew=ntets
      nedge_save=0
      do iedge=1,nedge
         it=iedge_tet(iedge)
         i=iedge_face(iedge)
         if(itflag(it).ne.0) then
            nedge_save=nedge_save+1
            iedge_tet(nedge_save)=it
            iedge_face(nedge_save)=i
            xedge(nedge_save)=xedge(iedge)
            goto 130
         endif
         if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
            jt=itetnn1(i,it)
            jf=itetnn2(i,it)
            if(itflag(jt).ne.0) then
               nedge_save=nedge_save+1
               iedge_tet(nedge_save)=it
               iedge_face(nedge_save)=i
               xedge(nedge_save)=xedge(iedge)
               goto 130
            endif
         else
            jt=-1
            jf=-1
         endif
         j1=itet(itriface1(3,i),it)
         j2=itet(itriface1(1,i),it)
         j3=itet(itriface1(2,i),it)
C
         if(xedge(iedge).eq.0.0) then
            x1=xic(j1)
            y1=yic(j1)
            z1=zic(j1)
            x2=xic(j2)-x1
            y2=yic(j2)-y1
            z2=zic(j2)-z1
            x3=xic(j3)-x1
            y3=yic(j3)-y1
            z3=zic(j3)-z1
            var1 = zero
            call int_edge(var1,var1,var1,x2,y2,z2,x3,y3,z3,xint,yint,
     *              zint)
            ds23=sqrt((x3-x2)**2+(y3-y2)**2+(z3-z2)**2)
            ds2i=sqrt((xint-x2)**2+(yint-y2)**2+(zint-z2)**2)
            ds3i=sqrt((xint-x3)**2+(yint-y3)**2+(zint-z3)**2)
            xint=xint+x1
            yint=yint+y1
            zint=zint+z1
         else
            xint=0.5*(xic(j2)+xic(j3))
            yint=0.5*(yic(j2)+yic(j3))
            zint=0.5*(zic(j2)+zic(j3))
         endif
         ds23=sqrt((xic(j3)-xic(j2))**2+
     *             (yic(j3)-yic(j2))**2+
     *             (zic(j3)-zic(j2))**2)
         ds2i=sqrt((xint-xic(j2))**2+
     *             (yint-yic(j2))**2+
     *             (zint-zic(j2))**2)
         ds3i=sqrt((xint-xic(j3))**2+
     *             (yint-yic(j3))**2+
     *             (zint-zic(j3))**2)
C
         itstart=it
         ifstart=i
         iestart=j
         itlast=itstart
         iflast=ifstart
         irefine=irefine+1
         call mmfindbk('xic',cmo,ipxic,length,icscode)
         if((npointsnew+1).gt.length) then
            npointsinc=npointsnew+1000
            call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
            call mmgetlen(ipitetclr,nelementsmm,icscode)
            call cmo_set_info('nelements',cmo,nelementsmm,1,1,ierror)
            call cmo_newlen(cmo,ierror)
            call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype
     *                  ,ierror)
            call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
            call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
            call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
         endif
         npointsnew=npointsnew+1
C
         ktlast=itstart
         kflast=ifstart
         i1=itet(itriface1(3,kflast),ktlast)
         i2=itet(itriface1(1,kflast),ktlast)
         i3=itet(itriface1(2,kflast),ktlast)
C
         iparent(npointsnew)=npointsnew
         if(int1(i2).eq.1.and.int1(i3).eq.1) then
            int1(npointsnew)=1
         else
            int1(npointsnew)=0
         endif
C
c     These arrays contain the node pairs for edge refinement
c     and the location on the edge to place the new point
c
         nadd1=nadd1+1
         int1add(nadd1)=int1(npointsnew)
         list_sink(nadd1)=npointsnew
         list_source(1,nadd1)=j2
         list_source(2,nadd1)=j3
         xweight_source(1,nadd1)=ds3i
         xweight_source(2,nadd1)=ds2i
C
         call mmgetlen(ipitetclr,length,icscode)
         if((ntetsnew+2).gt.length) then
            inc=1000
            ntetsinc=ntetsnew+inc
            call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
            call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
            call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
            call cmo_newlen(cmo,ierror)
            call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype
     *                  ,ierror)
            call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,lenitetclr,icmotype,ier)
            call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,lenitettyp,icmotype,ier)
            call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,lenitetoff,icmotype,ier)
            call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,lenjtetoff,icmotype,ier)
            call cmo_get_info('itet',cmo,
     *                        ipitet,lenitet,icmotype,ierror)
            call cmo_get_info('jtet',cmo,
     *                        ipjtet,lenjtet,icmotype,ierror)
         endif
         call mmgetlen(ipitflag,length,icscode)
         if((ntetsnew+2).gt.length) then
            inc=1000
            call mmgetnam(ipitflag,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                    ics)
            inc1=nen*inc
            call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                    ics)
            inc2=nef*inc
            call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                    ics)
            call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                    ics)
         endif
         ntetsnew=ntetsnew+1
         itflag(ntetsnew)=1
         itetclr(ntetsnew)=itetclr(ktlast)
         itettyp(ntetsnew)=itettyp(ktlast)
         itetoff(ntetsnew)=nen*(ntetsnew-1)
         jtetoff(ntetsnew)=nef*(ntetsnew-1)
         itetnn(1,ntetsnew)=i1
         itetnn(2,ntetsnew)=npointsnew
         itetnn(3,ntetsnew)=i3
         itetnn1(1,ntetsnew)=-1
         itetnn1(2,ntetsnew)=-1
         itetnn1(3,ntetsnew)=-1
         itetnn2(1,ntetsnew)=-1
         itetnn2(2,ntetsnew)=-1
         itetnn2(3,ntetsnew)=-1
         itflag(ktlast)=1
         itetnn(1,ktlast)=i1
         itetnn(2,ktlast)=i2
         itetnn(3,ktlast)=npointsnew
         itetnn1(1,ktlast)=-1
         itetnn1(2,ktlast)=-1
         itetnn1(3,ktlast)=-1
         itetnn2(1,ktlast)=-1
         itetnn2(2,ktlast)=-1
         itetnn2(3,ktlast)=-1
         if(jt.gt.0) then
            j1=itet(itriface1(3,jf),jt)
            j2=itet(itriface1(1,jf),jt)
            j3=itet(itriface1(2,jf),jt)
            ntetsnew=ntetsnew+1
            itflag(ntetsnew)=1
            itetclr(ntetsnew)=itetclr(jt)
            itettyp(ntetsnew)=itettyp(jt)
            itetoff(ntetsnew)=nen*(ntetsnew-1)
            jtetoff(ntetsnew)=nef*(ntetsnew-1)
            itetnn(1,ntetsnew)=j1
            itetnn(2,ntetsnew)=npointsnew
            itetnn(3,ntetsnew)=j3
            itetnn1(1,ntetsnew)=-1
            itetnn1(2,ntetsnew)=-1
            itetnn1(3,ntetsnew)=-1
            itetnn2(1,ntetsnew)=-1
            itetnn2(2,ntetsnew)=-1
            itetnn2(3,ntetsnew)=-1
            itflag(jt)=1
            itetnn(1,jt)=j1
            itetnn(2,jt)=j2
            itetnn(3,jt)=npointsnew
            itetnn1(1,jt)=-1
            itetnn1(2,jt)=-1
            itetnn1(3,jt)=-1
            itetnn2(1,jt)=-1
            itetnn2(2,jt)=-1
            itetnn2(3,jt)=-1
         endif
 130     continue
      enddo
      write(logmess,'(a,i10,a,i10)') 
     *'Edge-refined tris: before=',ntets,
     *                   ' after=',ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntetsnew
            do i=1,3
               itet(i,it)=itetnn(i,it)
            enddo
            do i=1,3
               itetnn(i,it)=iparent(itetnn(i,it))
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,3
                  kt=1+(jtet(i,it)-1)/nef
                  kf=jtet(i,it)-nef*(kt-1)
                  if(kt.gt.0.and.kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,3,3,ntets,npoints,
     *               2,npoints,ntets)
         do it=1,ntets
            do i=1,3
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  if(itetclr(it).eq.itetclr(itetnn1(i,it))) then
                     jtet(i,it)=nef*(itetnn1(i,it)-1)+itetnn2(i,it)
                  else
                     jtet(i,it)=mbndry+nef*(itetnn1(i,it)-1)+
     *                       itetnn2(i,it)
                  endif
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
      endif
      if(nedge_save.gt.0) then
         nedge=nedge_save
         goto 10
      endif
C
      cmolength='nnodes'
      call cmo_interpolate(cmo,cmo,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      length=npoints
      call mmnewlen("int1",isubname,ipint1,length,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      icount=0
      jcount=0
      do i=1,nadd1
         i1=list_sink(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         if(int1add(i).eq.1) then
            icount=icount+1
         endif
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('ipointj',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0.or.icount.gt.0) then
         call dotaskx3d('settets/parents ; finish',ierror)
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
      return
      end
*dk,refine2db
      subroutine refine2db()
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/x3d/src/refine_edge_2d.f_a  $
CPVCS
CPVCS       Rev 1.1   03/28/95 12:34:50   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:54   pvcs
CPVCS    Original version.
C
       implicit real*8 (a-h,o-z)
C
      include "local_element.h"
      include 'consts.h'
C
      character*132 logmess
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipint1, int1)
      integer itp1(1000000), isn1(1000000), int1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet(3,1000000), jtet(3,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      dimension xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      integer itetnn(3,1000000),
     *        itetnn1(3,1000000),
     *        itetnn2(3,1000000)
      pointer (ipiedge_tet, iedge_tet)
      pointer (ipiedge_face, iedge_face)
      pointer (ipiedge_edge, iedge_edge)
      integer iedge_tet(6*1000000), iedge_face(6*1000000),
     *        iedge_edge(6*1000000)
C
      parameter (nvalues=2)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(1000000), list_source(nvalues,1000000)
      real*8 xweight_source(nvalues,1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitlist, itlist)
      pointer (ipifadd, ifadd)
      integer itlist(1000000)
      integer itflag(1000000)
      integer ifadd(3,1000000)
C
      integer ieadd(3)
C
      pointer (ipint1add, int1add)
      integer int1add(1000000)
C
      character*32 isubname, iblknam, iprtnam
      character*32 cmo, cmolength
      integer itriface0(3), itriface1(3,3)
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
C
      data xarea_ref / 0.0d+00 /
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C
C ######################################################################
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
      isubname='refine2db'
      iedgeiter = 0
      nedge = 0
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
C
C     ******************************************************************
C
C     Get the parents for each node.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk('int1add',isubname,ipint1add,length,2,icscode)
      call mmgetblk('iparent',isubname,ipiparent,length,2,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=nef*ntets
      call mmgetblk('iedgetet',isubname,ipiedge_tet,length,2,icscode)
      call mmgetblk('iedgefac',isubname,ipiedge_face,length,2,icscode)
      call mmgetblk('iedgeedg',isubname,ipiedge_edge,length,2,icscode)
      length=nen*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,2,icscode)
      length=nef*ntets
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,2,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,2,icscode)
C
      write(logmess,'(a,i10,i10)')
     *     'Edge iteration refine2db: ',iedgeiter,nedge
      call writloga('default',0,logmess,0,ierrw)
C
      length=ntets
      call mmgetblk('itlist',isubname,ipitlist,length,2,icscode)
      call mmgetblk('itflag',isubname,ipitflag,length,2,icscode)
      length=nef*ntets
      call mmgetblk('ifadd',isubname,ipifadd,length,2,icscode)
C
      do it=1,ntets
         do i=1,nen
            itetnn(i,it)=itet(i,it)
         enddo
         do i=1,nelmnef(itettyp(it))
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/nef
               itetnn2(i,it)=jtet(i,it)-mbndry-nef*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/nef
               itetnn2(i,it)=jtet(i,it)-nef*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
C
      ntlist=0
      do it=1,ntets
         x1=xic(itet(1,it))
         y1=yic(itet(1,it))
         z1=zic(itet(1,it))
         x2=xic(itet(2,it))
         y2=yic(itet(2,it))
         z2=zic(itet(2,it))
         x3=xic(itet(3,it))
         y3=yic(itet(3,it))
         z3=zic(itet(3,it))
         xa=-crosx((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         ya=-crosy((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         za=-crosz((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         xarea=0.5*sqrt(xa**2+ya**2+za**2)
         if(xarea.ge.xarea_ref) then
            ntlist=ntlist+1
            itlist(ntlist)=it
         endif
      enddo
C
      length=6*ntets
      call mmgetblk('list_sink',isubname,iplist_sink,length,2,icscode)
      length=nvalues*6*ntets
      call mmgetblk('list_source',isubname,iplist_source,length,2,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      nadd1=0
C
      do it=1,ntets
         itflag(it)=0
         do i=1,nelmnef(ifelmtri)
            ifadd(i,it)=0
         enddo
      enddo
C
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      do i=1,ntlist
         it=itlist(i)
         if(itflag(it).eq.0) then
            itflag(it)=1
            nef1=nelmnef(ifelmtri)
            do j=1,nef1
               if(ifadd(j,it).eq.0) then
                  nadd1=nadd1+1
                  npointsnew=npointsnew+1
                  call mmfindbk('xic',cmo,ipxic,length,icscode)
                  if((npointsnew+1).gt.length) then
                     npointsinc=npointsnew+1000
                     call cmo_set_info('nnodes',cmo,npointsinc,1,1,ier)
                     call mmgetlen(ipitetclr,nelementsmm,icscode)
                     call cmo_set_info('nelements',cmo,
     *                                 nelementsmm,1,1,ier)
                     call cmo_newlen(cmo,ierror)
                     call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                  icmotype,ierror)
                     call mmnewlen('iparent',isubname,
     *                             ipiparent,npointsinc,icscode)
                     call mmnewlen('int1add',isubname,
     *                             ipint1add,npointsinc,icscode)
                  endif
                  i1=itet1(itetoff(it)+ielmface1(1,j,ifelmtri))
                  i2=itet1(itetoff(it)+ielmface1(2,j,ifelmtri))
                  list_sink(npointsnew-npoints)=npointsnew
                  list_source(1,npointsnew-npoints)=i1
                  list_source(2,npointsnew-npoints)=i2
                  xweight_source(1,npointsnew-npoints)=1.0
                  xweight_source(2,npointsnew-npoints)=1.0
                  ifadd(j,it)=npointsnew
                  if(jtet1(jtetoff(it)+j).gt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+j)-mbndry-1)/nef
                     jf=jtet1(jtetoff(it)+j)-mbndry-nef*(jt-1)
                     ifadd(jf,jt)=npointsnew
                     int1add(npointsnew-npoints)=1
                  elseif(jtet1(jtetoff(it)+j).gt.0.and.
     *                   jtet1(jtetoff(it)+j).lt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+j)-1)/nef
                     jf=jtet1(jtetoff(it)+j)-nef*(jt-1)
                     ifadd(jf,jt)=npointsnew
                     int1add(npointsnew-npoints)=0
                  endif
               endif
            enddo
         endif
      enddo
C
      do it=1,ntets
         icount=0
         do i=1,nelmnef(ifelmtri)
            if(ifadd(i,it).gt.0) then
               icount=icount+1
               ieadd(icount)=i
            endif
         enddo
         if(icount.gt.0) then
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+3).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype
     *                  ,ierror)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,lenitetclr,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,lenitettyp,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,lenitetoff,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,lenjtetoff,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,lenitet,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,lenjtet,icmotype,ierror)
            endif
            call mmgetlen(ipitflag,length,icscode)
            if((ntetsnew+3).gt.length) then
               inc=1000
               call mmgetnam(ipitflag,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       ics)
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       ics)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       ics)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       ics)
            endif
         endif
C
         if(icount.eq.3) then
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            i3=itet1(itetoff(it)+3)
            i23=ifadd(1,it)
            i31=ifadd(2,it)
            i12=ifadd(3,it)
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i12
            itetnn(2,it)=i23
            itetnn(3,it)=i31
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i12
               itetnn(2,ntetsnew)=i2
               itetnn(3,ntetsnew)=i23
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i23
               itetnn(2,ntetsnew)=i3
               itetnn(3,ntetsnew)=i31
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i31
               itetnn(2,ntetsnew)=i1
               itetnn(3,ntetsnew)=i12
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         elseif(icount.eq.2) then
            iedge1=ieadd(1)
            iedge2=ieadd(2)
            if(iedge1.eq.2.and.iedge2.eq.3) then
               iedge=1
               ie1=ifadd(2,it)
               ie2=ifadd(3,it)
            elseif(iedge1.eq.1.and.iedge2.eq.3) then
               iedge=2
               ie1=ifadd(1,it)
               ie2=ifadd(3,it)
            elseif(iedge1.eq.1.and.iedge2.eq.2) then
               iedge=3
               ie1=ifadd(1,it)
               ie2=ifadd(2,it)
            endif
            i2=itet1(itetoff(it)+ielmface1(1,iedge,ifelmtri))
            i3=itet1(itetoff(it)+ielmface1(2,iedge,ifelmtri))
            isum=itet1(itetoff(it)+1)+
     *           itet1(itetoff(it)+2)+
     *           itet1(itetoff(it)+3)
            i1=isum-i2-i3
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i1
            itetnn(2,it)=ie2
            itetnn(3,it)=ie1
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=ie2
               itetnn(2,ntetsnew)=i2
               itetnn(3,ntetsnew)=ie1
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i2
               itetnn(2,ntetsnew)=i3
               itetnn(3,ntetsnew)=ie1
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         elseif(icount.eq.1) then
            iedge=ieadd(1)
            ie1=ifadd(iedge,it)
            i2=itet1(itetoff(it)+ielmface1(1,iedge,ifelmtri))
            i3=itet1(itetoff(it)+ielmface1(2,iedge,ifelmtri))
            isum=itet1(itetoff(it)+1)+
     *           itet1(itetoff(it)+2)+
     *           itet1(itetoff(it)+3)
            i1=isum-i2-i3
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i1
            itetnn(2,it)=i2
            itetnn(3,it)=ie1
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i1
               itetnn(2,ntetsnew)=ie1
               itetnn(3,ntetsnew)=i3
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         endif
      enddo
      write(logmess,'(a,i10,a,i10)') 
     *   'Edge-refined tris: before=',ntets,
     *                      ' after=',ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,3
                  kt=1+(jtet(i,it)-1)/nef
                  kf=jtet(i,it)-nef*(kt-1)
                  if(kt.gt.0.and.kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,3,3,ntets,npoints,
     *               2,npoints,ntets)
         do it=1,ntets
            do i=1,3
               itet(i,it)=itetnn(i,it)
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  jtet(i,it)=nef*(itetnn1(i,it)-1)+itetnn2(i,it)
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
      endif
C
      goto 9999
 9999 continue
C
      cmolength='nnodes'
      call cmo_interpolate(cmo,cmo,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
C
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ier)
C
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      length=npoints
      call mmgetblk("int1",isubname,ipint1,length,1,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      icount=0
      jcount=0
      do i=1,nadd1
         i1=list_sink(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         if(int1add(i).eq.1) then
            icount=icount+1
         endif
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('ipointj',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0.or.icount.gt.0) then
         call dotaskx3d('settets/parents ; finish',ierror)
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      endif
C
      call mmrelprt(isubname,icscode)
      return
      end
