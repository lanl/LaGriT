      subroutine shttstv(qx,qy,qz,npts,epsln,cmoin,ickin,
     &   isurftst)
C
C########################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE TESTS WHETHER A SET OF POINTS ARE LT, GT, OR EQ
C     (IN, OUT OR ON) A SHEET SURFACE.
C
C
C     INPUT ARGUMENTS -
C
C        qx - X COORDINATE OF THE POINTS TO CHECK
C        qy - Y COORDINATE OF THE POINTS TO CHECK
C        qz - Z COORDINATE OF THE POINTS TO CHECK
C        npts - NO. OF QUERY POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        cmoin - name of sheet cmo
C        ickin - TYPE OF CHECK TO PERFORM (lt, gt OR eq)
C
C
C     OUTPUT ARGUMENTS -
C
C        isurftst - TEST VALUE PER POINT (0-FALSE, 1-TRUE)
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/shttstv_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.6   29 Aug 2007 16:30:16   gable
CPVCS    Modified so that if the input region sheet MO is empty, code continues without
CPVCS    a mmgetblk crash.
CPVCS    
CPVCS       Rev 1.5   06 Apr 2004 09:30:54   dcg
CPVCS    use explicit lengths in comparisons using chkin
CPVCS    
CPVCS       Rev 1.4   28 Mar 2000 14:09:30   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.3   Thu Mar 09 08:57:44 2000   dcg
CPVCS    pass sheetname from surftst to shttst
CPVCS    
CPVCS       Rev 1.2   Thu Feb 03 09:00:34 2000   dcg
CPVCS    
CPVCS       Rev 1.1   04 Jan 2000 16:48:08   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.18   Tue Sep 22 13:45:34 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.17   Fri Oct 03 11:03:26 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.16   Wed Jun 25 16:25:28 1997   kuprat
CPVCS    Major revision.  We take the closest point from the list of
CPVCS    triangles returned by the kd-tree, and then we recover adjacent
CPVCS    triangles for the 'edge' and 'vertex' cases using the
CPVCS    JTET relation.
CPVCS
CPVCS       Rev 1.15   Tue Jun 24 23:47:12 1997   kuprat
CPVCS    Band-aid fix:  Instead of taking 'first' closest point of
CPVCS    sufficiently nearby triangles, take CLOSEST point overall.
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 17:01:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Wed Feb 28 18:12:22 1996   ahmed
CPVCS    fixed epsilon
CPVCS
CPVCS       Rev 1.12   Tue Feb 13 14:43:42 1996   ahmed
CPVCS    base the algorithm on k-D search
CPVCS
CPVCS       Rev 1.11   12/05/95 08:21:22   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.10   11/07/95 17:26:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   10/18/95 12:17:46   het
CPVCS    Allow greater that 8 character names for sheets in the surface
c command.
CPVCS
CPVCS       Rev 1.8   10/11/95 10:30:00   ahmed
CPVCS    implement new definitions of inside outside wrt. sheets
CPVCS
C
C
C########################################################################
C
      implicit none
C
      include "chydro.h"
      include "local_element.h"
C
      integer lenptr
      parameter (lenptr=1000000)
C
C########################################################################
C
      pointer (ipitfound, itfound)
      pointer (ipwork, work)
      pointer (ipickout, ickout)
C
      integer npts,ierror,nnodes,ilen,icmotype,nelements,
     &   lenmm2,lenmm4,icscode,j,nfound,it,itri,i1,i2,i3,i,itrimin,
     &   indx,iface,nedges,jadj,mbndry,ind,ncc,nc,ics
      real*8 epsln,cpx1,cpy1,cpz1,dmintri,x1,y1,z1,x2,y2,z2,x3,y3,
     &   z3,dcross,dist,ax,ay,az,bx,by,bz,denom,t,eps,xs,ys,zs,
     &   dedge,dface,el,area,vx,vy,vz,vnorm,top,bot,ang,a,b,c,test
      real*8 work(lenptr),tn(3)
      real*8 qx(npts),qy(npts),qz(npts)
      integer itfound(lenptr),isurftst(npts),
     &   node1
      character*2 ickout(lenptr)
      character*(*) ickin
      character*2 lt,le,gt,ge,eq,blank
C
C     SET POINTERS FOR THE SHEET cmo
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
 
c.... This algorithm is meant for triangular sheets.
      integer nen,nef
      parameter (nen=3,nef=3)
C
      real*8 xic(lenptr),yic(lenptr),zic(lenptr),sbox(lenptr)
      integer   linkt(lenptr),itet(3,lenptr),jtet(3,lenptr)
C
      character*32  cmoin
      character*32  isubname
      character*132 logmess
      character*8 cptype
      real*8 u1,v1,u2,v2,u3,v3,dbarea
      integer ii,next3,iprev3
      real*8 alargenumber
      data alargenumber/1.d+99/
C
C########################################################################
C
C     STATEMENT FUNCTION DBAREA GIVES DOUBLE THE AREA OF A TRIANGLE
C     ORDERED (COUNTERCLOCKWISE) 1,2,3 IN THE u-v PLANE. THIS MEANS
C     THAT FOR A TRIANGLE ORDERED 1,2,3 IN x-y-z SPACE, THE (R.H. RULE)
C     VECTOR NORMAL TO THIS TRIANGLE WITH MAGNITUDE EQUAL TO DOUBLE THE
C     AREA IS GIVEN BY ( dbarea(y1,z1,y2,z2,y3,z3),
C                        dbarea(z1,x1,z2,x2,z3,x3),
C                        dbarea(x1,y1,x2,y2,x3,y3) ).
 
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
C
      next3(ii)=mod(ii,3)+1
      iprev3(ii)=mod(ii+1,3)+1
C
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ierror = 0
C
      lt='lt'
      le='le'
      gt='gt'
      ge='ge'
      eq='eq'
      blank='  '
C
C     *******************************************************************
C     SET THE MEMORY MANAGED PARTITION NAME.
C
      isubname='shttstv'
C
C     *******************************************************************
C     use cmo NAME
C
C     *******************************************************************
C     GET THE SHEET DATA FROM THE cmo.
C
      call cmo_get_info('nnodes',cmoin,nnodes,ilen,icmotype,ierror)
      call cmo_get_info('nelements',cmoin,
     &   nelements,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoin,ipsbox,ilen,icmotype,ierror)
      call cmo_get_info('linkt',cmoin,iplinkt,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmoin,ipjtet,ilen,icmotype,ierror)
      call cmo_get_info('mbndry',cmoin,mbndry,ilen,icmotype,ierror)
C     ******************************************************************
C
         if((nnodes .eq. 0).or.(nelements .eq. 0))then
         if(nnodes .eq. 0)then
            write(logmess,'(a)')
     1       'SHTTSTV:WARNING Sheet mesh object has 0 nodes.'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         endif
         if(nelements .eq. 0)then
            write(logmess,'(a)')
     1       'SHTTSTV:WARNING Sheet mesh object has 0 elements.'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         endif
            write(logmess,'(a)')
     1       'SHTSTV:WARNING Exit with no action'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         go to 9999
         endif

C     ******************************************************************
C     GET MEMORY FOR LOCAL ARRAYS.
C
      lenmm2=nelements
      lenmm4=npts
C
      call mmgetblk('itfound',isubname,ipitfound,lenmm2,2,icscode)
      call mmgetblk('work',isubname,ipwork,lenmm2,2,icscode)
      call mmgetblk('ickout',isubname,ipickout,lenmm4,2,icscode)
C
C     ******************************************************************
C     LOOP THROUGH QUERY POINTS AND TEST isurftst FOR (0-FALSE, 1-TRUE).
C
      node1=itet(1,1)
      cpx1=xic(node1)
      cpy1=yic(node1)
      cpz1=zic(node1)
C
      do 100 j=1,npts
         ickout(j)='  '
C
C        ***************************************************************
C        GET A FEASIBLE SUBSET OF CLOSEST TRIANGLES (USING THE k-D TREE)
C        GUARANTEED TO CONTAIN THE NEAREST POINT TO THE QUERY POINT.
C
         call nearestpoint(qx(j),qy(j),qz(j),xic,yic,zic, itet,
     &      cpx1,cpy1,cpz1,linkt,sbox,epsln,work,
     &      nfound,itfound,ierror)
C
         if (nfound .eq. 0) then
            write(logmess,'(a)') 'Error in subroutine shttstv:
     &         kd tree returns no triangles'
            call writloga('default',0,logmess,0,ierror)
         endif
C
C            ***********************************************************
C            COMPUTE THE MIN DISTANCE FROM THE QUERY POINT TO EACH
C            SELECTED ELEMENT AND FIND THE CLOSEST POINT OVERALL.
C            ALSO FIND THE TYPE OF THE CLOSEST POINT (VERTEX, EDGE,
C            OR FACE.)
C
         dmintri=alargenumber
         do it=1,nfound
            itri=itfound(it)
C
C               ------------------------
C               COMPUTE TRIANGLE NORMAL.
C
            i1=itet(1,itri)
            i2=itet(2,itri)
            i3=itet(3,itri)
C
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            x3=xic(i3)
            y3=yic(i3)
            z3=zic(i3)
C
            tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
            tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
            tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
            dcross=sqrt( tn(1)*tn(1)+
     &         tn(2)*tn(2)+
     &         tn(3)*tn(3) )
            tn(1)=tn(1)/dcross
            tn(2)=tn(2)/dcross
            tn(3)=tn(3)/dcross
C
C               --------------------------------------------------------
C               FIRST CHECK ELEMENT VERTICES.
C
            do i=1,3
               ii=itet(i,itri)
C
               dist=sqrt( (qx(j)-xic(ii))*(qx(j)-xic(ii))+
     &            (qy(j)-yic(ii))*(qy(j)-yic(ii))+
     &            (qz(j)-zic(ii))*(qz(j)-zic(ii)) )
C
C               --------------------------------------------------------
C               TEST IF THE QUERY POINT LIES ON A VERTEX.
C
               if (dist.lt. epsln) then
                  ickout(j)=eq
                  cpx1=xic(ii)
                  cpy1=yic(ii)
                  cpz1=zic(ii)
                  goto 8888
               endif
C
               if (dist.lt. dmintri) then
                  itrimin=itri
                  dmintri=dist
                  cpx1=xic(ii)
                  cpy1=yic(ii)
                  cpz1=zic(ii)
                  cptype='vertex'
                  indx=i
               endif
            enddo
C
C               --------------------------------------------------------
C               SECOND CHECK ELEMENT EDGES.
C
            nedges=0
            do i=1,3
               i1=itet(next3(i),itri)
               i2=itet(iprev3(i),itri)
               ax=xic(i2)-xic(i1)
               ay=yic(i2)-yic(i1)
               az=zic(i2)-zic(i1)
               bx=qx(j)-xic(i1)
               by=qy(j)-yic(i1)
               bz=qz(j)-zic(i1)
               denom=ax*ax+ay*ay+az*az
               t=(ax*bx+ay*by+az*bz)/denom
               eps=2.0*epsln/sqrt(denom)
               if (t .gt. eps .and. t .lt. 1.0-eps) then
                  nedges=nedges+1
                  xs=t*xic(i2)+(1.0-t)*xic(i1)
                  ys=t*yic(i2)+(1.0-t)*yic(i1)
                  zs=t*zic(i2)+(1.0-t)*zic(i1)
                  dedge=sqrt( (qx(j)-xs)*(qx(j)-xs)+
     &               (qy(j)-ys)*(qy(j)-ys)+
     &               (qz(j)-zs)*(qz(j)-zs) )
C
C                  -----------------------------------------------------
C                  TEST IF THE QUERY POINT LIES ON A EDGE.
C
                  if (dedge .lt. epsln) then
                     ickout(j)=eq
                     cpx1=xs
                     cpy1=ys
                     cpz1=zs
                     goto 8888
                  endif
C
                  if (dedge .lt. dmintri) then
                     itrimin=itri
                     dmintri=dedge
                     cpx1=xs
                     cpy1=ys
                     cpz1=zs
                     cptype='edge'
                     indx=i
                  endif
               endif
            enddo
C
C               --------------------------------------------------------
C               FINALLY CHECK THE FACES OF ELEMENTS IF NEEDED.
C
            if (nedges .gt. 1) then
               i1=itet(1,itri)
               i2=itet(2,itri)
               i3=itet(3,itri)
C
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
C
               iface=1
               dface=(qx(j)-x1)*tn(1)+(qy(j)-y1)*tn(2)+
     &            (qz(j)-z1)*tn(3)
C
               if (abs(dface) .lt. dmintri) then
                  xs=qx(j)-tn(1)*dface
                  ys=qy(j)-tn(2)*dface
                  zs=qz(j)-tn(3)*dface
C
                  do i=1,3
                     i1=itet(i       ,itri)
                     i2=itet(next3(i),itri)
                     x1=xic(i1)
                     y1=yic(i1)
                     z1=zic(i1)
                     x2=xic(i2)
                     y2=yic(i2)
                     z2=zic(i2)
                     el=sqrt( (x2-x1)*(x2-x1)+
     &                  (y2-y1)*(y2-y1)+
     &                  (z2-z1)*(z2-z1) )
                     area=dbarea(y1,z1,y2,z2,ys,zs)*tn(1)+
     &                  dbarea(z1,x1,z2,x2,zs,xs)*tn(2)+
     &                  dbarea(x1,y1,x2,y2,xs,ys)*tn(3)
                     if (area .le. 2.0*el*epsln) iface=0
                  enddo
                  if (iface .eq. 1) then
C
C                     --------------------------------------------------
C                     TEST IF THE QUERY POINT LIES ON A TRIANGLE'S FACE.
C
                     if (abs(dface) .lt. epsln) then
                        ickout(j)=eq
                        cpx1=xs
                        cpy1=ys
                        cpz1=zs
                        goto 8888
                     endif
C
                     itrimin=itri
                     dmintri=abs(dface)
                     cpx1=xs
                     cpy1=ys
                     cpz1=zs
                     cptype='face'
                  endif
               endif
            endif
         enddo
C
C            ***********************************************************
C            TEST THE QUERY POINT FOR INSIDE, ON, AND OUTSIDE.
C
         if (dmintri .lt. epsln) ickout(j)=eq
         if (ickout(j) .eq. '  ') then
C
C           ************************************************************
C           COMPUTE THE "SYNTHETIC" NORMAL (VX,VY,VZ) AT THE NEAREST POINT. NEAREST
C           POINT COULD LIE ON A 'VERTEX', 'EDGE', OR A 'FACE'.
C
            if (cptype.eq.'face') then
 
               itri=itrimin
 
c...(Re)compute the triangle normal of the nearest triangle and
c...assign it to the normal (VX,VY,VZ) used for testing.
 
               i1=itet(1,itri)
               i2=itet(2,itri)
               i3=itet(3,itri)
C
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
C
               tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
               tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
               tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
               dcross=sqrt( tn(1)*tn(1)+
     &            tn(2)*tn(2)+
     &            tn(3)*tn(3) )
               tn(1)=tn(1)/dcross
               tn(2)=tn(2)/dcross
               tn(3)=tn(3)/dcross
 
               vx=tn(1)
               vy=tn(2)
               vz=tn(3)
 
            elseif (cptype.eq.'edge') then
               itri=itrimin
 
c...(Re)compute the triangle normal of the nearest triangle and
c...assign it to the normal (VX,VY,VZ) used for testing.
 
               i1=itet(1,itri)
               i2=itet(2,itri)
               i3=itet(3,itri)
C
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
C
               tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
               tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
               tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
               dcross=sqrt( tn(1)*tn(1)+
     &            tn(2)*tn(2)+
     &            tn(3)*tn(3) )
               tn(1)=tn(1)/dcross
               tn(2)=tn(2)/dcross
               tn(3)=tn(3)/dcross
 
               vx=tn(1)
               vy=tn(2)
               vz=tn(3)
 
c.... If there is a triangle across the edge, compute its normal and
c.... add in contribution.
 
               jadj=jtet(indx,itrimin)
               if (jadj.lt.mbndry) then
                  itri=(jadj-1)/nef+1
 
                  i1=itet(1,itri)
                  i2=itet(2,itri)
                  i3=itet(3,itri)
C
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
C
                  tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
                  tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
                  tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
                  dcross=sqrt( tn(1)*tn(1)+
     &               tn(2)*tn(2)+
     &               tn(3)*tn(3) )
                  tn(1)=tn(1)/dcross
                  tn(2)=tn(2)/dcross
                  tn(3)=tn(3)/dcross
 
c.... Add this normal to the testing normal and normalize.
 
                  vx=vx+tn(1)
                  vy=vy+tn(2)
                  vz=vz+tn(3)
 
                  vnorm=max(epsln,sqrt(vx**2+vy**2+vz**2))
 
                  vx=vx/vnorm
                  vy=vy/vnorm
                  vz=vz/vnorm
               endif
            elseif( cptype .eq. 'vertex') then
 
               itri=itrimin
 
c... (Re)compute the triangle normal of the nearest triangle and
c... assign it to the normal (VX,VY,VZ) used for testing, weighted
c... by face angle ANG.
 
               i1=itet(indx,itri)
               i2=itet(next3(indx),itri)
               i3=itet(iprev3(indx),itri)
C
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
C
               tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
               tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
               tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
               dcross=sqrt( tn(1)*tn(1)+
     &            tn(2)*tn(2)+
     &            tn(3)*tn(3) )
               tn(1)=tn(1)/dcross
               tn(2)=tn(2)/dcross
               tn(3)=tn(3)/dcross
               top=(x2-x1)*(x3-x1)+(y2-y1)*(y3-y1)+(z2-z1)*(z3-z1)
               bot=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)*
     &            sqrt((x3-x1)**2+(y3-y1)**2+(z3-z1)**2)
               if (bot.lt.epsln**2) then
                  ang=0.
               else
                  ang=acos(top/bot)
               endif
               vx=ang*tn(1)
               vy=ang*tn(2)
               vz=ang*tn(3)
 
               ind=next3(indx)
               jadj=jtet(ind,itrimin)
               ncc=0
 
c.... For each triangle in the counter-clockwise direction,
c.... add in angle-weighted triangle normal.
 
               do while(jadj.lt.mbndry.and.ncc.lt.1000)
                  itri=(jadj-1)/nef+1
                  ind=mod(jadj-1,nef)+1
                  if (itri.eq.itrimin) goto 20
                  ncc=ncc+1
                  i1=itet(next3(ind),itri)
                  i2=itet(iprev3(ind),itri)
                  i3=itet(ind,itri)
C
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
C
                  tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
                  tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
                  tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
                  dcross=sqrt( tn(1)*tn(1)+
     &               tn(2)*tn(2)+
     &               tn(3)*tn(3) )
                  tn(1)=tn(1)/dcross
                  tn(2)=tn(2)/dcross
                  tn(3)=tn(3)/dcross
                  top=(x2-x1)*(x3-x1)+(y2-y1)*(y3-y1)+(z2-z1)*(z3-z1)
                  bot=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)*
     &               sqrt((x3-x1)**2+(y3-y1)**2+(z3-z1)**2)
                  if (bot.lt.epsln**2) then
                     ang=0.
                  else
                     ang=acos(top/bot)
                  endif
                  vx=vx+ang*tn(1)
                  vy=vy+ang*tn(2)
                  vz=vz+ang*tn(3)
 
                  ind=iprev3(ind)
                  jadj=jtet(ind,itri)
               enddo
 
               if (ncc.ge.1000) then
                  print*,'Corrupted topology at triangle ',itrimin,
     &               '; local vertex',indx
               endif
 
               ind=iprev3(indx)
               jadj=jtet(ind,itrimin)
               nc=0
 
c.... For each triangle in the clockwise direction,
c.... add in angle-weighted triangle normal.
 
               do while(jadj.lt.mbndry.and.nc.lt.1000)
                  itri=(jadj-1)/nef+1
                  ind=mod(jadj-1,nef)+1
 
c.... If we 'caught our tail' here, we should have caught it going
c.... counter-clockwise, so topology is corrupt.
                  if (itri.eq.itrimin) then
                     print*,'Corrupted topology at triangle ',itrimin,
     &                  '; local vertex',indx
                     goto 20
                  endif
 
                  nc=nc+1
                  i1=itet(iprev3(ind),itri)
                  i2=itet(ind,itri)
                  i3=itet(next3(ind),itri)
C
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
C
                  tn(1)=dbarea(y1,z1,y2,z2,y3,z3)
                  tn(2)=dbarea(z1,x1,z2,x2,z3,x3)
                  tn(3)=dbarea(x1,y1,x2,y2,x3,y3)
                  dcross=sqrt( tn(1)*tn(1)+
     &               tn(2)*tn(2)+
     &               tn(3)*tn(3) )
                  tn(1)=tn(1)/dcross
                  tn(2)=tn(2)/dcross
                  tn(3)=tn(3)/dcross
                  top=(x2-x1)*(x3-x1)+(y2-y1)*(y3-y1)+(z2-z1)*(z3-z1)
                  bot=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)*
     &               sqrt((x3-x1)**2+(y3-y1)**2+(z3-z1)**2)
                  if (bot.lt.epsln**2) then
                     ang=0.
                  else
                     ang=acos(top/bot)
                  endif
                  vx=vx+ang*tn(1)
                  vy=vy+ang*tn(2)
                  vz=vz+ang*tn(3)
 
                  ind=next3(ind)
                  jadj=jtet(ind,itri)
               enddo
 
               if (nc.ge.1000) then
                  print*,'Corrupted topology at triangle ',itrimin,
     &               '; local vertex',indx
               endif
 
 20            continue
 
               vnorm=max(epsln,sqrt(vx**2+vy**2+vz**2))
 
               vx=vx/vnorm
               vy=vy/vnorm
               vz=vz/vnorm
 
            else
               write(logmess,'(a)')
     &            'Error in subroutine shttstv:
     &            Nearest point type not defined'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               go to 9999
            endif
 
            a=qx(j)-cpx1
            b=qy(j)-cpy1
            c=qz(j)-cpz1
            test=a*vx+b*vy+c*vz
            if (test .lt. 0.0) then
               ickout(j)=lt
            else
               ickout(j)=gt
            endif
         endif
C
C        ****************************************************************
C        SET RETURN ARRAY AND FILL IN THOSE THAT HAVE NOT BEEN
C        DETERMINED AS gt.
C
         goto 8888
 8888    continue
C
         if (ickout(j) .eq. blank) ickout(j)=gt
         if ((ickin(1:2) .eq. le) .and.
     &      (ickout(j) .eq. lt .or. 
     &       ickout(j) .eq. eq)) ickout(j)=le
         if ((ickin(1:2) .eq. ge) .and.
     &      (ickout(j) .eq. gt .or. 
     &       ickout(j) .eq. eq)) ickout(j)=ge
         if (ickin(1:2) .eq. ickout(j)) then
            isurftst(j)=1
         else
            isurftst(j)=0
         endif
C
 100  continue
C
      goto 9999
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,ics)
C
C     *******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT.
C
      return
      end
