      subroutine refine_edge_3d(cmo,ierr1)
C
C     CHANGE HISTORY -
C
C $Log:   /pvcs.config/t3d/src/refine_edge_3d.f_a  $
CPVCS    
CPVCS       Rev 1.1   08 Feb 2006 14:38:18   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.0   21 Mar 2002 10:07:18   dcg

CPVCS    Initial revision.

C
      implicit real*8 (a-h,o-z)
C
      include 'local_element.h'
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
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipint1, int1)
      integer int1(1000000)
C
      pointer (ipitadd, itadd)
      pointer (ipieadd, ieadd)
      integer itadd(1000000), ieadd(1000000)
      pointer (ipip1, ip1)
      pointer (ipip2, ip2)
      integer ip1(1000000), ip2(1000000)
      pointer (ipiadd, iadd)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      integer iadd(1000000)
      real*8 xadd(1000000), yadd(1000000), zadd(1000000)
      pointer (ipxdot1, xdot1)
      real*8 xdot1(1000000)
C
      parameter (nvalues=2)
      real*8 atolerance
      parameter (atolerance=1.0d-10)
c
      character*32 isubname
      integer itriface0(3), itriface1(3,3)
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
C
C*****data xfactri / 0.333333333333333d+00 /
      data xfactri / 0.5d+00 /
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
C
      isubname='refine_edge_3d'
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
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
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
      call get_epsilon('epsilonl', epsilonl)
C
      length=nelmnee(ifelmtet)*ntets
      call mmgetblk('ip1',isubname,ipip1,length,1,icscode)
      call mmgetblk('ip2',isubname,ipip2,length,1,icscode)
      call mmgetblk('itadd',isubname,ipitadd,length,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,length,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,length,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,length,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,length,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,length,2,icscode)
      call mmgetblk('xdot1',isubname,ipxdot1,length,2,icscode)
      nadd=0
      do it=1,ntets
         do i=1,nelmnef(itettyp(it))
            iflag=0
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               iflag=1
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               iflag=1
            endif
            if(iflag.ne.0) then
               i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
               i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
               i3=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
               do j=1,ielmface0(i,itettyp(it))
                  ie=ielmface2(j,i,itettyp(it))
                  j2=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                  j3=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                  j1=i1+i2+i3-j2-j3
                  xdot=(xic(j2)-xic(j1))*(xic(j3)-xic(j1)) +
     *                 (yic(j2)-yic(j1))*(yic(j3)-yic(j1)) +
     *                 (zic(j2)-zic(j1))*(zic(j3)-zic(j1))
                  if(xdot.lt.-atolerance) then
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
     *                         (yic(j3)-yic(j2))**2+
     *                         (zic(j3)-zic(j2))**2)
                     xarea=sqrt((xv-x23)**2+(yv-y23)**2+(zv-z23)**2)/ds23
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)-x1
                     y2=yic(j2)-y1
                     z2=zic(j2)-z1
                     x3=xic(j3)-x1
                     y3=yic(j3)-y1
                     z3=zic(j3)-z1
                     call int_edge(zero,zero,zero,
     *                             x2,y2,z2,
     *                             x3,y3,z3,
     *                             xint,yint,zint)
                     ds23=sqrt((x3-x2)**2+(y3-y2)**2+(z3-z2)**2)
                     ds2i=sqrt((xint-x2)**2+(yint-y2)**2+(zint-z2)**2)
                     ds3i=sqrt((xint-x3)**2+(yint-y3)**2+(zint-z3)**2)
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
                     iflag1=0
                     do l=1,nadd
                        k2=iparent(ip1(l))
                        k3=iparent(ip2(l))
                        if((k2.eq.iparent(j2).and.
     *                         k3.eq.iparent(j3)) .or.
     *                     (k2.eq.iparent(j3).and.
     *                         k3.eq.iparent(j2))) then
                           iflag1=l
                           if(xdot.lt.xdot1(l)) then
                              itadd(l)=it
                              ieadd(l)=ie
                              iadd(l)=0
                              xadd(l)=xint
                              yadd(l)=yint
                              zadd(l)=zint
                              xdot1(l)=xdot
                           endif
                        endif
                     enddo
                     if(iflag1.eq.0) then
                        nadd=nadd+1
                        ip1(nadd)=j2
                        ip2(nadd)=j3
                        itadd(nadd)=it
                        ieadd(nadd)=ie
                        iadd(nadd)=0
                        xadd(nadd)=xint
                        yadd(nadd)=yint
                        zadd(nadd)=zint
                        xdot1(nadd)=xdot
                     endif
                  endif
               enddo
            endif
         enddo
      enddo
c
      if(nadd.gt.0) then
         ierr1=nadd
         call refine_edge_add(cmo,
     *                        nadd,ipitadd,ipieadd,
     *                        ipiadd,ipxadd,ipyadd,ipzadd)
      endif
c
      goto 9999
 9999 continue
C
      call dotaskx3d('dump/gmv/gmv_refine_edge_3d ; finish',ierror)
C
      call mmrelprt(isubname,icscode)
      return
      end
