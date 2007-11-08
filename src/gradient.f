      subroutine gradient(coption1,coption2,
     *                    inclusive,
     *                    npoints,ntets,
     *                    ipxfield,interp,
     *                    xrefine,
     *                    mpno,ipmpary,
     *                    nadd,ipitadd,ipifadd,ipieadd)
C
C#######################################################################
C
C     PURPOSE - To indicate which elements should be refined based on
C               conditions.
C
C
C     INPUT ARGUMENTS -
C          cmo - mesh object name
C          coption1 - refinement type (e.g. face, edge, element)
C          coption2 - refinement criterion (e.g. maxsize,aspect)
C          inclusive - should node set range be considered in
C                      and inclusive or exclusive sense - i.e.
C                      if inclusive criterion will be met if
C                      satisfied by at least one node in range
C          npoints  -  number of nodes in the mesh
C          ntets    -  number of elements in the mesh
C          ipxfield -  pointer to attribute field to be used in tests
C          interp   -  should attribute field be converted using log or
C                      asinh
C          xrefine  -  values to compare against(array of 3 ,x,y,z)
C          mpno     -  number of nodes in range
C          ipmpary  -  pointer to array of node number in range
C
C     OUTPUT ARGUMENTS -
C
C          nadd     -  number of nodes to be added
C          ipitadd  -  pointer to array of elements to be refined
C          ipifadd  -  pointer to array of local faces to be refined
C          ipieadd  -  pointer to array of local edges to be refined
C
C     CHANGE HISTORY - 11/26/94 algorithm changes by D. Kilcrease.
C
C        $Log:   /pvcs.config/t3d/src/gradient.f_a  $
CPVCS    
CPVCS       Rev 1.14   14 Mar 2001 14:19:32   dcg
CPVCS    get rid of upper case
CPVCS    move data statements 
CPVCS
CPVCS       Rev 1.13   17 Oct 2000 10:25:16   dcg
CPVCS    fix error in pic reference - is node based variable
CPVCS    and must be indexed by itet(itetoff)+face
CPVCS
CPVCS       Rev 1.12   21 Jun 2000 11:44:10   dcg
CPVCS    don't pass cmo name - get active cmo name in gradient
CPVCS
CPVCS       Rev 1.11   20 Jun 2000 10:46:06   dcg
CPVCS    fix bug in getting mins and maxs
CPVCS
CPVCS       Rev 1.9   Fri Jun 19 09:40:04 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.8   Wed Sep 24 15:10:00 1997   dcg
CPVCS    make implicit none, fix calling list for intrp.
CPVCS
CPVCS       Rev 1.7   Wed Jul 16 16:20:54 1997   dcg
CPVCS    set face number for max,minsize face refinement for 2d meshes
CPVCS    note face for 2d refinement is test of volume of element
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:50:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Sun Feb 23 10:34:40 1997   het
CPVCS    Add the refinement for triangles.
CPVCS
CPVCS       Rev 1.4   Fri Mar 29 13:18:24 1996   dpk
CPVCS    lambda_refine/edge changed to exclude cases where field does not change along edge.
CPVCS
CPVCS       Rev 1.3   Thu Feb 29 09:22:10 1996   dpk
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu Feb 01 01:43:54 1996   het
CPVCS    Fix an error with selecting exclusive edges
CPVCS
CPVCS       Rev 1.1   Wed Jan 31 16:49:12 1996   dpk
CPVCS    Bug fixed in delta/edge lambda_refine/edge and lambda_derefine/edge
CPVCS    involving the edge do loop.    DPK
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 14:39:28 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      integer nen,ilen,itype,icscode,nef,nee,nsd,
     *  ierr,ityp,length,ntets,i,mpno,npoints,i1,
     *  it,i2,i3,i4,isum,icount,ierror,nadd,
     *  isum1,isum2,j,jcount,kcount,ibinoff,ie,
     *  inclusive,interp,ierrwrt,isum12
      integer icharlnf
      real*8 xzero,xhalf,xone,epsilon,xl1,yl1,zl1,xl2,
     *  yl2,zl2,xl3,yl3,zl3,xn1,yn1,zn1,ax4,ay4,az4,
     *  ax1,ay1,az1,ax2,ay2,az2,ds,
     *  xn3,yn3,zn3,ax3,ay3,az3,xl4,yl4,
     *  zl4,xmin,ymin,zmin,xmax,ymax,zmax,pmax,
     *  dsmax,pmin,xsum,pavg,xgradref,dpmax,
     *  p1,p2,dis,area,xb,yb,zb,xc,yc,zc,xd,yd,zd,
     *  xn,yn,zn,x2,y2,z2,q,xa,ya,za,dvor,qvor2,rcir,
     *  ac1,bc1,cc1,dc1,ac2,bc2,cc2,dc2,ac3,bc3,cc3,
     *  dc3,ac4,bc4,cc4,dc4,dn1,dn2,dn3,dn4,
     *  a11,a12,a13,d1,a21,a22,a23,d3,a31,a32,
     *  a33,qdet,rx,ry,rz,rinsc,srat
      real*8 sa,sb,sc,farea,cirrat,afx1,afy1,afz1,
     *  afx2, afy2,afz2, afx3,afy3,afz3,xmag1,
     *  xmag2,xmag3,afmag1,afmag2,afmag3,
     *  xgradmag,xscale,xlam,xn2,yn2,zn2,
     *  pdif,dz3,dy3,dx3,dz2,dy2,dx2,dz1,dy1,dx1,p3,d2
C
      include 'local_element.h'
C
      character*132 logmess
C
      character*32 cmo, coption1, coption2
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipxfield, xfield)
      real*8 xic(npoints), yic(npoints), zic(npoints),
     *       xfield(npoints)
      pointer (ipitadd, itadd)
      pointer (ipifadd, ifadd)
      pointer (ipieadd, ieadd)
      integer itadd(*), ifadd(*), ieadd(*)
C
      real*8 xrefine(3)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
C
      pointer (ipmpary, mpary(mpno))
      integer mpary
C
C#######################################################################
C
      pointer (ipdelxb, delxb)
      pointer (ipdelyb, delyb)
      pointer (ipdelzb, delzb)
      pointer (ipax, ax)
      pointer (ipay, ay)
      pointer (ipaz, az)
      pointer (ipdpdx, dpdx)
      pointer (ipdpdy, dpdy)
      pointer (ipdpdz, dpdz)
      pointer (ipvoltet, voltet)
      pointer (ipxgradtest, xgradtest)
      real*8 delxb(*), delyb(*), delzb(*)
      real*8 ax(*), ay(*), az(*)
      real*8 dpdx(*), dpdy(*), dpdz(*)
      real*8 voltet(*), xgradtest(*)
C
      pointer (ipitestp, itestp(*))
      integer itestp
      pointer (ippic, pic(*))
      real*8 pic
      real*8 xv(10),yv(10),zv(10)
C
      character*32 isubname
C
      character*32 cmotable, ctable
C
      pointer (ipxavg, xavg)
      pointer (ipyavg, yavg)
      pointer (ipzavg, zavg)
      pointer (ipvalue, value)
      real*8 xavg(*), yavg(*), zavg(*), value(*)
C
      pointer (ipiedge, iedge)
      integer iedge(10000000)
C
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      pointer (ipiedge_i0, iedge_i0 )
      pointer (ipiedge_i1, iedge_i1 )
      pointer (ipiedge_i2, iedge_i2 )
      integer nedge_bin(*), nedge_off(*)
      integer iedge_i0(*), iedge_i1(*), iedge_i2(*)
C
C
      logical linclusive
C
C
C#######################################################################
C
C
C#######################################################################
C
C     DEFINE THE STRUCTURE OF A GENERIC TETRAHEDRON.
C
C
C                     i4 [itet(4,it)]
C                            $
C                           **  *
C                          * *     *
C                         *  *       *
C                        *   *         *
C                       *    *           * b34
C                  b41 *     *             *
C                     *      *               *
C                    *       *                 *
C                   *        * b24               *
C                  *         *                     *
C                 *          *    b31     _   _   _  $
C            i1  $_   _  -   * -   -   -          *   i3 [itet(3,it)]
C    [itet(1,it)] *          *                *
C                   *        *             *
C                 b21 *      *         *    b23
C                       *    *     *
C                         *  *  *
C                            $
C                     i2 [itet(2,it)]
C
C
C     ******************************************************************
C
      real *8 x1,x3,y1,y3,qx1,qx2,qx3,qy1,qy2,qy3,qz1,qz2,qz3,
     *  z1,z3
      real *8 a,d,triarea,xinterpolate
      data xzero, xhalf, xone / 0.0d+00, 0.5d+00, 1.0d+00 /
      data epsilon / 1.0d-06 /
C  MACROS.
C
      a(x1,x2,x3,y1,y2,y3) = x1*(y2-y3) - y1*(x2-x3) + x2*y3 - y2*x3
      d(x1,x2,x3,y1,y2,y3,z1,z2,z3) = y1*(x2*z3 - z2*x3)
     &         - x1*(y2*z3 - z2*y3) - z1*(x2*y3 - y2*x3)
C   statement function for the area of a triangle:
C
      triarea(qx1,qy1,qz1,qx2,qy2,qz2,qx3,qy3,qz3) =
     *  0.5*sqrt(
     *  ((qz2-qz1)*(qy3-qy1)-(qy2-qy1)*(qz3-qz1)) *
     *  ((qz2-qz1)*(qy3-qy1)-(qy2-qy1)*(qz3-qz1)) +
     *  ((qx2-qx1)*(qz3-qz1)-(qz2-qz1)*(qx3-qx1)) *
     *  ((qx2-qx1)*(qz3-qz1)-(qz2-qz1)*(qx3-qx1)) +
     *  ((qy2-qy1)*(qx3-qx1)-(qx2-qx1)*(qy3-qy1)) *
     *  ((qy2-qy1)*(qx3-qx1)-(qx2-qx1)*(qy3-qy1)) )
C
C
      isubname = 'gradient'
C
      call cmo_get_name(cmo,icscode)
      call cmo_get_intinfo('nodes_per_element',cmo,nen,ilen,itype,
     *  icscode)
      call cmo_get_intinfo('faces_per_element',cmo,nef,ilen,itype,
     *  icscode)
      call cmo_get_intinfo('edges_per_element',cmo,nee,ilen,itype,
     *  icscode)
      call cmo_get_intinfo('ndimensions_topo',cmo,nsd,ilen,itype,
     *  icscode)
C
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
C
C     ...............................................................
C     ALLOCATE TEMPORARY MEMORY
      length = nen*ntets
      call mmgetblk("ax",isubname,ipax,length,2,icscode)
      call mmgetblk("ay",isubname,ipay,length,2,icscode)
      call mmgetblk("az",isubname,ipaz,length,2,icscode)
      call mmgetblk("dpdx",isubname,ipdpdx,length,2,icscode)
      call mmgetblk("dpdy",isubname,ipdpdy,length,2,icscode)
      call mmgetblk("dpdz",isubname,ipdpdz,length,2,icscode)
      call mmgetblk("voltet",isubname,ipvoltet,length,2,icscode)
      call mmgetblk("xgradtest",isubname,ipxgradtest,length,2,
     *    icscode)
      length = nee*ntets
      call mmgetblk("delxb",isubname,ipdelxb,length,2,icscode)
      call mmgetblk("delyb",isubname,ipdelyb,length,2,icscode)
      call mmgetblk("delzb",isubname,ipdelzb,length,2,icscode)
C
      length = npoints
      call mmgetblk('itestp',isubname,ipitestp,length,2,icscode)
      do i=1,npoints
         itestp(i) = 0
      enddo
      do i=1,mpno
         i1 = mpary(i)
         itestp(i1) = 1
      enddo
C
C     *****************************************************************
C     INTERPOLATE THE FIELD
C
      length = npoints
      call mmgetblk('pic',isubname,ippic,length,2,icscode)
      do i1=1,npoints
         pic(i1) = xinterpolate(1,interp,xfield(i1))
      enddo
C
C     *****************************************************************
C     NOW COLLECT THE DATA INTO LOCAL ARRAYS.
C
 
      do it=1,ntets
         do i=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
            delxb(itetoff(it)+i) = xic(i2)-xic(i1)
            delyb(itetoff(it)+i) = yic(i2)-yic(i1)
            delzb(itetoff(it)+i) = zic(i2)-zic(i1)
         enddo
      enddo
C
C     _______________________________________________________________
C
C     COMPUTE VECTOR AREAS OF THE FOUR FACES OF THE TETRAHEDRON.
C     THE FIRST INDEX DENOTES THE FACE OPPOSITE THAT VERTEX.
C     THE NORMALS OF THESE AREAS POINT OUTWARDS (AWAY FROM THE
C     TETRAHEDRON).
C
      do it=1,ntets
         if(itettyp(it).eq.ifelmtri) then
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            i3=itet1(itetoff(it)+3)
            xl1=xic(i1)
            yl1=yic(i1)
            zl1=zic(i1)
            xl2=xic(i2)
            yl2=yic(i2)
            zl2=zic(i2)
            xl3=xic(i3)
            yl3=yic(i3)
            zl3=zic(i3)
            ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
            ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
            az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
            voltet(it)=0.5d+00*sqrt(ax4**2+ay4**2+az4**2)
            xn1=  (yl3-yl2)*az4-(zl3-zl2)*ay4
            yn1=-((xl3-xl2)*az4-(zl3-zl2)*ax4)
            zn1=  (xl3-xl2)*ay4-(yl3-yl2)*ax4
            xn=sqrt(xn1**2+yn1**2+zn1**2)
            ds=sqrt((xl3-xl2)**2+(yl3-yl2)**2+(zl3-zl2)**2)
            ax1=ds*xn1
            ay1=ds*yn1
            az1=ds*zn1
            xn2=  (yl1-yl3)*az4-(zl1-zl3)*ay4
            yn2=-((xl1-xl3)*az4-(zl1-zl3)*ax4)
            zn2=  (xl1-xl3)*ay4-(yl1-yl3)*ax4
            xn=sqrt(xn2**2+yn2**2+zn2**2)
            ds=sqrt((xl1-xl3)**2+(yl1-yl3)**2+(zl1-zl3)**2)
            ax2=ds*xn2
            ay2=ds*yn2
            az2=ds*zn2
            xn3=  (yl2-yl1)*az4-(zl2-zl1)*ay4
            yn3=-((xl2-xl1)*az4-(zl2-zl1)*ax4)
            zn3=  (xl2-xl1)*ay4-(yl2-yl1)*ax4
            xn=sqrt(xn3**2+yn3**2+zn3**2)
            ds=sqrt((xl2-xl1)**2+(yl2-yl1)**2+(zl2-zl1)**2)
            ax3=ds*xn3
            ay3=ds*yn3
            az3=ds*zn3
            ax(nelmnef(itettyp(it))*(it-1)+1) = ax1
            ay(nelmnef(itettyp(it))*(it-1)+1) = ay1
            az(nelmnef(itettyp(it))*(it-1)+1) = az1
            ax(nelmnef(itettyp(it))*(it-1)+2) = ax2
            ay(nelmnef(itettyp(it))*(it-1)+2) = ay2
            az(nelmnef(itettyp(it))*(it-1)+2) = az2
            ax(nelmnef(itettyp(it))*(it-1)+3) = ax3
            ay(nelmnef(itettyp(it))*(it-1)+3) = ay3
            az(nelmnef(itettyp(it))*(it-1)+3) = az3
         elseif(itettyp(it).eq.ifelmtet) then
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            i3=itet1(itetoff(it)+3)
            i4=itet1(itetoff(it)+4)
            xl1=xic(i1)
            yl1=yic(i1)
            zl1=zic(i1)
            xl2=xic(i2)
            yl2=yic(i2)
            zl2=zic(i2)
            xl3=xic(i3)
            yl3=yic(i3)
            zl3=zic(i3)
            xl4=xic(i4)
            yl4=yic(i4)
            zl4=zic(i4)
            ax1=  (yl3-yl2)*(zl4-zl2)-(zl3-zl2)*(yl4-yl2)
            ay1=-((xl3-xl2)*(zl4-zl2)-(zl3-zl2)*(xl4-xl2))
            az1=  (xl3-xl2)*(yl4-yl2)-(yl3-yl2)*(xl4-xl2)
            ax2=  (yl4-yl1)*(zl3-zl1)-(zl4-zl1)*(yl3-yl1)
            ay2=-((xl4-xl1)*(zl3-zl1)-(zl4-zl1)*(xl3-xl1))
            az2=  (xl4-xl1)*(yl3-yl1)-(yl4-yl1)*(xl3-xl1)
            ax3=  (yl2-yl1)*(zl4-zl1)-(zl2-zl1)*(yl4-yl1)
            ay3=-((xl2-xl1)*(zl4-zl1)-(zl2-zl1)*(xl4-xl1))
            az3=  (xl2-xl1)*(yl4-yl1)-(yl2-yl1)*(xl4-xl1)
            ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
            ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
            az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
            ax(nelmnef(itettyp(it))*(it-1)+1) = xhalf*ax1
            ay(nelmnef(itettyp(it))*(it-1)+1) = xhalf*ay1
            az(nelmnef(itettyp(it))*(it-1)+1) = xhalf*az1
            ax(nelmnef(itettyp(it))*(it-1)+2) = xhalf*ax2
            ay(nelmnef(itettyp(it))*(it-1)+2) = xhalf*ay2
            az(nelmnef(itettyp(it))*(it-1)+2) = xhalf*az2
            ax(nelmnef(itettyp(it))*(it-1)+3) = xhalf*ax3
            ay(nelmnef(itettyp(it))*(it-1)+3) = xhalf*ay3
            az(nelmnef(itettyp(it))*(it-1)+3) = xhalf*az3
            ax(nelmnef(itettyp(it))*(it-1)+4) = xhalf*ax4
            ay(nelmnef(itettyp(it))*(it-1)+4) = xhalf*ay4
            az(nelmnef(itettyp(it))*(it-1)+4) = xhalf*az4
            voltet(it)=-((xl4-xl1)*ax4+(yl4-yl1)*ay4+(zl4-zl1)*az4)/
     *                  6.0d+00
         else
            do i=1,nelmnen(itettyp(it))
              xv(i)=xic(itet1(itetoff(it)+i))
              yv(i)=yic(itet1(itetoff(it)+i))
              zv(i)=zic(itet1(itetoff(it)+i))
            enddo
            call volume_element(itettyp(it),xv,yv,zv,voltet(it))
         endif
      enddo
C
C     _______________________________________________________________
C
C     CALCULATE THE REQUIRED DERIVATIVES IN THE TETRAHEDRON.
C
      do it=1,ntets
         if(itettyp(it).eq.ifelmtet.or.itettyp(it).eq.ifelmtri) then
            dpdx(it)=0.0
            dpdy(it)=0.0
            dpdz(it)=0.0
            do i=1,nelmnef(itettyp(it))
               dpdx(it)= dpdx(it) +
     *                   pic(itet1(itetoff(it)+i))*ax(itetoff(it)+i)
               dpdy(it)= dpdy(it) +
     *                   pic(itet1(itetoff(it)+i))*ay(itetoff(it)+i)
               dpdz(it)= dpdz(it) +
     *                   pic(itet1(itetoff(it)+i))*az(itetoff(it)+i)
            enddo
            do i=1,nelmnef(itettyp(it))
               dpdx(it)=(xone/voltet(it))*dpdx(it)
               dpdy(it)=(xone/voltet(it))*dpdy(it)
               dpdz(it)=(xone/voltet(it))*dpdz(it)
            enddo
         endif
      enddo
C
C     *****************************************************************
C
C     CALCUATE THE SCALE LENGTH ASSOCIATE WITH THE GRADIENT OF A SCALAR.
C
C
      xmin = xone/epsilon
      ymin = xone/epsilon
      zmin = xone/epsilon
      pmin = xone/epsilon
      xmax = -xmin
      ymax = -ymin
      zmax = -zmin
      pmax = -pmin
      do it=1,ntets
         do i=1,nelmnen(itettyp(it))
            xmin = min(xmin,xic(itet1(itetoff(it)+i)))
            ymin = min(ymin,yic(itet1(itetoff(it)+i)))
            zmin = min(zmin,zic(itet1(itetoff(it)+i)))
            pmin = min(pmin,pic(itet1(itetoff(it)+i)))
            xmax = max(xmax,xic(itet1(itetoff(it)+i)))
            ymax = max(ymax,yic(itet1(itetoff(it)+i)))
            zmax = max(zmax,zic(itet1(itetoff(it)+i)))
            pmax = max(pmax,pic(itet1(itetoff(it)+i)))
         enddo
      enddo
      dsmax = sqrt((xmax-xmin)**2+(ymax-ymin)**2+(zmax-zmin)**2)
      dpmax = abs( pmax - pmin )
      xgradref = max( epsilon , dpmax/(dsmax + epsilon) )
C
C     Loop over elements
C
      if(coption2(1:icharlnf(coption2)).eq.'errormax') then
         if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *      coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *      coption1(1:icharlnf(coption1)).eq.'element') then
            length=ntets
            call mmgetblk('xavg',isubname,ipxavg,length,2,icscode)
            call mmgetblk('yavg',isubname,ipyavg,length,2,icscode)
            call mmgetblk('zavg',isubname,ipzavg,length,2,icscode)
            call mmgetblk('value',isubname,ipvalue,length,2,icscode)
            icount=0
            do it=1,ntets
               if(inclusive.eq.0) then
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  linclusive=.false.
                  if(isum.eq.nelmnen(itettyp(it))) linclusive=.true.
               else
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  linclusive=.false.
                  if(isum.gt.0) linclusive=.true.
               endif
               if(linclusive) then
                  icount=icount+1
                  xavg(icount)=0.0
                  yavg(icount)=0.0
                  zavg(icount)=0.0
                  value(icount)=0.0
                  xsum=0.0
                  do i=1,nelmnen(itettyp(it))
                     xsum=xsum+1.0
                     i1=itet1(itetoff(it)+i)
                     xavg(icount)=xavg(icount)+xic(i1)
                     yavg(icount)=yavg(icount)+yic(i1)
                     zavg(icount)=zavg(icount)+zic(i1)
                  enddo
                  xavg(icount)=xavg(icount)/xsum
                  yavg(icount)=yavg(icount)/xsum
                  zavg(icount)=zavg(icount)/xsum
               endif
            enddo
            cmotable='cmo_hex'
            ctable='pic'
            call intrp_element('xyz','xyz',
     *                         ipxavg,ipyavg,ipzavg,icount,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ipvalue,
     *                         ierror)
            icount=0
            do it=1,ntets
               if(inclusive.eq.0) then
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  linclusive=.false.
                  if(isum.eq.nelmnen(itettyp(it))) linclusive=.true.
               else
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  linclusive=.false.
                  if(isum.gt.0) linclusive=.true.
               endif
               if(linclusive) then
                  pavg=0.0
                  pmax=-1.0d+200
                  xsum=0.0
                  do i=1,nelmnen(itettyp(it))
                     xsum=xsum+1.0
                     i1=itet1(itetoff(it)+i)
                     pavg=pavg+pic(i1)
                     pmax=max(pmax,pic(i1))
                  enddo
                  pavg=pavg/xsum
C
C                 THIS IS ERRORMAX/TET
C
                  icount=icount+1
                  if(abs(value(icount)-pavg) .gt.
     *               xrefine(1)*value(icount)) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
            enddo
         elseif(coption1(1:icharlnf(coption1)).eq.'face') then
            length=nef*ntets
            call mmgetblk('xavg',isubname,ipxavg,length,2,icscode)
            call mmgetblk('yavg',isubname,ipyavg,length,2,icscode)
            call mmgetblk('zavg',isubname,ipzavg,length,2,icscode)
            call mmgetblk('value',isubname,ipvalue,length,2,icscode)
            icount=0
            do it=1,ntets
               if(inclusive.eq.0) then
                  isum1=0
                  isum2=0
                  do i=1,nelmnef(itettyp(it))
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        isum1=isum1+itestp(i1)
                        isum2=isum2+1
                     enddo
                  enddo
                  if(isum1.eq.isum2) linclusive=.true.
               else
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  if(isum.gt.0) linclusive=.true.
               endif
               if(linclusive) then
                  do i=1,nelmnef(itettyp(it))
                     icount=icount+1
                     xavg(icount)=0.0
                     yavg(icount)=0.0
                     zavg(icount)=0.0
                     value(icount)=0.0
                     xsum=0.0
                     do j=1,ielmface0(i,itettyp(it))
                        xsum=xsum+1.0
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        xavg(icount)=xavg(icount)+xic(i1)
                        yavg(icount)=yavg(icount)+yic(i1)
                        zavg(icount)=zavg(icount)+zic(i1)
                     enddo
                     xavg(icount)=xavg(icount)/xsum
                     yavg(icount)=yavg(icount)/xsum
                     zavg(icount)=zavg(icount)/xsum
                  enddo
               endif
            enddo
            cmotable='cmo_hex'
            ctable='pic'
            call intrp_element('xyz','xyz',
     *                         ipxavg,ipyavg,ipzavg,icount,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ipvalue,
     *                         ierror)
            icount=0
            do it=1,ntets
               if(inclusive.eq.0) then
                  isum1=0
                  isum2=0
                  do i=1,nelmnef(itettyp(it))
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        isum1=isum1+itestp(i1)
                        isum2=isum2+1
                     enddo
                  enddo
                  if(isum1.eq.isum2) linclusive=.true.
               else
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     isum=isum+itestp(i1)
                  enddo
                  if(isum.gt.0) linclusive=.true.
               endif
               if(linclusive) then
                  do i=1,nelmnef(itettyp(it))
                     icount=icount+1
                     pavg=0.0
                     pmax=-1.0d+200
                     xsum=0.0
                     do j=1,ielmface0(i,itettyp(it))
                        xsum=xsum+1.0
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        pavg=pavg+pic(i1)
                        pmax=max(pmax,pic(i1))
                     enddo
C
C                    THIS IS ERRORMAX/TET
C
                     pavg=pavg/xsum
                     icount=icount+1
                     if(abs(value(icount)-pavg) .gt.
     *                  xrefine(1)*value(icount)) then
                        nadd = nadd + 1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
            enddo
         elseif(coption1(1:icharlnf(coption1)).eq.'edge') then
            length=2*npoints
            call mmgetblk('nedge_bin',isubname,
     *                    ipnedge_bin,length,2,icscode)
            call mmgetblk('nedge_off',isubname,
     *                    ipnedge_off,length,2,icscode)
            do i=1,2*npoints
               nedge_bin(i)=0
               nedge_off(i)=0
            enddo
            do it=1,ntets
               do ie=1,nelmnee(itettyp(it))
                  i1 = itet1(itetoff(it)+ielmface1(1,ie,itettyp(it)))
                  i2 = itet1(itetoff(it)+ielmface1(2,ie,itettyp(it)))
                  isum12=i1+i2
                  nedge_bin(isum12)=nedge_bin(isum12)+1
               enddo
            enddo
            isum=0
            do i=1,2*npoints
               if(nedge_bin(i).gt.0) then
                  nedge_off(i)=isum
                  isum=isum+nedge_bin(i)
               endif
               nedge_bin(i)=0
            enddo
            length=isum+1
            call mmgetblk('iedge_i0',isubname,
     *                   ipiedge_i0,length,2,icscode)
            call mmgetblk('iedge_i1',isubname,
     *                   ipiedge_i1,length,2,icscode)
            call mmgetblk('iedge_i2',isubname,
     *                   ipiedge_i2,length,2,icscode)
            do i=1,length
               iedge_i0(i)=0
               iedge_i1(i)=0
               iedge_i2(i)=0
            enddo
            length=nee*ntets
            call mmgetblk('iedge',isubname,ipiedge,length,2,icscode)
            call mmgetblk('xavg',isubname,ipxavg,length,2,icscode)
            call mmgetblk('yavg',isubname,ipyavg,length,2,icscode)
            call mmgetblk('zavg',isubname,ipzavg,length,2,icscode)
            call mmgetblk('value',isubname,ipvalue,length,2,icscode)
            icount=0
            jcount=0
            do it=1,ntets
               do ie=1,nelmnee(itettyp(it))
                  i1 = itet1(itetoff(it)+ielmface1(1,ie,itettyp(it)))
                  i2 = itet1(itetoff(it)+ielmface1(2,ie,itettyp(it)))
                  if(inclusive.eq.0) then
                     linclusive=itestp(i1).eq.1.and.itestp(i2).eq.1
                  else
                     linclusive=itestp(i1).eq.1.or.itestp(i2).eq.1
                  endif
                  if(linclusive) then
                     isum=i1+i2
                     if(nedge_bin(isum).eq.0) then
                        jcount=jcount+1
                        kcount=jcount
                        nedge_bin(isum)=nedge_bin(isum)+1
                        ibinoff=nedge_off(isum)+nedge_bin(isum)
                        iedge_i0(ibinoff)=jcount
                        iedge_i1(ibinoff)=i1
                        iedge_i2(ibinoff)=i2
                     else
                        kcount=0
                        do i=1,nedge_bin(isum)
                           ibinoff=nedge_off(isum)+nedge_bin(isum)
                           if((i1.eq.iedge_i1(ibinoff).and.
     *                            i2.eq.iedge_i2(ibinoff)) .or.
     *                        (i1.eq.iedge_i2(ibinoff).and.
     *                            i2.eq.iedge_i1(ibinoff))) then
                               kcount=iedge_i0(ibinoff)
                           endif
                        enddo
                        if(kcount.eq.0) then
                           jcount=jcount+1
                           kcount=jcount
                           nedge_bin(isum)=nedge_bin(isum)+1
                           ibinoff=nedge_off(isum)+nedge_bin(isum)
                           iedge_i0(ibinoff)=jcount
                           iedge_i1(ibinoff)=i1
                           iedge_i2(ibinoff)=i2
                        endif
                     endif
                     icount=icount+1
                     iedge(icount)=kcount
                     xavg(kcount)=0.5d+00*(xic(i1)+xic(i2))
                     yavg(kcount)=0.5d+00*(yic(i1)+yic(i2))
                     zavg(kcount)=0.5d+00*(zic(i1)+zic(i2))
                     value(kcount)=0.0
                  endif
               enddo
            enddo
            cmotable='cmo_hex'
            ctable='pic'
            call intrp_element('xyz','xyz',
     *                         ipxic,ipyic,ipzic,npoints,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ippic,
     *                         ierror)
            call intrp_element('xyz','xyz',
     *                         ipxavg,ipyavg,ipzavg,jcount,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ipvalue,
     *                         ierror)
            icount=0
            do it=1,ntets
               do ie=1,nelmnee(itettyp(it))
                  i1 = itet1(itetoff(it)+ielmface1(1,ie,itettyp(it)))
                  i2 = itet1(itetoff(it)+ielmface1(2,ie,itettyp(it)))
                  if(inclusive.eq.0) then
                     linclusive=itestp(i1).eq.1.and.itestp(i2).eq.1
                  else
                     linclusive=itestp(i1).eq.1.or.itestp(i2).eq.1
                  endif
                  if(linclusive) then
C
C                    THIS IS ERRORMAX/TET
C
                     pavg=0.5d+00*(pic(i1)+pic(i2))
                     pmax=max(pic(i1),pic(i2))
                     icount=icount+1
                     jcount=iedge(icount)
                     if(abs(value(jcount)-pavg) .gt.
     *                  xrefine(1)*value(jcount)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  endif
               enddo
            enddo
         endif
         goto 9999
      endif
C
C
C     ******************************************************************
C     Loop over all elements to check whether the refinement criteria
C        is met.
C
      do it=1,ntets
C
C        ...............................................................
C        Set a logical flag that indicates whether this element will be
C           tested or not. The two criteria depend on the topology
C           (edge, face, element) and the inclusive/exclusive flag.
C
         linclusive=.false.
         if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *      coption1(1:icharlnf(coption1)).eq.'edge' .or.
     *      coption1(1:icharlnf(coption1)).eq.'faceedge' .or.
     *      coption1(1:icharlnf(coption1)).eq.'tetedge') then
            if(inclusive.eq.0) then
               do i=1,nelmnee(itettyp(it))
                  isum1=0
                  isum2=0
                  i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                  i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                  isum1=isum1+itestp(i1)+itestp(i2)
                  isum2=isum2+2
                  if(isum1.eq.isum2) linclusive=.true.
               enddo
            else
               isum=0
               do i=1,nelmnee(itettyp(it))
                  i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                  i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                  isum=isum+itestp(i1)+itestp(i2)
               enddo
               if(isum.gt.0) linclusive=.true.
            endif
         endif
         if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *      coption1(1:icharlnf(coption1)).eq.'tetface' .or.
     *      coption1(1:icharlnf(coption1)).eq.'face') then
            if(inclusive.eq.0) then
               isum1=0
               isum2=0
               do i=1,nelmnef(itettyp(it))
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     isum1=isum1+itestp(i1)
                     isum2=isum2+1
                  enddo
               enddo
               if(isum1.eq.isum2) linclusive=.true.
            else
               isum=0
               do i=1,nelmnef(itettyp(it))
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     isum=isum+itestp(i1)
                  enddo
               enddo
               if(isum.gt.0) linclusive=.true.
            endif
         endif
         if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *      coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *      coption1(1:icharlnf(coption1)).eq.'element') then
            if(inclusive.eq.0) then
               isum1=0
               isum2=0
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  isum1=isum1+itestp(i1)
                  isum2=isum2+1
               enddo
               if(isum1.eq.isum2) linclusive=.true.
            else
               isum=0
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  isum=isum+itestp(i1)
               enddo
               if(isum.gt.0) linclusive=.true.
            endif
         endif
C
C        ...............................................................
C        If this element is to be tested then check all the possible
C           options:
C
         if(linclusive) then
            if(coption2(1:icharlnf(coption2)).eq.'junction') then
C
C              THIS IS JUNCTION/TET
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
                  isum=0
                  do i=1,nelmnee(itettyp(it))
                     i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                     p1 = sign( xone , pic(i1) - xrefine(1) )
                     p2 = sign( xone , pic(i2) - xrefine(1) )
                     if(p1*p2.lt.xzero) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        if(j.eq.ielmface0(i,itettyp(it))) then
                           i2=itet1(itetoff(it)+
     *                        ielmface1(1,i,itettyp(it)))
                        else
                           i2=itet1(itetoff(it)+
     *                        ielmface1(j+1,i,itettyp(it)))
                        endif
                        p1 = sign( xone , pic(i1) - xrefine(1) )
                        p2 = sign( xone , pic(i2) - xrefine(1) )
                        if(p1*p2.lt.xzero) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
                  do ie=1,nelmnee(itettyp(it))
                     isum=0
                     i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                     p1 = sign( xone , pic(i1) - xrefine(1) )
                     p2 = sign( xone , pic(i2) - xrefine(1) )
                     if(p1*p2.lt.xzero) isum=isum+1
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetface') then
                  isum=0
                  do i=1,nelmnef(itettyp(it))
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        if(j.eq.ielmface0(i,itettyp(it))) then
                           i2=itet1(itetoff(it)+
     *                           ielmface1(1,i,itettyp(it)))
                        else
                           i2=itet1(itetoff(it)+
     *                           ielmface1(j+1,i,itettyp(it)))
                        endif
                        p1 = sign( xone , pic(i1) - xrefine(1) )
                        p2 = sign( xone , pic(i2) - xrefine(1) )
                        if(p1*p2.lt.xzero) isum=isum+1
                     enddo
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetedge') then
                  isum=0
                  do i=1,nelmnee(itettyp(it))
                     i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                     p1 = sign( xone , pic(i1) - xrefine(1) )
                     p2 = sign( xone , pic(i2) - xrefine(1) )
                     if(p1*p2.lt.xzero) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'faceedge') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        if(j.eq.ielmface0(i,itettyp(it))) then
                           i2=itet1(itetoff(it)+
     *                        ielmface1(1,i,itettyp(it)))
                        else
                           i2=itet1(itetoff(it)+
     *                        ielmface1(j+1,i,itettyp(it)))
                        endif
                        p1 = sign( xone , pic(i1) - xrefine(1) )
                        p2 = sign( xone , pic(i2) - xrefine(1) )
                        if(p1*p2.lt.xzero) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'errormax') then
C
C              THIS IS ERRORMAX/TET
C
               pavg=0.0
               pmax=-1.0d+200
               xsum=0.0
               do i=1,nelmnen(itettyp(it))
                  xsum=xsum+1.0
                  i1=itet1(itetoff(it)+i)
                  pavg=pavg+pic(i1)
                  pmax=max(pmax,pic(i1))
               enddo
               pavg=pavg/xsum
               if(abs(pmax-pavg).gt.xrefine(1)*pmax) then
                  nadd=nadd+1
                  itadd(nadd) = it
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'constant') then
C
C              THIS IS CONSTANT/TET
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
                  isum=0
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     p1 = pic(i1) - xrefine(1)
                     if(p1.gt.xzero) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        p1 = pic(i1) - xrefine(1)
                        if(p1.gt.xzero) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
                  do ie=1,nelmnee(itettyp(it))
                     isum=0
                     i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                     p1 = pic(i1) - xrefine(1)
                     if(p1.gt.xzero) isum=isum+1
                     p2 = pic(i2) - xrefine(1)
                     if(p2.gt.xzero) isum=isum+1
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetface') then
                  isum=0
                  do i=1,nelmnef(itettyp(it))
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        p1 = pic(i1) - xrefine(1)
                        if(p1.gt.xzero) isum=isum+1
                     enddo
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetedge') then
                  isum=0
                  do i=1,nelmnee(itettyp(it))
                     i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                     p1 = pic(i1) - xrefine(1)
                     if(p1.gt.xzero) isum=isum+1
                     p2 = pic(i2) - xrefine(1)
                     if(p2.gt.xzero) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'faceedge') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                        p1 = pic(i1) - xrefine(1)
                        if(p1.gt.xzero) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'maxsize') then
C
C              THIS IS MAXSIZE/TET
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
                  isum=0
                  if(voltet(it).gt.xrefine(1)) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
                  if(itettyp(it).eq.ifelmtri) then
                     i1=itet1(itetoff(it)+1)
                     i2=itet1(itetoff(it)+2)
                     i3=itet1(itetoff(it)+3)
                     area=triarea(xic(i1),yic(i1),zic(i1),
     *                            xic(i2),yic(i2),zic(i2),
     *                            xic(i3),yic(i3),zic(i3))
                     if(area.gt.xrefine(1)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = 1
                     endif
                  elseif(itettyp(it).eq.ifelmtet) then
                     do i=1,nelmnef(itettyp(it))
                        if(itettyp(it).eq.ifelmtet) then
                           i1=itet1(itetoff(it)+
     *                              ielmface1(1,i,itettyp(it)))
                           i2=itet1(itetoff(it)+
     *                              ielmface1(2,i,itettyp(it)))
                           i3=itet1(itetoff(it)+
     *                              ielmface1(3,i,itettyp(it)))
                           area=triarea(xic(i1),yic(i1),zic(i1),
     *                                  xic(i2),yic(i2),zic(i2),
     *                                  xic(i3),yic(i3),zic(i3))
                        endif
                        if(area.gt.xrefine(1)) then
                           nadd=nadd+1
                           itadd(nadd) = it
                           ifadd(nadd) = i
                        endif
                     enddo
                  else
                     write(logmess,'(a,i3)')
     *                'Refine implemented only for tet elements',
     *                 itettyp(it)
                     call writloga('default',1,logmess,1,ierrwrt)
                     go to 9999
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
                  do ie=1,nelmnee(itettyp(it))
                     isum=0
                     i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                     dis=sqrt((xic(i1)-xic(i2))**2
     *                      + (yic(i1)-yic(i2))**2
     *                      + (zic(i1)-zic(i2))**2)
                     if(dis.gt.xrefine(1)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetface') then
                  isum=0
                  do i=1,nelmnef(itettyp(it))
                     if(itettyp(it).ne.ifelmtet) then
                        write(logmess,'(a,i3)')
     *                   'Refine implemented only for tet elements',
     *                    itettyp(it)
                        go to 9999
                     endif
                     i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                     i3=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                     area=triarea(xic(i1),yic(i1),zic(i1),
     *                            xic(i2),yic(i2),zic(i2),
     *                            xic(i3),yic(i3),zic(i3))
                     if(area.gt.xrefine(1)) then
                        isum=isum+1
                     endif
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetedge') then
                  isum=0
                  do i=1,nelmnee(itettyp(it))
                     i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                     dis=sqrt((xic(i1)-xic(i2))**2
     *                      + (yic(i1)-yic(i2))**2
     *                      + (zic(i1)-zic(i2))**2)
                     if(dis.gt.xrefine(1)) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'faceedge') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        ie=ielmface2(j,i,itettyp(it))
                        i1=itet1(itetoff(it)+
     *                           ielmedge1(1,ie,itettyp(it)))
                        i2=itet1(itetoff(it)+
     *                           ielmedge1(2,ie,itettyp(it)))
                        dis=sqrt((xic(i1)-xic(i2))**2
     *                         + (yic(i1)-yic(i2))**2
     *                         + (zic(i1)-zic(i2))**2)
                        if(dis.gt.xrefine(1)) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'minsize') then
C
C              THIS IS MINSIZE/TET
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
                  isum=0
                  if(voltet(it).lt.xrefine(1)) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     if(itettyp(it).ne.ifelmtet) then
                        write(logmess,'(a,i3)')
     *                   'Refine implemented only for tet elements',
     *                    itettyp(it)
                        go to 9999
                     endif
                     i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                     i3=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                     area=triarea(xic(i1),yic(i1),zic(i1),
     *                            xic(i2),yic(i2),zic(i2),
     *                            xic(i3),yic(i3),zic(i3))
                     if(area.lt.xrefine(1)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = 1
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
                  do ie=1,nelmnee(itettyp(it))
                     isum=0
                     i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                     dis=sqrt((xic(i1)-xic(i2))**2
     *                      + (yic(i1)-yic(i2))**2
     *                      + (zic(i1)-zic(i2))**2)
                     if(dis.lt.xrefine(1)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  enddo
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetface') then
                  isum=0
                  do i=1,nelmnef(itettyp(it))
                     if(itettyp(it).ne.ifelmtet) then
                        write(logmess,'(a,i3)')
     *                   'Refine implemented only for tet elements',
     *                    itettyp(it)
                        go to 9999
                     endif
                     i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                     i3=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                     area=triarea(xic(i1),yic(i1),zic(i1),
     *                            xic(i2),yic(i2),zic(i2),
     *                            xic(i3),yic(i3),zic(i3))
                     if(area.lt.xrefine(1)) then
                        isum=isum+1
                     endif
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tetedge') then
                  isum=0
                  do i=1,nelmnee(itettyp(it))
                     i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                     dis=sqrt((xic(i1)-xic(i2))**2
     *                      + (yic(i1)-yic(i2))**2
     *                      + (zic(i1)-zic(i2))**2)
                     if(dis.lt.xrefine(1)) isum=isum+1
                  enddo
                  if(isum.gt.0) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'faceedge') then
                  do i=1,nelmnef(itettyp(it))
                     isum=0
                     do j=1,ielmface0(i,itettyp(it))
                        ie=ielmface2(j,i,itettyp(it))
                        i1=itet1(itetoff(it)+
     *                           ielmedge1(1,ie,itettyp(it)))
                        i2=itet1(itetoff(it)+
     *                           ielmedge1(2,ie,itettyp(it)))
                        dis=sqrt((xic(i1)-xic(i2))**2
     *                         + (yic(i1)-yic(i2))**2
     *                         + (zic(i1)-zic(i2))**2)
                        if(dis.lt.xrefine(1)) isum=isum+1
                     enddo
                     if(isum.gt.0) then
                        nadd=nadd+1
                        itadd(nadd) = it
                        ifadd(nadd) = i
                     endif
                  enddo
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'aspect') then
C
C              THIS IS ASPECT_RATIO/TET
C
C              Computes the ratio of the radius of the circumsphere to
C                  the radius of the inscribed sphere of a tetrahedron.
C                  Ratio is multiplied by 3 so that a value of one
C                  indicates a regular tetrahedron. If the ratio is
C                  smaller than xrefine(1) the tet and edges are taged.
C                  The ratio should never be greater than one.
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
                   i1=itet1(itetoff(it)+1)
                   i2=itet1(itetoff(it)+2)
                   i3=itet1(itetoff(it)+3)
                   i4=itet1(itetoff(it)+4)
                   xl1 = xic(i1)
                   yl1 = yic(i1)
                   zl1 = zic(i1)
                   xl2 = xic(i2)
                   yl2 = yic(i2)
                   zl2 = zic(i2)
                   xl3 = xic(i3)
                   yl3 = yic(i3)
                   zl3 = zic(i3)
                   xl4 = xic(i4)
                   yl4 = yic(i4)
                   zl4 = zic(i4)
C
C                  Calculation of "rcir", the radius of the
C                     circumscribed sphere of the tet.
C
                    xb = xl3 - xl2
                    yb = yl3 - yl2
                    zb = zl3 - zl2
                    xc = xl4 - xl2
                    yc = yl4 - yl2
                    zc = zl4 - zl2
                    xd = xl1 - xl2
                    yd = yl1 - yl2
                    zd = zl1 - zl2
                    xn =   yb*zc - yc*zb
                    yn = -(xb*zc - xc*zb)
                    zn =   xb*yc - xc*yb
                    x2 =   yn*zb - yb*zn
                    y2 = -(xn*zb - xb*zn)
                    z2 =   xn*yb - xb*yn
                    q = -0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                       (x2*xc+y2*yc+z2*zc+epsilon)
                    xa = q*x2 + 0.5*xb
                    ya = q*y2 + 0.5*yb
                    za = q*z2 + 0.5*zb
                    dvor = -0.5*(xd*xd + yd*yd + zd*zd)
                    qvor2 = -(xd*xa+yd*ya+zd*za+dvor)/
     *                       (xd*xn+yd*yn+zd*zn+1.0d-30)
 
                    rcir = sqrt( (qvor2*xn + xa)**2
     *                         + (qvor2*yn + ya)**2
     *                         + (qvor2*zn + za)**2 )
C
C                 Calculation of "rinsc", the radius of the inscribed
C                    sphere of the tet.
C
                   ac1 = a(yl2,yl4,yl3,zl2,zl4,zl3)
                   bc1 = a(zl2,zl4,zl3,xl2,xl4,xl3)
                   cc1 = a(xl2,xl4,xl3,yl2,yl4,yl3)
                   dc1 = d(xl2,xl4,xl3,yl2,yl4,yl3,zl2,zl4,zl3)
                   ac2 = a(yl1,yl4,yl2,zl1,zl4,zl2)
                   bc2 = a(zl1,zl4,zl2,xl1,xl4,xl2)
                   cc2 = a(xl1,xl4,xl2,yl1,yl4,yl2)
                   dc2 = d(xl1,xl4,xl2,yl1,yl4,yl2,zl1,zl4,zl2)
                   ac3 = a(yl1,yl2,yl3,zl1,zl2,zl3)
                   bc3 = a(zl1,zl2,zl3,xl1,xl2,xl3)
                   cc3 = a(xl1,xl2,xl3,yl1,yl2,yl3)
                   dc3 = d(xl1,xl2,xl3,yl1,yl2,yl3,zl1,zl2,zl3)
                   ac4 = a(yl1,yl3,yl4,zl1,zl3,zl4)
                   bc4 = a(zl1,zl3,zl4,xl1,xl3,xl4)
                   cc4 = a(xl1,xl3,xl4,yl1,yl3,yl4)
                   dc4 = d(xl1,xl3,xl4,yl1,yl3,yl4,zl1,zl3,zl4)
                   dn1 = sqrt(ac1**2 + bc1**2 + cc1**2)
                   dn2 = sqrt(ac2**2 + bc2**2 + cc2**2)
                   dn3 = sqrt(ac3**2 + bc3**2 + cc3**2)
                   dn4 = sqrt(ac4**2 + bc4**2 + cc4**2)
 
                   ac1 = ac1/dn1
                   bc1 = bc1/dn1
                   cc1 = cc1/dn1
                   dc1 = dc1/dn1
                   ac2 = ac2/dn2
                   bc2 = bc2/dn2
                   cc2 = cc2/dn2
                   dc2 = dc2/dn2
                   ac3 = ac3/dn3
                   bc3 = bc3/dn3
                   cc3 = cc3/dn3
                   dc3 = dc3/dn3
                   ac4 = ac4/dn4
                   bc4 = bc4/dn4
                   cc4 = cc4/dn4
                   dc4 = dc4/dn4
 
                   a11 = ac1 - ac2
                   a12 = bc1 - bc2
                   a13 = cc1 - cc2
                   d1  = dc2 - dc1
                   a21 = ac1 - ac3
                   a22 = bc1 - bc3
                   a23 = cc1 - cc3
                   d2  = dc3 - dc1
                   a31 = ac1 - ac4
                   a32 = bc1 - bc4
                   a33 = cc1 - cc4
                   d3  = dc4 - dc1
 
                   qdet = (a12*a23 - a13*a22)*a31
     *                  + (a13*a21 - a11*a23)*a32
     *                  + (a11*a22 - a12*a21)*a33
 
                   rx = ( (a22*a33 - a23*a32)*d1
     *                +   (a13*a32 - a12*a33)*d2
     *                +   (a12*a23 - a13*a22)*d3 )/qdet
 
                   ry = ( (a23*a31 - a21*a33)*d1
     *                +   (a11*a33 - a13*a31)*d2
     *                +   (a13*a21 - a11*a23)*d3 )/qdet
 
                   rz = ( (a21*a32 - a22*a31)*d1
     *                  + (a12*a31 - a11*a32)*d2
     *                  + (a11*a22 - a12*a21)*d3 )/qdet
 
                   rinsc =  ac1*rx + bc1*ry + cc1*rz + dc1
 
                   srat = 3.d0*rinsc/rcir
                   if(srat.lt.xrefine(1)) then
                      nadd=nadd+1
                      itadd(nadd) = it
                   endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
C
C                 Computes the ratio of the radius of the inscribed
C                    circle to the radius of the circumscribed circle
C                    of the triangular face. Ratio is multiplied by 2
C                    so that a value of one indicates an equilateral
C                    triangle. If the ratio is less than xrefine(1)
C                    the face is taged. The ratio should never be
C                    greater than one.
C
                  if(itettyp(it).eq.ifelmtet) then
                     do i=1,nelmnef(itettyp(it))
                        i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                        i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                        i3=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                        sa = sqrt( (xic(i2) - xic(i3))**2
     *                           + (yic(i2) - yic(i3))**2
     *                           + (zic(i2) - zic(i3))**2 )
                        sb = sqrt( (xic(i4) - xic(i2))**2
     *                           + (yic(i4) - yic(i2))**2
     *                           + (zic(i4) - zic(i2))**2 )
                        sc = sqrt( (xic(i3) - xic(i4))**2
     *                           + (yic(i3) - yic(i4))**2
     *                           + (zic(i3) - zic(i4))**2 )
                        farea=sqrt(ax(nelmnef(itettyp(it))*(it-1)+i)**2+
     *                             ay(nelmnef(itettyp(it))*(it-1)+i)**2+
     *                             az(nelmnef(itettyp(it))*(it-1)+i)**2)
C*****                  cirrat = sa*sb*sc*(sa+sb+sc)/(16.d0*farea**2)
                        cirrat = 16.d0*farea**2/(sa*sb*sc*(sa+sb+sc))
                        if(cirrat.lt.xrefine(1)) then
                           nadd = nadd + 1
                           itadd(nadd) = it
                           ifadd(nadd) = i
                        endif
                     enddo
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
C
C                 THIS IS ASPECT/EDGE -- THERE IS NO ASPECT/EDGE
C                      CRITERION
C
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'delta') then
C
C                 THIS IS DELTA/EDGE
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
                  do ie=1,nelmnee(itettyp(it))
                     isum=0
                     i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                     i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                     p1 = pic(i1)
                     p2 = pic(i2)
                     if(abs( p1 - p2 ).gt.xrefine(1)) then
                        nadd = nadd + 1
                        itadd(nadd) = it
                        ieadd(nadd) = ie
                     endif
                  enddo
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'grading') then
C
C              THIS IS GRADING/TET -- THERE IS NO GRADING/TET CRITERION
C
C
C                THIS IS GRADING/FACE
C
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
                  if(itettyp(it).eq.ifelmtet) then
                  endif
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'lambda') then
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
C
C                 THIS IS LAMBDA_REFINE/TET
C
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  xl1 = xic(i1)
                  yl1 = yic(i1)
                  zl1 = zic(i1)
                  xl2 = xic(i2)
                  yl2 = yic(i2)
                  zl2 = zic(i2)
                  xl3 = xic(i3)
                  yl3 = yic(i3)
                  zl3 = zic(i3)
                  xl4 = xic(i4)
                  yl4 = yic(i4)
                  zl4 = zic(i4)
C
C                 Calculation of "rcir", the radius of the circumscribed
C                    sphere of the tet.
C
                  xb = xl3 - xl2
                  yb = yl3 - yl2
                  zb = zl3 - zl2
                  xc = xl4 - xl2
                  yc = yl4 - yl2
                  zc = zl4 - zl2
                  xd = xl1 - xl2
                  yd = yl1 - yl2
                  zd = zl1 - zl2
                  xn =   yb*zc - yc*zb
                  yn = -(xb*zc - xc*zb)
                  zn =   xb*yc - xc*yb
                  x2 =   yn*zb - yb*zn
                  y2 = -(xn*zb - xb*zn)
                  z2 =   xn*yb - xb*yn
                  q = -0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     &                     (x2*xc+y2*yc+z2*zc + epsilon)
                  xa = q*x2 + 0.5*xb
                  ya = q*y2 + 0.5*yb
                  za = q*z2 + 0.5*zb
                  dvor = -0.5*(xd*xd + yd*yd + zd*zd)
                  qvor2 =-(xd*xa+yd*ya+zd*za+dvor)/
     *                    (xd*xn+yd*yn+zd*zn+1.0d-30)
 
                  rcir = sqrt( (qvor2*xn + xa)**2
     &                       + (qvor2*yn + ya)**2
     &                       + (qvor2*zn + za)**2 )
C
C                 Scale length is the circum radius of the tet
C
                  xscale = rcir
 
                  pavg = ( pic(i1) +
     *                     pic(i2) +
     *                     pic(i3) +
     *                     pic(i4) )/4.d0
                  xgradmag = sqrt(dpdx(it)**2+dpdy(it)**2+dpdz(it)**2)
                  if(xgradmag.lt.epsilon*xgradref) then
                     xgradtest(it) = xrefine(1)
                  else
                     xlam = abs( pavg )/( xgradmag + epsilon )
                     xgradtest(it) = xlam/( xscale + epsilon )
                  endif
                  if(xgradtest(it).lt.xrefine(1)) then
                        nadd=nadd+1
                        itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
C
C                 THIS IS LAMBDA_REFINE/FACE
C
                  if(itettyp(it).eq.ifelmtet) then
                     do i=1,nelmnef(itettyp(it))
                        i2=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                        i3=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                        i4=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                        p1 = ( pic(i3) + pic(i4) )/2.d0
                        p2 = ( pic(i4) + pic(i2) )/2.d0
                        p3 = ( pic(i2) + pic(i3) )/2.d0
                        pavg = ( pic(i2) + pic(i3) + pic(i4) )/3.d0
                        farea=sqrt(ax(nelmnef(itettyp(it))*(it-1)+i)**2+
     *                             ay(nelmnef(itettyp(it))*(it-1)+i)**2+
     *                             az(nelmnef(itettyp(it))*(it-1)+i)**2)
                        dx1 = xic(i4) - xic(i3)
                        dy1 = yic(i4) - yic(i3)
                        dz1 = zic(i4) - zic(i3)
                        dx2 = xic(i4) - xic(i2)
                        dy2 = yic(i4) - yic(i2)
                        dz2 = zic(i4) - zic(i2)
                        dx3 = xic(i3) - xic(i2)
                        dy3 = yic(i3) - yic(i2)
                        dz3 = zic(i3) - zic(i2)
                        afx1 =   dy1*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz1
                        afy1 = -(dx1*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz1)
                        afz1 =   dx1*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy1
                        afx2 =   dy2*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz2
                        afy2 = -(dx2*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz2)
                        afz2 =   dx2*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy2
                        afx3 =   dy3*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz3
                        afy3 = -(dx3*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz3)
                        afz3 =   dx3*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy3
                        xmag1 = sqrt( dx1**2 + dy1**2 + dz1**2 )
                        xmag2 = sqrt( dx2**2 + dy2**2 + dz2**2 )
                        xmag3 = sqrt( dx3**2 + dy3**2 + dz3**2 )
                        afmag1 = sqrt( afx1**2 + afy1**2 + afz1**2 )
                        afmag2 = sqrt( afx2**2 + afy2**2 + afz2**2 )
                        afmag3 = sqrt( afx3**2 + afy3**2 + afz3**2 )
                        dpdx(itetoff(it)+i) = ( p1*xmag1*afx1/afmag1
     *                                        + p2*xmag2*afx2/afmag2
     *                                        + p3*xmag3*afx3/afmag3 ) /
     *                                          farea
                        dpdy(itetoff(it)+i) = ( p1*xmag1*afy1/afmag1
     *                                        + p2*xmag2*afy2/afmag2
     *                                        + p3*xmag3*afy3/afmag3 ) /
     *                                          farea
                        dpdz(itetoff(it)+i) = ( p1*xmag1*afz1/afmag1
     *                                        + p2*xmag2*afz2/afmag2
     *                                        + p3*xmag3*afz3/afmag3 ) /
     *                                          farea
                        xgradmag = sqrt( dpdx(itetoff(it)+i)**2
     *                                 + dpdy(itetoff(it)+i)**2
     *                                 + dpdz(itetoff(it)+i)**2 )
C
C                       Scale length is the circum radius of the face
C
                        xscale = xmag1*xmag2*xmag3/(4.d0*farea)
 
                        if(xgradmag.lt.epsilon*xgradref) then
                           xgradtest(itetoff(it)+i) = xrefine(1)
                        else
                           xlam = abs( pavg )/( xgradmag + epsilon )
                           xgradtest(itetoff(it)+i) = xlam/
     *                                                (xscale + epsilon)
                        endif
                        if(xgradtest(itetoff(it)+i).lt.xrefine(1)) then
                           nadd = nadd + 1
                           itadd(nadd) = it
                           ifadd(nadd) = i
                        endif
                     enddo
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
C
C                 THIS IS LAMBDA_REFINE/EDGE
C
                  if(itettyp(it).eq.ifelmtet) then
                    do ie=1,nelmnee(itettyp(it))
                       i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                       i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                       pavg = ( pic(i2) + pic(i1) )/2.d0
                       pdif = abs( pic(i2) - pic(i1) )
                       xgradtest(1) = abs( pavg )/( pdif + epsilon )
                       if(xgradtest(1).lt.xrefine(1)
     *                    .and.pic(i1).ne.pic(i2)) then
                          nadd = nadd + 1
                          itadd(nadd) = it
                          ieadd(nadd) = ie
                       endif
                    enddo
                  endif
               endif
            elseif(coption2(1:icharlnf(coption2)).eq.'lambdade') then
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'tet' .or.
     *            coption1(1:icharlnf(coption1)).eq.'element') then
C
C                 THIS IS LAMBDA_(DE)REFINE/TET
C
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  xl1 = xic(i1)
                  yl1 = yic(i1)
                  zl1 = zic(i1)
                  xl2 = xic(i2)
                  yl2 = yic(i2)
                  zl2 = zic(i2)
                  xl3 = xic(i3)
                  yl3 = yic(i3)
                  zl3 = zic(i3)
                  xl4 = xic(i4)
                  yl4 = yic(i4)
                  zl4 = zic(i4)
C
C                 Calculation of "rcir", the radius of the
C                    circumscribed sphere of the tet.
C
                  xb = xl3 - xl2
                  yb = yl3 - yl2
                  zb = zl3 - zl2
                  xc = xl4 - xl2
                  yc = yl4 - yl2
                  zc = zl4 - zl2
                  xd = xl1 - xl2
                  yd = yl1 - yl2
                  zd = zl1 - zl2
                  xn =   yb*zc - yc*zb
                  yn = -(xb*zc - xc*zb)
                  zn =   xb*yc - xc*yb
                  x2 =   yn*zb - yb*zn
                  y2 = -(xn*zb - xb*zn)
                  z2 =   xn*yb - xb*yn
                  q = -0.5*(xc*xb+yc*yb+zc*zb - xc*xc-yc*yc-zc*zc)/
     &                     (x2*xc+y2*yc+z2*zc + epsilon)
                  xa = q*x2 + 0.5*xb
                  ya = q*y2 + 0.5*yb
                  za = q*z2 + 0.5*zb
                  dvor = -0.5*(xd*xd + yd*yd + zd*zd)
                  qvor2 =-(xd*xa+yd*ya+zd*za+dvor)/
     *                    (xd*xn+yd*yn+zd*zn+1.0d-30)
 
                  rcir = sqrt( (qvor2*xn + xa)**2
     &                       + (qvor2*yn + ya)**2
     &                       + (qvor2*zn + za)**2 )
C
C                 Scale length is the circum radius of the face
C
                  xscale = rcir
 
                  pavg = ( pic(i1) +
     *                     pic(i2) +
     *                     pic(i3) +
     *                     pic(i4) )/4.d0
                  xgradmag = sqrt(dpdx(it)**2+dpdy(it)**2+dpdz(it)**2)
                  if(xgradmag.lt.epsilon*xgradref) then
                     xgradtest(it) = xrefine(1)
                  else
                     xlam = abs( pavg )/( xgradmag + epsilon )
                     xgradtest(it) = xlam/( xscale + epsilon )
                  endif
                  if(xgradtest(it).gt.xrefine(1)) then
                     nadd=nadd+1
                     itadd(nadd) = it
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'face') then
C
C                 THIS IS LAMBDA_(DE)REFINE/FACE
C
                  if(itettyp(it).eq.ifelmtet) then
                     do i=1,nelmnef(itettyp(it))
                        i2=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                        i3=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                        i4=itet1(itetoff(it)+ielmface1(3,i,itettyp(it)))
                        p1 = ( pic(i3) + pic(i4) )/2.d0
                        p2 = ( pic(i4) + pic(i2) )/2.d0
                        p3 = ( pic(i2) + pic(i3) )/2.d0
                        pavg = ( pic(i2) + pic(i3) + pic(i4) )/3.d0
                        farea = sqrt(
     *                            ax(nelmnef(itettyp(it))*(it-1)+i)**2 +
     *                            ay(nelmnef(itettyp(it))*(it-1)+i)**2 +
     *                            az(nelmnef(itettyp(it))*(it-1)+i)**2)
                        dx1 = xic(i4) - xic(i3)
                        dy1 = yic(i4) - yic(i3)
                        dz1 = zic(i4) - zic(i3)
                        dx2 = xic(i4) - xic(i2)
                        dy2 = yic(i4) - yic(i2)
                        dz2 = zic(i4) - zic(i2)
                        dx3 = xic(i3) - xic(i2)
                        dy3 = yic(i3) - yic(i2)
                        dz3 = zic(i3) - zic(i2)
                        afx1 =   dy1*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz1
                        afy1 = -(dx1*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz1)
                        afz1 =   dx1*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy1
                        afx2 =   dy2*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz2
                        afy2 = -(dx2*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz2)
                        afz2 =   dx2*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy2
                        afx3 =   dy3*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ay(nelmnef(itettyp(it))*(it-1)+i)*dz3
                        afy3 = -(dx3*az(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dz3)
                        afz3 =   dx3*ay(nelmnef(itettyp(it))*(it-1)+i) -
     *                           ax(nelmnef(itettyp(it))*(it-1)+i)*dy3
                        xmag1 = sqrt( dx1**2 + dy1**2 + dz1**2 )
                        xmag2 = sqrt( dx2**2 + dy2**2 + dz2**2 )
                        xmag3 = sqrt( dx3**2 + dy3**2 + dz3**2 )
                        afmag1 = sqrt( afx1**2 + afy1**2 + afz1**2 )
                        afmag2 = sqrt( afx2**2 + afy2**2 + afz2**2 )
                        afmag3 = sqrt( afx3**2 + afy3**2 + afz3**2 )
                        dpdx(itetoff(it)+i) = ( p1*xmag1*afx1/afmag1
     *                                        + p2*xmag2*afx2/afmag2
     *                                        + p3*xmag3*afx3/afmag3 ) /
     *                                          farea
                        dpdy(itetoff(it)+i) = ( p1*xmag1*afy1/afmag1
     *                                        + p2*xmag2*afy2/afmag2
     *                                        + p3*xmag3*afy3/afmag3 ) /
     *                                          farea
                        dpdz(itetoff(it)+i) = ( p1*xmag1*afz1/afmag1
     *                                        + p2*xmag2*afz2/afmag2
     *                                        + p3*xmag3*afz3/afmag3 ) /
     *                                          farea
                        xgradmag = sqrt( dpdx(itetoff(it)+i)**2
     *                                 + dpdy(itetoff(it)+i)**2
     *                                 + dpdz(itetoff(it)+i)**2 )
C
C                       Scale length is the circum radius of the face
C
                        xscale = xmag1*xmag2*xmag3/(4.d0*farea)
 
                        if(xgradmag.lt.epsilon*xgradref) then
                           xgradtest(itetoff(it)+i) = xrefine(1)
                        else
                           xlam = abs( pavg )/( xgradmag + epsilon )
                           xgradtest(itetoff(it)+i) = xlam /
     *                                                (xscale + epsilon)
                        endif
                        if(xgradtest(itetoff(it)+i).gt.xrefine(1)) then
                           nadd = nadd + 1
                           itadd(nadd) = it
                           ifadd(nadd) = i
                        endif
                     enddo
                  endif
               endif
               if(coption1(1:icharlnf(coption1)).eq.'-all-' .or.
     *            coption1(1:icharlnf(coption1)).eq.'edge') then
C
C                 THIS IS LAMBDA_(DE)REFINE/EDGE
C
                  if(itettyp(it).eq.ifelmtet) then
                    do ie=1,nelmnee(itettyp(it))
                       i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                       i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                       pavg = ( pic(i2) + pic(i1) )/2.d0
                       pdif = abs( pic(i2) - pic(i1) )
                       xgradtest(1) = abs( pavg )/( pdif + epsilon )
                       if(xgradtest(1).ge.xrefine(1)) then
                          nadd = nadd + 1
                          itadd(nadd) = it
                          ieadd(nadd) = ie
                       endif
                    enddo
                  endif
               endif
            endif
         endif
      enddo
C
C     ******************************************************************
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
      return
      end
