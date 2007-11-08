      subroutine matbld2d_stor(ifile)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1996                              C
C This program was prepared by the Regents of the University of        C
C California at Los Alamos National Laboratory (the University) under  C
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). C
C The University has certain rights in the program pursuant to the     C
C contract and the program should not be copied or distributed outside C
C your organization. All rights in the program are reserved by the DOE C
C and the University. Neither the U.S. Government nor the University   C
C makes any warranty, express or implied, or assumes and liability or  C
C responsibility for the use of this software.                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C
C#######################################################################
C
C      PURPOSE -
C
C         BUILD A SPARSE MATRIX THAT CAN BE FED TO AN ITERATIVE SOLVER.
C
C      INPUT ARGUMENTS -
C
C         ifile - Base file name (.stor is appended).
C
C      OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: matbld2d_stor.f,v $
C         Revision 2.00  2007/11/05 19:46:01  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.13   08 Feb 2006 14:35:42   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.12   Thu Apr 06 09:14:04 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.11   Wed Apr 05 13:34:38 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.10   Thu Nov 11 10:26:20 1999   gable
CPVCS    Removed declaration with x1, y1, z1, xa21, ya21, za21
CPVCS    defined twice.
CPVCS
CPVCS       Rev 1.9   Wed Nov 10 15:45:50 1999   dcg
CPVCS    change subroutine name of sort routine to hpsort1
CPVCS
CPVCS       Rev 1.8   Mon Aug 30 15:14:42 1999   dcg
CPVCS    remove calls to ssort routines replace with hpsort
CPVCS
CPVCS       Rev 1.7   Wed Jun 16 10:37:32 1999   nnc
CPVCS    Fixed multiple variable declaration and incorrect declaration.
CPVCS
CPVCS       Rev 1.6   Thu Jun 03 14:12:08 1999   tam
CPVCS    removed print*
CPVCS
CPVCS       Rev 1.5   Fri Mar 19 10:22:18 1999   gable
CPVCS    Modified message output and made stor file headers
CPVCS    the same format as 3D ascii stor files.
CPVCS
CPVCS       Rev 1.4   Mon Mar 15 09:11:54 1999   gable
CPVCS    Changed file header to include max connections to a node.
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:53:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Fri Mar 28 13:01:00 1997   dcg
CPVCS    return area_tri as area of original triangle
CPVCS
CPVCS       Rev 1.1   Tue Mar 18 08:28:20 1997   gable
CPVCS    Major modification to the way voronoi areas are accumulated.
CPVCS    Previous version had not taken into account that the area
CPVCS    vectors needed to be summed rather than the area magnitude.
CPVCS    The result was previous versions sometimes got incorrect
CPVCS    area contributions for triangles with angles greater than
CPVCS    90 degrees.
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:40:50 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      character*(*) ifile
C
C
C*******************************************************************
C
      include "chydro.h"
      include "consts.h"
C
      integer ierror
C
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000),
     *        itet(3,1000000), jtet(3,1000000), itetclr(1000000)
      integer itet1(3*1000000), jtet1(3*1000000)
      real*8   xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipvolic, volic)
      real*8 volic(1000000)
      pointer (ipvolicx, volicx)
      real*8 volicx(1000000)
      pointer (ipvolicy, volicy)
      real*8 volicy(1000000)
      pointer (ipvolicz, volicz)
      real*8 volicz(1000000)
C
      character*32 cmo
C
      pointer (ipumat, umat)
      real*8 umat(1000000)
      pointer (ipbmat, bmat)
      real*8 bmat(1000000)
C
      pointer (ipitemp, itemp)
      integer itemp(10000000)
C
      character*32 isubname, ifilename
      character*72  title_string
      character*24 string, fdate
C
C#######################################################################
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
      pointer (ipitetp, itetp1)
      integer itetp1(3*1000000)
      pointer (ipitetp, itetp)
      integer itetp(3,1000000)
C
      pointer (ipisort, isort)
      pointer (ipicolmat, icolmat)
      pointer (ipirowmat, irowmat)
      pointer (ipisendnn, isendnn)
      pointer (ipxsendnn, xsendnn)
      pointer (ipirowcnt, irowcnt)
      pointer (ipirowoff, irowoff)
      pointer (ipirowdag, irowdag)
      integer isort(6*1000000+1000000)
      integer icolmat(6*1000000+1000000), irowmat(6*1000000+1000000)
      real*8 isendnn(6*1000000+1000000), xsendnn(6*1000000+1000000)
      integer irowcnt(1000000), irowoff(1000000), irowdag(1000000)
 
      pointer (ipamat, amat)
      real*8 amat(6*1000000+1000000)
      pointer (ipnmat, nmat)
      integer nmat(6*1000000+1000000)
      pointer (ipconst, const)
      real*8 const(6*1000000+1000000)
      pointer (ipimat, imat)
      integer imat(12*1000000+1000000)
      pointer (ipxmat, xmat)
      real*8 xmat(12*1000000+1000000)
      pointer (ipidxmat, idxmat)
      integer idxmat(12*1000000+1000000)
      integer nconn,itmax,isolve,ier,icmotype,
     *  npoints,mbndry,nsdtopo,nsdgeom,ntets,nen,nef,i1p,i2p,
     *  i3p,i1,i2,i3,ncoefs,nnmax,i,index,it,n12,idiag,
     *  ipackopt,ierrw,length,icscode,irow,idsave,icount,
     *  jcount,j,ierr1,num_conn_max,neq,neqp1,ncont,iwtotl,
     *  icharlnf,iunit,narea
      real*8 cpmult,a,b,c,d,e,f,crosx,crosy,crosz,a1x,a1y,a1z,
     *  ds23,ds13,xdot,cvx,cvy,cvz,cmx,cmy,cmz,xarea1,xarea2,
     *  xarea3,xdot3,xdot2,xdot1,x12,y12,z12,x13,y13,z13,x23,y23,
     *  z23,xv,yv,zv,ds1,ds2,ds3,ql,rb3,xl,yl,zl,dotb3,
     *  xn,yn,zn,xn1,yn1,zn1,xd,yd,zd,xa,ya,za,xb,yb,zb,xm,ym,zm,
     *  x1,y1,z1,x2,y2,z2,voltotal_tri,x3,y3,z3,ds12,a2x,a2y,a2z,
     *  a3x,a3y,a3z,area1,area2,area3,area,amatmin,amatmax,dot3,
     *  volic_check,volic_diff,volmin,volmax,rn,
     *  voltotal_vor
C
      parameter (nconn=6)
      integer lconn(2,nconn)
      data lconn / 1, 2,
     *             2, 1,
     *             1, 3,
     *             3, 1,
     *             2, 3,
     *             3, 2 /
C
      data cpmult / 0.0 /
      data itmax / 10 /
      data isolve / 0 /
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C
C#######################################################################
C
C
C
      isubname='matbld2d'
C
C
      call cmo_get_name(cmo,ierror)
 
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ier)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('idebug',cmo,idebug,length,icmotype,ierror)
      if(idebug.ne.0) then
        write(logmess,'(a,i5)')"debug option set to: ",idebug
        call writloga('default',0,logmess,0,ierrw)
      endif
 
C
C     ..................................................................
C     SET THE MATRIX PACKING FLAG:  =0 ==> THE SPARSE MATRIX PATTERN
C                                             IS PADDED TO A CONSTANT
C                                             COLUMN WIDTH.
C                                   =1 ==> THE SPARSE MATRIX PATTERN
C                                             IS FULLY COMPRESSED WITH
C                                             OFFSETS TO THE ROWS.
C
      ipackopt=1
C
C
C     ..................................................................
C     SET THE MATRIX DIAGONAL FLAG: =0 ==> DON'T INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C                                   =1 ==> INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C
      idiag=1
C
C
C     ..................................................................
C
      n12=nconn*ntets+idiag*npoints
C
C
C     ..................................................................
C     ALLOCATE SOME TEMPARY ARRAYS.
C
      length=nconn*ntets+idiag*npoints
      call mmgetblk("isort",isubname,ipisort,length,2,icscode)
      call mmgetblk("icolmat",isubname,ipicolmat,length,2,icscode)
      call mmgetblk("irowmat",isubname,ipirowmat,length,2,icscode)
      call mmgetblk("isendnn",isubname,ipisendnn,length,2,icscode)
      call mmgetblk("xsendnn",isubname,ipxsendnn,length,2,icscode)
      length=npoints
      call mmgetblk("irowcnt",isubname,ipirowcnt,length,2,icscode)
      call mmgetblk("irowoff",isubname,ipirowoff,length,2,icscode)
      call mmgetblk("irowdag",isubname,ipirowdag,length,2,icscode)
      length=npoints
      call mmgetblk("umat",isubname,ipumat,length,2,icscode)
      call mmgetblk("bmat",isubname,ipbmat,length,2,icscode)
      length=npoints
      call mmgetblk("iparent",isubname,ipiparent,length,2,icscode)
      length=nen*ntets
      call mmgetblk("itetp",isubname,ipitetp,length,2,icscode)
C
C
C     ..................................................................
C     FIND THE PARENTS OF EACH NODE.
C
      call unpackpc(npoints,itp1,isn1,iparent)
C
      do it=1,ntets
         index=nen*(it-1)
         do i=1,nen
            itetp1(index+i)=iparent(itet1(index+i))
         enddo
      enddo
C
C     ..................................................................
c     build some matrix arrays from the tet list.
c
      do i=1,n12
         irowmat(i)=0
         icolmat(i)=0
         isendnn(i)=0
      enddo
      do i=1,npoints
         irowcnt(i)=0
         irowoff(i)=0
         irowdag(i)=0
      enddo
c
      call matbld1(idiag,ipackopt,
     *             nsdtopo,nen,nef,
     *             nconn,lconn,
     *             npoints,ntets,itet1,
     *             irowmat,icolmat,isort,isendnn,
     *             irowcnt,irowoff)
      do irow=1,npoints
         do i=irowoff(irow)+1 , irowoff(irow)+irowcnt(irow)
            if(irowmat(i).eq.icolmat(i)) then
               idsave=i
            endif
         enddo
         irowdag(irow)=idsave
      enddo
C
C
C     ..................................................................
C     BUILD THE (PARTIAL) MATRIX ELEMENTS FROM THE TET LIST.
C
C     XSENDNN=1.0
C
C
C     ..................................................................
C     FORM THE SPARSE COEFFICIENT MATRIX AND THE SPARSE MATRIX PATTERN.
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     ALLOCATE MEMORY FOR THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
      nnmax = 0
      do i=1,npoints
         nnmax=max(nnmax,irowcnt(i))
      enddo
      if(ipackopt.eq.0) then
         length=nnmax*npoints
      elseif(ipackopt.eq.1) then
         length=irowoff(npoints)+irowcnt(npoints)
      endif
      ncoefs=length
      call mmgetblk("nmat",isubname,ipnmat,length,1,icscode)
      call mmgetblk("amat",isubname,ipamat,length,2,icscode)
      call mmgetblk("imat",isubname,ipimat,length,1,icscode)
      call mmgetblk("xmat",isubname,ipxmat,length,2,icscode)
      call mmgetblk("idxmat",isubname,ipidxmat,length,1,icscode)
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
C
      length=n12
      call mmgetblk("const",isubname,ipconst,length,2,icscode)
      length=npoints
      call mmgetblk("volic",isubname,ipvolic,length,2,icscode)
      call mmgetblk("volicx",isubname,ipvolicx,length,2,icscode)
      call mmgetblk("volicy",isubname,ipvolicy,length,2,icscode)
      call mmgetblk("volicz",isubname,ipvolicz,length,2,icscode)
      do i=1,npoints
         volic(i)=0.0
         volicx(i)=0.0
         volicy(i)=0.0
         volicz(i)=0.0
      enddo
c
c    Loop throught all the triangle elements and
c    accumulate the Voronoi length and area contribution
c    from each element.
c
      voltotal_tri = 0.0d0
      do it=1,ntets
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
c
c        Median point of triangle
c
         xm=(xic(i1)+xic(i2)+xic(i3))/3.0
         ym=(yic(i1)+yic(i2)+yic(i3))/3.0
         zm=(zic(i1)+zic(i2)+zic(i3))/3.0
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
         xb=xic(i2)-xa
         yb=yic(i2)-ya
         zb=zic(i2)-za
         xd=xic(i3)-xa
         yd=yic(i3)-ya
         zd=zic(i3)-za
         xn1= crosx(xb,yb,zb,xd,yd,zd)
         yn1= crosy(xb,yb,zb,xd,yd,zd)
         zn1= crosz(xb,yb,zb,xd,yd,zd)
         xn = crosx(xb,yb,zb,xn1,yn1,zn1)
         yn = crosy(xb,yb,zb,xn1,yn1,zn1)
         zn = crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv=xl+xa
         yv=yl+ya
         zv=zl+za
         x12=0.5*(xic(i1)+xic(i2))
         y12=0.5*(yic(i1)+yic(i2))
         z12=0.5*(zic(i1)+zic(i2))
         x13=0.5*(xic(i1)+xic(i3))
         y13=0.5*(yic(i1)+yic(i3))
         z13=0.5*(zic(i1)+zic(i3))
         x23=0.5*(xic(i2)+xic(i3))
         y23=0.5*(yic(i2)+yic(i3))
         z23=0.5*(zic(i2)+zic(i3))
         xdot1=(x2-x1)*(x3-x1)+(y2-y1)*(y3-y1)+(z2-z1)*(y3-y1)
         xdot2=(x3-x2)*(x1-x2)+(y3-y2)*(y1-y2)+(z3-z2)*(y1-y2)
         xdot3=(x1-x3)*(x2-x3)+(y1-y3)*(y2-y3)+(z1-z3)*(y2-y3)
         xarea1=sqrt((xv-x12)*(xv-x12)+
     *               (yv-y12)*(yv-y12)+
     *               (zv-z12)*(zv-z12))
         xarea2=sqrt((xv-x13)*(xv-x13)+
     *               (yv-y13)*(yv-y13)+
     *               (zv-z13)*(zv-z13))
         xarea3=sqrt((xv-x23)*(xv-x23)+
     *               (yv-y23)*(yv-y23)+
     *               (zv-z23)*(zv-z23))
         ds12=sqrt((xic(i2)-xic(i1))**2+
     *             (yic(i2)-yic(i1))**2+
     *             (zic(i2)-zic(i1))**2)
         cmx=  (y2-y1)*(zm-z1)-(ym-y1)*(z2-z1)
         cmy=-((x2-x1)*(zm-z1)-(xm-x1)*(z2-z1))
         cmz=  (x2-x1)*(ym-y1)-(xm-x1)*(y2-y1)
         cvx=  (y2-y1)*(zv-z1)-(yv-y1)*(z2-z1)
         cvy=-((x2-x1)*(zv-z1)-(xv-x1)*(z2-z1))
         cvz=  (x2-x1)*(yv-y1)-(xv-x1)*(y2-y1)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it      )=-xarea1/ds12
            const(it+ntets)=-xarea1/ds12
c*****      print *,"negative area 1: ",it,i1,i2,xdot1,xarea1,xdot
         else
            const(it       )= xarea1/ds12
            const(it+ntets)= xarea1/ds12
         endif
         ds13=sqrt((xic(i3)-xic(i1))**2+
     *             (yic(i3)-yic(i1))**2+
     *             (zic(i3)-zic(i1))**2)
         cmx=  (y1-y3)*(zm-z3)-(ym-y3)*(z1-z3)
         cmy=-((x1-x3)*(zm-z3)-(xm-x3)*(z1-z3))
         cmz=  (x1-x3)*(ym-y3)-(xm-x3)*(y1-y3)
         cvx=  (y1-y3)*(zv-z3)-(yv-y3)*(z1-z3)
         cvy=-((x1-x3)*(zv-z3)-(xv-x3)*(z1-z3))
         cvz=  (x1-x3)*(yv-y3)-(xv-x3)*(y1-y3)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it+2*ntets)=-xarea2/ds13
            const(it+3*ntets)=-xarea2/ds13
c*****      print *,"negative area 2: ",it,i1,i3,xdot2,xarea2,xdot
         else
            const(it+2*ntets)= xarea2/ds13
            const(it+3*ntets)= xarea2/ds13
         endif
         ds23=sqrt((xic(i3)-xic(i2))**2+
     *             (yic(i3)-yic(i2))**2+
     *             (zic(i3)-zic(i2))**2)
         cmx=  (y3-y2)*(zm-z2)-(ym-y2)*(z3-z2)
         cmy=-((x3-x2)*(zm-z2)-(xm-x2)*(z3-z2))
         cmz=  (x3-x2)*(ym-y2)-(xm-x2)*(y3-y2)
         cvx=  (y3-y2)*(zv-z2)-(yv-y2)*(z3-z2)
         cvy=-((x3-x2)*(zv-z2)-(xv-x2)*(z3-z2))
         cvz=  (x3-x2)*(yv-y2)-(xv-x2)*(y3-y2)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it+4*ntets)=-xarea3/ds23
            const(it+5*ntets)=-xarea3/ds23
c*****      print *,"negative area 3: ",it,i2,i3,xdot3,xarea3,xdot
         else
            const(it+4*ntets)= xarea3/ds23
            const(it+5*ntets)= xarea3/ds23
         endif
         i1p=iparent(i1)
         i2p=iparent(i2)
         i3p=iparent(i3)
c
         call   volume_tri_voronoi(
     *                      x1,y1,z1,
     *                      x2,y2,z2,
     *                      x3,y3,z3,
     *                      xv,yv,zv,
     *                      a1x,a1y,a1z,
     *                      a2x,a2y,a2z,
     *                      a3x,a3y,a3z,
     *                      area1,area2,area3,area)
 
         voltotal_tri = voltotal_tri + area
c
c        note that we can either accumulate the vector areas
c        associated with each node as vector components or we
c        can sum the signed areas associated with each node.
c        either way is equivalent.
c
         volic (i1)=volic (i1) + area1
         volic (i2)=volic (i2) + area2
         volic (i3)=volic (i3) + area3
         volicx(i1)=volicx(i1) + a1x
         volicy(i1)=volicy(i1) + a1y
         volicz(i1)=volicz(i1) + a1z
         volicx(i2)=volicx(i2) + a2x
         volicy(i2)=volicy(i2) + a2y
         volicz(i2)=volicz(i2) + a2z
         volicx(i3)=volicx(i3) + a3x
         volicy(i3)=volicy(i3) + a3y
         volicz(i3)=volicz(i3) + a3z
c
      enddo
      do i=1,npoints
         const(i+6*ntets)=0.0
      enddo
      do i=1,n12
         xsendnn(i)=const(isort(i))
      enddo
      do i=1,ncoefs
         amat(i)=0.0
      enddo
      call xsumsp2r(ncoefs,n12,amat,isendnn,xsendnn)
      icount=0
      amatmin=1.0d+30
      amatmax=-amatmin
      do i=1,ncoefs
         amatmin=min(amatmin,amat(i))
         amatmax=max(amatmax,amat(i))
         if(amat(i).lt.0.0) then
            icount=icount+1
            imat(icount)=i
            xmat(icount)=amat(i)
            idxmat(icount)=icount
         endif
      enddo
      if(icount.gt.0) then
         call hpsort1(icount,xmat,one,idxmat)
         jcount=0
         do i=1,icount
            j=imat(idxmat(i))
            if(amat(j).lt.-1.0e-06*abs(amatmax)) then
               jcount=jcount+1
            endif
         enddo
         if(jcount.gt.0) then
            ierr1=1
         else
            ierr1=0
         endif
 
         if(idebug.eq.0) then
            write(logmess,'(a,i9,a,1pe15.7,a,1pe15.7))')
     *         "Matbldtri: total neg",icount,
     *          " mincoef= ",amatmin," maxcoef= ",amatmax
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Negative coeff2d:        sort   edge     vor_coeff
     *  row_node  col_node '
            call writloga('default',0,logmess,0,ierrw)
            do i=1,min(icount,30)
               j=imat(idxmat(i))
               write(logmess,'(a,2i10,1pe15.7,2i10)')
     *           "Negative coeff2d: ",i,j,amat(j),
     *              irowmat(j),icolmat(j)
               call writloga('default',0,logmess,0,ierrw)
            enddo
 
         else
            write(logmess,'(a,i10,2(1pe15.7))')
     *         "Matbldtri: ",icount,amatmin,amatmax
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Negative coeff2d:        sort   edge     vor_coeff
     *  row_node  col_node '
            call writloga('default',0,logmess,0,ierrw)
            do i=1,icount
               j=imat(idxmat(i))
               write(logmess,'(a,2i10,1pe15.7,2i10)')
     *            "Negative coeff2d: ",i,j,amat(j),
     *               irowmat(j),icolmat(j)
               call writloga('default',0,logmess,0,ierrw)
            enddo
         endif
      else
         ierr1=0
      endif
C
C     Calculate the magnitude of the voronoi area vector from the
c     vector contributions. Note that although the
c     signed areas have already been accumulated the calculation
c     below is being done as a check.
c
      do i=1,npoints
         volic_check = volic(i)
         volic(i) = 0.5*sqrt(volicx(i)**2+volicy(i)**2+volicz(i)**2)
         volic_diff = volic_check - volic(i)
         if(volic_diff .gt. 1.e-9*abs(volic(i)))then
C***       print *, "MATBLD2D: ERROR in calculation of volic(i)"
C***       print *,  i,volic_check, volic(i),volic_diff
         write(logmess,'(a,i10,1pe15.7,1pe15.7,1pe15.7)')
     *           "MATBLD2D: ERROR in calculation of volic(i)",
     *            i,volic_check, volic(i),volic_diff
 
         endif
      enddo
C
      volmin=1.0e+30
      volmax=-volmin
      voltotal_vor=0.0d0
      do i=1,npoints
         volmin=min(volmin,volic(i))
         volmax=max(volmax,volic(i))
         voltotal_vor=volic(i) + voltotal_vor
      enddo
      amatmin=1.0e+30
      amatmax=-amatmin
      do i=1,ncoefs
         amatmin=min(amatmin,amat(i))
         amatmax=max(amatmax,amat(i))
      enddo
c
c     Calculate the max number of connections to a single node.
c
      num_conn_max = irowoff(1)
      do i=2, npoints
         num_conn_max = max(num_conn_max,irowoff(i)-irowoff(i-1))
      enddo
c
      write(logmess,'(a)'        )
     1  "------------"
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)'        )
     1  "Matbldtri"
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i10)'    )
     1  "Number of Nodes             = ",npoints
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i10)'    )
     1  "Max. Connection to a Node   = ",num_conn_max
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i10)'    )
     1  "Number of Area Coefficients = ",ncoefs
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Minimum Voronoi area        = ",volmin
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Maximum Voronoi area        = ",volmax
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Minimum Voronoi edge length = ",amatmin
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Maximum Voronoi edge length = ",amatmax
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Total   Voronoi area        = ",voltotal_vor
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)')
     1  "Total   Triangle area       = ",voltotal_vor
      call writloga('default',0,logmess,0,ierrw)
 
      neq=npoints
      neqp1=neq+1
      ncont=neqp1+ncoefs
      iwtotl=ncoefs
C
      length=ncont
      call mmgetblk('itemp',isubname,ipitemp,length,2,icscode)
C
      ifilename=ifile(1:icharlnf(ifile)) // '.stor'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
C
C
C     Get a time stamp for the file header (second line).
C
      string = fdate()
 
C     ASCII header
      write(iunit,'(a)')
     1'fehmstor ascir8i4 X3D Sparse Matrix Voronoi Coupling Coefficents'
      write(iunit,*)
     1     string,' 2-D Linear Diffusion Model (matbld2d_stor)'
      write(title_string,*)
     1     string,' 2-D Linear Diffusion Model (matbld2d_stor)'
C
c
c     FEHM is only using the scalar area coefficients so only output
c     one array nnodes long with the scalar areas. Old version of the
c     code had the scalar areas in the first nnodes entries and then
c     5*nnodes of zeros
c
      narea = 1
      write(iunit,9010) iwtotl, neq, ncont, narea, num_conn_max
C
      write(iunit,9000) (volic(i),i=1,neq)
 9000 format(5(1pe20.12))
C
      do i=1,neq
         itemp(i)=irowoff(i)+neqp1
      enddo
      itemp(neqp1)=irowoff(neq)+irowcnt(neq)+neqp1
      do i=1,ncoefs
         itemp(neqp1+i)=icolmat(i)
      enddo
      write(iunit,9010) (itemp(i),i=1,ncont)
 9010 format(5i10)
C
      do i=1,ncoefs
         itemp(i)=i
      enddo
      do i=1,neqp1
         itemp(ncoefs+i)=0
      enddo
      write(iunit,9010) (itemp(i),i=1,ncont)
C
      write(iunit,9010) (irowdag(i)+neqp1,i=1,neq)
C
      write(iunit,9000) (-amat(i),i=1,ncoefs)
c      write(iunit,9000) (0.0,i=1,ncoefs)
c      write(iunit,9000) (0.0,i=1,ncoefs)
c      write(iunit,9000) (0.0,i=1,ncoefs)
c      write(iunit,9000) (0.0,i=1,ncoefs)
c      write(iunit,9000) (0.0,i=1,ncoefs)
C
      close(iunit)
C
C     ..................................................................
C
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
      subroutine volume_tri_voronoi(
     1                      x1,y1,z1,
     2                      x2,y2,z2,
     3                      x3,y3,z3,
     4                      xv,yv,zv,
     5                      a1x,a1y,a1z,
     6                      a2x,a2y,a2z,
     7                      a3x,a3y,a3z,
     8                      area1,area2,area3,area_tri)
c
C
C#######################################################################
C
C      PURPOSE -
C
C         Calculate the 3 Voronoi area vectors of a Delaunay Triangle
C
C      INPUT ARGUMENTS -
C
C         x1,y1,z1,x2,y2,z2,x3,y3,z3 - Coordinates of triangle vertex
C
C      OUTPUT ARGUMENTS -
C
C         xv,yv,zv                   - Coordinate of Voronoi point
C
C         a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z - Voronoi area vector for
C                                               each vertex of triangle
C         area_tri                            - Area of triangle
C
      implicit none
      real*8 a,b,c,d,e,f,crosx,crosy,crosz,vecmag,
     *  xa,ya,za,xb,yb,zb,xn1,yn1,zn1,xn,yn,zn,rn,dotb3,dot3,
     *  ql,rb3,xl,yl,zl,ds1,ds2,ds3,xv,yv,zv,dotpr,x1,y1,z1,
     *  x2,y2,z2,x3,y3,z3,a2x,a2y,a2z,a3x,a3y,a3z,
     *  area1,area2,area3,area_tri,xd,yd,zd,x12,y12,z12,x13,y13,
     *  z13,x23,y23,z23,xa11,ya11,za11,xa12,ya12,za12,xa22,ya22,
     *  za22,xa31,ya31,za31,xa32,ya32,za32,a123x,a123y,a123z,
     *  area_vor,area_error,a1_sign,a2_sign,a3_sign,xa21,ya21,
     *  za21,a1x,a1y,a1z
c
      character*132 logmess
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
      dotpr(a,b,c,d,e,f)=a*d + b*e + c*f
      vecmag(a,b,c) = 0.5d0*sqrt(a**2 + b**2 + c**2)
c
c     Calculate the Voronoi point
c
      xa = x1
      ya = y1
      za = z1
      xb = x2-x1
      yb = y2-y1
      zb = z2-z1
      xd = x3-x1
      yd = y3-y1
      zd = z3-z1
      xn1=crosx(xb,yb,zb,xd,yd,zd)
      yn1=crosy(xb,yb,zb,xd,yd,zd)
      zn1=crosz(xb,yb,zb,xd,yd,zd)
      xn=crosx(xb,yb,zb,xn1,yn1,zn1)
      yn=crosy(xb,yb,zb,xn1,yn1,zn1)
      zn=crosz(xb,yb,zb,xn1,yn1,zn1)
      rn=1.0d0/sqrt(xn*xn+yn*yn+zn*zn)
      xn=xn*rn
      yn=yn*rn
      zn=zn*rn
      dotb3=xb*xd+yb*yd+zb*zd
      dot3=dotb3/(xd*xd+yd*yd+zd*zd)
      rb3=1.0/(xb*xb+yb*yb+zb*zb)
      ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
      xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
      yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
      zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
      ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
      ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
      ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
      xv=xl+xa
      yv=yl+ya
      zv=zl+za
c
      x12=0.5d0*(x1+x2)
      y12=0.5d0*(y1+y2)
      z12=0.5d0*(z1+z2)
      x13=0.5d0*(x1+x3)
      y13=0.5d0*(y1+y3)
      z13=0.5d0*(z1+z3)
      x23=0.5d0*(x2+x3)
      y23=0.5d0*(y2+y3)
      z23=0.5d0*(z2+z3)
 
      xa11= crosx((x12-x1),(y12-y1),(z12-z1),(xv-x1),(yv-y1),(zv-z1))
      ya11= crosy((x12-x1),(y12-y1),(z12-z1),(xv-x1),(yv-y1),(zv-z1))
      za11= crosz((x12-x1),(y12-y1),(z12-z1),(xv-x1),(yv-y1),(zv-z1))
 
      xa12=-crosx((x13-x1),(y13-y1),(z13-z1),(xv-x1),(yv-y1),(zv-z1))
      ya12=-crosy((x13-x1),(y13-y1),(z13-z1),(xv-x1),(yv-y1),(zv-z1))
      za12=-crosz((x13-x1),(y13-y1),(z13-z1),(xv-x1),(yv-y1),(zv-z1))
 
      xa21= crosx((x23-x2),(y23-y2),(z23-z2),(xv-x2),(yv-y2),(zv-z2))
      ya21= crosy((x23-x2),(y23-y2),(z23-z2),(xv-x2),(yv-y2),(zv-z2))
      za21= crosz((x23-x2),(y23-y2),(z23-z2),(xv-x2),(yv-y2),(zv-z2))
 
      xa22=-crosx((x12-x2),(y12-y2),(z12-z2),(xv-x2),(yv-y2),(zv-z2))
      ya22=-crosy((x12-x2),(y12-y2),(z12-z2),(xv-x2),(yv-y2),(zv-z2))
      za22=-crosz((x12-x2),(y12-y2),(z12-z2),(xv-x2),(yv-y2),(zv-z2))
 
      xa31= crosx((x13-x3),(y13-y3),(z13-z3),(xv-x3),(yv-y3),(zv-z3))
      ya31= crosy((x13-x3),(y13-y3),(z13-z3),(xv-x3),(yv-y3),(zv-z3))
      za31= crosz((x13-x3),(y13-y3),(z13-z3),(xv-x3),(yv-y3),(zv-z3))
 
      xa32=-crosx((x23-x3),(y23-y3),(z23-z3),(xv-x3),(yv-y3),(zv-z3))
      ya32=-crosy((x23-x3),(y23-y3),(z23-z3),(xv-x3),(yv-y3),(zv-z3))
      za32=-crosz((x23-x3),(y23-y3),(z23-z3),(xv-x3),(yv-y3),(zv-z3))
c
c     Area vector of triangle
c
      xa= crosx((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
      ya= crosy((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
      za= crosz((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
c
c     Vector sum of Voronoi area vector for each of three nodes in the triangle
c
      a1x = xa11 + xa12
      a1y = ya11 + ya12
      a1z = za11 + za12
      a2x = xa21 + xa22
      a2y = ya21 + ya22
      a2z = za21 + za22
      a3x = xa31 + xa32
      a3y = ya31 + ya32
      a3z = za31 + za32
c
c     Total area vector of triangle formed by summing 6 voronoi contributions
c
      a123x = a1x + a2x + a3x
      a123y = a1y + a2y + a3y
      a123z = a1z + a2z + a3z
c
c     Comparison of area calculated two different ways
c
      area_vor  =0.5*sqrt(a123x**2+a123y**2+a123z**2)
      area_tri  =0.5*sqrt(xa**2+ya**2+za**2)
      area_error=0.5*sqrt((a123x-xa)**2+(a123y-ya)**2+(a123z-za)**2)
 
      if(area_error .gt. 1.e-9*area_tri)then
C***        print *,'Error Calculating Voronoi Area'
C***        print *,area_vor,area_tri,area_error
        write(logmess,'(a,1pe15.7,1pe15.7,1pe15.7)')
     *        "Error Calculating Voronoi Area",
     *         area_vor,area_tri,area_error
      endif
c
c     Dot product of voronoi area vector with triangle area vector. This
c     quantity will be positive for positive area contributions and negative
c     for negative area contributions
c
      a1_sign = dotpr(a1x,a1y,a1z,xa,ya,za)
      a2_sign = dotpr(a2x,a2y,a2z,xa,ya,za)
      a3_sign = dotpr(a3x,a3y,a3z,xa,ya,za)
c
c     Find the absolute magnitude of the voronoi contributions
c
      area1   = vecmag(a1x,a1y,a1z)
      area2   = vecmag(a2x,a2y,a2z)
      area3   = vecmag(a3x,a3y,a3z)
c
c     Give the area contributions their correct sign
c
      area1 = sign(area1,a1_sign)
      area2 = sign(area2,a2_sign)
      area3 = sign(area3,a3_sign)
      return
      end
