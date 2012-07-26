*dk,matbld0tri
      subroutine matbld0tri(nneg,ipineg,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: matbld0tri.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.18   08 Feb 2006 14:38:12   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.17   Thu Apr 06 09:12:06 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.16   Wed Apr 05 13:34:36 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.15   Tue Nov 02 09:25:32 1999   dcg
CPVCS    fix bad declarations
CPVCS
CPVCS       Rev 1.14   Mon Aug 30 15:14:40 1999   dcg
CPVCS    remove calls to ssort routines replace with hpsort
CPVCS
CPVCS       Rev 1.13   Mon Jul 12 16:39:34 1999   tam
CPVCS    assigned idebug, reduced screen output, removed print*
CPVCS
CPVCS       Rev 1.12   Fri Mar 19 09:24:46 1999   gable
CPVCS    Modified message output when reporting negative coefficients.
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 16:52:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Wed Nov 13 12:40:00 1996   dcg
CPVCS    comment out debug print loop
CPVCS
CPVCS       Rev 1.9   Mon Nov 11 21:01:02 1996   het
CPVCS    Initialize an unitialized variable.
CPVCS
CPVCS       Rev 1.6   Tue Jan 23 09:16:06 1996   het
CPVCS    Add an epsilon to the Voronoi point calculation
CPVCS
CPVCS       Rev 1.5   Wed Jan 03 10:07:28 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.4   11/07/95 17:20:00   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.3   08/15/95 18:20:16   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.2   06/27/95 11:11:30   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.1   05/30/95 07:51:02   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.0   11/10/94 12:16:04   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
C#######################################################################
C
C      PURPOSE -
C
C         BUILD A SPARSE MATRIX THAT CAN BE FED TO AN ITERATIVE SOLVER.
C
C      INPUT ARGUMENTS -
C
C         npoints   - NUMBER OF POINTS IN THE MESH.
C         ntets     - NUMBER OF TETS IN THE MESH.
C
C      OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C         HTMMDDAA-YY
C
C#######################################################################
C
      include "local_element.h"
      include "consts.h"

C arguments (nneg,ipineg,ierr1)
      integer nneg,ierr1
      pointer (ipineg, ineg)
      integer ineg(2,*)

C variables
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer imt1(*), itp1(*), isn1(*)

      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet(3,*), jtet(3,*)
      integer itet1(*), jtet1(*)

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*),    yic(*),  zic(*)

      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipitetp, itetp1)
      integer itetp1(*)
      pointer (ipitetp, itetp)
      integer itetp(3,*)
C
      pointer (ipisort, isort)
      integer isort(*)

      pointer (ipicolmat, icolmat)
      pointer (ipirowmat, irowmat)
      integer icolmat(*), irowmat(*)
      pointer (ipirowcnt, irowcnt)
      pointer (ipirowoff, irowoff)
      integer irowcnt(*), irowoff(*)
      pointer (ipnmat, nmat)
      integer nmat(*)
      pointer (ipimat, imat)
      integer imat(*)
      pointer (ipidxmat, idxmat)
      integer idxmat(*)

      pointer (ipisendnn, isendnn)
      integer isendnn(*)
      pointer (ipxsendnn, xsendnn)
      real*8 xsendnn(*)
C
      pointer (ipamat, amat)
      real*8 amat(*)
      pointer (ipxmat, xmat)
      real*8 xmat(*)
      pointer (ipconst, const)
      real*8 const(*)

      real*8 cvx,cvy,cvz,xdot,amatmax,amatmin,xdot1,xdot2,xdot3,
     * xarea1,xarea2,xarea3,ds1,ds2,ds3,x12,y12,z12,x13,y13,z13,
     * x23,y23,z23,ql,xl,yl,zl,dot3,rn,dotb3,xn,yn,zn,xn1,yn1,
     * zn1,xd,yd,zd,xb,yb,zb,xfac,za,ya,xa,xv,yv,zv,xm,ym,zm,
     * x2,y2,z2,x3,y3,z3,x1,y1,z1,rb3,cmx,cmy,cmz
C

      integer ierror,ierr,ityp,ilen,npoints,ntets,
     *  ipackopt,idiag

      integer idebug
      integer nconn,length,indxmin,ncoefs,i1,i2,i3,it,index,i,
     *  nen,nef,n12,itype,icscode,nsd,nnmax,indxmax,ierrw,
     *  icount,j, maxineg

      parameter (nconn=6)
      integer lconn(2,nconn)
      data lconn / 1, 2,
     *             2, 1,
     *             1, 3,
     *             3, 1,
     *             2, 3,
     *             3, 2 /
C
      real*8 crosx,crosy,crosz,a,b,c,d,e,f,xfactri
      data xfactri / 1.0d-06 /
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
      character*132 logmess
      character*32 cmo, isubname
C
C#######################################################################
C BEGIN begin
C
      isubname='matbld0tri'
 
C
C     ******************************************************************
C     GET CMO INFORMATION
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierr)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
 
      call cmo_get_info('idebug',cmo,idebug,ilen,ityp,ierr)
      if(idebug.ne.0) then
        write(logmess,'(a,i5)')"matbl0tri idebug set to: ",idebug
        call writloga('default',0,logmess,0,ierr)
      endif

C     calculate max length of work array passed in from reconloop2d
      maxineg = (nelmnee(ifelmtri)*ntets)+100
 
C
C     ******************************************************************
C
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
      length=npoints
      call mmgetblk('irowcnt',isubname,ipirowcnt,length,1,icscode)
      call mmgetblk('irowoff',isubname,ipirowoff,length,1,icscode)
      length=nconn*ntets+idiag*npoints
      call mmgetblk('isort',isubname,ipisort,length,1,icscode)
      call mmgetblk('icolmat',isubname,ipicolmat,length,1,icscode)
      call mmgetblk('irowmat',isubname,ipirowmat,length,1,icscode)
      call mmgetblk('isendnn',isubname,ipisendnn,length,1,icscode)
      call mmgetblk('xsendnn',isubname,ipxsendnn,length,2,icscode)
C
      length=npoints
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      length=3*ntets
      call mmgetblk("itetp",isubname,ipitetp,length,1,icscode)
C
C
C     ..................................................................
C     FIND THE PARENTS OF EACH NODE.
C
      call unpackpc(npoints,itp1,isn1,iparent)
C
      do it=1,ntets
         index=3*(it-1)
         do i=1,3
C***********itetp1(index+i)=iparent(itet1(index+i))
            itetp1(index+i)=itet1(index+i)
         enddo
      enddo
C
C
C     ..................................................................
C     BUILD SOME MATRIX ARRAYS FROM THE TET LIST.
C
      do i=1,n12
         irowmat(i)=0
         icolmat(i)=0
         isendnn(i)=0
      enddo
      do i=1,npoints
         irowcnt(i)=0
         irowoff(i)=0
      enddo
      nen=3
      nef=3
      call matbld1(idiag,ipackopt,
     *             nsd,nen,nef,
     *             nconn,lconn,
     *             npoints,ntets,itetp1,
     *             irowmat,icolmat,isort,isendnn,
     *             irowcnt,irowoff)
c
c
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
c
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
      call mmgetblk('nmat',isubname,ipnmat,length,1,icscode)
      call mmgetblk('amat',isubname,ipamat,length,2,icscode)
      call mmgetblk('imat',isubname,ipimat,length,1,icscode)
      call mmgetblk('xmat',isubname,ipxmat,length,2,icscode)
      call mmgetblk('idxmat',isubname,ipidxmat,length,1,icscode)
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
C
      length=n12
      call mmgetblk('const',isubname,ipconst,length,2,icscode)
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
         xm=(xic(i1)+xic(i2)+xic(i3))/3.0
         ym=(yic(i1)+yic(i2)+yic(i3))/3.0
         zm=(zic(i1)+zic(i2)+zic(i3))/3.0
         xv=xm
         yv=ym
         zv=zm
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
         xfac=1.0
         xb=xfac*(xic(i2)-xa)
         yb=xfac*(yic(i2)-ya)
         zb=xfac*(zic(i2)-za)
         xd=xfac*(xic(i3)-xa)
         yd=xfac*(yic(i3)-ya)
         zd=xfac*(zic(i3)-za)
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
         cmx=  (y2-y1)*(zm-z1)-(ym-y1)*(z2-z1)
         cmy=-((x2-x1)*(zm-z1)-(xm-x1)*(z2-z1))
         cmz=  (x2-x1)*(ym-y1)-(xm-x1)*(y2-y1)
         cvx=  (y2-y1)*(zv-z1)-(yv-y1)*(z2-z1)
         cvy=-((x2-x1)*(zv-z1)-(xv-x1)*(z2-z1))
         cvz=  (x2-x1)*(yv-y1)-(xv-x1)*(y2-y1)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it      )=-xarea1
            const(it+ntets)=-xarea1
C*****      print *,'Negative area 1: ',it,i1,i2,xdot1,xarea1,xdot
         else
            const(it       )= xarea1
            const(it+ntets)= xarea1
         endif
         cmx=  (y1-y3)*(zm-z3)-(ym-y3)*(z1-z3)
         cmy=-((x1-x3)*(zm-z3)-(xm-x3)*(z1-z3))
         cmz=  (x1-x3)*(ym-y3)-(xm-x3)*(y1-y3)
         cvx=  (y1-y3)*(zv-z3)-(yv-y3)*(z1-z3)
         cvy=-((x1-x3)*(zv-z3)-(xv-x3)*(z1-z3))
         cvz=  (x1-x3)*(yv-y3)-(xv-x3)*(y1-y3)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it+2*ntets)=-xarea2
            const(it+3*ntets)=-xarea2
C*****      print *,'Negative area 2: ',it,i1,i3,xdot2,xarea2,xdot
         else
            const(it+2*ntets)= xarea2
            const(it+3*ntets)= xarea2
         endif
         cmx=  (y3-y2)*(zm-z2)-(ym-y2)*(z3-z2)
         cmy=-((x3-x2)*(zm-z2)-(xm-x2)*(z3-z2))
         cmz=  (x3-x2)*(ym-y2)-(xm-x2)*(y3-y2)
         cvx=  (y3-y2)*(zv-z2)-(yv-y2)*(z3-z2)
         cvy=-((x3-x2)*(zv-z2)-(xv-x2)*(z3-z2))
         cvz=  (x3-x2)*(yv-y2)-(xv-x2)*(y3-y2)
         xdot=cmx*cvx+cmy*cvy+cmz*cvz
         if(xdot.lt.0.0) then
            const(it+4*ntets)=-xarea3
            const(it+5*ntets)=-xarea3
C*****      print *,'Negative area 3: ',it,i2,i3,xdot3,xarea3,xdot
         else
            const(it+4*ntets)= xarea3
            const(it+5*ntets)= xarea3
         endif
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
      amatmin=1.0d+30
      amatmax=-amatmin
      do i=1,ncoefs
         if(amat(i).lt.amatmin) then
            indxmin=i
            amatmin=amat(i)
         endif
         if(amat(i).gt.amatmax) then
            indxmax=i
            amatmax=amat(i)
         endif
      enddo

C*****
C     write Min and Max value and index of isendnn
      do i=1,n12
         if(isendnn(i).eq.indxmin) then
           write(logmess,'(a,i10,i10)') 'Min send: ',indxmin,i
           call writloga('tty',0,logmess,0,ierrw)
         endif
         if(isendnn(i).eq.indxmax) then
           write(logmess,'(a,i10,i10)') 'Max send: ',indxmax,i
           call writloga('tty',0,logmess,0,ierrw)
         endif
      enddo
C     do i=1,ncoefs
C        print *,i,irowmat(i),icolmat(i),amat(i)
C     enddo
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
      nneg=0
      if(icount.gt.0) then
         call hpsort1(icount,xmat,one,idxmat)
         if (icount .gt. maxineg) then
            call x3d_error(isubname,'ineg array too short.') 
            ierr1 = 1
            goto 9999
         endif
         do i=1,icount
            j=imat(idxmat(i))
            if(amat(j).lt.-xfactri*abs(amatmax)) then
               nneg=nneg+1
               ineg(1,nneg)=irowmat(j)
               ineg(2,nneg)=icolmat(j)
            endif
         enddo
         if(nneg.gt.0) then
            ierr1=1
         else
            ierr1=0
         endif
         if(idebug.eq.0) then
           write(logmess,'(a,i10,a,1pe15.7,a,1pe15.7)')
     *         'Matbld0tri: total neg',icount,
     *          ' mincoef= ',amatmin,' maxcoef= ',amatmax
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Negative coeff2d:        sort   edge     vor_coeff
     *  row_node  col_node '
            call writloga('default',0,logmess,0,ierrw)
            do i=1,min(icount,30)
               j=imat(idxmat(i))
               write(logmess,'(a,2i10,1pe15.7,2i10)')
     *           'Negative coeff2d: ',i,j,amat(j),
     *              irowmat(j),icolmat(j)
               call writloga('default',0,logmess,0,ierrw)
            enddo
         else
            write(logmess,'(a,i9,a,1pe15.7,a,1pe15.7)')
     *         'Matbld0tri: total neg',icount,
     *          ' mincoef= ',amatmin,' maxcoef= ',amatmax
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Negative coeff2d:        sort   edge     vor_coeff
     *  row_node  col_node '
            do i=1,icount
               j=imat(idxmat(i))
               write(logmess,'(a,2i10,1pe15.7,2i10)')
     *            'Negative coeff2d: ',i,j,amat(j),
     *               irowmat(j),icolmat(j)
               call writloga('default',0,logmess,0,ierrw)
            enddo
         endif
      else
         nneg=0
         ierr1=0
      endif
C
C
C     ..................................................................
C
C
      goto 9999
 9999 continue
      
      if (idebug.gt.1) then
         print*,isubname,' mmprint() '
         call mmprint()
      endif
      call mmrelprt(isubname,icscode)
      return
      end
