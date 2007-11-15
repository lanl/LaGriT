      subroutine esug2d(cmo,mpary,mpno,ctrl,
     *   action,cfield,climit1,climit2,climit3,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     ESUG2D smooths or performs r-adaption on 2D mesh objects using
C     Elliptic Smoothing for Unstructured Grids.
C
C     INPUT ARGUMENTS -
C
C         cmo - name of current mesh object
C         mpary - array of nodes to be smoothed
C         mpno - length of mpary
C         ctrl - control parameter for controlled smoothing
C         action - type of smoothing to perform
C         cfield - block name of cmo reference field for the
C                  case of r-adaption on a field
C         climit1, -  point index boundaries
C         climit2, climit3
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: esug2d.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   04 Apr 2000 12:04:56   kuprat
CPVCS    Added MBNDRY into argument list.
CPVCS    
CPVCS       Rev 1.1   Tue Feb 15 14:58:04 2000   dcg
CPVCS    fix call to get_global
CPVCS    
CPVCS       Rev 1.0   27 Jan 2000 13:02:42   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.12   Wed Jul 07 13:51:08 1999   kuprat
CPVCS    Replaced call to RAN2 with call to RAN2_LG.
CPVCS
CPVCS       Rev 1.11   Tue Sep 02 23:05:58 1997   kuprat
CPVCS    Changed interpolation scheme to DOTASKX3D call to DOPING.
CPVCS
CPVCS       Rev 1.10   Mon Jul 28 10:38:10 1997   kuprat
CPVCS    Put in 'random' option for 2d grids.
CPVCS
CPVCS       Rev 1.9   Mon Apr 14 16:44:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.8   Thu Mar 20 13:06:54 1997   kuprat
CPVCS    Moved xsave definition down later in the code to
CPVCS    take into account the fact that getnodnod might change
CPVCS    mpary.
CPVCS
CPVCS       Rev 1.7   Wed Mar 19 11:46:44 1997   kuprat
CPVCS    Changed the minimum allowed triangle area to the
CPVCS    maximum of (1) 90% of the input minimum area and
CPVCS    (2) one-millionth of the input maximum triangle area
CPVCS
CPVCS       Rev 1.6   Tue Nov 12 09:29:26 1996   kuprat
CPVCS    We now pass material types to fadpt in 'user' option.
CPVCS
CPVCS
CPVCS       Rev 1.5   Tue Nov 12 09:20:42 1996   kuprat
CPVCS    Added extra argument to fadpt call.
CPVCS
CPVCS       Rev 1.4   Wed May 29 21:39:34 1996   kuprat
CPVCS    We call 'fadpt' at each nodal relaxation now.
CPVCS
CPVCS       Rev 1.3   Tue May 28 21:27:06 1996   kuprat
CPVCS    Allowed adaptive smoothing on user subroutine 'fadpt'.
CPVCS
CPVCS       Rev 1.2   11/16/95 15:21:42   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.1   11/07/95 17:16:56   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.0   08/24/95 16:34:16   kuprat
CPVCS    Initial revision.
C
      implicit none
 
      integer lenptr
      parameter (lenptr=1000000)
 
      character*132 logmess
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      integer imt1(lenptr)
      integer itp1(lenptr)
      integer isn1(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itetclr(lenptr)
      integer itet(3,lenptr)
      integer jtet(3,lenptr)
 
      pointer (ipf, f)
      pointer (ipx, x)
      pointer (ipy, y)
      pointer (ipz, z)
      real*8 f(0:lenptr)
      real*8 x(0:lenptr)
      real*8 y(0:lenptr)
      real*8 z(0:lenptr)
      integer mat(0:lenptr)
 
      pointer (ipnodnod,nodnod), (ipnnfirst,nnfirst),
     &   (ipu,u), (ipv,v), (ipxsave,xsave), (ipysave,ysave),
     &   (ipzsave,zsave), (ipmat,mat), (ipreffield,reffield)
      real*8 tn1(3),u(0:lenptr), v(0:lenptr),eu1(3), ev1(3), tni(3),
     &   xsave(lenptr),ysave(lenptr),zsave(lenptr),reffield(lenptr),
     &   ctrl,time,x1,y1,z1,x2,y2,z2,x3,y3,z3,dcross,d,areamin,
     &   areamax,dotcross,areaminallow,tolconv_sm,rdum,err,
     &   unew,vnew,wsum,distsq,usave,vsave,xold,yold,zold,du,dv,
     &   ran2_lg,err1,areaminafter,areamaxafter
      logical planar,usef,lrandom
      integer nodnod(lenptr),nnfirst(lenptr)
      integer mpary(lenptr),mpno,ierror,nnodes,nelements,length,
     &   icmotype,mbndry,ilen,ityp,icscode,i,ierrw,maxiter_sm,
     &   itypconv_sm,maxdeg,k,icharlnf,iseed,iter,node,ndeg,iout
 
      character*32 cmo,action,cfield,climit1,climit2,climit3,cmo1
      character*32 isubname,blkname,cout
      pointer(ipout,out)
      real*8 out(*)
      character*132 com
      real*8 dbarea,u1,v1,u2,v2,u3,v3
 
c     Statement function DBAREA gives double the area of a triangle
c ordered
c     (counterclockwise) 1,2,3 in the u-v plane.  This means that for a
c     triangle ordered 1,2,3 in x-y-z space, the (r.h. rule) vector
c normal to
c     this triangle with magnitude equal to double the area is given by
c     ( dbarea(y1,z1,y2,z2,y3,z3), dbarea(z1,x1,z2,x2,z3,x3),
c       dbarea(x1,y1,x2,y2,x3,y3) ).
 
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
 
      isubname = 'esug2d'
 
      ierror=0
 
C  get info from mesh object to be smoothed
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
      call cmo_get_info('itetclr',cmo,
     *   ipitetclr,ilen,ityp,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierror)
 
      call get_global('time',ilen,time,
     *   cout,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
 
c  Determine if we are working with a planar or nonplanar triangular
c grid.
      x1=xic(itet(1,1))
      y1=yic(itet(1,1))
      z1=zic(itet(1,1))
      x2=xic(itet(2,1))
      y2=yic(itet(2,1))
      z2=zic(itet(2,1))
      x3=xic(itet(3,1))
      y3=yic(itet(3,1))
      z3=zic(itet(3,1))
      tn1(1)=dbarea(y1,z1,y2,z2,y3,z3)
      tn1(2)=dbarea(z1,x1,z2,x2,z3,x3)
      tn1(3)=dbarea(x1,y1,x2,y2,x3,y3)
      dcross=sqrt(tn1(1)**2+tn1(2)**2+tn1(3)**2)
      tn1(1)=tn1(1)/dcross
      tn1(2)=tn1(2)/dcross
      tn1(3)=tn1(3)/dcross
      eu1(1)=x2-x1
      eu1(2)=y2-y1
      eu1(3)=z2-z1
      d=sqrt(eu1(1)**2+eu1(2)**2+eu1(3)**2)
      eu1(1)=eu1(1)/d
      eu1(2)=eu1(2)/d
      eu1(3)=eu1(3)/d
      x1=0.
      y1=0.
      z1=0.
      x2=tn1(1)
      y2=tn1(2)
      z2=tn1(3)
      x3=eu1(1)
      y3=eu1(2)
      z3=eu1(3)
      ev1(1)=dbarea(y1,z1,y2,z2,y3,z3)
      ev1(2)=dbarea(z1,x1,z2,x2,z3,x3)
      ev1(3)=dbarea(x1,y1,x2,y2,x3,y3)
      planar=.true.
      do i=2,nelements
         x1=xic(itet(1,i))
         y1=yic(itet(1,i))
         z1=zic(itet(1,i))
         x2=xic(itet(2,i))
         y2=yic(itet(2,i))
         z2=zic(itet(2,i))
         x3=xic(itet(3,i))
         y3=yic(itet(3,i))
         z3=zic(itet(3,i))
         tni(1)=dbarea(y1,z1,y2,z2,y3,z3)
         tni(2)=dbarea(z1,x1,z2,x2,z3,x3)
         tni(3)=dbarea(x1,y1,x2,y2,x3,y3)
         dcross=sqrt(tni(1)**2+tni(2)**2+tni(3)**2)
         tni(1)=tni(1)/dcross
         tni(2)=tni(2)/dcross
         tni(3)=tni(3)/dcross
         if ((tni(1)-tn1(1))**2+(tni(2)-tn1(2))**2+
     &      (tni(3)-tn1(3))**2.gt.1.e-10) then
            planar=.false.
            goto 100
         endif
      enddo
 100  continue
      if (.not.planar) then
         ierror=1
         write(logmess,'(a)')
     *      'ESUG2D: not set up to do nonplanar yet.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
c     Work out areas of triangles before smoothing.
 
      areamin=1.d99
      areamax=-1.d99
      do i=1,nelements
         x1=xic(itet(1,i))
         y1=yic(itet(1,i))
         z1=zic(itet(1,i))
         x2=xic(itet(2,i))
         y2=yic(itet(2,i))
         z2=zic(itet(2,i))
         x3=xic(itet(3,i))
         y3=yic(itet(3,i))
         z3=zic(itet(3,i))
         dotcross=dbarea(y1,z1,y2,z2,y3,z3)*tn1(1)+
     *      dbarea(z1,x1,z2,x2,z3,x3)*tn1(2)+
     *      dbarea(x1,y1,x2,y2,x3,y3)*tn1(3)
         areamin=min(areamin,dotcross*0.5)
         areamax=max(areamax,dotcross*0.5)
      enddo
      write(logmess,'(a,e14.7)')
     *   ' Min. area BEFORE smoothing=',areamin
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,e14.7)')
     *   ' Max. area BEFORE smoothing=',areamax
      call writloga('default',0,logmess,0,ierrw)
c      areaminallow=areamin/100.
c      areaminallow=1.e-4
      areaminallow=max(areamin*0.9,areamax*1.e-6)
 
      call cmo_get_info('maxiter_sm',cmo,
     *   maxiter_sm,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('itypconv_sm',cmo,
     *   itypconv_sm,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_attinfo('tolconv_sm',cmo,iout,
     *   tolconv_sm,cout,ipout,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
 
      itypconv_sm=1
      tolconv_sm=.05
 
      write(logmess,'(a,i5)')
     *   'maxiter_sm=',maxiter_sm
c      call writloga('default',0,logmess,0,ierrw)
c      write(logmess,'(a,i5)')
c     *   'itypconv_sm=',itypconv_sm
c      call writloga('default',0,logmess,0,ierrw)
c      write(logmess,'(a,e14.7)')
c     *   'tolconv_sm=',tolconv_sm
c      call writloga('default',0,logmess,0,ierrw)
 
c Obtain ordered node-node relation.  (The node neighbours of all nodes
c are obtained in counter-clockwise order.)
c
c*****CAUTION***** getnodnod may alter mpary and mpno by deleting
c                  nodes with topological inconsistencies.
c
      call mmgetblk('nodnod',isubname,ipnodnod,mpno,2,icscode)
      call mmgetblk('nnfirst',isubname,ipnnfirst,mpno+1,2,icscode)
      call getnodnod(ipnodnod,nnfirst,itet,jtet,mpary,mpno,
     &   nelements,nnodes,mbndry)
 
c If we are using the 'control' option, we need to save the
c node positions of the unsmoothed mesh.
      if (ctrl.ne.0.) then
         call mmgetblk('xsave',isubname,ipxsave,mpno,2,icscode)
         call mmgetblk('ysave',isubname,ipysave,mpno,2,icscode)
         call mmgetblk('zsave',isubname,ipzsave,mpno,2,icscode)
         do i=1,mpno
            xsave(i)=xic(mpary(i))
            ysave(i)=yic(mpary(i))
            zsave(i)=zic(mpary(i))
         enddo
      endif
      if (action(1:13).eq.'field:refresh') then
         cmo1="radapt_cmo1"
         com="cmo/copy/radapt_cmo1/"//cmo//" ; finish"
         call dotaskx3d(com,icscode)
         com='cmo/select/'//cmo//
     &         " ; finish "
         call dotaskx3d(com,icscode)
      endif
 
c U and V will contain, for a node NODE, the U-V coordinates for the
c node and for all its first neighbours.  The 0'th element of the
c U,V arrays corresponds to NODE, while the other elements
c correspond to the neighbours of NODE in counter-clockwise order.
c Hence we need to dimension U,V to have length equal to one plus the
c maximum number of neighbours that any node has in the grid.
      maxdeg=0
      do k=1,mpno
         maxdeg=max(maxdeg,nnfirst(k+1)-nnfirst(k))
      enddo
      call mmgetblk('u',isubname,ipu,maxdeg+1,2,icscode)
      call mmgetblk('v',isubname,ipv,maxdeg+1,2,icscode)
 
      if (action(1:4).eq.'user'.or.
     &   action(1:5).eq.'field') then
         usef=.true.
         call mmgetblk('f',isubname,ipf,maxdeg+1,2,icscode)
         call mmgetblk('x',isubname,ipx,maxdeg+1,2,icscode)
         call mmgetblk('y',isubname,ipy,maxdeg+1,2,icscode)
         call mmgetblk('z',isubname,ipz,maxdeg+1,2,icscode)
         call mmgetblk('mat',isubname,ipmat,maxdeg+1,1,icscode)
         blkname=cfield(1:icharlnf(cfield))
         call cmo_get_info(blkname,cmo,ipreffield,ilen,ityp,icscode)
         if(icscode.ne.0) then
            ierror=icscode
            write(logmess,'(a)')
     *         'ESUG2D: bad reference field'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
      else
         usef=.false.
      endif
 
      if (action(1:6).eq.'random') then
         lrandom=.true.
         iseed=-888
         rdum=ran2_lg(iseed)
      else
         lrandom=.false.
      endif
 
c Smoothing iterations.
      do iter=1,maxiter_sm
         err=0.
         do 20 k=1,mpno
            node=mpary(k)
            ndeg=nnfirst(k+1)-nnfirst(k)
            if (itp1(node).ne.0 .or. ndeg.lt.3) goto 20
 
c Given the x,y,z coordinates of a node and its neighbours, return
c the U-V coordinates.
            call getuv(node,nodnod(nnfirst(k)),ndeg,xic,yic,zic,
     &         eu1,ev1,planar,u,v)
 
c Perform elliptic smoothing.  Every node is relaxed to a position
c equal to a weighted average position of its neighbouring nodes.
c The weight for a given neighbour is equal to the square of the
c distance between the succeeding and preceding neighbours.
c We now find UNEW,VNEW:  the new U,V coordinates of the relaxed
c node.
            unew   = 0.0
            vnew   = 0.0
            wsum   = 0.0
 
            if (action(1:4).eq.'user') then
               x(0)=xic(node)
               y(0)=yic(node)
               z(0)=zic(node)
               do i=1,ndeg
                  x(i)=xic(nodnod(nnfirst(k)+i-1))
                  y(i)=yic(nodnod(nnfirst(k)+i-1))
                  z(i)=zic(nodnod(nnfirst(k)+i-1))
                  mat(i)=imt1(nodnod(nnfirst(k)+i-1))
               enddo
               call fadpt(x,y,z,mat,ndeg+1,time,f)
            elseif (action(1:5).eq.'field') then
               f(0)=reffield(node)
               do i=1,ndeg
                  f(i)=reffield(nodnod(nnfirst(k)+i-1))
               enddo
            endif
 
            if (lrandom) then
               distsq=ran2_lg(iseed)
            else
               distsq = (u(2)-u(ndeg))**2 +
     &            (v(2)-v(ndeg))**2
               if (usef) distsq=distsq+(f(2)-f(ndeg))**2
            endif
 
            unew =unew+ distsq*u(1)
            vnew =vnew+ distsq*v(1)
            wsum = wsum + distsq
 
            if (lrandom) then
               distsq=ran2_lg(iseed)
            else
               distsq = (u(1)-u(ndeg-1))**2 +
     &            (v(1)-v(ndeg-1))**2
               if (usef) distsq=distsq+(f(1)-f(ndeg-1))**2
            endif
 
            unew = unew+ distsq*u(ndeg)
            vnew = vnew+ distsq*v(ndeg)
            wsum = wsum + distsq
 
            do i=2,ndeg-1
               if (lrandom) then
                  distsq=ran2_lg(iseed)
               else
                  distsq = (u(i+1)-u(i-1))**2 +
     &               (v(i+1)-v(i-1))**2
                  if (usef) distsq=distsq+(f(i+1)-f(i-1))**2
               endif
               unew = unew+ distsq*u(i)
               vnew = vnew+ distsq*v(i)
               wsum = wsum + distsq
            enddo
 
            unew = unew/wsum
            vnew = vnew/wsum
 
c If ctrl is nonzero, we let the new U,V coordinates of the
c central node be equal to a linear combination of the
c OLD U,V coordinates and the relaxed U,V coordinates.
            if (ctrl.ne.0.) then
               usave=eu1(1)*xsave(k)+eu1(2)*ysave(k)
     &            +eu1(3)*zsave(k)
               vsave=ev1(1)*xsave(k)+ev1(2)*ysave(k)
     &            +ev1(3)*zsave(k)
               unew = unew*(1.-ctrl)+usave*ctrl
               vnew = vnew*(1.-ctrl)+vsave*ctrl
            endif
 
            xold = xic(node)
            yold = yic(node)
            zold = zic(node)
 
c We convert the new U,V coordinates back to X,Y,Z coordinates.
c The subroutine GETXYZ actually DAMPENS the U,V movement (and
c correspondingly changes the returned X,Y,Z coordinates) if the
c movement would have resulted in a triangle being returned with
c area less than AREAMINALLOW.
            du=unew-u(0)
            dv=vnew-v(0)
            call getxyz(ndeg,xold,yold,zold,du,dv,eu1,ev1,u,v,
     &         xic(node),yic(node),zic(node),areaminallow)
 
            err=err+(xic(node)-xold)**2+
     &         (yic(node)-yold)**2+(zic(node)-zold)**2
 20      continue
         err=sqrt(err/mpno)
 
         write(logmess,'(a,i4,a,e14.7)')
     *      'Iteration=',iter,'; root mean square error=',err
         call writloga('default',0,logmess,0,ierrw)
 
         if (action(1:13).eq.'field:refresh') then
            com="doping/table/"//
     &         cfield(1:icharlnf(cfield))//
     &         "/set/"//
     &         climit1(1:icharlnf(climit1))//
     &         ","//
     &         climit2(1:icharlnf(climit2))//
     &         ","//
     &         climit3(1:icharlnf(climit3))//
     &         "/radapt_cmo1/"//
     &         cfield(1:icharlnf(cfield))//
     &         " ; finish "
            call dotaskx3d(com,icscode)
 
C...  refresh pointers
 
            call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierror)
            call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
            call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierror)
            call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
            call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
            call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
            call cmo_get_info('itetclr',cmo,
     *         ipitetclr,ilen,ityp,ierror)
            call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierror)
            call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierror)
            blkname=cfield(1:icharlnf(cfield))
            call cmo_get_info(blkname,cmo,ipreffield,ilen,ityp,icscode)
            if(icscode.ne.0) then
               ierror=icscode
               write(logmess,'(a)')
     *            'ESUG2D: bad reference field'
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
            endif
 
         endif
 
         if (iter.eq.1) then
            err1=err
         else
            if (err.le.err1*.01) goto 9000
         endif
      enddo
 
c     Work out areas of smoothed triangles.
 
 9000 continue
      areaminafter=1.d99
      areamaxafter=-1.d99
      do i=1,nelements
         x1=xic(itet(1,i))
         y1=yic(itet(1,i))
         z1=zic(itet(1,i))
         x2=xic(itet(2,i))
         y2=yic(itet(2,i))
         z2=zic(itet(2,i))
         x3=xic(itet(3,i))
         y3=yic(itet(3,i))
         z3=zic(itet(3,i))
         dotcross=dbarea(y1,z1,y2,z2,y3,z3)*tn1(1)+
     *      dbarea(z1,x1,z2,x2,z3,x3)*tn1(2)+
     *      dbarea(x1,y1,x2,y2,x3,y3)*tn1(3)
         areaminafter=min(areaminafter,dotcross*0.5)
         areamaxafter=max(areamaxafter,dotcross*0.5)
      enddo
      write(logmess,'(a,e14.7)')
     *   ' Min. area AFTER smoothing=',areaminafter
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,e14.7)')
     *   ' Max. area AFTER smoothing=',areamaxafter
      call writloga('default',0,logmess,0,ierrw)
 
      if (action(1:13).eq.'field:refresh') then
         com='cmo/release/radapt_cmo1'//
     &         " ; finish "
         call dotaskx3d(com,icscode)
         com='cmo/select/'//cmo//
     &         " ; finish "
         call dotaskx3d(com,icscode)
      endif
 
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
