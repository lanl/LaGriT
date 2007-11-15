      subroutine nurbl(ioption,
     *                 k1,k1points,
     *                 m1,ia,s,w,x,y,z,v,
     *                 irow,ipt,ict,icttot,
     *                 npoints,ntets,nbpoints,nbtets,
     *                 itoff,jtoff)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE PROCESSES AN IGES TYPE "126" ENTITY (A CURVE).
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C      $Log: nurbl.f,v $
C      Revision 2.00  2007/11/05 19:46:02  spchu
C      Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   08 Feb 2006 14:35:36   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.1   30 Sep 2004 09:18:52   dcg

CPVCS    replace calls to real( with calls to dble(

CPVCS    

CPVCS       Rev 1.0   27 Jan 2000 12:30:52   dcg

CPVCS    Initial revision.

CPVCS
CPVCS       Rev 1.5   Fri Oct 23 13:11:34 1998   dcg
CPVCS    declare k1, ia before use - DEC compiler complaint
CPVCS
CPVCS       Rev 1.4   Fri Aug 28 14:24:58 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:55:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu Oct 10 08:41:52 1996   het
CPVCS    Do an automatic addatt for "vels" to contain normal directions.
CPVCS
CPVCS       Rev 1.1   Thu Jun 27 14:52:30 1996   het
CPVCS    Put unit normals into the vels array for each NURB.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 15:20:22 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C
C ######################################################################
C
      integer ia,k1
      real*8 s(ia)
      real*8 w(k1), x(k1), y(k1), z(k1)
      real*8 v(*)
C
      pointer (ipxs, xs(2,1000000))
      pointer (ipbs, bs(ia,1000000))
      pointer (ipgx, gx(k1))
      pointer (ipgy, gy(k1))
      pointer (ipgz, gz(k1))
C
      character*32 cmo
      character*32 isubname
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer itp1(10000000), imt1(10000000)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
      pointer(ipvels,vels)
      real*8 vels(3,1000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(4*1000000), jtet1(4*1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      character*32 cvelnm
      character*8092 cbuff
      real*8 bs,gx,gy,gz,epsilon,smin,smax,ds1,s1,term1,term2,
     *  bsum,xsum,ysum,zsum,x1,y1,z1,x2,y2,z2,dx1,dx,xf1,xf2,
     *  xs,rout
      integer it,ityp,i1,i2,nnodesmm,ntetsinc,inc,iout,lout,
     *  nelementsmm,ierr,npointsinc,npsave,ntetsave,j,ik1,ks,js,
     *  mbndry,ioption,k1points,irow,ipt
      integer npoints,length,icmotype,ierror,itin,lin,m1,
     *  icscode,ict,icttot,ntets,nbpoints,nbtets,
     *  itoff,jtoff,k1point,i,nx,ix,k,ilen
      pointer(ipout,out)
      real*8 out(*)
      real*8 alargenumber
      parameter (alargenumber=1.d+99)
C
C ######################################################################
C
C
      data epsilon / 1.0d-10 /
C
C ######################################################################
C
      isubname="nurbl"
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      if(k1points.le.0) then
         k1point=k1
      else
         k1point=k1points
      endif
C
      length=2*ia
      call mmgetblk("xs",isubname,ipxs,length,2,icscode)
      length=ia*(m1+1)
      call mmgetblk("bs",isubname,ipbs,length,2,icscode)
      length=k1point
      call mmgetblk("gx",isubname,ipgx,length,2,icscode)
      call mmgetblk("gy",isubname,ipgy,length,2,icscode)
      call mmgetblk("gz",isubname,ipgz,length,2,icscode)
C
      smin=alargenumber
      smax=-smin
      js=0
      ks=0
      do 100 i=1,ia-1
C*****   if(s(i).ne.s(i+1)) then
         if(abs(s(i)-s(i+1)).gt.epsilon) then
            js=js+1
            xs(1,js)=s(i)
            xs(2,js)=s(i+1)
            smin=min(smin,xs(1,js),xs(2,js))
            smax=max(smax,xs(1,js),xs(2,js))
         else
            ks=ks+1
         endif
 100  continue
      ds1=(v(2)-v(1))/(k1point-1)
      s1=v(1)-ds1
      do 240 ik1=1,k1point
         s1=s1+ds1
         do 245 j=1,m1+1
            do 246 i=1,ia
               bs(i,j)=0.0
 246        continue
 245     continue
         do 250 i=1,ia-1
C*****      if(s1.ge.s(i).and.s1.le.s(i+1).and.s(i).ne.s(i+1)) then
            if((s1-s(i)).gt.-epsilon .and.
     *         (s1-s(i+1)).lt.epsilon .and.
     *         abs(s(i)-s(i+1)).gt.epsilon) then
               bs(i,1)=1
            else
               bs(i,1)=0
            endif
 250     continue
         do 260 j=1,m1
            do 270 i=1,ia-j-1
C*****         if(s(i+j).eq.s(i)) then
               if(abs(s(i+j)-s(i)).lt.epsilon) then
                  term1=0.0
               else
                  term1=(s1-s(i))/(s(i+j)-s(i))
               endif
C*****         if(s(i+j+1).eq.s(i+1)) then
               if(abs(s(i+j+1)-s(i+1)).lt.epsilon) then
                  term2=0.0
               else
                  term2=(s(i+j+1)-s1)/(s(i+j+1)-s(i+1))
               endif
               bs(i,j+1)=term1*bs(i,j)+term2*bs(i+1,j)
 270        continue
 260     continue
         bsum=0.0
         xsum=0.0
         ysum=0.0
         zsum=0.0
         do 310 i=1,k1
            bsum=bsum+w(i)*bs(i,m1+1)
            xsum=xsum+w(i)*x(i)*bs(i,m1+1)
            ysum=ysum+w(i)*y(i)*bs(i,m1+1)
            zsum=zsum+w(i)*z(i)*bs(i,m1+1)
 310     continue
         if(abs(bsum).lt.epsilon) then
            gx(ik1)=0.0
            gy(ik1)=0.0
            gz(ik1)=0.0
         else
            gx(ik1)=xsum/bsum
            gy(ik1)=ysum/bsum
            gz(ik1)=zsum/bsum
         endif
 240  continue
      call cmo_get_info('itp1',cmo,
     *                  ipitp1,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,
     *                  ipimt1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,
     *                  ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,
     *                  ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,
     *                  ipzic,length,icmotype,ierror)
      ntetsave=ntets
      do 510 i=1,k1point-1
         x1=gx(i)
         y1=gy(i)
         z1=gz(i)
         x2=gx(i+1)
         y2=gy(i+1)
         z2=gz(i+1)
         npsave=npoints
         nx=1
         dx1=1.0/dble(nx)
         dx=-dx1
         do 530 ix=1,nx+1
            dx=dx+dx1
            xf1=(1.0-dx)
            xf2=dx
            npoints=npoints+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((npoints+1).gt.length) then
               npointsinc=npoints+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierr)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,
     *                           nelementsmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('itp1',cmo,
     *                           ipitp1,length,icmotype,ierror)
               call cmo_get_info('imt1',cmo,
     *                           ipimt1,length,icmotype,ierror)
               call cmo_get_info('xic',cmo,
     *                           ipxic,length,icmotype,ierror)
               call cmo_get_info('yic',cmo,
     *                           ipyic,length,icmotype,ierror)
               call cmo_get_info('zic',cmo,
     *                           ipzic,length,icmotype,ierror)
            endif
            imt1(npoints)=1+mod(irow-1,64)
            itp1(npoints)=0
            xic(npoints)=xf1*x1+xf2*x2
            yic(npoints)=xf1*y1+xf2*y2
            zic(npoints)=xf1*z1+xf2*z2
 530     continue
         do 560 k=1,nx
            call mmgetlen(ipitetclr,length,icscode)
            if((ntets+1).gt.length) then
               inc=1000
               ntetsinc=ntets+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierr)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,length,icmotype,ierror)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,length,icmotype,ierror)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,length,icmotype,ierror)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,length,icmotype,ierror)
               call cmo_get_info('itet',cmo,
     *                           ipitet,length,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,length,icmotype,ierror)
            endif
            i1=k+npsave
            i2=k+1+npsave
            ntets=ntets+1
            itetclr(ntets)=imt1(i1)
            itettyp(ntets)=2
            itetoff(ntets)=itoff
            jtetoff(ntets)=jtoff
            itoff=itoff+2
            jtoff=jtoff+2
            itet1(1+itetoff(ntets))=i1
            itet1(2+itetoff(ntets))=i2
            jtet1(1+jtetoff(ntets))=-1
            jtet1(2+jtetoff(ntets))=-1
 560      continue
 510  continue
 
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *   ipout,lout,ityp,ierror)
      if(ierror.ne.0) cvelnm='vels'
      call cmo_get_info(cvelnm,cmo,ipvels,ilen,ityp,ierr)
      if(ierr.ne.0) then
         cbuff ='cmo/addatt/-def-/vels/VDOUBLE/vector/' //
     *          'nnodes/linear/permanent/gxa/0.0 ;  ' //
     *          'finish'
         call dotaskx3d(cbuff,ierror)
      endif
      call cmo_get_info(cvelnm,cmo,ipvels,lin,itin,ierror)
      do it=ntetsave+1,ntets
         i1=itet1(itetoff(it)+1)
         i2=itet1(itetoff(it)+2)
         vels(1,i1)=vels(1,i1)+(xic(i2)-xic(i1))
         vels(2,i1)=vels(2,i1)+(yic(i2)-yic(i1))
         vels(3,i1)=vels(3,i1)+(zic(i2)-zic(i1))
         vels(1,i2)=vels(1,i2)+(xic(i2)-xic(i1))
         vels(2,i2)=vels(2,i2)+(yic(i2)-yic(i1))
         vels(3,i2)=vels(3,i2)+(zic(i2)-zic(i1))
      enddo
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call mmrelprt(isubname,icscode)
      goto 9999
 9999 continue
      return
      end
