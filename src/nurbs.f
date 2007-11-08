      subroutine nurbs(ioption,
     *                 k1,k2,k1points,k2points,
     *                 m1,m2,ia,ib,s,t,w,x,y,z,u,v,
     *                 irow,ipt,ict,icttot,
     *                 npoints,ntets,nbpoints,nbtets,
     *                 itoff,jtoff)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE PROCESSES AN IGES TYPE "128" (A SURFACE).
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C      $Log: nurbs.f,v $
C      Revision 2.00  2007/11/05 19:46:02  spchu
C      Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   08 Feb 2006 14:35:34   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.2   30 Sep 2004 09:19:42   dcg

CPVCS    replace calls to real( with calls to dble(

CPVCS    

CPVCS       Rev 1.1   03 Feb 2000 12:32:22   dcg

CPVCS    

CPVCS       Rev 1.0   27 Jan 2000 12:30:58   dcg

CPVCS    Initial revision.

CPVCS
CPVCS       Rev 1.4   Fri Aug 28 14:25:00 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:55:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu Oct 10 08:42:40 1996   het
CPVCS    Do an automatic addatt for "vels" to contain normal directions.
CPVCS
CPVCS       Rev 1.1   Thu Jun 27 14:51:56 1996   het
CPVCS    Put unit normals into the vels array for each NURB.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 15:20:20 1996   dcg
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit none
      integer ioption,k1,k2,k1points,k2points,m1,m2,ia,ib,irow,
     *   ipt,ict,icttot,npoints,ntets,nbpoints,nbtets,itoff,jtoff,
     *  icmotype,ierror,length,lin,itin
      real*8 xarea,yarea,zarea,xf1,xf2,xf4,x1,y1,z1,
     * x2,y2,z2,x3,y3,z3,x4,y4,z4,xsum,ysum,zsum,bsum,term1,term2
      integer it,i1,i2,i3,nnodesmm,ntetsinc,inc,ninc,k,l,
     *   nelementsmm,npointsinc,idum,jdum,ilen,ityp,npsave,ntetsave
      real*8 xs,xt,bs,bt,gx,gy,gz,epsilon,smin,smax,tmin,tmax,
     *  ds1,dt1,t1,s1,dx1,dy1,dy,dx,xf3,rout
      integer ix,iy,ij,ip1j,ijp1,ip1jp1,j,ik2,kt,jt,i,js,ks,iout,lout,
     *  nxdum,mbndry,k1point,k2point,icscode,nx,ny,i4,ik1
      pointer(ipout,out)
      real*8 out(*)
      real*8 alargenumber
      parameter (alargenumber=1.0d+99)
C
C
C ######################################################################
C
      real*8 s(ia), t(ib)
      real*8 w(k1,k2), x(k1,k2), y(k1,k2), z(k1,k2)
      real*8 u(*), v(*)
C
      pointer (ipxs, xs(2,1000000))
      pointer (ipxt, xt(2,1000000))
      pointer (ipbs, bs(ia,1000000))
      pointer (ipbt, bt(ib,1000000))
      pointer (ipgx, gx(1000000))
      pointer (ipgy, gy(1000000))
      pointer (ipgz, gz(1000000))
C
      character*32 cmo
      character*32 isubname
      character*32 cvelnm
      character*8092 cbuff
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
      integer ijtrg,ijtet
C
C ######################################################################
C
C
      data epsilon / 1.0d-10 /
C
C ######################################################################
C
      ijtet(idum,jdum,nxdum)=idum+(jdum-1)*(nxdum+1) + npsave
      ijtrg(idum,jdum,nxdum)=2*(idum-1+(jdum-1)*nxdum) + ntetsave
C
C ######################################################################
C
C
      isubname="nurbs"
C
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
      if(k2points.le.0) then
         k2point=k2
      else
         k2point=k2points
      endif
C
      length=2*ia
      call mmgetblk("xs",isubname,ipxs,length,2,icscode)
      length=2*ib
      call mmgetblk("xt",isubname,ipxt,length,2,icscode)
      length=ia*(m1+1)
      call mmgetblk("bs",isubname,ipbs,length,2,icscode)
      length=ib*(m2+1)
      call mmgetblk("bt",isubname,ipbt,length,2,icscode)
      length=k1point*k2point
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
      tmin=alargenumber
      tmax=-tmin
      jt=0
      kt=0
      do 110 i=1,ib-1
C*****   if(t(i).ne.t(i+1)) then
         if(abs(t(i)-t(i+1)).gt.epsilon) then
            jt=jt+1
            xt(1,jt)=t(i)
            xt(2,jt)=t(i+1)
            tmin=min(tmin,xt(1,jt),xt(2,jt))
            tmax=max(tmax,xt(1,jt),xt(2,jt))
         else
            kt=kt+1
         endif
 110  continue
      ds1=(u(2)-u(1))/(k1point-1)
      dt1=(v(2)-v(1))/(k2point-1)
      t1=v(1)-dt1
      do 200 ik2=1,k2point
         t1=t1+dt1
         do 205 j=1,m2+1
            do 206 i=1,ib
               bt(i,j)=0.0
 206        continue
 205     continue
         do 210 i=1,ib-1
C*****      if(t1.ge.t(i).and.t1.le.t(i+1).and.t(i).ne.t(i+1)) then
            if((t1-t(i)).gt.-epsilon .and.
     *         (t1-t(i+1)).lt.epsilon .and.
     *         abs(t(i)-t(i+1)).gt.epsilon) then
               bt(i,1)=1
            else
               bt(i,1)=0
            endif
 210     continue
         do 220 j=1,m2
            do 230 i=1,ib-j-1
C*****         if(t(i+j).eq.t(i)) then
               if(abs(t(i+j)-t(i)).lt.epsilon) then
                  term1=0.0
               else
                  term1=(t1-t(i))/(t(i+j)-t(i))
               endif
C*****         if(t(i+j+1).eq.t(i+1)) then
               if(abs(t(i+j+1)-t(i+1)).lt.epsilon) then
                  term2=0.0
               else
                  term2=(t(i+j+1)-t1)/(t(i+j+1)-t(i+1))
               endif
               bt(i,j+1)=term1*bt(i,j)+term2*bt(i+1,j)
 230        continue
 220     continue
         s1=u(1)-ds1
         do 240 ik1=1,k1point
            s1=s1+ds1
            do 245 j=1,m1+1
               do 246 i=1,ia
                  bs(i,j)=0.0
 246           continue
 245        continue
            do 250 i=1,ia-1
C*****         if(s1.ge.s(i).and.s1.le.s(i+1).and.s(i).ne.s(i+1)) then
               if((s1-s(i)).gt.-epsilon .and.
     *            (s1-s(i+1)).lt.epsilon .and.
     *            abs(s(i)-s(i+1)).gt.epsilon) then
                  bs(i,1)=1
               else
                  bs(i,1)=0
               endif
 250        continue
            do 260 j=1,m1
               do 270 i=1,ia-j-1
C*****            if(s(i+j).eq.s(i)) then
                  if(abs(s(i+j)-s(i)).lt.epsilon) then
                     term1=0.0
                  else
                     term1=(s1-s(i))/(s(i+j)-s(i))
                  endif
C*****            if(s(i+j+1).eq.s(i+1)) then
                  if(abs(s(i+j+1)-s(i+1)).lt.epsilon) then
                     term2=0.0
                  else
                     term2=(s(i+j+1)-s1)/(s(i+j+1)-s(i+1))
                  endif
                  bs(i,j+1)=term1*bs(i,j)+term2*bs(i+1,j)
 270           continue
 260        continue
            bsum=0.0
            xsum=0.0
            ysum=0.0
            zsum=0.0
            do 300 j=1,k2
               do 310 i=1,k1
                  bsum=bsum+w(i,j)*bt(j,m2+1)*bs(i,m1+1)
                  xsum=xsum+w(i,j)*x(i,j)*bt(j,m2+1)*bs(i,m1+1)
                  ysum=ysum+w(i,j)*y(i,j)*bt(j,m2+1)*bs(i,m1+1)
                  zsum=zsum+w(i,j)*z(i,j)*bt(j,m2+1)*bs(i,m1+1)
 310           continue
 300        continue
C*****      print *,ik1+(ik2-1)*k1point,xsum,ysum,bsum
            if(abs(bsum).lt.epsilon) then
               gx(ik1+(ik2-1)*k1point)=0.0
               gy(ik1+(ik2-1)*k1point)=0.0
               gz(ik1+(ik2-1)*k1point)=0.0
            else
               gx(ik1+(ik2-1)*k1point)=xsum/bsum
               gy(ik1+(ik2-1)*k1point)=ysum/bsum
               gz(ik1+(ik2-1)*k1point)=zsum/bsum
            endif
 240     continue
 200  continue
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
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *  ipout,lout,ityp,ierror)
      if(ierror.ne.0) cvelnm='vels'
      call cmo_get_info(cvelnm,cmo,ipvels,ilen,ityp,ierror)
      if(ierror.ne.0) then
         cbuff ='cmo/addatt/-def-/vels/VDOUBLE/vector/' //
     *          'nnodes/linear/permanent/gxa/0.0 ;  ' //
     *          'finish'
         call dotaskx3d(cbuff,ierror)
      endif
      call cmo_get_info(cvelnm,cmo,ipvels,lin,itin,ierror)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,
     *                  ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,
     *                  ipjtet,length,icmotype,ierror)
      ntetsave=ntets
      do 500 j=1,k2point-1
         do 510 i=1,k1point-1
            ij=i+(j-1)*k1point
            ip1j=(i+1)+(j-1)*k1point
            ijp1=i+j*k1point
            ip1jp1=(i+1)+j*k1point
            x1=gx(ij)
            y1=gy(ij)
            z1=gz(ij)
            x2=gx(ip1j)
            y2=gy(ip1j)
            z2=gz(ip1j)
            x3=gx(ip1jp1)
            y3=gy(ip1jp1)
            z3=gz(ip1jp1)
            x4=gx(ijp1)
            y4=gy(ijp1)
            z4=gz(ijp1)
            npsave=npoints
            nx=1
            ny=1
            dx1=1.0/dble(nx)
            dy1=1.0/dble(ny)
            dy=-dy1
            do 520 iy=1,ny+1
               dx=-dx1
               dy=dy+dy1
               do 530 ix=1,nx+1
                  dx=dx+dx1
                  xf1=(1.0-dx)*(1.0-dy)
                  xf2=dx*(1.0-dy)
                  xf3=dx*dy
                  xf4=(1.0-dx)*dy
                  npoints=npoints+1
                  call mmfindbk('xic',cmo,ipxic,length,icscode)
                  if((npoints+1).gt.length) then
                     npointsinc=npoints+1000
                     call cmo_set_info('nnodes',cmo,npointsinc,1,1,
     *                        ierror)
                     call mmgetlen(ipitetclr,nelementsmm,icscode)
                     call cmo_set_info('nelements',cmo,
     *                                 nelementsmm,1,1,ierror)
                     call cmo_newlen(cmo,ierror)
                     call cmo_get_info('itp1',cmo,
     *                                 ipitp1,length,icmotype,ierror)
                     call cmo_get_info('imt1',cmo,
     *                                 ipimt1,length,icmotype,ierror)
                     call cmo_get_info('xic',cmo,
     *                                 ipxic,length,icmotype,ierror)
                     call cmo_get_info('yic',cmo,
     *                                 ipyic,length,icmotype,ierror)
                     call cmo_get_info('zic',cmo,
     *                                 ipzic,length,icmotype,ierror)
                     call cmo_get_info(cvelnm,cmo,ipvels,lin,itin,
     *                         ierror)
                  endif
                  imt1(npoints)=1+mod(irow-1,64)
                  itp1(npoints)=0
                  xic(npoints)=xf1*x1+xf2*x2+xf3*x3+xf4*x4
                  yic(npoints)=xf1*y1+xf2*y2+xf3*y3+xf4*y4
                  zic(npoints)=xf1*z1+xf2*z2+xf3*z3+xf4*z4
                  vels(1,npoints)=0.0
                  vels(2,npoints)=0.0
                  vels(3,npoints)=0.0
 530          continue
 520       continue
           do 550 l=1,ny
              do 560 k=1,nx
                 if(ioption.eq.1) then
                    ninc=1
                 else
                    ninc=2
                 endif
                 call mmgetlen(ipitetclr,length,icscode)
                 if((ntets+ninc).gt.length) then
                    inc=1000
                    ntetsinc=ntets+inc
                    call cmo_set_info('nelements',cmo,ntetsinc,1,1,
     *                          ierror)
                    call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
                    call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
                    call cmo_newlen(cmo,ierror)
                    call cmo_get_info('itetclr',cmo,
     *                                ipitetclr,length,icmotype,ierror)
                    call cmo_get_info('itettyp',cmo,
     *                                ipitettyp,length,icmotype,ierror)
                    call cmo_get_info('itetoff',cmo,
     *                                ipitetoff,length,icmotype,ierror)
                    call cmo_get_info('jtetoff',cmo,
     *                                ipjtetoff,length,icmotype,ierror)
                    call cmo_get_info('itet',cmo,
     *                                ipitet,length,icmotype,ierror)
                    call cmo_get_info('jtet',cmo,
     *                                ipjtet,length,icmotype,ierror)
                 endif
                 if(ioption.eq.2) then
                    i1=ijtet(k  ,l  ,nx)
                    i2=ijtet(k+1,l  ,nx)
                    i3=ijtet(k+1,l+1,nx)
                    ntets=ntets+1
                    itetclr(ntets)=imt1(i1)
                    itettyp(ntets)=3
                    itetoff(ntets)=itoff
                    jtetoff(ntets)=jtoff
                    itoff=itoff+3
                    jtoff=jtoff+3
                    itet1(1+itetoff(ntets))=i1
                    itet1(2+itetoff(ntets))=i2
                    itet1(3+itetoff(ntets))=i3
                    jtet1(1+jtetoff(ntets))=-1
                    jtet1(2+jtetoff(ntets))=-1
                    jtet1(3+jtetoff(ntets))=-1
                    i1=ijtet(k  ,l  ,nx)
                    i2=ijtet(k+1,l+1,nx)
                    i3=ijtet(k  ,l+1,nx)
                    ntets=ntets+1
                    itetclr(ntets)=imt1(i1)
                    itettyp(ntets)=3
                    itetoff(ntets)=itoff
                    jtetoff(ntets)=jtoff
                    itoff=itoff+3
                    jtoff=jtoff+3
                    itet1(1+itetoff(ntets))=i1
                    itet1(2+itetoff(ntets))=i2
                    itet1(3+itetoff(ntets))=i3
                    jtet1(1+jtetoff(ntets))=-1
                    jtet1(2+jtetoff(ntets))=-1
                    jtet1(3+jtetoff(ntets))=-1
                 else
                    i1=ijtet(k  ,l  ,nx)
                    i2=ijtet(k+1,l  ,nx)
                    i3=ijtet(k+1,l+1,nx)
                    i4=ijtet(k  ,l+1,nx)
                    ntets=ntets+1
                    itetclr(ntets)=imt1(i1)
                    itettyp(ntets)=4
                    itetoff(ntets)=itoff
                    jtetoff(ntets)=jtoff
                    itoff=itoff+4
                    jtoff=jtoff+4
                    itet1(1+itetoff(ntets))=i1
                    itet1(2+itetoff(ntets))=i2
                    itet1(3+itetoff(ntets))=i3
                    itet1(4+itetoff(ntets))=i4
                    jtet1(1+jtetoff(ntets))=-1
                    jtet1(2+jtetoff(ntets))=-1
                    jtet1(3+jtetoff(ntets))=-1
                    jtet1(4+jtetoff(ntets))=-1
                 endif
 560           continue
 550        continue
 510     continue
 500  continue
      call cmo_get_info('xic',cmo,
     *                  ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,
     *                  ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,
     *                  ipzic,length,icmotype,ierror)
      call cmo_get_info(cvelnm,cmo,ipvels,lin,itin,ierror)
      call cmo_get_info('itet',cmo,
     *                  ipitet,length,icmotype,ierror)
      do it=ntetsave+1,ntets
         i1=itet1(itetoff(it)+1)
         i2=itet1(itetoff(it)+2)
         i3=itet1(itetoff(it)+3)
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         xarea=  (y2-y1)*(z3-z1)-(y3-y1)*(z2-z1)
         yarea=-((x2-x1)*(z3-z1)-(x3-x1)*(z2-z1))
         zarea=  (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
         vels(1,i1)=vels(1,i1)+xarea
         vels(2,i1)=vels(2,i1)+yarea
         vels(3,i1)=vels(3,i1)+zarea
         vels(1,i2)=vels(1,i2)+xarea
         vels(2,i2)=vels(2,i2)+yarea
         vels(3,i2)=vels(3,i2)+zarea
         vels(1,i3)=vels(1,i3)+xarea
         vels(2,i3)=vels(2,i3)+yarea
         vels(3,i3)=vels(3,i3)+zarea
      enddo
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call mmrelprt(isubname,icscode)
      goto 9999
 9999 continue
      return
      end
