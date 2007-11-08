      subroutine chglocl(ipt1,ipt2,ipt3)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE TRANSFORMS POINTS FROM THE NORMAL COORDINATE SYSTEM
C     TO THE CURRENT LOCAL COORDINATE SYSTEM.  THE CURRENT ORIGIN AND
C     ROTATION MATRIX ARE USED FOR THE TRANSALTION.  BOTH POINTS
C     AND DIRECTION VELOCITIES ARE TRANSFORMED.
C
C
C     INPUT ARGUMENTS -
C
C        ipt1 - FIRST POINT TO TRANSFORM
C        ipt2 - LAST POINT TO TRANSFORM
C        ipt3 - STRIDE
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        FO0825AA-89, FO0828AA-89, FO0918AB-89
C
C
C        $Log: chglocl.f,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
      implicit none
C
      include 'chydro.h'
C
      character*32 cmo, isubname
C
C#######################################################################
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(100000),yic(100000),zic(100000)
C
      real*8 ax,bx,cx,ay,by,cy,az,bz,cz,u1,v1,w1,x1,y1,z1,x0,y0,z0
      integer i2,icmotype,icmolen,ierror_return,index,ier,nmcmoatt,
     *  ipt1,ipt2,ipt3,ierr1,ierror,ilen,i1,ityp,icmotyp,
     *  i,icscode
C
      pointer (ipcmo,xcmo)
      real*8 xcmo(3,1000000)
      character*32 cname,crank,cio,cpers,cinter,clen,ctype
C#######################################################################
C
C     ******************************************************************
C     GET THE CURRENT ORIGIN
C
      x0=origc(1)
      y0=origc(2)
      z0=origc(3)
C
C     ******************************************************************
C     GET THE CURRENT ROTATION MATRIX
C
      ax=rotatc(1,1)
      bx=rotatc(1,2)
      cx=rotatc(1,3)
      ay=rotatc(2,1)
      by=rotatc(2,2)
      cy=rotatc(2,3)
      az=rotatc(3,1)
      bz=rotatc(3,2)
      cz=rotatc(3,3)
C
C     ******************************************************************
      isubname = 'chglocl'
      ierr1 = 0
C
C   Get the existing cmo
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
C     TRANSPOSE AND ROTATE THE POINTS TO
C     THE LOCAL ORIGIN AND AXIS
C
      if (ipt3 .lt. 1) ipt3=1
      do 10 i1=ipt1,ipt2,ipt3
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         xic(i1)=ax*(x1-x0) + bx*(y1-y0) + cx*(z1-z0)
         yic(i1)=ay*(x1-x0) + by*(y1-y0) + cy*(z1-z0)
         zic(i1)=az*(x1-x0) + bz*(y1-y0) + cz*(z1-z0)
   10 continue
C
C     look for mesh object attributes that are vectors
C     rotate them
C
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   icscode)
      do i=1,nmcmoatt
         call cmo_get_attribute_name(cmo,i,cname,ier)
         call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierror_return)
         if(crank(1:6).eq.'vector') then
            call cmo_get_info(cname,cmo,ipcmo,icmolen,icmotyp,icscode)
            do i1=ipt1,ipt2,ipt3
               u1=xcmo(1,i1)
               v1=xcmo(2,i1)
               w1=xcmo(3,i1)
               xcmo(1,i1)= ax*u1 + bx*v1 + cx*w1
               xcmo(2,i1)= ay*u1 + by*v1 + cy*w1
               xcmo(3,i1)= az*u1 + bz*v1 + cz*w1
            enddo
         endif
      enddo
C
C     ******************************************************************
C     TRANSPOSE AND ROTATE BOUNDARY DEFINITIONS VELOCITIES TO
C     THE LOCAL ORIGIN AND AXIS
C
      if (nb .gt. 0) then
         do 30 i1=1,3
            do 20 i2=1,nb
               x1=xbb(i1,i2)
               y1=ybb(i1,i2)
               z1=zbb(i1,i2)
               xbb(i1,i2)=ax*(x1-x0) + bx*(y1-y0) + cx*(z1-z0)
               ybb(i1,i2)=ay*(x1-x0) + by*(y1-y0) + cy*(z1-z0)
               zbb(i1,i2)=az*(x1-x0) + bz*(y1-y0) + cz*(z1-z0)
                 u1=ubb(i1,i2)
                 v1=vbb(i1,i2)
                 w1=wbb(i1,i2)
                 ubb(i1,i2)=ax*u1 + bx*v1 + cx*w1
                 vbb(i1,i2)=ay*u1 + by*v1 + cy*w1
                 wbb(i1,i2)=az*u1 + bz*v1 + cz*w1
   20       continue
   30    continue
      endif
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
