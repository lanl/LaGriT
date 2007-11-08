*dk nurbp
      subroutine nurbp(ioption,
     *                 k1,
     *                 x,y,z,
     *                 irow,ipt,ict,icttot,
     *                 npoints,ntets,nbpoints,nbtets,
     *                 itoff,jtoff)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE PROCESSES AN IGES TYPE "116" (A POINT ENTITY).
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/nurbp.f_a  $
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:55:56 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Thu Oct 10 08:42:38 1996   het
CPVCS    Do an automatic addatt for "vels" to contain normal directions.
CPVCS    
CPVCS       Rev 1.1   Thu Jun 27 14:52:34 1996   het
CPVCS    Put unit normals into the vels array for each NURB.
CPVCS    
CPVCS       Rev 1.0   Tue Jan 30 15:20:26 1996   dcg
CPVCS    Initial revision.
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
C ######################################################################
C
      real*8 x(k1), y(k1), z(k1)
C
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
      data epsilon / 1.0d-10 /
C
      isubname="nurbp"
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      k1points=k1
C
      length=k1points
      call mmgetblk("gx",isubname,ipgx,length,2,icscode)
      call mmgetblk("gy",isubname,ipgy,length,2,icscode)
      call mmgetblk("gz",isubname,ipgz,length,2,icscode)
C
      do 240 ik1=1,k1points
         gx(ik1)=x(ik1)
         gy(ik1)=y(ik1)
         gz(ik1)=z(ik1)
 240  continue
      do 510 i=1,k1points
         x1=gx(i)
         y1=gy(i)
         z1=gz(i)
         npsave=npoints
         ntetsave=ntets
         nx=1
         do 530 ix=1,nx
            npoints=npoints+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((npoints+1).gt.length) then
               npointsinc=npoints+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierr)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,
     *                           nelementsmm,1,1,ier)
               call cmo_newlen(cmo,ierror)
            endif
            call cmo_get_info('itp1',cmo,
     *                        ipitp1,lenitp1,icmotype,ier)
            call cmo_get_info('imt1',cmo,
     *                        ipimt1,lenimt1,icmotype,ier)
            call cmo_get_info('xic',cmo,
     *                        ipxic,lenxic,icmotype,ier)
            call cmo_get_info('yic',cmo,
     *                        ipyic,lenyic,icmotype,ier)
            call cmo_get_info('zic',cmo,
     *                        ipzic,lenzic,icmotype,ier)
            imt1(npoints)=1+mod(irow-1,64)
            itp1(npoints)=0
            xic(npoints)=x1
            yic(npoints)=y1
            zic(npoints)=z1
 530     continue
 510  continue
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call mmrelprt(isubname,icscode)
      goto 9999
 9999 continue
      return
      end
