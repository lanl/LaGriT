      subroutine cmo_get_stdptrs(cmo,ipimt1,ipitp1,ipicr1,ipisn1,ipxic
     &   ,ipyic,ipzic,ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet
     &   ,ipjtet,ierror)

C
C######################################################################
C
C     PURPOSE -
C This routine gets a standard set of pointers associated with a cmo.
C
C
C######################################################################
C     CHANGE HISTORY -
C
C        $Log: cmo_get_stdptrs.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   14 Jun 2007 14:37:38   tam
CPVCS    preserve errors for return out of subroutine
CPVCS    
CPVCS       Rev 1.0   14 Jun 2007 08:03:44   tam
CPVCS    Initial revision.
CPVCS    
CPVCS
CPVCS    Initial Version from kuprat.
CPVCS
C
C######################################################################
C
      implicit none

      character*32 cmo
      integer ierror

      pointer(ipimt1,imt1)
      integer imt1(*)

      pointer (ipitp1,itp1)
      integer itp1(*)

      pointer (ipicr1,icr1)
      integer icr1(*)

      pointer (ipisn1,isn1)
      integer isn1(*)

      pointer (ipxic,xic)
      real*8 xic(*)

      pointer (ipyic,yic)
      real*8 yic(*)

      pointer (ipzic,zic)
      real*8 zic(*)

      pointer (ipitetclr,itetclr)
      integer itetclr(*)

      pointer (ipitettyp,itettyp)
      integer itettyp(*)

      pointer (ipitetoff,itetoff)
      integer itetoff(*)

      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)

      pointer (ipitet,itet)
      integer itet(*)

      pointer (ipjtet,jtet)
      integer jtet(*)
 
      integer ierr,ierr2,leni,icmotype,if_local_debug
      character*132 logmess

      if_local_debug = 0
      ierror = 0

C     flag error for attributes that may not be present
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1

      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr2)
      if (ierr2.ne.0) ierror = -1
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr2)
      if (ierr.ne.0) ierror = -1

C     preserve error and leni for primary attributes
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
      if (ierr.ne.0) ierror = ierr
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
      if (ierr.ne.0) ierror = ierr
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
      if (ierr.ne.0) ierror = ierr
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
      if (ierr.ne.0) ierror = ierr

      if(if_local_debug .ne. 0)then
      write(logmess,"(a)")
     *    "cmo_get_stdptrs "
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer itp     = ", ipitp1
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer icr     = ", ipicr1
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer isn     = ", ipisn1
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer itetclr = ", ipitetclr
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer itettyp = ", ipitettyp
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer itetoff = ", ipitetoff
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer jtetoff = ", ipjtetoff
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer itet    = ", ipitet
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer jtet    = ", ipjtet
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer imt     = ", ipimt1
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer xic     = ", ipxic
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer yic     = ", ipyic
          call writloga('default',0,logmess,0,ierr)
      write(logmess,"(a,i12)")
     *    "Pointer zic     = ", ipzic
          call writloga('default',0,logmess,0,ierr)
      endif
      return
      end
