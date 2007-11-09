*dk,reconloop3d
      subroutine reconloop3d(iobtuse,toldamage)
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
C        $Log:   /pvcs.config/t3d/src/reconloop3d.f_a  $
CPVCS    
CPVCS       Rev 1.1   15 May 2001 14:45:54   kuprat
CPVCS    We now pass TOLDAMAGE to RECON2.
CPVCS    
CPVCS       Rev 1.0   Fri Apr 17 15:34:38 1998   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
      include 'neibor.h'
C
      integer iobtuse,ierror,nsdtopo,length,icmotype,i,npoints,ilen
     &   ,itype,ntets,ierrmat
      real*8 toldamage

      character*132 logmess
      character*32 cmo, isubname
C
      isubname='reconloop3d'
C
C  get info from mesh object and make sure it is a 2d mesh
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')'RECON3D: no 3d mesh object'
         call writloga('default',0,logmess,0,ierror)
         return
      endif
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      if(nsdtopo.ne.3) then
         write(logmess,'(a)')'RECON3D: bad 3d mesh object'
         call writloga('default',0,logmess,0,ierror)
         return
      endif
C
C
      do i=1,1
         write(logmess,'(a,i5)') 'RECON3D reconnection loop: ',i
         call writloga('default',0,logmess,0,ierror)
         call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierror)
         call cmo_get_info('nelements',cmo,ntets,ilen,itype,ierror)
         irecnall=1
         nrecon=ntets
         call recon2(npoints,ntets,toldamage)
         call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierror)
         call cmo_get_info('nelements',cmo,ntets,ilen,itype,ierror)
         call cmo_newlen(cmo,ierror)
C*****   call matbld3d(cmo,ierrmat)
         if(iobtuse.eq.1) then
            call refine_edge_3d(cmo,ierror)
C*****      if(ierror.gt.0) goto 100
            call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierror)
            call cmo_get_info('nelements',cmo,ntets,ilen,itype,ierror)
            irecnall=1
            nrecon=ntets
            call recon2(npoints,ntets,toldamage)
            call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierror)
            call cmo_get_info('nelements',cmo,ntets,ilen,itype,ierror)
            call cmo_newlen(cmo,ierror)
            ierrmat=-1
C*****      call matbld3d(cmo,ierrmat)
            if(ierrmat.lt.0) goto 110
         else
            goto 100
         endif
      enddo
 100  continue
 110  continue
C
      goto 9999
 9999 continue
C
      return
      end
