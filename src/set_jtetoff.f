      subroutine set_jtetoff
C
C########################################################################
C
C     PURPOSE -
C
c  this subroutine fills in the jtetoff array based on itettyp
c
c
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/set_jtetoff.f_a  $
CPVCS    
CPVCS       Rev 1.0   Tue Feb 01 16:21:58 2000   dcg
CPVCS    Initial revision.
C########################################################################
      implicit none
      include 'local_element.h'
c
      pointer (ipjtetoff, jtetoff)
      pointer (ipitettyp, itettyp)
      integer jtetoff(*), itettyp(*)
      character*32 cmo
      character*132 logmess
      integer ilen,icmotype,icscode,nelements,it
C########################################################################
      call cmo_get_name(cmo,icscode)
      call cmo_get_info('nelements',cmo,
     &                      nelements,ilen,icmotype,icscode)
      call cmo_get_info('jtetoff',cmo,
     &                      ipjtetoff,ilen,icmotype,icscode)
      call cmo_get_info('itettyp',cmo,
     &                      ipitettyp,ilen,icmotype,icscode)
      jtetoff(1)=0
      if(itettyp(1).eq.0) then
        write(logmess,'(a)') ' ittettyp not set in set_jtetoff'
        call writloga('default',0,logmess,0,icscode)
        go to 9999
      endif
      do it=2,nelements
        jtetoff(it)=jtetoff(it-1)+nelmnef(itettyp(it-1))
      enddo
 9999 return
      end
