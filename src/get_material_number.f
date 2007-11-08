      subroutine get_material_number(name,number,ierror)
C
C
C#######################################################################
C
C     PURPOSE - get material number associated with at mregion name
C
C     INPUT ARGUMENTS -
C
c     name  mregion name
c
c     OUTPUT ARGUMENTS -
c
c     number  material number for mregion
c     ierror  return flag
c
c $Log:   /pvcs.config/t3d/src/get_material_number_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.0   Tue Feb 08 15:08:50 2000   dcg
CPVCS    Initial revision.
C#######################################################################
c
      implicit none
C
C#######################################################################
C
      include 'geom_lg.h'
      character *(*) name
      integer number,ierror,i,ierr,length,iout,lout,itype
      character*32 cmo,geom_name
      pointer(ipout,out)
      real*8 out(*),rout
c
C#######################################################################
c
      ierror=1
      number=0
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) go to 9999
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)

      if (nregs.ne.0) then
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
         if(ierror.eq.0) then
            do i=1,nregs
              if(cmregs(i).eq.name) then
                 number=matregs(i)
                 ierror=0
                 go to 9999
              endif
            enddo
         endif
      endif
 9999 return
      end
