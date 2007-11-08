      subroutine get_material_info(nmats,names,matno,ierror)
C
C
C#######################################################################
C
C     PURPOSE - get material number associated with at mregion name
C
C     INPUT ARGUMENTS -
C
c     none
c
c     OUTPUT ARGUMENTS -
c
c     nmats  number of materials
c     names  material region names
c     matno  material number for mregion
c     ierror  return flag
c
c $Log:   /pvcs.config/t3d/src/get_material_info.f_a  $
CPVCS    
CPVCS       Rev 1.0   25 Feb 2000 09:53:10   dcg
CPVCS    Initial revision.
C#######################################################################
c
      implicit none
C
C#######################################################################
C
      include 'geom_lg.h'
      character*32 names(*)
      integer nmats,matno(*),ierror
      integeri,ierr,length,iout,lout,itype
      character*32 cmo,geom_name
      pointer(ipout,out)
      real*8 out(*),rout
c
C#######################################################################
c
      ierror=1
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) go to 9999
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)

      if (nmregs.ne.0) then
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
         if(ierror.eq.0) then
            nmats=0
            do i=1,nmregs
              if(cmregs(i).ne.' ') then
                 nmats=nmats+1
                 names(nmats)=cmregs(i)
                 matno(nmats)=matregs(i)
               endif
            enddo
         endif
      endif
 9999 return
      end
