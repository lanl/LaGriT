      subroutine get_epsilon(epsilon_name, epsilon)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE GETS THE SEARCH RANGE TO BE USED IN DETERMINING
C        POINT TYPES, MATERIAL TYPES, AND INTERFACE LOCATIONS.
C
C     INPUT ARGUMENTS -
C
C        epsilon_name - Name of the epsilon to be returned.
C
C     OUTPUT ARGUMENTS -
C
C        epsilon - The Search Range TO BE USED IN DETERMINING
C                  POINT TYPES, MATERIAL TYPES, AND INTERFACE LOCATIONS.
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/get_epsilon_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.0   Wed Jan 19 17:29:30 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Sep 29 15:04:40 1998   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) epsilon_name
C
      REAL*8 epsilon
C
C#######################################################################
C
      character*32 cmo_name, cout, isubname
C
      integer icscode,iout,lout,itype
      pointer (ipout,out)
      real*8 out(*)
C
C
C#######################################################################
      isubname='get_epsilon'
C
C
      call cmo_get_name(cmo_name,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_attinfo(epsilon_name,cmo_name,iout,epsilon,cout,
     *  ipout,lout,itype,icscode)

      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_attinfo')
C
      return
      end
