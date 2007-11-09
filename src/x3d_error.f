*dk, x3d_error
      subroutine x3d_error(sub_name,func_name)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        Prints the subroutine and function when an error occurs.
C
C     INPUT ARGUMENTS -
C
C        sub_name   - (character) Subroutine Name.
C        func_name  - (character) Function Name.
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C     $Log: x3d_error.f,v $
C     Revision 2.00  2007/11/09 20:04:06  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   11 Jan 2000 01:11:44   jtg
CPVCS    call to writloga modified so doesn't crash code if the
CPVCS    character strings written into logmess is longer than
CPVCS    132 characters
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 17:06:10 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   04/14/95 14:18:18   ejl
CPVCS    Fixed write format.
CPVCS    
CPVCS    
CPVCS       Rev 1.1   03/21/95 13:02:36   dcg
CPVCS    Add end statement
CPVCS
CPVCS       Rev 1.0   03/15/95 15:33:20   ejl
CPVCS    Add to help trap errors in X3D.
CPVCS
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) sub_name, func_name
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr
C
      character*132 logmess
C
C#######################################################################
C
C
C
c$$      write(logmess,'(a,2x,a,2x,a)')
c$$     *   ' Error from: ',sub_name, func_name
c$$      call writloga('default',0,logmess,0,ierr)

      call writloga('default',0,' Error from:   '
     &              //sub_name//'  '//func_name,0,ierr)
C
      return
      end
