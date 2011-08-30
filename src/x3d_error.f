C x3d_error and x3d_warn
C 
C crude versions for better error messages - list style
C names are kept short so they can fit on single line if statement
C
C for all subroutines list_err, list_erri, list_errd, list_errc
C arguments and code are the same except for value type
C
C     ierr_count - this is incremented for each error
C                  if 0, use default style which writes one line of error
C                  if 1, write ERROR line once, write list style
C                  if gt 1, continue with list style
C     cname -  routine name in which errors occur length of 32 or less
C     cmess -  message to display, no more than 132 characters
C     ivalue,dvalue,cvalue, or none
C
C---------------------
C list style looks like:
C
C ERROR:  connect3d                                                               
C connect3d > bad cmo mo                                                          
C connect3d > bad factor   1.0000000E+00                                          
C connect3d > exiting with error num:     1                                            
C ERROR END COUNT:  3 for connect  
C
C---------------------
C single line list style looks like:
C
C ERROR: connect > invalid number of nodes:          1                          
C
C---------------------
C old style x3d_error looks like
C              Error: connect                         set info 0 elements
C

C#######################################################################
      subroutine list_err(ierr_count,cname,cmess)
C#######################################################################
C
      implicit none
      integer ierr_count
      character*32  cname
      character*(*) cmess
      integer  ierr, icharlnf
      character*132 logmess
C
C      check to see if this is a single line style of error report
       if (ierr_count.le.0) then

         write(logmess,'(a,a,a3,a,i10)')'ERROR: ',
     *   cname(1:icharlnf(cname)),' > ',cmess
         call writloga('default',0,logmess,0,ierr)

C      otherwise use the list style of error report
       else

C         check to see if this is the first in the list
          if (ierr_count.eq.1) then
            write(logmess,'(a,a)')'ERROR:  ',
     *      cname(1:icharlnf(cname))
            call writloga('default',1,logmess,0,ierr)
          endif

C         write this instance of error for the list
          write(logmess,'(a,a3,a,i10)')
     *    cname(1:icharlnf(cname)),' > ',cmess
          call writloga('default',0,logmess,0,ierr)

       endif
C
      return
      end


C#######################################################################
      subroutine list_erri(ierr_count,cname,cmess,ivalue)
C#######################################################################
C
      implicit none
      integer ierr_count
      character*32  cname
      character*(*) cmess
      integer ivalue, ierr, icharlnf
      character*132 logmess
C
C      check to see if this is a single line style of error report
       if (ierr_count.le.0) then

         write(logmess,'(a,a,a3,a,i10)')'ERROR: ',
     *   cname(1:icharlnf(cname)),' > ',cmess,ivalue
         call writloga('default',0,logmess,0,ierr)

C      otherwise use the list style of error report
       else

C         check to see if this is the first in the list
          if (ierr_count.eq.1) then
            write(logmess,'(a,a)')'ERROR:  ',
     *      cname(1:icharlnf(cname))
            call writloga('default',1,logmess,0,ierr)
          endif

C         write this instance of error for the list
          write(logmess,'(a,a3,a,i10)')
     *    cname(1:icharlnf(cname)),' > ',cmess,ivalue
          call writloga('default',0,logmess,0,ierr)

       endif
C
      return
      end

C#######################################################################
      subroutine list_errc(ierr_count,cname,cmess,cvalue)
C#######################################################################
C
C
      implicit none
      integer ierr_count
      character*32  cname
      character*(*) cmess
      character*(*) cvalue
      integer ivalue, ierr, icharlnf
      character*132 logmess
C
C
C      check to see if this is a single line style of error report
       if (ierr_count.le.0) then

         write(logmess,'(a,a,a3,a,a)')'ERROR: ',
     *   cname(1:icharlnf(cname)),' > ',cmess,
     *   cvalue(1:icharlnf(cvalue))
         call writloga('default',0,logmess,0,ierr)

C      otherwise use the list style of error report
       else

C         check to see if this is the first in the list
          if (ierr_count.eq.1) then
            write(logmess,'(a,a)')'ERROR:  ',
     *      cname(1:icharlnf(cname))
            call writloga('default',1,logmess,0,ierr)
          endif

C         write this instance of error for the list
          write(logmess,'(a,a3,a,a)')
     *    cname(1:icharlnf(cname)),' > ',cmess,
     *    cvalue(1:icharlnf(cvalue))
          call writloga('default',0,logmess,0,ierr)

       endif

C
      return
      end


C#######################################################################
      subroutine list_errd(ierr_count,cname,cmess,dvalue)
C#######################################################################
C
C
      implicit none
      integer ierr_count
      character*32  cname
      character*(*) cmess
      real*8 dvalue
      integer ierr, icharlnf
      character*132 logmess
C
C
C      check to see if this is a single line style of error report
       if (ierr_count.le.0) then

         write(logmess,'(a,a,a3,a,1pe15.7)')'ERROR: ',
     *   cname(1:icharlnf(cname)),' > ',cmess,dvalue
         call writloga('default',0,logmess,0,ierr)

C      otherwise use the list style of error report
       else

C         check to see if this is the first in the list
          if (ierr_count.eq.1) then
            write(logmess,'(a,a)')'ERROR:  ',
     *      cname(1:icharlnf(cname))
            call writloga('default',1,logmess,0,ierr)
          endif

C         write this instance of error for the list
          write(logmess,'(a,a3,a,1pe15.7)')
     *    cname(1:icharlnf(cname)),' > ',cmess,dvalue
          call writloga('default',0,logmess,0,ierr)

       endif

C
      return
      end

C#######################################################################
      subroutine list_err_end(ierr_count,cname)
C#######################################################################
C
C     cname -  subroutine name
C     cmess -  message to display, no more than 132 characters
C
      implicit none
      integer ierr_count
      character*(*) cname
      integer icharlnf, ierr
      character*132 logmess

      write(logmess,'(a17,i5,a5,a)')
     * 'ERROR END COUNT: ',ierr_count,' for ',
     *  cname(1:icharlnf(cname))
      call writloga('default',0,logmess,1,ierr)
C
      return
      end



C     old routines
C
C#######################################################################
      subroutine x3d_error(sub_name,func_name)
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

      call writloga('default',0,'Error from:   '
     &              //sub_name//'  '//func_name,0,ierr)
C
      return
      end

      subroutine x3d_warn(sub_name,func_name)
C#######################################################################
C
      implicit none
C
      character*(*) sub_name, func_name
      integer ierr
      character*132 logmess
C

      call writloga('default',0,'Warning from: '
     &              //sub_name//'  '//func_name,0,ierr)
C
      return
      end

