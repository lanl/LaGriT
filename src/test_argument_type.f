*dk,test_argument_type.f
      subroutine test_argument_type (nargs,itype,istart,imsgin,xmsgin,
     x      cmsgin,msgtype,nwds)
C    ###################################################################
C
C      PURPOSE:
C         check argument for type --convert istart possible
C      INPUT:
C         nargs:    number of argunmets to check
C         itype:    desired type -- 1 for integer, 2 for real, 3 for char
C         itype:    desired type -- 1 for integer, 2 for real, 3 for char
C         imsgin:   array of integer arguments to check
C         xmsgin:   array of real arguments to check
C         cmsgin:   array of character arguments to check
C         msgtype:  array of argument types(1,2 or 3)
C         nwds:     number or argument
C
C      OUTPUT:
C
C    CHANGE HISTORY
C$Log: test_argument_type.f,v $
CRevision 2.00  2007/11/09 20:04:04  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   Fri Oct 03 11:03:06 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:04:42 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   09/20/95 09:48:48   dcg
CPVCS    Initial revision.
C
C    ###################################################################
      implicit none
      integer itype,istart,nwds,i,index,ierr,nargs
      real*8 xmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
C
      if(istart+nargs-1.gt.nwds) then
         do i=nwds+1,istart+nargs-1
            imsgin(i)=0
            msgtype(i)=1
         enddo
      endif
      do i=1,nargs
         index=istart+i-1
         if(msgtype(index).ne.itype) then
            if(itype.eq.1.and.msgtype(index).eq.2) then
               imsgin( index) = nint(xmsgin(index))
               msgtype(index)=1
            elseif(itype.eq.2.and.msgtype(index).eq.1) then
               xmsgin( index) = imsgin(index)
               msgtype(index)=2
            elseif(itype.eq.2.and.msgtype(index).eq.3) then
               xmsgin( index) = 0.
               msgtype(index)=2
               write(logmess,9000) cmsgin(index),index
 9000          format('character type parameter set to zero ',a,i5)
               call writloga('default',0,logmess,0,ierr)
            elseif(itype.eq.1.and.msgtype(index).eq.3) then
               imsgin( index) = 0.
               msgtype(index)=1
               write(logmess,9000) cmsgin(index),index
               call writloga('default',0,logmess,0,ierr)
            elseif(itype.eq.3.and.msgtype(index).eq.1) then
               cmsgin( index) = ' '
               msgtype(index)=3
               write(logmess,9001) imsgin(index),index
 9001          format('integer type parameter set to blank ',i5,i5)
               call writloga('default',0,logmess,0,ierr)
            elseif(itype.eq.3.and.msgtype(index).eq.2) then
               cmsgin( index) = ' '
               msgtype(index)=3
               write(logmess,9002) xmsgin(index),index
 9002          format('real type parameter set to blank ',f7.2,i5)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
      enddo
      return
      end
