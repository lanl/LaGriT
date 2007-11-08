      subroutine helpdic(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                   ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         print value of mesh object or global variables
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/helpdic_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.2   03 Jul 2006 16:42:36   gable
CPVCS    Updated to point to correct URL.
CPVCS    
CPVCS       Rev 1.1   30 Sep 2004 14:23:04   dcg
CPVCS    allow printing of a few global variables in chydro.h
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 16:32:50   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'chydro.h'
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character *32 cout,name,isubname,partname
      real*8 rout
      integer iout,itype,ierror_return,ierrw,lout,ierr,icscode
      pointer (ipout,out)
      real*8 out(*)
      character*132 logmess
C
C#######################################################################
      isubname='helpdic'
C
      ierror=0
C
      if(nwds.lt.2) then
         write(logmess,10)
 10      format('See LaGriT Home Page http://lagrit.lanl.gov')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,20)
 20      format('See Manual at http://lagrit.lanl.gov/manual.shtml')
         call writloga('default',0,logmess,0,ierr)
         go to 9999
      endif
C
C  look in the global data arrays
C
      write(logmess,10)
      call writloga('default',0,logmess,0,ierr)
      write(logmess,20)
      call writloga('default',0,logmess,0,ierr)
      name=cmsgin(2)
      call get_global(name,iout,rout,cout,
     *                        itype,ierror_return)
      if(ierror_return.eq.0) then
            if(itype.eq.1) then
               write(logmess,'(a," = ",i10)') name,iout
            elseif(itype.eq.2) then
               write(logmess,'(a," = ",e10.3)') name,rout
            elseif(itype.eq.3) then
               write(logmess,'(a," = ",a)') name,cout
            endif
            go to 9999
      endif
c
c  not global see if cmo attribute
c
      call cmo_get_name(partname,icscode)
      iout=0
      rout=0.
      cout=' '
      call cmo_get_attinfo(name,partname,iout,rout,cout,
     *                        ipout,lout,itype,ierror_return)
      if(ierror_return.eq.0) then
         if(itype.eq.1) then
            write(logmess,'(a," = ",i10)') name,iout
         elseif(itype.eq.2) then
            write(logmess,'(a," = ",e10.3)') name,rout
         elseif(itype.eq.3) then
            write(logmess,'(a," = ",a)') name,cout
         else
            write(logmess,'(a,a)') 'Illegal attribute type ',name
         endif  
         go to 9999
      endif
c  look for global variable in chydro
      if(name(1:3).eq.'pie') then
         write(logmess,'(a," = ",e20.13)') name,pie
         go to 9999
      elseif(name(1:8).eq.'ivoronoi') then
         write(logmess,'(a," = ",i5)') name,ivoronoi
         go to 9999
      elseif(name(1:8).eq.'nconsurf') then
         write(logmess,'(a," = ",i5)') name,nconsurf
         go to 9999
      elseif(name(1:6).eq.'idebug') then
         write(logmess,'(a," = ",i5)') name,nconsurf
         go to 9999
      endif


C
C  if can't find name in any storage block print error and return
C
      write(logmess,'(a,a)')
     *         'This variable not global or cmo: ',name
      ierror=1
c
 9999 call writloga('default',0,logmess,0,ierrw)

      return
      end
 
 
 
 
 
