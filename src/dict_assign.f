      subroutine dict_assign(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                       ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         assigns values to variables in global or cmo blocks
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
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: dict_assign.f,v $
C         Revision 2.00  2007/11/05 19:45:52  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   03 Feb 2000 09:22:00   dcg
CPVCS    
CPVCS       Rev 1.1   28 Jan 2000 16:33:14   dcg
 
C
C#######################################################################
C
      implicit none
 
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
      character*32 isubname, name,partname,cout
 
      real*8 rout
      integer iout, icscode, ierrw, idx,itype,lout
      pointer(ipout,out)
      real*8 out(*)
 
C
      character*132 logmess
C
C#######################################################################
C
      isubname = 'dict_assign'
C
      name=cmsgin(2)
      idx=3
      if(cmsgin(2)(1:4).eq.'-def') then
         name=cmsgin(3)
         idx=4
         if(name.eq.'-def-') then
            name=cmsgin(4)
            idx=5
         endif
      endif
C
C  look in the global data arrays
C
      call get_global(name,iout,rout,cout,
     *                        itype,ierror_return)
      if(ierror_return.eq.0) then
            if(msgtype(idx).eq.1) then
               itype=1
               iout=imsgin(idx)
               write(logmess,'(a," = ",i10)') name,imsgin(idx)
            elseif(msgtype(idx).eq.2) then
               itype=2
               rout=xmsgin(idx)
               write(logmess,'(a," = ",e10.3)') name,xmsgin(idx)
            elseif(msgtype(idx).eq.3) then
               itype=3
               cout=cmsgin(idx)
               write(logmess,'(a," = ",a)') name,cmsgin(idx)
            endif
            call set_global(name,iout,rout,cout,
     *                        itype,ierror_return)
            if(ierror_return.eq.0) go to 9999
      endif
 
c
c  not global see if cmo attribute
c
      call cmo_get_name(partname,icscode)
      call cmo_get_attinfo(name,partname,iout,rout,cout,
     *                        ipout,lout,itype,ierror_return)
      if(ierror_return.eq.0) then
         iout=0
         rout=0.
         cout=' '
         if(msgtype(idx).eq.1) then
            itype=1
            iout=imsgin(idx)
            write(logmess,'(a," = ",i10)') name,imsgin(idx)
         elseif(msgtype(idx).eq.2) then
            itype=2
            rout=xmsgin(idx)
            write(logmess,'(a," = ",e10.3)') name,xmsgin(idx)
         elseif(msgtype(idx).eq.3) then
            itype=3
            cout=cmsgin(idx)
            write(logmess,'(a," = ",a)') name,cmsgin(idx)
         else
            write(logmess,'(a,a)') 'Illegal attribute type ',name
         endif
         call  cmo_set_attinfo(name,partname,iout,rout,cout,
     *                        itype,
     *                        ierror_return)
         if(ierror_return.eq.0)go to 9999
      endif
 
C
C  if can't find name in any storage block print error and return
C
      write(logmess,'(a,a)')
     *         'This variable not global or cmo: ',name
      ierror_return =1
c
 9999 call writloga('default',0,logmess,0,ierrw)
 
      return
      end
