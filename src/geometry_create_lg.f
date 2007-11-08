      subroutine geometry_create_lg(imsgin,xmsgin,cmsgin,msgtype,
     *   nwds,ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Create a new geometry entry
c         geometry/create/geom_name
c
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
c
c      OUTPUT
c         ierror -  return flag (0= ok, 1=error)
c
c      CHANGE
C  $Log: geometry_create_lg.f,v $
C  Revision 2.00  2007/11/05 19:45:56  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 11:07:04 2000   dcg
CPVCS    check for null current geometry name
CPVCS    
CPVCS       Rev 1.1   Mon Mar 20 17:08:22 2000   dcg
CPVCS    fix mesh object/ geometry correspondence
CPVCS     add geometry/release option
CPVCS
CPVCS       Rev 1.0   Tue Feb 15 10:31:50 2000   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'geom_lg.h'
      integer imsgin(*),msgtype(*),ierror,nwds
      real*8 xmsgin(*)
      character*32 cmsgin(*)
      character*32 geom_name
c
      character*132 logmess
      integer i,len,lengeom,ierrw
      character*32 partname
c
c  if command is release call correct subroutine
c
      if(cmsgin(2).eq.'release') then
         call geometry_release_lg (imsgin,xmsgin,cmsgin,msgtype,
     *   nwds,ierror)
         go to 9999
      endif
c
c  see if geometry name is the current one - if not save the old
c  state and get the new one
c
      ierror=0
      geom_name=cmsgin(3)
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,lengeom,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      if(geom_name.eq.current_geom_name) then
        write(logmess,'(a)') 'Geometry already the current one '
        call writloga('default',0,logmess,0,ierrw)
        ierror=1
        go to 9999
      endif
c
c  new one see if enough room
c
      if(number_of_geometries+1.gt.lengeom) then
         call mmincblk ('geom_names',partname,
     *  ipgeom_names,10,ierror)
         call mmincblk ('geom_info',partname,
     *  ipgeom_info,80,ierror)
      endif
c
c  save old info
c
      if(current_geom_name.eq.' ') go to 10
      do i=1,number_of_geometries
           if(current_geom_name.eq.geom_names(i)) then
              if(nsurf.ne.0) geom_info(1,i)=nsurf
              if(nregs.ne.0) geom_info(2,i)=nregs
              if(nmregs.ne.0) geom_info(3,i)=nmregs
              if(maxdef.ne.0) geom_info(4,i)=maxdef
              if(maxmdef.ne.0) geom_info(5,i)=maxmdef
              if(lastregdef.ne.0) geom_info(6,i)=lastregdef
              if(lastmregdef.ne.0) geom_info(7,i)=lastmregdef
              if(lastsparam.ne.0) geom_info(8,i)=lastsparam
              go to 10
           endif
         enddo
         write(logmess,8) current_geom_name
 8       format ('cannot find current geometry: ',a32)
         call writloga('default',0,logmess,0,ierrw)
         ierror=1
         go to 9999
c
c  initialize new
c
 10   number_of_geometries=number_of_geometries+1
      i=number_of_geometries
      geom_names(i)=geom_name
      nsurf=0
      nregs=0
      nmregs=0
      maxdef=0
      maxmdef=0
      lastregdef=0
      lastmregdef=0
      lastsparam=0
      geom_info(1,i)=0
      geom_info(2,i)=0
      geom_info(3,i)=0
      geom_info(4,i)=0
      geom_info(5,i)=0
      geom_info(6,i)=0
      geom_info(7,i)=0
      geom_info(8,i)=0
      write(logmess,12) geom_name
 12   format(' Current geometry name set to: ',a32)
      call writloga('default',0,logmess,0,ierrw)
      current_geom_name=geom_name
9999  return
      end
 
 
 
 
