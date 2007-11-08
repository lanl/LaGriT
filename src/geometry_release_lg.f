      subroutine geometry_release_lg(imsgin,xmsgin,cmsgin,msgtype,
     *   nwds,ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Release a geometry entry
c         geometry/release/geom_name
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
C  $Log: geometry_release_lg.f,v $
C  Revision 2.00  2007/11/05 19:45:56  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   22 Mar 2002 16:42:14   dcg
CPVCS    allow for a mesh object with no geometry
CPVCS    
CPVCS       Rev 1.0   20 Mar 2000 13:45:00   dcg
CPVCS    Initial revision.

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
      integer n,k,j,lengeom,len,icscode,i
      character*32 partname
c
c  find geometry name 
c
      ierror=0
      geom_name=cmsgin(3)
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,lengeom,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      n=number_of_geometries
      do i=1,n
          if(geom_name.eq.geom_names(i)) then
               call mmrelprt(geom_name,icscode)
               do j=i+1,number_of_geometries
                  do k=1,8
                     geom_info(j-1,k)=geom_info(j,k)
                  enddo
                  geom_names(j-1)=geom_names(j)
               enddo
               number_of_geometries=number_of_geometries-1
               if(geom_name.eq.current_geom_name)
     *           current_geom_name=' '
                 nsurf=0
                 nregs=0
                 nmregs=0
               go to 9999
           endif
      enddo
C
      write(logmess,12) geom_name
 12   format(' Cannot find geometry: ',a32)
      call writloga('default',0,logmess,0,ierror)
C 
9999  return
      end       

              
      
      
