*DK unpackpc
      subroutine unpackpc(npoints,ITP1_TEMP,ISN1_TEMP,IPARENT_TEMP)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE PARENT OF EACH CHILD. FOR ORDINARY
C            POINTS THEY ARE THEIR OWN PARENTS.
C
C      INPUT ARGUMENTS -
C
C         npoints   - NUMBER OF POINTS IN THE MESH.
C         ITP1_TEMP()    - ARRAY OF POINT TYPES.
C         ISN1_TEMP()    - ARRAY OF PARENT/CHILDREN POINTERS.
C
C      OUTPUT ARGUMENTS -
C
C         IPARENT_TEMP() - THE PARENT POINT FOR EACH POINT.
C
C      CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/unpackpc.f_a  $
CPVCS    
CPVCS       Rev 1.11   08 Feb 2006 14:35:38   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.10   26 Nov 2001 17:43:50   dcg
CPVCS    check valid node type must be < 20
CPVCS    
CPVCS       Rev 1.9   06 Sep 2001 12:47:42   dcg
CPVCS    simplify the logic 
C
C#######################################################################
C
      implicit none
C
      include 'chydro.h'
      integer nmulti
      parameter (nmulti=100)
C
      character*132 logmess
      character*32 isubname
C
C#######################################################################
C
      integer i,npoints,j,jcount,ier
      integer itp1_temp(npoints), isn1_temp(npoints),
     *          iparent_temp(npoints)
C#######################################################################
C
C
C#######################################################################
C
C
      isubname='unpackpc'
C
c
      do i=1,npoints
         iparent_temp(i)=i
      enddo
c  this loop sets parents for all children in a chain beginning
c  with a parent
      do i=1,npoints
         if(itp1_temp(i).eq.ifitpcup) then
            j=isn1_temp(i)
            jcount=0
 5          if (i.ne.j) then
               jcount=jcount+1
               if(jcount.ge.nmulti.or.jcount.gt.npoints.or.j.le.0) then
                  write(logmess,7) i
 7                format(' Non-terminating parent/child chain ',
     *                       i10)
                  call writloga('default',0,logmess,0,ier)
                  go to 10
               endif
               iparent_temp(j)=i
               j=isn1_temp(j)
               go to 5
            endif
         endif
 10      continue
      enddo
c  this loop looks for children that have no parents
      do i=1,npoints
         if(itp1_temp(i).lt.ifitpst3
     *           .and.isn1_temp(i).ne.0.
     *           .and.iparent_temp(i).eq.i) then
             write(logmess,8) i
 8           format(' Child with no parent ',i10)
             call writloga('default',0,logmess,0,ier)
         endif
      enddo
 9999 continue
      return
      end
