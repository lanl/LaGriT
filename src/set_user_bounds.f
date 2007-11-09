      subroutine set_user_bounds(nubndpts,ubndpts,cmo,
     *       ipattr,idfield)
C
C  THis is a sample subroutine that lets the user set boundary nodes
C  The selected set of boundary nodes contains nubndpts and ubndpts
C  is the array contianing the index to these nodes
C
C  cmo and attr give the mesh object and the attribute to be set
C  idfield is the character string passed in through the command
C
C  Change Record
C$Log:   /pvcs.config/t3d/src/set_user_bounds.f_a  $
CPVCS    
CPVCS       Rev 1.1   Tue Mar 03 09:14:26 1998   dcg
CPVCS    version to use with solve_poisson cone in cube geom.
C
      implicit none
      character*32 cmo,idfield
      integer i,nubndpts,len,type,ierror
      integer ubndpts(*)
      pointer (ipattr,attr)
      real*8 attr(*)
      pointer (ipzic,zic)
      real*8 zic(*)
      real*8 zmax,zmin,zdif
 
C
C  in this sample deck we will set the boundary values on the selected
C  surfaces to a value between 0 and 2 based on the z coord of the point
C
      if(idfield(1:8).eq.'mystrinf') then
         call cmo_get_info('zic',cmo, ipzic, len, type, ierror)
         zmax=zic(ubndpts(1))
         zmin=zic(ubndpts(1))
         do i=2,nubndpts
            if (zic(ubndpts(i)).gt.zmax) zmax=zic(ubndpts(i))
            if (zic(ubndpts(i)).lt.zmin) zmin=zic(ubndpts(i))
         enddo
         zdif=zmax-zmin
         do i=2,nubndpts
c            attr(ubndpts(i))=2.0*(zic(ubndpts(i))-zmin)/zdif
            attr(ubndpts(i))=2.0*(zmax-(zic(ubndpts(i))-zmin))/zdif
         enddo
      endif
      return
      end
 
