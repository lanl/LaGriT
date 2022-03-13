      subroutine distance_field_signed
     &   (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C       Compute the signed distance field, dist, between the nodes of one
C       mesh object, mo_source, and another mesh object, mo_sink. The
C       attribute dist will contain the minimum distance from each node
C       of mo_sink to the nearest node in mo_source.
C
C     NOTES -
C
C      Syntax for this command:
C      compute / signed_distance_field / mo_sink / mo_source / distance_field_attribute
C
C        
C######################################################################
C
      implicit none
C
C     Subroutine Input Variables
C
      integer       nwds, ierror, imesh_type, ilen , itype
      integer       ndt, ndg
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
      character*32  mesh_type
      character*32  isubname 
      character*32  mo_sink
      character*32  mo_src
      character*32  cattribute
      character*132 logmess
      character*256 cmdmess
C
C     Function variable
      integer icharlnf
C
C######################################################################
C
      isubname = 'distance_field'
C
C
C     Check input mesh objects
C
C     If we get this far, syntax of argument 1 and 2 is ok. Move on to
C     arguments 3, 4, 5
C
      if(nwds .ne. 5) then
         write(logmess,'(a)')
     &    'ERROR distance_field: Command requires 5 arguments.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(3) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR distance_field: Argument 3 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(4) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR distance_field: Argument 4 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(5) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR distance_field: Argument 5 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
C
      mo_sink = cmsgin(3)
      mo_src  = cmsgin(4)
      cattribute = cmsgin(5)
      print *, mo_sink, mo_src, cattribute
C
      call cmo_exist(mo_sink,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &    'ERROR distance_field: sink mesh object does not exist.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif
      call cmo_exist(mo_src,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &    'ERROR distance_field: source mesh object does not exist.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif
C
C     For the signed distance field, the only valid source mesh object is a a quad or tri sheet.
C     
C    dimensions geometry =         3        
C    element type =                qua
C    dimensions topology =         2        
C   or
C    dimensions geometry =         3        
C    element type =                tri
C    dimensions topology =         2        
      call cmo_get_info('ndimensions_geom',mo_src,ndg,ilen,itype,ierror)
      call cmo_get_info('ndimensions_topo',mo_src,ndt,ilen,itype,ierror)
      call cmo_get_mesh_type(mo_src,mesh_type,imesh_type,ierror)
      if(((ndg.eq.3).and.(ndt.eq.2).and.(mesh_type(1:3).eq.'qua')) .or.
     1   ((ndg.eq.3).and.(ndt.eq.2).and.(mesh_type(1:3).eq.'tri'))) then
         continue
      else
         write(logmess,'(a)')
     & 'ERROR distance_field: source MO must be tri or quad'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     & 'ERROR distance_field: source MO must be in 3 dimensions'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     & 'ERROR distance_field: source MO must be topologically 2d'
         call writloga('default',0,logmess,0,ierror)
      cmdmess = 'cmo/status/'// mo_src(1:icharlnf(mo_src)) //
     &     '/brief;finish'
      call dotask(cmdmess,ierror)
         ierror = -1
         go to 9999
      endif
C
C     Now execute a series of dotask operations that compute the signed distance field.
C
C     The general strategy is to compute two regions, one on either side of the source
C     mesh object and change the sign of the distance field attribute for nodes below (le)
C     the source mesh object.
C
      cmdmess = 'surface/s1_signed_distance_s1/reflect/sheet/'//
     &   mo_src(1:icharlnf(mo_src)) // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'cmo/select/'// mo_sink(1:icharlnf(mo_sink))
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'region/r2_signed_distance_r2/le s1_signed_distance_s1'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess='pset/p2_signed_distance_p2/region/r2_signed_distance_r2'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'compute / distance_field /' // 
     &  mo_sink(1:icharlnf(mo_sink)) // '/' //
     &  mo_src(1:icharlnf(mo_src)) // '/' //
     &  cattribute(1:icharlnf(cattribute)) // '/'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'math/multiply/' //
     &  mo_sink(1:icharlnf(mo_sink))// '/' //
     &  cattribute(1:icharlnf(cattribute)) // '/' //
     &  'pset/get/p2_signed_distance_p2/' // 
     &  mo_sink(1:icharlnf(mo_sink))// '/' //
     &  cattribute(1:icharlnf(cattribute)) // '/'
     &  // '-1.0; finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'pset/p2_signed_distance_p2/delete'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'region/r2_signed_distance_r2/delete'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'surface/s1_signed_distance_s1/delete'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'cmo/delete/s1_signed_distance_s1'
     &  // ';finish'
      call dotask(cmdmess,ierror)

      cmdmess = 'cmo/select/'// mo_sink(1:icharlnf(mo_sink))
     &  // ';finish'
      call dotask(cmdmess,ierror)
C
C     All done
C
      ierror = 0
 9999 continue
      return
      end
      
