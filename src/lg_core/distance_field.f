      subroutine distance_field
     &   (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C       Compute the distance field, dist, between the nodes of one
C       mesh object, mo_source, and another mesh object, mo_sink. The
C       attribute dist will contain the minimum distance from each node
C       of mo_sink to the nearest node in mo_source.
C
C     NOTES -
C
C      Syntax for this command:
C      compute / distance_field / mo_sink / mo_source / distance_field_attribute
C
C        
C######################################################################
C     CHANGE HISTORY -
C
C        $Log: distance_field.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   05 Jun 2007 15:33:24   gable
CPVCS    Initial revision.
C
C######################################################################
C
      implicit none
C
C     Subroutine Input Variables
C
      integer       nwds, ierror
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
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
C     Now execute a series of dotask operations that compute the distance field.
C
C    Create temporary work arrays.
C
      cmdmess = 'cmo/addatt/'// mo_sink(1:icharlnf(mo_sink))
     &  // '/temp_tmp_var/VDOUBLE/scalar/nnodes/linear/' //
     &     'temporary/   /-1.0;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/addatt/'// mo_sink(1:icharlnf(mo_sink))
     &  // '/xobj_tmp_var/VDOUBLE/scalar/nnodes/linear/' //
     &     'temporary/   /-1.0;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/addatt/'// mo_sink(1:icharlnf(mo_sink))
     & // '/yobj_tmp_var/VDOUBLE/scalar/nnodes/linear/' //
     &    'temporary/   /-1.0;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/addatt/'// mo_sink(1:icharlnf(mo_sink))
     & // '/zobj_tmp_var/VDOUBLE/scalar/nnodes/linear/' //
     &    'temporary/   /-1.0;finish'
      call dotask(cmdmess,ierror)
C
C     Create permanent array to hold the distance field.
C
      cmdmess = 'cmo/addatt/'// mo_sink(1:icharlnf(mo_sink)) // '/'
     &     // cattribute(1:icharlnf(cattribute)) // 
     &    '/VDOUBLE/scalar/nnodes/linear/' //
     &    'permanent/   /-1.0;finish'
      call dotask(cmdmess,ierror)
C
C     Use voronoi interpolation to find the x,y,z coordinate of the
C     nearest node of mo_src to each node of mo_sink
C
      cmdmess = 'interpolate / voronoi / ' 
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/xobj_tmp_var/ 1 0 0 / ' 
     &    // mo_src(1:icharlnf(mo_src)) //
     &    '/ xic / keepatt;finish '
      call dotask(cmdmess,ierror)
      cmdmess = 'interpolate / voronoi / ' 
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/yobj_tmp_var/ 1 0 0 / ' 
     &    // mo_src(1:icharlnf(mo_src)) //
     &    '/ yic / keepatt;finish '
      call dotask(cmdmess,ierror)
      cmdmess = 'interpolate / voronoi / ' 
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/zobj_tmp_var/ 1 0 0 / ' 
     &    // mo_src(1:icharlnf(mo_src)) //
     &    '/ zic ;finish '
      call dotask(cmdmess,ierror)
C
C Now compute distance = sqrt(x**2 + y**2 + z**2)
C
      cmdmess = 'math/sub/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/xic/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/xobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/power/'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) // 
     &    '/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/2.0 ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/sub/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/yic/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/yobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/power/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var' //
     &    '/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/2.0 ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/add/'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) //
     &     '/1 0 0 /'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) // '/'
     &    //  mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/sub/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/zic/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/zobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/power/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var' //
     &    '/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var/2.0 ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/add/'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) //
     &     '/1 0 0 /'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) // '/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'math/power/'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) // 
     &    '/1,0,0/'
     &    // mo_sink(1:icharlnf(mo_sink)) // '/'
     &    // cattribute(1:icharlnf(cattribute)) // 
     &    '/0.5 ;finish'
      call dotask(cmdmess,ierror)
C
C Clean Up - remove temporary arrays
C
      cmdmess = 'cmo/delatt/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/temp_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/delatt/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/xobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/delatt/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/yobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
      cmdmess = 'cmo/delatt/'
     &    // mo_sink(1:icharlnf(mo_sink)) //
     &    '/zobj_tmp_var ;finish'
      call dotask(cmdmess,ierror)
C
C     All done
C
      ierror = 0
 9999 continue
      return
      end
      
