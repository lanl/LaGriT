      subroutine tree_to_fe 
     &   (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C      Given a 3D hexmesh or a 2D quadmesh (that has been produced using
C      the octree/quadtree refine methods) this will remove all parent
C      elements.
C
C      When a quadtree or octree is refined (splitting the quads or
C      hexes into smaller child-elements), the older, larger elements
C      remain part of the data structure for bookkeeping purposes.
C      However, it is sometimes useful to remove those elements, and
C      only be left with the elements at the smallest scale at each
C      location. This method removes the parent elements and compresses
C      the data structure to contain only the lowest level of the quad-
C      or octree.
C      
C     SYNTAX -
C      grid2grid / tree_to_fe / [sink_name] / [src_name]
C      
C      If the source and sink names are not specified, it will use the
C      current active mesh, and overwrite itself. If just the source
C      name is not specified, it will create a new mesh using the
C      current mesh object as the source. The sink mesh will be the
C      current active mesh object at the conclusion of the routine.

C######################################################################
C     CHANGE HISTORY -
C
C      Developed by agable 6-10-2010
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
      character*32  mo_name, src_mo, sink_mo
      character*132 logmess
      character*256 cmd
C
C     Variables for subroutine calls
C
      integer       i
      character*32 ctype,crank,clen,cinter,cpers,cio
C
C     Function variable
C
      integer icharln
C
C######################################################################
C
      isubname = 'tree_to_fe'
C
C     Check validity of arguments
C
      if(nwds < 2 .or. nwds > 4) then
         write(logmess,'(a)')
     &    'ERROR tree_to_fe: Wrong number of arguments.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(1) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR tree_to_fe: Argument 1 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(2) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR tree_to_fe: Argument 2 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
C     If they specified a sink mesh, make sure it is valid. If they
C     didn't specify a source mesh, use the current mesh object. 
      if (nwds .eq. 3) then
          if(msgtype(3) .ne. 3) then
             write(logmess,'(a)')
     &     'ERROR tree_to_fe: Argument 3 must be of type character.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -2
             goto 9999
          endif
          sink_mo = cmsgin(3)
          call cmo_get_name(src_mo, ierror)
          if(ierror.ne.0) then
             write(logmess,'(a)')
     &        'ERROR tree_to_fe: no existing default source mesh.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -1
             go to 9999          
          endif
C     If they specified a source mesh, make sure it is valid.
      elseif (nwds .eq. 4) then
          if (msgtype(4) .ne. 3) then
              write(logmess,'(a)')
     &      'Error tree_to_fe: Argument 4 must be of type character.'
              call writloga('default',0,logmess,0,ierror)
              ierror = -2
              goto 9999
          endif
          src_mo= cmsgin(4)
          call cmo_exist(src_mo,ierror)
          if(ierror.ne.0) then
             write(logmess,'(a)')
     &        'ERROR tree_to_fe: source mesh object does not exist.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -1
             go to 9999
          endif
          if(msgtype(3) .ne. 3) then
             write(logmess,'(a)')
     &     'ERROR tree_to_fe: Argument 3 must be of type character.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -2
             goto 9999
          endif
          sink_mo = cmsgin(3)
C     If they specified neither a source nor a sink mesh, make both
C     be the current mesh object.
      else
          call cmo_get_name(src_mo, ierror)
          if(ierror.ne.0) then
             write(logmess,'(a)')
     &        'ERROR tree_to_fe: no existing default source mesh.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -1
             go to 9999          
          endif
          sink_mo = src_mo
      endif

C     Make sure itetkid and itetlev exist in this mesh
      call cmo_get_attparam('itetkid', src_mo, i, ctype, crank,
     &                      clen, cinter, cpers, cio, ierror)
      if (clen(1:4) .ne. 'nele') then
         write(logmess,'(a)')
     &    'ERROR tree_to_fe: mesh object is not a quad/octree'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif
C     same check for itetlev just to be sure
      call cmo_get_attparam('itetlev', src_mo, i, ctype, crank,
     &                      clen, cinter, cpers, cio, ierror)
      if (clen(1:4) .ne. 'nele') then
         write(logmess,'(a)')
     &    'ERROR tree_to_fe: mesh object is not a quad/octree'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif

C     Create the sink mesh, which will also be our working mesh
      cmd = 'cmo/copy/'//sink_mo//'/'//src_mo//'; finish'
      call dotask(cmd, ierror)


C
C     The following code essentially executes this macro:
C
C     eltset/parentset/itetkid/gt/0
C
C     rmpoint/element/eltset get parentset
C     rmpoint compress
C     resetpts / itp
C
C     eltset/parentset/delete
C
C     finish
C

C     Create an element set of all elements with at least one kid
      cmd = 'eltset / -parentset- / itetkid / gt / 0; finish'
      call dotask(cmd, ierror)
      
C     Remove all elements that are parents
      cmd = 'rmpoint/element/eltset,get,-parentset-; finish'
      call dotask(cmd, ierror)

C     Update the mesh object to its new size
      cmd = 'rmpoint / compress; finish'
      call dotask(cmd, ierror)

C     Make sure the mesh object is consistent
      cmd = 'resetpts / itp; finish'
      call dotask(cmd, ierror)

C     Remove the temporary elementset
      cmd = 'eltset/-parentset-/delete; finish'
      call dotask(cmd, ierror)

C     Show the user that everything went well
      cmd = 'quality; finish'
      call dotask(cmd, ierror)

C
C     All done
C
      ierror = 0
 9999 continue
      return
      end
C     
