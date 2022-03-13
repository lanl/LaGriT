      subroutine boundary_components
     1     (imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C     
C#######################################################################
C     
C     PURPOSE -
C     
C     Adds the attribute "numbnd" if node based or "numbnd_e"
C     option to current MO to which is written a representative
C     node or element  number of each connected components.
C     In addition, a node attribute id_numb is assigned to the nodes
C     associated with each boundary component set. This is an integer
C     that starts with 1 for the first component, 2 for the second, etc.
C
C     In the option with no
C     second argument or second argument 'node', the edge based
C     connectivity graph is traversed to determine then number of
C     outside boundaries based on itp values. The itp array may
C     need to be updated first, this can be done with the command
C     resetpts/itp. By default, resetpts/itp is called with the
C     node option, it is not called with the element options.
C
C     In the node option numbnd = 0 if a node is an interior node.
C     In the node option with material_id_number, nodes with
C     itp .ne. id_number, numbnd = 0.
C     In the element option all elements numbnd_e are assigned a
C     positive integer value.
C     In the element option with material_id_number, elements with
C     itetclr .ne. material_id_number are assigned, numbnd_e = 0.
C     
C     FORMAT:
C     boundary_components / [reset|noreset]
C     boundary_components / node / [reset|noreset]
C     boundary_components / element / [reset|noreset]
C     boundary_components / node    / material_id_number / [reset|noreset]
C     boundary_components / element / material_id_number / [reset|noreset]
C     
C     
C     INPUT ARGUMENTS -
C     
C     XMSGIN ]
C     MSGIN  ] - Input message
C     IMSGIN ]
C     
C     NWDS     - Number of words in the input message
C     
C     OUTPUT ARGUMENTS -
C     
C     IERR1    - Error flag
C     
C     CHANGE HISTORY -
C     
C     Original Version - Mike Murphy - 97
C     Error checking added - T.Cherry - 9/97
C     
C#######################################################################
C
C      $Log: boundary_components.f,v $
C      Revision 2.00  2007/11/05 19:45:46  spchu
C      Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   11 Apr 2007 14:41:44   gable
CPVCS    Clean up some parsing of input arguments.
CPVCS    
CPVCS       Rev 1.5   11 Apr 2007 09:53:14   gable
CPVCS    Added a bunch of new options to compute connected components based on
CPVCS    edge connectivity of a single material and options for element face
CPVCS    based connectivity. Also set default to call resetpts/itp but added
CPVCS    option to call or not call resetpts.
CPVCS    
CPVCS       Rev 1.4   04 Apr 2007 16:53:02   gable
CPVCS    Add log info inside source code.
C
C#######################################################################
C     
C     
      implicit NONE
C     
      include 'local_element.h'
      include 'chydro.h'
      include 'consts.h'
C     
C#######################################################################
C     
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierr1
      integer icharlnf
C     
C#######################################################################
C     
C     LOCAL
c     
      integer ierror, icscode, ierrw
      integer nnodes, ilencmo, itypcmo, nelements
      integer ilen, ityp, itype, if_reset
      integer icount, ic, in, istart, idum, it, jt, jf, itest
      integer i_edge, inode1, inode2, ifind1, ifind2
      integer i_face, nvector, nef_cmo, mbndry, ijob, id_material
      character*132 logmess
C     
      pointer (ipimt, imt)
      integer  imt(1000000)
      pointer (ipitp1, itp1)
      integer  itp1(1000000)
      pointer (ipisn1, isn1)
      integer  isn1(1000000)
      pointer (ipnumbnd, numbnd)
      integer numbnd(1000000)
      pointer (ipid_numb, id_numb)
      integer id_numb(1000000)
      pointer (ipid_numb_e, id_numb_e)
      integer id_numb_e(1000000)
      pointer (ipikey_tmp, ikey_tmp)
      integer ikey_tmp(1000000)
C     
      integer findset
C     *****************************************************************
C     
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
      pointer (ipitettyp, itettyp )
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff )
      integer itetoff(1000000)
      pointer (ipitet, itet1 )
      integer itet1(1000000)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(1000000)
      pointer (ipjtet, jtet )
      integer jtet(1000000)
C     
      pointer (iprank, rank)
      integer rank  (10000000)
      pointer (ipdegree, degree)
      integer degree(10000000)
       pointer (ipic_set, ic_set)
      integer ic_set(10000000)

      
C#######################################################################
C     
      character*132 dotask_command
      character*32 isubname, cmo
C     
C#######################################################################
C     
C     Define the memory-management partition.
C     
      isubname='boundary_components'
C     
C     ******************************************************************
      call cmo_get_name(cmo,icscode)
C     
C     Parse the input arguments.
C     
      id_material = 0
C     Find number of exterior connected components.
      if (nwds .eq. 1) then
         ijob = 1
         if_reset = 1
      endif
C
C     Find number of disjoint element or boundary node sets.
C
      if((nwds .ge. 2) .and. (msgtype(2) .eq. 3)) then
         if(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'element')then
            ijob = 2
            if_reset = 0
         elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'node')then
            ijob = 1
            if_reset = 1
         elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'reset')then
            if_reset = 1
         elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'noreset')then
            if_reset = 0
         endif
      endif
C
C     Find number of disjoint element or node sets in a subset of the mesh
C     defined by a single material color. Node color defined by imt,
C     element color defined by itetclr.
C
      if ((nwds .ge. 3) .and. (msgtype(3) .eq. 1)) then
         if(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'element')then
            id_material = imsgin(3)
            ijob = 2
            if_reset = 0
         elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'node')then
            id_material = imsgin(3)
            ijob = 1
            if_reset = 1
         endif
      elseif ((nwds .ge. 3) .and. (msgtype(3) .eq. 3)) then
         if(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'reset')then
            if_reset = 1
         elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'noreset')then
            if_reset = 0
         endif
      endif

      if ((nwds .eq. 4) .and. (msgtype(4) .eq. 3)) then
         if(cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'reset')then
            if_reset = 1
         elseif(cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'noreset')then
            if_reset = 0
         endif
      endif
C     ******************************************************************
C     
      call cmo_get_name(cmo,icscode)
C     
      call cmo_get_info('nnodes',cmo,nnodes,ilencmo,itypcmo,icscode)
      call cmo_get_info('nelements',cmo,
     *     nelements,ilencmo,itypcmo,icscode)
C
C     Exit if MO is empty
C
      if(ijob .eq. 1)then
         if(nnodes .eq. 0)then
            write(logmess,*)'Number of nodes = 0'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,*)'No Action'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
      elseif(ijob .eq. 2)then
         if(nelements .eq. 0)then
            write(logmess,*)'Number of elements = 0'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,*)'No Action'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
      endif

      call cmo_get_info('imt',cmo,ipimt,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C     
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info
     >     ('faces_per_element',cmo,nef_cmo,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      dotask_command = 'log/tty/off ; finish'
      call dotask(dotask_command,ierror)
C
C     If if_reset = 1, reset itp array values.
C
      if(if_reset .eq. 1)then
         dotask_command = 'resetpts/itp;finish'
         call dotask(dotask_command,ierror)
      endif
C     
C     If ijob = 1, add or check for a node attribute called numbnd to cmo
C
      if(ijob .eq. 1)then
      call cmo_get_info('numbnd',cmo,ipnumbnd,ilen,ityp,icscode)
      if(icscode.ne.0) then         
         dotask_command = 'cmo/addatt/' //
     >        cmo(1:icharlnf(cmo)) //
     >        '/' //
     >        'numbnd' //
     >        '/vint/scalar/nnodes/linear/permanent/afgx/0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >        call x3d_error(isubname,'addatt boundry_components')
C
C     Get the pointer to the newly created array
         call cmo_get_info('numbnd',cmo,ipnumbnd,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      endif
         
      call cmo_get_info('id_numb',cmo,ipid_numb,ilen,ityp,icscode)
      if(icscode.ne.0) then         
         dotask_command = 'cmo/addatt/' //
     >        cmo(1:icharlnf(cmo)) //
     >        '/' //
     >        'id_numb' //
     >        '/vint/scalar/nnodes/linear/permanent/afgx/0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >        call x3d_error(isubname,'addatt boundry_components')
C
C     Get the pointer to the newly created array
         call cmo_get_info('id_numb',cmo,ipid_numb,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      endif
C     
C     If ijob = 2, add or check for an element  attribute called numbnd_e to cmo
C
      elseif(ijob .eq. 2)then
      call cmo_get_info('numbnd_e',cmo,ipnumbnd,ilen,ityp,icscode)
      if(icscode.ne.0) then
         
         dotask_command = 'cmo/addatt/' //
     >        cmo(1:icharlnf(cmo)) //
     >        '/' //
     >        'numbnd_e' //
     >        '/vint/scalar/nelements/linear/permanent/afgx/0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >        call x3d_error(isubname,'addatt boundary_components')
C
C     Get the pointer to the newly created array
C     CAUTION: numbnd_e has same memory pointer as numbnd
C
         call cmo_get_info('numbnd_e',cmo,ipnumbnd,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      endif
      call cmo_get_info('id_numb_e',cmo,ipid_numb_e,ilen,ityp,icscode)
      if(icscode.ne.0) then         
         dotask_command = 'cmo/addatt/' //
     >        cmo(1:icharlnf(cmo)) //
     >        '/' //
     >        'id_numb_e' //
     >        '/vint/scalar/nelements/linear/permanent/afgx/0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >        call x3d_error(isubname,'addatt boundry_components')
C
C     Get the pointer to the newly created array
       call cmo_get_info('id_numb_e',cmo,ipid_numb_e,ilen,ityp,icscode)
       if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
         
      endif
      endif
c     
c     set up work vectors nnodes or nelements long
c     
      if(ijob .eq. 1)then
         nvector = nnodes
      elseif(ijob .eq. 2)then
         nvector = nelements
      endif
C
C     Create some temporary work vectors.
C
      call mmgetblk("rank",  isubname,iprank,  nvector,1,ierror)
      call mmgetblk("degree",isubname,ipdegree,nvector,1,ierror)
      call mmgetblk("ic_set",isubname,ipic_set,nvector,1,ierror)
      icount = 0
      ic = 0
      do in = 1, nvector
         rank(in)   = 0
         degree(in) = 0
         numbnd(in) = 0
         ic_set(in) = 0
      enddo
      
      if(ijob .eq. 1)then
         do in = 1, nnodes
            if(id_material .eq. 0)then
C
C           Make a set of exterior boundary nodes, itp=10
C
               if(itp1(in) .eq. 10)then
                  call makeset(in,numbnd,rank,nnodes)
                  icount = icount + 1
               endif
            elseif(id_material .gt. 0)then
C
C           Make a set of all nodes with imt = id_material
C
               if(imt(in) .eq. id_material)then
                  call makeset(in,numbnd,rank,nnodes)
                  icount = icount + 1
               endif
            endif
C
C           Do some extra work to deal with parent/child nodes
C            
            if(id_material .eq. 0)then
             if(itp1(in) .eq. 41)then
               call makeset(in,numbnd,rank,nnodes)
               istart = isn1(in)
               if(itp1(istart) .eq. 12)then
                  do while (istart .ne. in)
                     call makeset(istart,numbnd,rank,nnodes)
                     idum = findset(in,numbnd,rank,nnodes)
                     call unionset(idum,istart,numbnd,rank,nnodes)
                     istart = isn1(istart)
                  enddo
                  icount = icount + 1
               endif
             endif
            
             if((itp1(in) .eq. 12) .and. (isn1(in) .eq. 0))then
               call makeset(in,numbnd,rank,nnodes)
               icount = icount + 1
             endif
            endif
         enddo
      elseif(ijob .eq. 2)then
         if(id_material .eq. 0)then
         do in = 1, nelements
            call makeset(in,numbnd,rank,nvector)
            icount = icount + 1
         enddo
         elseif(id_material .gt. 0)then
         do in = 1, nelements
            if(itetclr(in) .eq. id_material)then
            call makeset(in,numbnd,rank,nvector)
            icount = icount + 1
            endif
         enddo
         endif
      endif
C     
C     Look for disjoint  node (vertex) sets
C     
      if(ijob .eq. 1)then
C     Loop over all elements
         do it=1,nelements
C           Loop over all node pairs of all edges of element it
            do i_edge = 1, nelmnee(itettyp(it))
               inode1 =
     1              itet1(itetoff(it)+ielmedge1(1,i_edge,itettyp(it)))
               inode2 =
     1              itet1(itetoff(it)+ielmedge1(2,i_edge,itettyp(it)))
            if(id_material .eq. 0)then
            if(((itp1(inode1) .eq. 10).or.(itp1(inode1) .eq. 12)) .and.
     1         ((itp1(inode2) .eq. 10).or.(itp1(inode2) .eq. 12)))then
                  ifind1 = findset(inode1,numbnd,rank,nvector)
                  ifind2 = findset(inode2,numbnd,rank,nvector)
                  if(ifind1 .ne. ifind2)then
                     call unionset(inode1,inode2,numbnd,rank,nnodes)
                     icount = icount - 1
                  endif
                  degree(inode1) = degree(inode1) + 1
                  degree(inode2) = degree(inode2) + 1
               endif
            elseif(id_material .gt. 0)then
            if((imt(inode1) .eq. id_material) .and. 
     1         (imt(inode2) .eq. id_material))then
                  ifind1 = findset(inode1,numbnd,rank,nvector)
                  ifind2 = findset(inode2,numbnd,rank,nvector)
                  if(ifind1 .ne. ifind2)then
                     call unionset(inode1,inode2,numbnd,rank,nnodes)
                     icount = icount - 1
                  endif
                  degree(inode1) = degree(inode1) + 1
                  degree(inode2) = degree(inode2) + 1
            endif
            endif
            enddo
         enddo
C     
         if(id_material .eq. 0)then
         do in = 1, nnodes
            if((itp1(in) .eq. 10).or.
     1           (itp1(in) .eq. 12).or. (itp1(in).eq. 41))then
               idum = findset(in,numbnd,rank,nnodes)
            endif
         enddo
         endif
C     
C     Look for disjoint element sets
C     
      elseif(ijob .eq. 2)then
C     get number of faces per element for this mesh object
C        Loop over all elements
         do it=1,nelements
C           Loop over all faces of element it
            do i_face=1,nelmnef(itettyp(it))
C     check if element face is on an external boundry
               if(jtet(jtetoff(it)+i_face).eq.mbndry) then
                  jt=0
                  jf=0
C     Check if element face is on an internal boundary
               elseif(jtet(jtetoff(it)+i_face).gt.mbndry)then
                  jt=1+(jtet(jtetoff(it)+i_face)-mbndry-1)/nef_cmo
                  jf=jtet(jtetoff(it)+i_face)-mbndry-nef_cmo*(jt-1)
C     Volume element
               else
                  jt=1+(jtet(jtetoff(it)+i_face)-1)/nef_cmo
                  jf=jtet(jtetoff(it)+i_face)-nef_cmo*(jt-1)
               endif
C     Only call unionset for non-exterior faces
C     jt is the element number of the element on the other side of the face
C
               if(jt .ne. 0)then
               if(id_material .eq. 0)then
               ifind1 = findset(it,numbnd,rank,nvector)
               ifind2 = findset(jt,numbnd,rank,nvector)
               if(ifind1 .ne. ifind2)then
                  call unionset(it,jt,numbnd,rank,nvector)
                  icount = icount - 1
               endif
               degree(it) = degree(it) + 1
               degree(jt) = degree(jt) + 1
               elseif((id_material .gt. 0) .and. 
     1                (itetclr(it) .eq. id_material) .and.
     2                (itetclr(jt) .eq. id_material)) then
               ifind1 = findset(it,numbnd,rank,nvector)
               ifind2 = findset(jt,numbnd,rank,nvector)
               if(ifind1 .ne. ifind2)then
                  call unionset(it,jt,numbnd,rank,nvector)
                  icount = icount - 1
               endif
               degree(it) = degree(it) + 1
               degree(jt) = degree(jt) + 1
               endif
               endif
            enddo
         enddo
      endif
C     ...............................................................
C     Clean up numbnd array
C
      it = 1
      if(ijob .eq. 1)then
         do in = 1, nnodes
            if(id_material .eq. 0)then
            if((itp1(in) .eq. 10).or.
     1           (itp1(in) .eq. 12).or. (itp1(in).eq. 41))then
               idum = findset(in,numbnd,rank,nvector)
               if (degree(idum).ne.9999) then
                  degree(idum) = 9999
                  it = it + 1
               endif
            endif
            elseif(id_material .gt. 0)then
            if(imt(in) .eq. id_material)then
               idum = findset(in,numbnd,rank,nvector)
               if (degree(idum).ne.9999) then
                  degree(idum) = 9999
                  it = it + 1
               endif
            endif
            endif
         enddo
      elseif(ijob .eq. 2)then
         if(id_material .eq. 0)then
         do in = 1, nelements
            idum = findset(in,numbnd,rank,nvector)
            if (degree(idum).ne.9999) then
               degree(idum) = 9999
            endif
         enddo
         elseif(id_material .gt. 0)then
         do in = 1, nelements
            if(itetclr(in) .eq. id_material)then
            idum = findset(in,numbnd,rank,nvector)
            if (degree(idum).ne.9999) then
               degree(idum) = 9999
            endif
            endif
         enddo
         endif
      endif
C     ...............................................................
C     Sort the numbnd array and count up the number of members in each set.
C
C     Write output
C
      if(ijob .eq. 1)then
         dotask_command =
     $        'sort/-def-/index/-def-/ikey_tmp/numbnd;finish'
      elseif(ijob .eq. 2)then
         dotask_command =
     $        'sort/-def-/index/-def-/ikey_tmp/numbnd_e;finish'
      endif
      call dotask(dotask_command,ierror)
      call cmo_get_info('ikey_tmp',cmo,ipikey_tmp,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')

         write (logmess,*) 'CONNECTED COMPONENTS: '
         call writloga('default',1,logmess,0,ierrw)
      if(ijob .eq. 1)then
         if(id_material .eq. 0)then
           write (logmess,*)
     1    icount, ' different exterior boundary components'
         elseif(id_material .gt. 0)then
           write (logmess,*)icount, 
     1       ' different connected components with imt = ',id_material
         endif
         call writloga('default',0,logmess,0,ierrw)
         write (logmess,*)
     1     'Set#        Representitive Node #        # Nodes in Set '
         call writloga('default',0,logmess,0,ierrw)
         write (logmess,*)
     1     '           (numbnd_e(node#) attribute) '
         call writloga('default',0,logmess,0,ierrw)
      elseif(ijob .eq. 2)then
         if(id_material .eq. 0)then
         write (logmess,*)
     1        icount, ' disjoint element mesh components'
         elseif(id_material .gt. 0)then
         write (logmess,*)icount, 
     1     ' disjoint element mesh components with itetclr = ',
     2       id_material
         endif
         call writloga('default',0,logmess,0,ierrw)
         write (logmess,*)
     1     'Set#       Representitive Element#     #Elements in Set '
         call writloga('default',0,logmess,0,ierrw)
         write (logmess,*)
     1     '           (numbnd_e(elem#) attribute) '
         call writloga('default',0,logmess,0,ierrw)
      endif

      icount = 0
      do in = 1, nvector
         if(numbnd(ikey_tmp(in)) .ne. 0)then
            if(icount .eq. 0)then
               itest = numbnd(ikey_tmp(in))
               icount = icount + 1
               if(ijob .eq. 1)then
                  id_numb(ikey_tmp(in)) = icount
               elseif(ijob .eq. 2)then
                  id_numb_e(ikey_tmp(in)) = icount
               endif
            endif
            if(itest .eq. numbnd(ikey_tmp(in)))then
               ic_set(icount) = ic_set(icount) + 1
               if(ijob .eq. 1)then
                  id_numb(ikey_tmp(in)) = icount
               elseif(ijob .eq. 2)then
                  id_numb_e(ikey_tmp(in)) = icount
               endif
            else
               write(logmess,210)icount, itest, ic_set(icount)
               call writloga('default',0,logmess,0,ierrw)
               icount = icount + 1
               if(ijob .eq. 1)then
                  id_numb(ikey_tmp(in)) = icount
               elseif(ijob .eq. 2)then
                  id_numb_e(ikey_tmp(in)) = icount
               endif
               ic_set(icount) = ic_set(icount) + 1
               itest = numbnd(ikey_tmp(in))
            endif
         endif
      enddo
      
      write(logmess,210)icount, itest, ic_set(icount)
      call writloga('default',0,logmess,1,ierrw)
 210  format(i7,10x,i10,10x,i10)
C     ...............................................................
C     Clean up sort key vector
C
      dotask_command = 'cmo/DELATT/' // cmo(1:icharlnf(cmo)) //
     1     '/ikey_tmp;finish'
      call dotask(dotask_command,ierror)
      dotask_command = 'log/tty/on ; finish'
      call dotask(dotask_command,ierror)
C     
C     Set the error flag to normal.
C     
 9999 ierr1=0
C     
C     ******************************************************************
C     
C     Release the temporary memory back to the memory manager.
C     
      call mmrelprt(isubname,icscode)
C     
C     ******************************************************************
C     
      return
      end
c
c Implemented by Michael Murphy (murphy@lanl.gov)
c
c Uses the Union-Find (aka "disjoint-set") data structure,
c non-recursive and implemented in arrays, to compute
c connected components of the boundary.
c
c Reference:
c     "Introduction to Algorithms", Chapter 22
c      Corman, Leiserson, and Rivest
c
c Each procedure has the following arguments in common:
c              p   --  an array containing the representative elements
c                      of the sets.  p[i] is the representative element of
c                      the set containing element i.
c
c              rank -- an array used for efficiency.  Internal use only.
c
c              nvert -- the size of the arrays p and rank
 
 
c
c  PROCEDURE:   printcomponents(p, rank, nvert)
c
      subroutine printcomponents(p, rank, nvert)
      implicit none
 
      integer p(*)
      integer rank(*)
      integer nvert
      integer findset
      integer i
 
      do i=1,nvert
         print *, i,' is a member of set ',findset(i,p,rank,nvert)
      end do
      end
 
 
c
c  FUNCTION:   findset(x, p, rank, nvert)
c     Finds the "representative element" of the set associated with x.
c
      integer function findset(x, p, rank, nvert)
      implicit none
      integer x
      integer p(*)
      integer rank(*)
      integer nvert
      integer res
      integer prev,next
 
      res = x
      do while (res .ne. p(res))
         res = p(res)
      end do
 
      findset=res
 
 
c I'm forced to perform path compression non-recursively because f77 sucks
c and f90 doesn't work
 
      prev = x
      do while(res.ne.p(prev))
         next = p(prev)
         p(prev) = res
         prev = next
      end do
      return
      end
 
 
c
c  PROCEDURE:   makeset(x, p, rank, nvert)
c     Makes a set for vertex x.
c
      subroutine makeset(x, p, rank, nvert)
      implicit none
      integer x
      integer p(*)
      integer rank(*)
      integer nvert
 
      p(x) = x
      rank(x) = 0
      end
 
 
c
c  PROCEDURE:  unionset(x, y, p, rank, nvert)
c     Takes the union of the set associated with vertex x with the
c     set associated with vertex y.
c
      subroutine unionset(x, y, p, rank, nvert)
      implicit none
      integer x,x1
      integer y,y1
      integer p(*)
      integer rank(*)
      integer nvert
      integer findset
 
 
c      print *,'unionset calling linkset'
      x1=findset(x,p,rank,nvert)
      y1=findset(y,p,rank,nvert)
      call linkset(x1,y1,p,rank,nvert)
c      print *,'unionset returned from linkset'
      return
      end
 
 
c
c  PROCEDURE:   linkset(x, y, p, rank, nvert)
c      Links two sets (x, y) together.  (Used only by unionset)
c
      subroutine linkset(x, y, p, rank, nvert)
      implicit none
      integer x
      integer y
      integer p(*)
      integer rank(*)
      integer nvert
 
      if (rank(x).gt.rank(y)) then
         p(y) = x
      else
         p(x) = y
         if (rank(x).eq.rank(y)) then
            rank(y) = rank(y) + 1
         end if
      end if
c      print *,'returning from linkset'
c      call printcomponents(p,rank,nvert)
      return
      end
 
 
