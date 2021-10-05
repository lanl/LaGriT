
       subroutine stack_fill(imsgin,xmsgin,cmsgin,msgtyp,nwds,
     *             ierr_return)
C
C#####################################################################
C
C
C      PURPOSE -
C
C       Input is a stacked layer cmo created with the topology of a 
C       sequence of surfaces. The stacked cmo is created with the
C       stack/layers command. 
C       By assuming that the surfaces have identical connectivity, 
C       the code then generates the connectivity of a hex or prism
C       grid by joining the elements of consecutive surfaces that have 
C       the same logical location on those surfaces.
C       The stack cmo must have the added attributes:
C       nlayers = number of layers in the stack cmo
C       nnperlayer = number of nodes in each layer
C       neperlayer = number of elements in each layer
C
C       The options allow a tet mesh to be created from the 
C       hex or prism mesh.
C       By default hextotet is called with 3 for prism elements.
C       By default hextotet is called with 6 for hex elements.
c
c       given the following prism:
c       data ifelmpri / 7 / 1,3,2 4,5,6
c
c           4------6     Node Coordinates:
c           |\    /|      1 (0,0,0)
c           | \  / |      2 (5,0,0)
c           |  5   |      3 (0,5,0)
c           |  |   |      4 (0,0,6)
c           1--|---3      5 (5,0,6)
c            \ |  /       6 (0,5,6)
c             \| /
c              2
c
c       write the connectivity list as 123456
c
c      ielmface1(vertex#,face#,7) (see blockcom.f)
c      normal to each face points out from the center of the element
C
C      given the folowing hex:
C      data ifelmhex / 8 / 1,4,3,2 5,6,7,8
C
C      SYNTAX:
C      STACK/ FILL / cmoout/ cmostack / [tet] [color_method] 
C
C        cmoout        - the output prism, hex or tet cmo
C        cmoin         - the input stacked layer cmo
C        tet           - calls hextotet 6 for volume hex 
C                        calls hextotet 3 for volume prism
C        color_method  - method of filling itetclr and imt
C                        stratclr, surfclr, or usrclr (default) 
C
C      EXAMPLES:
C        cmo create cmo_stack
C        stack/quadlayers/avs/ q3.inp 1, q5.inp, qtop.inp    
C        stack/fill/ cmo_hex / cmo_stack
C        dump gmv stack.gmv cmo_stack
C        dump gmv hex.gmv cmo_hex
C
C
C      INPUT ARGUMENTS - 
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C      Original version - T.Cherry - jul 2001
C
C        $Log: stack_fill.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#####################################################################
C
      implicit none
      include "local_element.h"
      integer lenptr
C
      parameter (lenptr=1000000)
C
C####################################################################
C
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)

C     SET POINTERS FOR INCOMING STACK CMO
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)

      pointer (ipimt1, imt1)
C
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
      integer imt1(lenptr),
     &        itetclr(lenptr), itettyp(lenptr),
     &        itet(lenptr), jtet(lenptr)

C     SET POINTERS FOR OUTGOING cmo
C
      pointer (ipxica, xica)
      pointer (ipyica, yica)
      pointer (ipzica, zica)
      pointer (ipitetclra, itetclra)
      pointer (ipitettypa, itettypa)
      pointer (ipiteta, iteta)
      pointer (ipjteta, jteta)
      pointer (ipitetoffa, itetoffa)
      pointer (ipjtetoffa, jtetoffa)

      pointer (ipimt1a, imt1a)
C
      real*8 xica(lenptr),yica(lenptr),zica(lenptr)
      integer imt1a(lenptr),
     *        itetclra(lenptr), itettypa(lenptr),
     *        iteta(lenptr),jteta(lenptr),
     *        itetoffa(lenptr),jtetoffa(lenptr)


C     LOCAL VARIABLES
      integer STRATCLR, USRCLR, SURFCLR
      integer i,ii,ics,ier,ierr,ierrw, ierr_return
      integer ntet, ielmtype, iopttet, ioptclr, itoff, jtoff, 
     *   nnodes,nelm,nlay,ilen,ityp,nodperlay,nlayers, 
     *   nremainder,nnod_per_lay,nelm_per_lay,nelmout,
     *   imesh_type,topo_dim,geom_dim,nod_per_elm,
     *   fac_per_elm,edg_per_elm,nen,lnen
      integer nelmin,icnt,icnt1,jcnt,jcnt_plus,isurf,inod

      integer icharlnf

      character*32 isubname, cmoout,cmoin,cmoh,mesh_type,coutype
      character*132 logmess
      character*1024  cbuf

      data STRATCLR / 2 /
      data SURFCLR / 1 /
      data USRCLR / 0 /
C
C BEGIN
      isubname = 'stack_fill'
      ierr_return = -1
      ierr = 0
      ntet = 3
      nodperlay = 0
      nlayers = 0
      iopttet = 0
      ioptclr = 0
      cmoh = '-defh-'
      cmoin = '-notset-'
      cmoout = '-notset-'
C
C     READ ALL PARSER VALUES, i is next value, nwds is last
C     stack / fill / cmoout / cmoin [tet] [color_method]

c     Read required parameters
      i = 3
      if (msgtyp(i).eq.3 ) then
        cmoout = cmsgin(i)
      else
        write(logmess,'(a)')'ERROR: expected output cmo name'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999
      endif
      i= i+1

      if (msgtyp(i).eq.3 ) then
        cmoin = cmsgin(i)
      else
        write(logmess,'(a)')'ERROR: expected input cmo name'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999
      endif
      i= i+1

C     Read optional parameters
      do ii = i, nwds
        if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:3).eq.'tet') 
     *      iopttet = 1 
        if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:5).eq.'strat') 
     *      ioptclr = STRATCLR 
        if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:3).eq.'usr') 
     *      ioptclr = USRCLR 
        if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:4).eq.'surf') 
     *      ioptclr = SURFCLR 
      enddo

C     Check the mesh object names
      call cmo_exist(cmoin,ics)
      if(ics.ne.0) then
         write(logmess,'(a,a)')
     *   'STACK: Not a valid mesh object: ',cmoin
         call writloga('default',1,logmess,1,ierrw)
         goto 9999
      endif

c     Get number of layers and number of nodes per layer from cmo 
      call cmo_get_info('nnodes',cmoin,nnodes,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info nnodes')
      call cmo_get_info('nelements',cmoin,nelmin,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info nelements')

c     Get attributes added during creation of stack cmo
      call cmo_get_info('nlayers',    cmoin, nlay,
     *                   ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info nlayers')
      call cmo_get_info('nnperlayer', cmoin, nnod_per_lay,
     *                   ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info nnperlayer')
      if (nlay.le.0 .or. nnod_per_lay.le.0) then
         write(logmess,'(a,i5)')
     >  'ERROR: Undefined number of layers or nodes: ',nlay,nnod_per_lay
         call writloga('default',0,logmess,1,ics)
         goto 9999
      endif

      nremainder = mod(nnodes,nlay)
      if (nremainder.ne.0) then
          write(logmess,'(a,i5)')
     >    'ERROR: INCORRECT number of layers: ',nlay
          call writloga('default',0,logmess,1,ics)
          return
      endif
      if ((nnodes/nlay).ne.nnod_per_lay) then
          write(logmess,'(a,i5)')
     >    'ERROR: INCORRECT number of layers or nodes: ',nlay,nnodes
          call writloga('default',0,logmess,1,ics)
          return
      endif

      nelm_per_lay = nelmin/nlay
      nelmout = nelmin - nelm_per_lay

c     Get the mesh type of the incoming cmo
c     Should be quad, tri, or hybrid
      mesh_type = 'notset'
      call cmo_get_mesh_type(cmoin,mesh_type,imesh_type,ier)
      if (ier.ne.0 .or. imesh_type.le.0) then
          write(logmess,'(a,a,a,a)') 'ERROR: Undefined mesh type: ',
     >    mesh_type(1:3),' for ',cmoin
          call writloga('default',0,logmess,1,ics)
          goto 9999
      endif
      if (mesh_type(1:3).ne.'tri' .and. mesh_type(1:3).ne.'qua') then
          write(logmess,'(a,a)')
     >    'ERROR: Unsupported stack mesh type: ',mesh_type
          call writloga('default',0,logmess,1,ics)
          goto 9999
      endif

c     Get pointers to the incoming vector mesh variables
      call cmo_get_info('xic', cmoin, ipxic,  ilen,  ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info xic')
      call cmo_get_info('yic', cmoin, ipyic,  ilen,  ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info yic')
      call cmo_get_info('zic', cmoin, ipzic,  ilen,  ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info zic')

      call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itettyp')
      call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itetclr')
      call cmo_get_info('itet',cmoin, ipitet, ilen,  ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itet')
      call cmo_get_info('jtet',cmoin, ipjtet, ilen,  ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info jtet')

      call cmo_get_info('imt1',cmoin, ipimt1, ilen, ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info imt1')


c------------------------------------------------------------------------
c     Derive new cmo from old,  Set active cmo to cmoout
c
c     There are --> nnodes   <-- total nodes
c     There are --> nelmout  <-- total elements
c     There are --> nlay     <-- total surfaces
c     There are --> nnod_per_lay = nnodes/nlay   <-- nodes per surface
c     There are --> nelem_per_lay = nelem/nlay   <-- elements per surface
c     There are --> npri = nelem - nelem_per_lay <-- total prisms
c------------------------------------------------------------------------

c     Create the outgoing cmo  
      call cmo_exist(cmoout, ier)
      if (ier.eq.0) call cmo_release(cmoout, ics)
      cbuf = 'cmo/copy/' // cmoout(1:icharlnf(cmoout)) // '/ '
     *       // cmoin(1:icharlnf(cmoin))
     *       // '/ ; finish'
      call dotaskx3d(cbuf,ier)
      if(ier.ne.0)call x3d_error(isubname, 'cmo copy')
      if (ier.ne.0) goto 9999
      call cmo_set_name(cmoout,ier)

c     SET the outgoing mesh type
c     tri and quad become pri and hex,  hyb added in future
c     need add check to incoming to make sure it is tri or quad
      if (mesh_type(1:3) .eq. 'tri' ) then
         coutype = 'pri'
         ielmtype = ifelmpri
      elseif (mesh_type(1:3) .eq. 'qua' ) then
         coutype = 'hex'
         ielmtype = ifelmhex
      endif

      nod_per_elm = nelmnen(ielmtype)
      fac_per_elm = nelmnef(ielmtype)
      edg_per_elm = nelmnee(ielmtype)
      topo_dim = 3
      geom_dim = 3
      itoff = 0
      jtoff = 0

c     Set pointers for the outgoing mesh scalar variables 
      call cmo_set_info('nnodes', cmoout, nnodes,     1,1,ier)
      call cmo_set_info('nelements',cmoout, nelmout,1,1,ier)
      call cmo_set_info('ndimensions_topo',cmoout,topo_dim,1,1,ier)
      call cmo_set_info('ndimensions_geom',cmoout,geom_dim,1,1,ier)
      call cmo_set_info('nodes_per_element',cmoout,nod_per_elm,1,1,ier)
      call cmo_set_info('faces_per_element',cmoout,fac_per_elm,1,1,ier)
      call cmo_set_info('edges_per_element',cmoout,edg_per_elm,1,1,ier)
      call cmo_newlen(cmoout,ier)

c     Get pointers for the outgoing vector variables
      call cmo_get_info('xic',cmoout,ipxica,ilen,ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info xic')
      call cmo_get_info('yic',cmoout,ipyica,ilen,ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info yic')
      call cmo_get_info('zic',cmoout,ipzica,ilen,ityp, ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info zic')
      call cmo_get_info('itettyp',cmoout,ipitettypa,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itettyp')
      call cmo_get_info('itetclr',cmoout,ipitetclra,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itetclr')

      call cmo_get_info('itet',cmoout,ipiteta,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itet')
      call cmo_get_info('itetoff',cmoout,ipitetoffa,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info itetoff')
      call cmo_get_info('jtet',cmoout,ipjteta,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info jtet')
      call cmo_get_info('jtetoff',cmoout,ipjtetoffa,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info jtetoff')

      call cmo_get_info('imt1',cmoout,ipimt1a,ilen,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info imt1')

C     FOR EACH ELEMENT
C        FILL connectivity in itet arrays
c          nen are the nodes in the new elements
c          lnen are the nodes in the old surface elements 
c        SET element color according to color_method

      nen   = nod_per_elm 
      lnen  = nod_per_elm / 2
      do icnt = 1, nelmout
        icnt1 = icnt- 1
        jcnt = lnen*icnt1
        jcnt_plus = jcnt+(lnen*nelm_per_lay)

C       FILL itet array for prisms
        if (ielmtype .eq. ifelmpri) then
          iteta(nen*(icnt1)+1)=itet(jcnt+1)
          iteta(nen*(icnt1)+2)=itet(jcnt+2)
          iteta(nen*(icnt1)+3)=itet(jcnt+3)
          iteta(nen*(icnt1)+4)=itet(jcnt_plus+1)
          iteta(nen*(icnt1)+5)=itet(jcnt_plus+2)
          iteta(nen*(icnt1)+6)=itet(jcnt_plus+3)

C       FILL itet array for hex
        elseif (ielmtype .eq. ifelmhex) then
          iteta(nen*(icnt1)+1)=itet(jcnt+1)
          iteta(nen*(icnt1)+2)=itet(jcnt+2)
          iteta(nen*(icnt1)+3)=itet(jcnt+3)
          iteta(nen*(icnt1)+4)=itet(jcnt+4)
          iteta(nen*(icnt1)+5)=itet(jcnt_plus+1)
          iteta(nen*(icnt1)+6)=itet(jcnt_plus+2)
          iteta(nen*(icnt1)+7)=itet(jcnt_plus+3)
          iteta(nen*(icnt1)+8)=itet(jcnt_plus+4)
        endif
        itettypa(icnt) = ielmtype
        itetoffa(icnt) = itoff
        itoff          = itoff + nod_per_elm
        jtetoffa(icnt) = jtoff
        jtoff          = jtoff + fac_per_elm

c       SET  materials using the chosen color_method 

c       USRCLR has defined the material values for each surface
c       The itetclr values for input surfaces were ignored
c       This is the default assignment for material colors
        if (ioptclr .eq. USRCLR) then

          itetclra(icnt) = itetclr(icnt)

          if (ielmtype .eq. ifelmpri) then
            imt1a( iteta(nen*(icnt1)+1)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+2)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+3)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+4)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+5)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+6)) = itetclr(icnt)
          elseif (ielmtype .eq. ifelmhex) then
            imt1a( iteta(nen*(icnt1)+1)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+2)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+3)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+4)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+5)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+6)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+7)) = itetclr(icnt)
            imt1a( iteta(nen*(icnt1)+8)) = itetclr(icnt)
          endif

c       SURFCLR materials dependent on input surface colors
c       these will be values 1 or 2 for the background grid
c       and integers larger than 2 for vertical features
ctodo   untested since original versions of code
        elseif (ioptclr .ne. SURFCLR) then

           if (itetclr(icnt).eq.1 .or. itetclr(icnt).eq.2 ) then
             itetclra(icnt) = int( icnt1 / nelm_per_lay ) + 1
           else
             itetclra(icnt) = itetclr(icnt) + nlay
           endif

c       STRATCLR  materials dependent on the layer order
c       imt materials assigned in loop outside this one
c       This will override element materials from the stack cmo
ctodo   untested since old versions of code
        elseif (ioptclr .eq. STRATCLR) then

          itetclra(icnt) = int( icnt1 / nelm_per_lay ) + 1
        endif

      enddo
C     end loop through each new element

C     STRATCLR fill node imt material arrays in seperate loop 
C     unit color defined by bottom layer
C     top layer same as previous layer
      if (ioptclr .eq. STRATCLR) then
        call x3d_error('stratclr ','untested for this version.')
        do isurf = 0, nlay-1
          do i = 1, nnod_per_lay
            ii = i+isurf*nnod_per_lay
            imt1a(ii) = isurf+1
          enddo
          if (isurf .eq. nlay-1) then
          do i = 1, nnod_per_lay
            ii = i+isurf*nnod_per_lay
            imt1a(ii) = isurf
          enddo
          endif
        enddo
      endif
c     end stratclr color method

C     FILL node coordinate arrays and attributes
      do inod = 1, nnodes
        xica(inod) = xic(inod)
        yica(inod) = yic(inod)
        zica(inod) = zic(inod)
      enddo

      call cmo_set_info('nnodes', cmoout,nnodes,1,1,ier)
      call cmo_set_info('nelements',cmoout,nelmout,1,1,ier)
      call cmo_set_mesh_type(cmoout, coutype, ier)
      if(ier .ne. 0)call x3d_error(isubname, 'cmo set mesh_type')

c     geniee will check for topologic errors in elements
      call dotaskx3d('geniee ; finish',ier)

C-----3D hex or prism structure done
c     Deallocate temporary memory for local variables
      call mmrelprt(isubname, ics)

c-------------------------------------------------------------------
C     Convert the new mesh to a tetrahedral mesh
C     For a hex grid, use hextotet 6 (default) or 24
C     For a prism grid, use hextotet 3 (default) or 18
C     hextotet 6 or 24 for prisms makes degenerate hex's and has zero vols
c     This option is not reccommended due to problems removing of 0 vol tets
ctodo May be able to use massage to remove 0 volume tets

      if ( iopttet.gt.0 ) then
        write(cbuf,91) ntet, cmoout(1:icharlnf(cmoout))
 91     format('hextotet/',i10,'/',a32,'/-defp-/ ; finish')
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'hextotet')

        call cmo_exist('-defp-',ierr)
        if (ierr.eq.0) call cmo_release('-defp-',ierr)
        call cmo_exist('-defh-',ierr)
        if (ierr.eq.0) call cmo_release('-defh-',ierr)
      endif

C     Done with new cmo, make it current
      call cmo_select(cmoout,ierr)

      ierr_return = 0
9999  return
      end

       subroutine stack_tet(imsgin,xmsgin,cmsgin,msgtyp,nwds,
     *             ierr_return)
C
C#####################################################################
C
C
C      PURPOSE -
C
C       Input is a stacked layer cmo filled with volume elements
C       of either hex or prism type. This routine is used by the
C       stack routines to convert the filled stack cmo to
C       a tetrahedral mesh.
C       It can be called through stack/fill options or on the
C       command line.
C       By default hextotet is called with 3 for prism elements.
C       By default hextotet is called with 6 for hex elements.
C
C      SYNTAX:
C      STACK/ TET / cmoout/ cmoin / [num_tet] [rmvolume] 
C
C        cmoout    - the output tet cmo
C        cmoin     - the input stacked prism or hex cmo
C
C      INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      CHANGE HISTORY -
C
C      Original version - T.Cherry - jul 2001
C
C#####################################################################
C
      implicit none
C
C####################################################################
C
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds),ierr_return
      character*32 cmsgin(nwds)
C
      integer ntet, imesh_type,iopt_rmvol
      integer i,ii,ierr,ierrw,ics,ier
      integer icharlnf
      character*32 isubname, cmoout, cmoin, mesh_type
      character*132 logmess
      character*1024  cbuf
C
C BEGIN
      isubname = 'stack_tet'
      ierr = 0
      iopt_rmvol = 0
      ntet = 3
      cmoin = '-notset-'
      cmoout = '-notset-'

C     READ PARSER VALUES, i is next value, nwds is last
C     STACK/ TET / cmoout/ cmostack / [num_tet] [rmvolume] 
      i = 3
      if (msgtyp(i).eq.3 ) then
        cmoout = cmsgin(i)
      else
        write(logmess,'(a)')'ERROR: expected output cmo name'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999
      endif
      i= i+1

      if (msgtyp(i).eq.3 ) then
        cmoin = cmsgin(i)
      else
        write(logmess,'(a)')'ERROR: expected input cmo name'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999
      endif
      i= i+1

C     Read optional parameters
      do ii = i, nwds
        if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:5).eq.'rmvol')
     *      iopt_rmvol = 1
        if(msgtyp(ii).eq.1 ) ntet = imsgin(i)
      enddo

C     Check the mesh object name
      call cmo_exist(cmoin,ics)
      if(ics.ne.0) then
         write(logmess,'(a,a)')
     *   'STACK/TET: Not a valid mesh object: ',cmoin
         call writloga('default',1,logmess,1,ierrw)
         goto 9999
      endif

c     Get the mesh type of the incoming cmo
c     Should be quad, tri, or hybrid
      mesh_type = 'notset'
      call cmo_get_mesh_type(cmoin,mesh_type,imesh_type,ier)
      if (ier.ne.0 .or. imesh_type.le.0) then
          write(logmess,'(a,a,a,a)') 'ERROR: Undefined mesh type: ',
     >    mesh_type(1:3),' for ',cmoin
          call writloga('default',0,logmess,1,ics)
          goto 9999
      endif
      if (mesh_type(1:3).ne.'hex' .and. mesh_type(1:3).ne.'pri') then
          write(logmess,'(a,a)')
     >    'ERROR: Unsupported stack mesh type: ',mesh_type
          call writloga('default',0,logmess,1,ics)
          goto 9999
      endif

C If 6 or 24, then degenerate hex's formed, no remove of zero vols
c This option is not reccommended due to removal of 0 vol tet problems
      if (ntet.eq.6 .or. ntet.eq.24) then
        cbuf =' 6 and 24 options temporarily not available.'
        call writloga('default',1,cbuf,1,ierr)
        goto 9999
      endif
      ierr=0

c     set hextotet globals to remove zero volume elements
      if (iopt_rmvol.eq.1) then
        write(cbuf,'(a)') 
     *  'assign / / / hextotet_remove_volume / yes ; finish'
        call dotaskx3d(cbuf,ierr)
        write(cbuf,'(a)') 
     *  'assign / / / hextotet_remove_duplicates / yes ; finish'
        call dotaskx3d(cbuf,ierr)
      endif

C-----convert prism cmoin to tet cmoout
      if (mesh_type(1:3).eq.'pri' ) then

        ntet = 3
        write(cbuf,90) ntet, 
     *  cmoout(1:icharlnf(cmoout)),cmoin(1:icharlnf(cmoin))
 90     format('hextotet/',i10,'/',a32,'/',a32,' ; finish')
        call dotaskx3d(cbuf,ierr)

C-----convert hex cmo cmoin to tet cmoout
      elseif (mesh_type(1:3).ne.'hex' ) then

        ntet = 6
        write(cbuf,91) ntet,
     *  cmoout(1:icharlnf(cmoout)),cmoin(1:icharlnf(cmoin))
 91     format('hextotet/',i10,'/',a32,'/',a32,' ; finish')
        call dotaskx3d(cbuf,ierr)
      endif
      if(ierr.ne.0)call x3d_error(isubname, 'hextotet')

      call cmo_select(cmoout,ierr)


      ierr_return = 0
9999  return
      end
 
