*DK pgg
     	subroutine pgg (imsgin, xmsgin, cmsgin, msgtyp, nwds, ierror)
C #####################################################################
C
C     PURPOSE -
C
C       Originally converted stacked trilayers to prisms,
C       changed to also convert stacked quadlayers to hexs.
C       Future version will allow hybrid mesh
C
c       This routine reads a file that contains the connectivity and
c       the topology of a sequence of surfaces. By assuming that the
c       surfaces have identical connectivity, the code then generates
c       the connectivity of a prismatic grid. The prisms are constructed
c       by joining the elements of consecutive surfaces that have the
c       same logical location on those surfaces.
c       Used with the mread trilayers routine
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
c      data ifelmhex / 8 / 1,4,3,2 5,6,7,8
c
c    token order
c    1 = pgg
c    2 = cmo outgoing
c    3 = cmo incoming
c    4 = (if integer) = number of surfaces
c    4 = (if char "lay") = number of surfaces (default)
c    4 = (if char "nod") = number of points per surface
c    n = (if char "hex") = create hex from trilayers
c    n = (if char "pri") = create prisms from trilayers (default)
c    n = (if char "usrclr") = color elements by user defined surface colors (default)
c    n = (if char "stratclr") = color elements by layer (previously layerclr)
c
c     currently checks command line for mesh type, 
c     this needs to change to check incoming cmo for mesh type
c     before hybrid meshes can be built
c
c     EXAMPLE:
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
c       Modified:
c       vcg - summer/96  - initial code
c       t.cherry -  fall/96   - corrections to itet, itetoffset, mem
C         $Log: pgg.f,v $
C         Revision 2.00  2007/11/09 20:03:58  spchu
C         Import to CVS
C  
CPVCS    
CPVCS       Rev 1.8   Wed Apr 05 13:34:50 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
C
CPVCS
C
c#######################################################################
c
c     FORMAT:
c
c     PGG/ cmoout / cmoin / num_layers
c
c
c     cmo/create/cmotri
c     mread trilayers/avs/ tri1.inp, tri2.inp, tri3.inp / pinch
c     pgg/cmoprism/cmotri/ 3
c     prismtohex/cmohex/cmoprism
c     hextotet/24/cmotet/cmohex
c
c#######################################################################
C
	implicit none
 
        include "local_element.h"
	
	integer nwds, imsgin(nwds), msgtyp(nwds), ierror
	character*32 cmsgin(nwds)
	real*8 xmsgin(nwds)
 
	character*32 cmoin, cmoout, coutype
	integer i,ii, nsur,isurf, ier,nwrite
        integer ierr_tv, ierr_pv, ierr_eq
        integer itoff, jtoff
        integer usrclr, stratclr, volclr
	integer nnod, nelm, ndt, ndg, npe, nfpe, nsur_nodes,
     >		mbndry, lencm, itypcm
 
 	integer  ictype, ielmtype, nelm_per_sur, nelmout
	integer  idelete
	integer  itype, ilen
 
     	integer nnod_per_sur, nen,nena, topo_dim,
     >		geom_dim, nod_per_elm, fac_per_elm, edg_per_elm
	integer icnt, icnt1, jcnt, jcnt_plus, inod
	
        integer nremainder
        integer icharlnf
	character*32 isubname
	character*132 logmess
	character*162 cbuf
	integer icscode, ics
	
c  Definitions for INcoming --> cmo <-- [existing]
 
      	pointer (ipimt1, imt1)  ! node material
	integer   imt1(10000000)
      	pointer (ipitp1, itp1)  ! node type
      	integer   itp1(10000000)
	pointer (ipicr1, icr1)  ! constraint numbers for nodes
      	integer   icr1(10000000)      	
	pointer (ipisn1, isn1)  ! child, parent node correspondance
      	integer   isn1(10000000)
 
c      	pointer (ipitet, itet)        ! node vertices for each element
c	pointer (ipjtet, jtet)        ! element connectivity
c	integer   itet(3,10000000)
c	integer   jtet(3,10000000)
 
C  The 8 is used to ensure that the array can handle any element type
        pointer (ipitet, itet)        ! node vertices for each element
        pointer (ipjtet, jtet)        ! element connectivity
        integer   itet(8*1000000), jtet(8*10000000)
 
	pointer (ipitetoff, itetoff)  ! index into itet-array
	integer   itetoff(10000000)
	pointer (ipjtetoff, jtetoff)  ! index into jtet-array
	integer   jtetoff(10000000)
      	pointer (ipitettyp, itettyp)  ! element geometry
	integer   itettyp(10000000)
      	pointer (ipitetclr, itetclr)  ! element material
	integer   itetclr(10000000)
	pointer (ipxic, xic)    ! node x-coordinates
      	
      	real*8  xic(10000000)
      	pointer (ipyic, yic)    ! node y-coordinates
      	
      	real*8  yic(10000000)
      	pointer (ipzic, zic)    ! node z-coordinates
      	
      	real*8  zic(10000000)
 
 
c   Definitions for OUTgoing --> cmo <-- [to be created]
     	
	pointer (ipimt1a, imt1a)  ! node material
      	pointer (ipxica, xica)    ! node x-coordinates
      	pointer (ipyica, yica)    ! node y-coordinates
      	pointer (ipzica, zica)    ! node z-coordinates
	integer imt1a(10000000)
      	real*8 yica(10000000)
      	real*8 xica(10000000)
      	real*8 zica(10000000)
      	
       	pointer (ipiteta, iteta)        ! node vertices for each element
 	pointer (ipjteta, jteta)        ! element connectivity
c	integer  jteta(6,10000000)
c	integer   iteta(6,10000000)
 	integer   jteta(8*10000000)
 	integer   iteta(8*10000000)
 
      	pointer (ipitetoffa, itetoffa)  ! index into itet array
	integer   itetoffa(10000000)
      	pointer (ipjtetoffa, jtetoffa)  ! index into jtet array
	integer   jtetoffa(10000000)
      	pointer (ipitettypa, itettypa)  ! element geometry
	integer   itettypa(10000000)
      	pointer (ipitetclra, itetclra)  ! element material
	integer   itetclra(10000000)
 
c BEGIN
  	
       isubname = 'pgg'
       usrclr = 1
       stratclr = 0
       volclr = 0
       ielmtype = ifelmpri
 
       nsur_nodes = 0
       nwrite = 5
       ierr_tv = 0
       ierr_pv = 0
       ierr_eq = 0
       ierror = -1
 
c    parse tokens
c    Get the name of INcoming --> cmo <-- [ call it cmoin ]
 
c    token order
c    1 = pgg
c    2 = cmo outgoing
c    3 = cmo incoming
c    4 = (if integer) = number of surfaces
c    4 = (if char "lay") = number of surfaces
c    4 = (if char "nod") = number of points per surface
c    n = (if char "hex") = create hex from trilayers
c    n = (if char "pri") = create prisms from trilayers
c    n = (if char "usrclr") = color elements by original surface colors
c    n = (if char "stratclr") = color elements by layer (used to be layerclr)
 
      cmoout = cmsgin(2)
      cmoin  = cmsgin(3)
      if (msgtyp(4) .eq. 1) nsur = imsgin(4)
      do i = 4, nwds
        if (msgtyp(i) .eq. 3) then
          if (cmsgin(i)(1:3).eq.'hex') ielmtype = ifelmhex
          if (cmsgin(i)(1:3).eq.'pri') ielmtype = ifelmpri
          if (cmsgin(i)(1:5).eq.'strat') stratclr = 1
          if (cmsgin(i)(1:3).eq.'vol') volclr = 1
          if (cmsgin(i)(1:3).eq.'usr') usrclr = 1
          if (cmsgin(i)(1:3).eq.'nod') nsur_nodes = imsgin(i+1)
          if (cmsgin(i)(2:4).eq.'nod') nsur_nodes = imsgin(i+1)
          if (cmsgin(i)(1:3).eq.'lay') nsur = imsgin(i+1)
          if (cmsgin(i)(2:4).eq.'lay') nsur = imsgin(i+1)
        endif
      enddo
      
 
c     Get the INcoming scalar mesh variables
      call cmo_get_info('nnodes',   cmoin, nnod,lencm, itypcm, ier)
      call cmo_get_info('nelements',cmoin, nelm,lencm, itypcm, ier)
      call cmo_get_info('ndimensions_topo',cmoin,ndt,lencm,itypcm,ier)
      call cmo_get_info('ndimensions_geom',cmoin,ndg,lencm,itypcm,ier)
      call cmo_get_info('nodes_per_element',cmoin,npe,lencm,itypcm,ier)
      call cmo_get_info('faces_per_element',cmoin,nfpe,lencm,itypcm,ier)
      call cmo_get_info('mbndry',cmoin, mbndry, lencm, itypcm, ier)
 
c     Get pointers to the INcoming vector mesh variables
      call cmo_get_info('imt1',cmoin,ipimt1,  ilen, ictype,ier)
      call cmo_get_info('itp1',cmoin, ipitp1, ilen, ictype, ier)
      call cmo_get_info('icr1',cmoin, ipicr1, ilen, ictype, ier)
      call cmo_get_info('isn1',cmoin, ipisn1, ilen, ictype, ier)
      call cmo_get_info('xic', cmoin, ipxic,  ilen,  ictype, ier)
      call cmo_get_info('yic', cmoin, ipyic,  ilen,  ictype, ier)
      call cmo_get_info('zic', cmoin, ipzic,  ilen,  ictype, ier)
      call cmo_get_info('itet',cmoin, ipitet, ilen,  ictype, ier)
      call cmo_get_info('jtet',cmoin, ipjtet, ilen,  ictype, ier)
      call cmo_get_info('itetoff',cmoin,ipitetoff,ilen,ictype,ier)
      call cmo_get_info('jtetoff',cmoin,ipjtetoff,ilen,ictype,ier)
      call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,ictype,ier)
      call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,ictype,ier)
 
c     Calculate number of surfaces and nodes per surface
c     Check that we have a reasonable number of surfaces
 
      if (nsur.eq.0 .and. nsur_nodes.ne.0) then
         nsur = nnod/nsur_nodes
      endif
      if (nsur_nodes.eq.0 .and. nsur.ne.0) then
         nsur_nodes = nnod/nsur
      endif
      if (nsur.le.0 .or. nsur_nodes.eq.0) then
          write(logmess,'(a,i5)')
     >    'ERROR: Zero number of layers or nodes: ',nsur,nsur_nodes
          call writloga('default',0,logmess,1,ics)
          return
      endif
 
      nremainder = mod(nnod,nsur)
      if (nremainder.ne.0) then
          write(logmess,'(a,i5)')
     >    'ERROR: INCORRECT number of layers: ',nsur
          call writloga('default',0,logmess,1,ics)
          return
      endif
 
      nnod_per_sur = nnod/nsur
      nelm_per_sur = nelm/nsur
      nelmout = nelm - nelm_per_sur
	
c     Create the name of OUTgoing --> cmo <-- [ call it cmoout ]
c     If  ier .EQ. 0, then the cmo already exists - if so release it

      call cmo_exist (cmoout, ier)
      if (ier .EQ. 0) call cmo_release (cmoout, idelete)

c     Derive new cmo from old
      cbuf = 'cmo/copy/' // cmoout(1:icharlnf(cmoout)) // '/ '
     *       // cmoin(1:icharlnf(cmoin))
     *       // '/ ; finish'
      call dotaskx3d(cbuf,ier)
      if(ier .ne. 0)call x3d_error(isubname, 'cmo copy')
      if (ier.ne.0) goto 999
c     Set active cmo to cmoout
      call cmo_set_name(cmoout,ier) 

c     Set the OUTgoing scalar mesh variables:
c------------------------------------------------------------------------
c	There are --> nnod <-- total nodes
c	There are --> ntri <-- total triangles
c	There are --> nsur <-- total surfaces
c	There are --> nnod_per_sur = nnod/nsur <-- nodes per surface
c	There are --> ntri_per_sur = ntri/nsur <-- triangles per surface
c	There are --> npri = ntri - ntri_per_sur <-- total prisms
c------------------------------------------------------------------------
c
c	---> Triangulation Consistency Conditions <---
c	Suppose we are given a connected polygonal domain having
c	--> Nnod <--   number of nodes,
c	--> Nbdnod <-- number of boundary nodes, and possibly
c	--> Nhol <--   number of holes.
c	Then --> any triangulation <-- of the domain consists of:
c	--> Ntri = 2*[ Nnod + Nhol - 1 ] - Nbdnod <-- number of triangles
c	--> Nedg = 3*[ Nnod + Nhol - 1 ] - Nbdnod <-- number of edges
c
c------------------------------------------------------------------------

c     set the mesh type
c     tri and quad become pri and hex,  hyb added in future
c     need add check to incoming to make sure it is tri or quad
      if (ielmtype .eq. ifelmhex ) then
         coutype = 'hex'
      else
         coutype = 'pri'
      endif
      call cmo_set_mesh_type(cmoout, coutype, ier)
      if(ier .ne. 0)call x3d_error(isubname, 'cmo set mesh_type')
 
      topo_dim = 3
      geom_dim = 3
      nod_per_elm = nelmnen(ielmtype)
      fac_per_elm = nelmnef(ielmtype)
      edg_per_elm = nelmnee(ielmtype)
 
      itoff = 0
      jtoff = 0
 
      call cmo_set_info('nnodes', cmoout, nnod,     1,1,ier)
      call cmo_set_info('nelements',cmoout, nelmout,1,1,ier)
      call cmo_set_info('ndimensions_topo',cmoout,topo_dim,1,1,ier)
      call cmo_set_info('ndimensions_geom',cmoout,geom_dim,1,1,ier)
      call cmo_set_info('nodes_per_element',cmoout,nod_per_elm,1,1,ier)
      call cmo_set_info('faces_per_element',cmoout,fac_per_elm,1,1,ier)
      call cmo_set_info('edges_per_element',cmoout,edg_per_elm,1,1,ier)
      call cmo_newlen(cmoout,ier)
 
c    	Get pointers for the OUTgoing vector variables
C       ATTRIBUTES
C       xica, yica, zica
C       iteta, itetoffa, itettypa, itetclra
C       imta
 
      call cmo_get_info('xic',cmoout,ipxica,ilen,itype, ier)
      call cmo_get_info('yic',cmoout,ipyica,ilen,itype, ier)
      call cmo_get_info('zic',cmoout,ipzica,ilen,itype, ier)
      call cmo_get_info('itet',cmoout,ipiteta,ilen,itype,ier)
      call cmo_get_info('itetoff',cmoout,ipitetoffa,ilen,itype,ier)
      call cmo_get_info('itettyp',cmoout,ipitettypa,ilen,itype,ier)
      call cmo_get_info('itetclr',cmoout,ipitetclra,ilen,itype,ier)
      call cmo_get_info('imt1',cmoout,ipimt1a,ilen,itype,ier)
      if (ier .ne. 0) then
         call x3d_error(isubname,'cmo_get_info')
         call mmverify
      endif
 
      call cmo_get_info('jtet',cmoout,ipjteta,ilen,itype,ier)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffa,ilen,itype,ier)
      if (ier .ne. 0) then
         call x3d_error(isubname,'cmo_get_info for jtet')
         call mmverify
      endif
 
C     FILL connectivity in itet arrays
      nena = nod_per_elm
      nen  = nod_per_elm / 2
      do icnt = 1, nelmout
        icnt1 = icnt- 1
        jcnt = nen*icnt1
        jcnt_plus = jcnt+(nen*nelm_per_sur)
 
C       SET itet array for prisms
        if (ielmtype .eq. ifelmpri) then
          iteta(nena*(icnt1)+1)=itet(jcnt+1)
     	  iteta(nena*(icnt1)+2)=itet(jcnt+2)
          iteta(nena*(icnt1)+3)=itet(jcnt+3)
     	  iteta(nena*(icnt1)+4)=itet(jcnt_plus+1)
     	  iteta(nena*(icnt1)+5)=itet(jcnt_plus+2)
          iteta(nena*(icnt1)+6)=itet(jcnt_plus+3)
 
C       SET itet array for hex
        elseif (ielmtype .eq. ifelmhex) then
          iteta(nena*(icnt1)+1)=itet(jcnt+1)
          iteta(nena*(icnt1)+2)=itet(jcnt+2)
          iteta(nena*(icnt1)+3)=itet(jcnt+3)
          iteta(nena*(icnt1)+4)=itet(jcnt+4)
          iteta(nena*(icnt1)+5)=itet(jcnt_plus+1)
          iteta(nena*(icnt1)+6)=itet(jcnt_plus+2)
          iteta(nena*(icnt1)+7)=itet(jcnt_plus+3)
          iteta(nena*(icnt1)+8)=itet(jcnt_plus+4)
        endif
 
        itettypa(icnt) = ielmtype
        itetoffa(icnt) = itoff
        itoff          = itoff + nod_per_elm
        jtetoffa(icnt) = jtoff
        jtoff          = jtoff + fac_per_elm
 
 
c       SET tet colors dependent on layer order
        if (stratclr .ne. 0) then
          itetclra(icnt) = int( icnt1 / nelm_per_sur ) + 1
 
c       SET tet colors as what user set
        elseif (usrclr .ne. 0) then
          itetclra(icnt) = itetclr(icnt)
 
          if (ielmtype .eq. ifelmpri) then
            imt1a( iteta(nena*(icnt1)+1)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+2)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+3)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+4)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+5)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+6)) = itetclr(icnt)
          elseif (ielmtype .eq. ifelmhex) then
            imt1a( iteta(nena*(icnt1)+1)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+2)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+3)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+4)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+5)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+6)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+7)) = itetclr(icnt)
            imt1a( iteta(nena*(icnt1)+8)) = itetclr(icnt)
          endif
 
c      SET tet colors dependent on old tet colors
       else
          if (itetclr(icnt).eq.1 .or. itetclr(icnt).eq.2 ) then
 	    itetclra(icnt) = int( icnt1 / nelm_per_sur ) + 1
          else
            itetclra(icnt) = itetclr(icnt) + nsur
          endif
        endif
      enddo
 
C     FILL node coordinate arrays
      do inod = 1, nnod
        xica(inod) = xic(inod)
        yica(inod) = yic(inod)
        zica(inod) = zic(inod)
      enddo
 
C     FILL node material arrays
C     unit color defined by bottom layer
C     top layer same as previous layer
 
      if (stratclr .ne. 0) then
        call x3d_error('stratclr ','untested for this version.')
        do isurf = 0, nsur-1
          do i = 1, nnod_per_sur
            ii = i+isurf*nnod_per_sur
            imt1a(ii) = isurf+1
          enddo
          if (isurf .eq. nsur-1) then
          do i = 1, nnod_per_sur
            ii = i+isurf*nnod_per_sur
            imt1a(ii) = isurf
          enddo
          endif
        enddo
      endif
      call cmo_set_info('nnodes', cmoout,nnod,1,1,ier)
      call cmo_set_info('nelements',cmoout,nelmout,1,1,ier)

c     geniee will check for topologic errors in elements
c     call dotaskx3d('dump/gmv/hexraw.gmv/ ; finish',ier )
      call dotaskx3d('geniee ; finish',ier)
 
C-----3D hex or prism structure done
c     Deallocate temporary memory for local variables
      call mmrelprt(isubname, icscode)
      ierror = 0
 
 999  return
      end


 
c----------------------------------------------------------------------
	subroutine cv_hex ( x, y, z, cx, cy, cz, area, vol, dist )
 
c
c       cv_hex should go into hextotet_hybrid
c	Computes the Area-Weighted   Centroids of the Faces
c	Computes the Volume-Weighted Centroid  of the Hexahedron
c	Computes the Volume of the Hexahedron
c----------------------------------------------------------------------	
 
ctodo change this to implicit none
	implicit real*8 ( a - h, o - z )
	dimension x(1:8), y(1:8), z(1:8), cx(1:7), cy(1:7), cz(1:7)
 
 
        call get_epsilon('epsilon',epsilon)
c	Compute Area-Weighted Centroid and Area of Face 1->2->3->4 :
	call ca_quad ( x(1), y(1), z(1), x(2), y(2), z(2),
     >		       x(3), y(3), z(3), x(4), y(4), z(4),
     >		       cx(1), cy(1), cz(1), a1234 )
 
 
c	Compute Area-Weighted Centroid and Area of Face 6->5->8->7 :
	call ca_quad ( x(6), y(6), z(6), x(5), y(5), z(5),
     >		       x(8), y(8), z(8), x(7), y(7), z(7),
     >		       cx(2), cy(2), cz(2), a6587 )
 
 
c	Compute Area-Weighted Centroid and Area of Face 2->1->5->6 :
	call ca_quad ( x(2), y(2), z(2), x(1), y(1), z(1),
     >		       x(5), y(5), z(5), x(6), y(6), z(6),
     >		       cx(3), cy(3), cz(3), a2156 )
 
 
c	Compute Area-Weighted Centroid and Area of Face 3->2->6->7 :
	call ca_quad ( x(3), y(3), z(3), x(2), y(2), z(2),
     >		       x(6), y(6), z(6), x(7), y(7), z(7),
     >		       cx(4), cy(4), cz(4), a3267 )
 
c	Compute Area-Weighted Centroid and Area of Face 4->3->7->8 :
	call ca_quad ( x(4), y(4), z(4), x(3), y(3), z(3),
     >		       x(7), y(7), z(7), x(8), y(8), z(8),
     >		       cx(5), cy(5), cz(5), a4378 )
 
c	Compute Area-Weighted Centroid and Area of Face 1->4->8->5 :
	call ca_quad ( x(1), y(1), z(1), x(4), y(4), z(4),
     >		       x(8), y(8), z(8), x(5), y(5), z(5),
     >		       cx(6), cy(6), cz(6), a1485 )
 
 
c	Compute Area-Weighted Temporary Centroid of the Hexahedron :
     	area =   a1234 + a6587 + a2156 + a3267 + a4378 + a1485
     	
     	if ( area .GT. epsilon ) then
     	  tcx =   cx(1) * a1234 + cx(2) * a6587 + cx(3) * a2156
     >	        + cx(4) * a3267 + cx(5) * a4378 + cx(6) * a1485
     	  tcx =   tcx / area
     	
      	  tcy =   cy(1) * a1234 + cy(2) * a6587 + cy(3) * a2156
     >	        + cy(4) * a3267 + cy(5) * a4378 + cy(6) * a1485
     	  tcy =   tcy / area
     	    	
     	  tcz =   cz(1) * a1234 + cz(2) * a6587 + cz(3) * a2156
     >	        + cz(4) * a3267 + cz(5) * a4378 + cz(6) * a1485
     	  tcz =   tcz / area     	
     	else     	
     	  tcx = cx(1) + cx(2) + cx(3) + cx(4) + cx(5) + cx(6)
     	  tcx =   tcx / 6.0d+00
     	  tcy = cy(1) + cy(2) + cy(3) + cy(4) + cy(5) + cy(6)
     	  tcy =   tcy / 6.0d+00
     	  tcz = cz(1) + cz(2) + cz(3) + cz(4) + cz(5) + cz(6)
     	  tcz =   tcz / 6.0d+00
     	endif
     	
     	       	
c	Compute Centroids and the Volumes of the 24 Tetrahedra
c 	Face 1->2->3->4 :
	 call cv_tet(  x(1),  y(1),  z(1),  x(2),  y(2),  z(2),
     >		      cx(1), cy(1), cz(1),  tcx,   tcy,   tcz,
     >		      cx112,  cy112,  cz112, v112 )
         	
	 call cv_tet(  x(2),  y(2),  z(2),  x(3),  y(3),  z(3),
     >		      cx(1), cy(1), cz(1),  tcx,   tcy,   tcz,
     >		      cx123,  cy123,  cz123, v123 )
 
	 call cv_tet(  x(3),  y(3),  z(3),  x(4),  y(4),  z(4),
     >		      cx(1), cy(1), cz(1),  tcx,   tcy,   tcz,
     >		      cx134,  cy134,  cz134, v134 )         	
 
	 call cv_tet(  x(4),  y(4),  z(4),  x(1),  y(1),  z(1),
     >		      cx(1), cy(1), cz(1),  tcx,   tcy,   tcz,
     >		      cx141,  cy141,  cz141, v141 )     	
     	
c 	Face 6->5->8->7 :
	 call cv_tet(  x(6),  y(6),  z(6),  x(5),  y(5),  z(5),
     >		      cx(2), cy(2), cz(2),  tcx,   tcy,   tcz,
     >		      cx265,  cy265,  cz265, v265 )
          	     	
 	 call cv_tet(  x(5),  y(5),  z(5),  x(8),  y(8),  z(8),
     >		      cx(2), cy(2), cz(2),  tcx,   tcy,   tcz,
     >		      cx258,  cy258,  cz258, v258 )
     	     	
	 call cv_tet(  x(8),  y(8),  z(8),  x(7),  y(7),  z(7),
     >		      cx(2), cy(2), cz(2),  tcx,   tcy,   tcz,
     >		      cx287,  cy287,  cz287, v287 )
                       	
	 call cv_tet(  x(7),  y(7),  z(7),  x(6),  y(6),  z(6),
     >		      cx(2), cy(2), cz(2),  tcx,   tcy,   tcz,
     >		      cx276,  cy276,  cz276, v276 )     	     	     	
     	
c 	Face 2->1->5->6 :
	 call cv_tet(  x(2),  y(2),  z(2),  x(1),  y(1),  z(1),
     >		      cx(3), cy(3), cz(3),  tcx,   tcy,   tcz,
     >		      cx321,  cy321,  cz321, v321 )
          	     	
	 call cv_tet(  x(1),  y(1),  z(1),  x(5),  y(5),  z(5),
     >		      cx(3), cy(3), cz(3),  tcx,   tcy,   tcz,
     >		      cx315,  cy315,  cz315, v315 )        	
          	     	
	 call cv_tet(  x(5),  y(5),  z(5),  x(6),  y(6),  z(6),
     >		      cx(3), cy(3), cz(3),  tcx,   tcy,   tcz,
     >		      cx356,  cy356,  cz356, v356 )    	
          	     	
	 call cv_tet(  x(6),  y(6),  z(6),  x(2),  y(2),  z(2),
     >		      cx(3), cy(3), cz(3),  tcx,   tcy,   tcz,
     >		      cx362,  cy362,  cz362, v362 )    	     	
     	     	
c 	Face 3->2->6->7 :
	 call cv_tet(  x(3),  y(3),  z(3),  x(2),  y(2),  z(2),
     >		      cx(4), cy(4), cz(4),  tcx,   tcy,   tcz,
     >		      cx432,  cy432,  cz432, v432 )    	
 
	 call cv_tet(  x(2),  y(2),  z(2),  x(6),  y(6),  z(6),
     >		      cx(4), cy(4), cz(4),  tcx,   tcy,   tcz,
     >		      cx426,  cy426,  cz426, v426 )
 
	 call cv_tet(  x(6),  y(6),  z(6),  x(7),  y(7),  z(7),
     >		      cx(4), cy(4), cz(4),  tcx,   tcy,   tcz,
     >		      cx467,  cy467,  cz467, v467 )
 
	 call cv_tet(  x(7),  y(7),  z(7),  x(3),  y(3),  z(3),
     >		      cx(4), cy(4), cz(4),  tcx,   tcy,   tcz,
     >		      cx473,  cy473,  cz473, v473 )
 
c  	Face 4->3->7->8 :
	 call cv_tet(  x(4),  y(4),  z(4),  x(3),  y(3),  z(3),
     >		      cx(5), cy(5), cz(5),  tcx,   tcy,   tcz,
     >		      cx543,  cy543,  cz543, v543 )
 
	 call cv_tet(  x(3),  y(3),  z(3),  x(7),  y(7),  z(7),
     >		      cx(5), cy(5), cz(5),  tcx,   tcy,   tcz,
     >		      cx537,  cy537,  cz537, v537 )
 
	 call cv_tet(  x(7),  y(7),  z(7),  x(8),  y(8),  z(8),
     >		      cx(5), cy(5), cz(5),  tcx,   tcy,   tcz,
     >		      cx578,  cy578,  cz578, v578 )
 
	 call cv_tet(  x(8),  y(8),  z(8),  x(4),  y(4),  z(4),
     >		      cx(5), cy(5), cz(5),  tcx,   tcy,   tcz,
     >		      cx584,  cy584,  cz584, v584 )
 
 
c  	Face 1->4->8->5 :
	 call cv_tet(  x(1),  y(1),  z(1),  x(4),  y(4),  z(4),
     >		      cx(6), cy(6), cz(6),  tcx,   tcy,   tcz,
     >		      cx614,  cy614,  cz614, v614 )
 
	 call cv_tet(  x(4),  y(4),  z(4),  x(8),  y(8),  z(8),
     >		      cx(6), cy(6), cz(6),  tcx,   tcy,   tcz,
     >		      cx648,  cy648,  cz648, v648 )
 
	 call cv_tet(  x(8),  y(8),  z(8),  x(5),  y(5),  z(5),
     >		      cx(6), cy(6), cz(6),  tcx,   tcy,   tcz,
     >		      cx685,  cy685,  cz685, v685 )
 
	 call cv_tet(  x(5),  y(5),  z(5),  x(1),  y(1),  z(1),
     >		      cx(6), cy(6), cz(6),  tcx,   tcy,   tcz,
     >		      cx651,  cy651,  cz651, v651 )
 
 
c	Compute Volume-Weighted Centroid of the Hexahedron
     	
     	vol =   v112 + v123 + v134 + v141
     >	      + v265 + v258 + v287 + v276
     >	      + v321 + v315 + v356 + v362
     >        + v432 + v426 + v467 + v473
     >	      + v543 + v537 + v578 + v584
     >	      + v614 + v648 + v685 + v651
 
     	if ( vol .GT. epsilon ) then     		  	     	
     	  cx(7) =  v112*cx112 + v123*cx123  + v134*cx134  + v141*cx141
     >	         + v265*cx265 + v258*cx258  + v287*cx287  + v276*cx276
     >	         + v321*cx321 + v315*cx315  + v356*cx356  + v362*cx362
     >           + v432*cx432 + v426*cx426  + v467*cx467  + v473*cx473
     >	         + v543*cx543 + v537*cx537  + v578*cx578  + v584*cx584
     >	         + v614*cx614 + v648*cx648  + v685*cx685  + v651*cx651      	
     	  cx(7) =  cx(7) /vol
     	  	     	
     	  cy(7) =  v112*cy112 + v123*cy123  + v134*cy134  + v141*cy141
     >	         + v265*cy265 + v258*cy258  + v287*cy287  + v276*cy276
     >	         + v321*cy321 + v315*cy315  + v356*cy356  + v362*cy362
     >           + v432*cy432 + v426*cy426  + v467*cy467  + v473*cy473
     >	         + v543*cy543 + v537*cy537  + v578*cy578  + v584*cy584
     >	         + v614*cy614 + v648*cy648  + v685*cy685  + v651*cy651      	
     	  cy(7) =  cy(7) /vol
     	     	
	  cz(7) =  v112*cz112 + v123*cz123  + v134*cz134  + v141*cz141
     >	         + v265*cz265 + v258*cz258  + v287*cz287  + v276*cz276
     >	         + v321*cz321 + v315*cz315  + v356*cz356  + v362*cz362
     >           + v432*cz432 + v426*cz426  + v467*cz467  + v473*cz473
     >	         + v543*cz543 + v537*cz537  + v578*cz578  + v584*cz584
     >	         + v614*cz614 + v648*cz648  + v685*cz685  + v651*cz651
     	  cz(7) =  cz(7) / vol     	
     	else
     	  cx(7) = tcx
     	  cy(7) = tcy
     	  cz(7) = tcz
     	endif
 
	
c	Compute the distance between (tcx, tcy, tcz)
c                               and  (cx(7), cy(7), cz(7))
	dist = (tcx - cx(7))**2 + (tcy - cy(7))**2 + (tcz - cz(7))**2
	dist = dsqrt( dist )
 
 
      	
     	return
	end
 
c-----------------------------------------------------------------------
	subroutine ca_quad ( x1, y1, z1, x2, y2, z2,
     >			     x3, y3, z3, x4, y4, z4,
     >			     cx, cy, cz, area )
c-----------------------------------------------------------------------	
	implicit real*8 ( a - h, o - z )
 
        call get_epsilon('epsilon',epsilon)
c	Compute Centroid and Area of Triangle 1->2->3 :
	cx123 = ( x1 + x2 + x3 ) / 3.0d+00
	cy123 = ( y1 + y2 + y3 ) / 3.0d+00
	cz123 = ( z1 + z2 + z3 ) / 3.0d+00
 
	x23 = x3 - x2
	y23 = y3 - y2
	z23 = z3 - z2
	x21 = x1 - x2
	y21 = y1 - y2
	z21 = z1 - z2
	xn2 = y23 * z21 - z23 * y21
	yn2 = z23 * x21 - x23 * z21
	zn2 = x23 * y21 - y23 * x21
	area123 = xn2 * xn2 + yn2 * yn2 + zn2 * zn2
	area123 = 0.5d+00 * dsqrt( area123 )
	
		
c	Compute the Centroid and Area of Triangle 3->4->1 :
	cx341 = ( x3 + x4 + x1 ) / 3.0d+00
	cy341 = ( y3 + y4 + y1 ) / 3.0d+00
	cz341 = ( z3 + z4 + z1 ) / 3.0d+00
 
	x41 = x1 - x4
	y41 = y1 - y4
	z41 = z1 - z4
	x43 = x3 - x4
	y43 = y3 - y4
	z43 = z3 - z4
	xn4 = y41 * z43 - z41 * y43
	yn4 = z41 * x43 - x41 * z43
	zn4 = x41 * y43 - y41 * x43
	area341 = xn4 * xn4 + yn4 * yn4 + zn4 * zn4
	area341 = 0.5d+00 * dsqrt( area341 )
	
	
c	Compute Area-Weighted Centroid of Quadrilateral 1->2->3->4 :
	area = area123 + area341
	
	 if ( area .GT. epsilon ) then
	   cx = ( area123 * cx123 + area341 * cx341 ) / area
	   cy = ( area123 * cy123 + area341 * cy341 ) / area
	   cz = ( area123 * cz123 + area341 * cz341 ) / area
	 else
	   cx =  ( x1 + x2 + x3 + x4 ) / 4.0d+00
	   cy =  ( y1 + y2 + y3 + y4 ) / 4.0d+00
	   cz =  ( z1 + z2 + z3 + z4 ) / 4.0d+00
	endif
		
	return
	end
c----------------------------------------------------------------------
	subroutine cv_tet ( x1, y1, z1, x2, y2, z2,
     >			    x3, y3, z3, x4, y4, z4,
     >			    cx, cy, cz, vol )
c----------------------------------------------------------------------	
 
	implicit real*8 ( a - h, o - z )
 
c	Compute the Centroid of Tetrahedron 1->2->3->4 :
	cx =  ( x1 + x2 + x3 + x4 ) / 4.0d+00
	cy =  ( y1 + y2 + y3 + y4 ) / 4.0d+00
	cz =  ( z1 + z2 + z3 + z4 ) / 4.0d+00
	
	
c	Compute the Volume   of Tetrahedron 1->2->3->4 :
	x14 = x1 - x4
	y14 = y1 - y4
	z14 = z1 - z4
	
	x24 = x2 - x4
	y24 = y2 - y4
	z24 = z2 - z4
	
	x34 = x3 - x4
	y34 = y3 - y4
	z34 = z3 - z4
		
	vol =   x14 * ( y24 * z34 - z24 * y34 )
     >        + y14 * ( z24 * x34 - x24 * z34 )
     >        + z14 * ( x24 * y34 - y24 * x34 )
 
     	vol = dabs( vol ) / 6.0d+00
		
	return
	end
 
C----------------------------------------------------------------------
C######################################################################
C NAME:
       subroutine prismtohex(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
 
c       Modified:
c       k.bower, 10/22/96  - initial code
c       t.cherry, 11/14/96 - itetoff changed to start with 0
c                 1/4/97 - update cmo info at end
c
c
c       The command is 'prism2hex/cmo_hex/cmo_prism/'
c
c       Purpose:
c       This routine uses a prism cmo and converts to hexahedrons
c       with two nodes at each end in identical places.
c       The purpose of this is two make
c       a starting point for a command prism to tet.
c
c       The code for hex2tet is already developed.
c
c       Input Arguments:
c         cmsgin - character array of tokens returned by parser
c         imsgin - integer   array of tokens returned by parser
c         msgtyp - integer   array of tokens returned by parser
c         xmsgin - real      array of tokens returned by parser
c         nwds   - number          of tokens returned by parser
c
c       Output Arguments:
c         ierror - (=0 for successful completion), (=-1 otherwise)
 
c-----------------------------------------------------------------------
 
        implicit none
        include "chydro.h"
        include "local_element.h"
 
 
        integer nwds, imsgin(nwds), msgtyp(nwds), ierror
        integer itoff, jtoff
        character*32 cmsgin(nwds)
        real*8 xmsgin(nwds)
 
        character*32 cmo, cmoin, cmoout
        integer  ier,ics
        integer  ierr_hv,ierr_eq
        integer  i, ii
        integer ipointi, ipointj
        integer inod,nnod,npri,ndt,ndg, npe, nfpe,
     >          mbndry, lencm, itypcm
 
        integer  ictype
        integer  lenicr1, lenisn1,
     >           lenitet, lenjtet, lenitetoff, lenjtetoff,
     >           lenitettyp, lenitetclr,
     >           lenxic,lenyic,lenzic,len,itype
 
        integer  idelete
        integer  lenimt1a, lenicr1a, lenisn1a,
     >           leniteta,  lenjteta, lenoffa, ilen,
     >           lentypa, lenclra,lenxica,lenyica,lenzica
        integer  icmotype
        integer topo_dim, edg_per_elm,
     >          geom_dim, nod_per_elm, fac_per_elm
        integer icnt, icnt1
 
        real*8 xvolume
        real*8 xicvol(100), yicvol(100), zicvol(100)
 
        character*8 cglobal, cdefault
        character*32 isubname
        character*132 logmess
c       character*132 ccommand
c  Definitions for INcoming --> cmo <-- [existing]
 
        pointer (ipimt1, imt1)  ! node material
        integer   imt1(10000000)
        pointer (ipitp1, itp1)  ! node type
        integer  itp1(10000000)
        pointer (ipicr1, icr1)  ! constraint numbers for nodes
        integer  icr1(10000000)
        pointer (ipisn1, isn1)  ! child, parent node correspondance
        integer  isn1(10000000)
        pointer (ipitet, itet)        ! node vertices for each element
        integer   itet(6,10000000)
        pointer (ipjtet, jtet)        ! element connectivity
        integer  jtet(5,10000000)
        pointer (ipitetoff, itetoff)  ! index into itet-array
        integer  itetoff(10000000)
        pointer (ipjtetoff, jtetoff)  ! index into jtet-array
        integer   jtetoff(10000000)
        pointer (ipitettyp, itettyp)  ! element geometry
        integer   itettyp(10000000)
        pointer (ipitetclr, itetclr)  ! element material
        integer  itetclr(10000000)
        pointer (ipxic, xic)    ! node x-coordinates
 
        real*8  xic(10000000)
        pointer (ipyic, yic)    ! node y-coordinates
 
        real*8  yic(10000000)
        pointer (ipzic, zic)    ! node z-coordinates
 
        real*8  zic(10000000)
 
 
 
c   Definitions for OUTgoing --> cmo <-- [to be created]
 
 
        pointer (ipimt1a, imt1a)  ! node material
        integer   imt1a(10000000)
        pointer (ipitp1a, itp1a)  ! node type
        integer   itp1a(10000000)
        pointer (ipicr1a, icr1a)  ! constraint numbers for nodes
        integer   icr1a(10000000)
        pointer (ipisn1a, isn1a)  ! child, parent node correspondance
        integer   isn1a(10000000)
        pointer (ipxica, xica)    ! node x-coordinates
        real*8 xica(10000000)
        pointer (ipyica, yica)    ! node y-coordinates
        real*8 yica(10000000)
        pointer (ipzica, zica)    ! node z-coordinates
        real*8 zica(10000000)
        pointer(ipiteta,iteta) !node vertices for each element
        integer   iteta(8,10000000)
        pointer (ipjteta, jteta)        ! element connectivity
        integer   jteta(6,10000000)
        pointer (ipitetoffa, itetoffa)  ! index into itet array
        integer   itetoffa(10000000)
        pointer (ipjtetoffa, jtetoffa)  ! index into jtet array
        integer   jtetoffa(10000000)
        pointer (ipitettypa, itettypa)  ! element geometry
        integer   itettypa(10000000)
        pointer (ipitetclra, itetclra)  ! element material
        integer  itetclra(10000000)
 
 
c       Definitions for local arrays
 
        isubname = 'prism2hex'
        cglobal='global'
        cdefault='default'
        ierror = -1
 
 
c       Get the name of INcoming --> cmo <-- [ call it cmoin ]
c       cmoout is the created hex cmo
c
        cmoout = cmsgin(2)
        cmoin  = cmsgin(3)
        call cmo_get_name(cmoin, ier)
 
 
c       Get the INcoming scalar mesh variables
 
        call cmo_get_info
     >      ('nnodes',            cmoin, nnod,   lencm, itypcm, ier)
        call cmo_get_info
     >      ('nelements',         cmoin, npri,   lencm, itypcm, ier)
        call cmo_get_info
     >      ('ndimensions_topo',  cmoin, ndt,    lencm, itypcm, ier)
        call cmo_get_info
     >      ('ndimensions_geom',  cmoin, ndg,    lencm, itypcm, ier)
        call cmo_get_info
     >      ('nodes_per_element', cmoin, npe,    lencm, itypcm, ier)
        call cmo_get_info
     >      ('faces_per_element', cmoin, nfpe,   lencm, itypcm, ier)
        call cmo_get_info
     >      ('mbndry',            cmoin, mbndry, lencm, itypcm, ier)
 
 
c       Get pointers to the INcoming vector mesh variables
 
        call cmo_get_info
     >      ('imt1',    cmoin, ipimt1, ilen, ictype, ier)
        call cmo_get_info
     >      ('itp1',    cmoin, ipitp1, ilen, ictype, ier)
        call cmo_get_info
     >      ('icr1',    cmoin, ipicr1, lenicr1, ictype, ier)
        call cmo_get_info
     >      ('isn1',    cmoin, ipisn1, lenisn1, ictype, ier)
        call cmo_get_info
     >      ('xic',     cmoin, ipxic,  lenxic,  ictype, ier)
        call cmo_get_info
     >      ('yic',     cmoin, ipyic,  lenyic,  ictype, ier)
        call cmo_get_info
     >      ('zic',     cmoin, ipzic,  lenzic,  ictype, ier)
        call cmo_get_info
     >      ('itet',    cmoin, ipitet, lenitet, ictype, ier)
        call cmo_get_info
     >      ('jtet',    cmoin, ipjtet, lenjtet, icmotype, ier)
        call cmo_get_info
     >      ('itetoff', cmoin, ipitetoff, lenitetoff, ictype, ier)
        call cmo_get_info
     >      ('jtetoff', cmoin, ipjtetoff, lenjtetoff, ictype, ier)
        call cmo_get_info
     >      ('itettyp', cmoin, ipitettyp, lenitettyp, ictype, ier)
        call cmo_get_info
     >      ('itetclr', cmoin, ipitetclr, lenitetclr, ictype, ier)
 
 
c      Create the name of OUTgoing --> cmo <-- [ call it cmoout ]
 
        call cmo_exist (cmoout, ier)
c       If  ier .EQ. 0, then the cmo already exists - if so release it
        if (ier .EQ. 0) call cmo_release (cmoout, idelete)
 
c       Set active cmo to cmoout
        call cmo_set_name(cmoout, ier)
 
 
c       Set the OUTgoing scalar mesh variables:
c-------------------------------------------------------------------
c       There are --> nnod <-- total prism nodes
c       There are --> npri <-- total prisms
c       There are --> nsur <-- total surfaces
c
c--------------------------------------------------------------------
c
 
        topo_dim = 3
        geom_dim = 3
        nod_per_elm = 8
        fac_per_elm = 6
        edg_per_elm = 12
 
 
        call cmo_set_info
     >      ('nnodes',            cmoout, nnod,len,itype,ier)
        call cmo_set_info
     >      ('nelements',         cmoout, npri,len,itype,ier)
        call cmo_set_info
     >      ('ndimensions_topo',  cmoout,topo_dim,len,itype,ier)
        call cmo_set_info
     >      ('ndimensions_geom',  cmoout,geom_dim,len,itype,ier)
        call cmo_set_info
     >      ('nodes_per_element', cmoout,nod_per_elm,len,itype,ier)
        call cmo_set_info
     >      ('faces_per_element', cmoout,fac_per_elm,len,itype,ier)
 
 
 
c       Allocate memory for vector variables
 
        call cmo_newlen(cmoout,ier)
 
c       Get pointers for the OUTgoing vector variables
 
        call cmo_get_info
     >      ('imt1',    cmoout, ipimt1a, lenimt1a, icmotype, ier)
        call cmo_get_info
     >      ('itp1',    cmoout, ipitp1a, ilen, icmotype, ier)
        call cmo_get_info
     >      ('icr1',    cmoout, ipicr1a, lenicr1a, icmotype, ier)
        call cmo_get_info
     >      ('isn1',    cmoout, ipisn1a, lenisn1a, icmotype, ier)
        call cmo_get_info
     >      ('xic',     cmoout, ipxica,  lenxica,  icmotype, ier)
        call cmo_get_info
     >      ('yic',     cmoout, ipyica,  lenyica,  icmotype, ier)
        call cmo_get_info
     >      ('zic',     cmoout, ipzica,  lenzica,  icmotype, ier)
        call cmo_get_info
     >      ('itet',    cmoout, ipiteta, leniteta, icmotype, ier)
        call cmo_get_info
     >      ('jtet',    cmoout, ipjteta, lenjteta, icmotype, ier)
        call cmo_get_info
     >      ('itetoff', cmoout, ipitetoffa, lenoffa, icmotype, ier)
        call cmo_get_info
     >      ('jtetoff', cmoout, ipjtetoffa, lenoffa, icmotype, ier)
        call cmo_get_info
     >      ('itettyp', cmoout, ipitettypa, lentypa, icmotype, ier)
        call cmo_get_info
     >      ('itetclr', cmoout, ipitetclra, lenclra, icmotype, ier)
 
c       Get the values for the vector components of the 3d mesh
 
c       npri is the total number of prisms
 
        itoff = 0
        jtoff = 0
        do icnt = 1, npri
          icnt1 = icnt  - 1
 
c         FILL connectivity for the hex elements
          iteta(1,icnt)=itet(1, icnt)
          iteta(2,icnt)=itet(2, icnt)
          iteta(3,icnt)=itet(2, icnt)
          iteta(4,icnt)=itet(3, icnt)
          iteta(5,icnt)=itet(4, icnt)
          iteta(6,icnt)=itet(5, icnt)
          iteta(7,icnt)=itet(5, icnt)
          iteta(8,icnt)=itet(6, icnt)
 
c         FILL offsets for jtet and itet arrays
          itetoffa(icnt) = itoff
          jtetoffa(icnt) = jtoff
          itoff = itoff + 8
          jtoff = jtoff + 6
 
c         ASSIGN  element type to hex, color to current unit
          itettypa(icnt) = 8
          itetclra(icnt)=itetclr(icnt)
 
        enddo
 
 
c       FILL node coord, material, itp,icr,isn arrays
c       note the number of nodes remain same as for prisms
        do inod=1,nnod
          xica(inod)=xic(inod)
          yica(inod)=yic(inod)
          zica(inod)=zic(inod)
          imt1a(inod)=imt1(inod)
          itp1a(inod)=itp1(inod)
          icr1a(inod)=icr1(inod)
          isn1a(inod)=isn1(inod)
        enddo
 
c Check hex volumes
        ierr_hv = 0
        ierr_eq = 0
        do icnt = 1, npri
           do i = 1, 8
             ii = iteta(i,icnt)
             xicvol(i) = xica(ii)
             yicvol(i) = yica(ii)
             zicvol(i) = zica(ii)
           enddo
           call volume_element( 8,
     *                         xicvol,yicvol,zicvol,
     *                         xvolume)
 
           if ( xvolume .lt. 0.0 ) then
             if (ierr_hv .lt. 20) then
              write(logmess,9010) icnt,xvolume
 9010         format('WARNING: Hex Volume ',i10,' is ',1pe15.7)
              call writloga('default',0,logmess,1,ics)
             endif
             ierr_hv = ierr_hv+1
 
           elseif ( xvolume .eq. 0.0 ) then
             if (ierr_eq .lt. 20) then
               write(logmess,9011) icnt,xvolume
 9011          format('WARNING: Hex Volume ',i10,' is ',1pe15.7)
               call writloga('default',0,logmess,1,ics)
             endif
             ierr_eq = ierr_eq+1
 
           endif
        enddo
 
c update cmo info
            cmo=cmoout
            call cmo_set_info('nnodes',cmo,nnod,1,1,ier)
            call cmo_set_info('nelements',cmo,npri,1,1,ier)
 
            ipointi=1
            ipointj=nnod
C
            call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                      ipointi,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
            call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                      ipointj,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
 
 
        if (ierr_hv.ne.0) then
            write(logmess,9020) ierr_hv
 9020       format('Total Negative Hex Volumes: ',i10)
            call writloga('default',0,logmess,1,ics)
        endif
        if (ierr_eq.ne.0) then
            write(logmess,9030) ierr_eq
 9030       format('Total Zero Hex Volumes: ',i10)
            call writloga('default',0,logmess,1,ics)
        endif
 
        ierror=0
        return
        end
 
 
