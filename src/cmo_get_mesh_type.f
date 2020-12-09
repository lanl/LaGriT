      subroutine cmo_get_mesh_type(cmo_name, mesh_type, imesh_type,
     *            ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine gets the Mesh Object Type using nen, nef, nee.
C     The subroutine mirrors the code in cmo_mesh_type() which sets
C     nen, nef, nee correctly for each element type.
C
C
C     INPUT ARGUMENTS -
C
C        cmo_name   - (character) Mesh Object Name.
C
C     OUTPUT ARGUMENTS -
C
C        mesh_type  - (character) Mesh Type.
C        imesh_type - (integer)   Mesh Type.
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C
C        $Log: cmo_get_mesh_type.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'local_element.h'
C
C#######################################################################
C
      character*(*) cmo_name, mesh_type
C
      integer imesh_type, ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ilen, ityp, ierr, nelements
      integer nsdg, nsdt, nen, nef, nee, ilen_typ, ielem_typ
      pointer (ipitettyp, itettyp(*))
      integer itettyp
C
      character*32 isubname, elem_typ
      character*132 logmess
C
C#######################################################################
C
C
C
      isubname = 'cmo_get_mesh_type'
      mesh_type = 'notset'
      imesh_type = -1
      ilen_typ = 0
      ielem_typ = 0
 
      call cmo_get_info('ndimensions_geom',cmo_name,nsdg,ilen,ityp,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info ndimensions_geom')
      call cmo_get_info('ndimensions_topo',cmo_name,nsdt,ilen,ityp,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info ndimensions_topo')
      call cmo_get_info('nodes_per_element',cmo_name,nen,ilen,ityp,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info nodes_per_element')
      call cmo_get_info('faces_per_element',cmo_name,nef,ilen,ityp,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info faces_per_element')
      call cmo_get_info('edges_per_element',cmo_name,nee,ilen,ityp,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info edges_per_element')
 
C     Check nen, nef and nee to identify the mesh type
C
C.... Tetrahedron Mesh = tet 4, 4, 6
      if (( nen.eq.nelmnen(ifelmtet) ) .and.
     *    ( nef.eq.nelmnef(ifelmtet) ) .and.
     *    ( nee.eq.nelmnee(ifelmtet) ) ) then
 
         ierror_return=0
         mesh_type = 'tet'
         imesh_type = ifelmtet
 
C.... Hexahedron Mesh = hex 8, 6, 12
      elseif ( ( nen.eq.nelmnen(ifelmhex) ) .and.
     *         ( nef.eq.nelmnef(ifelmhex) ) .and.
     *         ( nee.eq.nelmnee(ifelmhex) ) ) then
 
         ierror_return=0
         mesh_type = 'hex'
         imesh_type = ifelmhex
 
C.... Prism Mesh = pri 6, 5, 9
      elseif ( ( nen.eq.nelmnen(ifelmpri) ) .and.
     *         ( nef.eq.nelmnef(ifelmpri) ) .and.
     *         ( nee.eq.nelmnee(ifelmpri) ) ) then
 
         ierror_return=0
         mesh_type = 'pri'
         imesh_type = ifelmpri
 
C.... Pyramid Mesh = pyr 5, 5, 8
      elseif ( ( nen.eq.nelmnen(ifelmpyr) ) .and.
     *         ( nef.eq.nelmnef(ifelmpyr) ) .and.
     *         ( nee.eq.nelmnee(ifelmpyr) ) ) then
 
         ierror_return=0
         mesh_type = 'pyr'
         imesh_type = ifelmpyr
 
C.... Triangle Mesh = tri or triplane 3, 3, 3
      elseif ( ( nen.eq.nelmnen(ifelmtri) ) .and.
     *         ( nef.eq.nelmnef(ifelmtri) ) .and.
     *         ( nee.eq.nelmnee(ifelmtri) ) ) then
 
         ierror_return=0
         imesh_type = ifelmtri
 
C        Triangle Mesh in a plane nsdgeom = nsdtopo = 2
         if (nsdg.eq.2 .and. nsdt.eq.2 ) then
           mesh_type = 'triplane'
         else
           mesh_type = 'tri'
         endif
 
C.... Quadrilateral Mesh = quad 4, 4, 4
      elseif ( ( nen.eq.nelmnen(ifelmqud) ) .and.
     *         ( nef.eq.nelmnef(ifelmqud) ) .and.
     *         ( nee.eq.nelmnee(ifelmqud) ) ) then
 
         ierror_return=0
         mesh_type = 'quad'
         imesh_type = ifelmqud
 
C
      elseif ((mesh_type(1:3) .eq. 'hyb') .or.
     *        (mesh_type(1:3) .eq. 'HYB')) then
C
C.... Hybrid Mesh = hyb 10, 10, 12
      elseif ( ( nen.eq.nelmnen(ifelmhyb) ) .and.
     *         ( nef.eq.nelmnef(ifelmhyb) ) .and.
     *         ( nee.eq.nelmnee(ifelmhyb) ) ) then
 
         ierror_return=0
         mesh_type = 'hyb'
         imesh_type = ifelmhyb
 
C.... Line Mesh = line 2, 2, 1
      elseif ( ( nen.eq.nelmnen(ifelmlin) ) .and.
     *         ( nef.eq.nelmnef(ifelmlin) ) .and.
     *         ( nee.eq.nelmnee(ifelmlin) ) ) then
 
         ierror_return=0
         mesh_type = 'line'
         imesh_type = ifelmlin
 
C.... Point Mesh = pnt 1, 0, 0
      elseif ( ( nen.eq.nelmnen(ifelmpnt) ) .and.
     *         ( nef.eq.nelmnef(ifelmpnt) ) .and.
     *         ( nee.eq.nelmnee(ifelmpnt) ) ) then
 
         ierror_return=0
         mesh_type = 'pnt'
         imesh_type = ifelmpnt
 
C.... Mesh Type not found
      else
 
         ierror_return=-1
         write(logmess,'(a,i4,i4,i4)')
     *   'ERROR: Unrecognized Mesh Type nen,nef,nee: ',nen,nef,nee
         call writloga('default',0,logmess,0,ierr)
 
      endif
 
C     For error checking, get element type of element 1
C     if ielem_typ is 0, then it may have not been set yet
      call cmo_get_info('itettyp',cmo_name,ipitettyp,ilen_typ,ityp,ierr)
      call cmo_get_intinfo('nelements',cmo_name,nelements,ilen,ityp,
     *        ierr)
      if(ierr.ne.0)call x3d_error(isubname,'get_info itettyp')
 
      ielem_typ = itettyp(1)
      if (ilen_typ.gt.0 .and. ielem_typ.ne.0 .and. nelements.gt.0) then
        elem_typ = 'non'
        if(ielem_typ .eq. ifelmpnt) elem_typ = 'pnt'
        if(ielem_typ .eq. ifelmlin) elem_typ = 'lin'
        if(ielem_typ .eq. ifelmtri) elem_typ = 'tri'
        if(ielem_typ .eq. ifelmtet) elem_typ = 'tet'
        if(ielem_typ .eq. ifelmhex) elem_typ = 'hex'
        if(ielem_typ .eq. ifelmpyr) elem_typ = 'pyr'
        if(ielem_typ .eq. ifelmpri) elem_typ = 'pri'
        if(ielem_typ .eq. ifelmqud) elem_typ = 'qud'
        if(ielem_typ .eq. ifelmhyb) elem_typ = 'hyb'
 
        if (ielem_typ.ne.imesh_type .and. imesh_type.ne.ifelmhyb) then
           write(logmess,'(a)')
     *    'Warning cmo_get_mesh_type: mesh_type differs from elem type.'
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,i4,1x,a)')
     *     'cmo mesh_type:      ',imesh_type,mesh_type(1:3)
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,i4,1x,a)')
     *     'first element type: ',ielem_typ,elem_typ(1:3)
           call writloga('default',0,logmess,1,ierr)
        endif
      endif
C
      return
      end
