      subroutine cmo_set_mesh_type(cmo_name, mesh_type, ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine sets the defaults for a Mesh Object Type.
C
C     INPUT ARGUMENTS -
C
C        cmo_name  - (character) Mesh Object Name.
C        mesh_type - (character) Mesh Type.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/cmo_set_mesh_type.f_a  $
CPVCS    
CPVCS       Rev 1.1   06 Dec 2001 12:20:26   tam
CPVCS    no warning if itettyp is 0 (not set yet)
CPVCS    
CPVCS       Rev 1.0   27 Jul 2001 11:24:20   tam
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.1   08 Jun 2000 13:10:38   dcg
CPVCS    add triplane type like tri but ndimensions_geom is 2
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:12   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:41:30 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu May 02 13:23:48 1996   dcg
CPVCS    parametize the routine using ifelm...
CPVCS
CPVCS       Rev 1.1   Wed Apr 24 13:28:44 1996   dcg
CPVCS    change nen and nef to 10 for hybrid grids
CPVCS
CPVCS       Rev 1.0   03/15/95 15:40:58   ejl
CPVCS    New options added.
CPVCS
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
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer nsdgeom, nsdtopo, nen, nef, nee
      integer ierror, ierr
      integer ilen_typ,ityp,ielem_typ,imesh_type
      pointer (ipitettyp, itettyp(*))
      integer itettyp
C
      character*32  elem_typ
      character*132 logmess
C
C#######################################################################
C
C
C
      if ((mesh_type(1:3) .eq. 'tet') .or.
     *    (mesh_type(1:3) .eq. 'TET')) then
C
C....    Tetrahedron Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  3
         nen     =  nelmnen(ifelmtet)
         nef     =  nelmnef(ifelmtet)
         nee     =  nelmnee(ifelmtet)
         imesh_type = ifelmtet
C
      elseif ((mesh_type(1:3) .eq. 'hex') .or.
     *        (mesh_type(1:3) .eq. 'HEX')) then
C
C....    Hexahedron Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  3
         nen     =  nelmnen(ifelmhex)
         nef     =  nelmnef(ifelmhex)
         nee     =  nelmnee(ifelmhex)
         imesh_type = ifelmhex
C
      elseif ((mesh_type(1:3) .eq. 'pri') .or.
     *        (mesh_type(1:3) .eq. 'PRI')) then
C
C....    Prism Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  3
         nen     =  nelmnen(ifelmpri)
         nef     =  nelmnef(ifelmpri)
         nee     =  nelmnee(ifelmpri)
         imesh_type = ifelmpri
C
      elseif ((mesh_type(1:3) .eq. 'pyr') .or.
     *        (mesh_type(1:3) .eq. 'PYR')) then
C
C....    Pyramid Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  3
         nen     =  nelmnen(ifelmpyr)
         nef     =  nelmnef(ifelmpyr)
         nee     =  nelmnee(ifelmpyr)
         imesh_type = ifelmpyr
C
      elseif ((mesh_type(1:8) .eq. 'triplane') .or.
     *        (mesh_type(1:8) .eq. 'TRIPLANE')) then
C
C....    Triangle Mesh in a plane.
C
         ierror_return=0
         nsdgeom =  2
         nsdtopo =  2
         nen     =  nelmnen(ifelmtri)
         nef     =  nelmnef(ifelmtri)
         nee     =  nelmnee(ifelmtri)
         imesh_type = ifelmtri
C
      elseif ((mesh_type(1:3) .eq. 'tri') .or.
     *        (mesh_type(1:3) .eq. 'TRI')) then
C
C....    Triangle Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  2
         nen     =  nelmnen(ifelmtri)
         nef     =  nelmnef(ifelmtri)
         nee     =  nelmnee(ifelmtri)
         imesh_type = ifelmtri
C
      elseif ((mesh_type(1:3) .eq. 'qua') .or.
     *        (mesh_type(1:3) .eq. 'QUA')) then
C
C....    Quadrilateral Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  2
         nen     =  nelmnen(ifelmqud)
         nef     =  nelmnef(ifelmqud)
         nee     =  nelmnee(ifelmqud)
         imesh_type = ifelmqud
C
      elseif ((mesh_type(1:3) .eq. 'hyb') .or.
     *        (mesh_type(1:3) .eq. 'HYB')) then
C
C....    Hybrid Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  3
         nen     =  nelmnen(ifelmhyb)
         nef     =  nelmnef(ifelmhyb)
         nee     =  nelmnee(ifelmhyb)
         imesh_type = ifelmhyb
C
      elseif ((mesh_type(1:3) .eq. 'lin') .or.
     *        (mesh_type(1:3) .eq. 'LIN')) then
C
C....    Line Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  1
         nen     =  nelmnen(ifelmlin)
         nef     =  nelmnef(ifelmlin)
         nee     =  nelmnee(ifelmlin)
         imesh_type = ifelmlin
C
      elseif ((mesh_type(1:3) .eq. 'pnt') .or.
     *        (mesh_type(1:3) .eq. 'PNT')) then
C
C....    Point Mesh.
C
         ierror_return=0
         nsdgeom =  3
         nsdtopo =  1
         nen     =  nelmnen(ifelmpnt)
         nef     =  nelmnef(ifelmpnt)
         nee     =  nelmnee(ifelmpnt)
         imesh_type = ifelmpnt
C
      else
C
C....    Illegal Mesh Type.
         ierror_return=-1
         write(logmess,'(a,a)') 
     *    'ERROR: Illegal Mesh Type: ',mesh_type
         call writloga('default',0,logmess,0,ierr)
      endif
C
      if (ierror_return .eq. 0) then
C
C....    Store the data in the Mesh Object.
         call cmo_set_info('ndimensions_geom',cmo_name,nsdgeom,
     *                     1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmo_name,nsdtopo,
     *                     1,1,ierror)
         call cmo_set_info('nodes_per_element',cmo_name,nen,
     *                     1,1,ierror)
         call cmo_set_info('faces_per_element',cmo_name,nef,
     *                     1,1,ierror)
         call cmo_set_info('edges_per_element',cmo_name,nee,
     *                     1,1,ierror)
      endif

C     For error checking, get element type of element 1
      call cmo_get_info('itettyp',cmo_name,ipitettyp,ilen_typ,ityp,ierr)
      if(ierr.ne.0)call x3d_error('set_mesh_type','get_info itettyp')

      ielem_typ = itettyp(1)
      if (ilen_typ.gt.0 .and. ielem_typ.ne.0) then
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
     *'Warning cmo_set_mesh_type: mesh_type now differs from elem type.'
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
