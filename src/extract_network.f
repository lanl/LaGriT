!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  EXTRACT_NETWORK -- Extract the material interface network.
!!
!!      CALL EXTRACT_NETWORK (CMOOUT, CMOIN, MASK, IERROR)
!!
!!    This routine extracts the material interface network from a multimaterial
!!    mesh to create a new mesh object.  In contrast to other interface
!!    extraction routines that extract the "child interface" on one (or both)
!!    sides of the interface, this routine extracts the "parent interface".
!!    Because of the nature of this extraction, not all attributes in the output
!!    mesh object are given values.  Among the array-valued attributes, only
!!    XIC, YIC, ZIC, ITET, ITETOFF, ITETTYP, and ICR1 are set.  In particular
!!    JTET is not set, because it is possible, in general, for more than two
!!    cells to share a face in a network.  The ICONTAB array is copied from the
!!    input mesh object to the output mesh object.
!!
!!    The output mesh object is given an additional node-based attribute named
!!    "map" which provides the mapping from nodes in the extracted interface
!!    network to (parent) nodes in the input mesh object; that is, MAP(J) is
!!    the parent node in the input mesh object that corresponds to node J in
!!    the output mesh object.
!!
!!  INPUT ARGUMENTS
!!
!!    CMOOUT -- Output mesh object name.
!!    CMOIN  -- Input mesh object name.
!!    MASK   -- Node mask.  Currently ignored!  All nodes are considered.
!!
!!  OUTPUT ARGUMENTS
!!
!!    IERROR -- Error flag
!!
!!  CHANGE HISTORY --
!!
c$Log: extract_network.f,v $
cRevision 2.00  2007/11/05 19:45:54  spchu
cImport to CVS
c
CPVCS    
CPVCS       Rev 1.2   05 May 2000 15:12:16   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.1   Mon Nov 30 11:03:02 1998   nnc
CPVCS    Added documentation.
CPVCS
CPVCS       Rev 1.0   Thu Nov 05 12:55:48 1998   dcg
CPVCS    Initial revision.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      subroutine extract_network (cmoout, cmoin, mask, ierror)
      implicit none
 
      character*(*) cmoout, cmoin
      integer mask(*), ierror
 
      include "local_element.h"
 
      !! Define the element type associated with each element face.  It is
      !! purely coincidental that the number of points on a face is the same
      !! as the face element type.  (Move into local_element.h/blockcom.f?)
 
      integer ifacetype(maxnef,nelmtypes)
      data ifacetype / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 2, 0, 0, 0, 0, 0, 0,
     *                 3, 3, 3, 3, 0, 0, 0, 0, 0, 0,
     *                 4, 3, 3, 3, 3, 0, 0, 0, 0, 0,
     *                 3, 3, 4, 4, 4, 0, 0, 0, 0, 0,
     *                 4, 4, 4, 4, 4, 4, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     *               /
 
      pointer (ipxic,xic), (ipyic,yic), (ipzic,zic)
      real*8 xic(*), yic(*), zic(*)
 
      pointer (ipitet,itet), (ipitetoff,itetoff), (ipitettyp,itettyp),
     &        (ipjtet,jtet), (ipjtetoff,jtetoff), (ipitetclr,itetclr),
     &        (ipitp1,itp1), (ipisn1,isn1), (ipicr1,icr1),
     &        (ipicontab,icontab)
      integer itet(*), jtet(*), itetoff(*), jtetoff(*), itettyp(*),
     &        itetclr(*), itp1(*), isn1(*), icr1(*), icontab(*)
 
      pointer (ipxicn,xicn), (ipyicn,yicn), (ipzicn,zicn)
      real*8 xicn(*), yicn(*), zicn(*)
 
      pointer (ipitetn,itetn), (ipitetoffn,itetoffn), (ipcpmap,cpmap),
     &        (ipitettypn,itettypn), (ipicr1n,icr1n), (ipfmap,fmap),
     &        (ipicontabn,icontabn), (ipmap,map)
      integer itetn(*), itetoffn(*), itettypn(*), icr1n(*),
     &        icontabn(*), map(*), cpmap(*), fmap(*)
 
      integer i, j, k, m, n, ilen, ityp, nnnpe, nnfpe, nnepe, jnbr
      integer nncells, nnnodes, minft, maxft, nfpe, nconbnd, node
      integer nnodes, ncells, topo_dim, geom_dim, mbndry, ncon50
 
      character*132 cbuf, logmess
      character*32 isubname
 
      isubname = 'extract_network'
      ierror = 0
 
     !!!
     !!! Get the attributes from the input mesh object.
 
      call cmo_get_info('ndimensions_topo',cmoin,topo_dim,ilen,ityp,
     &                                                           ierror)
      call cmo_get_info('ndimensions_geom',cmoin,geom_dim,ilen,ityp,
     &                                                           ierror)
      call cmo_get_info('nelements',cmoin,ncells,ilen,ityp,ierror)
      call cmo_get_info('nnodes',cmoin,nnodes,ilen,ityp,ierror)
      call cmo_get_info('faces_per_element',cmoin,nfpe,ilen,ityp,ierror)
      call cmo_get_info('mbndry',cmoin,mbndry,ilen,ityp,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,ityp,ierror)
      call cmo_get_info('jtet',cmoin,ipjtet,ilen,ityp,ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,ilen,ityp,ierror)
      call cmo_get_info('jtetoff',cmoin,ipjtetoff,ilen,ityp,ierror)
      call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,ityp,ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmoin,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('isn1',cmoin,ipisn1,ilen,ityp,ierror)
      call cmo_get_info('icr1',cmoin,ipicr1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,ityp,ierror)
 
     !!!
     !!! Preliminary pass through the cells, counting faces and nodes.
 
      call mmgetblk ('cpmap', isubname, ipcpmap, nnodes, 1, ierror)
      call mmgetblk ( 'fmap', isubname,  ipfmap, nnodes, 1, ierror)
      call unpackpc (nnodes, itp1, isn1, cpmap)
      do j = 1, nnodes
        fmap(j) = 0
      end do
 
      nncells = 0
      nnnodes = 0
      minft = nelmtypes
      maxft = 0
 
      do j = 1, ncells
        do k = 1, nelmnef(itettyp(j))
          if (jtet(k+jtetoff(j)) .gt. mbndry) then
            jnbr = 1 + (jtet(k+jtetoff(j)) - mbndry - 1) / nfpe
            if (itetclr(jnbr) .gt. itetclr(j)) then
              nncells = nncells + 1
              minft = min( ifacetype(k,itettyp(j)), minft )
              maxft = max( ifacetype(k,itettyp(j)), maxft )
              do i = 1, ielmface0(k,itettyp(j))
                node = cpmap(itet(itetoff(j)+ielmface1(i,k,itettyp(j))))
                if (fmap(node) .eq. 0) then
                  nnnodes = nnnodes + 1
                  fmap(node) = nnnodes
                end if
              end do
            end if
          end if
        end do
      end do
 
      if (nncells .eq. 0) then
        logmess = 'Input mesh object has no material interfaces.'
        call writloga ('default', 0, logmess, 0, ierror)
        return
      end if
 
      !! Determine the type of the extracted mesh.
      if (minft.eq.ifelmlin .and. maxft.eq.ifelmlin) then
        nnnpe = nelmnen(ifelmlin)
        nnfpe = nelmnef(ifelmlin)
        nnepe = nelmnee(ifelmlin)
      else if (minft.eq.ifelmtri .and. maxft.eq.ifelmtri) then
        nnnpe = nelmnen(ifelmtri)
        nnfpe = nelmnef(ifelmtri)
        nnepe = nelmnee(ifelmtri)
      else if (minft.eq.ifelmqud .and. maxft.eq.ifelmqud) then
        nnnpe = nelmnen(ifelmqud)
        nnfpe = nelmnef(ifelmqud)
        nnepe = nelmnee(ifelmqud)
      else if (minft.eq.ifelmtri .and. maxft.eq.ifelmqud) then
        nnnpe = nelmnen(ifelmhyb)
        nnfpe = nelmnef(ifelmhyb)
        nnepe = nelmnee(ifelmhyb)
      else
        logmess = 'EXTRACT_NETWORK: PANIC!  bad minft/maxft values.'
        call writloga ('default', 0, logmess, 0, ierror)
        stop
      end if
 
     !!!
     !!! Create the output mesh object
 
      call cmo_exist (cmoout, ierror)
      if (ierror .eq. 0) call cmo_release (cmoout, ierror)
      call cmo_create (cmoout, ierror)
 
      call cmo_set_info('nnodes',cmoout,nnnodes,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nncells,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,topo_dim-1,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,geom_dim,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,nnnpe,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,nnfpe,1,1,ierror)
      call cmo_set_info('edges_per_element',cmoout,nnepe,1,1,ierror)
 
      call cmo_newlen (cmoout, ierror)
 
      call cmo_get_intinfo('mbndry',cmoin,mbndry,ilen,ityp,ierror)
      call cmo_get_info('itet',cmoout,ipitetn,ilen,ityp,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffn,ilen,ityp,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypn,ilen,ityp,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1n,ilen,ityp,ierror)
      call cmo_get_info('xic',cmoout,ipxicn,ilen,ityp,ierror)
      call cmo_get_info('yic',cmoout,ipyicn,ilen,ityp,ierror)
      call cmo_get_info('zic',cmoout,ipzicn,ilen,ityp,ierror)
 
      !! Add the output->input node mapping as an attribute of the
      !! output mesh object.
      cbuf = "cmo/addatt/" // cmoout //
     &       "/map/vint/scalar/nnodes////0; finish"
      call dotaskx3d (cbuf, ierror)
      call cmo_get_info ("map", cmoout, ipmap, ilen, ityp, ierror)
      do j = 1, nnodes
        if (fmap(j) .gt. 0) then
          map(fmap(j)) = j
        end if
      end do
 
     !!!
     !!! Second pass through the cells.
 
      n = 0
      m = 0
 
      do j = 1, ncells
        do k = 1, nelmnef(itettyp(j))
          if (jtet(k+jtetoff(j)) .gt. mbndry) then
            jnbr = 1 + (jtet(k+jtetoff(j)) - mbndry - 1) / nfpe
            if (itetclr(jnbr) .gt. itetclr(j)) then
              n = n + 1
              itettypn(n) = ifacetype(k,itettyp(j))
              itetoffn(n) = m
              m = m + nelmnen(itettypn(n))
              do i = 1, ielmface0(k,itettyp(j))
                node = cpmap(itet(itetoff(j)+ielmface1(i,k,itettyp(j))))
                itetn(itetoffn(n)+i) = fmap(node)
              end do
            end if
          end if
        end do
      end do
 
     !!!
     !!! Copy node-based data into new mesh object.
 
      if (geom_dim .eq. 2) then
 
        do j = 1, nnnodes
          xicn(j) = xic(map(j))
          yicn(j) = yic(map(j))
          icr1n(j) = icr1(map(j))
        end do
 
      else if (geom_dim .eq. 3) then
 
        do j = 1, nnnodes
          xicn(j) = xic(map(j))
          yicn(j) = yic(map(j))
          zicn(j) = zic(map(j))
          icr1n(j) = icr1(map(j))
        end do
 
      end if
 
      !! NOTE.  The following node-based attributes are left uninitialized:
      !! ialias, imt1, itp1, isn1, ign1.  The following cell-based attributes
      !! are left uninitialized: itetclr, jtet, jtetoff.
 
     !!!
     !!! Copy the constraint table into the new mesh object.
 
      cbuf = 'cmo/addatt/' // cmoout //
     &       '/ncon50/int/scalar/scalar/constant/permanent/x/0; finish'
      call dotaskx3d (cbuf, ierror)
      call cmo_get_info ('ncon50',cmoin,  ncon50, ilen,ityp,ierror)
      call cmo_set_info ('ncon50',cmoout, ncon50, i,1,ierror)
 
      cbuf = 'cmo/addatt/' // cmoout //
     &       '/nconbnd/int/scalar/scalar/constant/permanent/x/0; finish'
      call dotaskx3d (cbuf, ierror)
      call cmo_get_info ('nconbnd',cmoin,  nconbnd, ilen,ityp,ierror)
      call cmo_set_info ('nconbnd',cmoout, nconbnd, 1,1,ierror)
 
      cbuf = 'cmo/addatt/' // cmoout //
     &     '/icontab/vint/scalar/ncon50/constant/permanent/x/0; finish'
      call dotaskx3d (cbuf, ierror)
      call cmo_get_info ('icontab',cmoin,  ipicontab,  ilen,ityp,ierror)
      call cmo_get_info ('icontab',cmoout, ipicontabn, ilen,ityp,ierror)
      do j = 1, ncon50
       icontabn(j) = icontab(j)
      end do
 
      call mmrelprt(isubname,ierror)
 
      return
      end
