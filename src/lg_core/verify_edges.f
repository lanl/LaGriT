      subroutine verify_edges()
c
c #####################################################################
c
c    PURPOSE
c
c      loop through all elements and all edges to
C      verify that edge connectivity is correct
c
c    CHANGE HISTORY -
c
C $Log: verify_edges.f,v $
C Revision 2.00  2007/11/09 20:04:05  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   04 May 2000 16:12:40   dcg
CPVCS    Initial revision.

c #####################################################################
 
      implicit none
      include 'local_element.h'

      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)

      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
      integer itp1(*)
      integer isn1(*)
 
      integer itet(*)
      integer itetoff(*)
      integer itettyp(*)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(*),jtetoff(*)
 
      pointer (ipiparent,iparent)
      pointer (ipelts,elts)
      integer elts(*),iparent(*)
      pointer (ipedges,edges)
      integer edges(*)
c
      character*32 cmo,isubname
       integer nnodes,length,icmotype,ierror,nef_cmo,mbndry,
     *   nelements,icscode,i,j,ityp,nelts
c #####################################################################
 
      isubname = 'verify_edges'
 
c....
c.... Create temporary storage for elements that share an edge and
c.... their local edge numbers
c....
      call mmgetblk('elts',isubname,ipelts,100,1,icscode)
      call mmgetblk('edges',isubname,ipedges,100,1,icscode)
 
c.... Get info from mesh object.
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,nef_cmo,
     &   length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
c
c.... Get memory for arrays that should have length NNODES.
      call mmgetblk('iparent',isubname,ipiparent,
     &      nnodes,1,icscode)
c     ..................................................................
c     find the parents of each node.
c
      call unpackpc(nnodes,itp1,isn1,iparent)
c
c  loop through all elements and all edges
c  this loop will hit an error if the edges are not valid
c
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnee(ityp)
            call get_elements_on_edge(i,j,nelts,ipelts,
     *        ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *        ipitettyp,ipiparent, nef_cmo,mbndry)
         enddo
      enddo
c
      call mmrelprt(isubname,icscode)
      return
      end
