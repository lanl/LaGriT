      subroutine bin_ij_edge(cmo,prtname)
C
C #####################################################################
C
C     PURPOSE -
C
C        Set up bins that bin element edges
C        by the sum of the node numbers.
C
C     INPUT ARGUMENTS -
C
C        cmo      - The name of the CMO.
C        prtname  - Name of memory partition for bin arrays.
C
C     OUTPUT ARGUMENTS -
C        NONE
C        Arrays created are passed back to the calling routine
C        via memory managed pointers. To access arrays created:
C      call cmo_get_info('nedge_bin',prtname,ipnedge_bin,length,1,icscode)
C      call cmo_get_info('nedge_off',prtname,ipnedge_off,length,1,icscode)
C      call mmgetblk('iedge1',prtname,ipiedge1,length,1,icscode)
C      call mmgetblk('iedge2',prtname,ipiedge2,length,1,icscode)
C
C        ioff     - offset into element,edge and face arrays
C        ipit     - pointer to array of element numbers
C        ipie     - pointer to array of edge numbers
C        ipif     - pointer to array of face numbers
C
C     CHANGE HISTORY -
C
C        $Log: bin_ij_edge.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jul 09 09:54:00 1997   gable
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Jun 24 16:36:16 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      include "local_element.h"
      character*40 cmo
      character*40 prtname
      character*40 isubname
      
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      pointer (ipiedge1, iedge1)
      pointer (ipiedge2, iedge2)
c      pointer (ipxedge1, xedge1)
c      pointer (ipyedge1, yedge1)
c      pointer (ipzedge1, zedge1)
      integer nedge_bin(1000000), nedge_off(1000000)
      integer iedge1(10000000), iedge2(10000000)
c
      pointer (ipitet, itet1(1000000))
      pointer (ipitettyp, itettyp(1000000))
      pointer (ipitp1, itp1(1000000))
      pointer (ipisn1, isn1(1000000))
      pointer (ipiparent, iparent(1000000))
      pointer (ipitetoff, itetoff(1000000))
C
      isubname = 'bin_ij_edge'
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
C
      call mmgetblk("iparent",isubname,ipiparent,nnodes,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
C
C     Create bins of edges using the sum of the i,j global node
C     numbers as the bin.
C
      length=2*nnodes
      call mmgetblk('nedge_bin',prtname,
     *              ipnedge_bin,length,1,icscode)
      call mmgetblk('nedge_off',prtname,
     *              ipnedge_off,length,1,icscode)
      do i=1,2*nnodes
         nedge_bin(i)=0
         nedge_off(i)=0
      enddo
      do it=1,nelements
         do ie=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
            isum12=i1+i2
            nedge_bin(isum12)=nedge_bin(isum12)+1
         enddo
      enddo
      isum=0
      do i=1,2*nnodes
         if(nedge_bin(i).gt.0) then
            nedge_off(i)=isum
            isum=isum+nedge_bin(i)
         endif
         nedge_bin(i)=0
      enddo
      length=isum+1
C
C     Now we have the actual number of edges in the simplex
C
      call mmgetblk('iedge1',prtname,
     *              ipiedge1,length,1,icscode)
      call mmgetblk('iedge2',prtname,
     *              ipiedge2,length,1,icscode)
      do i=1,length
         iedge1(i)=0
         iedge2(i)=0
      enddo
      do it=1,nelements
         do ie=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
            isum12=i1+i2
            if(nedge_bin(isum12).gt.0) then
               do j=1,nedge_bin(isum12)
                  j1=iedge1(nedge_off(isum12)+j)
                  j2=iedge2(nedge_off(isum12)+j)
                  if((i1.eq.j1.and.i2.eq.j2) .or.
     *               (i1.eq.j2.and.i2.eq.j1)) goto 300
               enddo
            endif
            nedge_bin(isum12)=nedge_bin(isum12)+1
            iedge1(nedge_off(isum12)+nedge_bin(isum12))=i1
            iedge2(nedge_off(isum12)+nedge_bin(isum12))=i2
 300     continue
         enddo
      enddo
C
      do i=1,length
         write(6,*)i, iedge1(i), iedge2(i)
      enddo
9999  continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
