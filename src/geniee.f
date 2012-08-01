*dk,geniee
C #####################################################################
C
C     PURPOSE -
C
C      Various subroutines (geniee, geniee_cmo, sub_geniee_cmo, geniee_slow)
C      to generate mesh-neighbor connectivity.  See notes for each subroutine.
C
C     CHANGE HISTORY -
C
C        $Log: geniee.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.29   12 Jul 2007 07:34:20   gable
CPVCS    Install bug fixes to normal_check_flip by Andrew Kuprat. There were some problems with the way itet and jtet arrays were updated. That has been fixed. Also fixed problems with flipping and repair of quad mesh objects.
CPVCS    
CPVCS       Rev 1.28   08 Feb 2006 14:38:18   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.27   29 Jun 2004 08:57:24   dcg
CPVCS    fix declarations for linux compiler
CPVCS
CPVCS       Rev 1.26   30 Mar 2004 15:31:06   gable
CPVCS    Inline subroutines normal_check_flip and
CPVCS    reverse_itet_jtet withing the geniee.f file.
CPVCS
CPVCS       Rev 1.25   24 Dec 2003 10:21:54   tam
CPVCS    add het subroutine geniee_cmo_subset()  used with refine 'amr' iprd option
CPVCS
CPVCS       Rev 1.24   10 Jul 2003 14:45:08   gable
CPVCS    No change.
CPVCS
CPVCS       Rev 1.23   14 Mar 2001 14:13:52   dcg
CPVCS    move data statements to end of declarations
CPVCS    get rid of upper case
CPVCS
CPVCS       Rev 1.22   05 May 2000 09:00:38   jtg
CPVCS    nwrite added as argument to elmtestd,sub_elmtestd calls
CPVCS
CPVCS       Rev 1.21   03 May 2000 10:57:18   jtg
CPVCS    modifed comments to correspond to what it coded
CPVCS
CPVCS       Rev 1.20   Wed Apr 05 13:34:24 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.19   04 Feb 2000 13:10:54   jtg
CPVCS    changed error to warning if nelements=0, also added check on jtetoff
CPVCS
CPVCS       Rev 1.18   28 Jan 2000 14:43:24   jtg
CPVCS    fixed binning error when jtet_reduce_nnd=1
CPVCS
CPVCS       Rev 1.17   27 Jan 2000 11:34:14   jtg
CPVCS    incorrect length for nedge_bin fixed
CPVCS
CPVCS       Rev 1.16   24 Jan 2000 15:15:06   jtg
CPVCS    modifed to use jtet_cycle_max to limit jtet loops
CPVCS    and to handle elements with repeated nodes if jtet_reduce_nnd=1.
CPVCS    Also now writes diagnostics to default output if there are faces with
CPVCS    repeated nodes, repeated faces within an element, or jtet cycles
CPVCS    of length longer than 2.
CPVCS
CPVCS       Rev 1.15   10 Jan 2000 11:52:32   jtg
CPVCS    removed call to x3d_error that was causing grief
CPVCS
CPVCS       Rev 1.14   Tue Nov 30 16:59:04 1999   jtg
CPVCS    now can handle networks, and if mbndry=0 it uses the convention
CPVCS    that jtet < 0 instead of jtet > mbndry for interfaces.
CPVCS
CPVCS       Rev 1.13   Fri Jan 22 12:02:28 1999   dcg
CPVCS    fix typo ifelmpry ifelmpyr
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 16:49:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   Mon Dec 09 09:01:06 1996   het
CPVCS    Take into account AMR refinement levels.
CPVCS
CPVCS       Rev 1.9   Wed Jul 24 17:31:28 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.8   Wed May 22 07:02:20 1996   het
CPVCS    Change the i1=i1*j1 test to i1=i1.or.j1 because of overflows.
CPVCS
CPVCS       Rev 1.7   11/07/95 17:17:56   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   08/22/95 06:52:14   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.5   08/15/95 18:20:22   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.4   06/27/95 11:10:54   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.3   06/07/95 06:21:32   het
CPVCS    Add more local_element.h references
CPVCS
CPVCS       Rev 1.2   02/10/95 08:27:24   het
CPVCS    Change the nfaces variable to nface because of cmo.h
CPVCS
CPVCS       Rev 1.1   01/04/95 22:02:30   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:20   pvcs
CPVCS    Original version.
C
C ######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C #####################################################################
C
      subroutine geniee (itet1,jtet1,jtet2,
     &                   nen,nef,
     &                   nelements,nnodes,
     &                   nsd,
     &                   nnodes_max,nelements_max)
C
C #####################################################################
C
C     PURPOSE -
C
C       CREATE NEIGHBOR COnneCTIVITY LISTS FOR HOMOGENEOUS MESHES
C       geniee_cmo or sub_geniee_cmo_lg should be used instead
C       for hybrid meshes, networks, and/or degenerate elements.
C
C     INPUT ARGUMENTS -
C
C        ITET1 NODE COnneCTIVITY LIST
C        NEN NUMBER OF NODES PER ELEMENT
C        NEF NUMBER OF FACETS PER ELEMENT
C        NELEMENTS NUMBER OF ELEMENTS
C        NNNODES  NUMBER OF NODES
C        NSD    TOPOGRAPHIC DIMENSION (2 OR 3)
C
C     OUTPUT ARGUMENTS -
C
C        JTET1  LIST OF NEIGHBOR ELEMENTS
C        JTET2  LIST OF NEIGHBOR ELEMENT  FACETS
C
C     CHANGE HISTORY -
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
C

C arguments itet1,jtet1,jtet2,
C                        nen,nef,
C                        nelements,nnodes,
C                        nsd,
C                        nnodes_max,nelements_max)

      integer nen,nef,nelements,nnodes,nsd,nnodes_max,nelements_max
      integer itet1(nen*nelements)
      integer jtet1(nef*nelements)
      integer jtet2(nef*nelements)



C variables
      pointer (ipitface, itface)
      pointer (ipifface, ifface)
      integer itface(*), ifface(*)

      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      integer nedge_bin(*), nedge_off(*)

      integer itettyp,ierrwrt,jtettyp,length,icscode,i,it,ifsum,j,
     *        j1,isum,nodemax1,nodemin1,nodesum1,nodemax2,nodesum2,
     *        nodesum3,k,jt,jf,modemax1,modemax2,modemin1,modesum2,
     *        modesum3,modesum1


      character*132 logmess
      character*32 isubname
C
C ######################################################################
C BEGIN begin
C
      isubname='geniee'
C
      if(nsd.eq.1) then
         itettyp=ifelmlin
      elseif(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         itettyp=ifelmtri
      elseif(nsd.eq.2.and.
     *       nen.eq.nelmnen(ifelmqud).and.nef.eq.nelmnef(ifelmqud)) then
         itettyp=ifelmqud
      elseif(nsd.eq.3.and.
     *       nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet)) then
         itettyp=ifelmtet
      elseif(nsd.eq.3.and.
     *       nen.eq.nelmnen(ifelmpyr).and.nef.eq.nelmnef(ifelmpyr)) then
         itettyp=ifelmpyr
      elseif(nsd.eq.3.and.
     *       nen.eq.nelmnen(ifelmpri).and.nef.eq.nelmnef(ifelmpri)) then
         itettyp=ifelmpri
      elseif(nsd.eq.3.and.
     *       nen.eq.nelmnen(ifelmhex).and.nef.eq.nelmnef(ifelmhex)) then
         itettyp=ifelmhex
      else
         write(logmess,9000) nsd,nen,nef
 9000    format('Illegal geniee parameters: nsd=',i3,
     *          ' nen=',i3,' nef=',i3)
         call writloga('default',1,logmess,1,ierrwrt)
      endif
C
      jtettyp=itettyp
C
      length=4*nnodes
      call mmgetblk('nedge_bin',isubname,
     *              ipnedge_bin,length,2,icscode)
      call mmgetblk('nedge_off',isubname,
     *              ipnedge_off,length,2,icscode)
C
C  nedge_bin contains a count of number of elements that share a facet
C  indexed by the sum of the node numbers of the facet
C  Since this sum is not unique they different facets will be sorted
C  out later
C  nedge_off is the offset into the itface and ifface arrays by element
C
      do i=1,4*nnodes
         nedge_bin(i)=0
         nedge_off(i)=0
      enddo
      do it=1,nelements
         do i=1,nef
            ifsum=0
            do j=1,ielmface0(i,itettyp)
               j1=itet1(nen*(it-1)+ielmface1(j,i,itettyp))
               ifsum=ifsum+j1
            enddo
            nedge_bin(ifsum)=nedge_bin(ifsum)+1
         enddo
      enddo
      isum=0
      do i=1,4*nnodes
         if(nedge_bin(i).gt.0) then
            nedge_off(i)=isum
            isum=isum+nedge_bin(i)
         endif
         nedge_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('itface',isubname,ipitface,length,2,icscode)
      call mmgetblk('ifface',isubname,ipifface,length,2,icscode)
      do i=1,length
         itface(i)=0
         ifface(i)=0
      enddo
      do it=1,nelements
         do i=1,nelmnef(itettyp)
            ifsum=0
            do j=1,ielmface0(i,itettyp)
               j1=itet1(nen*(it-1)+ielmface1(j,i,itettyp))
               ifsum=ifsum+j1
            enddo
            nedge_bin(ifsum)=nedge_bin(ifsum)+1
            itface(nedge_off(ifsum)+nedge_bin(ifsum))=it
            ifface(nedge_off(ifsum)+nedge_bin(ifsum))=i
         enddo
      enddo
      do it=1,nelements
         do i=1,nelmnef(itettyp)
            nodemax1=0
            nodemin1=nnodes+1
            nodesum1=0
            do j=1,ielmface0(i,itettyp)
               j1=itet1(nen*(it-1)+ielmface1(j,i,itettyp))
               nodemax1=max(nodemax1,j1)
               nodemin1=min(nodemin1,j1)
               nodesum1=nodesum1+j1
            enddo
            nodemax2=0
            do j=1,ielmface0(i,itettyp)
               j1=itet1(nen*(it-1)+ielmface1(j,i,itettyp))
               if(j1.lt.nodemax1) nodemax2=max(nodemax2,j1)
            enddo
            if(nedge_bin(nodesum1).eq.1) then
               jtet1(nef*(it-1)+i)=0
               jtet2(nef*(it-1)+i)=0
            else
               nodesum2=nodemax1 - nodesum1
               nodesum3=nodemin1 - nodesum1
               do j=1,nedge_bin(nodesum1)
                  jt=itface(nedge_off(nodesum1)+j)
                  jf=ifface(nedge_off(nodesum1)+j)
                  if(it.eq.jt.and.i.eq.jf) then
                  else
                     modemax1=0
                     modemin1=nnodes+1
                     modesum1=0
                     modemax2=1
                     do k=1,ielmface0(jf,jtettyp)
                        j1=itet1(nen*(jt-1)+ielmface1(k,jf,jtettyp))
                        modemax1=max(modemax1,j1)
                        modemin1=min(modemin1,j1)
                        modesum1=modesum1+j1
                     enddo
                     modemax2=0
                     do k=1,ielmface0(jf,jtettyp)
                        j1=itet1(nen*(jt-1)+ielmface1(k,jf,jtettyp))
                        if(j1.lt.modemax1) modemax2=max(modemax2,j1)
                     enddo
                     modesum2=modemax1 - modesum1
                     modesum3=modemin1 - modesum1
                     if((nodesum1-modesum1.eq.0).and.
     *                  (nodesum2-modesum2.eq.0).and.
     *                  (nodesum3-modesum3.eq.0).and.
     *                  (nodemax2-modemax2.eq.0) ) then
                        jtet1(nef*(it-1)+ i)=jt
                        jtet2(nef*(it-1)+ i)=jf
                        jtet1(nef*(jt-1)+jf)=it
                        jtet2(nef*(jt-1)+jf)= i
                        goto 100
                     endif
                  endif
               enddo
               jtet1(nef*(it-1)+i)=0
               jtet2(nef*(it-1)+i)=0
 100           continue
            endif
         enddo
      enddo
C
C***** call tettestd()
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
C ######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C ######################################################################
*dk,geniee_cmo
C
      subroutine geniee_cmo(cmo)
C
C #####################################################################
C
C     PURPOSE -
C
C        Generate neighbor connectivity for an arbitary mesh
C       The itet attribute on input can now have child points,
C       and now works for networks,
C       and now sets lower_d_flag to indicate lower d structure
C       probably not valid (since geniee will be called anytime
C       mesh changed)
C
C     INPUT ARGUMENTS -
C
C        cmo   name of mesh object
C
C     OUTPUT ARGUMENTS -
C
C        None  (correct neighbor connectivity will be in jtet atribute)
C              (and lower_d_flag attribute reset if exists to indicate
C               lower d structures are no longer valid as call to geniee
C               means mesh most likely changed)
C
C     CHANGE HISTORY -
C
C       Modified from geniee_cmo ver 1.13 to work for jtet chains (networks)
C       in a mesh with parents, and "main work" now in sub_geniee_cmo_lg
C       (so same code can be used for lower d structures within mesh as for
C        "top level" of mesh)
 
C ######################################################################
 
      implicit none
 
      character*(*) cmo
 
      pointer (ipitetclr, itetclr )
      integer itetclr(*)
      pointer (ipitettyp, itettyp )
      integer itettyp(*)
      pointer (ipitetoff, itetoff )
      integer itetoff(*)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(*)
      pointer (ipitet, itet )
      integer itet(*)
      pointer (ipjtet, jtet )
      integer jtet(*)
 
      pointer (ipitp1, itp1 )
      integer itp1(*)
      pointer (ipisn1, isn1 )
      integer isn1(*)
      pointer (ipiparent, iparent )
      integer iparent(*)
 
      integer nnodes ,nelements ,faces_per_element ,mbndry
 
      integer i, local_debug ,ilen ,ityp ,ierr ,is_old
     &         ,jtet_cycle_max, jtet_reduce_nnd
 
      character*32 isubname
      character*132 cbuf
 
      integer imsgin(11),msgtype(11),nwds
      real*8 xmsgin(11)
      character*40 cmsgin(11)
 
c-----------------------------------------------------------------------
c23456789012345678901234567890123456789012345678901234567890123456789012
c ........................................................................
 
      local_debug=0
      jtet_cycle_max=0
      jtet_reduce_nnd=0
      isubname='geniee_cmo'
 
      if (local_debug.gt.0) call mmverify()
 
c........ (get info) ..................
 
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
         if (nelements.le.0) goto 9000
      call cmo_get_info('faces_per_element',cmo,faces_per_element
     &                  ,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
         if (faces_per_element.le.0) goto 9000
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ierr)
         if(ierr.ne.0) mbndry=0
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
 
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
         if(ierr.ne.0 .or. nnodes.le.0) goto 9999
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      call mmggetbk('iparent',isubname,ipiparent,nnodes,1,ierr)
         if(ierr.ne.0) goto 9999
      call unpackpc(nnodes,itp1,isn1,iparent)
 
      call cmo_get_info('jtet_reduce_nnd',cmo,jtet_reduce_nnd
     &                  ,ilen,ityp,ierr)
      if (ierr.ne.0) jtet_reduce_nnd=0
 
c........ (do the actual work) ..................
 
      call sub_geniee_cmo_lg(
     &      jtet_reduce_nnd,nelements,faces_per_element,mbndry
     &     ,ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,ipiparent
     &     ,ipjtet,jtet_cycle_max,ierr)
         if(ierr.ne.0) goto 9999
 
c........ (set jtet_cycle_max flag) ..................

 
      call cmo_get_info('jtet_cycle_max',cmo,is_old,ilen,ityp,ierr)

      if (ierr.ne.0 .and. jtet_cycle_max.gt.2) then
 
         nwds=11
         imsgin(11)=0
         xmsgin(11)=0.
         msgtype(11)=1
         do i=1,10
            msgtype(i)=3
         enddo
         cmsgin(1)='cmo'
         cmsgin(2)='addatt'
         cmsgin(3)=cmo
         if (cmo(1:1).eq.'-') then
            call cmo_get_name(cmsgin(3),ierr)
         endif
         cmsgin(4)='jtet_cycle_max'
         cmsgin(5)='INT'
         cmsgin(6)='scalar'
         cmsgin(7)='scalar'
         cmsgin(8)='constant'
         cmsgin(9)='temporary'
         cmsgin(10)='x'
         call cmo_addatt(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
         if(ierr.ne.0) goto 9999
         ilen=1
         ityp=1
         call cmo_set_info('jtet_cycle_max',cmo,jtet_cycle_max
     &                        ,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
 
      elseif (ierr.eq.0 .and. jtet_cycle_max.ne.is_old) then
 
         ilen=1
         ityp=1
         call cmo_set_info('jtet_cycle_max',cmo,jtet_cycle_max
     &                      ,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
 
      endif
 
c........ (test) ..................
 
      ! no: tested in sub_geniee_cmo_lg
      ! call elmtestd(cmo,20,ierr)
      ! if(ierr.ne.0) goto 9999
 
c........ (reset lower_d_flag if exists) ..................
c lower_d_flag=1 means structures valid
c lower_d_flag=2 means structures no longer valid
c (any other lower_d_flag equivalent to no lower d structures)
 
      call cmo_get_info('lower_d_flag',cmo,is_old,ilen,ityp,ierr)
      if ( ierr.eq.0 .and. is_old.eq.1 ) then
         ilen=1
         ityp=1
         is_old=2
         call cmo_set_info('lower_d_flag',cmo,is_old,ilen,ityp,ierr)
         if(ierr.ne.0) goto 9999
      endif
 
c........ (successful return) ..................
 
 9000 continue
      if (nelements.le.0 .or. faces_per_element.le.0) then
         cbuf='geniee_cmo warning: no elements with faces so no jtet'
         call writloga('default',0,cbuf,0,ierr)
      endif
      if (local_debug.gt.0) call mmverify()
      call mmrelprt(isubname,ierr)
      return
 
c........ (failure return) ..................
 
9999  continue
      if (local_debug.gt.0) call mmverify()
      cbuf='ERROR IN ROUTine geniee_cmo: ABORTING'
      !if (local_debug.gt.0) then
         call writloga('default',0,cbuf,0,ierr)
      !else
      !   ! this screws up for some reason....
      !   call x3d_error(isubname,cbuf)
      !endif
      call mmrelprt(isubname,ierr)
      return
 
      end
 
C ######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C ######################################################################
*dk,sub_geniee_cmo_lg
C
      subroutine sub_geniee_cmo_lg(
     &      jtet_reduce_nnd,nelements,faces_per_element,mbndry
     &     ,ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,ipiparent
     &     ,ipjtet,jtet_cycle_max,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C       Generate neighbor connectivity for an arbitary mesh.
C       This version has the arguments passed so it can be used with
C       the lower_d structures within a mesh.
C
C     INPUT ARGUMENTS -
C
C     jtet_reduce_nnd -
C        flag to indicate if (.eq.1) faces with different nnd touch
C                         or (.ne.1) not
C        if this routine is called from geniee_cmo, then:
C             if jtet_reduce_nnd exists in the cmo, geniee_cmo passes it
C             otherwise geniee_cmo passed jtet_reduce_nnd=0
C     standard variables:
C        nelements, faces_per_element, mbndry
C     standard pointers for already assigned arrays:
C        ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,ipiparent
C     standard pointers for array to assign:
C        ipjet
C
C     OUTPUT ARGUMENTS -
C
C        (jet will contain neighbor connectivity)
C        jtet_cycle_max - 2 if not network,
C                     >2 if network (any jtet cycle longer than 2)
C
C     PROGRAMMING NOTES -
C
C       Pointers are passed as may not be the main mesh.
C       The parent translation is passed, so itet can refer to children.
C
C       The connectivity is determined by summing the nodes for each face
C       and binning all faces with a given sum in "nedge_bin".
C       The separate bins are then sifted to find the matching faces.
C       Binning significantly reduces the search time.
C
C       The conventions:
C       Define [ifc,iel] as a shorthand to refer to face ifc of element iel.
C
C       (1) mbndry=0 vs mbndry>nelements*faces_per_element:
C       Both the old convention of mbndry>nelements*faces_per_element,
C       where jtet=mbndry signals exterior boundaries and jtet>mbndry
C       signals interior interfaces, and the to-be-implemented mbndry=0
C       convention, where jtet=0 signals exterior boundaries and jtet<0
C       signals interior interfaces, are coded. However, often only
C       the old convention is used for the descriptive words.
C
C       (2) "special" boundary values are not allowed:
C       As there is no way to pack, eg, icr1 into interior boundaries
C       (although it could be coded into exterior boundaries),
C       no "special" or "reserved" values of jtet are coded.
C       It is suggested that if needed, e.g. to distinguish surfaces
C       which move with Dirichlet vs Neuman boundary conditions,
C       that the data structures of lower_d_lg are used, as for the
C       lower_d elements the itetclr is a packed combination of
C       the icr1, itp1, and imt1 values for a given surface.
C       Alternatively the icr1 decoding into a surface constraint
C       can be done "by hand", e.g. copying the coding in lower_d_lg.
C
C       (3) networks are allowed:
C       For a given list of unique parent nodes,
C       - if only a single face is found:
C             jtet[ifc,iel]=mbndry
C       - if two faces are found:
C             jtet[ifc1,iel1]=[ifc2,iel2]
C                            +mbndry*kronecker(itetclr(iel1),itetclr(iel2))
C             jtet[ifc2,iel2]=[ifc1,iel1]
C                            +mbndry*kronecker(itetclr(iel1),itetclr(iel2))
C         where kronecker=1 if the two arguments are the same, 0 otherwise.
C         This is also referred to here as a "jtet loop of length 2".
C       - if three or more faces are found:
C             jtet[ifc(1),iel(1)]=[ifc(2,iel(2)]+mbndry
C             jtet[ifc(2),iel(2)]=[ifc(3,iel(3)]+mbndry
C                  ....
C             jtet[ifc(n),iel(n)]=[ifc(1),iel(1)]+mbndry
C         This is also referred to here as a "jtet loop of length n".
C         If more than 2 faces occur in the loop (and the loop
C         cannot be broken re degeneracies; see below), mbndry
C         is always added.
C
C       On output, jtet_cycle_max>2 if jtet loops of length longer
C       than 2 occur signalling that the mesh is a network.
C       Otherwise jtet_cycle_max=2 signalling the mesh is not a network.
C       Note most routines which use jtet currently are not coded
C       for networks (jtet loops of length longer than 2 are invalid
C       connections as far as these routines are concerned).
C
C       (4) IF jtet_reduce_nnd=1 (jtet_reduce_nnd passed)
C       Faces with different numbers of nodes can touch:
C       Faces with a repeated parent nodes (self-degenerate faces)
C       are considered equivalent to a face with the reduced set
C       of unique parent nodes.
C       E.g., a quad face with nodes 1,1,2,2 would be connected by jtet
C       to a tri face with nodes 1,2,2 or a lin face with nodes 1,2.
C       This can be used to connect pieces of the mesh with
C       different topological dimension if desired.
C
C       (4') IF jtet_reduce_nnd.ne.1 (jtet_reduce_nnd passed)
C       Faces with different numbers of nodes can not touch.
C       (This is the old LaGriT convention.)
C
C       (5) elements can touch themselves:
C       Faces on the same element which each have the same list of unique
C       parent nodes (degenerate faces) are considered connected.
C
C --- the following is suggested for a future revision
C     but not coded -----------------------------------------------------
C
C       (5') ..., unless
C       this connection can be broken in an unambiguous fashion:
C       a test is made to see if this degeneracy can be broken by using
C       the order in which the nodes occur in itet (the "direction" of
C       the face) to break the degeneracy. If so, jtet is modified
C       so the 2 faces "do not touch" (are not connected by jtet).
C       Otherwise the mesh is left as a network and the degenerate
C       faces "touch each other" (are connected by jtet).
C
C       E.g., consider the 3 following tri elements
C       element 1: iparent(itet(1:3,1))=1,2,3
C                  iparent(itet(ielmface1(1:2,1,ifelmtri),1))=2,3
C       element 2: iparent(itet(1:3,2))=2,2,3
C                  iparent(itet(ielmface1(1:2,1,ifelmtri),1))=2,3
C                  iparent(itet(ielmface3(1:2,2,ifelmtri),1))=3,2
C       element 3: iparent(itet(1:3,3))=2,4,3
C                  iparent(itet(ielmface3(1:2,2,ifelmtri),1))=3,2
C       Assume no other faces consist of just nodes 2,3.
C       Thus without considering the itet order, the code
C       would connect [1,1], [1,2], [2,2], and [2,3] using the
C       network jtet-loop conventions.
C       In a consistently ordered non-network mesh, faces always only
C       touch faces with the opposite order. The assumption is made
C       here that this is locally true, thus the code breaks this
C       jtet 4-loop into two 2-loops connecting [1,1] with [2,2]
C       and [1,2] with [2,3].
C       If instead the itet order of element 3 were inverted, or more
C       than 4 faces all had the same unique parent node list,
C       then the 4-loop would remain and not get broken.
C       If instead no element 3 were connected, then the 3-loop
C       connecting [1,1], [1,2], [2,2] would be broken into a 2-loop
C       connecting [1,1] with [2,2] and the boundary face [1,2].
C       In other words, itet is used to artificially "flatten"
C       degenerate elements to create non-network jtet where possible.
C
C --- end suggestion ------------------------------------------------
C
C     CHANGE HISTORY -
C
C       Modified from geniee_cmo ver 1.13 to work for jtet chains (networks)
C       in a mesh with parents, and can be called for the lower d structures
C       within a mesh
C
C ######################################################################
 
      implicit none
C
      include "local_element.h"
C
      integer nelements,faces_per_element,mbndry,jtet_cycle_max,ierror
      pointer (ipitetclr, itetclr )
      integer itetclr(*)
      pointer (ipitettyp, itettyp )
      integer itettyp(*)
      pointer (ipitetoff, itetoff )
      integer itetoff(*)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(*)
      pointer (ipitet, itet )
      integer itet(*)
      pointer (ipjtet, jtet )
      integer jtet(*)
      pointer (ipiparent, iparent )
      integer iparent(*)
 
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      integer nedge_bin(*), nedge_off(*)
      pointer (ipitface, itface)
      integer itface(*)
 
      character*32 isubname
      character*132 cbuf
 
      integer local_debug,i,j,k,j1,ioff,joff,koff,it,jt,jf,kt,kf,ierr
     &       ,ifsum,ityp,isum,ifsummax,nef,length,j2,nnd,isort
     &       ,n_degen,n_self_degen,icycle,jcycle,kcycle,n_cycles
     &       ,ksdegen,isdegen,jsdegen,nnd2,jtet_reduce_nnd,n_diff_nnd
     &       ,irepeat(2*maxnen),loc_nodes(2*maxnen),nwrite
 
c-----------------------------------------------------------------------
c23456789012345678901234567890123456789012345678901234567890123456789012
c ........................................................................
 
      local_debug=0
      ierror=0
      jtet_cycle_max=0
 
      if (local_debug.gt.0) call mmverify()
 
      isubname='sub_geniee_cmo_lg'
 
c........ (do trivial cases) ..................
 
      if (nelements.lt.1.or.faces_per_element.lt.1) goto 9998
 
c assume elements "flat": two faces of same element cannot touch
c -> for nelements=1, jtet==mbndry
c Note: code should work even if two faces of same element touch -> keep this?
      if (nelements.eq.1) then
         it=1
         ityp=itettyp(it)
         ioff=itetoff(it)
         joff=jtetoff(it)
         do i=1,nelmnef(ityp)
            jtet(i)=mbndry
         enddo
         goto 9000
      endif
 
c........ (get and initialize temporary storage) ..................
 
      isum=0
      ifsummax=0
      do it=1,nelements
         ityp=itettyp(it)
         nef=nelmnef(ityp)
         isum=isum+nef
         ioff=itetoff(it)
         joff=jtetoff(it)
         if (it.ne.nelements) then
            ! check that jtetoff allowable
            if (joff+nef.gt.jtetoff(it+1)) goto 9999
         endif
         do i=1,nef
            nnd=ielmface0(i,ityp)
            if (jtet_reduce_nnd.eq.1) then
               do j=1,nnd
                  loc_nodes(j)=iparent(itet(ioff+ielmface1(j,i,ityp)))
               enddo
100            isort=0
               do j=1,nnd-1
                  if (loc_nodes(j+1).lt.loc_nodes(j)) then
                     k=loc_nodes(j)
                     loc_nodes(j)=loc_nodes(j+1)
                     loc_nodes(j+1)=k
                     isort=1
                  endif
               enddo
               if (isort.ne.0) goto 100
               ifsum=loc_nodes(1)
               do j=2,nnd
                  if (loc_nodes(j).ne.loc_nodes(j-1))
     &                            ifsum=ifsum+loc_nodes(j)
               enddo
            else
               ifsum=0
               do j=1,nnd
                  ifsum=ifsum+iparent(itet(ioff+ielmface1(j,i,ityp)))
               enddo
            endif
            ifsummax=max(ifsummax,ifsum)
            ! for now: store ifsum in jtet so don't have to recalc
            jtet(joff+i)=ifsum
         enddo
      enddo
 
      length=max(ifsummax,nelements)
      call mmggetbk('nedge_bin',isubname,
     &              ipnedge_bin,length,1,ierr)
      if (ierr.ne.0) goto 9999
      length=ifsummax+1
      call mmggetbk('nedge_off',isubname,
     &              ipnedge_off,length,1,ierr)
      if (ierr.ne.0) goto 9999
      ! use packed itface to minimize temporary storage needed
      call mmggetbk('itface',isubname,
     &              ipitface,isum,1,ierr)
      if (ierr.ne.0) goto 9999
 
c........ (set the bin offset) ..................
C  nedge_bin contains a count of number of elements that share a facet
C  indexed by the sum of the node numbers of the facet
C  Since this sum is not unique they different facets will be sorted
C  out later
C  nedge_off is the offset into the itface array by element
C
      do i=1,ifsummax
         nedge_off(i)=0
         nedge_bin(i)=0
      enddo
 
      do it=1,nelements
         ityp=itettyp(it)
         ioff=itetoff(it)
         joff=jtetoff(it)
         do i=1,nelmnef(ityp)
            ifsum=jtet(joff+i)
            nedge_off(ifsum)=nedge_off(ifsum)+1
         enddo
      enddo
 
      isum=0
      do i=1,ifsummax
         j=nedge_off(i)
         nedge_off(i)=isum
         isum=isum+j
      enddo
      nedge_off(ifsummax+1)=isum
 
      do i=1,isum
         itface(i)=0
      enddo
 
c........ (fill the nedge_bin,itface arrays, initialize jtet) ..................
 
      do it=1,nelements
         ityp=itettyp(it)
         joff=jtetoff(it)
         do i=1,nelmnef(ityp)
            ifsum=jtet(joff+i)
            nedge_bin(ifsum)=nedge_bin(ifsum)+1
            itface(nedge_off(ifsum)+nedge_bin(ifsum))
     &           =(it-1)*faces_per_element+i
            jtet(joff+i)=mbndry
         enddo
      enddo
 
c........ (set jtet and jtet_cycle_max) ..................
c rewritten so can handle jtet cycles possibly longer than 2,
c (final coding similar to icontab checks)
 
      if (local_debug.ne.0) call mmverify()
 
      ! jtet_reduce_nnd: if =1 allow diff nnd to touch; otherwise do not allow
 
      jtet_cycle_max=2
      n_cycles=0
      n_self_degen=0
      n_diff_nnd=0
      n_degen=0
 
      j2=0
      do ifsum=1,ifsummax
         j1=j2+1
         j2=nedge_off(ifsum+1)
200      if (j2-j1.ge.1) then
            jf=itface(j1)
            jt=1+(jf-1)/faces_per_element
            jf=jf-(jt-1)*faces_per_element
            joff=jtetoff(jt)+jf
            if (jtet(joff).ne.mbndry) then
               j1=j1+1
               goto 200
            endif
            ityp=itettyp(jt)
            ioff=itetoff(jt)
            nnd=ielmface0(jf,ityp)
            do j=1,nnd
               loc_nodes(j)=iparent(itet(ioff+ielmface1(j,jf,ityp)))
            enddo
300         isort=0
            do j=1,nnd-1
               if (loc_nodes(j+1).lt.loc_nodes(j)) then
                  i=loc_nodes(j)
                  loc_nodes(j)=loc_nodes(j+1)
                  loc_nodes(j+1)=i
                  isort=1
               endif
            enddo
            if (isort.ne.0) goto 300
            ! test if current face is self-degenerate (repeated nodes)
            do j=1,nnd
               irepeat(j)=1
            enddo
            isdegen=0

C           Begin loop (I wish Fortran had while loops...)
            j=1
302         if (j .lt. nnd) then
               if (loc_nodes(j+1).eq.loc_nodes(j)) then
                  isdegen=isdegen+1
                  if (jtet_reduce_nnd.eq.1) then
                     nnd=nnd-1
                     irepeat(j)=irepeat(j)+1
                     do isort=j+1,nnd
                        loc_nodes(isort)=loc_nodes(isort+1)
                     enddo
                  else
                      j = j + 1
                  endif
               else
                   j = j + 1
               endif
               goto 302
            endif
C           End loop

            if (isdegen.gt.0) n_self_degen=n_self_degen+1
            icycle=1
            ksdegen=0
            jcycle=1
            nedge_bin(1)=jt
            do j=j1+1,j2
               kf=itface(j)
               kt=1+(kf-1)/faces_per_element
               kf=kf-(kt-1)*faces_per_element
               ityp=itettyp(kt)
               koff=jtetoff(kt)+kf
               nnd2=ielmface0(kf,ityp)
               if (jtet(koff).ne.mbndry .or. nnd2.lt.nnd
     &             .or.(nnd2.ne.nnd.and.jtet_reduce_nnd.ne.1)) goto 500
               ioff=itetoff(kt)
               do k=1,nnd2
                  loc_nodes(nnd+k)
     &                  =iparent(itet(ioff+ielmface1(k,kf,ityp)))
               enddo
400            isort=0
               do k=nnd+1,nnd+nnd2-1
                  if (loc_nodes(k+1).lt.loc_nodes(k)) then
                     i=loc_nodes(k)
                     loc_nodes(k)=loc_nodes(k+1)
                     loc_nodes(k+1)=i
                     isort=1
                  endif
               enddo
               if (isort.ne.0) goto 400
               do k=nnd+1,nnd+nnd2
                  irepeat(k)=1
               enddo
               if (jtet_reduce_nnd.eq.1) then
                  jsdegen=0
                  k=nnd+1
402               if (loc_nodes(k+1).eq.loc_nodes(k)) then
                     jsdegen=jsdegen+1
                     nnd2=nnd2-1
                     irepeat(k)=irepeat(k)+1
                     do isort=k+1,nnd+nnd2
                        loc_nodes(isort)=loc_nodes(isort+1)
                     enddo
                     if (k.lt.nnd+nnd2) goto 402
                  endif
                  k=k+1
                  if (k.lt.nnd+nnd2.and.nnd2.ge.nnd) goto 402
                  if (nnd2.ne.nnd) goto 500
               else
                  jsdegen=isdegen
               endif
               ! test if this face has same nodes as current face of interest
               do k=1,nnd
                  if (loc_nodes(k).ne.loc_nodes(nnd+k)) goto 500
               enddo
               ! if get here, nodes are same -> link faces with jtet
               if (jsdegen.gt.0) n_self_degen=n_self_degen+1
               ! test that degeneracy same (must always be if jtet_reduce_nnd.ne.1)
               if (isdegen.ne.jsdegen) then
                  ksdegen=1
               elseif (isdegen.ne.0) then
                  do k=1,nnd
                     if (irepeat(k).ne.irepeat(k+nnd)) ksdegen=1
                  enddo
               endif
               icycle=icycle+1
               if (jcycle.ne.0) then
                  do kcycle=1,jcycle
                     if (nedge_bin(kcycle).eq.kt) then
                        jcycle=0
                        goto 403
                     endif
                  enddo
                  jcycle=jcycle+1
                  nedge_bin(jcycle)=kt
               endif
403            if (jtet(joff).eq.mbndry) then
                  if (itetclr(kt).ne.itetclr(jt).or.ksdegen.ne.0) then
                     if (mbndry.eq.0) then
                        jtet(joff)=-kf-(kt-1)*faces_per_element
                        jtet(koff)=-jf-(jt-1)*faces_per_element
                     else
                        jtet(joff)=kf+(kt-1)*faces_per_element+mbndry
                        jtet(koff)=jf+(jt-1)*faces_per_element+mbndry
                     endif
                  else
                     jtet(joff)=kf+(kt-1)*faces_per_element
                     jtet(koff)=jf+(jt-1)*faces_per_element
                  endif
               else
                  i=jtet(joff)
                  if (mbndry.eq.0) then
                     if (i.gt.0) then
                        it=1+(i-1)/faces_per_element
                        k=i-(it-1)*faces_per_element+jtetoff(it)
                        jtet(k)=-jtet(k)
                        i=-i
                     else
                        it=1+(-i-1)/faces_per_element
                     endif
                     jtet(joff)=-kf-(kt-1)*faces_per_element
                     jtet(koff)=i
                  else
                     if (i.lt.mbndry) then
                        it=1+(i-1)/faces_per_element
                        k=i-(it-1)*faces_per_element+jtetoff(it)
                        jtet(k)=jtet(k)+mbndry
                        i=i+mbndry
                     else
                        it=1+(i-mbndry-1)/faces_per_element
                     endif
                     jtet(joff)=kf+(kt-1)*faces_per_element+mbndry
                     jtet(koff)=i
                  endif
               endif
500            continue
            enddo
            if (icycle.gt.jtet_cycle_max) jtet_cycle_max=icycle
            if (icycle.gt.2) n_cycles=n_cycles+1
            if (ksdegen.ne.0) n_diff_nnd=n_diff_nnd+1
            ! test cycle for degenerate elements
            if (jcycle.ne.icycle) n_degen=n_degen+1
505         j1=j1+1
            goto 200
         endif
      enddo
      if (jtet_cycle_max.gt.2 .or. local_debug.ne.0) then
         write(cbuf,*)
     &    'geniee: mesh has ',n_cycles,' jtet loops, max cycle length='
     &    ,jtet_cycle_max
         call writloga('default',0,cbuf,0,ierr)
      endif
      if (n_self_degen.gt.0.or.local_debug.ne.0) then
         write(cbuf,*)
     &    'geniee: mesh has ',n_self_degen,' self-degenerate faces'
         call writloga('default',0,cbuf,0,ierr)
      endif
      if (n_diff_nnd.gt.0 .or. local_debug.ne.0) then
         write(cbuf,*)
     &    'geniee: mesh has ',n_diff_nnd,' jtet loops linking faces'
     &     //' with diff no.s of nodes'
         call writloga('default',0,cbuf,0,ierr)
         write(cbuf,*) '        or diff repeated nodes'
         call writloga('default',0,cbuf,0,ierr)
      endif
      if (n_degen.gt.0.or.local_debug.ne.0) then
         write(cbuf,*)
     &    'geniee: mesh has ',n_degen,' jtet loops linking faces'
     &     //' within the same element'
         call writloga('default',0,cbuf,0,ierr)
      endif
 
c........ (test) ..................
      if (local_debug.gt.0) call mmverify()
 
      nwrite=20
      call sub_elmtestd_lg(mbndry,nelements,faces_per_element
     &     ,ipitettyp,ipjtetoff,ipjtet
     &     ,ipitetoff,ipitet,ipiparent
     &     ,jtet_cycle_max,jtet_reduce_nnd
     &     ,local_debug,nwrite,ierr)
      if (ierr.ne.0) goto 9999
 
c........ (successful return) ..................
 
 9000 continue
      call mmrelprt(isubname,ierr)
      if (local_debug.gt.0) call mmverify()
      ierror=0
      return
 
c........ (failure return) ..................
 
9999  continue
      call mmrelprt(isubname,ierr)
      ierror=1
9998  if (local_debug.gt.0) call mmverify()
      if (nelements.gt.0 .and. faces_per_element.gt.0) then
         cbuf='ERROR IN ROUTine sub_geniee_cmo_lg: ABORTING'
      else
         cbuf='sub_geniee_cmo_lg warning: no elements with faces'
      endif
      call writloga('default',0,cbuf,0,ierr)
      return
 
      end
 
C ######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C ######################################################################
*dk,geniee_slow
C
      subroutine geniee_slow (ien,iee1,iee2,nen,nef,numel,numnp,
     &                        nsd, maxnode, maxelm)
C
C #####################################################################
C
C     PURPOSE -
C
c  This subroutine generates the element connectivity array iee
c  from the nodal connectivity array ien.
c
c  CAUTION: Works only for quads, triangles, tetrahedra, wedges
c           and bricks.
c  CAUTION: geniee_cmo better in (almost?) all cases
c
C     INPUT ARGUMENTS -
C
c  ien(nen,numel)	: nodal connectivity
c  nen			: number of element nodes
c  nef		: number of element faces
c  numel		: number of elements
c  numnp		: number of nodes
c  nsd			: number of space dimensions
c
C     OUTPUT ARGUMENTS -
C
c  IEE(nef,numel)	: element connectivity
c
C     CHANGE HISTORY -
C
C        $Log: geniee.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.4   06/27/95 11:10:54   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.3   06/07/95 06:21:32   het
CPVCS    Add more local_element.h references
CPVCS
CPVCS       Rev 1.2   02/10/95 08:27:24   het
CPVCS    Change the nfaces variable to nface because of cmo.h
CPVCS
CPVCS       Rev 1.1   01/04/95 22:02:30   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:20   pvcs
CPVCS    Original version.
C
c Zdenek Johan, Summer 1991.
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
c
c----------------------------------------------------------------------
c
C*****	parameter (npmax = 16000, maxnbrs=30)
	parameter (maxnbrs_guess=30)
c
c*****dimensionien(nen,*),     iee(nef,*)
      integer ien(nen,maxelm)
      integer iee1(nef,maxelm), iee2(nef,maxelm)
CMF$  LAYOUT ien(:serial,:news)
CMF$  LAYOUT IEE1(:serial,:news)
CMF$  LAYOUT IEE2(:serial,:news)
C
      integer list1(maxnbrs_guess), list2(maxnbrs_guess),
     &        list3(maxnbrs_guess), list4(maxnbrs_guess)
       pointer (iplist1, list1)
       pointer (iplist2, list2)
       pointer (iplist3, list3)
       pointer (iplist4, list4)
CMF$    layout list1(:serial)
CMF$    layout list2(:serial)
CMF$    layout list3(:serial)
CMF$    layout list4(:serial)
C   c
	dimension nelint(2),
     &            ip(8),          iq1(4),              iq2(4),
     &            ipw1(3),        ipw2(3)
C
C*****	dimension nne(npmax),     ine(maxnbrs,npmax)
        pointer (ipnne, nne(numnp))
        pointer (ipine, ine(maxnbrs_guess*numnp))
CMF$  LAYOUT nne(:news)
CMF$  LAYOUT ine(:serial,:news)
c
C
      parameter (nenlin=2, nfacelin=2)
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      dimension ihexface0(nfacehex), ihexface1(4,nfacehex)
      dimension iprismface0(nfaceprism), iprismface1(4,nfaceprism)
      integer intpairhex(2,12)
      dimension itetface0(nfacetet), itetface1(4,nfacetet)
      dimension itriface0(nfacetri), itriface1(4,nfacetri)
      dimension ilinface0(nfacelin), ilinface1(4,nfacelin)
      integer intpairtet(2,6)
      character*132 logmess
      character*8 isubname
	data      iq1  /  2, 3, 4, 1  /
	data      iq2  /  6, 7, 8, 5  /
	data      ipw1 /  2, 3, 1     /
	data      ipw2 /  5, 6, 4     /
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
C
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 0, 0,
     *                 3, 1, 0, 0,
     *                 1, 2, 0, 0 /
C
C     top,back,left,right
      data ilinface0 / 1, 1 /
      data ilinface1 / 2, 1, 0, 0,
     *                 1, 2, 0, 0 /
 
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C
C     DEFine A STATEMENT FUNCTION TO MAP THE NEIGHBOR LIST
        ine_FUNC(i,j) = ine(i+(j-1)*maxnbrs)
C
      isubname='geniee_slow'
C
C
        length=numnp
        call mmgetblk('nne',isubname,ipnne,length,2,icscode)
c
c.... generate the permutation array
c
        do 10 i = 1, nen-1
          ip(i) = i+1
10      continue
        ip(nen) = 1
c
c.... generate the list of elements connected to a node
c
	do 100 n = 1, numnp
	  nne(n) = 0
100	continue
c
	do 300 nel = 1, numel
	  do 200 i = 1, nen
	    node = ien(i,nel)
	    nne(node) = nne(node) + 1
C*****	    ine(nne(node),node) = nel
200	  continue
 300   continue
       maxnbrs=0
       do i=1,numnp
          maxnbrs=max(maxnbrs,nne(i))
       enddo
       write(logmess,'(a,4i10)') 'GENIEE: ', numnp,numel,maxnbrs
       call writloga('default',0,logmess,0,ierrw)
       length=maxnbrs
       call mmgetblk('list1',isubname,iplist1,length,2,icscode)
       call mmgetblk('list2',isubname,iplist2,length,2,icscode)
       call mmgetblk('list3',isubname,iplist3,length,2,icscode)
       call mmgetblk('list4',isubname,iplist4,length,2,icscode)
       length=maxnbrs*numnp
       call mmgetblk('ine',isubname,ipine,length,2,icscode)
c
	do n = 1, numnp
	  nne(n) = 0
        enddo
	do 301 nel = 1, numel
	  do 201 i = 1, nen
	    node = ien(i,nel)
            nne(node) = nne(node) + 1
            ine(nne(node)+(node-1)*maxnbrs)=nel
 201     continue
 301  continue
c
	do 400 n = 1, numnp
	  if (nne(n) .gt. maxnbrs) then
	    write (*,'(a,i10,a)') 'nne > ',maxnbrs,'  !!!!!'
	    stop
	  endif
400	continue
c
c.... generate the list of elements connected to an element
c
c.... one-dimension line
c
	if (nsd .eq. 1) then
	  do 610 nel = 1, numel
	    do 510 i = 1, nen
 	      node1 = ien(ilinface1(1,i),nel)
              num1=nne(node1)
              do j=1,2
                 nelint(j)=ine(j+maxnbrs*(node1-1))
              enddo
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
              if(iee1(i,nel).gt.0) then
                 do j=1,nfacelin
    	            m1 = ien(ilinface1(1,j),iee1(i,nel))
                    if(m1.eq.node1) then
                       iee2(i,nel)=j
                    endif
                 enddo
              else
                 iee2(i,nel)=0
              endif
510	    continue
610	  continue
	endif
c
c.... triangle and quadrilateral
c
	if (nsd .eq. 2) then
	  do 600 nel = 1, numel
	    do 500 i = 1, nen
 	      node1 = ien(itriface1(1,i),nel)
	      node2 = ien(itriface1(2,i),nel)
	      if (node1 .eq. node2) goto 500
              num1=nne(node1)
              do j=1,num1
                 list1(j)=ine(j+maxnbrs*(node1-1))
              enddo
              num2=nne(node2)
              do j=1,num2
                 list2(j)=ine(j+maxnbrs*(node2-1))
              enddo
	      call inter2 (list1,  num1,
     &                     list2,  num2,
     &                     nelint )
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
              if(iee1(i,nel).gt.0) then
                 nodesum1=node1+node2
                 nodesum2=max(node1,node2) - nodesum1
                 do j=1,nfacetri
    	            m1 = ien(itriface1(1,j),iee1(i,nel))
	            m2 = ien(itriface1(2,j),iee1(i,nel))
                    if((m1.eq.node1.and.m2.eq.node2).or.
     *                 (m2.eq.node1.and.m1.eq.node2)) then
                       iee2(i,nel)=j
                    endif
                    modesum1=m1+m2
                    modesum2=max(m1,m2) - modesum1
                    if((nodesum1-modesum1.eq.0). and.
     *                 (nodesum2-modesum2.eq.0) ) iee2(i,nel)=j
                 enddo
              else
                 iee2(i,nel)=0
              endif
500	    continue
600	  continue
	endif
c
c.... tetrahedron
c
	if ((nen .eq. 4) .and. (nsd .eq. 3)) then
           do 800 nel = 1, numel
              do 700 i = 1, nfacetet
                 if(iee1(i,nel).le.0.or.iee2(i,nel).le.0) then
	      node1 = ien(itetface1(1,i),nel)
	      node2 = ien(itetface1(2,i),nel)
	      node3 = ien(itetface1(3,i),nel)
              num1=nne(node1)
              do j=1,num1
                 list1(j)=ine(j+maxnbrs*(node1-1))
              enddo
              num2=nne(node2)
              do j=1,num2
                 list2(j)=ine(j+maxnbrs*(node2-1))
              enddo
              num3=nne(node3)
              do j=1,num3
                 list3(j)=ine(j+maxnbrs*(node3-1))
              enddo
	      call inter3 (list1,  num1,
     &                     list2,  num2,
     &                     list3,  num3,
     &                     nelint)
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
              if(iee1(i,nel).gt.0) then
                 nodemax1=max(node1,node2,node3)
                 nodemin1=min(node1,node2,node3)
                 nodesum1=node1+node2+node3
                 nodesum2=nodemax1 - nodesum1
                 nodesum3=nodemin1 - nodesum1
                 nodemax2=(node1*node2*node3) /
     *                    (nodemax1*nodemin1)
              do j=1,nfacetet
 	         m1 = ien(itetface1(1,j),iee1(i,nel))
	         m2 = ien(itetface1(2,j),iee1(i,nel))
	         m3 = ien(itetface1(3,j),iee1(i,nel))
                  modemax1=max(m1,m2,m3)
                  modemin1=min(m1,m2,m3)
                  modesum1=m1+m2+m3
                  modesum2=modemax1 - modesum1
                  modesum3=modemin1 - modesum1
                  modemax2=(m1*m2*m3)/(modemax1*modemin1)
                  if((nodesum1-modesum1.eq.0).and.
     *               (nodesum2-modesum2.eq.0).and.
     *               (nodesum3-modesum3.eq.0).and.
     *               (nodemax2-modemax2.eq.0) ) then
                    iee2(i,nel)=j
                  endif
              enddo
              else
                 iee2(i,nel)=0
              endif
           endif
 700       continue
 800    continue
	endif
c
c.... wedge
c
	if ((nen .eq. 6) .and. (nsd .eq. 3)) then
	  do 1100 nel = 1, numel
	    do 900 i = 1, 2
	      node1 = ien(iprismface1(1,i),nel)
	      node2 = ien(iprismface1(2,i),nel)
	      node3 = ien(iprismface1(3,i),nel)
              num1=nne(node1)
              do j=1,num1
                 list1(j)=ine(j+maxnbrs*(node1-1))
              enddo
              num2=nne(node2)
              do j=1,num2
                 list2(j)=ine(j+maxnbrs*(node2-1))
              enddo
              num3=nne(node3)
              do j=1,num3
                 list3(j)=ine(j+maxnbrs*(node3-1))
              enddo
	      call inter3 (list1,  num1,
     &                     list2,  num2,
     &                     list3,  num3,
     &                     nelint)
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
900	    continue
	    do 1000 i = 3, 5
	      node1 = ien(iprismface1(1,i),nel)
	      node2 = ien(iprismface1(2,i),nel)
	      node3 = ien(iprismface1(3,i),nel)
	      node4 = ien(iprismface1(4,i),nel)
              num1=nne(node1)
              do j=1,num1
                 list1(j)=ine(j+maxnbrs*(node1-1))
              enddo
              num2=nne(node2)
              do j=1,num2
                 list2(j)=ine(j+maxnbrs*(node2-1))
              enddo
              num3=nne(node3)
              do j=1,num3
                 list3(j)=ine(j+maxnbrs*(node3-1))
              enddo
              num4=nne(node4)
              do j=1,num4
                 list4(j)=ine(j+maxnbrs*(node4-1))
              enddo
	      call inter4 (list1,  num1,
     &                     list2,  num2,
     &                     list3,  num3,
     &                     list4,  num4,
     &                     nelint)
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
 1000      continue
           do i=1,nfaceprism
	      node1 = ien(iprismface1(1,i),nel)
	      node2 = ien(iprismface1(2,i),nel)
	      node3 = ien(iprismface1(3,i),nel)
              if(iprismface1(4,i).eq.0) then
                 node4=0
              else
      	         node4 = ien(iprismface1(4,i),nel)
              endif
              nodesum=node1+node2+node3+node4
              do j=1,nfaceprism
  	         m1 = ien(iprismface1(1,j),iee1(i,nel))
	         m2 = ien(iprismface1(2,j),iee1(i,nel))
	         m3 = ien(iprismface1(3,j),iee1(i,nel))
                 if(node4.eq.0) then
                    m4=0
                 else
                    m4 = ien(ihexface1(4,j),iee1(i,nel))
                 endif
                 if(nodesum.eq.(m1+m2+m3+m4)) iee2(i,nel)=j
              enddo
           enddo
1100	  continue
	endif
c
c.... brick
c
	if ((nen .eq. 8) .and. (nsd .eq. 3)) then
	  do 1400 nel = 1, numel
	    do 1200 i = 1, nfacehex
	      node1 = ien(ihexface1(1,i),nel)
	      node2 = ien(ihexface1(2,i),nel)
	      node3 = ien(ihexface1(3,i),nel)
	      node4 = ien(ihexface1(4,i),nel)
              num1=nne(node1)
              do idum=1,num1
                 list1(idum)=ine(idum+maxnbrs*(node1-1))
              enddo
              num2=nne(node2)
              do idum=1,num2
                 list2(idum)=ine(idum+maxnbrs*(node2-1))
              enddo
              num3=nne(node3)
              do idum=1,num3
                 list3(idum)=ine(idum+maxnbrs*(node3-1))
              enddo
              num4=nne(node4)
              do idum=1,num4
                 list4(idum)=ine(idum+maxnbrs*(node4-1))
              enddo
	      call inter4 (list1,  num1,
     &                     list2,  num2,
     &                     list3,  num3,
     &                     list4,  num4,
     &                     nelint)
	      iee1(i,nel) = nelint(1) + nelint(2) - nel
              nodesum1=node1+node2+node3+node4
              nodesum2=max(node1,node2,node3,node4) - nodesum1
              do j=1,nfacehex
  	         m1 = ien(ihexface1(1,j),iee1(i,nel))
	         m2 = ien(ihexface1(2,j),iee1(i,nel))
	         m3 = ien(ihexface1(3,j),iee1(i,nel))
	         m4 = ien(ihexface1(4,j),iee1(i,nel))
                 modesum1=m1+m2+m3+m4
                 modesum2=max(m1,m2,m3,m4) - modesum1
                 if((nodesum1-modesum1.eq.0). and.
     *              (nodesum2-modesum2.eq.0) ) iee2(i,nel)=j
              enddo
              if(iee1(i,nel).gt.0) then
                 nodemax1=max(node1,node2,node3,node4)
                 nodemin1=min(node1,node2,node3,node4)
                 nodesum1=node1+node2+node3+node4
                 nodesum2=nodemax1 - nodesum1
                 nodesum3=nodemin1 - nodesum1
                 nodemax2=(node1*node2*node3*node4) /
     *                    (nodemax1*nodemin1)
                 do j=1,nfacehex
  	            m1 = ien(ihexface1(1,j),iee1(i,nel))
	            m2 = ien(ihexface1(2,j),iee1(i,nel))
                    m3 = ien(ihexface1(3,j),iee1(i,nel))
                    m4 = ien(ihexface1(4,j),iee1(i,nel))
                    modemax1=max(m1,m2,m3,m4)
                    modemin1=min(m1,m2,m3,m4)
                    modesum1=m1+m2+m3+m4
                    modesum2=modemax1 - modesum1
                    modesum3=modemin1 - modesum1
                    modemax2=(m1*m2*m3*m4)/(modemax1*modemin1)
                    if((nodesum1-modesum1.eq.0).and.
     *                 (nodesum2-modesum2.eq.0).and.
     *                 (nodesum3-modesum3.eq.0).and.
     *                 (nodemax2-modemax2.eq.0) ) then
                       iee2(i,nel)=j
                    endif
                 enddo
               endif
1200	    continue
1400    continue
       endif
       nint=0
       nbndry=0
       nerror=0
       do j=1,numel
          do i=1,nef
             if(iee1(i,j).eq.0) then
                nbndry=nbndry+1
             else
                nint=nint+1
                jtest=iee1(iee2(i,j),iee1(i,j))
                itest=iee2(iee2(i,j),iee1(i,j))
                if(nerror.lt.100.and.(j.ne.jtest.or.i.ne.itest)) then
                   nerror=nerror+1
                   write(logmess,'(a,i10,a,i10,a,8i10)')
     *                   'Conn err: tet=',j,' face=',i,
     *                   ' nodes:',(ien(k,j),k=1,nen)
                   call writloga('default',0,logmess,0,ierrw)
                   if(nsd.eq.3.and.nen.eq.4.and.nef.eq.4) then
                      node1=ien(itetface1(1,i),j)
                      node2=ien(itetface1(2,i),j)
                      node3=ien(itetface1(3,i),j)
                      nodemax1=max(node1,node2,node3)
                      nodemin1=min(node1,node2,node3)
                      nodesum1=node1+node2+node3
                      nodesum2=nodemax1 - nodesum1
                      nodesum3=nodemin1 - nodesum1
                      nodemax2=(node1*node2*node3) /
     *                         (nodemax1*nodemin1)
                      do nel = 1, numel
                         do l = 1, nfacetet
                            m1 = ien(itetface1(1,l),nel)
                            m2 = ien(itetface1(2,l),nel)
                            m3 = ien(itetface1(3,l),nel)
                            modemax1=max(m1,m2,m3)
                            modemin1=min(m1,m2,m3)
                            modesum1=m1+m2+m3
                            modesum2=modemax1 - modesum1
                            modesum3=modemin1 - modesum1
                            modemax2=(m1*m2*m3)/(modemax1*modemin1)
                            if((nodesum1-modesum1.eq.0).and.
     *                        (nodesum2-modesum2.eq.0).and.
     *                        (nodesum3-modesum3.eq.0).and.
     *                        (nodemax2-modemax2.eq.0) ) then
                              write(logmess,'(a,i10,a,i10,8i10)')
     *                              '  Match face: tet=',nel,
     *                              ' face=',l,(ien(k,nel),k=1,nen)
                              call writloga('default',0,logmess,0,ierrw)
                            endif
                          enddo
                       enddo
                   endif
                endif
             endif
          enddo
       enddo
       if(nef*numel.eq.(nint+nbndry)) then
         write(logmess,'(a,3i10)') 'Connectivity matrix is correct: ',
     *                    numel,nint,nbndry
         call writloga('default',0,logmess,0,ierrw)
       endif
c
c.... return
c
       call mmrelprt(isubname,icscode)
C
       goto 9999
 9999  continue
C
	return
	end
C ######################################################################
 
*dk,geniee_cmo_subset
      subroutine geniee_cmo_subset(cmonam,nelm,ielm)
C
C #####################################################################
C
C     PURPOSE -
C
C        Generate neighbor connectivity for an arbitary mesh
C       The itet attribute on input must refer only to parent
C       points.
C
C     INPUT ARGUMENTS -
C
C        cmonam   name of mesh object
C
C     OUTPUT ARGUMENTS -
C
C        None  (correct neighbor connectivity will be in jtet atribute)
C
C     CHANGE HISTORY -
C
C        $Log: geniee.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
C
C NOV 2003 tam
C Added from Harold's sgi code for the amr option of refinement
C with principal refine direction (prd) using topology
C Used with refine_hex_prd() which is the PRD version of refine_hex_add()
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
      character*(*) cmonam
      integer ielm(nelm)
C
      include "local_element.h"
C
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      integer nedge_bin(1000000), nedge_off(1000000)
      pointer (ipitface, itface)
      pointer (ipifface, ifface)
      integer itface(1000000), ifface(1000000)
C
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
      pointer (ipitettyp, itettyp )
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff )
      integer itetoff(1000000)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(1000000)
      pointer (ipitet, itet1 )
      integer itet1(1000000)
      pointer (ipjtet, jtet1 )
      integer jtet1(1000000)
C
      pointer (ipitetlev, itetlev)
      integer itetlev(1000000)
C
C
      character*32 isubname, cmo
C
C ######################################################################
C
C
      isubname='geniee_cmo_subset'
C
      logmess = ' '

      cmo=cmonam
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('faces_per_element',cmo,nefc,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetlev',cmo,ipitetlev,ilen,itype,icslev)
 
C
C
      nenmax=0
      do it=1,nelements
         nenmax=max(nenmax,nelmnen(itettyp(it)))
      enddo
      length=nenmax*nnodes
C
C  nedge_bin contains a count of number of elements that share a facet
C  indexed by the sum of the node numbers of the facet
C  Since this sum is not unique they different facets will be sorted
C  out later
C  nedge_off is the offset into the itface and ifface arrays by element
C
      call mmgetblk('nedge_bin',isubname,
     *              ipnedge_bin,length,2,icscode)
      call mmgetblk('nedge_off',isubname,
     *              ipnedge_off,length,2,icscode)
      do i=1,nenmax*nnodes
         nedge_bin(i)=0
         nedge_off(i)=0
      enddo
      do it=1,nelements
         do i=1,nelmnef(itettyp(it))
            ifsum=0
            do j=1,ielmface0(i,itettyp(it))
               j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               ifsum=ifsum+j1
            enddo
            nedge_bin(ifsum)=nedge_bin(ifsum)+1
         enddo
      enddo
      isum=0
      do i=1,nenmax*nnodes
         if(nedge_bin(i).gt.0) then
            nedge_off(i)=isum
            isum=isum+nedge_bin(i)
         endif
         nedge_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('itface',isubname,
     *              ipitface,length,2,icscode)
      call mmgetblk('ifface',isubname,
     *              ipifface,length,2,icscode)
      do i=1,length
         itface(i)=0
         ifface(i)=0
      enddo
      do it=1,nelements
         do i=1,nelmnef(itettyp(it))
            ifsum=0
            do j=1,ielmface0(i,itettyp(it))
               j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               ifsum=ifsum+j1
            enddo
            nedge_bin(ifsum)=nedge_bin(ifsum)+1
            itface(nedge_off(ifsum)+nedge_bin(ifsum))=it
            ifface(nedge_off(ifsum)+nedge_bin(ifsum))=i
         enddo
      enddo
      do idum=1,nelm
         it=ielm(idum)
         do i=1,nelmnef(itettyp(it))
            nodemax1=0
            nodemin1=nnodes+1
            nodesum1=0
            do j=1,ielmface0(i,itettyp(it))
               j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               nodemax1=max(nodemax1,j1)
               nodemin1=min(nodemin1,j1)
               nodesum1=nodesum1+j1
            enddo
            nodemax2=0
            do j=1,ielmface0(i,itettyp(it))
               j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               if(j1.lt.nodemax1) nodemax2=max(nodemax2,j1)
            enddo
            if(nedge_bin(nodesum1).eq.1) then
               jtet1(jtetoff(it)+i)=mbndry
            else
               nodesum2=nodemax1 - nodesum1
               nodesum3=nodemin1 - nodesum1
               do j=1,nedge_bin(nodesum1)
                  jt=itface(nedge_off(nodesum1)+j)
                  jf=ifface(nedge_off(nodesum1)+j)
                  if(it.eq.jt.and.i.eq.jf) then
                  else
                     modemax1=0
                     modemin1=nnodes+1
                     modesum1=0
                     do k=1,ielmface0(jf,itettyp(jt))
                        j1=itet1(itetoff(jt) +
     *                     ielmface1(k,jf,itettyp(jt)))
                        modemax1=max(modemax1,j1)
                        modemin1=min(modemin1,j1)
                        modesum1=modesum1+j1
                     enddo
                     modemax2=0
                     do k=1,ielmface0(jf,itettyp(jt))
                        j1=itet1(itetoff(jt) +
     *                     ielmface1(k,jf,itettyp(jt)))
                        if(j1.lt.modemax1) modemax2=max(modemax2,j1)
                     enddo
                     modesum2=modemax1 - modesum1
                     modesum3=modemin1 - modesum1
                     if(icslev.eq.0) then
                        if(itetlev(it).eq.itetlev(jt).and.
     *                     (nodesum1-modesum1.eq.0).and.
     *                     (nodesum2-modesum2.eq.0).and.
     *                     (nodesum3-modesum3.eq.0).and.
     *                     (nodemax2-modemax2.eq.0) ) then
                           jtet1(jtetoff(it)+i)=
     *                                       nefc*(jt-1)+jf
                           jtet1(jtetoff(jt)+jf)=
     *                                       nefc*(it-1)+i
                           goto 100
                        endif
                     else
                        if((nodesum1-modesum1.eq.0).and.
     *                     (nodesum2-modesum2.eq.0).and.
     *                     (nodesum3-modesum3.eq.0).and.
     *                     (nodemax2-modemax2.eq.0) ) then
                           jtet1(jtetoff(it)+i)=
     *                                       nefc*(jt-1)+jf
                           jtet1(jtetoff(jt)+jf)=
     *                                       nefc*(it-1)+i
                           goto 100
                        endif
                     endif
                  endif
               enddo
               jtet1(jtetoff(it)+i)=mbndry
 100           continue
            endif
         enddo
      enddo
C
      call elmtestd(cmonam,20,ierror)
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end

      subroutine normal_check_flip
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#######################################################################
C
C      PURPOSE - Attempt to make the topological orientation (itet array) of
C      a triangle, quad, or hybrid tri/quad mesh consistent so that
C      shared edges are traversed in opposite directions. This is
C      only possible in a mesh with jtet_loop_max = 2. For networks
C      with jtet_loop_max > 2 there may not be a configuration that
C      meets the goal.
C
C      This command could also be used to flip the normals of a mesh that
C      is already consistent. For example:
C
C      geniee / -def- / 2dnormal / -1
C
C      For the case where a mesh is not completely edge connected, this
C      module will detect that all elements have not been tested and will
C      warn the user and suggest a command line syntax to test elements
C      not visited.
C
C      Note: Code not set up for a mesh with parent/child chains
C      When check is made, it compares child points. When permutation
C      of elements is done, only itet and jtet arrays are updated.
C
C      COMMAND LINE SYNTAX
C
C      geniee / cmoname / 2dnormal / reference_element_number / [addatt]
C
C      cmoname - name of mesh object to operate on. Can be / / or /-def-/
C
C      2dnormal - keyword to cause this module to execute
C
C      reference_element_number - default = 1
C      This is the element number that will be the reference element that
C      all other elements are compared to. If this parameter is a negative
C      number, the orientation of element number abs(reference_element_number)
C      is reversed and then used as the reference.
C
C      reference_element_number = 0 will check and report if orientation
C      is consistent, but will not do any flipping.
C
C      addatt - if the keyword addatt is included, two new arrays are added
C      to the mesh object.
C          ipath - The order in which elements are visited.
C
C          ifflip - 0 if the element orientation was NOT changed
C                   1 if the element orientation was changed
C
C      INPUT ARGUMENTS -
C
C         XMSGIN ]
C         MSGIN  ] - Input message
C         IMSGIN ]
C
C         NWDS     - Number of words in the input message
C
C
C      OUTPUT ARGUMENTS -
C
C         IERR1    - Error flag
C
C
C      CHANGE HISTORY -
C           Rev 1.0   30 Mar 2004 14:20:32   gable
C           Initial revision.
C
C#######################################################################
C
      implicit none
C
      include "local_element.h"
C
C#######################################################################
C
C     Command line variables
C
      integer nwds,ierr
      integer imsgin(nwds), msgtyp(nwds)
      real*8  xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer itet, itetoff, jtet, jtetoff, itettyp,
     1        mbndry, nnodes, nelements, ilen, itype,
     2        iarg, nef_cmo
 
      pointer (ipitet,    itet(*) )
      pointer (ipitetoff, itetoff(*) )
      pointer (ipjtet,    jtet(*) )
      pointer (ipjtetoff, jtetoff(*) )
      pointer (ipitettyp, itettyp(*) )
 
      integer ipath, ipath_mo, mark_path, ifflip
 
      pointer (ipipath, ipath(*) )
      pointer (ipipath_mo, ipath_mo(*) )
      pointer (ipifflip, ifflip(*) )
      pointer (ipmark_path, mark_path(*) )
 
      integer jtet_loop_max
      pointer (ipjtet_loop_max, jtet_loop_max)
 
      integer nstack,n_count,i,iface1,iface2,itetuse,n_reference
      integer n_reference_orientation
      integer i1,i2,j1,j2,jt,kt
      integer iflip_count,if_addatt, if_action
      integer n_not_marked, reference_restart1, reference_restart2
 
      character*512 cbuff
      character*132 logmess
      character*32 isubname,cmonam
      integer icharlnf
      integer if_debug_normal_check_flip
      data if_debug_normal_check_flip / 0 / 
C
C#######################################################################
C
      isubname='normal_check_flip'
      iflip_count = 0
c
c    Decide what to do based on command line strings
c
c    Argument 2 - Get the mesh object
c
      cmonam = cmsgin(2)
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmonam, ierr)
         if(ierr.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format(" flip_topology: MO not found: ",a)
           call writloga('default',0,logmess,0,ierr)
           goto 9999
         endif
      endif
C
C        ...............................................................
C
C     Check that cmonam is a valid mesh object
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ierr)
      if(ierr .ne. 0)then
           write(logmess,9005) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9005   format(" flip_topology: ERROR looking for nnodes of MO: ",a)
           call writloga('default',0,logmess,0,ierr)
           goto 9999
      endif
      call cmo_get_info('nelements',cmonam,nelements,ilen,itype,ierr)
      if(ierr .ne. 0)then
           write(logmess,9006) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9006 format(" flip_topology: ERROR looking for nelements of MO: ",a)
           call writloga('default',0,logmess,0,ierr)
           goto 9999
      endif
C
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ierr)
      call cmo_get_info('itet',cmonam,ipitet,ilen,itype,ierr)
      call cmo_get_info('itetoff',cmonam,ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtet',cmonam,ipjtet,ilen,itype,ierr)
      call cmo_get_info('jtetoff',cmonam,ipjtetoff,ilen,itype,ierr)
      call cmo_get_info('itettyp',cmonam,ipitettyp,ilen,itype,ierr)
      call cmo_get_info('faces_per_element',cmonam,nef_cmo,ilen,itype
     &   ,ierr)
C
C        ...............................................................
C
C    Some temporary memory for walking algorithm
C    ******************************************************************
      call mmgetblk('ipath',isubname,ipipath,nelements,1,ierr)
      call mmgetblk('mark_path',isubname,ipmark_path,nelements,1,ierr)
c
C    Argument 3 - 2dnormal
C
C    No other options. Do nothing.
C
C
c    Argument 4 - Get the number of the reference element
C
      iarg = 4
c
      if(nwds .eq. 2)then
         n_reference = 1
         n_reference_orientation =  1
      elseif(nwds .eq. 3)then
         n_reference = 1
         n_reference_orientation =  1
      elseif(msgtyp(iarg) .eq. 1)then
         if(imsgin(iarg) .eq. 0)then
           if_action = 0
           n_reference = 1
           n_reference_orientation =  1
         else
           if_action = 1
           n_reference = abs(imsgin(iarg))
         endif
 
         if(imsgin(iarg) .lt. 0)then
            n_reference_orientation = -1
C
C           Flip the orientation of the reference element
C
            call reverse_itet_jtet
     1         (n_reference,itet,itetoff,jtet,jtetoff,itettyp,nef_cmo
     &         ,mbndry)
         elseif(imsgin(iarg) .gt. 0)then
            n_reference_orientation =  1
         endif
      endif
C
C
C        ...............................................................
C
C     Check that all elements are either tri or quad
C
      n_count = 0
      do i = 1, nelements
       if((itettyp(i) .ne. ifelmtri).and.
     1    (itettyp(i) .ne. ifelmqud))then
        n_count = n_count + 1
        write(logmess,9007) i
 9007   format(" flip_topology: ERROR, element not tri or quad: ",i10)
        call writloga('default',0,logmess,0,ierr)
        endif
        if(n_count .ne. 0)then
           goto 9999
        endif
      enddo
C
C
C        ...............................................................
C
C     Check if the mesh has jtet loops greater than 2.
C
      call cmo_get_info
     1    ('jtet_loop_max',cmonam,ipjtet_loop_max,ilen,itype,ierr)
      if(ierr .eq. 0) then
        if(jtet_loop_max .gt. 2)then
          write(logmess,'("---WARNING----        ")')
          call writloga('default',0,logmess,0,ierr)
          write(logmess,'(" jtet_loop_max = ",i10)')jtet_loop_max
          call writloga('default',0,logmess,0,ierr)
          write(logmess,'("Consistent normals may not be possible")')
          call writloga('default',0,logmess,0,ierr)
          write(logmess,'("---WARNING----        ")')
          call writloga('default',0,logmess,0,ierr)
        endif
      endif
C
C
C        ...............................................................
C
c    Argument 5 - Add attributes for tracking progress
C
      iarg = 5
c
      if(nwds .eq. iarg)then
      if((cmsgin(iarg)(1:icharlnf(cmsgin(iarg))).eq.'addatt'))then
 
        if_addatt = 1
 
        call cmo_get_info
     *       ('ipath',cmonam,ipipath_mo,ilen,itype,ierr)
        if(ierr .ne. 0)then
        ierr = 0
        cbuff='cmo/addatt//ipath/VINT/scalar/nelements' //
     *     ' ; finish'
        call dotaskx3d(cbuff,ierr)
        call mmgetpr('ipath',cmonam,ipipath_mo,ierr)
        endif
 
        call cmo_get_info
     *       ('ifflip',cmonam,ipifflip,ilen,itype,ierr)
        if(ierr .ne. 0)then
        ierr = 0
        cbuff='cmo/addatt//ifflip/VINT/scalar/nelements' //
     *     ' ; finish'
        call dotaskx3d(cbuff,ierr)
        call mmgetpr('ifflip',cmonam,ipifflip,ierr)
        endif
 
        if(n_reference_orientation .lt. 0)then
           ifflip(n_reference) = -1
        endif
      else
        if_addatt = 0
      endif
      endif
C
C        ...............................................................
C
C
C  Create a stack and push potential elements on stack with their neighbors
C  Pop elements off stack one at a time and quit when stack is empty
C  ipath is the stack
C  mark_path is one if element has been processed
C  Initialize stack
      nstack=1
      n_count=1
      ipath(1) = n_reference
      mark_path(n_reference)  = 1
      if(if_addatt .eq. 1)ipath_mo(1) = n_reference
C
C  Pop element off the stack
C
      dowhile (nstack .ne. 0)
       itetuse=ipath(nstack)
       nstack=nstack-1
       if(if_addatt .eq. 1)ipath_mo(itetuse) = n_count
       n_count = n_count + 1
 
 
C
C  Check all neighbors of popped element
C  put on stack if not already on stack.
C
C  Test direction of edge traversal. If the edge is
C  traversed in opposite directions, the normals are
C  set correctly. If the edge is traversed in the same
C  direction by both elements, keep the reference element
C  the same, and flip the connectivity of the neighbor.
C
      do iface1=1,nelmnef(itettyp(itetuse))
         if(jtet(jtetoff(itetuse)+iface1).ne.mbndry.and.
     *      jtet(jtetoff(itetuse)+iface1).ne.0) then
            if (jtet(jtetoff(itetuse)+iface1).lt.mbndry) then
               jt=1+(jtet(jtetoff(itetuse)+iface1)-1)
     *             /nelmnef(itettyp(itetuse))
            else
               jt=1+((jtet(jtetoff(itetuse)+iface1)-1)-mbndry)
     *             /nelmnef(itettyp(itetuse))
            endif
            if(mark_path(jt).eq.0) then
               mark_path(jt)=1
               nstack=nstack+1
               ipath(nstack)=jt
c
c       flip node order if needed
c
         i1=itet(itetoff(itetuse)+ielmface1(1,iface1,itettyp(itetuse)))
         i2=itet(itetoff(itetuse)+ielmface1(2,iface1,itettyp(itetuse)))
C
C     If parent/child chains exist, compare the parent nodes
C
C     Note: Code not set up for a mesh with parent/child chains
C
      do iface2=1,nelmnef(itettyp(jt))
         if(jtet(jtetoff(jt)+iface2).ne.mbndry.and.
     *      jtet(jtetoff(jt)+iface2).ne.0) then
            if (jtet(jtetoff(jt)+iface2).lt.mbndry) then
               kt=1+(jtet(jtetoff(jt)+iface2)-1)
     *             /nelmnef(itettyp(jt))
            else
               kt=1+((jtet(jtetoff(jt)+iface2)-1)-mbndry)
     *             /nelmnef(itettyp(jt))
            endif
C
C        If the faces (edges) are shared...
C
         if(kt .eq. itetuse)then
c
         j1=itet(itetoff(jt)+ielmface1(1,iface2,itettyp(jt)))
         j2=itet(itetoff(jt)+ielmface1(2,iface2,itettyp(jt)))
 
         if((i1 .eq. j2) .and. (i2 .eq. j1))then
C
C           Normals are correct
C
         if(if_addatt .eq. 1)ifflip(jt) = 1
C
         elseif((i1 .eq. j1) .and. (i2 .eq. j2))then
C
C           Normals must be flipped
C
C       Apply permutation and correct jtet array
C
        if(if_action .eq. 1)then
           call reverse_itet_jtet
     1          (jt,itet,itetoff,jtet,jtetoff,itettyp,nef_cmo,mbndry)
        endif
        if(if_addatt .eq. 1)ifflip(jt) = -1
        iflip_count = iflip_count + 1
C
         else
         if(if_addatt .eq. 1)ifflip(jt) = 2
         endif
         endif
         endif
         enddo
         endif
         endif
        enddo
      enddo
C
C        ...............................................................
C
C     Check to see if all elements were visited
C
C     Identify the first one found that is not marked
C
      n_not_marked = 0
      reference_restart1 = 0
      reference_restart2 = 0
      do jt = 1, nelements
         if(mark_path(jt) .ne. 1)then
            n_not_marked = n_not_marked + 1
            if(n_not_marked .eq. 1)then
                reference_restart1 = jt
            endif
            if((reference_restart2 .eq. 0).and.
     1         (jt .gt. n_reference))then
                 reference_restart2 = jt
            endif
         endif
      enddo
 
      if(n_not_marked .ne. 0)then
 
      write(logmess,'(" WARNING:# elements not tested         ",i10)')
     *      n_not_marked
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" WARNING:First untested element        ",i10)')
     *      reference_restart1
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" WARNING:First after reference element ",i10)')
     *      reference_restart2
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" SUGGESTION: Restart using new reference",i10)')
     *      reference_restart2
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" SUGGESTION: geniee / -def- / 2dnormal /",i10)')
     *      reference_restart2
      call writloga('default',0,logmess,0,ierr)
 
      endif
C
C        ...............................................................
C
C     Output: reference element number
C             reference element orientation
C             number of elements flipped
C
      write(logmess,'("---TOPOLOGY MODIFY----        ")')
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" Number of Elements Total   ",i10)')nelements
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" Number of Elements Tested  ",i10)')
     1                  nelements - n_not_marked
      call writloga('default',0,logmess,0,ierr)
      if(n_reference_orientation .gt. 0)then
      write(logmess,'(" Reference Element          ",i10)')n_reference
      else
      write(logmess,'(" Reference Element REVERSED ",i10)')n_reference
      endif
      call writloga('default',0,logmess,0,ierr)
 
      if(if_action .eq. 0)then
      write(logmess,'("---NO ACTION, Only Check--- ")')
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(" Inconsistent Elements      ",i10)')
     1                  iflip_count
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'("---NO ACTION, Only Check--- ")')
      call writloga('default',0,logmess,0,ierr)
      elseif(if_action .eq. 1)then
      write(logmess,'(" Number of Elements Flipped ",i10)')iflip_count
      call writloga('default',0,logmess,0,ierr)
      endif
      write(logmess,'("---TOPOLOGY MODIFY----        ")')
      call writloga('default',0,logmess,0,ierr)
C
 9999 call mmrelprt(isubname,ierr)
 
 
      return
      end
      subroutine reverse_itet_jtet
     1           (jt,itet,itetoff,jtet,jtetoff,itettyp,nef_cmo,mbndry)
      implicit none
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Reverse order of itet and jtet for element jt
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      include "local_element.h"
C
      integer jt,itet(*),itetoff(*),jtet(*),jtetoff(*),itettyp(*)
      integer j,nef_cmo,mbndry,nen,itemp(4)
      integer jtet1,jtet3,jtopp,jfopp
      integer ierr
      character*132 logmess

c.... This only works for tri's and quad's.
c.... The permation is :

c.... tri:  nodes:  1<->3, 2<->2.
c....       faces:  1<->3, 2<->2.
c.... quad: nodes:  1<->4, 2<->3.
c....       faces:  1<->3, 2<->2, 4<->4

C
C     Apply permutation and correct jtet array
C
C     Reverse itet order.
C
      nen=nelmnen(itettyp(jt))
      if (nen.gt.4) then
         write(logmess,
     *         '("REVERSE_ITET_JTET: supports tris, quads only ")')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'("REVERSE_ITET_JTET: STOP ")')
         call writloga('default',0,logmess,0,ierr)
         stop
      endif
      do j=1,nen
         itemp(j)=itet(itetoff(jt)+j)
      enddo
      do j=1,nen
         itet(itetoff(jt)+j)=itemp(nen+1-j)
      enddo
C
C     Reverse jtet order.
C
      jtet1=jtet(jtetoff(jt)+1)
      jtet3=jtet(jtetoff(jt)+3)
      jtet(jtetoff(jt)+1)=jtet3
      jtet(jtetoff(jt)+3)=jtet1

c... We must also relabel jtets of opposite elements, since
c....local face numbering has changed.

      if (jtet3.lt.mbndry) then
         jtopp=1+(jtet3-1)/nef_cmo
         jfopp=jtet3-nef_cmo*(jtopp-1)
         jtet(jtetoff(jtopp)+jfopp)=(jt-1)*nef_cmo+1
      elseif (jtet3.gt.mbndry) then
         jtopp=1+(jtet3-mbndry-1)/nef_cmo
         jfopp=jtet3-mbndry-nef_cmo*(jtopp-1)
         jtet(jtetoff(jtopp)+jfopp)=(jt-1)*nef_cmo+1+mbndry
      endif

      if (jtet1.lt.mbndry) then
         jtopp=1+(jtet1-1)/nef_cmo
         jfopp=jtet1-nef_cmo*(jtopp-1)
         jtet(jtetoff(jtopp)+jfopp)=(jt-1)*nef_cmo+3
      elseif (jtet1.gt.mbndry) then
         jtopp=1+(jtet1-mbndry-1)/nef_cmo
         jfopp=jtet1-mbndry-nef_cmo*(jtopp-1)
         jtet(jtetoff(jtopp)+jfopp)=(jt-1)*nef_cmo+3+mbndry
      endif

      return
      end
