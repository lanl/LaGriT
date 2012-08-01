*dk,refine_fix_add
      subroutine refine_fix_add(cmo_name,
     *                           nadd,
     *                           ipitadd,ipieadd,
     *                           ipiadd,ipitpnew,ipicrnew)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine fixes the type and constraint attributes
C           of nodes to be added as a result of a refine/addpts.
C           Call this routine before adding the points (while
C           edge to be refined is still intact)
C           Look at materials of all elements surrounding the
C           edge, to determine node type.
C           Instersect the constraint of edge points to get
C           the constraint of the new node.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        nadd     - Number of nodes added.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)ieadd  - Integer pointer to the list of local edges where
C                        nodes will be added.
C        (ip)iadd(nadd) - The node numbers of the nodes to added.
C        (ip)itpnew - Pointer supplied as input - array filled on exit
C        (ip)icrnew _ Pointer supplied as input - array filled on exit
C
C     OUTPUT ARGUMENTS -
C        (ip)itpnew - New point types for nodes to be added
C        (ip)icrnew _ New constraint flags for points to be added
C
C     CHANGE HISTORY -
C
C        $Log: refine_fix_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   Mon Jun 14 17:36:08 1999   dcg
CPVCS    make changes so that this will work for triangles in 2d
CPVCS    
CPVCS       Rev 1.6   Tue Mar 02 10:22:54 1999   dcg
CPVCS    set icr new values for new constrained interface edges
CPVCS    
CPVCS       Rev 1.5   Thu Sep 24 11:09:02 1998   dcg
CPVCS    fix bad call to itsttp ( argument must be node type not node
CPVCS    number)
CPVCS
CPVCS       Rev 1.4   Tue Jul 15 14:20:26 1997   dcg
CPVCS    prevent premature termination
CPVCS
CPVCS       Rev 1.3   Thu Jun 26 17:15:02 1997   dcg
CPVCS    change to run on all point types
CPVCS
CPVCS       Rev 1.2   Thu May 15 17:33:36 1997   dcg
CPVCS    check both directions for external bndry
CPVCS    restrict point types to 0,2,10,12 for now
CPVCS
CPVCS       Rev 1.1   Fri May 09 15:26:04 1997   dcg
CPVCS    fix indexing problems
CPVCS
CPVCS       Rev 1.0   Mon May 05 16:39:58 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
C
C ######################################################################
C
C
      character*(*) cmo_name
      integer nadd
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      pointer (ipieadd, ieadd)
      integer ieadd(nadd)
      pointer (ipiadd, iadd)
      integer iadd(nadd)
 
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipitpnew, itpnew)
      pointer (ipicrnew, icrnew)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*),
     *        itp1(*), icr1(*),
     *        isn1(*),
     *        itpnew(*), icrnew(*)
C
 
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)

      pointer (ipicontab, icontab)
      integer icontab(50,*)
      pointer (ipiparent, iparent)
      integer iparent(*)

      pointer (ipictemp,ictemp)
      integer ictemp(*)
C
      integer ilen,itype,icscode,ierror,n,iip1,iip2,
     *        i,ie,it,ip1,ip2,nef,nconbnd,nnodes,
     *        iflag,i1,i2,itstart,itt,itold,itnew,
     *        iff,ifold,ifnew,j,mbndry,icra,icrb,
     *        nsa,nsb,nsc,n1,n2,icnt,ittyp,nf,
     *        iflag1,jt,idir,nfstart,nf1,nsdtopo

      integer icharlnf

      logical itsttp,ivrt,ifre,irfl

      character*132 logmess
      character*32 isubname
C
C#######################################################################
C BEGIN begin
C
      isubname='refine_fix'
      call cmo_exist(cmo_name,ierror)
      if(ierror.ne.0) then
         write(logmess,8) cmo_name(1:icharlnf(cmo_name))
 8       format('CMO does not exist: ',a)
         call writloga('default',1,logmess,1,ierror)
         goto 9999
      endif
C
 
      call cmo_get_info('faces_per_element',cmo_name,
     *                  nef,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo_name,
     *                  mbndry,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo_name,
     *                  nsdtopo,ilen,itype,icscode)
      call cmo_get_info('nnodes',cmo_name,nnodes,ilen,itype,icscode)
      call cmo_get_info('itetclr',cmo_name,
     *                  ipitetclr,ilen,itype,icscode)
      call cmo_get_info('itettyp',cmo_name,
     *                  ipitettyp,ilen,itype,icscode)
      call cmo_get_info('itetoff',cmo_name,
     *                  ipitetoff,ilen,itype,icscode)
      call cmo_get_info('jtetoff',cmo_name,
     *                  ipjtetoff,ilen,itype,icscode)
      call cmo_get_info('itet',cmo_name,ipitet,ilen,itype,icscode)
      call cmo_get_info('jtet',cmo_name,ipjtet,ilen,itype,icscode)
      call cmo_get_info('itp1',cmo_name,ipitp1,ilen,itype,icscode)
      call cmo_get_info('icr1',cmo_name,ipicr1,ilen,itype,icscode)
      call cmo_get_info('isn1',cmo_name,ipisn1,ilen,itype,icscode)
      call cmo_get_info('nconbnd',cmo_name,nconbnd,ilen,itype,
     *                   icscode)
      if(ierror.ne.0)  then
         nconbnd=0
      else
         call cmo_get_info('icontab',cmo_name,ipicontab,ilen,itype,
     *                      icscode)
         call mmgetblk('ictemp',isubname,ipictemp,200,1,icscode)
      endif
C
C  put parent nodes in iparent
C
      call mmgetblk("iparent",isubname,ipiparent,nnodes,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
 
 
C
      do i=1,nadd
         it=itadd(i)
         ie=ieadd(i)
         iip1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
         iip2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
         ip1=iparent(iip1)
         ip2=iparent(iip2)
C
C  Loop through all neigbors of element to find one that shares
C  this edge
C
C
C iflag  = 0  only one material around edge
C        = 1  more than one material around edge
C iflag1 = 2  on and exterior boundary
C
         iflag=0
         iflag1=0
         itstart=it
         nfstart=1
         itold=0
         ifold=0
         itt=it
         idir=0
         ittyp=itettyp(itt)
         nf1=1
 10      do nf =nf1,nelmnef(ittyp)
           do j=1,ielmface0(nf,ittyp)
             ie=ielmface2(j,nf,ittyp)
             if(ie.eq.0) go to 18
             i1=itet1(itetoff(itt)+ielmedge1(1,ie,ittyp))
             i2=itet1(itetoff(itt)+ielmedge1(2,ie,ittyp))
             i1=iparent(i1)
             i2=iparent(i2)
             if(i1.eq.ip1.and.i2.eq.ip2.or.
     *         i2.eq.ip1.and.i1.eq.ip2) then
               iff=nf
               if(itt.eq.itstart) nfstart=nf
C
C  check if external boundary face - if so both edges
C  and node to be added are on an external boundary
C  make sure to check both directions for material
C  interfaces (use idir)
C
               jt=jtet1(jtetoff(itt)+iff)
               if(jt.eq.mbndry) then
                  iflag1=2
                  if(nsdtopo.eq.2) then
                     go to 30
                  endif
                  if(iflag.eq.1) go to 30
                  idir=idir+1
                  if(idir.eq.2) go to 30
                  itt=itstart
                  itold=0
                  ifold=0
                  nf1=nfstart+1
                  go to 10
               endif
               nf1=1
C
C  check if interface face - if so both edges
C  and node to be added are on an internal interface
C
               if(jt.gt.mbndry)then
                  jt=jt-mbndry
                  iflag=1
               endif
C
C  check that we didn't go backwards
C
               itnew=1+(jt-1)/nef
               ifnew=jt-nef*(itnew-1)
               if (itnew.ne.itold.or.ifnew.ne.ifold.or.
     *          nsdtopo.eq.2) then
C
C  found next element in chain
C  check for change in materials
C
                  if (itetclr(itnew).ne.itetclr(itt)) then
                      iflag=1
                  endif
                  ittyp=itettyp(itnew)
                  itold=itt
                  ifold=iff
C
C  see if back to starting element
C
                  if( itnew.eq.itstart) then
                      go to 30
                  endif
C
C  still looking for more tets surrounding the edge
C
               itt=itnew
               go to 10
               endif
             endif
C end loop on edges
           enddo
 18        continue
C end loop on faces
         enddo
C
C  shouldn't ever get here
C
         write(logmess, 20)
  20     format (' error in refine_fix_add',
     *           ' itp,icr set to zero ' )
         call writloga('default',0,logmess,0,icscode)
         icrnew(i)=0
         itpnew(i)=0
         go to  100
C
C  itp has been determined -set it if needed -
C  and check for intersection of icontabs for
C  the two edge points
C
  30     ivrt=.false.
         ifre=.false.
         irfl=.false.
         if(iflag.ne.1) then
            if(iflag1.ne.2) then
               itpnew(i)=0
               if (itsttp('virtual',itp1(iip1)).and.
     *             itsttp('virtual',itp1(iip2)))
     *            itpnew(i)=3
               icrnew(i)=0
               go to 100
            else
               itpnew(i)=10
               if (itsttp('virtual',itp1(iip1)).and.
     *            itsttp('virtual',itp1(iip2))) ivrt=.true.
               if (itsttp('free',itp1(iip1)).and.
     *            itsttp('free',itp1(iip2))) ifre=.true.
               if (itsttp('reflect',itp1(iip1)).and.
     *            itsttp('reflect',itp1(iip2))) irfl=.true.
               if (ivrt.and.ifre.and.irfl) itpnew(i)=18
               if (ivrt.and.ifre.and..not.irfl) itpnew(i)=17
               if (ivrt.and..not.ifre.and.irfl) itpnew(i)=16
               if (.not.ivrt.and.ifre.and.irfl) itpnew(i)=14
               if (.not.ivrt.and.ifre.and..not.irfl) itpnew(i)=11
            endif
 
         else
            if(iflag1.ne.2) then
               itpnew(i)=2
               if(icr1(iip1).eq.0.or.icr1(iip2).eq.0)then
                  icrnew(i)=0
                  go to 100
               endif
            else
               itpnew(i)=12
               if (itsttp('virtual',itp1(iip1)).and.
     *            itsttp('virtual',itp1(iip2))) ivrt=.true.
               if (itsttp('free',itp1(iip1)).and.
     *            itsttp('free',itp1(iip2))) ifre=.true.
               if (itsttp('reflect',itp1(iip1)).and.
     *            itsttp('reflect',itp1(iip2))) irfl=.true.
               if (ivrt.and.ifre.and.irfl) itpnew(i)=9
               if (ivrt.and.ifre.and..not.irfl) itpnew(i)=8
               if (ivrt.and..not.ifre.and.irfl) itpnew(i)=19
               if (.not.ivrt.and.ifre.and.irfl) itpnew(i)=15
               if (.not.ivrt.and.ifre.and..not.irfl) itpnew(i)=13
            endif
         endif
         icra=icr1(iip1)
         icrb=icr1(iip2)
         if(nconbnd.eq.0.or.icra.eq.0.or.icrb.eq.0) then
            icrnew(i)=0
            go to 100
         endif
         nsc=0
         nsa=icontab(1,icra)
         nsb=icontab(1,icrb)
         do n1=1,nsa
            do n2=1,nsb
               if (icontab(2+n1,icra).eq.icontab(2+n2,icrb))
     *             then
                   nsc=nsc+1
                   ictemp(nsc)=icontab(2+n1,icra)
               endif
            enddo
         enddo
C
C  if the intersection is empty then icrnew =0
C
         if( nsc.eq.0) then
            icrnew(i)=0
            go to 100
         endif
C
C  if the intersection is not empty
C  find the correct icontab entry
C  print message if it doesn't exist
C
         do n=1,nconbnd
            if(icontab(1,n).eq.nsc) then
               icnt=0
               do nsa=1,nsc
                  do nsb=1,nsc
                     if(ictemp(nsa).eq.icontab(2+nsb,n))
     *                  icnt=icnt+1
                  enddo
               enddo
               if(icnt.eq.nsc) then
                  icrnew(i)=n
                  go to 100
               endif
            endif
         enddo
         write(logmess,22)
 22      format('error in refine_fix_add',
     *         'cant find constraint entry')
         call writloga('default',0,logmess,0,ierror)
 100     continue
      enddo
      call mmrelprt(isubname,icscode)
9999  continue
      return
      end
