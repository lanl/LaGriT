      subroutine filterkd(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE SORTS THE MASS POINTS THAT ARE CLOSER THAN
C         dsmin AND DUDS OUT THE DUPLICATES.  IF ANY TETRAHEDRA HAVE
C         BEEN DEFINED, THE ITET VALUES IN THE CONNECTIVITY ARRAY ARE
C         ADJUSTED.  IF ANY TETRAHEDRA HAVE ZERO VOLUME, THEY ARE
C         REMOVED FROM THE TETRAHEDRAL LIST.
C
C         FORMAT: FILTERKD/pset get psetname/dsmin/nocheck
C
C            nocheck specified means filterkd can change topology
C            (Probably only suitable for readstl)
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C        Revision 1.7  2011/05/06 01:27:37  kuprat
C        adjusted documentation
C
C        Revision 1.6  2010/01/27 16:09:54  kuprat
C        nocheck option
C
C        Revision 1.5  2010/01/27 01:45:18  kuprat
C        merge only along edges w/o highval edges
C
C        Revision 1.4  2007/03/05 20:43:26  kuprat
C        Put in (*)
C
C        Revision 1.3  2007/03/05 03:29:46  kuprat
C        we now use kdtree for filter_points
C
C        Revision 1.2  2007/02/20 04:09:52  kuprat
C        we now retain LOWEST index point
C
C        Revision 1.1.1.1  2005/11/29 22:14:44  kuprat
C        initial import into CVS
C
CPVCS    
CPVCS       Rev 1.31   24 Dec 2003 10:20:24   tam
CPVCS    add het subroutine filter_subset() used with refine 'amr' iprd option 
CPVCS    
C
C#######################################################################
C
      implicit none
      character*132 logmess
C
C#######################################################################
C
      include "local_element.h"
      include "chydro.h"
      include "neibor.h"
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      integer nnodes
C
      pointer (ipisn1, isn1 )
      integer isn1(*)
C
      pointer (ipitp1, itp1 )
      integer itp1(*)
C
      pointer (ipisetwd, isetwd )
      integer isetwd(*)
C
      pointer (ipxic, xic )
      pointer (ipyic, yic )
      pointer (ipzic, zic )
      REAL*8 xic(*), yic(*), zic(*)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(*), itetoff(*)
      pointer (ipitet, itet1 )
      integer itet1(*)
C
C
      pointer (ipialias, ialiastmp)
      integer ialiastmp(*)
C
      pointer (ipiparent, iparent)
      integer iparent(*)
C
      pointer (ipialiasf, ialiasf)
      integer ialiasf(*)
      pointer (ipxicf, xicf)
      pointer (ipyicf, yicf)
      pointer (ipzicf, zicf)
      real*8 xicf(*), yicf(*), zicf(*)
      real*8 xnoise
C
C#######################################################################
C
      integer ipt1, ipt2, ipt3
      character*32 ich1,ich2,ich3
C
      character*32 isubname, cmo
C
      pointer (ipint1, int1)
      integer int1(*)
C
      pointer (ipmodpnt, modpnt )
      integer modpnt(*)
C
      pointer (ipmpary , mpary )
      integer mpary(*)

      pointer (ipifirstitet,ifirstitet)
      integer ifirstitet(*)

      pointer (ipifirst,ifirst)
      integer ifirst(*)

      pointer (ipnodstatus,nodstatus)
      integer nodstatus(*)

      pointer (ipnodelt,nodelt)
      integer nodelt(*)

      pointer (ipwork,work)
      integer work(*)
C
      integer  i1,eltj,eltk,itarg,m,nod,k
      REAL*8 dsmin, dsmin1
      integer len, itype, ierr, icscode, ierrw
      logical mapped,badmerge,changed,nocheck
      integer nelements,ierrdum,nptsmax,ipointi,ipointj,mpno,
     *  length,i,i2,icount,it,j,maxnbr
C
      character*8 cglobal, cdefault
C
C#######################################################################
C
C
C#######################################################################
C
C
      isubname='filterkd'
      cglobal='global'
      cdefault='default'
C
C     ******************************************************************
C     SET THE RETURN ERROR CODE.
C
      ierror = 0
C
      xnoise=1.0d-99
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,nnodes,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('isetwd',cmo,ipisetwd,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1',cmo,ipisn1,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,len,itype,ierror)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itettyp',cmo,ipitettyp,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,len,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
C        2) Do we have an interface.
C             int1() =  0 ==> not an interface point.
C             int1() =  1 ==> an interface point.
C
      call mmgetblk('int1',isubname,ipint1,nnodes,1,icscode)
         if(icscode.ne.0) call x3d_error(isubname, 'mmgetblk')
      call unpacktp('intrface','set',nnodes,ipitp1,ipint1,ierrdum)
         if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
      call mmgetblk('ipartent',isubname,ipiparent,nnodes,1,icscode)
         if(icscode.ne.0) call x3d_error(isubname, 'mmgetblk')
      call unpackpc(nnodes,itp1,isn1,iparent)
C
C
C     ******************************************************************
C     ALLOCATE TEMPORARY ARRAYS.
C
      nptsmax=nnodes
      call mmgetblk('modpnt',isubname,ipmodpnt,nnodes,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('mpary' ,isubname,ipmpary ,nnodes,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
C     ******************************************************************
C     CHECK POINT LIMITS AND PUT POINTS TO BE FILTERED INTO ARRAY mpary.
C
      call cmo_get_info('ipointi',cmo,ipointi,len,itype,ierr)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,len,itype,ierr)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      if(msgtype(2).eq.1.and.imsgin(2).eq.0) then
         imsgin(2)=ipointi
      endif
      if(msgtype(3).eq.1.and.imsgin(3).eq.0) then
         imsgin(3)=ipointj
      endif
      if(msgtype(4).eq.1.and.imsgin(4).eq.0) then
         imsgin(4)=1
      endif
C
c  find pset to operate on - default is all nodes
c
      if(nwds.eq.1) then
         ipt1=1
         ipt2=0
         ipt3=0
      endif
      if(msgtype(2).eq.1.or.nwds.eq.1) then
         if(nwds.eq.1) then
         elseif(nwds.eq.2) then
            ipt1=imsgin(2)
            ipt2=ipt1
            ipt3=1
         elseif(nwds.eq.3) then
            ipt1=imsgin(2)
            ipt2=imsgin(3)
            ipt3=1
         else
            ipt1=imsgin(2)
            ipt2=imsgin(3)
            ipt3=imsgin(4)
         endif
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      else
         ich1=cmsgin(2)
         ich2=cmsgin(3)
         ich3=cmsgin(4)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
C
C     GET THE minimum distance for duplicate node test.
C
      call get_epsilon('epsilonl', dsmin1)
      if(nwds.lt.5.or.msgtype(5).ne.2) then
         dsmin=dsmin1
         write(logmess,9910) dsmin
 9910    format('FILTERKD:Use internal epsilonl value =',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      else
         dsmin=xmsgin(5)
         write(logmess,9915) dsmin
 9915    format('FILTERKD:User specified value = ',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      endif
      if(dsmin.lt.0.0) then
         write(logmess,9920)
 9920    format
     1   ('FILTERKD:INVALID INPUT, negative user value  ')
         dsmin=dsmin1
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,9925) dsmin
 9925    format('FILTERKD:Use internal epsilonl value =',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      endif
      nocheck=.false.
      if (nwds.ge.6) then
         if (cmsgin(6)(1:7).eq.'nocheck') then
            nocheck=.true.
         endif
      endif
C
C     FILTER THE SELECTED POINT SET.
c     move coordinates into temp locations
C
      length=mpno
      if(length .eq. 0)then
         write(logmess,9928)
 9928    format('FILTERKD:Point set is empty, no points to filter')
         call writloga('default',1,logmess,0,ierrw)
         go to 9999
      endif
      call mmgetblk('xicf',isubname,ipxicf,length,2,icscode)
      call mmgetblk('yicf',isubname,ipyicf,length,2,icscode)
      call mmgetblk('zicf',isubname,ipzicf,length,2,icscode)
      call mmgetblk('ialiasf',isubname,ipialiasf,length,1,icscode)
      do i=1,mpno
         i1=mpary(i)
         ialiasf(i)=i
         xicf(i)=xic(i1)
         yicf(i)=yic(i1)
         zicf(i)=zic(i1)
      enddo
c
c  this call does the work - ialiasf will contain the node number
c  that the duplicate is equivalent to - if it is a duplicate
c
      call filterkd_points(mpno,xicf,yicf,zicf,dsmin,ialiasf)
      length=nnodes
      call mmgetblk('ialiastmp',isubname,ipialias,length,1,icscode)
      do i1=1,nnodes
         ialiastmp(i1)=i1
      enddo
      do i=1,mpno
         i1=mpary(i)
         i2=mpary(ialiasf(i))
         ialiastmp(i1)=i2
      enddo
C
c  set duplicate nodes to point type 'dud'
c
cNEED TO CORRECT LOGIC FOR MULTIMAT CASE:
cFOLLOWING LOOP ALLOWS SINGLE CHILD TO BE DUDDED, LEAVING
cOTHER CHILDREN UNDUDDED

      if (nelements.eq.0.or.nocheck) then
         icount=0
         do i2=1,nnodes
            i1=ialiastmp(i2)
            if(i1.ne.i2) then
               if(iparent(i1).ne.iparent(i2)) then
                  icount=icount+1
                  itp1(i2)=ifitpdud
               else
                  ialiastmp(i2)=i2
               endif
            endif
         enddo
C
c  fix up the itet array so that duplicate nodes have been replaced
c
         do it=1,nelements
            do i=1,nelmnen(itettyp(it))
               i1=itet1(itetoff(it)+i)
               itet1(itetoff(it)+i)=ialiastmp(i1)
            enddo
         enddo

         goto 9900
      endif

c  fix up the itet array so that duplicate nodes have been replaced
c
c  When nodes are present there are two principles that are followed
c  to assure that merging doesn't change the topology of the mesh:
c  (1) We will only merge nodes along existing edges; otherwise
c      a 'fold' would be created in a surface mesh
c  (2) We will only perform merges that do not create edges that
c      already exist; otherwise a 'high valence' edge would be 
c      created in a surface mesh.
c  It is speculated these restrictions are also adequate for 
c  volume mesh merging.
c  These restrictions seek to preserve topology, but do not seek
c  to preserve geometric things like positivity of signed areas
c  or volumes, smoothness of surface normals, etc.  If these
c  things become damaged, 'massage' or 'mmvc_smooth' are suggested
c  as fixes...

c...  Create the node-element connectivity relation.

      icount=0
      call mmgetblk('ifirstitet',isubname,ipifirstitet,nelements+1,1
     &   ,ierr)
      call mmgetblk('ifirst',isubname,ipifirst,nnodes+1,1
     &   ,ierr)
      do i=1,nelements
         ifirstitet(i)=itetoff(i)+1
      enddo
      ifirstitet(nelements+1)=ifirstitet(nelements)
     &   +nelmnen(itettyp(nelements))
      call mmgetblk('nodelt',isubname,ipnodelt,
     &   ifirstitet(nelements+1)-1,1,ierr)
      call reverseform(nelements,ifirstitet,itet1,nnodes,ifirst,nodelt
     &   )

      maxnbr=0
      do j=1,nnodes
         maxnbr=max(maxnbr,ifirst(j+1)-ifirst(j))
      enddo
      call mmgetblk('work',isubname,ipwork,maxnbr,1,ierr)
      call mmgetblk('nodstatus',isubname,ipnodstatus,nnodes,1,ierr)
      do j=1,nnodes
         nodstatus(j)=0
      enddo
c... nodstatus 2 for nodes of elements shared by both i and itarg
c    nodstatus 1 for remaining nbr nodes of i
c    nodstatus 0 for all remaining nodes

c... Loop over nodes and see if there are nodes we can merge
c     If ialiastmp(i).ne.i there was intent to merge node i into
C     ialiastmp.  If this 
      changed=.true.
      do while (changed) 
         changed=.false.
         do 1000 i=1,nnodes
            if (itp1(i).eq.ifitpdud.or.ialiastmp(i).eq.i) goto 1000
            itarg=ialiastmp(i)
            do 1010 j=ifirst(i),ifirst(i+1)-1
               eltj=nodelt(j)
               if (itet1(itetoff(eltj)+1).eq.-1) goto 1010
               do m=1,nelmnen(itettyp(eltj))
                  nod=itet1(itetoff(eltj)+m)
                  if (nodstatus(nod).eq.0) nodstatus(nod)=1
               enddo

               do 1020 k=ifirst(itarg),ifirst(itarg+1)-1
                  eltk=nodelt(k)
                  if (itet1(itetoff(eltk)+1).eq.-1) goto 1020
                  if (eltj.eq.eltk) then
                     do m=1,nelmnen(itettyp(eltj))
                        nod=itet1(itetoff(eltj)+m)
                        nodstatus(nod)=2
                     enddo
                  endif
 1020          enddo
 1010       enddo

c... Now run thru itargs elements and see if there are nodes that are 
c... type 1 (which would indicate creation of a high-valence edge)


            badmerge=.false.
            if (nodstatus(i).ne.2) then
c... this occurs if i,itarg not connected by edge
               badmerge=.true.
            else
               do 1030 k=ifirst(itarg),ifirst(itarg+1)-1
                  eltk=nodelt(k)
                  if (itet1(itetoff(eltk)+1).eq.-1) goto 1030

                  do m=1,nelmnen(itettyp(eltk))
                     nod=itet1(itetoff(eltk)+m)
                     if (nodstatus(nod).eq.1) then
                        badmerge=.true.
                     endif
                  enddo
 1030          enddo
            endif

c... Reset nodstatus
            do 1040 k=ifirst(i),ifirst(i+1)-1
               eltk=nodelt(k)
               if (itet1(itetoff(eltk)+1).eq.-1) goto 1040
               
               do m=1,nelmnen(itettyp(eltk))
                  nod=itet1(itetoff(eltk)+m)
                  nodstatus(nod)=0
               enddo
 1040       enddo

            
c.. Do merge if it isn't bad

            if (.not.badmerge) then
               changed=.true.
               icount=icount+1
               itp1(i)=ifitpdud

               do 1050 k=ifirst(i),ifirst(i+1)-1
                  eltk=nodelt(k)
                  if (itet1(itetoff(eltk)+1).eq.-1) goto 1050

c... If itarg appears in the element, the element is dudded
c... Otherwise, node i gets changed to node itarg

                  do m=1,nelmnen(itettyp(eltk))
                     nod=itet1(itetoff(eltk)+m)
                     if (nod.eq.itarg) then
                        itet1(itetoff(eltk)+1)=-1
                     elseif (nod.eq.i) then
                        itet1(itetoff(eltk)+m)=itarg
                     endif
                  enddo

c MODIFY NODELT SO THAT LIVE ELTS OF I GET ADDED TO ITARG;
c ELTS FOR NODE I GET REMOVED
 1050          enddo

               call inherit_tris(nodelt,ifirst,work,nnodes,i,itarg)
            endif

 1000    enddo
            
      enddo
C     
 9900 write(logmess,9930) icount
 9930    format('FILTERKD:Dudding', i7, ' duplicate points.')
         call writloga('default',1,logmess,0,ierrw)
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
 9999 call mmrelprt(isubname,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmrelprt')
C
C     ******************************************************************
C
      return
      end
c
      subroutine filterkd_points(nnodes,xic,yic,zic,dsmin,ialiastmp)
C
C
C#######################################################################
C
C      PURPOSE -
C
C       THIS ROUTINE IDENTIFIES NODES WHICH ARE CLOSER
C       THAN dsmin AND RETURNS AN "ALIAS" LIST THAT CONTAINS THE NEW
C       NAMES OF THE POINTS INDICATING WHICH POINT THEY DUPLICATE.
C       THE LOWER NUMBERED NODE WINS IN THAT IT IS IT'S OWN ALIAS.
C       We use a kdtree, replacing the old binning routine.
C
C      INPUT ARGUMENTS -
C
C         nnodes - THE NUMBER OF POINTS TO FILTER.
C         xic - THE LIST OF X-COORDINATES TO FILTER.
C         yic - THE LIST OF Y-COORDINATES TO FILTER.
C         zic - THE LIST OF Z-COORDINATES TO FILTER.
C         dsmin - THE MINIMUM DISTANCE CRITERIA.
C
C      OUTPUT ARGUMENTS -
C
C         ialiastmp - THE NAMES OF EACH POINT. IF THIS IS NOT AN
C                     IDENTITY THEN THIS POINT HAS BEEN FOUND
C                     TO BE CLOSE THE POINT INDEX CONTAINED IN
C                     ARRAY.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nnodes
      real*8 xic(nnodes), yic(nnodes), zic(nnodes)
      real*8 dsmin
      integer ialiastmp(nnodes)
C
      integer ierror
      pointer (iplinkt,linkt)
      integer linkt(*)
      pointer (ipsbox,sbox)
      real*8 sbox(2,3,*)
      pointer (ipifound,ifound)
      integer ifound(*)
      integer icscode,i,nfound,jj,j
      character*32 isubname
C
C#######################################################################
C
      isubname='filterkd_points'
C
C     SET THE RETURN ERROR CODE.
C
      ierror = 0

c... Build kdtree 
      call mmgetblk('linkt',isubname,iplinkt,2*nnodes,1,icscode)
      call mmgetblk('sbox',isubname,ipsbox,12*nnodes,2,icscode)
      call mmgetblk('ifound',isubname,ipifound,nnodes,1,icscode)
      call kdtree0(xic,yic,zic,nnodes,linkt,sbox,ierror)

c... Everyone aliased to themselves at first
      do i=1,nnodes
         ialiastmp(i)=i
      enddo
c... For each node, see if higher-index closeby nodes can be mapped
c... to it.
      do i=1,nnodes
c... If ialiastmp(i).ne.i, we consider this node 'mapped' and so 
c... it is not eligible for having other nodes map to it.
         if (ialiastmp(i).eq.i) then
            call retrieve_within_eps(xic(i),yic(i),zic(i),linkt,sbox,
     &         dsmin,nfound,ifound,ierror)

            do jj=1,nfound
               j=ifound(jj)
c... If ialiastmp(j).ne.j, we consider this node 'mapped' and so 
c... it is not eligible for mapping to the current i.
               if (ialiastmp(j).eq.j) then
                  if (j.gt.i) ialiastmp(j)=i
               endif
            enddo
         endif
      enddo

C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmrelprt')
C
C     ******************************************************************
C
      return
      end

      subroutine inherit_tris(nodelt,ifirst,tris,nnodes,i,itarg)

      implicit none

      integer nodelt(*),ifirst(*),tris(*),nnodes,i,itarg,nshift
      integer k,nod

      nshift=ifirst(i+1)-ifirst(i)
      do k=ifirst(i),ifirst(i+1)-1
         tris(k-ifirst(i)+1)=nodelt(k)
      enddo

      if (itarg.gt.i) then
         do nod=i+1,itarg
            do k=ifirst(nod),ifirst(nod+1)-1
               nodelt(k-nshift)=nodelt(k)
            enddo
         enddo
         do k=ifirst(itarg+1)-nshift,ifirst(itarg+1)-1
            nodelt(k)=tris(k-ifirst(itarg+1)+nshift+1)
         enddo
         do nod=i+1,itarg
            ifirst(nod)=ifirst(nod)-nshift
         enddo
      else
         do nod=i-1,itarg,-1
            do k=ifirst(nod+1)-1,ifirst(nod),-1
               nodelt(k+nshift)=nodelt(k)
            enddo
         enddo
         do k=ifirst(itarg),ifirst(itarg)+nshift-1
            nodelt(k)=tris(k-ifirst(itarg)+1)
         enddo
         do nod=itarg+1,i
            ifirst(nod)=ifirst(nod)+nshift
         enddo
      endif
      return
      end
