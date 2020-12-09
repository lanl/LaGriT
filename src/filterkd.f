C File subroutines: filterkd, filterkd_points, inherit_tris
C
      subroutine filterkd(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         Default and OPTION nocheck:
C           THIS ROUTINE SORTS THE MASS POINTS THAT ARE CLOSER THAN
C           dsmin AND DUDS OUT THE DUPLICATES.  IF ANY TETRAHEDRA HAVE
C           BEEN DEFINED, THE ITET VALUES IN THE CONNECTIVITY ARRAY ARE
C           ADJUSTED.  
C
C         OPTION zero_element:
C           IF ANY TETRAHEDRA HAVE ZERO VOLUME, 
C           THEY ARE REMOVED FROM THE TETRAHEDRAL LIST.
C           dsmin is mininum edge length for zero volume elements
C           NOTE: rmpoint/element must be used to clean itet array
C           which can have negative values after this routine
C
C         FORMAT: FILTERKD/pset get psetname/dsmin/nocheck (default)
C         FORMAT: FILTERKD/pset get psetname/dsmin/zero_element
C
C         Default is nocheck = skip portion that removes zero element
C         This is similar to original filter() except a kdtree structure
C         is used instead of the binning method and this versiion has
C         been shown to be more accurate where precision matters
C
C         zero_element is Kuprat algorithm to remove flat or zero elements
C         and will not remove duplicate elements or nodes not associated
C         with zero elements
C         
C         nocheck specified means filterkd can change topology
C         (Probably only suitable for readstl - kuprat)
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
C        Revision 3.2 August 2017 tamiller
C        major changes to workflow to make nocheck the default
C        This will replace older version filter() using bin method
C        Added error checking and more reporting consistend with filter()
C        Still needs some options copied from filter() to here
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
C#######################################################################
C
      implicit none
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
      pointer (ipxicf, xicf)
      pointer (ipyicf, yicf)
      pointer (ipzicf, zicf)
      real*8 xicf(*), yicf(*), zicf(*)
      real*8 xnoise
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(*), itetoff(*)
      pointer (ipitet, itet1 )
      integer itet1(*)
C
      pointer (ipialias, ialiastmp)
      integer ialiastmp(*)
C
      pointer (ipiparent, iparent)
      integer iparent(*)
C
      pointer (ipialiasf, ialiasf)
      integer ialiasf(*)

      integer ipt1, ipt2, ipt3
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
      integer izero_element
      integer  i1,eltj,eltk,itarg,m,nod,k
      integer len, itype, ierr, icscode, ierrw
      integer nelements,ierrdum,nptsmax,ipointi,ipointj,mpno,
     *  length,i,i2,icount,ecount,it,j,maxnbr

      integer ilen,icnt1,icnt2,icnt3

      logical badmerge,changed,nocheck

      REAL*8 dsmin, dsmin1

      integer icharlnf

      character*8 cglobal, cdefault
      character*132 logmess
      character*32 ich1,ich2,ich3
      character*32 isubname, cmo, coption
C
C#######################################################################
C BEGIN begin
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
      izero_element = 0
      nocheck=.true.
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
C     ******************************************************************
C
C     Return if you have an empty mesh object
C
      if(nnodes .eq. 0)then
         write(logmess,3005)
 3005    format('WARNING: FILTERKD Number of nodes = 0 ')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,3006)
 3006    format('WARNING: FILTERKD RETURN no action ')
         call writloga('default',0,logmess,1,ierr)
         go to 9999
       endif


C
C     ******************************************************************
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

 
c     GET pset to operate on - default is all nodes
 
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
C     GET THE minimum distance for duplicate node and edge length 
C
      call get_epsilon('epsilonl', dsmin1)
      if(nwds.lt.5.or.msgtype(5).ne.2) then
         dsmin=dsmin1
         write(logmess,9910) dsmin
 9910    format('FILTERKD:Use internal epsilonl value =',e21.12)
         call writloga('default',0,logmess,0,ierrw)
      else
         dsmin=xmsgin(5)
         write(logmess,9915) dsmin
 9915    format('FILTERKD:User specified value = ',e21.12)
         call writloga('default',0,logmess,0,ierrw)
      endif

      if(dsmin.lt.0.0) then
         write(logmess,9920)
 9920    format
     1   ('FILTERKD:INVALID INPUT, negative user value  ')
         dsmin=dsmin1
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,9925) dsmin
 9925    format('FILTERKD:Use internal epsilonl value =',e21.12)
         call writloga('default',0,logmess,0,ierrw)
      endif

C     check for 2nd parameter element which is not an option
      if (msgtype(2).eq.3.and.cmsgin(2)(1:7).eq.'element') then
          write(logmess,'(a)')
     *    'FILTERKD:INVALID option element, use filter/element'
          call writloga('default',0,logmess,0,ierrw)
          goto 9999
      endif

C     nwds 5 can be optional epsilon or word choice
C     nwds 6 is a word choice
C     Default does not use the remove zero element method
C     Check for min max used in filter() but not yet implemented here

      izero_element = 0
      nocheck=.true.
      coption='notset'
      if (nwds.eq.5.and.msgtype(5).eq.3) then
         coption = cmsgin(5)
         ilen=icharlnf(coption)
      else if (nwds.eq.6.and.msgtype(6).eq.3) then
         coption = cmsgin(6)
         ilen=icharlnf(coption)
      endif

      if (coption(1:6).ne.'notset') then 

         if (coption(1:ilen).eq.'nocheck') then
            nocheck=.true.
            izero_element = 0
            write(logmess,'(a)') 
     *      'FILTERKD: option nocheck'
            call writloga('default',0,logmess,0,ierrw)

         else if (coption(1:ilen).eq.'zero_element') then 
          izero_element = 1
          write(logmess,'(a)') 
     *    'FILTERKD: option zero_element'
          call writloga('default',0,logmess,0,ierrw)

        else if (coption(1:ilen).eq.'min') then
            write(logmess,'(a)')
     *      'FILTERKD:INVALID option min, use filter'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999

        else if (coption(1:ilen).eq.'max') then
            write(logmess,'(a)')
     *      'FILTERKD:INVALID option max, use filter'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999

        else 
            write(logmess,'(a,a)')
     *    'FILTERKD:INVALID option: ',coption(1:icharlnf(coption))
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
        endif
      endif

CCCC TODO ADD min/max option here same as filter

C
C     FILTER THE SELECTED POINT SET.
c     move coordinates into temp locations
C
      length=mpno
      if(length .eq. 0)then
         write(logmess,9928)
 9928    format('FILTERKD:Point set is empty, no points to filter')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
      call mmgetblk('xicf',isubname,ipxicf,length,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk xicf')
      call mmgetblk('yicf',isubname,ipyicf,length,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk yicf')
      call mmgetblk('zicf',isubname,ipzicf,length,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk zicf')
      call mmgetblk('ialiasf',isubname,ipialiasf,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk ialiasf')

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
c  this replaces filter_points using the binning method
c
      call filterkd_points(mpno,xicf,yicf,zicf,dsmin,ialiasf)
      length=nnodes
      call mmgetblk('ialiastmp',isubname,ipialias,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk ialiastmp')

      do i1=1,nnodes
         ialiastmp(i1)=i1
      enddo
      do i=1,mpno
         i1=mpary(i)
         i2=mpary(ialiasf(i))
         ialiastmp(i1)=i2
      enddo
C
C ##################################################################
C  IF NOCHECK and NOT RM_ZERO_ELEMENT
c  set duplicate nodes to point type 'dud'

C  This is start of code different from regular filter
C  because this part is skipped if 0 elements or using nocheck 

c
cNEED TO CORRECT LOGIC FOR MULTIMAT CASE:
cFOLLOWING LOOP ALLOWS SINGLE CHILD TO BE DUDDED, LEAVING
cOTHER CHILDREN UNDUDDED - kuprat

      if (izero_element.eq.0 .or. nelements.eq.0) then
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
         ecount = 0
         do it=1,nelements
            do i=1,nelmnen(itettyp(it))
               i1=itet1(itetoff(it)+i)
               itet1(itetoff(it)+i)=ialiastmp(i1)
               if (ialiastmp(i1).lt.1) ecount = ecount+1
            enddo
         enddo

C        skip to end past zero element removal
C        goto 9900
       
      else

C ##################################################################
C     ELSE OPTION RM_ZERO_ELEMENT 
C     All this code is the main difference to original filter and
C     was added to collapse zero volume tets based on edge length 

C TODO find where to get the count of dup nodes in this section.
C The number of duds in itp1 are less than total from rmpoint.

       icnt1=0
       icnt2=0
       icnt3=0
       icount = 0
       ecount = 0
       write(logmess,'(a)') 
     * 'FILTERKD: dudding for zero elements only'
       call writloga('default',0,logmess,0,ierrw)

c  method for zero_element
C
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
               itp1(i)=ifitpdud
               icount=icount+1
               do 1050 k=ifirst(i),ifirst(i+1)-1
                  eltk=nodelt(k)
                  if (itet1(itetoff(eltk)+1).eq.-1) goto 1050

c... If itarg appears in the element, the element is dudded
c... Otherwise, node i gets changed to node itarg

                  do m=1,nelmnen(itettyp(eltk))
                     nod=itet1(itetoff(eltk)+m)
                     if (nod.eq.itarg) then
                        itet1(itetoff(eltk)+1)=-1
                        ecount= ecount+1
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
      endif
C SKIP to here if not removing zero elements 

C REPORT count totals for points and elements
C   icount are the number of nodes dudded
C   ecount are the zero volume elements marked with -1 
  
      if (izero_element.eq.0) then
 9900   write(logmess,9930) icount
 9930 format('FILTERKD:Dudding duplicate points:',4x,i10)
        call writloga('default',0,logmess,0,ierrw)

      else
         write(logmess,9932) icount
 9932 format('FILTERKD:Dudding zero element points:',1x,i10)
         call writloga('default',0,logmess,0,ierrw)
      endif
         
      if (izero_element.ne.0 .or. ecount.gt.0) then
         write(logmess,9931) ecount
 9931 format('FILTERKD:Marking zero elements:',7x,i10)
         call writloga('default',0,logmess,0,ierrw)
      endif 

C     use rmpoint/element to deal with negative itet values
C     which are fatal to many routines if they are used
C     as an index into cmo attributes

      if (izero_element.ne.0) then
        call dotaskx3d('rmpoint/element;finish',ierr)
        if (ierr.ne.0) call x3d_error(isubname,'rmpoint')
      endif

 9999 continue

      if (ierror .ne. 0) then
         write(logmess,'(a,i5)') 
     *   'ERROR FILTERKD: exit with error: ',ierror
          call writloga('default',0,logmess,1,ierrw)
      endif

      call mmrelprt(isubname,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmrelprt')

      return
      end
C end filterkd


      subroutine filterkd_points(nnodes,xic,yic,zic,dsmin,ialiastmp)
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
      integer icount, ichange
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
      icount = 0
      ichange = 0
      if (nnodes.lt.1) then
        ierror = -1
        goto 999
      endif

c... Build kdtree 
      call mmgetblk('linkt',isubname,iplinkt,2*nnodes,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname, 'mmgetblk linkt')
      call mmgetblk('sbox',isubname,ipsbox,12*nnodes,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname, 'mmgetblk sbox')
      call mmgetblk('ifound',isubname,ipifound,nnodes,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname, 'mmgetblk ifound')
      call kdtree0(xic,yic,zic,nnodes,linkt,sbox,ierror)
      if(ierror.ne.0) call x3d_error(isubname, 'kdtree0')

c... Everyone aliased to themselves at first
      do i=1,nnodes
         ialiastmp(i)=i
      enddo

c... For each node, see if higher-index closeby nodes can be mapped
c... to it.
c... If ialiastmp(i).ne.i, we consider this node 'mapped' and so 
c... it is not eligible for having other nodes map to it.

      do i=1,nnodes
         if (ialiastmp(i).eq.i) then
            call retrieve_within_eps(xic(i),yic(i),zic(i),linkt,sbox,
     &         dsmin,nfound,ifound,ierror)

            do jj=1,nfound
               icount = icount+1
               j=ifound(jj)
c... If ialiastmp(j).ne.j, we consider this node 'mapped' and so 
c... it is not eligible for mapping to the current i.
               if (ialiastmp(j).eq.j) then
                  if (j.gt.i) ialiastmp(j)=i
                  ichange = ichange+1
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
999   if(ierror.ne.0) call x3d_error(isubname, 'Exit with error.')
C
      return
      end
C end filterkd_points

C#######################################################################
      subroutine inherit_tris(nodelt,ifirst,tris,nnodes,i,itarg)

      implicit none

      integer nnodes,i,itarg,nshift
      integer nodelt(*),ifirst(*),tris(*)

      integer k,nod

C#######################################################################
C Begin inherit_tris

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
C end inherit_tris


