      subroutine filter(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
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
C         FORMAT: FILTER/ipstart/ipend/ipstep/dsmin
C
C         Optional min|max format:
C         FORMAT: FILTER/ipstart/ipend/ipstep/dsmin/[min|max/attribute]
C         FORMAT: FILTER/ipstart/ipend/ipstep/-def-/[min|max/attribute]
C         FORMAT: FILTER/ipstart/ipend/ipstep/ /[min|max/attribute]
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
C        $Log: filter.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.32   29 Aug 2007 10:53:50   gable
CPVCS    Added option [min|max/attribute] and some additional error checking.
CPVCS    
CPVCS       Rev 1.31   24 Dec 2003 10:20:24   tam
CPVCS    add het subroutine filter_subset() used with refine 'amr' iprd option 
CPVCS    
CPVCS       Rev 1.30   20 Aug 2003 07:49:56   gable
CPVCS    In cases where the specified pset was empty the
CPVCS    code would crash due to asking for zero lenght
CPVCS    array. Put in error check to return if pset is
CPVCS    empty set.
CPVCS    
CPVCS       Rev 1.29   03 Oct 2000 09:56:52   dcg
CPVCS    change variable name of ialias to ialiastmp to 
CPVCS    avoid confusion with mesh object attribute ialias
CPVCS
CPVCS       Rev 1.28   Thu Apr 06 09:02:48 2000   dcg
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.27   22 Mar 2000 13:39:26   gable
CPVCS    Changed screen IO, print out value of epsilonl used
CPVCS    for filter comparison. Output number of points
CPVCS    dudded even if the number is zero.
CPVCS
CPVCS       Rev 1.26   Wed Nov 10 14:30:28 1999   dcg
CPVCS    make xnoise a local variable
CPVCS
CPVCS       Rev 1.25   Tue May 18 10:29:12 1999   dcg
CPVCS    clean-up and comment
CPVCS
CPVCS       Rev 1.24   Fri Aug 28 14:25:54 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.23   Thu Jun 05 10:48:32 1997   dcg
CPVCS    use epsilon not epsilon squared to test for
CPVCS    minimum distance when setting number of bins
CPVCS    and bin width.
CPVCS
CPVCS       Rev 1.22   Mon Apr 14 16:47:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.21   Thu Apr 03 09:56:36 1997   dcg
CPVCS    change 'nint' to 'int'
CPVCS
CPVCS       Rev 1.20   Thu Apr 03 09:46:28 1997   dcg
CPVCS    add nint calls for mixed mode clarification on
CPVCS    min/max calculations
CPVCS
CPVCS       Rev 1.19   Sun Feb 23 10:43:04 1997   het
CPVCS    Change 1.01 to 0.01 and minimum ncube to 2.
CPVCS
CPVCS       Rev 1.18   Fri Jan 24 13:23:52 1997   het
CPVCS    Add the filter_points routine.
CPVCS
CPVCS       Rev 1.17   Thu Jan 23 10:46:34 1997   dcg
CPVCS    check neighboring bins for duplicates
CPVCS    check for more that one duplicate
CPVCS
CPVCS       Rev 1.16   Tue Dec 03 12:54:28 1996   het
CPVCS    Use a new filter algorithm that is based on binning
CPVCS    points in a regular background grid.
CPVCS
CPVCS       Rev 1.15   Fri Feb 16 21:50:06 1996   het
CPVCS    Filter from point 1 to n instead for n to 1.
CPVCS
CPVCS       Rev 1.14   Fri Dec 22 14:11:50 1995   het
CPVCS    Unpack the interface points into separate arrays.
CPVCS
CPVCS       Rev 1.13   12/05/95 08:20:40   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.12   11/16/95 15:21:54   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.11   11/07/95 17:17:28   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.10   09/05/95 15:49:00   dcg
CPVCS    fix defaults if no arguments on command line
CPVCS
CPVCS       Rev 1.9   08/02/95 09:59:38   ejl
CPVCS    Search through the points in reverse order
CPVCS
CPVCS       Rev 1.8   06/13/95 09:01:44   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.7   05/26/95 13:18:36   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.6   04/18/95 14:21:10   ejl
CPVCS    Installed print for number of pointes dudded.
CPVCS
CPVCS
CPVCS       Rev 1.5   02/18/95 06:56:32   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.4   01/23/95 16:58:42   het
CPVCS    Correct errors in the names for some cmo_get_info calles.
CPVCS
CPVCS
CPVCS       Rev 1.3   01/09/95 17:45:18   het
CPVCS
CPVCS
CPVCS       Rev 1.3   01/09/95 17:43:34   het
CPVCS    Unicos changes
CPVCS
CPVCS
CPVCS       Rev 1.2   01/04/95 22:02:22   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/23/94 17:07:44   het
CPVCS    Fix the filter command.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:58   pvcs
CPVCS    Original version.
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
      integer ierror

C
C
C#######################################################################
C
      integer nnodes
C
      pointer (ipisn1, isn1 )
      integer isn1(*)
 
      pointer (ipitp1, itp1 )
      integer itp1(*)
 
      pointer (ipisetwd, isetwd )
      integer isetwd(*)
 
      pointer (ipxic, xic )
      pointer (ipyic, yic )
      pointer (ipzic, zic )
      REAL*8 xic(*), yic(*), zic(*)
 
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(*), itetoff(*)
      pointer (ipitet, itet1 )
      integer itet1(*)
 
      pointer (ipialias, ialiastmp)
      integer ialiastmp(*)
 
      pointer (ipiparent, iparent)
      integer iparent(*)
 
      pointer (ipialiasf, ialiasf)
      integer ialiasf(*)

      pointer (ipint1, int1)
      integer int1(*)
 
      pointer (ipmodpnt, modpnt )
      integer modpnt(*)
 
      pointer (ipmpary , mpary )
      integer mpary(*)
 
      pointer(ipxfield,xfield)
      real*8 xfield(*)
      pointer(ipxfield,ifield)
      integer ifield(*)

      pointer (ipxicf, xicf)
      pointer (ipyicf, yicf)
      pointer (ipzicf, zicf)
      real*8 xicf(*), yicf(*), zicf(*)
      real*8 xnoise
      REAL*8 dsmin, dsmin1
 
      integer ipt1, ipt2, ipt3
      integer  i1
      integer len, itype, ierr, icscode, ierrw
      integer ilen_fld,ityp_fld,irank_fld, if_sort
      integer nelements,ierrdum,nptsmax,ipointi,ipointj,mpno,
     *  length,i,i2,icount,it
 
      integer icharlnf
 
      character*8 cglobal, cdefault, coption
      character*10 sort_type
      character*32 ich1,ich2,ich3
      character*32 isubname, cmo
      character*128 cattribute
      character*132 logmess
      character*1024 cmdmess
C
C#######################################################################
C BEGIN begin
C
      isubname='filter'
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
C     ******************************************************************
C
C     Return if you have an empty mesh object
C
      if(nnodes .eq. 0)then
         write(logmess,3005)
 3005    format('WARNING: FILTER Number of nodes = 0 ')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,3006)
 3006    format('WARNING: FILTER RETURN no action ')
         call writloga('default',0,logmess,0,ierr)
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
      if((nwds.lt.5) .or. 
     1   (xmsgin(5) .lt. 1.e-12*dsmin1) .or.
     2   (cmsgin(5) .eq. '-def-'))then
         dsmin=dsmin1
         write(logmess,9910) dsmin
 9910    format('FILTER:Use internal epsilonl value =',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      else
         dsmin=xmsgin(5)
         write(logmess,9915) dsmin
 9915    format('FILTER:User specified value = ',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      endif
      if(dsmin.lt.0.0) then
         write(logmess,9920)
 9920    format
     1   ('FILTER:INVALID INPUT, negative user value  ')
         dsmin=dsmin1
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,9925) dsmin
 9925    format('FILTER:Use internal epsilonl value =',e21.12)
         call writloga('default',1,logmess,0,ierrw)
      endif
C
C     Optional min/max attribute option
C
C     min option means that when nodes, i1, i2,  are marked for filtering
C     the node with att = min(attribute(i1), attribute(i2)) is kept.
C
C     max option means that when nodes, i1, i2,  are marked for filtering
C     the node with att = max(attribute(i1), attribute(i2)) is kept.
C
C
      if((nwds .gt. 5) .and. (nwds .le. 7))then
         if(msgtype(6) .eq. 3) then
            coption = cmsgin(6)
C           Valid options are min/max
            if((coption(1:icharlnf(coption)).ne.'min') .and.
     1         (coption(1:icharlnf(coption)).ne.'max'))then
               write(logmess,9926) coption(1:icharlnf(coption))
 9926          format('FILTER:Invalid option: ',a)
               call writloga('default',1,logmess,0,ierrw)
               goto 9999
            elseif(coption(1:icharlnf(coption)).eq.'min')then
               sort_type = 'descending'
            elseif(coption(1:icharlnf(coption)).eq.'max')then
               sort_type = 'ascending '
            endif
         else
            write(logmess,9927)
 9927       format('FILTER:Invalid option. Valid keywords min|max.')
            call writloga('default',1,logmess,0,ierrw)
            goto 9999
         endif
       endif
C
C     Locate MO attribute and collect pointer and other information.
C
      if_sort = 0
      if(nwds .ge. 7)then
      if(msgtype(7) .ne. 3) then
         write(logmess,9928)
 9928    format('FILTER:Invalid keyword. Attribute  must be character')
         call writloga('default',1,logmess,0,ierrw)
         goto 9999
      else
         if_sort = 1
         cattribute = cmsgin(7)(1:icharlnf(cmsgin(7)))
         call get_mo_attr(ipxfield,cmo,cattribute,
     *              ilen_fld,ityp_fld,irank_fld,ierr)
         if (ierr .ne. 0) go to 9999
C
C      min|max implementation is based on knowing that the standard filter
C      command keeps the node with the larger node number and deletes nodes
C      with smaller node numbers. Sort and Reorder the nodes based on the
C      user specified attribute, so that the node order will take advantage
C      of the standard behavior of filter.
C
         cmdmess = 'cmo/set_id/'//cmo//'/node/id_node_tmp_filter;finish'
         call dotask(cmdmess,ierr)
         cmdmess = 
     1   'sort /'//cmo//'/index/'//sort_type//
     2   '/itmp_filter_key/'//cattribute//
     3   ';finish'
         call dotask(cmdmess,ierr)
         cmdmess = 'reorder/'//cmo//'/itmp_filter_key;finish'
         call dotask(cmdmess,ierr)
      endif
      endif
c
C
C     FILTER THE SELECTED POINT SET.
c     move coordinates into temp locations
C
      length=mpno
      if(length .eq. 0)then
         write(logmess,9929)
 9929    format('FILTER:Point set is empty, no points to filter')
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
      call filter_points(mpno,xicf,yicf,zicf,dsmin,ialiasf)
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
C
      if(if_sort .eq. 1)then
         cmdmess = 
     1   'sort /'//cmo//'/index/ascending/'//
     2   'itmp_filter_key/id_node_tmp_filter'//
     3   ';finish'
         call dotask(cmdmess,ierr)
         cmdmess = 'reorder/'//cmo//'/itmp_filter_key;finish'
         call dotask(cmdmess,ierr)
         cmdmess = 'cmo/DELATT/'//cmo//'itmp_filter_key;finish'
         call dotask(cmdmess,ierr)
         cmdmess = 'cmo/DELATT/'//cmo//'id_node_tmp_filter;finish'
         call dotask(cmdmess,ierr)
      endif

         write(logmess,9930) icount
 9930    format('FILTER:Dudding duplicate points: ',1x,i10)
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
      subroutine filter_points(nnodes,xic,yic,zic,dsmin,ialiastmp)
C
C
C#######################################################################
C
C      PURPOSE -
C
C       THIS ROUTINE BINS THE NODES, IDENTIFIES THOSE WHICH ARE CLOSER
C       THAN dsmin AND RETURNS AN "ALIAS" LIST THAT CONTAINS THE NEW
C       NAMES OF THE POINTS INDICATING WHICH POINT THEY DUPLICATE.
C
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
C
C#######################################################################
C
      pointer (ipncoord_bin, ncoord_bin)
      pointer (ipncoord_off, ncoord_off)
      pointer (ipialias1, ialias1)
      integer ncoord_bin(*), ncoord_off(*)
      integer ialias1(*)
C
      integer length,  icscode
      integer  i1, i2, i, j, k,
     *        ix, iy, iz, ixpe, ixme, iype, iyme, izpe, izme,
     *        ncube, nx, ny, nz, nxyz, isum,
     *        ibin, ibinpe, ibinme, ibin1

      real*8 xnoise, dsmin1, dsminsq
      real*8 xmin1, ymin1, zmin1, xmax1, ymax1, zmax1,
     *       xavg1, yavg1, zavg1, dx1, dy1, dz1, distance

      real*8 alargenumber
      parameter (alargenumber=1.d+30)
C
C#######################################################################
C
      integer icc
      icc(i,j,k)=i+(j-1+(k-1)*(ny-1))*(nx-1)
C
      character*32 isubname
C
C#######################################################################
C BEGIN begin
C
      isubname='filter_points'
C
C     SET THE RETURN ERROR CODE.
C
      ierror = 0
c
c  set the test distance
c
      xnoise=1.0d-99
      dsminsq=dsmin**2+xnoise
C
c  initialize to the identity
c
      do i1=1,nnodes
         ialiastmp(i1)=i1
      enddo
C
C     FIND THE MAXIMUM AND MINIMUM EXTENTS OF the mesh
c     add a small tolerance to take care of epsilon issues
C
      xmin1=alargenumber
      ymin1=alargenumber
      zmin1=alargenumber
      xmax1=-xmin1
      ymax1=-ymin1
      zmax1=-zmin1
      do i=1,nnodes
         xmin1=min(xmin1,xic(i))
         ymin1=min(ymin1,yic(i))
         zmin1=min(zmin1,zic(i))
         xmax1=max(xmax1,xic(i))
         ymax1=max(ymax1,yic(i))
         zmax1=max(zmax1,zic(i))
      enddo
      xavg1=0.5*(xmin1+xmax1)
      yavg1=0.5*(ymin1+ymax1)
      zavg1=0.5*(zmin1+zmax1)
      dx1=abs(xmax1-xmin1)
      dy1=abs(ymax1-ymin1)
      dz1=abs(zmax1-zmin1)
      if(dx1.lt.dsmin) then
         dx1=0.0
      else
         xmin1=xmin1-0.000001*dx1
         xmax1=xmax1+0.000001*dx1
         dx1=abs(xmax1-xmin1)
      endif
      if(dy1.lt.dsmin)  then
         dy1=0.0
      else
         ymin1=ymin1-0.000001*dy1
         ymax1=ymax1+0.000001*dy1
         dy1=abs(ymax1-ymin1)
      endif
      if(dz1.lt.dsmin) then
         dz1=0.0
      else
         zmin1=zmin1-0.000001*dz1
         zmax1=zmax1+0.000001*dz1
         dz1=abs(zmax1-zmin1)
      endif
C
c  use as a first guess of the number of bins in one direction
c  100 or the cube root of the number of nodes which ever is smaller
C  If spread is less than input minimum distance
C  use only one bin in that dimension
C
      ncube=max(2,min(100,nint(nnodes**(1.0d+00/3.0d+00))))
      nx=ncube
      ny=ncube
      nz=ncube
      if(dx1.le.dsminsq) then
         nx=1
         dx1=0.0d+00
      else
         dx1=dx1/(nx-1)
      endif
      if(dy1.le.dsminsq) then
         ny=1
         dy1=0.0d+00
      else
         dy1=dy1/(ny-1)
      endif
      if(dz1.le.dsminsq) then
         nz=1
         dz1=0.0d+00
      else
         dz1=dz1/(nz-1)
      endif
c
c  can now get temporary memory for the bins and index to the bins
c
      nxyz=nx*ny*nz
      length=nxyz
      call mmgetblk('ncoord_bin',isubname,ipncoord_bin,length,1,icscode)
      call mmgetblk('ncoord_off',isubname,ipncoord_off,length,1,icscode)
c
c  for each node figure out then index of the bin it lies in (ix,iy,iz)
c  also figure out the index of the bin it would lie in
c  if a tolerance (2*dsminsq) were added to or subracted from
c  each of the coordinates
c  (ixpe,iype,izpe) and (ixpm,iypm,izpm)
c
      dsmin1=dsmin
      do i1=1,nnodes
         if(nx.eq.1) then
            ix=1
            ixpe=1
            ixme=1
         else
            ix=1+(xic(i1)-xmin1+0.5*dsmin1)/dx1
            ixpe=min(nx,int(1+(xic(i1)+2*dsminsq-xmin1+.5*dsmin1)/dx1))
            ixme=max( 1,int(1+(xic(i1)-2*dsminsq-xmin1+.5*dsmin1)/dx1))
         endif
         if(ny.eq.1) then
            iy=1
            iype=1
            iyme=1
         else
            iy=1+(yic(i1)-ymin1+0.5*dsmin1)/dy1
            iype=min(ny,int(1+(yic(i1)+2*dsminsq-ymin1+.5*dsmin1)/dy1))
            iyme=max( 1,int(1+(yic(i1)-2*dsminsq-ymin1+.5*dsmin1)/dy1))
         endif
         if(nz.eq.1) then
            iz=1
            izpe=1
            izme=1
         else
            iz=1+(zic(i1)-zmin1+0.5*dsmin1)/dz1
            izpe=min(nz,int(1+(zic(i1)+2*dsminsq-zmin1+.5*dsmin1)/dz1))
            izme=max( 1,int(1+(zic(i1)-2*dsminsq-zmin1+.5*dsmin1)/dz1))
         endif
c
c  get bin numbers
c
         ibin=icc(ix,iy,iz)
         ibinpe=icc(ixpe,iype,izpe)
         ibinme=icc(ixme,iyme,izme)
c
c  increment number of nodes in bins ibin, ibinpe, ibinpm
c
         ncoord_bin(ibin)=ncoord_bin(ibin)+1
         if(ibinpe.ne.ibin) then
            ncoord_bin(ibinpe)=ncoord_bin(ibinpe)+1
         endif
         if(ibinme.ne.ibin) then
            ncoord_bin(ibinme)=ncoord_bin(ibinme)+1
         endif
      enddo
c
c  from the number of nodes in each bin determine the
c  offset to each bin.
c
      isum=0
      do i=1,nxyz
         if(ncoord_bin(i).gt.0) then
            ncoord_off(i)=isum
            isum=isum+ncoord_bin(i)
         endif
         ncoord_bin(i)=0
      enddo
c
c get temporary memory for 'alias' or duplicate index array
c
      length=isum+1
      call mmgetblk('ialias1',isubname,ipialias1,length,1,icscode)
c
c again figure out what bins a node might fall in
c this time make a list of node number for each bin in ialias1.
c ncoord_off(ibin) will index the first node number in bin ibin.
c there will be ncoord_bin(ibin) node numbers stored in order at
c this location.
c
      do i1=1,nnodes
         if(nx.eq.1) then
            ix=1
            ixpe=1
            ixme=1
         else
            ix=1+(xic(i1)-xmin1+0.5*dsmin1)/dx1
            ixpe=min(nx,int(1+(xic(i1)+2*dsminsq-xmin1+.5*dsmin1)/dx1))
            ixme=max( 1,int(1+(xic(i1)-2*dsminsq-xmin1+.5*dsmin1)/dx1))
         endif
         if(ny.eq.1) then
            iy=1
            iype=1
            iyme=1
         else
            iy=1+(yic(i1)-ymin1+0.5*dsmin1)/dy1
            iype=min(ny,int(1+(yic(i1)+2*dsminsq-ymin1+.5*dsmin1)/dy1))
            iyme=max( 1,int(1+(yic(i1)-2*dsminsq-ymin1+.5*dsmin1)/dy1))
         endif
         if(nz.eq.1) then
            iz=1
            izpe=1
            izme=1
         else
            iz=1+(zic(i1)-zmin1+0.5*dsmin1)/dz1
            izpe=min(nz,int(1+(zic(i1)+2*dsminsq-zmin1+.5*dsmin1)/dz1))
            izme=max( 1,int(1+(zic(i1)-2*dsminsq-zmin1+.5*dsmin1)/dz1))
         endif
c
c  get the bin number from the indices
c
         ibin=icc(ix,iy,iz)
         ibinpe=icc(ixpe,iype,izpe)
         ibinme=icc(ixme,iyme,izme)
         ncoord_bin(ibin)=ncoord_bin(ibin)+1
c
c  store node number
c
         ialias1(ncoord_off(ibin)+ncoord_bin(ibin))=i1
         if(ibinpe.ne.ibin) then
            ncoord_bin(ibinpe)=ncoord_bin(ibinpe)+1
            ialias1(ncoord_off(ibinpe)+ncoord_bin(ibinpe))=i1
         endif
         if(ibinme.ne.ibin) then
            ncoord_bin(ibinme)=ncoord_bin(ibinme)+1
            ialias1(ncoord_off(ibinme)+ncoord_bin(ibinme))=i1
         endif
      enddo
c
c  now we look for duplicate nodes
c  first find bin node would be in
c
      do i1=1,nnodes
         if(nx.eq.1) then
            ix=1
         else
            ix=1+(xic(i1)-xmin1+0.5*dsmin1)/dx1
         endif
         if(ny.eq.1) then
            iy=1
         else
            iy=1+(yic(i1)-ymin1+0.5*dsmin1)/dy1
         endif
         if(nz.eq.1) then
            iz=1
         else
            iz=1+(zic(i1)-zmin1+0.5*dsmin1)/dz1
         endif
         ibin1=icc(ix,iy,iz)
c
c  look at all other nodes in this bin
c  check distance and set ialiastmp if needed
c
         do j=ncoord_off(ibin1)+1,
     *                  ncoord_off(ibin1)+ncoord_bin(ibin1)
            i2=ialias1(j)
            if(i1.lt.i2) then
               distance=(xic(i1)-xic(i2))**2 +
     *                    (yic(i1)-yic(i2))**2 +
     *                    (zic(i1)-zic(i2))**2
               if(distance.lt.dsminsq) then
                     ialiastmp(i1)=ialias1(j)
               endif
            endif
         enddo
      enddo
C
 9999 continue
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


*dk,filter_subset
      subroutine filter_subset(cmo,mpno,mpary,dsmin)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE SORTS THE MASS POINTS THAT ARE CLOSER THAN
C         dsmin AND RETURNS AN "ALIAS" LIST THAT CONTAINS THE NEW
C         NAMES OF THE POINTS INDICATING WHICH POINT THEY DUPLICATE.
C
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
C         ialias - THE NAMES OF EACH POINT. IF THIS IS NOT AN
C                     INDENITY THEN THIS POINT HAS BEEN FOUND
C                     TO BE CLOSE THE POINT INDEX CONTAINED IN
C                     ARRAY.
C
C      CHANGE HISTORY -
C
C        $Log: filter.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
C
C NOV 2003 tam
C Added from Harold's sgi code for the amr option of refinement
C with principal refine direction (prd) using topology
C Used with refine_hex_prd() which is the PRD version of refine_hex_add()
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "local_element.h"
      include "chydro.h"
C
      character*(*) cmo
      integer mpno, mpary(mpno)
      real*8 dsmin
C
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
      pointer (ipjtetoff, jtetoff)
      integer itettyp(*), itetoff(*), jtetoff(*)
      pointer (ipitet, itet1 )
      integer itet1(*)
      pointer (ipjtet, jtet1 )
      integer jtet1(*)
C
      pointer (ipialias, ialias)
      integer ialias(*)
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
C
C#######################################################################
C
      integer nnodes, nelements, mbndry
      integer len, itype, ierr, length, icscode, ierrw
      integer i1, it, i, i2, icount, ierror
C
      character*132 logmess
      character*32 isubname
C
C
C#######################################################################
C BEGIN begin
C
      isubname='filter_subset'
      ierror = 0
C
      call cmo_get_info('nnodes',cmo,nnodes,len,itype,ierr)
      if(ierr.ne.0) then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('nelements',cmo,nelements,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('mbndry',cmo,mbndry,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
C
      call cmo_get_info('isetwd',cmo,ipisetwd,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('itp1',cmo,ipitp1,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('isn1',cmo,ipisn1,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
C
      call cmo_get_info('xic',cmo,ipxic,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('yic',cmo,ipyic,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('zic',cmo,ipzic,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
C
      call cmo_get_info('itettyp',cmo,ipitettyp,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('itetoff',cmo,ipitetoff,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('jtetoff',cmo,ipjtetoff,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('itet',cmo,ipitet,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
      call cmo_get_info('jtet',cmo,ipjtet,len,itype,ierr)
      if(ierr.ne.0)then 
         call x3d_error(isubname,'cmo_get_info')
         ierror = 1
      endif
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
         if(icscode.ne.0)then 
           call x3d_error(isubname, 'mmgetblk')
           ierror = 1
          endif
      call unpackpc(nnodes,itp1,isn1,iparent)
C
C
C     ******************************************************************
C     FILTER THE SELECTED POINT SET.
C
      if (mpno .gt. nnodes) then
          write(logmess,9025) mpno,nnodes
 9025     format('ERROR filter_subset: mpno gt nnodes: ',i14,i14)
          call writloga('default',1,logmess,1,ierr)
          ierror = 1
          go to 9999
      endif

      length=mpno
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
      call filter_points(mpno,xicf,yicf,zicf,dsmin,ialiasf)
      length=nnodes
      call mmgetblk('ialias',isubname,ipialias,length,1,icscode)
      do i1=1,nnodes
         ialias(i1)=i1
      enddo
      do i=1,mpno
         i1=mpary(i)
         i2=mpary(ialiasf(i))
         ialias(i1)=i2
      enddo
C
      icount=0
      do i2=1,nnodes
         i1=ialias(i2)
         if(i1.ne.i2) then
            if(iparent(i1).ne.iparent(i2)) then
               icount=icount+1
               itp1(i2)=ifitpdud
            else
               ialias(i2)=i2
            endif
         endif
      enddo
C
      do it=1,nelements
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            itet1(itetoff(it)+i)=ialias(i1)
         enddo
      enddo
C
      if(icount.gt.0) then
         write(logmess,9930) icount
 9930    format(' Dudding duplicate points: ',1x,i10)
         call writloga('default',1,logmess,0,ierrw)
      endif
C
      goto 9999
 9999 continue
      if (ierror .gt. 0) then
         write(logmess,'(a)') 
     *   'ERROR filter_subset: dudded nodes not marked.'
          call writloga('default',1,logmess,1,ierrw)
      endif

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

