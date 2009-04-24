      subroutine intradd(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE ADDS THE COUPLED (CHILD) INTERFACE POINTS TO THE
C        COUPLED CHAINS FOR EACH PARENT INTERFACE POINT.
C        invoked by the settets command
C        options are:
c          settets set the itetclr attribute and create parent/child
c                  chains
C                  for elements containing one non-interface point,
C                  itetclr is set to the imt1 value of that point.
C                  for elements all of whose nodes are interface, the
C                  centroid of the element is calculated and if the
C                  centroid is in a material region the itetclr is
c                  set to that value.  If the centroid is not in a
C                  material region, then it must be on a surface, in
C                  this case all vertices of the elements are looked
C                  at to determine a material common to all vertices,
C                  and itetclr is set to this value
c          settets/newtets same as settets but elements with
C                  itetclr>0 are not changed
c          settets/parents create parent/child chains
C          settets/geometry set the itetclr attribute and create
c               parent/child   chains  -use the centroid of the
C               element to determince the value of itetclr
c          settets/color_tets same as settets
C          settets/color_points use itetclr to set all the node imt1
c               attribute values
C          settets/repair same as settets, but checks that all
C                non-interface nodes of an element are the same
C                material - if not the nodes are changed to interface
C                type and the face containing them is marked as an
C                material interface face
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: intradd.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   24 Apr 2001 10:19:58   jan
CPVCS    deleted duplicate definition for epsilon1
CPVCS    
CPVCS       Rev 1.9   10 Apr 2001 10:54:14   dcg
CPVCS    add consts.h and use zero and one in place
CPVCS    of single precision values
CPVCS    change 'real(nen)' to dble(nen)
CPVCS    
CPVCS       Rev 1.8   21 Apr 2000 07:06:00   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.7   28 Mar 2000 14:09:16   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.6   08 Mar 2000 16:23:04   dcg
CPVCS    call geniee if need to change mbndry - this will get
CPVCS    correct jtet values for boundary and interface faces
CPVCS    
CPVCS       Rev 1.5   Thu Feb 03 09:01:58 2000   dcg
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 08:52:58 2000   dcg
CPVCS    
CPVCS       Rev 1.3   24 Jan 2000 15:04:40   jtg
CPVCS    modifed to use jtet_cycle_max to limit jtet loops
CPVCS    does not yet have jtet_reduce_nnd (elements with repeated nodes) modifications
CPVCS    
CPVCS       Rev 1.2   24 Jan 2000 13:24:52   dcg
CPVCS
CPVCS       Rev 1.77   Tue Nov 30 17:03:16 1999   jtg
CPVCS    now can handle jtet loops, and if mbndry=0 it uses the convention
CPVCS    jtet<0 instead of jtet>mbndry on interfaces
CPVCS
CPVCS       Rev 1.76   Wed Nov 10 14:37:18 1999   dcg
CPVCS    make nonintr a local variable and set it to 0
CPVCS
CPVCS       Rev 1.75   Tue Oct 12 16:46:06 1999   dcg
CPVCS     check for valid mbndry and reset if needed
CPVCS
CPVCS       Rev 1.74   Mon Sep 20 15:48:52 1999   jtg
CPVCS    ifitpvrt->ifitpvin added (see lines with "ifitpv")
CPVCS
CPVCS       Rev 1.73   Thu Sep 09 17:08:14 1999   jtg
CPVCS    nef was used instead of nefcmo in a few spots, and
CPVCS    the loop checking jtet vs matls was only correct if nef=nefcmo.
CPVCS    These are now fixed.
CPVCS
CPVCS       Rev 1.72   Wed Jan 20 14:15:26 1999   dcg
CPVCS    fix mmgetblk length for iseedtet
CPVCS
CPVCS       Rev 1.71   Wed Jan 13 09:37:26 1999   dcg
CPVCS    child creation portion of the subroutine rewritten
CPVCS    to conserve memory
CPVCS    uses the new subroutine get_materials_around_node
CPVCS
CPVCS       Rev 1.69   Thu Sep 03 14:55:54 1998   dcg
CPVCS    if all else fails, set element color to the color
CPVCS    of the first node of the element
CPVCS
CPVCS       Rev 1.67   Fri Jun 19 09:40:26 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.66   Fri May 22 09:11:54 1998   dcg
CPVCS    add repair option to fix multimaterial tets
CPVCS
CPVCS       Rev 1.65   Thu Apr 30 15:17:30 1998   dcg
CPVCS    fix wasteful memory usage
CPVCS
CPVCS       Rev 1.63   Wed Mar 11 15:52:50 1998   dcg
CPVCS    add settets/newtets option that will not change any
CPVCS    existing itetclrs
CPVCS
CPVCS       Rev 1.62   Mon Nov 24 16:34:18 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.60   Wed Oct 08 16:53:58 1997   dcg
CPVCS    fix number of arguments in calls to x3d_error
CPVCS
CPVCS       Rev 1.59   Mon Apr 14 16:52:06 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.58   Fri Mar 07 11:53:44 1997   dcg
CPVCS    restore original logic for default -all- case
CPVCS    create child points for virtual interfaces with same material
CPVCS    for both children
CPVCS
CPVCS       Rev 1.57   Tue Feb 25 21:20:24 1997   het
CPVCS    Add a new option that converts prisms to 18 tets.
CPVCS
CPVCS       Rev 1.56   Sun Feb 23 10:36:22 1997   het
CPVCS    Don't write out all the multimaterial tets. Just the first 30.
CPVCS
CPVCS       Rev 1.55   Mon Nov 11 21:00:26 1996   het
CPVCS    Initialize an unitialized variable.
CPVCS
CPVCS       Rev 1.53   Wed Jul 24 17:32:42 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.52   Thu Jun 27 15:26:00 1996   dcg
CPVCS    add constrainv command to create xcontab table as needed
CPVCS
CPVCS       Rev 1.51   Wed Jun 19 10:18:50 1996   dcg
CPVCS    fixes to merge resulting from surface type changes
CPVCS
CPVCS       Rev 1.50   Mon Jun 03 14:06:06 1996   dcg
CPVCS    'virtual' interface changes
CPVCS
CPVCS       Rev 1.49   Mon Apr 29 14:38:00 1996   dcg
CPVCS    change call from cvmgt to cvmgtr to match argument types
CPVCS
CPVCS       Rev 1.48   Wed Mar 27 09:34:28 1996   dcg
CPVCS    if center of element is on surface look for
CPVCS    material common to all vertices to use as element color
CPVCS
CPVCS       Rev 1.47   Tue Mar 05 12:49:52 1996   dcg
CPVCS    remove icn1, int1
CPVCS
CPVCS       Rev 1.46   Mon Feb 26 15:47:58 1996   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.45   Thu Feb 15 09:42:22 1996   dcg
CPVCS    set child points from parent point information
CPVCS
CPVCS       Rev 1.44   Wed Jan 31 12:58:28 1996   het
CPVCS    Convert interface points to either interior or boundary points.
CPVCS
CPVCS       Rev 1.43   Fri Jan 26 11:45:46 1996   het
CPVCS    Give zero material elements the default color of MAT_MAX+1
CPVCS
CPVCS       Rev 1.42   Tue Jan 23 09:35:20 1996   dcg
CPVCS    add points types to call to getregv1
CPVCS
CPVCS       Rev 1.41   Tue Jan 23 09:25:32 1996   het
CPVCS    Fix an error in copying isn1 numbers.
CPVCS
CPVCS       Rev 1.40   Fri Dec 22 14:18:12 1995   het
CPVCS    Correct errors for inside_ routines.
CPVCS
CPVCS       Rev 1.39   12/05/95 08:20:26   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.38   11/17/95 15:23:28   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.37   11/16/95 17:07:22   het
CPVCS    Create the getregv1 routine as a special case.
CPVCS
CPVCS       Rev 1.36   11/07/95 17:19:28   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.35   10/19/95 09:34:06   het
CPVCS    Define a pointer for xweight
CPVCS
CPVCS       Rev 1.34   10/19/95 06:21:26   het
CPVCS    Assign reference points to isolated tets.
CPVCS
CPVCS       Rev 1.33   10/18/95 17:11:36   het
CPVCS    Fix the generation of children
CPVCS
CPVCS       Rev 1.32   10/18/95 13:20:46   het
CPVCS    Correct an error with boundary tets with no color
CPVCS
CPVCS       Rev 1.31   10/18/95 12:16:14   het
CPVCS    Propagate all attributes from parents to children
CPVCS
CPVCS       Rev 1.30   10/12/95 16:44:50   het
CPVCS    Correct the X3D restart dump routines
CPVCS
CPVCS       Rev 1.29   10/05/95 15:46:38   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.28   09/29/95 09:12:46   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.27   08/29/95 11:42:10   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.26   08/23/95 06:56:24   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.25   08/22/95 06:50:02   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.24   08/15/95 18:19:44   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.23   06/13/95 09:01:54   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.22   06/07/95 15:29:30   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.21   05/24/95 11:30:38   het
CPVCS    Correct an error for single material problems
CPVCS
CPVCS       Rev 1.20   05/16/95 11:49:32   ejl
CPVCS    Fixed error with idsbs(3) to idsbs(10)
CPVCS
CPVCS       Rev 1.19   05/16/95 08:48:22   ejl
CPVCS    Fixed type with icsode to icscode.
CPVCS
CPVCS       Rev 1.18   05/15/95 13:37:06   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.17   05/12/95 13:58:54   ejl
CPVCS    Fixed memory management error
CPVCS
CPVCS       Rev 1.15   05/01/95 08:33:32   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.14   03/31/95 09:07:38   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.13   03/30/95 05:00:12   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.12   03/28/95 12:35:36   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.11   03/23/95 15:19:06   dcg
CPVCS    changed interfac to intrface for consistency
CPVCS
CPVCS       Rev 1.10   03/23/95 15:06:50   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.9   03/22/95 13:19:48   dcg
CPVCS    update ipointi,ipointj - fix ialias for added points
CPVCS
CPVCS       Rev 1.8   03/10/95 17:15:14   dcg
CPVCS     insert get mesh object calls
CPVCS
CPVCS       Rev 1.7   02/12/95 08:41:52   het
CPVCS    Correct an error in setting itp1() and icr1() values
CPVCS
CPVCS       Rev 1.6   01/26/95 11:18:46   het
CPVCS    Assign (un)initialized variables when no interfaces are present.
CPVCS
CPVCS
CPVCS       Rev 1.5   01/11/95 21:46:32   het
CPVCS    Correct errors associated with the rzbrick and rzs commands.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/09/95 09:35:42   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.3   12/21/94 12:29:50   het
CPVCS    Correct some cmo errors.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/19/94 08:27:08   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/09/94 22:37:02   het
CPVCS    Added the new cmo_ calles.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:32   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
      include "chydro.h"
      include 'geom_lg.h'
      include 'consts.h'
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C ######################################################################
C
      character*32 cmo
C
      integer npoints, ntets, mbndry, jtet_cycle_max, jtet_reduce_nnd
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipint1, int1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      integer imt1(*), itp1(*), int1(*),
     *        icr1(*), isn1(*)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(*), yic(*), zic(*)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
C
      pointer (ipitet, itet1)
      integer itet1(*)
C
      pointer (ipjtet, jtet1)
      integer jtet1(*)
C
C#######################################################################
C
      pointer (ipitmat  , itmat   )
      integer itmat(*)
      pointer (ipitmatp , itmatp  )
      integer itmatp(*)
      pointer (iptetdst , tetdist )
      integer tetdist(*)
      pointer (ipitlist , itlist  )
      integer itlist(*)
      pointer (ipint2   , int2    )
      integer int2(*)
      pointer (ipintpts , intpts  )
      integer intpts(*)
      pointer (ipireal1, ireal1  )
      integer ireal1(*)
      pointer (ipibnd1 , ibnd1  )
      integer ibnd1(*)
      pointer (ipivrt1 , ivrt1  )
      integer ivrt1(*)
      pointer (ipimat , imat  )
      integer imat(*)
      pointer (ipipar , ipar  )
      integer ipar(*)
      pointer (ipiparent , iparent )
      integer iparent(*)
      pointer (ipiseedtet , iseedtet  )
      integer iseedtet(*)
 
C
      pointer (ipxcntr1, xcntr1)
      pointer (ipycntr1, ycntr1)
      pointer (ipzcntr1, zcntr1)
      real*8 xcntr1(*), ycntr1(*), zcntr1(*)
C
      pointer (ipiregno, iregno1)
      pointer (ipisurfno, isurfno1)
      integer iregno1(*), isurfno1(*)
      pointer (ipmtrls, mtrls)
      integer mtrls(*)
      pointer (ipstack, stack)
      integer stack(*)
C
      character*32 isubname
      character*8 cglobal, cdefault, cmolength
C
      character*132 logmess
C
      character*32 coption
      integer index1,ifun1,jfun1,imtmax,i,j,k,i1,i2,i3,i4,nonintr,
     *  ierrw,ilen,itype,ierr,icscode,ierr1,
     *  it,itetmats,len0,nen,jt,icount_mat_save,icount_mat,
     *  iter_mat,length,npts,ics,nmtst,l,
     *   itst,ii2,ifmin,itetmatp,nef,jt1,jt2,
     *   it2,itetmato,ict,lenm,lens,
     *   nintpts,ierrwrt,mbndry_old,
     *   ict1,ict2,imtsum,
     *   imtsav,isn,icount,
     *   icount_matp_save,icount_matp,n1,it1,n,
     *   jtoff,ierrdum,ityp,nefcmo,i1add,iter_matp,nnmtst,node1,
     *   node2,imt0,ier,jf,imttet,isnsave,npointsave,ninc,nmtrls,
     *   itin,naddmax,nf,nn,maxmat,icycle
      logical ibadface,itsttp,ibadtet
      real*8 crosx1, crosy1, crosz1,volume,srchval,xcntr,
     *   ycntr,zcntr,xfac,x0,y0,z0,
     *   x1,y1,z1
      integer icharlnf,ismin,iimax
      real*8 cvmgtr
C
C#######################################################################
C
      integer nadd
      pointer (iplist, list)
      integer list(*)
      pointer (ipilist, ilist)
      integer ilist(1,*)
      pointer (ipxweight, xweight)
      real*8 xweight(1,*)
C
      pointer (ipi1chk1, i1chk1)
      integer i1chk1(*)
      pointer (ipnmts1, nmts1)
      pointer (ipimts1, imts1)
      integer nmts1(*), imts1(*)
      pointer (ipxicf, xicf)
      pointer (ipyicf, yicf)
      pointer (ipzicf, zicf)
      real*8 xicf(*), yicf(*), zicf(*)
      pointer (ipfound,ifound)
      integer ifound(*)
C
C#######################################################################
C
C
C#######################################################################
C
C     MACROS.
C
      index1(ifun1,jfun1) = ifun1 + (jfun1-1)*imtmax
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
C
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C#######################################################################
C
C
C
c
c  nonintr = 0 disables alternate algorithm
c
      nonintr=0
      isubname = 'intradd'
      cglobal='global'
      cdefault='default'
C
      ierror = 0
C
C     ******************************************************************
C     Get the option to perform
C
      if(nwds.le.1) then
         coption='-all-'
      elseif(msgtype(2).ne.3) then
         coption='-all-'
      else
         coption=cmsgin(2)
      endif
C
C
C     ******************************************************************
C     Get mesh object.
C
      call cmo_get_name(cmo,ierror)
C
      if(ierror.ne.0) then
        write(logmess,'(a)') 'SETTETS found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
C
C     ******************************************************************
C     Get mesh object data.
C
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
c
c  check mbndry value
c
      call cmo_get_info('mbndry', cmo, mbndry, ilen, itype, ierr)
      if(ierr.ne.0) mbndry=0
      mbndry_old=mbndry
C        call checkmbndry_lg
      call set_mbndry()
      call cmo_get_info('mbndry', cmo, mbndry, ilen, itype, ierr)
      if(ierr.ne.0) mbndry=0
C
C   This is using a call to geniee to reset mbndry values in
C   jtet array. I think this could just as easily be done with
C   the call to set_mbndry but I'll leave it for now.
C   (If it ain't broke, don't fix it.)
C    CWG
C
      if(mbndry.ne.mbndry_old) call geniee_cmo(cmo)
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
 
      ! check if anything to do
      if(npoints.le.0 .or. ntets.le.0) goto 9999
 
      call cmo_get_info('faces_per_element',cmo,nefcmo,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('imt1', cmo, ipimt1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call mmgetblk('int1',isubname,ipint1,npoints,1,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'mmgetblk')
      call cmo_get_info('icr1', cmo, ipicr1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic', cmo, ipxic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic', cmo, ipyic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic', cmo, ipzic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itetclr', cmo, ipitetclr,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff', cmo, ipjtetoff,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet', cmo, ipjtet, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('jtet_cycle_max', cmo, jtet_cycle_max
     &                 , ilen, itype, ierr)
      if(ierr.ne.0 .or. jtet_cycle_max.lt.2) jtet_cycle_max=2
      call cmo_get_info('jtet_reduce_nnd', cmo, jtet_reduce_nnd
     &                 , ilen, itype, ierr)
      if(ierr.ne.0) jtet_reduce_nnd=0
      if (jtet_reduce_nnd.eq.1) then
         write(logmess,*) 'WARNING: intradd.f not jtet_reduce_nnd safe'
         call writloga(cdefault,0,logmess,0,ier)
      endif
C
C     ******************************************************************
C     SET UP TEMPORARY MEMORY-MANAGED ARRAYS.
C
      call mmgetblk('itmat',isubname,ipitmat,ntets,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('itmatp',isubname,ipitmatp,ntets,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('itlist',isubname,ipitlist,ntets,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call mmgetblk('int2',isubname,ipint2,npoints,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('intpts',isubname,ipintpts,npoints,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL REAL NODES.
C          IREAL1 = 1  -> Real Node.
C          IREAL1 = 0  -> Not a real node.
      length=npoints
      call mmgetblk('ireal1',isubname,ipireal1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE INTERFACE NODES.
C          INT1 = 1  -> Interface Node.
C          INT1 = 0  -> Interior Node.
C
      call unpacktp('intrface','set',npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
C     __________________________________________________________________
C     COUNT THE NUMBER OF INTERFACE NODES.
C
      nintpts=0
      do i1=1,npoints
         if(isn1(i1).le.0.and.int1(i1).ne.0) then
            nintpts=nintpts+1
            intpts(nintpts)=i1
            int2(i1)=nintpts
         else
            int2(i1)=0
         endif
      enddo
C
C     ******************************************************************
C     SET UP AN ARRAY THAT IDENTIFIES THE MATERIAL AND A POINT OF
C     THAT MATERIAL FOR EACH TETRAHEDRON UNDER THE ASSUMPTION THAT
C     EACH TETRAHEDRON IS SINGLE-MATERIAL.
C
      do it = 1,ntets
         itmat(it)  = 0
         itmatp(it) = 0
      enddo
C
C     __________________________________________________________________
C     ASSIGN MATERIAL TO TETRAHEDRA THAT CONTAIN AT LEAST ONE
C     INTERIOR REAL POINT.
C
      len0=icharlnf(coption)
      if(coption(1:len0).eq.'-all-'.or.
     *   coption(1:len0).eq.'color_tets') then
         itetmats = 0
         do it = 1,ntets
            nen=nelmnen(itettyp(it))
            j=0
            do while((j .lt. nen) .and. (itmat(it) .eq. 0))
               j=j+1
               i1=itet1(itetoff(it)+j)
               if (ireal1(i1) .eq. 1) then
                  if(int1(i1) .eq. 0 .or.
     *              (int1(i1) .eq. 1 .and. isn1(i1) .gt. 0)) then
                     itetmats = itetmats+1
                     itmat(it) = imt1(i1)
                     itmatp(it) = i1
                  endif
               endif
            enddo
         enddo
      elseif(coption(1:len0).eq.'repair') then
        itetmats = 0
        do it=1,ntets
           ibadtet=.false.
           imttet=0
           do i=1,nelmnef(itettyp(it))
c     check if material types of all nodes on the face
C     if there are more than one non-interface node material type
C     we have a multimaterial tet - make this face an
c     interface face and change point types
              imt0=0
              ibadface=.false.
              do j=1,ielmface0(i,itettyp(it))
                 node1=itet1(itetoff(it)+
     *              ielmface1(j,i,itettyp(it)))
                 if(.not.itsttp('intrface',itp1(node1))) then
                    if(imt0.eq.0) then
                       imt0=imt1(node1)
                       node2=node1
                       if(imttet.eq.0) imttet=imt0
                    else
                       if(imt1(node1).ne.imt0.or.ibadface) then
                          ibadface=.true.
                          nintpts=nintpts+1
                          int1(node1)=1
                          int2(node1)=nintpts
                          intpts(nintpts)=node1
                          if(itp1(node1).eq.ifitpint) then
                             itp1(node1)=ifitpini
                          elseif(itp1(node1).eq.ifitpvrt) then
                             itp1(node1)=ifitpvin
                          elseif(itp1(node1).eq.ifitprfl) then
                             itp1(node1)=ifitpirb
                          elseif(itp1(node1).eq.ifitpfre) then
                             itp1(node1)=ifitpifb
                          elseif(itp1(node1).eq.ifitprfb) then
                             itp1(node1)=ifitpirf
                          elseif(itp1(node1).eq.ifitpvrb) then
                             itp1(node1)=ifitpvir
                          elseif(itp1(node1).eq.ifitpvfb) then
                             itp1(node1)=ifitpvif
                          elseif(itp1(node1).eq.ifitpvrf) then
                             itp1(node1)=ifitpalb
                          else
                             write(logmess,7) it,node1,itp1(node1)
 7                           format(' Illegal node type for tet ',
     *                         i8,' node ',i8, ' type ',i5,
     *                          ' set to interface')
                             call writloga(cdefault,0,logmess,0,ier)
                             itp1(node1)=ifitpini
                          endif
                       endif
                    endif
                 endif
               enddo
               if(ibadface) then
                  ibadtet=.true.
                  nintpts=nintpts+1
                  int1(node2)=1
                  int2(node2)=nintpts
                  intpts(nintpts)=node2
                  if(itp1(node2).eq.ifitpint) then
                     itp1(node2)=ifitpini
                  elseif(itp1(node2).eq.ifitpvrt) then
                     itp1(node2)=ifitpvin
                  elseif(itp1(node2).eq.ifitprfl) then
                     itp1(node2)=ifitpirb
                  elseif(itp1(node2).eq.ifitpfre) then
                     itp1(node2)=ifitpifb
                  elseif(itp1(node1).eq.ifitprfb) then
                     itp1(node2)=ifitpirf
                  elseif(itp1(node2).eq.ifitpvrb) then
                     itp1(node2)=ifitpvir
                  elseif(itp1(node2).eq.ifitpvfb) then
                     itp1(node1)=ifitpvif
                  elseif(itp1(node2).eq.ifitpvrf) then
                     itp1(node2)=ifitpalb
                  else
                     write(logmess,7) it,node2,itp1(node2)
                     call writloga(cdefault,0,logmess,0,ier)
                     itp1(node2)=ifitpini
                 endif
                 if((jtet1(jtetoff(it)+i).lt.mbndry.and.mbndry.gt.0)
     &           .or.(jtet1(jtetoff(it)+i).gt.0.and.mbndry.le.0)) then
                   if (mbndry.gt.0) then
                     jtet1(jtetoff(it)+i)=jtet1(jtetoff(it)+i)+mbndry
                     jt1=jtet1(jtetoff(it)+i)-mbndry
                   else
                     jtet1(jtetoff(it)+i)=-jtet1(jtetoff(it)+i)
                     jt1=-jtet1(jtetoff(it)+i)
                   endif
                   jt2=jt1
                   jt=1+(jt1-1)/nefcmo
                   jf=jt1-(jt-1)*nefcmo
                   jt1=jtet1(jtetoff(jt)+jf)
                   if (mbndry.gt.0) then
                      if (jt1.lt.mbndry) then
                         jtet1(jtetoff(jt)+jf)=jt1+mbndry
                      else
                         jt1=jt1-mbndry
                      endif
                   else
                      if (jt1.gt.0) then
                         jtet1(jtetoff(jt)+jf)=-jt1
                      else
                         jt1=-jt1
                      endif
                   endif
                   icycle=0
                   do while (jt1.ne.jt2.and.icycle.lt.jtet_cycle_max)
                      icycle=icycle+1
                      jt=1+(jt1-1)/nefcmo
                      jf=jt1-(jt-1)*nefcmo
                      jt1=jtet1(jtetoff(jt)+jf)
                      if (mbndry.gt.0) then
                         if (jt1.lt.mbndry) then
                            jtet1(jtetoff(jt)+jf)=jt1+mbndry
                         else
                            jt1=jt1-mbndry
                         endif
                      else
                         if (jt1.gt.0) then
                            jtet1(jtetoff(jt)+jf)=-jt1
                         else
                            jt1=-jt1
                         endif
                      endif
                   enddo
                 endif
               endif
            enddo
            if(imttet.ne.0.and..not.ibadtet) then
               itetmats=itetmats+1
               itmat(it)=imttet
               itmatp(it)=node1
            endif
         enddo
      elseif(coption(1:len0).eq.'geometry') then
         itetmats = 0
      elseif(coption(1:len0).eq.'color_points') then
         itetmats = 0
         do it = 1,ntets
            nen=nelmnen(itettyp(it))
            do i=1,nen
               i1=itet1(itetoff(it)+i)
               imt1(i1)=itetclr(it)
            enddo
            j=0
            if(itetclr(it).gt.0) then
               itetmats=itetmats+1
               itmat(it)=itetclr(it)
            else
               do while((j .lt. nen) .and. (itmat(it) .eq. 0))
                  j=j+1
                  i1=itet1(itetoff(it)+j)
                  if (int1(i1) .eq. 0) then
                     itetmats = itetmats+1
                     if(itetclr(it).le.0) then
                        if(ireal1(i1).eq.1) itmat(it) = imt1(i1)
                     else
                        itmat(it) = itetclr(it)
                     endif
                     itmatp(it) = i1
                  endif
               enddo
            endif
         enddo
      elseif(coption(1:len0).eq.'newtets') then
         itetmats = 0
         do it = 1,ntets
            itmat(it) = itetclr(it)
            if(itmat(it).gt.0) itetmats=itetmats+1
            nen=nelmnen(itettyp(it))
            j=0
            do while((j .lt. nen) .and. (itmat(it) .eq. 0))
               j=j+1
               i1=itet1(itetoff(it)+j)
               if (int1(i1) .eq. 0) then
                  itetmats = itetmats+1
                  if(itetclr(it).le.0) then
                     if(ireal1(i1).eq.1) itmat(it) = imt1(i1)
                  else
                     itmat(it) = itetclr(it)
                  endif
                  itmatp(it) = i1
               endif
            enddo
         enddo
      elseif(coption(1:len0).eq.'parents') then
         itetmats = 0
         do it = 1,ntets
            nen=nelmnen(itettyp(it))
            j=0
            do while((j .lt. nen) .and. (itmat(it) .eq. 0))
               j=j+1
               i1=itet1(itetoff(it)+j)
               if (int1(i1) .eq. 0) then
                  itetmats = itetmats+1
                  if(itetclr(it).le.0) then
                     if(ireal1(i1).eq.1) itmat(it) = imt1(i1)
                  else
                     itmat(it) = itetclr(it)
                  endif
                  itmatp(it) = i1
               endif
            enddo
         enddo
      else
         write(logmess,3) coption(1:len0)
 3       format (' Unimplemented SETTETS option: ',a)
         call writloga (cdefault,0,logmess,0,ier)
      endif
      call mmrelblk('ireal1',isubname,ipireal1,icscode)
C
C
C     ******************************************************************
C     CHECK TO SEE THAT EACH TET HAS AN INDEX POINTER TO AN INTERIOR
C        POINT. SINCE A TET CAN HAVE ALL BOUNDARY POINTS A TET MAY HAVE
C        TO GRAB A NON-LOCAL POINT AS ITS REFERENCE. THIS WOULD SEEM THE
C        BEST WE CAN DO.
C
      icount_mat=0
      do it=1,ntets
         if(itmat(it).le.0) icount_mat=icount_mat+1
      enddo
      iter_mat=1
      dowhile(icount_mat.gt.0.and.iter_mat.gt.0.and.
     *   coption(1:len0).ne.'repair')
        icount_mat_save=icount_mat
        icount_mat=0
        do it=1,ntets
           i1=itet1(itetoff(it)+1)
           if(itmat(it).le.0) then
              do i=1,nelmnef(itettyp(it))
                 jtoff=jtetoff(it)+i
                 ! this jtet-safe only if all 3-loop get mbndry
                 if(jtet1(jtoff).gt.0.and.
     &                  (mbndry.le.0.or.jtet1(jtoff).lt.mbndry)) then
                    jt=1+(jtet1(jtoff)-1)/nefcmo
                    if(itmat(jt).gt.0.and.itmat(it).le.0) then
                       itetmats=itetmats+1
                       itmat(it)=itmat(jt)
                       itmatp(it)=i1
                    endif
                 endif
              enddo
              if(itmat(it).le.0) icount_mat=icount_mat+1
           endif
        enddo
        if(icount_mat.ge.icount_mat_save) then
           iter_mat=-iter_mat
        else
           iter_mat=iter_mat+1
        endif
      enddo
C
      itetmatp = itetmats
C
      if (itetmats .lt. ntets) then
C
C        _______________________________________________________________
C        ASSIGN MATERIAL TO TETRAHEDRA THAT CONTAIN ONLY INTERFACE
C        POINTS or that need repair.
C
         write(logmess,9000) ntets-itetmats
 9000    format(' SETTETS required ITMAT step 2 for ',i10,' tets')
         call writloga('default',0,logmess,0,ierrw)
C
c if mregions are defined use that info
c
         if(nmregs.gt.0) then
C           ............................................................
C           GET THE SEARCH RANGE.
C
            call get_epsilon('epsilonl', srchval)
C
C           ............................................................
C           MREGION DATA AVAILABLE TO SET TETRAHEDRAL MATERIAL INDEX.
C
            length=ntets
            call mmgetblk('xcntr1',isubname,ipxcntr1,length,2,icscode)
            call mmgetblk('ycntr1',isubname,ipycntr1,length,2,icscode)
            call mmgetblk('zcntr1',isubname,ipzcntr1,length,2,icscode)
            call mmgetblk('iregno',isubname,ipiregno,length,2,icscode)
            call mmgetblk('isurfno',isubname,ipisurfno,length,1,icscode)
C
            npts=0
            do it = 1,ntets
               if (itmat(it) .eq. 0) then
C
                  xcntr=zero
                  ycntr=zero
                  zcntr=zero
                  nen=nelmnen(itettyp(it))
                  do j=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+j)
                     xcntr=xcntr+xic(i1)
                     ycntr=ycntr+yic(i1)
                     zcntr=zcntr+zic(i1)
                  enddo
                  xfac=one/dble(nen)
                  npts=npts+1
                  xcntr1(npts)=xfac*xcntr
                  ycntr1(npts)=xfac*ycntr
                  zcntr1(npts)=xfac*zcntr
C
               endif
            enddo
C
            call getregv1(xcntr1,ycntr1,zcntr1,itp1,npts,srchval,
     &                    'mregion',0,cmo,
     &                    iregno1,isurfno1,
     &                    ierr)
C
            npts=0
            do it = 1,ntets
               if (itmat(it) .eq. 0) then
                  npts=npts+1
C
                  if (iregno1(npts) .gt. 0) then
                     itetmats = itetmats+1
                     itmat(it) = iregno1(npts)
                  endif
C
               endif
            enddo
            call mmrelblk('xcntr1',isubname,ipxcntr1,icscode)
            call mmrelblk('ycntr1',isubname,ipycntr1,icscode)
            call mmrelblk('zcntr1',isubname,ipzcntr1,icscode)
            call mmrelblk('iregno',isubname,ipiregno,icscode)
            call mmrelblk('isurfno',isubname,ipisurfno,icscode)
C
            if (itetmats .lt. ntets) then
C
               write(logmess,9020) ntets-itetmats
 9020          format(' SETTETS required ITMAT step 3 for ',i10,' tets')
               call writloga('default',0,logmess,0,ierrwrt)
C
               length=npoints
               call mmgetblk('i1chk1',isubname,ipi1chk1,length,1,ics)
               call mmgetblk('nmts1',isubname,ipnmts1,length,1,ics)
               length=nmregs*npoints
               call mmgetblk('imts1',isubname,ipimts1,length,1,ics)
               do i=1,npoints
                  i1chk1(i)=0
               enddo
               npts=0
               do it = 1,ntets
                  if (itmat(it) .eq. 0) then
                     nen=nelmnen(itettyp(it))
                     do j=1,nelmnen(itettyp(it))
                        i1=itet1(itetoff(it)+j)
                        if(i1chk1(i1).eq.0) then
                           npts=npts+1
                           i1chk1(i1)=npts
                        endif
                     enddo
                  endif
               enddo
               length=npts
               call mmgetblk('xicf',isubname,ipxicf,length,2,icscode)
               call mmgetblk('yicf',isubname,ipyicf,length,2,icscode)
               call mmgetblk('zicf',isubname,ipzicf,length,2,icscode)
               do i1=1,npoints
                  if(i1chk1(i1).gt.0) then
                     i2=i1chk1(i1)
                     xicf(i2)=xic(i1)
                     yicf(i2)=yic(i1)
                     zicf(i2)=zic(i1)
                  endif
               enddo
               call ifaceregv(xicf,yicf,zicf,npts,srchval,
     *                       imts1,nmts1,
     *                       ierr1)
               call mmrelblk('xicf',isubname,ipxicf,icscode)
               call mmrelblk('yicf',isubname,ipyicf,icscode)
               call mmrelblk('zicf',isubname,ipzicf,icscode)
C  get here if center of element is on a surface
C  look for material common to all vertices and set
C  element color from that
               call mmgetblk('ifound',isubname,ipfound,maxnen,1,ics)
               do it = 1,ntets
                  if (itmat(it) .eq. 0) then
                     nen=nelmnen(itettyp(it))
                     i2=i1chk1(itet1(itetoff(it)+1))
                     nmtst=nmts1(i2)
                     do k=1,nmtst
                     itst=imts1(k+nmregs*(i2-1))
                        do j=2,nen
                           ifound(j)=0
                           ii2=i1chk1(itet1(itetoff(it)+j))
                           nnmtst=nmts1(ii2)
                           do l=1,nnmtst
                             if(itst.eq.imts1(l+nmregs*(ii2-1)))
     *                                     ifound(j)=1
                           enddo
                        enddo
                        ifmin=10000
                        do j=2,nen
                           ifmin=min(ifmin,ifound(j))
                        enddo
                        if(ifmin.ne.0) then
                           itetmats=itetmats+1
                           itmat(it)=itst
                           go to 765
                        endif
                     enddo
 765                 continue
C
                  endif
               enddo
               call mmrelblk('ifound',isubname,ipfound,icscode)
C
            endif
C
         else
C
            write(logmess,9040) ntets-itetmats
 9040       format(' SETTETS required ITMAT step 4 for ',i10,' tets')
            call writloga('default',0,logmess,0,ierrwrt)
C
            do it = 1,ntets
               if (itmat(it) .eq. 0) then
                  if(itetclr(it).gt.0) then
                     itetmats = itetmats+1
                     itmat(it) = itetclr(it)
                     itmatp(it) = itet1(itetoff(it)+1)
                  endif
               endif
            enddo
C
            itetmatp = itetmats
C
         endif
C
      endif
C
      if (itetmats .lt. ntets) then
C
C        ...............................................................
C        THE MATERIAL MAP COULD NOT BE COMPLETED --- TERMINATE THE RUN.
C
         write(logmess,9060) ntets-itetmats
 9060    format(' Tet material map incomplete for ',i10,' tets')
         call writloga('default',1,logmess,1,ierrw)
C
C
         imtmax=imt1(iimax(npoints,imt1(1),1))
         ict = 0
         do it = 1,ntets
            if (itmat(it) .eq. 0) then
               ii2=i1chk1(itet1(itetoff(it)+1))
               itmat(it)=imts1(1+nmregs*(ii2-1))
               itmatp(it) = itet1(itetoff(it)+1)
               ict = ict+1
               write(logmess,9080) ict,it,imts1(1+nmregs*(ii2-1))
 9080          format(i10,'  No material for tet ',i10,
     *                ' setting to material of node 1 ',i10)
               if(ict.lt.30) then
                  call writloga('default',0,logmess,0,ierrw)
               else
                  call writloga('bat',0,logmess,0,ierrw)
               endif
            endif
         enddo
C
C*****   call termgen(1)
C
      endif
C
C     __________________________________________________________________
C     REMOVE INTERFACES SEPARATING IDENTICAL MATERIALS.
C
      do it = 1,ntets
         nef=nelmnef(itettyp(it))
         do j = 1,nef
C
            jt1 = jtet1(jtetoff(it)+j)
            if ((jt1 .gt. mbndry.and.mbndry.gt.0).or.jt1.lt.0) then
C
               ! interface: check itmat, loop to see if actually needed
               ! warning: should check if # nodes same for both faces
               ! when jtet_reduce_nnd=1
               if (jt1.gt.0) then
                  jt1 = jt1-mbndry
               else
                  jt1 = -jt1
               endif
               it2 = 1+(jt1-1)/nefcmo
               jf=jt1-(it2-1)*nefcmo
               if (itmat(it2) .eq. itmat(it) .and.
     &             (jtet1(jtetoff(it2)+jf)-mbndry .eq. (it-1)*nefcmo+j
     &               .or. -jtet1(jtetoff(it2)+jf) .eq. (it-1)*nefcmo+j )
     &            ) then
                  jtet1(jtetoff(it2)+jf) = (it-1)*nefcmo+j
                  jtet1(jtetoff(it)+j) = jt1
               endif
C
            elseif (jt1.ne.mbndry) then
C
               ! no interface: check itmat to see if needed
               ! warning: should check if # nodes same for both faces
               ! when jtet_reduce_nnd=1
               it2 = 1+(jt1-1)/nefcmo
               if (itmat(it2) .ne. itmat(it)) then
                  jf=jt1-(it2-1)*nefcmo
                  if (mbndry.gt.0) then
                     jtet1(jtetoff(it2)+jf) = (it-1)*nefcmo+j+mbndry
                     jtet1(jtetoff(it)+j) = jt1+mbndry
                  else
                     jtet1(jtetoff(it2)+jf) = -((it-1)*nefcmo+j)
                     jtet1(jtetoff(it)+j) = -jt1
                  endif
               endif
C
            endif
C
         enddo
      enddo
C
      if ((itetmatp .lt. ntets) .and. (nonintr .eq. 1)) then
C
C        _______________________________________________________________
C        COMPLETE THE itmatp ARRAY.
C
         write(logmess,9100) ntets-itetmatp
 9100    format(' SETTETS required ITMATP step 2 for ',i10,' tets')
         call writloga('default',0,logmess,0,ierrwrt)
C
         itetmato = -1
         n=0
         do while ((n .lt. 500) .and.
     &             (itetmatp .lt. ntets) .and. (itetmatp .ne. itetmato))
            n=n+1
            itetmato = itetmatp
            it=0
            do while ((it .lt. ntets) .and. (itetmatp .lt. ntets))
               it=it+1
               nef=nelmnef(itettyp(it))
               j=0
               do while ((j .lt. nef) .and. (itmatp(it) .eq. 0))
                  ! warning: if jtet_reduce_nnd=1, this may need revision
                  j=j+1
                  if ((jtet1(jtetoff(it)+j).lt.mbndry.and.mbndry.gt.0)
     &                 .or.(jtet1(jtetoff(it)+j).gt.0.and.mbndry.le.0)
     &               ) then
                     it1 = 1+(jtet1(jtetoff(it)+j)-1)/nefcmo
                     if (itmatp(it1) .ne. 0) then
                        itetmatp = itetmatp+1
                        itmatp(it) = itmatp(it1)
                     endif
                  endif
               enddo
            enddo
         enddo
C
         if (itetmatp .lt. ntets) then
            call mmgetblk('tetdist',isubname,iptetdst,ntets,2,icscode)
            if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
C           ............................................................
C           USE ALTERNATIVE ALGORITHM TO COMPLETE itmatp ARRAY.
C
            write(logmess,9120) ntets-itetmatp
 9120       format(' SETTETS required ITMATP step 3 for ',i10,' tets')
            call writloga('default',0,logmess,0,ierrwrt)
C
            do it = 1,ntets
               if (itmatp(it) .eq. 0) then
                  x0=zero
                  y0=zero
                  z0=zero
                  do i=1,nelmnen(itettyp(it))
                     i1 = itet1(itetoff(it)+i)
                     x0 = x0+xic(i1)
                     y0 = y0+yic(i1)
                     z0 = z0+zic(i1)
                  enddo
                  do n = 1,ntets
                     x1=zero
                     y1=zero
                     z1=zero
                     do i=1,nelmnen(itettyp(n))
                        i1 = itet1(itetoff(n)+i)
                        x1 = x1+xic(i1)
                        y1 = y1+yic(i1)
                        z1 = z1+zic(i1)
                     enddo
                     tetdist(n) = cvmgtr((x1-x0)**2+(y1-y0)**2+
     *                                  (z1-z0)**2,epsilonr,itmat(n)
     *                                  .eq. itmat(it) .and. itmatp(n)
     *                                  .ne. 0)
                  enddo
                  n1 = ismin(ntets,tetdist,1)
                  if (tetdist(n1) .lt. epsilonr) then
                     itetmatp = itetmatp+1
                     itmatp(it) = itmatp(n1)
                  endif
               endif
            enddo
            call mmrelblk('tetdist',isubname,iptetdst,icscode)
C
C           ............................................................
C           CHECK FOR ALL-BOUNDARY TETRAHEDRA.
C
            if (itetmatp .lt. ntets) then
C
               write(logmess,9140) ntets-itetmatp
 9140          format(' There are ',i10,' isolated tetrahedra')
               call writloga('default',1,logmess,1,ierrwrt)
C
               ict = 0
               do it = 1,ntets
                  if (itmatp(it) .eq. 0) then
                     ict = ict+1
                     write(logmess,9160) ict,it
 9160                format(i10,'  Isolated tet ',i10)
                     call writloga('bat',0,logmess,0,ierrwrt)
                  endif
               enddo
C
C*****         call termgen(1)
C
            endif
C
         endif
C
      endif
C
C
C     ******************************************************************
C     CHECK TO SEE THAT EACH TET HAS AN INDEX POINTER TO AN INTERIOR
C        POINT. SINCE A TET CAN HAVE ALL BOUNDARY POINTS A TET MAY HAVE
C        TO GRAB A NON-LOCAL POINT AS ITS REFERENCE. THIS WOULD SEEM THE
C        BEST WE CAN DO.
C
      icount_matp=0
      do it=1,ntets
         if(itmatp(it).le.0) icount_matp=icount_matp+1
      enddo
      iter_matp=1
      dowhile(icount_matp.gt.0.and.iter_matp.gt.0)
        icount_matp_save=icount_matp
        icount_matp=0
        do it=1,ntets
           if(itmatp(it).le.0) then
              do i=1,nelmnef(itettyp(it))
                 jtoff=jtetoff(it)+i
                 ! warning: if jtet_reduce_nnd=1, this may need revision
                 if ( (jtet1(jtoff).lt.mbndry.and.mbndry.gt.0)
     &                 .or.(jtet1(jtoff).gt.0.and.mbndry.le.0) ) then
                    jt=1+(jtet1(jtoff)-1)/nefcmo
                    if(itmatp(jt).gt.0.and.itmatp(it).le.0) then
                       itmatp(it)=itmatp(jt)
                    endif
                 endif
              enddo
              if(itmatp(it).le.0) icount_matp=icount_matp+1
           endif
        enddo
        if(icount_matp.ge.icount_matp_save) then
           iter_matp=-iter_matp
        else
           iter_matp=iter_matp+1
        endif
      enddo
C
C
C     ******************************************************************
C     CREATE child points on interfaces
C     ******************************************************************
C
C     IF NO INTERFACE POINTS EXIST THEN THIS MUST BE A SINGLE
C        MATERIAL PROLBEM AND WE CAN BAIL OUT AND JUST SET THE
C         TET COLORS.
C
      call mmrelblk('itmatp',isubname,ipitmatp,icscode)
      npointsave=npoints
      nadd=0
      if(nintpts .eq. 0) goto 9998
C
C     ******************************************************************
C
C  get parent child relationship in case children already exist
c
      call mmgetblk('iparent',isubname,ipiparent,npoints,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C     __________________________________________________________________
C     FILL ARRAY OF TET NUMBER THAT CONTAINS A NODE
C     find max material number
C
      call mmgetblk('iseedtet',isubname,ipiseedtet,npoints,1,icscode)
      maxmat=0
      do i =1,ntets
        do nn=1,nelmnen(itettyp(i))
           k=itet1(itetoff(i)+nn)
           iseedtet(k)=i
           iseedtet(iparent(k))=i
           if(imt1(k).gt.maxmat) maxmat=imt1(k)
         enddo
      enddo
C
C     Loop through interface nodes and construct child point for
C     each material
C
C     Estimate number of new nodes
      naddmax=3*nintpts
      call mmgetblk('ipar',isubname,ipipar,naddmax,1,icscode)
      call mmgetblk('imat',isubname,ipimat,naddmax,1,icscode)
      call mmgetblk('ilist',isubname,ipilist,naddmax,1,icscode)
      call mmgetblk('list',isubname,iplist,naddmax,1,icscode)
      call mmgetblk('xweight',isubname,ipxweight,naddmax,2,icscode)
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL BOUNDARY NODES.
C          IBND1 = 1  -> Boundary Node.
C          IBND1 = 0  -> Not a boundary node.
      length=npoints
      call mmgetblk('ibnd1',isubname,ipibnd1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call unpacktp('boundary','set',length,ipitp1,ipibnd1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL VIRTUAL INTERFACE NODES.
C          IVRT11 = 1  -> Virtual Node.
C          IVRT11 = 0  -> Not a virtual node.
      length=npoints
      call mmgetblk('ivrt1',isubname,ipivrt1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call unpacktp('virtual','set',length,ipitp1,ipivrt1,ierrdum)
c
      length=1000
      nmtrls=0
      call mmgetblk('mtrls',isubname,ipmtrls,length,1,icscode)
      call mmgetblk('stack',isubname,ipstack,length,1,icscode)
      lenm=length
      lens=length
      do i=1,nintpts
         i1=intpts(i)
         itin=iseedtet(i1)
         do nn=1,nelmnen(itettyp(itin))
            if (i1.eq.itet1(itetoff(itin)+nn))  then
               ! warning: if jtet_reduce_nnd=1 or jtet_cycle_max>0,
               ! get_materials_around_node may not be correct (not tested)
               call get_materials_around_node
     *          (itin,n,nmtrls,ipmtrls,lenm,ipstack,lens,
     *          itetoff,jtetoff,itet1,jtet1,itettyp,
     *          itmat,iparent, nefcmo,mbndry)
               go to 9145
            endif
         enddo
         write(logmess,'(a)') 'Bad mesh - settets stops'
         call writloga('default',0,logmess,0,icscode)
         go to 9999
 9145    if (nmtrls.eq.0) then
            write(logmess,'(a,i10)')
     *       'No materials for node ' ,i1
            call writloga('default',0,logmess,0,icscode)
            if(ibnd1(i1).eq.1) then
               itp1(i1)=ifitprfl
               icr1(i1)=icr1(i1)
             else
               itp1(i1)=ifitpint
               icr1(i1)=0
             endif
             isn1(i1)=0
         elseif (nmtrls.eq.1.and.ivrt1(i1).ne.1) then
            write(logmess,'(a,i10)')
     *       'Interface node converted to interior ' ,i1
            call writloga('bat',0,logmess,0,icscode)
            imt1(i1)=mtrls(1)
            if(ibnd1(i1).eq.1) then
               itp1(i1)=ifitprfl
               icr1(i1)=icr1(i1)
             else
               itp1(i1)=ifitpint
               icr1(i1)=0
             endif
             isn1(i1)=0
         else
            do j=1,nmtrls
               nadd=nadd+1
               if(nadd.gt.naddmax) then
                  ninc=1000
                  call mmincblk('ipar',isubname,ipipar,ninc,icscode)
                  call mmincblk('imat',isubname,ipimat,ninc,icscode)
                  call mmincblk('ilist',isubname,ipilist,ninc,icscode)
                  call mmincblk('list',isubname,iplist,ninc,icscode)
                  call mmincblk('xweight',isubname,ipxweight,ninc,
     *              icscode)
                  naddmax=naddmax+1000
               endif
               ipar(nadd)=i1
               imat(nadd)=mtrls(j)
               ilist(1,nadd)=i1
               list(nadd)=nadd+npoints
               xweight(1,nadd)=one
            enddo
         endif
      enddo
      call mmrelblk('ivrt1',isubname,ipibnd1,icscode)
      call mmrelblk('ibnd1',isubname,ipibnd1,icscode)
C
      write(logmess,9240) nadd
 9240 format(' Adding ',i9,' interface-coupled points.')
      call writloga('default',0,logmess,0,ierrdum)
C     __________________________________________________________________
C     INCREMENT MEMORY.
C
      call mmgetlen(ipxic,length,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetlen')
C
      if((npoints+nadd).gt.length) then
C
         length = npoints+nadd
         call cmo_memory(cmo, length, ntets, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_memory')
C
         call cmo_get_info('imt1', cmo, ipimt1, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('icr1', cmo, ipicr1, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
         call cmo_get_info('xic', cmo, ipxic, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('yic', cmo, ipyic, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('zic', cmo, ipzic, ilen, itype, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C
      endif
      cmolength='nnodes'
      call cmo_interpolate(cmo,cmo,
     *                     cmolength,
     *                     nadd,1,
     *                     list,ilist,xweight,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call mmrelblk('list',isubname,iplist,icscode)
      call mmrelblk('ilist',isubname,ipilist,icscode)
      call mmrelblk('xweight',isubname,ipxweight,icscode)
c
c  refresh mesh object pointers
c
      call cmo_get_info('imt1', cmo, ipimt1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icr1', cmo, ipicr1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic', cmo, ipxic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic', cmo, ipyic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic', cmo, ipzic, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C   Fix up itp,imt,isn for parents and children
C
      do i=1,nadd
         i1add=npointsave+i
         itp1(i1add)=itp1(ipar(i))
         imt1(i1add)=imat(i)
         isn=isn1(ipar(i))
         if(isn.eq.0) then
            isn1(ipar(i))=i1add
            isn1(i1add)=ipar(i)
         else
            isnsave=isn1(ipar(i))
            isn1(ipar(i))=i1add
            isn1(i1add)=isnsave
         endif
      enddo
      do i=1,nadd
         itp1(ipar(i))=ifitpcup
         imt1(ipar(i))=imt1(isn1(ipar(i)))
      enddo
C
C     ******************************************************************
C     ADJUST THE itet VALUES FOR TETRAHEDRA CONTAINING CHILD POINTS.
C
      do it=1,ntets
         do j=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+j)
            if(itp1(i1).eq.ifitpcup) then
               isn=isn1(itet1(itetoff(it)+j))
               icount=0
               do while (itmat(it).ne.imt1(isn))
                  icount=icount+1
                  isn=isn1(isn)
                  if(icount.gt.npoints.or.isn.gt.npoints+nadd) then
                     write(logmess,'(a,2i10)')
     *                 'bad parent child chain ',i1,isn
                     call writloga('default',0,logmess,0,icscode)
                     go to 9999
                  endif
               enddo
               itet1(itetoff(it)+j)=isn
             endif
          enddo
       enddo
C
C     CHECK THE TETRAHEDRAL DATA.
C
C
 9998 continue
C
C     **************************************************************
C     ASSIGN THE ELEMENT(TET) COLORS BASED ON THE NODE COLORS, SINCE
C        BY NOW ALL TETS SHOULD BE SURROUNDED BY SINGLE COLOR NODES.
C
      length=npoints+nadd
      call mmgetblk('ireal1',isubname,ipireal1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrw)
      if(ierrw.ne.0) call x3d_error(isubname,'unpacktp')
C
      ict1=0
      ict2=0
      do it=1,ntets
C
         itetclr(it) = itmat(it)
         imtsum = 0
         imtsav = imt1(itet1(itetoff(it)+1))
C
         do j=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+j)
            if(imt1(i1).eq.imtsav) imtsum=imtsum+1
            if(i1.le.0 .or.
     *         i1.gt.(npoints+nadd) .or.
     *         ireal1(i1).eq.0) then
               ict1=ict1+1
               if(ict1.le.30) then
                  write(logmess,9300) it,j,i1
                  call writloga('default',0,logmess,0,ierrdum)
 9300             format(' Bad itet: tet=',i10,' face=',i5,' itet=',i10)
               else
                  write(logmess,9300) it,j,i1
                  call writloga('bat',0,logmess,0,ierrw)
               endif
            elseif(ireal1(i1).ne.0) then
               ireal1(i1)=it
            endif
         enddo
C
         nen=nelmnen(itettyp(it))
         if(imtsum.ne.nen) then
            ict2=ict2+1
            if(ict2.le.30) then
               write(logmess,9320) it,(itet1(itetoff(it)+j),j=1,nen)
               call writloga('default',0,logmess,0,ierrw)
 9320          format(' Multimaterial tet: ',i10,' itet=',8i10)
            else
C*****         write(logmess,9320) it,(itet1(itetoff(it)+j),j=1,nen)
C*****         call writloga('bat',0,logmess,0,ierrw)
            endif
         endif
C
      enddo
C
      if(ict2.gt.0) then
         write(logmess,9340) ict2
 9340    format('There are', i7, '  multimaterial tets.')
         call writloga('default',1,logmess,0,ierr)
      endif
C
C
C     **************************************************************
C     BUMP THE POINT COUNT TO TELL THE GENERATOR THAT POINTS HAVE
C     BEEN ADDED.
C
C     ******************************************************************
C     UPDATE CMO LENGTHS.
C
      npoints=npointsave+nadd
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_set_info')
C
C     ******************************************************************
C     UPDATE ipointi AND ipointj.
C
      if (nadd.gt.0) then
         call set_info_i('ipointi',cmo,cglobal,cdefault,npointsave+1
     *    ,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
         call set_info_i('ipointj',cmo,cglobal,cdefault,npoints,
     *    icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      endif
 9999 continue
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmrelprt')
C
      return
      end
