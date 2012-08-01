      subroutine pset(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine builds point sets in a specified manner.
C
C     FORMAT: PSET/name/SEQ  /ifirst/ilast/istride
C                      /UNION/set1/set2/...
C                      /INTER/
C                      /LIST /
C                      /DELETE/
C                      /REGION/region name/ifirst/ilast/istride
C                      /MREGION/mregion name/ifirst/ilast/istride
C                      /SURFACE/surface name/ifirst/ilast/istride
C                      /ELTSET/element_set_name
C                      /GEOM /XYZ/ifirst/ilast/istride/xl,yl,zl/xu,yu,zu
C                                                     /xcen,ycen,zcen
C                            /RTZ/
C                            /RTP/
C                        surface name can be -all-, -interface-,
C                          -boundary- or any previously defined surface
C
C             pset/[name|-all-]/write/file_name[.pset]/[ascii|binary]
C             pset/[name|-all-]/zone /file_name[.zone]/[ascii]
C             pset/[name|-all-]/zonn /file_name[.zonn]/[ascii]
C
C            use assign/// idebug=1 to print element sets
C
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
C        $Log: pset.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.33   01 Oct 2007 08:17:40   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.32   09 May 2007 10:07:18   gable
CPVCS    Minor change to header of ascii and unformatted
CPVCS    *.pset output files. Request by Brad Aagaard.
CPVCS    
CPVCS       Rev 1.31   18 Apr 2007 15:32:56   gable
CPVCS    Minor change to comments.
CPVCS    
CPVCS       Rev 1.30   18 Apr 2007 15:30:48   gable
CPVCS    Add the write, zone and zonn options.
CPVCS    
CPVCS       Rev 1.29   08 Feb 2006 14:35:26   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.28   22 Nov 2005 11:09:44   gable
CPVCS    Fixed case in geom/xyz where min/max extents of specified
CPVCS    geometry do not overlap min/max extents of mesh object. Also
CPVCS    added a check of invalid input where the specified extents
CPVCS    have zero or negative volume.
CPVCS    
CPVCS       Rev 1.27   23 Feb 2005 08:43:34   tam
CPVCS    added imask to shiftr for union of more than one set
CPVCS    
CPVCS       Rev 1.26   01 Feb 2005 08:20:48   gable
CPVCS    Fixed logic for imt,icr,isn,itp test so that it will recognize
CPVCS    similar variables (imt_save, etc.) as being different.
CPVCS    
CPVCS       Rev 1.25   30 Sep 2004 11:22:12   dcg
CPVCS    make asmallnumber double precision
CPVCS    
CPVCS       Rev 1.24   30 Sep 2004 09:50:14   dcg
CPVCS    use iand and ior in place of .and. and .or. with integers
CPVCS    
CPVCS       Rev 1.23   10 Nov 2003 10:51:00   gable
CPVCS    Removed word MASS from log file output.
CPVCS    Changed format from a8 to a32 so the entire pset name is output.
CPVCS    
CPVCS       Rev 1.22   22 May 2002 08:02:44   gable
CPVCS    Corrected problem with geom/xyz when using limits
CPVCS    and xcen,ycen,zcen.
CPVCS    
CPVCS       Rev 1.21   09 Apr 2002 08:36:42   gable
CPVCS    Changed calculation of epsilon buffer in /geom/xyz /geom/rtz
CPVCS    calculations to correct errors when limits were much large
CPVCS    than data extents.
CPVCS    
CPVCS       Rev 1.20   19 Apr 2001 11:31:10   dcg
CPVCS    divide up if's even more carefully
CPVCS    
CPVCS       Rev 1.18   07 Dec 2000 10:03:04   dcg
CPVCS    keep track of number of psets
CPVCS    
CPVCS       Rev 1.17   16 Oct 2000 15:51:12   dcg
CPVCS    add materials option
CPVCS    
CPVCS       Rev 1.16   16 Oct 2000 09:29:30   dcg
CPVCS    add constraints option
CPVCS    pset/constraints/number/pset,get,psetname
CPVCS    
CPVCS       Rev 1.15   12 Oct 2000 09:42:46   dcg
CPVCS    restore default operation of eq
CPVCS    
CPVCS       Rev 1.14   03 Oct 2000 09:46:12   dcg
CPVCS    remove unused references to ialias
CPVCS    
CPVCS       Rev 1.13   02 Oct 2000 12:04:42   gable
CPVCS    Changed format of pset / list so that only 8
CPVCS    numbers rather than 10 are written out. Before
CPVCS    the 10 numbers ran over 80 characters and line
CPVCS    breaks were being put into the output.
CPVCS    
CPVCS       Rev 1.12   03 Sep 2000 11:51:18   gable
CPVCS      Changed format 6011 so element numbers gt 999999
CPVCS     do not run together.
CPVCS    
CPVCS       Rev 1.11   Wed Apr 05 13:34:52 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.10   14 Mar 2000 14:57:40   dcg
CPVCS    check for attribute type when attribute (or zq) option is in effect
CPVCS    
CPVCS       Rev 1.8   Tue Feb 15 14:55:12 2000   dcg
CPVCS    handle imd case correctly
CPVCS    
CPVCS       Rev 1.7   Fri Feb 04 16:35:30 2000   dcg
CPVCS    
CPVCS       Rev 1.5   02 Feb 2000 17:33:46   dcg
CPVCS    
CPVCS       Rev 1.4   Tue Feb 01 14:36:52 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Mon Jan 31 17:25:58 2000   dcg
CPVCS    
CPVCS       Rev 1.2   24 Jan 2000 16:21:34   dcg
CPVCS
C
C      Transition from pset.f to pset_nosb.f
C
CPVCS       Rev 1.62   Mon Dec 13 14:21:30 1999   dcg
CPVCS    allow for value/operation or operation/value in command format
CPVCS
CPVCS       Rev 1.61   Wed Nov 10 09:35:24 1999   dcg
CPVCS    remove test on miscellaneous storage block
CPVCS
CPVCS       Rev 1.60   Tue Oct 26 15:01:40 1999   dcg
CPVCS    allow use of word attribute in place or zq
CPVCS
CPVCS       Rev 1.59   Thu Sep 02 08:14:10 1999   dcg
CPVCS    fix type on matching mregion names
CPVCS
CPVCS       Rev 1.58   Fri Jun 18 13:58:20 1999   dcg
CPVCS    call chglocl and chgnorm to allow for previous
CPVCS    calls to coordsys
CPVCS
CPVCS       Rev 1.57   Tue Jun 15 16:01:08 1999   dcg
CPVCS    accept release or delete as legal option
CPVCS
CPVCS       Rev 1.56   Mon Mar 15 20:16:36 1999   tam
CPVCS    changed idebug declaration from real*8 to integer
CPVCS
CPVCS       Rev 1.55   Wed Dec 23 13:57:16 1998   jtg
CPVCS    fixed duplicate definition of nwval,nwatt
CPVCS
CPVCS       Rev 1.54   Thu Dec 03 14:45:26 1998   dcg
CPVCS    add intrface to surface option
CPVCS
CPVCS       Rev 1.53   Mon Nov 23 16:28:46 1998   dcg
CPVCS    add surface option
CPVCS
CPVCS       Rev 1.52   Mon Sep 21 13:57:00 1998   dcg
CPVCS    change calls to getregv to ask only for region
CPVCS    requested.
CPVCS
CPVCS       Rev 1.51   Fri Aug 28 14:25:04 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.50   Thu Jun 11 09:44:10 1998   dcg
CPVCS    don't print pset members unless idebug >=1
CPVCS    use 1,0,0 as default in region option for point set restriction
CPVCS
CPVCS       Rev 1.49   Sun Jun 07 14:29:40 1998   gable
CPVCS    Modified format statement for large psets.
CPVCS
CPVCS       Rev 1.48   Mon Mar 16 16:41:28 1998   dcg
CPVCS    test lengths of names when comparing
CPVCS
CPVCS       Rev 1.47   Wed Dec 17 13:53:14 1997   dcg
CPVCS    fix bad index in region name match loop
CPVCS
CPVCS       Rev 1.46   Mon Nov 24 16:34:34 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.45   Fri Oct 31 10:49:14 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.44   Fri Oct 03 11:01:56 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.43   Tue Jul 29 14:43:58 1997   dcg
CPVCS    add eltset option that marks all nodes of elements
CPVCS    in a given element_set as in this new pset.
CPVCS    delete obsolete womesh option
CPVCS
CPVCS       Rev 1.42   Wed Apr 23 11:06:58 1997   dcg
CPVCS    set number of psets to  nbytes_int*8
CPVCS    print error message and return if max number
CPVCS    of psets is exceeded
CPVCS
CPVCS       Rev 1.41   Mon Apr 14 16:56:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.40   Thu Jul 25 10:28:02 1996   dcg
CPVCS    set comparison type for character material specification
CPVCS
CPVCS       Rev 1.38   Mon Mar 04 11:11:38 1996   dcg
CPVCS    remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.37   Fri Feb 16 21:49:30 1996   het
CPVCS    Use on the specified region name for testing.
CPVCS
CPVCS       Rev 1.36   Mon Feb 12 12:57:28 1996   dcg
CPVCS    add mregion option
CPVCS    modify zq option for vectors (rank/=1)
CPVCS
CPVCS       Rev 1.35   Fri Feb 02 14:42:04 1996   dcg
CPVCS    remove references to explicit icn1 and int1
CPVCS
CPVCS       Rev 1.34   Fri Feb 02 14:22:42 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.33   Tue Jan 23 09:08:00 1996   dcg
CPVCS    add points types to call to getregv1
CPVCS
CPVCS       Rev 1.32   Fri Dec 22 14:36:04 1995   het
CPVCS    No change.
CPVCS
CPVCS       Rev 1.31   12/05/95 08:20:44   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.30   11/16/95 15:22:04   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.29   11/07/95 17:22:48   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.28   09/19/95 13:10:46   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.27   09/19/95 08:44:18   dcg
CPVCS    allow integer type for added attribute
CPVCS
CPVCS       Rev 1.26   09/18/95 19:43:02   dcg
CPVCS    look for mesh object added attributes
CPVCS
CPVCS       Rev 1.25   08/30/95 21:07:14   het
CPVCS    Change the name of the storage block id from pset to psetnames
CPVCS
CPVCS       Rev 1.24   08/29/95 11:52:14   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.23   08/23/95 06:58:24   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.22   08/22/95 06:50:34   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.21   06/20/95 15:41:28   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.20   06/13/95 09:02:22   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.19   06/07/95 15:31:06   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.18   05/24/95 15:44:18   het
CPVCS    Uncover some ipointi/ipointj stuff
CPVCS
CPVCS       Rev 1.17   05/17/95 15:43:02   dcg
CPVCS    remove dependence on ipointi, ipointj for pntlimn calls
CPVCS
CPVCS       Rev 1.16   05/11/95 13:47:48   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.15   05/04/95 16:08:32   ejl
CPVCS    Fixed error with length in region option.
CPVCS
CPVCS       Rev 1.14   05/01/95 16:02:54   dcg
CPVCS    fix region name bug
CPVCS
CPVCS       Rev 1.13   05/01/95 08:33:56   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.12   04/01/95 09:32:32   dcg
CPVCS    Look for -def- for empty pset name
CPVCS
CPVCS       Rev 1.11   03/31/95 10:52:40   het
CPVCS    Correct an error with idsb
CPVCS
CPVCS       Rev 1.10   03/31/95 09:09:20   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.9   03/30/95 05:00:34   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.8   03/28/95 12:35:48   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.7   03/23/95 15:08:00   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.6   03/22/95 10:17:32   dcg
CPVCS    remove isubname=regset and side effects
CPVCS
CPVCS       Rev 1.5   03/22/95 09:11:42   dcg
CPVCS    Release all of temporary memory (regset+pset)
CPVCS
CPVCS       Rev 1.4   03/10/95 17:13:30   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.3   02/18/95 06:56:54   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.2   01/04/95 22:04:08   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:18   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:08   pvcs
CPVCS    Original version.
C ######################################################################
      implicit none

C ######################################################################
C
      include 'machine.h'

      include 'local_element.h'
      include 'geom_lg.h'
      include 'cmo_lg.h'
      include 'chydro.h'
C ######################################################################
C
C     args
      integer nwds
      character*(*) cmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      integer ierror_return

C
      integer nplen,ntlen
      parameter (nplen=10000000,ntlen=10000000)
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipicontab,icontab)
      integer icontab(50,*)
      integer isetwd(*)
      integer imt1(*), itp1(*),
     *        icr1(*), isn1(*), xtetwd

      pointer (ipitetclr, itetclr(ntlen))
      pointer (ipitetoff, itetoff(ntlen))
      pointer (ipitettyp, itettyp(ntlen))
      pointer (ipxtetwd,  xtetwd(ntlen))
      pointer (ipitet,    itet(ntlen))
      integer itetclr,itetoff,itettyp,itet

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
C
C
C ######################################################################

      integer nextlun, iunit

      external shiftr,shiftl
      integer shiftr,shiftl

      integer ierror
      integer npoints,icmotype,length,ityp,ierr,ilen,lout,iout,itype,
     *   j,i,iotype,ioformat,
     *   icnt,len1,len2,len3,iprint,nsetcnt,ndup,number_of_psets,
     *   isetchg,ntets,nmelts,ifirst,ilast,istride,
     *   mpno,ii,nsets,ict,ipos1,ipos2,nwd1,nwd2,iintrfce,index,
     *   iunion,ipsetck,ibin,ivalue,ivaltyp,iprtindx,icount,
     *   ier,irank,l,iregck,ir,lenmm4,ics,npts,ibitpos,is,isurfck,
     *   icrtst,numconstraints,nconbnd,next,nmats,nummaterials,
     *   id_zone_number, if_id_zone_number
      integer ipointi,ipointj,icscode,lenname,lenmode
      integer icharlnf,mask2,nmask2,i1,inotflg,ierrw,mask,
     *   nmask,it,k,kk,ivaltypa
      integer n_entry_per_line

      integer nbitsmax
      parameter (nbitsmax=8*NBYTES_INT)
      integer itest(nbitsmax),ibpos(nbitsmax)


      real*8  xxlarge,xsmall,dummy,xc,yc,zc,
     *   xvalue,xxsmall
      real*8 xl,yl,zl,xu,yu,zu,xcen,ycen,zcen,a,b,c,zero,phi,
     *   thi,ratx,raty,ratz,srchval
      real*8 xmin_pset, xmax_pset,
     *       ymin_pset, ymax_pset,
     *       zmin_pset, zmax_pset, rmax_pset

      real*8 alargenumber,asmallnumber
      parameter (alargenumber=1.0d+30)
      parameter (asmallnumber=1.0d-20)

      pointer( ipmpary1 ,  mpary1(*) )
      pointer( ipmpary1 , xmpary1(*) )
      pointer( ipmpary2 ,  mpary2(*) )
      pointer( ipmpary2 , xmpary2(*) )
      pointer( ipmpary3 ,  mpary3(*) )
      pointer( ipmpary3 , xmpary3(*) )
      pointer( ipmpary4 ,  mpary4(*) )
      pointer( ipmpary4 , xmpary4(*) )
      pointer(iptmpwd, itmpwd(*))
      pointer(ipielts, ielts(*))
      integer mpary1,mpary2,mpary3,mpary4,itmpwd,ielts
      real*8 xmpary1,xmpary2,xmpary3,xmpary4
C
      character*2 itestit
      character*8 operation
      pointer(ippsetnames,psetnames)
      character*32 psetnames(*)

      character*32 isubname,name,mode,name1,name2,iregnam,
     *  isurfnam, cout
      character*32 names(100),ch1,ch2,geom_name
      pointer(ipout,out)
      real*8 out(*),rout
C
      pointer(ipregno, iregno(*))
      pointer(ipsurfno, isurfno(*))
      integer iregno,isurfno
C
      pointer(ipxfield,xfield)
      real*8 xfield(*)
      pointer(ipxfield,ifield)
      integer ifield(*)
      pointer(ipxfield,cfield)
      character*32 cfield(*)
C
      integer imask
      imask(i)=2**(i-1)
 
      character*32 cpt1, cpt2, cpt3,
     *             cfirst, clast, cstride
      character*32  cgeom
      character*32 cmo,cvalue
      character*128 file_name
      character*132 logmess

C ######################################################################
C BEGIN begin
C
      isubname='psetnames'
C
      ierror = 0
      i = 0
C
C     ******************************************************************
C
C  get mesh object
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call cmo_get_intinfo('idebug',cmo,idebug,length,icmotype,ierror)
      call cmo_get_intinfo('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
C
      call cmo_get_intinfo('ipointi',cmo,ipointi,ilen,ityp,icscode)
      call cmo_get_intinfo('ipointj',cmo,ipointj,ilen,ityp,icscode)
      call cmo_get_intinfo('number_of_psets',cmo,
     *    number_of_psets,ilen,ityp,icscode)
c
      iprint=0
C
C     ******************************************************************
      if(npoints .eq. 0) then
         write(logmess,'(a)')'WARNING: MO has zero nodes'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')'WARNING: NO ACTION'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')'RETURN'
         call writloga('default',0,logmess,0,ierr)
         goto 9998
      endif
C     ******************************************************************
C
C     ALLOCATE SOME LOCAL MEMORY FOR THIS ROUTINE.
C
      length=npoints
      call mmgetblk('xmpary1',isubname,ipmpary1,length,2,icscode)
      call mmgetblk('xmpary2',isubname,ipmpary2,length,2,icscode)
      call mmgetblk('xmpary3',isubname,ipmpary3,length,2,icscode)
      call mmgetblk('xmpary4',isubname,ipmpary4,length,2,icscode)
      call mmgetblk('itmpwd',isubname,iptmpwd,length,2,icscode)
C
C     ******************************************************************
C
C     SET CONSTANTS, NAME AND MODE
C
      xxlarge=alargenumber
      xxsmall=asmallnumber
      xsmall=alargenumber
C
      name=cmsgin(2)
      lenname=icharlnf(name)
      if(name(1:5) .eq. '-def-')name(1:5) = '-all-'

      mode=cmsgin(3)
      lenmode=icharlnf(mode)
      if ((name(1:lenname).eq.'0' .or. name(1:lenname).eq. ' ') .and.
     &    (mode(1:lenmode).eq.'0' .or. mode(1:lenmode).eq. ' ')) then
         mode='list'
      endif
      if (mode(1:lenmode).eq. 'write') then
         mode='write'
         ioformat = 1   ! PYLITH pset file format
      endif
      if (mode(1:lenmode).eq. 'zone') then
         mode='write'
         lenmode = 5
         ioformat = 2   ! FEHM zone file format
      endif
      if (mode(1:lenmode).eq. 'zonn') then
         mode='write'
         lenmode = 5
         ioformat = 3   ! FEHM zonn file format
      endif
C
C     ******************************************************************
C
      isetchg=0
      index=0
      call mmfindbk('psetnames',cmo,ippsetnames,ilen,icscode)
      do i=1,nbitsmax
        if(psetnames(i).eq.name) then
          index=i
          go to 12
        endif
      enddo
 
C     ******************************************************************
C
C     PROCESS THE 'LIST' and ''WRITE'  MODE
C
  12  if ((mode(1:lenmode) .eq. 'list') .or.
     $    (mode(1:lenmode) .eq. 'write')) then

      if(mode(1:lenmode) .eq. 'write') then
      if ((msgtype(2).eq.3.and.cmsgin(2)(1:5).eq.'-def-') .or.
     *    (msgtype(2).eq.3.and.cmsgin(2)(1:5).eq.'-all-') .or.
     *    (msgtype(2).eq.3.and.cmsgin(2).eq.' ')) then
           name = '-all-'
      endif
      endif
C
C        ---------------------------------------------------------------
C        IF NO NAME ENTERED, DISPLAY LIST OF PSET NAMES
C
         if ((msgtype(2).eq.1.and.imsgin(2).eq.0) .or.
     *       (msgtype(2).eq.3.and.cmsgin(2)(1:5).eq.'-def-') .or.
     *       (msgtype(2).eq.3.and.cmsgin(2).eq.' ') .or.
     *       (mode(1:lenmode) .eq. 'write')) then
            ierror=0
            icnt=0
            do 1 i=1,nbitsmax
               if (psetnames(i) .ne.' ') icnt=i
    1       continue
            write(logmess,6000) icnt
 6000       format('PSET: THERE ARE ',i2,' PSETS DEFINED')
            call writloga('default',0,logmess,0,ierr)
            if (icnt .eq. 0) go to 9998
            do 2 i=1,icnt,4
               write(logmess,6001) (psetnames(j),j=i,min0(i+3,icnt))
 6001          format(4(2x,a16))
               call writloga('default',0,logmess,0,ierr)
    2       continue
            if(mode(1:lenmode) .eq. 'list') then
               go to 9998
            elseif(mode(1:lenmode) .eq. 'write') then
C              Make sure that the pset actually exists
               icnt=0
               do 3 i=1,nbitsmax
                  len2=icharlnf(psetnames(i))
                  len1=max(lenname,len2)
                  if (name(1:len1) .eq. psetnames(i)(1:len1)) icnt=i
    3          continue
               if (name(1:5) .eq. '-all-') then
                   go to 9997
               elseif (icnt .eq. 0) then
                   write(logmess, 1005) name
                   call writloga('default',1,logmess,1,ierr)
                   go to 9998
               else
                   go to 9997
               endif
            endif
C
C        ---------------------------------------------------------------
C        IF NAME ENTERED, CHECK THAT IT EXISTS
C
         else
            if (index .eq. 0) then
               write(logmess,1005) name
               call writloga('default',1,logmess,1,ierr)
               go to 9998
            else
               iprint=1
               ierror=0
               go to 9997
            endif
         endif
      endif
C
C     ******************************************************************
C
C     CHECK TO SEE IF THE NAME HAS ALREADY BEEN USED AND SET THE
C     BIT POSITION.
C
      if(msgtype(2).ne.3.or.msgtype(3).ne.3) goto 9998
      do i=1,nbitsmax
         len2=icharlnf(psetnames(i))
         len1=max(lenname,len2)
         itest(i)=0
         if(name(1:len1).eq.psetnames(i)(1:len1)) itest(i)=i
      enddo
      call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
      nsetcnt=0
      if(ndup.eq.0) then
         do 15 i=1,nbitsmax
            if(psetnames(i).eq.' '.or.psetnames(i)(1:5).eq.'-def-') then
               nsetcnt=i
               goto 16
            endif
 15      continue
 16      continue
         ibitpos=nsetcnt
         if(nsetcnt.gt.nbitsmax.or.nsetcnt.le.0) then
            write(logmess,1000) nbitsmax
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
         if(nsetcnt.le.0) nsetcnt=1
         isetchg=isetchg+1
         psetnames(nsetcnt)=name(1:lenname)
         number_of_psets=number_of_psets+1
         call cmo_set_info('number_of_psets',cmo,number_of_psets,
     *     1,1,icscode)
      else
         ibitpos=itest(1)
      endif
      mask=imask(ibitpos)
      nmask=not(mask)
C
C     ******************************************************************
C
C     SAVE OLD DATA AND ZERO OUT THE POINT SET.
C
      do 30 i=1,npoints
         itmpwd(i)=iand(isetwd(i),nmask)
 30   continue
C
C     ******************************************************************
C
C     SET UP THE NEW POINT SET.
C     __________________________________________________________________
C
C
C     PROCESS THE 'ELTSET' MODE
C
      if (mode(1:lenmode) .eq. 'eltset'.or.
     *    mode(1:lenmode) .eq. 'ELTSET') then
         call mmgetblk('ielts',isubname,ipielts,ntets,1,icscode)
         call cmo_get_info('xtetwd',cmo,
     *                  ipxtetwd,ilen,ityp,ierr)
         ch1='eset'
         ch2='get'
         call eltlimc(ch1,ch2,cmsgin(4),ipielts,nmelts,ntets,xtetwd)
         do i=1,nmelts
            it=ielts(i)
            do j=1,nelmnen(itettyp(it))
               k=itet(itetoff(it)+j)
               itmpwd(k)=ior(isetwd(k),mask)
            enddo
         enddo
      go to 9996
      endif
C
C     PROCESS THE 'SEQ' MODE.
C
      if(mode(1:lenmode).eq.'seq') then
         if(msgtype(4).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(4)
         elseif(msgtype(4).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(4)
         elseif(msgtype(4).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(4)
         endif
         if(msgtype(5).eq.1) then
            clast  ='number'
            ilast  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            clast  ='number'
            ilast  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ilast  =0
            clast  =cmsgin(5)
         endif
         if(msgtype(6).eq.1) then
            cstride  ='number'
            istride  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            cstride  ='number'
            istride  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            istride  =0
            cstride  =cmsgin(6)
         endif
         if(cfirst(1:9).eq.'psetnames') goto 9998
         mpno=npoints
         if(msgtype(4).eq.1.or.msgtype(4).eq.2) then
            if(ifirst.le.0) then
               ifirst=ipointi
            endif
            ifirst=max(1,min(ifirst,npoints))
            if(ilast.le.0) then
               ilast=ipointj
            endif
            ilast=max(1,min(ilast,npoints))
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do 40 ii=1,mpno
            i=mpary1(ii)
            itmpwd(i)=ior(itmpwd(i),mask)
 40      continue
         ierror=0
         goto 9996
      endif
C
C     __________________________________________________________________
C
C     PROCESS THE 'UNION' AND 'INTER' (OR INTERSECTION) MODE.
C
      if(mode(1:lenmode).eq.'union' .or.
     *   mode(1:lenmode).eq.'inter' .or.
     *   mode(1:lenmode).eq.'not') then
         nsets=nwds-3
         if(nsets.le.0.or.nsets.ge.nbitsmax) goto 9998
C
C        ...............................................................
C        READ IN THE NAMES OF THE SETS TO BE UNIONED AND CHECK TO
C        SEE IF ANY OF THESE MATCH THE NEW SET'S NAME.
C
         ict=0
         do 50 i=1,nsets
            ict=ict+1
            names(ict)=cmsgin(ict+3)
 50      continue
C
C        ...............................................................
C        FIND THE BIT POSITIONS OF THE SETS TO BE OPERATED ON.
C
         do 70 i=1,nsets
            do 60 j=1,nbitsmax
               len1=icharlnf(psetnames(j))
               len2=icharlnf(names(i))
               len1=max(len1,len2)
               itest(j)=0
               if(psetnames(j)(1:len1).eq.names(i)(1:len1)) itest(j)=j
 60         continue
            call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
            if(ndup.eq.0) then
               write(logmess,1005) names(i)
               call writloga('default',1,logmess,1,ierr)
               goto 9998
            endif
            ibpos(i)=itest(1)
            ibpos(i+1)=itest(1)
 70      continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'union' operation.
C
         if(mode(1:lenmode).eq.'union') then
            ipos1=ibpos(1)
            ipos2=ibpos(2)
            do 80 j=1,npoints
               nwd1=shiftr(iand(isetwd(j),imask(ipos1)),ipos1-1)
               nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
               iunion=shiftl(ior(nwd1,nwd2),ibitpos-1)
               itmpwd(j)=ior(iand(isetwd(j),nmask),iunion)
 80         continue
            ipos1=ibitpos
            do 100 i=3,nsets
               ipos2=ibpos(i)
               do 90 j=1,npoints
                  nwd1=shiftr(iand(itmpwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
                  iunion=shiftl(ior(nwd1,nwd2),ibitpos-1)
                  itmpwd(j)=ior(iand(itmpwd(j),nmask),iunion)
 90            continue
 100        continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'intersection' operation.
C
         elseif(mode(1:lenmode).eq.'inter') then
            ipos1=ibpos(1)
            ipos2=ibpos(2)
            do 110 j=1,npoints
               nwd1=shiftr(iand(isetwd(j),imask(ipos1)),ipos1-1)
               nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
               iintrfce=shiftl(iand(nwd1,nwd2),ibitpos-1)
              itmpwd(j)=ior(iand(isetwd(j),nmask),iintrfce)
 110        continue
            ipos1=ibitpos
            do 130 i=3,nsets
               ipos2=ibpos(i)
               do 120 j=1,npoints
                  nwd1=shiftr(iand(itmpwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
                  iintrfce=shiftl(iand(nwd1,nwd2),ibitpos-1)
                  itmpwd(j)=ior(iand(itmpwd(j),nmask),iintrfce)
 120           continue
 130        continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'not' operation.
C
         elseif(mode(1:lenmode).eq.'not') then
            if(nsets.eq.1) then
               ipos1=ibpos(1)
               do 145 j=1,npoints
                  nwd1=shiftr(iand(isetwd(j),imask(ipos1)),ipos1-1)
                  iintrfce=shiftl(not(nwd1),ibitpos-1)
                  itmpwd(j)=ior(iand(isetwd(j),nmask),iintrfce)
  145          continue
            elseif(nsets.gt.1) then
               ipos1=ibpos(1)
               ipos2=ibpos(2)
               do 140 j=1,npoints
                  nwd1=shiftr(iand(isetwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
                  iintrfce=shiftl(iand(nwd1,not(nwd2)),ibitpos-1)
                  itmpwd(j)=ior(iand(isetwd(j),nmask),iintrfce)
 140           continue
               ipos1=ibitpos
               do 160 i=3,nsets
                  ipos2=ibpos(i)
                  do 150 j=1,npoints
                     nwd1=shiftr(iand(itmpwd(j),imask(ipos1)),ipos1-1)
                     nwd2=shiftr(iand(isetwd(j),imask(ipos2)),ipos2-1)
                     iintrfce=shiftl(iand(nwd1,not(nwd2)),ibitpos-1)
                     itmpwd(j)=ior(iand(itmpwd(j),nmask),iintrfce)
 150              continue
 160           continue
            endif
         else
            goto 9998
         endif
         ierror=0
         goto 9996
      endif
C
C     __________________________________________________________________
C
C     PROCESS THE 'GEOM' MODE.
C
      if(mode(1:lenmode).eq.'geom') then
          if(normflgc.eq.1) call chglocl(1,npoints,1)
         cgeom   =cmsgin(4)
         if(msgtype(5).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(5)
         endif
         if(msgtype(6).eq.1) then
            clast  ='number'
            ilast  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            clast  ='number'
            ilast  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            ilast  =0
            clast  =cmsgin(6)
         endif
         if(msgtype(7).eq.1) then
            cstride  ='number'
            istride  =imsgin(7)
         elseif(msgtype(7).eq.2) then
            cstride  ='number'
            istride  =xmsgin(7)
         elseif(msgtype(7).eq.3) then
            istride  =0
            cstride  =cmsgin(7)
         endif
         call test_argument_type(9,2,8,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         xl      =xmsgin(8)
         yl      =xmsgin(9)
         zl      =xmsgin(10)
         xu      =xmsgin(11)
         yu      =xmsgin(12)
         zu      =xmsgin(13)
         xcen    =xmsgin(14)
         ycen    =xmsgin(15)
         zcen    =xmsgin(16)
C
C
         call setsize()
 
      call cmo_get_attinfo
     1 ('xmin',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      xmin_pset = rout
      call cmo_get_attinfo
     1 ('xmax',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      xmax_pset = rout
      call cmo_get_attinfo
     1 ('ymin',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      ymin_pset = rout
      call cmo_get_attinfo
     1 ('ymax',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      ymax_pset = rout
      call cmo_get_attinfo
     1 ('zmin',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      zmin_pset = rout
      call cmo_get_attinfo
     1 ('zmax',cmo,iout,rout,cout,ipout,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      zmax_pset = rout
      rmax_pset = sqrt((xmax_pset-xmin_pset)**2 +
     1                 (ymax_pset-ymin_pset)**2 +
     1                 (zmax_pset-zmin_pset)**2 )
C
         if(cgeom(1:3).eq.'xyz') then
C
C    If for some reason someone puts characters into the min/max extents
C    just set them to large negative/positive numbers.
C
            if(msgtype(8) .eq.3) xl=-xxlarge
            if(msgtype(9) .eq.3) yl=-xxlarge
            if(msgtype(10).eq.3) zl=-xxlarge
            if(msgtype(11).eq.3) xu= xxlarge
            if(msgtype(12).eq.3) yu= xxlarge
            if(msgtype(13).eq.3) zu= xxlarge
C
C     Check for the case of no overlap between specified extents
C     of pset and extents of the mesh object. If there is no
C     overlap the pset is empty and we are done.
C
            if((xl+xcen .gt. xmax_pset+xxsmall).or.
     1         (xu+xcen .lt. xmin_pset-xxsmall).or.
     2         (yl+ycen .gt. ymax_pset+xxsmall).or.
     3         (yu+ycen .lt. ymin_pset-xxsmall).or.
     4         (zl+zcen .gt. zmax_pset+xxsmall).or.
     5         (zu+zcen .lt. zmin_pset-xxsmall).or.
     6         (xl .gt. xu).or.
     7         (yl .gt. yu).or.
     8         (zl .gt. zu)                   )then
                write(logmess,1003)
                call writloga('default',1,logmess,1,ierr)
                write(logmess,6030) name,0
 6030           format(
     1          ' THE PSET ',a32,' HAS ',i10,
     2          ' POINTS, NO OVERLAP')
                call writloga('default',1,logmess,0,ierr)
                go to 9998
      endif
C
C     Be sure geometric limits are not many orders of magnitude
C     greater than point set limits. This causes problems in the
C     buffer calculations inside loop 210
C
            if(xl+xcen .lt. xmin_pset-xxsmall)
     1         xl=xmin_pset-xxsmall-xcen
            if(xu+xcen .gt. xmax_pset+xxsmall)
     1         xu=xmax_pset+xxsmall-xcen
            if(yl+ycen .lt. ymin_pset-xxsmall)
     1         yl=ymin_pset-xxsmall-ycen
            if(yu+ycen .gt. ymax_pset+xxsmall)
     1         yu=ymax_pset+xxsmall-ycen
            if(zl+zcen .lt. zmin_pset-xxsmall)
     1         zl=zmin_pset-xxsmall-zcen
            if(zu+zcen .gt. zmax_pset+xxsmall)
     1         zu=zmax_pset+xxsmall-zcen
C
         elseif(cgeom(1:3).eq.'rtz') then
            if(msgtype(8).eq.3) xl=-xsmall
            if(msgtype(9).eq.3) yl=-xsmall
            if(msgtype(10).eq.3) zl=-xxlarge
            if(msgtype(11).eq.3) xu=xxlarge
            if(msgtype(12).eq.3) yu=360.0
            if(msgtype(13).eq.3) zu=xxlarge
            if( xl .lt. 0.0 ) xl = 0.0 - xxsmall
            if( xu .gt. rmax_pset+xxsmall ) xu = rmax_pset+xxsmall
            if(zl+zcen .lt. zmin_pset-xxsmall)
     1         zl=zmin_pset-xxsmall-zcen
            if(zu+zcen .gt. zmax_pset+xxsmall)
     1         zu=zmax_pset+xxsmall-zcen
         elseif(cgeom(1:3).eq.'rtp') then
            if(msgtype(8).eq.3) xl=-xsmall
            if(msgtype(9).eq.3) yl=0.0
            if(msgtype(10).eq.3) zl=0.0
            if(msgtype(11).eq.3) xu=xxlarge
            if(msgtype(12).eq.3) yu=180.0
            if(msgtype(13).eq.3) zu=360.0
         endif
         ipsetck=ifirst
         name1=cmsgin(7)
         mpno=npoints
         if(msgtype(5).eq.1.or.msgtype(5).eq.2) then
c           if(ifirst.le.0) then
c              ifirst=ipointi
c           endif
c           ifirst=max(1,min(ifirst,npoints))
c           if(ilast.le.0) then
c              ilast=ipointj
c           endif
c           ilast=max(1,min(ilast,npoints))
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary4,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary4,mpno,
     *                   npoints,isetwd,itp1)
         endif
         if (mpno .le. 0) goto 9998
         if (cfirst(1:9).eq.'psetnames') then
            len1=icharlnf(name1)
            len1=max(lenname,len1)
            if(name1(1:len1).eq.name(1:len1)) then
               write(logmess,1002)
               call writloga('default',1,logmess,1,ierr)
               goto 9998
            endif
            do 200 i=1,nbitsmax
               len1=icharlnf(name1)
               len2=icharlnf(psetnames(i))
               len1=max(len1,len2)
               itest(i)=0
               if(name1(1:len1).eq.psetnames(i)(1:len1)) itest(i)=i
 200        continue
            call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
            if(ndup.eq.0) then
               write(logmess,1005) name1
               call writloga('default',1,logmess,1,ierr)
               goto 9998
            endif
         endif
         ict=0
         do 210 i=1,mpno
            j=mpary4(i)
            ict=ict+1
            mpary4(ict)=j
            if(cgeom(1:3).eq.'xyz') then
               ierror=0
               a=xic(j)
               b=yic(j)
               c=zic(j)
               xmpary1(ict)=a-xcen
               xmpary2(ict)=b-ycen
               xmpary3(ict)=c-zcen
            elseif(cgeom(1:3).eq.'rtz') then
               ierror=0
               a=xic(j)-xcen
               b=yic(j)-ycen
               c=zic(j)-zcen
               zero=0.0d+00
               call angle3v(zero,zero,zero,a,b,c,thi,dummy)
               xmpary1(ict)=sqrt(a**2+b**2)
               xmpary2(ict)=thi*180.0/pie
               xmpary3(ict)=c
               if(xmpary1(ict).eq.0.0) xmpary2(ict)=0.5*(yl+yu)
            elseif(cgeom(1:3).eq.'rtp') then
               ierror=0
               a=xic(j)-xcen
               b=yic(j)-ycen
               c=zic(j)-zcen
               zero=0.0d+00
               call angle3v(zero,zero,zero,a,b,c,phi,thi)
               xmpary1(ict)=sqrt(a**2+b**2+c**2)
               xmpary2(ict)=thi*180.0/pie
               xmpary3(ict)=phi*180.0/pie
               if(xmpary1(ict).eq.0.0) then
                  xmpary2(ict)=0.5*(yl+yu)
                  xmpary3(ict)=0.5*(zl+zu)
               endif
            endif
 210     continue
         if(ierror.ne.0) goto 9998
C
C        ...............................................................
C        BIN THE POINTS.
C
         xc=0.5*(xl+xu)
         yc=0.5*(yl+yu)
         zc=0.5*(zl+zu)
         ratx=2.0/(abs(xl-xu)+xxsmall)
         raty=2.0/(abs(yl-yu)+xxsmall)
         ratz=2.0/(abs(zl-zu)+xxsmall)
         do 220 i=1,ict
            ibin=int(abs((xmpary1(i)-xc)*ratx)-1.0e-14)+
     *           int(abs((xmpary2(i)-yc)*raty)-1.0e-14)+
     *           int(abs((xmpary3(i)-zc)*ratz)-1.0e-14)
            if(ibin.ne.0) mpary4(i)=0
 220     continue
         call kmprsn(ict,mpary4,1,mpary4,1,mpary4,1,npts)
         if(npts.eq.0) then
            write(logmess,1003)
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
         do 230 i=1,npts
            j=mpary4(i)
            itmpwd(j)=ior(itmpwd(j),mask)
 230     continue
         if(normflgc.eq.1) call chgnorm(1,npoints,1)
      endif
C
C     __________________________________________________________________
C
C     PROCESS THE 'ZQ' or ' ATTRIBUTE' MODE.
C
      if(mode(1:lenmode).eq.'zq'.or.
     *   mode(1:lenmode).eq.'attribute') then
         name2=cmsgin(4)
         if(msgtype(5).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(5)
         endif
         if(msgtype(6).eq.1) then
            clast  ='number'
            ilast  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            clast  ='number'
            ilast  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            ilast  =0
            clast  =cmsgin(6)
         endif
         if(msgtype(7).eq.1) then
            cstride  ='number'
            istride  =imsgin(7)
         elseif(msgtype(7).eq.2) then
            cstride  ='number'
            istride  =xmsgin(7)
         elseif(msgtype(7).eq.3) then
            istride  =0
            cstride  =cmsgin(7)
         endif
         inotflg=0
         if(nwds.lt.8)then
            write(logmess,9002)
 9002       format(' value and test missing ')
            call writloga('default',0,logmess,0,ierror)
         endif
         if(msgtype(8).eq.3.and.msgtype(9).ne.3) then
           k=8
           kk=9
         elseif(msgtype(9).eq.3) then
           k=9
           kk=8
         endif
         if(nwds.le.8) then
            operation='eq'
            kk=8
         else
            len1=icharlnf(cmsgin(k))
            if(cmsgin(k)(1:len1).eq.'n'.and.nwds.ge.k) inotflg=1
            operation=cmsgin(k)(1:len1)
         endif
         len2=icharlnf(name2)
         if(name2(1:len2).eq.'imd') then
            iprtindx=0
            call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
            call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
            do j=1,nmregs
              if(cmsgin(kk).eq.cmregs(j)) then
                iprtindx=matregs(j)
              endif
            enddo
            if(iprtindx.eq.0) goto 9998
            name2='imt'
            ivalue=iprtindx
            ivaltyp=1
         else
            ivaltyp=msgtype(kk)
            if(msgtype(kk).eq.1)ivalue=imsgin(kk)
            if(msgtype(kk).eq.2)xvalue=xmsgin(kk)
            if(msgtype(kk).eq.3)cvalue=cmsgin(kk)
         endif
         len2=icharlnf(name2)
         if((name2(1:3).eq.'itp') .and. (len2 .eq. 3)) name2='itp1'
         if((name2(1:3).eq.'imt') .and. (len2 .eq. 3)) name2='imt1'
         if((name2(1:3).eq.'icr') .and. (len2 .eq. 3)) name2='icr1'
         if((name2(1:3).eq.'isn') .and. (len2 .eq. 3)) name2='isn1'
         len2=icharlnf(name2)
         mpno=npoints
         if(msgtype(5).eq.1.or.msgtype(5).eq.2) then
c           if(ifirst.le.0) then
c              ifirst=ipointi
c           endif
c           ifirst=max(1,min(ifirst,npoints))
c           if(ilast.le.0) then
c              ilast=ipointj
c           endif
c           ilast=max(1,min(ilast,npoints))
            if(istride.le.0) then
               istride=1
            endif
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         len2=icharlnf(name2)
         do 300 i=1,mpno
            j=mpary1(i)
            if(i.eq.1) then
c      look for added attribute
               call mmgetpr(name2,cmo,ipxfield,icscode)
               call mmgettyp(ipxfield,ivaltypa,icscode)
               if(ivaltypa.eq.1.and.ivaltyp.eq.2)then
                  ivaltyp=1
                  ivalue=nint(xvalue)
               elseif(ivaltypa.eq.2.and.ivaltyp.eq.1)then
                  ivaltyp=2
                  xvalue=ivalue
               elseif((ivaltypa.eq.3.and.ivaltyp.ne.3).or.
     *               (ivaltypa.eq.3.and. (operation(1:2).ne.'eq'
     *                .and.operation(1:2).ne.'EQ'
     *                .and.operation(1:2).ne.'ne'
     *                .and.operation(1:2).ne.'NE'))) then
                  write(logmess,9004) name2,cmo,operation
 9005             format(' illegal type for operation',3a)
                  call writloga('default',0,logmess,0,ierrw)
                  go to 9999
               endif
C   found existing attribute
               if(icscode.ne.0) then
                  write(logmess,9005) name2,cmo
 9004             format(' pset cannot find attribute ',a,' in ',a)
                  call writloga('default',0,logmess,0,ierrw)
                  go to 9999
               else
                 call cmo_get_length
     *            (name2,cmo,ilen,irank,ier)
               endif
            endif
            do l=1,irank
               if(operation(1:2).eq.'EQ'.or.
     *                  operation(1:2).eq.'eq'.or.
     *                  nwds.lt.k.or.operation(1:1).eq.' ') then
                  if(ivaltyp.eq.1) then
                     if(ivalue.eq.ifield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  elseif(ivaltyp.eq.2) then
                     if(xvalue.eq.xfield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  elseif(ivaltyp.eq.3) then
                     if( cvalue.eq.cfield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  endif
                elseif(operation(1:2).eq.'NE'.or.
     *                  operation(1:2).eq.'ne') then
                  if(ivaltyp.eq.1) then
                     if(ivalue.ne.ifield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  elseif(ivaltyp.eq.2) then
                     if(xvalue.ne.xfield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  elseif(ivaltyp.eq.3) then
                     if( cvalue.ne.cfield((j-1)*irank+l))
     *                  itmpwd(j)=ior(itmpwd(j),mask)
                  endif
                elseif(operation(1:2).eq.'GE'.or.
     *                  operation(1:2).eq.'ge') then
                   if(ivaltyp.eq.1) then
                      if(ivalue.le.ifield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   elseif(ivaltyp.eq.2) then
                      if(xvalue.le.xfield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   endif
                elseif(operation(1:2).eq.'GT'.or.
     *                  operation(1:2).eq.'gt') then
                   if(ivaltyp.eq.1) then
                      if(ivalue.lt.ifield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   elseif(ivaltyp.eq.2) then
                      if(xvalue.lt.xfield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   endif
                elseif(operation(1:2).eq.'LE'.or.
     *                  operation(1:2).eq.'le') then
                   if(ivaltyp.eq.1) then
                      if(ivalue.ge.ifield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   elseif(ivaltyp.eq.2) then
                      if(xvalue.ge.xfield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   endif
                elseif(operation(1:2).eq.'LT'.or.
     *                  operation(1:2).eq.'lt') then
                   if(ivaltyp.eq.1) then
                      if(ivalue.gt.ifield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   elseif(ivaltyp.eq.2) then
                      if(xvalue.gt.xfield((j-1)*irank+l))
     *                    itmpwd(j)=ior(itmpwd(j),mask)
                   endif
                endif
            enddo
 300     continue
         ierror=0
      endif
C
C     __________________________________________________________________
C
C     PROCESS 'REGION' MODE
C        GET REGION NUMBER FOR THE REQUESTED REGION AND NO. 0F REGIONS.
C
C
      if (mode(1:lenmode) .eq. 'region') then
         iregnam=cmsgin(4)
         call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
C
C        ...............................................................
C        CHECK THAT THIS REGION EXISTS
C
         do ir=1,nregs
            len1=icharlnf(iregnam)
            len2=icharlnf(cregs(ir))
            len3=max(len1,len2)
            if (cregs(ir)(1:len3).eq.iregnam(1:len3).and.len1.eq.len2)
     *             then
               iregck=ir
               go to 305
            endif
         enddo
         ierror=1
         write(logmess,9000) iregnam
 9000    format('  ERROR - REGION ',a8,' DOES NOT EXIST')
         call writloga('default',1,logmess,1,ierr)
         go to 9998
C        ...............................................................
C        GET THE SEARCH RANGE.
C
 305     call get_epsilon('epsilonl', srchval)
C
C        ...............................................................
C        GET MEMORY FOR ARRAYS CONTAINING REGION AND SURFACE POINTERS
c
         lenmm4=npoints+100
         call mmgetblk('iregno',isubname,ipregno,lenmm4,2,ics)
         call mmgetblk('isurfno',isubname,ipsurfno,lenmm4,2,ics)
C
C        ...............................................................
C        CALL getregv TO FIND THE REGIONS THE POINTS LIE IN
C
         npts=npoints
         do i=1,npts
            iregno(i)=-1
            isurfno(i)=-1
         enddo
         call tstregv(xic,yic,zic,npts,srchval,
     &                iregnam,iregno,
     &                ierr)
C
C        ...............................................................
C        LOOP THROUGH THE SELECTED POINTS
C
         if(nwds.le.4) then
            ifirst=1
         elseif(msgtype(5).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(5)
         endif
         if(nwds.le.4) then
            ilast=0
         elseif(msgtype(6).eq.1) then
            clast  ='number'
            ilast  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            clast  ='number'
            ilast  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            ilast  =0
            clast  =cmsgin(6)
         endif
         if(nwds.le.4) then
            istride=0
         elseif(msgtype(7).eq.1) then
            cstride  ='number'
            istride  =imsgin(7)
         elseif(msgtype(7).eq.2) then
            cstride  ='number'
            istride  =xmsgin(7)
         elseif(msgtype(7).eq.3) then
            istride  =0
            cstride  =cmsgin(7)
         endif
         if((msgtype(5).eq.1.or.msgtype(5).eq.2.and.nwds.gt.4).or.
     *      nwds.le.4) then
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do 400 i=1,mpno
            j=mpary1(i)
            if (iregno(j) .eq. 1)
     &         itmpwd(j)=ior(itmpwd(j),mask)
 
  400    continue
C
         ierror=0
C
      endif
C
C     __________________________________________________________________
C
C     PROCESS 'MREGION' MODE
C
      if (mode(1:lenmode) .eq. 'regions' .or.
     *    mode(1:lenmode) .eq. 'mregion') then
         iregnam=cmsgin(4)
       call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
C
C        ...............................................................
C        CHECK THAT THIS MREGION EXISTS
C
         do ir=1,nmregs
            len1=icharlnf(iregnam)
            len2=icharlnf(cmregs(ir))
            len3=min(len1,len2)
            if (cmregs(ir)(1:len3).eq.iregnam(1:len3).and.len1.eq.len2)
     *           then
               iregck=ir
               go to 407
            endif
         enddo
         ierror=1
         write(logmess,9001) iregnam
 9001    format('  ERROR - MREGION ',a8,' DOES NOT EXIST')
         call writloga('default',1,logmess,1,ierr)
         go to 9998
C        ...............................................................
C        GET THE SEARCH RANGE.
C
 407     call get_epsilon('epsilonl', srchval)
C
C        ...............................................................
C        GET MEMORY FOR ARRAYS CONTAINING REGION AND SURFACE POINTERS
C
 
         lenmm4=npoints+100
         call mmgetblk('iregno',isubname,ipregno,lenmm4,2,ics)
         call mmgetblk('isurfno',isubname,ipsurfno,lenmm4,2,ics)
C        ...............................................................
C        CALL getregv TO FIND THE REGIONS THE POINTS LIE IN
C
         npts=npoints
         call getregv1(xic,yic,zic,itp1,npts,srchval,'mregion',iregck,
     &                cmo,iregno,isurfno,
     &                ierr)
C
C        ...............................................................
C        LOOP THROUGH THE SELECTED POINTS
C
         if(msgtype(5).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(5)
         endif
         if(msgtype(6).eq.1) then
            clast  ='number'
            ilast  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            clast  ='number'
            ilast  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            ilast  =0
            clast  =cmsgin(6)
         endif
         if(msgtype(7).eq.1) then
            cstride  ='number'
            istride  =imsgin(7)
         elseif(msgtype(7).eq.2) then
            cstride  ='number'
            istride  =xmsgin(7)
         elseif(msgtype(7).eq.3) then
            istride  =0
            cstride  =cmsgin(7)
         endif
         if(msgtype(5).eq.1.or.msgtype(5).eq.2) then
c           if(ifirst.le.0) then
c              ifirst=ipointi
c           endif
c           ifirst=max(1,min(ifirst,npoints))
c           if(ilast.le.0) then
c              ilast=ipointj
c           endif
c           ilast=max(1,min(ilast,npoints))
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do i=1,mpno
            j=mpary1(i)
            if(mode(1:lenmode).eq.'regions') then
               if (iregno(j) .lt. 0)
     &            itmpwd(j)=ior(itmpwd(j),mask)
            else
               if (iregno(j) .eq. iregck)
     &            itmpwd(j)=ior(itmpwd(j),mask)
            endif
         enddo
C
         ierror=0
C
      endif
C
C     __________________________________________________________________
C
C     PROCESS 'SURFACE' MODE
C        GET SURFACE NUMBER FOR THE REQUESTED SURFACE.
C
C
 
      if (mode(1:lenmode) .eq. 'surface') then
C        ...............................................................
C        GET MEMORY FOR ARRAYS CONTAINING REGION AND SURFACE POINTERS
C        SET THE SURFACE NAMES
C
         lenmm4=npoints+100
         call mmgetblk('isurfno',isubname,ipsurfno,lenmm4,2,ics)
         isurfnam=cmsgin(4)
         call mmfindbk('csall',geom_name,ipcsall,length,ierror)
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
C
C        ...............................................................
C        CHECK THAT THIS surface EXISTS
C
         len1=icharlnf(isurfnam)
         if (isurfnam(1:len1).eq.'-all-') then
            call unpacktp('intrface','set',npoints,ipitp1,ipsurfno,ics)
            call unpacktp('boundary','or',npoints,ipitp1,ipsurfno,ics)
            go to 327
         elseif ((isurfnam(1:len1).eq.'-interface-') .or.
     *           (isurfnam(1:len1).eq.'-intrface-')) then
            call unpacktp('intrface','set',npoints,ipitp1,ipsurfno,ics)
            go to 327
         elseif (isurfnam(1:len1).eq.'-boundary-') then
            call unpacktp('boundary','set',npoints,ipitp1,ipsurfno,ics)
            go to 327
         else
            do is=1,nsurf
               len2=icharlnf(csall(is))
               len3=max(len1,len2)
               if (csall(is)(1:len3).eq.isurfnam(1:len3)
     *             .and.len1.eq.len2) then
                  isurfck=is
                  go to 325
               endif
            enddo
            ierror=1
            write(logmess,9025) isurfnam
 9025       format('  ERROR - SURFACE ',a8,' DOES NOT EXIST')
            call writloga('default',1,logmess,1,ierr)
            go to 9998
         endif
C        ...............................................................
C        GET THE SEARCH RANGE.
C
 325     call get_epsilon('epsilonl', srchval)
C
C        ...............................................................
C        CALL surftstv TO FIND THE REGIONS THE POINTS LIE IN
C
         npts=npoints
         do i=1,npts
            isurfno(i)=0
         enddo
         itestit='eq'
         call surftstv(xic,yic,zic,npts,srchval,cmo,istype(isurfck),
     &     surfparam(offsparam(isurfck)+1),sheetnm(isurfck),
     &     itestit,isurfno)
C
C        ...............................................................
C        LOOP THROUGH THE SELECTED POINTS
C
327      if(nwds.le.4) then
            ifirst=1
         elseif(msgtype(5).eq.1) then
            cfirst  ='number'
            ifirst  =imsgin(5)
         elseif(msgtype(5).eq.2) then
            cfirst  ='number'
            ifirst  =xmsgin(5)
         elseif(msgtype(5).eq.3) then
            ifirst  =0
            cfirst  =cmsgin(5)
         endif
         if(nwds.le.4) then
            ilast=0
         elseif(msgtype(6).eq.1) then
            clast  ='number'
            ilast  =imsgin(6)
         elseif(msgtype(6).eq.2) then
            clast  ='number'
            ilast  =xmsgin(6)
         elseif(msgtype(6).eq.3) then
            ilast  =0
            clast  =cmsgin(6)
         endif
         if(nwds.le.4) then
            istride=0
         elseif(msgtype(7).eq.1) then
            cstride  ='number'
            istride  =imsgin(7)
         elseif(msgtype(7).eq.2) then
            cstride  ='number'
            istride  =xmsgin(7)
         elseif(msgtype(7).eq.3) then
            istride  =0
            cstride  =cmsgin(7)
         endif
         if((msgtype(5).eq.1.or.msgtype(5).eq.2.and.nwds.gt.4).or.
     *      nwds.le.4) then
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            endif
            if(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do  i=1,mpno
            j=mpary1(i)
            if (isurfno(j) .eq. 1)
     &         itmpwd(j)=ior(itmpwd(j),mask)
         enddo
C
         ierror=0
C
      endif
C
C     __________________________________________________________________
C
C     PROCESS 'CONSTRAINTS' MODE
C
      if (mode(1:lenmode) .eq. 'constraints'.or.
     *   mode(1:lenmode) .eq. 'constraint') then
C        ...............................................................
C       access constraint info
C
         if(nwds.lt.4.or.msgtype(4).ne.1) go to 9998
         numconstraints=imsgin(4)
         if(numconstraints.le.0.or.numconstraints.gt.3) then
            write(logmess,6048)
 6048       format(' Number of  constraints must be > 1 and <= 3 ')
            call writloga('default',0,logmess,0,ierrw)
            ierror=1
            go to 9998
         endif
         call cmo_get_intinfo('nconbnd',cmo,nconbnd,ilen,ityp,ierr)
         call cmo_get_info('icontab',cmo,ipicontab,ilen,ityp,ierr)
         if(ierr.ne.0)  then
            write(logmess,6049)
 6049       format(' No constraint information ')
            call writloga('default',0,logmess,0,ierrw)
            ierror=1
            go to 9998
         endif
C        ...............................................................
C        LOOP THROUGH THE SELECTED POINTS
C
         if(nwds.lt.7) then
            ifirst=1
            ilast=0
            istride=0
         elseif(msgtype(5).eq.1.and.msgtype(6).eq.1.and.
     *          msgtype(7).eq.1) then
            ifirst  =imsgin(5)
            ilast  =imsgin(6)
            istride  =imsgin(7)
         elseif(msgtype(5).eq.2.and.msgtype(6).eq.2.and.
     *          msgtype(7).eq.2) then
            ifirst  =xmsgin(5)
            ilast  =xmsgin(6)
            istride  =xmsgin(7)
         elseif(msgtype(5).eq.3.and.msgtype(6).eq.3.and.
     *          msgtype(7).eq.3) then
            cfirst  =cmsgin(5)
            clast  =cmsgin(6)
            cstride  =cmsgin(7)
         else
            write(logmess,6050) name
 6050       format(' Error in pset command ',a)
            call writloga('default',0,logmess,0,ierrw)
            ierror=1
            go to 9998
         endif
         if((msgtype(5).ne.3.and.nwds.gt.4).or.
     *      nwds.le.4) then
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            elseif(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do  i=1,mpno
            j=mpary1(i)
            icrtst=icr1(j)
            if(icrtst.ge.1.and.icrtst.le.nconbnd) then
               if(icontab(1,icrtst).eq.numconstraints)
     &            itmpwd(j)=ior(itmpwd(j),mask)
            endif
         enddo
C
         ierror=0
C
      endif
C     __________________________________________________________________
C
C     PROCESS 'MATERIALS' MODE
C
      if (mode(1:lenmode) .eq. 'materials'.or.
     *   mode(1:lenmode) .eq. 'mats') then
C        ...............................................................
C       get number of materials
C
         if(nwds.lt.4.or.msgtype(4).ne.1) go to 9998
         nummaterials=imsgin(4)
         if(nummaterials.le.0.) then
            write(logmess,6052)
 6052       format(' Number of materials must be > 0')
            call writloga('default',0,logmess,0,ierrw)
            ierror=1
            go to 9998
         endif
C        ...............................................................
C        LOOP THROUGH THE SELECTED POINTS
C
         if(nwds.lt.7) then
            ifirst=1
            ilast=0
            istride=0
         elseif(msgtype(5).eq.1.and.msgtype(6).eq.1.and.
     *          msgtype(7).eq.1) then
            ifirst  =imsgin(5)
            ilast  =imsgin(6)
            istride  =imsgin(7)
         elseif(msgtype(5).eq.2.and.msgtype(6).eq.2.and.
     *          msgtype(7).eq.2) then
            ifirst  =xmsgin(5)
            ilast  =xmsgin(6)
            istride  =xmsgin(7)
         elseif(msgtype(5).eq.3.and.msgtype(6).eq.3.and.
     *          msgtype(7).eq.3) then
            cfirst  =cmsgin(5)
            clast  =cmsgin(6)
            cstride  =cmsgin(7)
         else
            write(logmess,6051) name
 6051       format(' Error in pset command ',a)
            call writloga('default',0,logmess,0,ierrw)
            ierror=1
            go to 9998
         endif
         if((msgtype(5).ne.3.and.nwds.gt.4).or.
     *      nwds.le.4) then
            if(ifirst.eq.0.and.ilast.eq.0) then
                 ifirst=max(1,ipointi)
                 ipointj=max(ipointi,ipointj)
                 ilast=max(1,ipointj)
            elseif(ifirst.eq.1.and.ilast.eq.0) then
                 ilast=max(1,npoints)
            endif
            if(istride.le.0) then
               istride=1
            endif
            istride=min(istride,npoints)
            call pntlimn(ifirst,ilast,istride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         else
            call pntlimc(cfirst,clast,cstride,ipmpary1,mpno,
     *                   npoints,isetwd,itp1)
         endif
         do  i=1,mpno
            j=mpary1(i)
            next=isn1(j)
            icount=0
            if(next.ne.0) then
              if(itp1(j).ne.ifitpcup) then
                 nmats=1
              else
                 nmats=0
              endif
              do while(next.ne.j.and.icount.lt.10000)
                 if(itp1(next).ne.ifitpcup) then
                    icount=icount+1
                    nmats=nmats+1
                 endif
                 next=isn1(next)
              enddo
            else
              nmats=1
            endif
            if(nmats.eq.nummaterials)
     &            itmpwd(j)=ior(itmpwd(j),mask)
         enddo
C
         ierror=0
C
      endif
C     __________________________________________________________________
C
C     PROCESS 'DELETE' MODE
C
      if (mode(1:lenmode) .eq. 'delete'.or.
     *  mode(1:lenmode) .eq. 'release') then
C
C        ...............................................................
C        SEE IF THE SET EXISTS
C
         if (ndup .eq. 0) then
            write(logmess,1005) name
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
C
C        ...............................................................
C        FIND LAST SET
C
         icnt=0
         do 410 i=1,nbitsmax
            if (psetnames(i) .ne. ' ') icnt=i
  410    continue
C
C        ...............................................................
C        GET THE LIST OF POINTS FOR THE LAST SET
C
         cpt1='psetnames'
         cpt2='get'
         cpt3=psetnames(icnt)
         mpno=npoints
         call pntlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *                npoints,isetwd,itp1)
 
C
C        ...............................................................
C        SET POINTS FOR LAST SET TO CURRENT SET
C
         do 420 i=1,mpno
            i1=mpary1(i)
            itmpwd(i1)=ior(itmpwd(i1),mask)
 420     continue
         isetchg=isetchg+1
         psetnames(ibitpos)=psetnames(icnt)
 
C        ...............................................................
C        ZERO OUT LAST POINT SET
C
         mask2=imask(icnt)
         nmask2=not(mask2)
         do 430 i=1,npoints
            isetwd(i)=iand(itmpwd(i),nmask2)
 430     continue
         isetchg=isetchg+1
         psetnames(icnt)='-def-'
         number_of_psets=number_of_psets-1
         call cmo_set_info('number_of_psets',cmo,number_of_psets,
     *     1,1,icscode)
 
         write(logmess,6020) name
 6020    format(' PSET ',a8,' DELETED')
         call writloga('default',1,logmess,1,ierrw)
         ierror=0
         go to 9998
      endif
C
C     ******************************************************************
C
C     PLACE TEMP. DATA INTO SETWORD.
C
 9996 do 490 i=1,npoints
         isetwd(i)=itmpwd(i)
  490 continue
C
C     ******************************************************************
C
C     PRINT OUT PSET INFO.
C
 9997 continue

      if(mode(1:lenmode) .ne. 'write') then
      cpt1='psetnames'
      cpt2='get'
      cpt3=name
      mpno=npoints
      call pntlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *             npoints,isetwd,itp1)
 
      write(logmess,6010)name(1:icharlnf(name)),mpno
 6010 format(' THE PSET  ',a,'  HAS ',i10,' POINTS')
      call writloga('default',1,logmess,0,ierr)

      if(idebug.ge.1.or.iprint.eq.1) then
      n_entry_per_line = 8
        do  i=1,mpno,n_entry_per_line
          write(logmess,6011)
     *     (mpary1(j),j=i,min0(i-1+n_entry_per_line,mpno))
 6011      format(2x,8(i8,1x))
          if (iprint .eq. 0)
     *       call writloga('bat',0,logmess,0,ierr)
          if (iprint .eq. 1)
     *       call writloga('default',0,logmess,0,ierr)
        enddo
      endif
C     ******************************************************************
C     Write pset info to a file.
C
      elseif(mode(1:lenmode) .eq. 'write') then
C
C     Count up the number of psets for output.
C
      icnt = 1
C
C     Use user supplied number for zone id number in output file
C
      if_id_zone_number = 0
      if(nwds .ge.6 .and. msgtype(6).ne.0) then
         id_zone_number = imsgin(6)
         if_id_zone_number = 1
      endif
C
C
C     Output pset lists
C
      do j=1,nbitsmax
        if((name(1:5) .eq. '-all-') .and.
     *     (psetnames(j) .ne.' ')   .and.
     *     (psetnames(j)(1:5) .ne. '-def-'))then
           index = j
           iprint = 1
        elseif(psetnames(j).eq.name) then
          index=j
          iprint = 1
        else
          iprint = 0
       endif

      if(iprint .eq. 1)then

      file_name = cmsgin(4)
      if (name(1:5) .eq. '-all-') then
          file_name = file_name(1:icharlnf(file_name)) // '_' //
     *      psetnames(j)(1:icharlnf(psetnames(j)))
      endif
      lenname = icharlnf(file_name)

      if(ioformat .eq. 1)then
      if(file_name(lenname-9:lenname) .ne. '.vertexset') then
         file_name = file_name(1:lenname) // '.vertexset'
         lenname = lenname + 10
         write(logmess,5000)'PSET: Appended .vertexset to the file name'
 5000    format(a)
         call writloga('default',0,logmess,0,ierr)
      endif
      elseif(ioformat .eq. 2)then
      if(file_name(lenname-4:lenname) .ne. '.zone') then
         file_name = file_name(1:lenname) // '.zone'
         lenname = lenname + 5
         write(logmess,5000)'PSET: Appended .zone to the file name'
         call writloga('default',0,logmess,0,ierr)
      endif
      elseif(ioformat .eq. 3)then
      if(file_name(lenname-4:lenname) .ne. '.zonn') then
         file_name = file_name(1:lenname) // '.zonn'
         lenname = lenname + 5
         write(logmess,5000)'PSET: Appended .zonn to the file name'
         call writloga('default',0,logmess,0,ierr)
      endif
      endif
C
C     Open file for pset output.
C
      iunit = nextlun()

      write(logmess,7007)name(1:icharlnf(name)),icnt
 7007 format('PSET: OUTPUT ',a,'  ',i3,' PSETS TO FILE ')
      call writloga('default',0,logmess,0,ierr)
      print *, 5, msgtype(5), cmsgin(5)
      if ((msgtype(5).eq.3.and.cmsgin(5)(1:5).eq.'ascii') .or.
     *    (msgtype(5).eq.3.and.cmsgin(5)(1:5).eq.'-def-') .or.
     *    (nwds .eq. 4)                                   .or.
     *    (msgtype(5).eq.3.and.cmsgin(5).eq.' ')) then
         open(unit=iunit, file = file_name, form = 'formatted')
         iotype = 1
      elseif(msgtype(5).eq.3.and.cmsgin(5)(1:6).eq.'binary') then
         open(unit=iunit, file = file_name, form = 'unformatted')
         iotype = 0
      endif
      
      if(iotype .eq. 1)then
      if(ioformat .eq. 1)then
      write(iunit,7005) 'pset ascii',icnt
 7005 format(a,i10)
      elseif(ioformat .eq. 2)then
      write(iunit,7013)'zone'
      elseif(ioformat .eq. 3)then
      write(iunit,7013)'zonn'
      endif
      elseif(iotype .eq. 0)then
      write(iunit) 'pset unformatted',icnt
      endif

      cpt1='psetnames'
      cpt2='get'
      cpt3=psetnames(index)
      mpno=npoints
      call pntlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *             npoints,isetwd,itp1)
 
      if(iotype .eq. 1)then
         if(ioformat .eq. 1)then
            write(iunit,7010) 
     1         psetnames(index)(1:icharlnf(psetnames(index))),
     2         index,mpno
 7010       format(a,i5,1x,i10)
         elseif((ioformat .eq. 2).or.(ioformat .eq. 3))then
           if(if_id_zone_number .eq. 0)then
            write(iunit,7012)index,
     1         psetnames(index)(1:icharlnf(psetnames(index)))
           elseif(if_id_zone_number .eq. 1)then
            write(iunit,7012)id_zone_number,
     1         psetnames(index)(1:icharlnf(psetnames(index)))
           endif
 7012       format(i6.6,5x,a)
            write(iunit,7013)'nnum'
 7013       format(a)
            write(iunit,7014)mpno
 7014       format(i10)
         endif
         n_entry_per_line = 10
         do  i=1,mpno,n_entry_per_line
          write(iunit,7011)
     *     (mpary1(k),k=i,min0(i-1+n_entry_per_line,mpno))
 7011      format(10(i10,1x))
        enddo
      elseif(iotype .eq. 0)then
         write(iunit) psetnames(index)(1:32),index,mpno
         write(iunit) ( mpary1(k),k=1,mpno )
      endif
 
      if((ioformat .eq. 2).or.(ioformat .eq. 3))then
         write(iunit,*)' '
         write(iunit,7013)'stop'
      endif
      close(iunit)

      endif
      enddo
      endif

c
C     ******************************************************************
C
C     RELEASE THE LOCAL MEMORY ALLOCATED FOR THIS ROUTINE.
C
 9998 continue
c
      call mmrelprt(isubname,icscode)
C
C     ******************************************************************
C
 1000 format('ERROR - THE NUMBER OF SETS EXCEEDS ',i4)
 1001 format(6x,'Set name',2x,a8,2x,'will be overwritten !')
 1002 format(' ERROR - The new set name is the same as one
     *                  of the sets to be worked on !')
 1003 format(' THERE ARE NO POINTS IN THE SET !!')
 1005 format(' ERROR - THE NAME ',2x,a8,2x,' DOES NOT EXIST !')
C
C     ******************************************************************
 9999 continue
C
      return
      end
