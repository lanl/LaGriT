       subroutine hextotet_hybrid(ioption,cmotet,cmohex,ierror)
C
C#####################################################################
C
C
C      PURPOSE -
C
C      Create a tet or tri grid. The parameter ioption specifies how
C   the element is divided.
C
C   ioption = -2 ==>    If QUAD = 2, HEX = 6, PRISM = 3, PYRAMID = 4
C   ioption = -1 ==>    HYBRID to TRI or HYBRID
C   ioption =  0 ==>    HYBRID to TRI or TET
C   ioption =  2 ==>  2 TRI   PER QUAD     NO NEW POINTS
C   ioption =  3 ==>  3 TETS  PER PRISM    NO NEW POINTS
C   ioption =  4 ==>  4 TRI   PER QUAD      1 NEW POINT PER QUAD
C   ioption =  4 ==>  4 TETS  PER PYRAMID   1 NEW POINT PER PYRAMID
C   ioption =  5 ==>  5 TETS  PER HEX      NO NEW POINTS
C   ioption =  6 ==>  6 TETS  PER HEX      NO NEW POINTS
C   ioption =  7 ==>  6 PYR   PER HEX      NO NEW POINTS
C   ioption = 14 ==> 14 TETS  PER PRISM     4 NEW POINTS (1 + 3 FACES)
C   ioption = 18 ==> 18 TETS  PER PRISM     6 NEW POINTS (1 + 5 FACES)
C   ioption = 24 ==> 24 TETS  PER HEX       7 NEW POINTS (1 + 6 FACES)
C
C   Reccomended options include:
C        2 TRI   PER QUAD
C        4 TRI   PER QUAD
C        3 TETS  PER PRISM
C        6 TETS  PER HEX
C       24 TETS  PER HEX
C
C
C      INPUT ARGUMENTS -
C         ioption   - specifies how elements are divided
C         cmotet    - output cmo
C         cmohex    - input cmo
C         ierror    - 0 if no errors
C
C      CHANGE HISTORY -
C
C
C        $Log: hextotet_hybrid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.19   16 Oct 2007 10:49:26   gable
CPVCS    Removed some print statements I accidently left in during debugging.
CPVCS    
CPVCS       Rev 1.18   01 Oct 2007 08:20:38   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty. Added a few debug flag output options. Fixed bug
CPVCS    that caused crash. Arrays of incompatible size were being compied into one another
CPVCS    when attempting to save element and node attributes.
CPVCS    
CPVCS       Rev 1.17   30 Sep 2004 09:14:24   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS
CPVCS       Rev 1.16   26 Feb 2002 17:16:46   tam
CPVCS    change dismat loop from 1000 to numhex
CPVCS
CPVCS       Rev 1.15   26 Feb 2002 12:41:00   dcg
CPVCS    get rid of nplen - this caused grief when coder forgets to
CPVCS    memory manage an array
CPVCS
CPVCS       Rev 1.14   14 Dec 2001 09:05:36   nnc
CPVCS    Removed duplicate variable declaration for absoft compiler.
CPVCS
CPVCS       Rev 1.13   06 Dec 2001 12:16:04   tam
CPVCS    assign mesh_type before using cmotet
CPVCS
CPVCS       Rev 1.11   14 Nov 2001 16:26:40   dcg
CPVCS    initialize ilist values to 1 - this avoids a problem in
CPVCS    cmo_interpolate that occurs when a zero index is passed
CPVCS    this fix assumes that the corresponding weight is zero
CPVCS    thus causing this node to have no effect on the interpolation
CPVCS
CPVCS       Rev 1.10   03 Aug 2001 09:09:44   tam
CPVCS    added default for coption that checks for mesh_type
CPVCS
CPVCS       Rev 1.9   03 Oct 2000 09:51:48   dcg
CPVCS    replace variable ialias in subroutine filter_htt_pts
CPVCS    with variable ialiasin to avoid confusion with
CPVCS    cmo attribute ialias
CPVCS
CPVCS       Rev 1.8   05 May 2000 09:07:46   jtg
CPVCS    nwrite added as argument to elmtestd
CPVCS
CPVCS       Rev 1.7   25 Apr 2000 14:45:30   dcg
CPVCS    set number of edges for new mesh object
CPVCS    set number of default attributes to 64 -
CPVCS    do not release memory for ialiasp array until end of routine
CPVCS
CPVCS       Rev 1.6   21 Apr 2000 07:05:26   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS
CPVCS       Rev 1.5   06 Apr 2000 10:35:26   tam
CPVCS    remove hardwired iremove variables for PRISM option
CPVCS
CPVCS       Rev 1.5   06 Apr 2000 09:42:02   tam
CPVCS    remove hardwired iremove variables for PRISM option
CPVCS
CPVCS       Rev 1.4   Wed Apr 05 13:34:30 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.3   03 Apr 2000 18:36:16   gable
CPVCS    Set mbndry according to problem size instead of old hardwired 16,000,000 value.
CPVCS
CPVCS       Rev 1.2   07 Mar 2000 09:39:44   tam
CPVCS    heavily modified to make implicit none
CPVCS    moved allocation of new attributes after loop thru old attributes
CPVCS    copy nelement attribute values from cmohex, like nnode attributes
CPVCS    default for iremove_vol = iremove_dups = 0, icheckpt = 1
CPVCS    allocate iign instead of old cmo attribute ign1
CPVCS    cleaned up declarations
CPVCS
CPVCS       Rev 1.1   Tue Feb 15 11:35:30 2000   dcg
CPVCS    use correct global names
CPVCS    put in missing comma in cmo_get_info call
CPVCS
CPVCS       Rev 1.0   Mon Jan 31 16:26:14 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.26   Fri Jun 25 16:56:28 1999   tam
CPVCS    fixed bad syntax in dotask call
CPVCS    allow removal of volumes for prism option
CPVCS    commented out extra pointer assignment with mmfindbk
CPVCS    which resulted in bogus values written to user attributes
CPVCS
CPVCS       Rev 1.25   Fri Mar 19 12:48:22 1999   gable
CPVCS    Major update. Starting with version 1.21 from the x3d archive
CPVCS    Changed default so that zero volumes are not removed.
CPVCS    Default iremove = 0 , irmvolume=0
CPVCS    Wrote a new point filtering routine. Old version used
CPVCS    an n**2 search. New version uses a modified version of the
CPVCS    filter.f routine. This is appended as the filter_htt_pts.f routine.
CPVCS
CPVCS    This piece of code still needs work. Use with caution if zero volume
CPVCS    elements need to be eliminated. Can leave holes in the mesh.
CPVCS
CPVCS       Rev 1.24   Thu May 29 11:19:52 1997   dcg
CPVCS    restore overwritten revisions
CPVCS
CPVCS       Rev 1.23   Wed May 07 16:37:30 1997   tam
CPVCS    cmo addatt to variable cmotet instead of "cmotet"
CPVCS
CPVCS       Rev 1.22   Sat Apr 26 22:43:24 1997   gable
CPVCS    Add an integer elements array, itetnorm, when the
CPVCS    option is 3, prism to 3 tets. itetnorm is used to
CPVCS    calculate the principle axis for anisotropy.
CPVCS
CPVCS       Rev 1.21   Fri Apr 25 15:40:00 1997   tam
CPVCS    get value of idebug to control amount of error output
CPVCS
CPVCS       Rev 1.20   Wed Apr 23 11:16:18 1997   gable
CPVCS    Modified by Christian Cordes
CPVCS    Change prisim to 3 tet code. Before these changes every
CPVCS    prism was converted to tets without regard to the neighbors.
CPVCS    This could result in mismatched faces. New version will choose
CPVCS    between 6 possible connectivities for prism to 3 tets. Criteria
CPVCS    is to always cross the shorter diagonal of the quad faces. If the
CPVCS    quad face has equal length diagonals, the diagonal will start at
CPVCS    the minimum (or was it max??) global node number of the lower
CPVCS    (local node number 1,2,3) triangle face.
CPVCS
CPVCS       Rev 1.18   Mon Mar 03 15:25:44 1997   tam
CPVCS    setup model options: iradavg, iremove, icheckpt, irmvolume
CPVCS
CPVCS       Rev 1.17   Tue Feb 25 21:20:02 1997   het
CPVCS    Add a new option that converts prisms to 18 tets.
CPVCS
CPVCS       Rev 1.16   Mon Feb 24 08:00:30 1997   het
CPVCS    Correct errors related to the interpolation of attributes to the tet mesh.
CPVCS
CPVCS       Rev 1.15   Wed Nov 27 14:25:38 1996   het
CPVCS    Set the number of tets in the CMO before calling addmesh_delete
CPVCS
CPVCS       Rev 1.14   Fri Nov 22 06:09:30 1996   het
CPVCS    Use the addmesh_delete routine to delete elements.
CPVCS
CPVCS       Rev 1.12   Mon Nov 11 20:59:54 1996   het
CPVCS    Add the hextotet option for hex-to-pri.
CPVCS
CPVCS       Rev 1.8   Wed Jul 24 17:37:28 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face numbers into jtet array
CPVCS
CPVCS       Rev 1.7   Tue Jul 16 14:56:46 1996   dcg
CPVCS    propogate user defined attributes
CPVCS
CPVCS       Rev 1.6   Wed Jun 19 10:18:18 1996   het
CPVCS    Fix an error for hybrid (quad-to-tri) grids.
CPVCS
CPVCS       Rev 1.5   Fri May 24 14:00:36 1996   het
CPVCS    Fix a memory management error and add the 6-tet brick option.
CPVCS
CPVCS       Rev 1.4   Wed May 22 07:05:26 1996   het
CPVCS    Fix an memory management error.
CPVCS
CPVCS       Rev 1.3   Thu May 02 12:41:48 1996   dcg
CPVCS    check for velocity, density and pressure attributes
CPVCS
CPVCS       Rev 1.2   Tue Apr 30 07:30:04 1996   het
CPVCS    Fix a memory management error.
CPVCS
CPVCS       Rev 1.1   Tue Mar 05 12:49:42 1996   dcg
CPVCS    remove icn1, int1
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 16:53:54 1996   dcg
CPVCS    Initial revision.
C#######################################################################
C
c     implicit real*8 (a-h, o-z)
      implicit none
C
      character*132 wlog
      character*4096 cbuf
 
C
C#######################################################################
C
      include "chydro.h"
      include "cmo.h"
      include "local_element.h"
C
C ######################################################################
C     cmo.h defines
C     isetwd, imt1, itp1, icr1, isn1 (ign1 old)
C     xic, yic, zic
C     itetclr, itettyp, itet, itet1, jtet, jtet1
C     *****************************************************************
C
      pointer (ipvel,vels)
      pointer (ipdens,dens)
      pointer (ippres,pres)
      REAL*8 vels(3,*), dens(*), pres(*)
      logical ifvels, ifpres, ifdens
 
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetoff(*), jtetoff(*)
 
C
      pointer (ipimt1hex, imt1hex)
      pointer (ipitp1hex, itp1hex)
      pointer (ipicr1hex, icr1hex)
      pointer (ipihexclr, ihexclr)
      pointer (ipihextyp, ihextyp)
      pointer (ipihexoff, ihexoff)
      pointer (ipjhexoff, jhexoff)
      pointer (ipihexnn,  ihexnn1)
      pointer (ipjhexnn,  jhexnn1)
      pointer (ipihexclr5, ihexclr5)
      integer imt1hex(*), itp1hex(*), icr1hex(*),
     *        ihexclr(*), ihextyp(*), ihexoff(*),
     *        jhexoff(*), ihexnn1(*), jhexnn1(*),
     *        ihexclr5(*)
C
      pointer (ipktet, ktet)
      pointer (ipialiasp, ialiasp)
      pointer (ipitdel, itdel)
      integer ktet(*), ialiasp(*), itdel(*)
 
c     replace ign1 with iign so not confused with old attribute
c     that is no longer supported, holds generation level of points
c     may be able to remove all references to this attribute
      pointer (ipiign, iign)
      integer iign(*)
C
      pointer (ipireal1, ireal1)
      pointer (ipitetnorm, itetnorm)
      pointer (ipidone, idone)
      integer ireal1(*), itetnorm(*), idone(*)
C
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      real*8  xic2(*), yic2(*), zic2(*)
      pointer (ipxhex, xhex)
      pointer (ipyhex, yhex)
      pointer (ipzhex, zhex)
      real*8  xhex(*), yhex(*), zhex(*)
C
      integer ihex5tet(4,5,2)
      data ihex5tet / 1, 2, 4, 5,
     *                3, 2, 7, 4,
     *                6, 2, 5, 7,
     *                8, 4, 7, 5,
     *                2, 4, 5, 7,
     *                2, 1, 6, 3,
     *                4, 1, 3, 8,
     *                5, 1, 8, 6,
     *                7, 3, 6, 8,
     *                1, 3, 8, 6 /
C
      integer ihex6tet(4,6)
      data ihex6tet / 6, 7, 2, 8,
     *                2, 7, 3, 8,
     *                2, 4, 8, 3,
     *                5, 6, 2, 8,
     *                5, 2, 1, 8,
     *                1, 4, 8, 2 /
C
      integer ipri3tet(4,3,6)
      data ipri3tet / 1,2,3,6,1,2,6,5,1,4,5,6,
     *                1,2,3,5,1,3,4,5,3,4,5,6,
     *                1,2,3,5,1,3,6,5,1,4,5,6,
     *                1,2,3,4,2,3,4,6,2,4,5,6,
     *                1,2,3,6,1,2,6,4,2,4,5,6,
     *                1,2,3,4,2,3,4,5,3,4,5,6 /
C
      pointer (ipiattyp,iattyp)
      pointer (ipiattrank,iattrank)
      pointer (ipatthex,iatthex)
      pointer (iplist,list)
      pointer (ipilist,ilist)
      integer iattyp(*), iattrank(*),
     *        iatthex(*), list(*), ilist(*)
 
      pointer (ipatt,xatt)
      pointer (ipatt,iiatt)
      real*8  xatt(*)
      integer iiatt(*)
 
      pointer (ipatthex,xatthex)
      pointer (ipxweight,xweight)
      real*8  xatthex(*), xweight(*)
 
      pointer (ipcattlen,cattlen)
      pointer (ipcattinterp,cattinterp)
      pointer (ipcattname,cattname)
      character*32 cattlen(*), cattinterp(*),
     *             cattname(*)
 
 
      integer ioption, numhex, numtet, imesh_type
      integer npoints, npoints1, ipointi,ipointj, icount, iwcnt
      integer ierror_return, iwerr, ier, ierr, ierror, icscode, ier1
      integer icskid, iout, imtmax, ntstart, ntetstart,
     *        ilen, ityp, lencmo, itpcmo, nsdgeomhex, length,
     *        nsdtopo, nsdgeom, nen, nef, ntet,nee,
     *        mbndry_old, mbndry_new, itp1_boundary,
     *        icmotype, nsdtopohex, nenhex, nefhex, neehex,
     *        lenimt1hex, lenitp1hex, lenicr1hex,
     *        lenxhex, lenyhex, lenzhex,
     *        lenihexclr, lenihextyp, lenihexoff, lenjhexoff,
     *        lenihex, lenjhex, itoff, jtoff, numhex1,
     *        ihexoff_save, jhexoff_save, ifelmnew,
     *        inegvol, npstart, nnodes_inc, nelements_inc,
     *        nelmadd1, nelmadd2, ntdel, ntneg, idup, nvals,
     *        lenisetwd, lenimt1, lenitp1, lenicr1, lenisn1,
     *        lenxic, lenyic, lenzic, leniign, lout, lenc,
     *        lenitetclr, lenitettyp, lenitetoff, lenjtetoff,
     *        lenitet, lenjtet, iatt, natt, len1, length1,
     *        ialiasnew, lalias8, iattlen,
     *        itetclr_min, itetclr_max, itetclr_range
 
      integer itp, icr, ign, ict, ict1, jp1, imt, imtel1,
     *        jhex, imtel2, n1, n2, n3, n4, n5, n6, jcc,
     *        index, i, j, k, l, m, jt, jf, jh, it,
     *        i1, i2, i3, j1, ih, ih1, ih2, ih3,
     *        ip1, ip2, ip3, ip4, jp2, jp3, jp4
 
      integer lalias(15)
 
      integer icharlnf
 
      real*8 rad1,rad2, voltet,voltot
      real*8 rout, distmin, xr, yr, zr
      real*8 distmax, xfacdist, xfacvol, dist,
     *       volmax, volmin, volhex,
     *       d15, d16, d24, d26, d34, d35,
     *       xfacei, yfacei, zfacei, xfacej, yfacej, zfacej
 
      real*8 xicvol(maxnen), yicvol(maxnen), zicvol(maxnen)
 
      character*32 cmohex, cmotet, mesh_type
      character*32 isubname, cmotype, cdensnm, cpresnm, cvelnm
      character*32  cuser, cout
 
C     Global options
c     changed iremove to iremove_dup irmvolume to iremove_vol
      integer iradavg, icheckpt, iremove_dup, iremove_vol
      data iradavg  / 0 /
      data icheckpt / 0 /
      data iremove_dup  / 0 /
      data iremove_vol  / 0 /
 
c     if a known fatal error, goto 8888, else ifatal becomes 0
      integer ifatal
      data ifatal / 1 /
C
C
C#######################################################################
C
C
      print*,'set idebug'
      idebug = 5

      isubname='hextotet_hybrid'
      cuser='nnodes'
      icskid=1

 
C
C     *****************************************************************
C     Globals
 
      call get_global('hextotet_radavg',iout,rout,cout,
     *                        ityp,ierror_return)
      if(cout(1:1).eq.'y') then
         iradavg=1
      elseif(cout(1:1).eq.'n') then
         iradavg=0
      else
         iradavg=0
      endif
C
      call get_global('hextotet_remove_duplicates',iout,rout,cout,
     *                        ityp,ierror_return)
      if(cout(1:1).eq.'y') then
         iremove_dup=1
      elseif(cout(1:1).eq.'n') then
         iremove_dup=0
      else
         iremove_dup=0
      endif
C
      call get_global('hextotet_check_imt',iout,rout,cout,
     *                        ityp,ierror_return)
 
      if(cout(1:1).eq.'y') then
         icheckpt=1
      elseif(cout(1:1).eq.'n') then
         icheckpt=0
      else
         icheckpt=1
      endif
C
      call get_global('hextotet_remove_volume',iout,rout,cout,
     *                        ityp,ierror_return)
      if(cout(1:1).eq.'y') then
         iremove_vol=1
      elseif(cout(1:1).eq.'n') then
         iremove_vol=0
      else
         iremove_vol=0
      endif
 
      write(wlog,'(a,i3,a,i3,a,i3)')
     * ' iremove_vol= ',iremove_vol,
     * ' iremove_dup= ',iremove_dup,
     * ' icheckpt= ',icheckpt
      call writloga('default',0,wlog,0,iwerr)
 
C     END model definitions
C     *****************************************************************
 
C     GET input mesh object definitions
C
      call cmo_set_name(cmohex,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_set_name')
 
c     Get the mesh type of the incoming cmo
      mesh_type = 'notset'
      call cmo_get_mesh_type(cmohex,mesh_type,imesh_type,ier)
      if (ier.ne.0 .or. imesh_type.le.0) then
          write(wlog,'(a,a,a,a)') 'WARNING: Undefined mesh type: ',
     >    mesh_type(1:3),' for ',cmohex
          call writloga('default',0,wlog,1,ier)
      endif
      if (ioption.eq.-2) then
        if (imesh_type.eq.ifelmtri) then
           ioption = 2
        elseif (imesh_type.eq.ifelmhex) then
           ioption = 6
        elseif (imesh_type.eq.ifelmpri) then
           ioption = 3
        elseif (imesh_type.eq.ifelmpyr) then
           ioption = 4
        else
           ioption = 24
        endif
      endif
      write(wlog,'(a,a,a,i4)')
     *'Input Mesh type ',mesh_type(1:3),' using option ',ioption
      call writloga('default',0,wlog,0,iwerr)
C
      iwcnt=0
      call cmo_get_info('idebug',cmohex,idebug,ilen,ityp,ierror)
C
      call cmo_exist(cmohex,ierror)
      if(ierror .ne. 0)then
         write(wlog,'(a)')'Input Mesh Object DOES NOT EXIST'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'NO ACTION'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'RETURN'
         call writloga('default',0,wlog,0,iwerr)
         goto 8888
      endif
      call cmo_get_info('nnodes',cmohex,npoints,lencmo,itpcmo,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info')
      call cmo_get_info('nelements',cmohex,numhex,lencmo,itpcmo,ierror)
      if((npoints .le. 0) .and. (numhex .le. 0))then
         write(wlog,'(a)')'Input Mesh Object is Empty'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'NO ACTION'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'RETURN'
         call writloga('default',0,wlog,0,iwerr)
         goto 8888
      endif
      call cmo_get_info('mbndry',cmohex,mbndry,lencmo,itpcmo,ierror)
      call cmo_get_info('ndimensions_geom',cmohex,
     *                  nsdgeomhex,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmohex,
     *                  nsdtopohex,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmohex,
     *                  nenhex,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmohex,
     *                  nefhex,length,icmotype,ierror)
      call cmo_get_info('edges_per_element',cmohex,
     *                  neehex,length,icmotype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info')
      if(ierror.ne.0) call x3d_error(isubname,'get_info first cmo')
 
      call cmo_get_info('imt1',cmohex,ipimt1hex,lenimt1hex,icmotype,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info imt1 first cmo')
      call cmo_get_info('itp1',cmohex,ipitp1hex,lenitp1hex,icmotype,ier)
      call cmo_get_info('icr1',cmohex,ipicr1hex,lenicr1hex,icmotype,ier)
      call cmo_get_info('xic',cmohex,ipxhex,lenxhex,icmotype,ierror)
      call cmo_get_info('yic',cmohex,ipyhex,lenyhex,icmotype,ierror)
      call cmo_get_info('zic',cmohex,ipzhex,lenzhex,icmotype,ierror)
 
      call cmo_get_info('itetclr',cmohex,
     *                   ipihexclr,lenihexclr,icmotype,ier)
      if(ierror.ne.0) call x3d_error(isubname,'get_info')
      call cmo_get_info('itettyp',cmohex,
     *                   ipihextyp,lenihextyp,icmotype,ier)
      call cmo_get_info('itetoff',cmohex,
     *                   ipihexoff,lenihexoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmohex,
     *                   ipjhexoff,lenjhexoff,icmotype,ier)
 
      call cmo_get_info('itet',cmohex,ipihexnn,lenihex,icmotype,ierror)
      call cmo_get_info('jtet',cmohex,ipjhexnn,lenjhex,icmotype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info')
 
C
C     ******************************************************************
C     COMPRESS OUT ANY HEXES THAT HAVE ZERO COLOR.  NOTE THAT
C        COMPRESSING THE JTET LIST MUST BE DONE VERY CAREFULLY.
C
      itoff=0
      jtoff=0
      numhex1=0
      do ih=1,numhex
         if(ihexclr(ih).gt.0) then
            numhex1=numhex1+1
            ihexclr(numhex1)=ihexclr(ih)
            ihextyp(numhex1)=ihextyp(ih)
            ihexoff_save=ihexoff(ih)
            jhexoff_save=jhexoff(ih)
            ihexoff(numhex1)=itoff
            jhexoff(numhex1)=jtoff
            do i=1,nelmnen(ihextyp(ih))
               ihexnn1(ihexoff(numhex1)+i)=ihexnn1(ihexoff_save+i)
            enddo
            do i=1,nelmnef(ihextyp(ih))
               if(jhexnn1(jhexoff_save+i).eq.mbndry) then
                  jhexnn1(jhexoff(numhex1)+i)=jhexnn1(jhexoff_save+i)
               elseif(jhexnn1(jhexoff_save+i).gt.0 .and.
     *                jhexnn1(jhexoff_save+i).lt.mbndry) then
                  jt=1+(jhexnn1(jhexoff_save+i)-1)/nefhex
                  jf=jhexnn1(jhexoff_save+i) -
     *                  nefhex*(jt-1)
                  jhexnn1(jhexoff(numhex1)+i)=
     *                          nefhex*(jt-1)+jf
                  jhexnn1(jhexoff(jt)+jf)=
     *                          nefhex*(numhex1-1)+i
               elseif(jhexnn1(jhexoff_save+i).gt.mbndry) then
                  jt=1+(jhexnn1(jhexoff_save+i)-mbndry-1)
     *                    /nefhex
                  jf=jhexnn1(jhexoff_save+i) -
     *                  nefhex*(jt-1) -
     *                     mbndry
                  jhexnn1(jhexoff(numhex1)+i)=mbndry +
     *                          nefhex*(jt-1)+jf
                  jhexnn1(jhexoff(jt)+jf)=mbndry +
     *                          nefhex*(numhex1-1)+i
               endif
            enddo
         endif
         itoff=itoff+nelmnen(ihextyp(ih))
         jtoff=jtoff+nelmnef(ihextyp(ih))
      enddo
      if(numhex1.gt.0.and.numhex1.lt.numhex) then
         write(wlog,9000) numhex,numhex1
         call writloga('default',0,wlog,0,iwerr)
 9000    format("Compressing out zero-color hexes: ",2i10)
      endif
C
C     ..................................................................
C     RESET THE NUMBER OF HEXES AND CHANGE THE CMO.
C
      numhex=numhex1
      call cmo_set_info('nelements',cmohex,numhex,1,1,ierror)
C
C
C     ******************************************************************
C     CHECK THE CONSISTENCY OF THE JTET LIST. IF WE FIND AN ERROR THE
C        WE WILL REBUILD THE JTET LIST FOR THE HEXES FROM SCRATCH.
C
      call elmtestd(cmohex,20,ierror)
      if (ierror.gt.0) then
        write(wlog,'(a)')'ERROR: Internal Inconsistency with jtet.'
        call writloga('default',0,wlog,0,iwerr)
        goto 8888
      endif
C
      if(ierror.gt.0) then
         call geniee_cmo(cmohex)
         do it=1,numhex
            do i=1,nelmnef(ihextyp(it))
               if(jhexnn1(jhexoff(it)+i).gt.0 .and.
     *            jhexnn1(jhexoff(it)+i).lt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(it)+i)-1)/nefhex
                  jf=jhexnn1(jhexoff(it)+i)-nefhex*(jt-1)
                  jhexnn1(jhexoff(it)+i )=nefhex*(jt-1)+jf
                  jhexnn1(jhexoff(jt)+jf)=nefhex*(it-1)+i
               endif
            enddo
         enddo
      endif
C
C     ******************************************************************
C     Compute the min/max/range of itetclr
C
      itetclr_max = -1000000000
      itetclr_min =  1000000000
      do it = 1, numhex
         itetclr_max = max(ihexclr(it), itetclr_max)
         itetclr_min = min(ihexclr(it), itetclr_min)
      enddo
      itetclr_range = itetclr_max - itetclr_min
      write(wlog,9008) itetclr_min, itetclr_max, itetclr_range
      call writloga('default',0,wlog,0,iwerr)
 9008 format("Element Material ID, min/max/range: ",3(1i10))
C
C     ******************************************************************
C     COPY THE COORDINATES FROM THE HEX INTO A SET OF ARRAYS THAT WILL
C        EVENTUALLY BECOME THE COORDINATES FOR THE TETS.
C
      length=npoints+numhex+nenhex*numhex
      call mmgetblk("xic2",isubname,ipxic2,length,2,icscode)
      call mmgetblk("yic2",isubname,ipyic2,length,2,icscode)
      call mmgetblk("zic2",isubname,ipzic2,length,2,icscode)
      do i=1,length
         xic2(i)=0.0d0
         yic2(i)=0.0d0
         zic2(i)=0.0d0
      enddo
      do i=1,npoints
         xic2(i)=xhex(i)
         yic2(i)=yhex(i)
         zic2(i)=zhex(i)
      enddo
C
C
C     ******************************************************************
C     DETERMINE AN EPSILON FOR DISTANCE TO BE USED THROUGHOUT THIS
C        ROUTINE.
C
      distmax=0.0
      distmin=1.0e+30
      imtmax=0
      do it=1,numhex
         do i=1,nelmnee(ihextyp(it))
            i1= ihexnn1(ihexoff(it)+ielmedge1(1,i,ihextyp(it)))
            i2= ihexnn1(ihexoff(it)+ielmedge1(2,i,ihextyp(it)))
            dist=(xic2(i1)-xic2(i2))**2 +
     *           (yic2(i1)-yic2(i2))**2 +
     *           (zic2(i1)-zic2(i2))**2
            distmax=max(distmax,dist)
            distmin=min(distmin,dist)
         enddo
      enddo
      xfacdist=1.0e-06 * sqrt(distmax)
      if (iremove_vol .ne. 1) xfacdist=-1.0*xfacdist
      write(wlog,9010) xfacdist,distmax,distmin
      call writloga('default',0,wlog,0,iwerr)
 9010 format("Epsilon-dist, distmax, distmin: ",3(1pe15.7))
C
C     ******************************************************************
C     CHECK FOR NEGATIVE VOLUME ELEMENTS (THIS IS JUST FOR INFORMATION
C        WE DON'T REMOVE THE NEGATIVE ONES FOR NOW).
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 1'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif

      inegvol=0
      numhex1=0
      volmax=-1.0d+30
      volmin=-volmax
      do it=1,numhex
         volmin=1.0d+30
         do i=1,nelmnen(ihextyp(it))
            i1=ihexnn1(ihexoff(it)+i)
            xicvol(i)=xic2(i1)
            yicvol(i)=yic2(i1)
            zicvol(i)=zic2(i1)
         enddo
         call volume_element(ihextyp(it),
     *                       xicvol,yicvol,zicvol,
     *                       volhex)
         volmax=max(volmax,volhex)
         volmin=min(volmin,volhex)
         numhex1=numhex1+1
         if(volhex.lt.-1.0e-06) then
            inegvol=inegvol+1
            if(inegvol.lt.20) then
               write(wlog,9030) inegvol,it,volhex
               call writloga('default',0,wlog,0,iwerr)
 9030          format("  Hex with negative volume: ",2i10,1pe15.7)
               write(wlog,9031)
     *            (ihexnn1(ihexoff(it)+i),i=1,nelmnen(ihextyp(it)))
               call writloga('default',0,wlog,0,iwerr)
 9031          format("  Hex indices: ",8i8)
            endif
         else
C
C           ............................................................
C           PUT CODE HERE TO COMPRESS ELEMENTS WITH NEGATIVE VOLUME.
C           ............................................................
C
         endif
      enddo
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 2'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
C
      if(numhex1.gt.0.and.numhex1.lt.numhex) then
         write(wlog,9040) numhex,numhex1
         call writloga('default',0,wlog,0,iwerr)
 9040    format("Zero-volume hexes: ",2i10)
      endif
      numhex=numhex1
C
      call cmo_set_info('nelements',cmohex,numhex,1,1,ierror)
      if(inegvol.gt.0) then
         write(wlog,9050) numhex,inegvol
         call writloga('default',0,wlog,0,iwerr)
 9050    format("Total number of negative volume hexes: ",2i10)
      endif
C
C     ..................................................................
C     DETERMINE AN EPSILON FOR VOLUME TO BE USED THROUGHOUT THIS
C        ROUTINE.
C
      xfacvol=1.0e-06 * volmax
      write(wlog,9011) xfacvol,volmax
      call writloga('default',0,wlog,0,iwerr)
 9011 format("Epsilon-volume, volmax: ",2(1pe15.7))
C
 9990 continue
C
C
C   ******************************************************************
C   COUNT THE NUMBER OF NEW NODES AND ELEMENTS THAT WE WILL BE ADDING.
C   calculate using the selected options that may or may not add points
C
 
      length=nefhex*numhex
      call mmgetblk("ktet",isubname,ipktet,length,2,icscode)
      do it=1,numhex
         do i=1,nelmnef(ihextyp(it))
            ktet(jhexoff(it)+i)=0
         enddo
      enddo
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 3'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      npstart=npoints
      if(ioption.eq.-1) then
         npoints=npoints+numhex
         nnodes_inc=numhex
         nelements_inc=0
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmtri)) then
                  nelements_inc=nelements_inc+1
               elseif(ielmface0(i,ihextyp(ih)) .eq.
     *                nelmnen(ifelmqud)) then
                  nelements_inc=nelements_inc+4
               endif
            enddo
         enddo
      elseif(ioption.eq.0) then
         npoints=npoints+numhex
         nelements_inc=0
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmtri)) then
                  nelements_inc=nelements_inc+1
               elseif(ielmface0(i,ihextyp(ih)) .eq.
     *                nelmnen(ifelmqud)) then
                  nelements_inc=nelements_inc+4
                  if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+i)-nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+i)-mbndry-nefhex*(jt-1)
                  endif
                  if(jt.eq.0.or.jt.gt.ih) then
                     npoints=npoints+1
                     ktet(jhexoff(ih)+i)=npoints
                  else
                     ktet(jhexoff(ih)+i)=ktet(jhexoff(jt)+jf)
                  endif
               endif
            enddo
         enddo
         nnodes_inc=npoints-npstart
      elseif(ioption.eq.2) then
         nnodes_inc=0
         nelements_inc=numhex
      elseif(ioption.eq.3) then
         nnodes_inc=0
         nelements_inc=3*numhex
      elseif(ioption.eq.4) then
         npoints=npoints+numhex
         nnodes_inc=numhex
         nelements_inc=4*numhex
      elseif(ioption.eq.5) then
         nnodes_inc=0
         nelements_inc=5*numhex
      elseif(ioption.eq.6) then
         nnodes_inc=0
         nelements_inc=6*numhex
      elseif(ioption.eq.7) then
         nnodes_inc=numhex
         nelements_inc=6*numhex
      elseif(ioption.eq.14) then
         npoints=npoints+numhex
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmqud)) then
                  if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                       nelmnef(ihextyp(ih))
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                     nefhex*(jt-1) -
     *                        mbndry
                  endif
                  if(jt.eq.0.or.jt.gt.ih) then
                     npoints=npoints+1
                     ktet(jhexoff(ih)+i)=npoints
                  else
                     ktet(jhexoff(ih)+i)=ktet(jhexoff(jt)+jf)
                  endif
               endif
            enddo
         enddo
         nnodes_inc=npoints-npstart
         nelements_inc=14*numhex
 
c     ioption = 18 ==> 18 TETS PER PRISM, 6 NEW POINTS (1 + 5 FACES)
      elseif(ioption.eq.18) then
         npoints=npoints+numhex
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                  jt=0
                  jf=0
               elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                  nefhex*(jt-1)
               elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                    nelmnef(ihextyp(ih))
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                  nefhex*(jt-1) -
     *                     mbndry
               endif
               if(jt.eq.0.or.jt.gt.ih) then
                  npoints=npoints+1
                  ktet(jhexoff(ih)+i)=npoints
               else
                  ktet(jhexoff(ih)+i)=ktet(jhexoff(jt)+jf)
               endif
            enddo
         enddo
         nnodes_inc=npoints-npstart
         nelements_inc=18*numhex
 
C     ioption = 24 ==> 24 TETS PER HEX, 7 NEW POINTS (1 + 6 FACES)
      elseif(ioption.eq.24) then
         npoints=npoints+numhex
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                  jt=0
                  jf=0
               elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                  nefhex*(jt-1)
               elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                    nelmnef(ihextyp(ih))
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                  nefhex*(jt-1) -
     *                     mbndry
               endif
               if(jt.eq.0.or.jt.gt.ih) then
                  npoints=npoints+1
                  ktet(jhexoff(ih)+i)=npoints
               else
                  ktet(jhexoff(ih)+i)=ktet(jhexoff(jt)+jf)
               endif
            enddo
         enddo
         nnodes_inc=npoints-npstart
         nelements_inc=24*numhex
      endif
 
C     set outgoing cmo
      call cmo_exist(cmotet,ierror)
      if(ierror.ne.0) then
         call cmo_derive(cmotet,cmohex,ierror)
C        Check Point
         if(idebug .gt. 1)then
            write(wlog,'(a)')'Check Point'
            call writloga('default',0,wlog,0,iwerr)
            call dotask('cmo/status/cmohex;finish',ierr)
            call dotask('cmo/status/cmotet;finish',ierr)
         endif
      endif
      call cmo_select(cmotet,ierror)
 
C     set outgoing element type and topo dimensions
      mesh_type = 'tet'
      if(nsdtopohex.eq.2) then
         mesh_type = 'tri'
         ifelmnew=ifelmtri
         nsdtopo=2
      elseif(nsdtopohex.eq.3) then
         nsdtopo=3
         if(ioption.eq.-1) then
            mesh_type = 'hyb'
            ifelmnew=ifelmhyb
         elseif(ioption.eq.0) then
            ifelmnew=ifelmtet
         elseif(ioption.eq.2) then
            mesh_type = 'pri'
            ifelmnew=ifelmpri
         else
            ifelmnew=ifelmtet
         endif
      endif
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 4'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      call cmo_set_mesh_type(cmotet, mesh_type, ierr)
      if(ierr .ne. 0)call x3d_error(isubname, 'cmo set mesh_type')
      nsdgeom=nsdgeomhex
      nen=nelmnen(ifelmnew)
      nef=nelmnef(ifelmnew)
      nee=nelmnee(ifelmnew)
      nnodes=npoints
      numtet=0
      nelements=numhex+nelements_inc
c
      call cmo_set_info('nnodes',cmotet,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmotet,nelements,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmotet,nsdgeom,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmotet,nsdtopo,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmotet,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmotet,nef,1,1,ierror)
      call cmo_set_info('edges_per_element',cmotet,nee,1,1,ierror)
      call cmo_newlen(cmotet,ierror)
C     Check Point
      if(idebug .gt. 1)then
         write(wlog,'(a)')'Check Point'
         call writloga('default',0,wlog,0,iwerr)
         call dotask('cmo/status/cmohex;finish',ierr)
         call dotask('cmo/status/cmotet;finish',ierr)
      endif
 
c     get pointers to the attributes for the new cmotet
c
      call cmo_get_intinfo('mbndry',cmotet,mbndry,length,icmotype,ier)
c
      call cmo_get_info('isetwd',cmotet,ipisetwd,lenisetwd,icmotype,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info tet cmo')
      call cmo_get_info('imt1',cmotet,ipimt1,lenimt1,icmotype,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info second cmo')

      call cmo_get_info('itp1',cmotet,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmotet,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmotet,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmotet,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmotet,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmotet,ipzic,lenzic,icmotype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info second cmo')
 
      call cmo_get_attinfo('velname',cmotet,iout,rout,cvelnm,
     *                        iout,lout,ityp,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_attinfo tet cmo')
 
      call cmo_get_info(cvelnm,cmotet,ipvel,lenc,icmotype,ierror)
      if(ierror.eq.0) then
         ifvels=.true.
      else
         ifvels=.false.
      endif
      call cmo_get_attinfo('densname',cmotet,iout,rout,cdensnm,
     *                        iout,lout,ityp,ierror_return)
      call cmo_get_info(cdensnm,cmotet,ipdens,lenc,icmotype,ierror)
      if(ierror.eq.0) then
         ifdens=.true.
      else
         ifdens=.false.
      endif
      call cmo_get_attinfo('presname',cmotet,iout,rout,cpresnm,
     *                        iout,lout,ityp,ierror_return)
      call cmo_get_info(cpresnm,cmotet,ippres,lenc,icmotype,ierror)
      if(ierror.eq.0) then
         ifpres=.true.
      else
         ifpres=.false.
      endif
      call cmo_get_info('itetclr',cmotet,
     *                   ipitetclr,lenitetclr,icmotype,ier)
      if(ier.ne.0) call x3d_error(isubname,'get_info tet cmo')
      call cmo_get_info('itettyp',cmotet,
     *                   ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmotet,
     *                   ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmotet,
     *                   ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmotet,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmotet,ipjtet,lenjtet,icmotype,ierror)
C
C     look for attributes derived from cmohex
C     copy the values from cmohex into the new cmo cmotet
C     assumes there will be no more than 100 attributes
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 4'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      length=100
      call mmgetblk("iattyp",isubname,ipiattyp,length,1,ier)
      call mmgetblk("iattrank",isubname,ipiattrank,length,1,ier)
      call mmgetblk("cattlen",isubname,ipcattlen,length*4,2,ier)
      call mmgetblk("cattinterp",isubname,ipcattinterp,length*4,2,ier)
      call mmgetblk("cattname",isubname,ipcattname,length*4,2,ier)
 
      call cmo_getattributes(cmotet,natt,cattlen,iattyp,iattrank,
     *          cattinterp,cattname,ier)
C
C     Note: numhex = number of elements in the source mesh (may not be hex)
C           numtet = number of elements in the sink   mesh (may not be tet)
C           npoints = number of nodes in the source mesh
C           nnodes  = number of nodes in the sink   mesh at this time. That may change later.
C
      if(natt.gt.0) then
      if(natt .gt. 100)then
         write(wlog,'(a)')'ERROR: Hextotet set for up to 100 attributes'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'ERROR: More than 100 attributes found'
         call writloga('default',0,wlog,0,iwerr)
         write(wlog,'(a)')'ERROR: STOP'
         call writloga('default',0,wlog,0,iwerr)
         stop
      endif
         do iatt=1,natt
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 5'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
            len1=icharlnf(cattlen(iatt))
 
            if(cattlen(iatt)(1:len1).eq.'nnodes') then
               if(iattyp(iatt).ne.0) then
                  call cmo_get_info(cattname(iatt),cmotet,ipatt,
     *                              lenitet,icmotype,ier)
                  call cmo_get_info(cattname(iatt),cmohex,ipatthex,
     *                              lenihex,icmotype,ier1)
                  if(ier.ne.0 .or. ier1.ne.0) then
                    write(wlog,'(a,a)')'get_info failed for ',cattname
     *                       (iatt)
                    call writloga('default',0,wlog,0,iwerr)
                    goto 8888
                  endif
                  if(idebug .gt. 1)then
                     write(wlog,'(a,i12,i12,i10,i10)')
     *                 cattname(iatt),ipatt,ipatthex,lenitet,lenihex
                     call writloga('default',0,wlog,0,iwerr)
                  endif
                  do j=1,lenihex
                    if(iattyp(iatt).eq.1) then
                       iiatt(j)=iatthex(j)
                     else
                        xatt(j)=xatthex(j)
                     endif
                  enddo
               endif
            elseif(cattlen(iatt)(1:len1).eq.'nelements') then
               if(iattyp(iatt).ne.0) then
                  call cmo_get_info(cattname(iatt),cmotet,ipatt,
     *                              lenitet,icmotype,ier)
                  call cmo_get_info(cattname(iatt),cmohex,ipatthex,
     *                              lenihex,icmotype,ier1)
                  if(ier.ne.0 .or. ier1.ne.0) then
                    write(wlog,'(a,a)')'get_info failed for ',cattname
     *                  (iatt)
                    call writloga('default',0,wlog,0,iwerr)
                    goto 8888
                  endif
                  if(idebug .gt. 1)then
                     write(wlog,'(a,i12,i12,i10,i10)')
     *                 cattname(iatt),ipatt,ipatthex,lenitet,lenihex
                     call writloga('default',0,wlog,0,iwerr)
                  endif
                  do j=1,lenihex
                    if(iattyp(iatt).eq.1) then
                       iiatt(j)=iatthex(j)
                     else
                        xatt(j)=xatthex(j)
                     endif
                  enddo
              endif
            endif
 
         enddo
      endif
 
c     Add new user attributes to cmotet
c     For the prism to 3 tet option create an array that will keep
c     track of which which of 3 tets each new tet is. This is
c     used in the anisotropic case where we need to know which face
c     of a tet the principle axis of anisotropy is parallel to.
c
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 6'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      if(ioption.eq.3) then
        cbuf=  'cmo/addatt/' // cmotet(1:icharlnf(cmotet)) //
     >  '/itetnorm/vint/scalar/nelements/-def-/-def-/gx/-def-' //
     >  ' ; finish '
        call dotaskx3d(cbuf,ier)
 
        call cmo_get_info
     >   ('itetnorm',cmotet,ipitetnorm,length,icmotype,ier)
          if (ierror.ne.0 .or. length.eq.-1)then
          call x3d_error(isubname,'addatt itetnorm')
          goto 8888
        endif
      endif
 
c     iign takes the place of old cmo attribute nsg1
c     may be ok to remove all references from the code
c     call cmo_get_info('iign',cmotet,ipiign,leniign,icmotype,ierror)
      cbuf=  'cmo/addatt/'//cmotet(1:icharlnf(cmotet)) //
     >  '/iign/vint/scalar/nnodes/-def-/-def-/gx/-def-' //
     >  ' ; finish '
      call dotaskx3d(cbuf,ier)
      call cmo_get_info('iign',cmotet,ipiign,leniign,icmotype,ierror)
      if (ierror.ne.0 .or. leniign.eq.-1)then
        call x3d_error(isubname,'addatt iign')
        goto 8888
      endif
 
c assign values from cmohex to cmotet
      do i1=1,npoints
         imt1(i1)=imt1hex(i1)
         itp1(i1)=itp1hex(i1)
         icr1(i1)=icr1hex(i1)
         xic(i1)=xhex(i1)
         yic(i1)=yhex(i1)
         zic(i1)=zhex(i1)
      enddo
C
      do i=npstart+1,npoints
         imt1(i)=0
         itp1(i)=0
         icr1(i)=0
         isn1(i)=0
         iign(i)=0
         xic(i)=0.0
         yic(i)=0.0
         zic(i)=0.0
      enddo
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 7'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      itoff=0
      jtoff=0
      do it=numtet+1,nelements
         itettyp(it)=ifelmnew
         itetclr(it)=0
         itetoff(it)=itoff
         jtetoff(it)=jtoff
         itoff=itoff+nelmnen(itettyp(it))
         jtoff=jtoff+nelmnef(itettyp(it))
         do i=1,nelmnen(itettyp(it))
            itet1(itetoff(it)+i)=0
         enddo
         do i=1,nelmnef(itettyp(it))
            jtet1(jtetoff(it)+i)=-1
         enddo
      enddo
C
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 8'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      ntetstart=numtet+1
      ntet=numtet
      itoff=0
      jtoff=0
      do ih1=1,numhex
         ntstart=ntet
         do i=1,nelmnen(ihextyp(ih1))
            lalias(i)=ihexnn1(ihexoff(ih1)+i)
         enddo
         if(ioption.eq.-1.and.nsdtopohex.eq.3) then
            ialiasnew=15
            lalias(15)=npstart+ih1
            imt1(lalias(15))=0
            itp1(lalias(15))=ifitpdud
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ihextyp(ih1).eq.ifelmtri) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
            elseif(ihextyp(ih1).eq.ifelmtet) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
                  itet1(itetoff(ntet)+4)=4
            else
               do i=1,nelmnef(ihextyp(ih1))
                  ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  if(ielmface0(i,ihextyp(ih1)).eq.ifelmtri) then
                     itettyp(ntet)=ifelmtet
                  elseif(ielmface0(i,ihextyp(ih1)).eq.ifelmqud) then
                     itettyp(ntet)=ifelmpyr
                  endif
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  if(ielmface0(i,ihextyp(ih1)).eq.ifelmtri) then
                     itet1(itetoff(ntet)+1)=15
                     itet1(itetoff(ntet)+2)=ielmface1(1,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+3)=ielmface1(2,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+4)=ielmface1(3,i,ihextyp(ih1))
                  elseif(ielmface0(i,ihextyp(ih1)).eq.ifelmqud) then
                     itet1(itetoff(ntet)+5)=15
                     itet1(itetoff(ntet)+1)=ielmface1(1,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+2)=ielmface1(4,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+3)=ielmface1(3,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+4)=ielmface1(2,i,ihextyp(ih1))
                  endif
               enddo
               imt1(lalias(ialiasnew))=0
               itp1(lalias(ialiasnew))=0
               icr1(lalias(ialiasnew))=0
               iign(lalias(ialiasnew))=0
               xic2(lalias(ialiasnew))=0.0
               yic2(lalias(ialiasnew))=0.0
               zic2(lalias(ialiasnew))=0.0
               itp=0
               icr=9999999
               ign=0
               ict=0
               do i=1,nelmnen(ihextyp(ih1))
                  i1=ihexnn1(ihexoff(ih1)+i)
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  ign=max(ign,iign(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(ialiasnew))=xic2(lalias(ialiasnew))+xr
                  yic2(lalias(ialiasnew))=yic2(lalias(ialiasnew))+yr
                  zic2(lalias(ialiasnew))=zic2(lalias(ialiasnew))+zr
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               imt1(lalias(ialiasnew))=ihexclr(ih1)
               itp1(lalias(ialiasnew))=itp
               icr1(lalias(ialiasnew))=icr
               iign(lalias(ialiasnew))=1+ign
               xic2(lalias(ialiasnew))=xic2(lalias(ialiasnew)) /
     *                                 nelmnen(ihextyp(ih1))
               yic2(lalias(ialiasnew))=yic2(lalias(ialiasnew)) /
     *                                 nelmnen(ihextyp(ih1))
               zic2(lalias(ialiasnew))=zic2(lalias(ialiasnew)) /
     *                                 nelmnen(ihextyp(ih1))
               rad1=rad1/dble(nelmnen(ihextyp(ih1)))
               rad2=sqrt(xic2(lalias(ialiasnew))**2+
     *                   yic2(lalias(ialiasnew))**2+
     *                   zic2(lalias(ialiasnew))**2)
               if(iradavg.eq.1) then
                  xic2(lalias(ialiasnew))=xic2(lalias(ialiasnew))*rad1 /
     *                                    rad2
                  yic2(lalias(ialiasnew))=yic2(lalias(ialiasnew))*rad1 /
     *                                    rad2
                  zic2(lalias(ialiasnew))=zic2(lalias(ialiasnew))*rad1 /
     *                                    rad2
               endif
            endif
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
         elseif(ioption.eq.0.and.nsdtopohex.eq.3) then
            lalias(15)=npstart+ih1
            imt1(lalias(15))=0
            itp1(lalias(15))=ifitpdud
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ihextyp(ih1).eq.ifelmtri) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
            elseif(ihextyp(ih1).eq.ifelmqud) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=3
                  itet1(itetoff(ntet)+3)=4
            elseif(ihextyp(ih1).eq.ifelmtet) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
                  itet1(itetoff(ntet)+4)=4
            else
               icount=0
               do i=1,nelmnef(ihextyp(ih1))
                  if(ielmface0(i,ihextyp(ih1)) .eq.
     *               nelmnen(ifelmqud)) then
                     icount=icount+1
                     lalias(icount+nelmnen(ihextyp(ih1)))=
     *                  ktet(jhexoff(ih1)+i)
                     do j=1,ielmface0(i,ihextyp(ih1))
                        jp1=j+1
                        if(jp1.gt.ielmface0(i,ihextyp(ih1))) jp1=1
                        ntet=ntet+1
                        itetclr(ntet)=ihexclr(ih1)
                        itettyp(ntet)=ifelmtet
                        itetoff(ntet)=itoff
                        jtetoff(ntet)=jtoff
                        itoff=itoff+nelmnen(itettyp(ntet))
                        jtoff=jtoff+nelmnef(itettyp(ntet))
                        itet1(itetoff(ntet)+1)=15
                        itet1(itetoff(ntet)+2)=
     *                     ielmface1(j,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+3)=
     *                     ielmface1(jp1,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+4)=icount+
     *                                         nelmnen(ihextyp(ih1))
                     enddo
                  elseif(ielmface0(i,ihextyp(ih1)).eq.
     *                   nelmnen(ifelmtri)) then
                        ntet=ntet+1
                        itetclr(ntet)=ihexclr(ih1)
                        itettyp(ntet)=ifelmtet
                        itetoff(ntet)=itoff
                        jtetoff(ntet)=jtoff
                        itoff=itoff+nelmnen(itettyp(ntet))
                        jtoff=jtoff+nelmnef(itettyp(ntet))
                        itet1(itetoff(ntet)+1)=15
                        itet1(itetoff(ntet)+2)=
     *                                      ielmface1(1,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+3)=
     *                                      ielmface1(2,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+4)=
     *                                      ielmface1(3,i,ihextyp(ih1))
                  endif
               enddo
               imt1(lalias(15))=0
               itp1(lalias(15))=0
               icr1(lalias(15))=0
               iign(lalias(15))=0
               xic2(lalias(15))=0.0
               yic2(lalias(15))=0.0
               zic2(lalias(15))=0.0
               itp=0
               icr=9999999
               ign=0
               ict=0
               do i=1,nelmnen(ihextyp(ih1))
                  i1=ihexnn1(ihexoff(ih1)+i)
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  ign=max(ign,iign(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               imt1(lalias(15))=ihexclr(ih1)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/nelmnen(ihextyp(ih1))
               yic2(lalias(15))=yic2(lalias(15))/nelmnen(ihextyp(ih1))
               zic2(lalias(15))=zic2(lalias(15))/nelmnen(ihextyp(ih1))
               rad1=rad1/dble(nelmnen(ihextyp(ih1)))
               rad2=sqrt(xic2(lalias(15))**2+
     *                   yic2(lalias(15))**2+
     *                   zic2(lalias(15))**2)
               if(iradavg.eq.1) then
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               icount=0
               do i=1,nelmnef(ihextyp(ih1))
                  if(ielmface0(i,ihextyp(ih1)) .eq.
     *               nelmnen(ifelmqud)) then
                     icount=icount+1
                     lalias8=lalias(icount+nelmnen(ihextyp(ih1)))
                     ict=0
                     imt1(lalias8)=imt1(ihexnn1(ihexoff(ih1)+
     *                             ielmface1(1,i,ihextyp(ih1))))
                     itp1(lalias8)=0
                     icr1(lalias8)=0
                     iign(lalias8)=0
                     xic2(lalias8)=0.0
                     yic2(lalias8)=0.0
                     zic2(lalias8)=0.0
                     if(ifvels) then
                        vels(1,lalias8)=0.0
                        vels(2,lalias8)=0.0
                        vels(3,lalias8)=0.0
                     endif
                     if(ifdens) then
                        dens(lalias8)=0.0
                     endif
                     if(ifpres) then
                        pres(lalias8)=0.0
                     endif
                     rad1=0.0
                     itp=0
                     icr=999999
                     ign=0
                     do j=1,ielmface0(i,ihextyp(ih1))
                        i1=ihexnn1(ihexoff(ih1)+
     *                             ielmface1(j,i,ihextyp(ih1)))
                        imt=imt1(i1)
                        if(imt1(lalias8).eq.imt) ict=ict+1
                        itp=max(itp,itp1(i1))
                        icr=min(icr,icr1(i1))
                        ign=max(ign,iign(i1))
                        xr=xic2(i1)
                        yr=yic2(i1)
                        zr=zic2(i1)
                        xic2(lalias8)=xic2(lalias8)+xr
                        yic2(lalias8)=yic2(lalias8)+yr
                        zic2(lalias8)=zic2(lalias8)+zr
                        rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                     enddo
                     itp1(lalias8)=itp
                     icr1(lalias8)=icr
                     iign(lalias8)=1+ign
                     if(ict.ne.ielmface0(i,ihextyp(ih1))) then
                        if(jhexnn1(jhexoff(ih1)+i).eq.mbndry) then
                           jt=0
                           jf=0
                        elseif(jhexnn1(jhexoff(ih1)+i).gt.0 .and.
     *                         jhexnn1(jhexoff(ih1)+i).lt.mbndry) then
                           jt=1+(jhexnn1(jhexoff(ih1)+i)-1)/
     *                           nefhex
                           jf=jhexnn1(jhexoff(ih1)+i) -
     *                           nefhex*(jt-1)
                        elseif(jhexnn1(jhexoff(ih1)+i).gt.mbndry) then
                           jt=1+(jhexnn1(jhexoff(ih1)+i)-mbndry-1)/
     *                             nefhex
                           jf=jhexnn1(jhexoff(ih1)+i) -
     *                           nefhex*(jt-1) -
     *                              mbndry
                        endif
                        imtel1=ihexclr(ih1)
                        if(jt.eq.0) then
                           imt1(lalias8)=imtel1
                        else
                           jhex=jt
                           if(jhex.gt.0.and.jhex.le.numhex) then
                              imtel2=ihexclr(jt)
                           else
                              imtel2=imtel1
                           endif
                           if(imtel1.eq.imtel2) then
                              imt1(lalias8)=imtel1
                           else
                              if(iwcnt.le.20 .or. idebug.gt.0) then
                                write(wlog,9080) ih1,i,lalias8
                                call writloga(
     *                             'default',0,wlog,0,iwerr)
                              endif
                              iwcnt=iwcnt+1
                              imt1(lalias8)=imtel1
                          endif
                        endif
                     endif
                     xic2(lalias8)=xic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     yic2(lalias8)=yic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     zic2(lalias8)=zic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     rad1=rad1/ielmface0(i,ihextyp(ih1))
                     rad2=sqrt(xic2(lalias8)**2+
     *                         yic2(lalias8)**2+
     *                         zic2(lalias8)**2)
                     if(iradavg.eq.1) then
                        xic2(lalias8)=xic2(lalias8)*rad1/rad2
                        yic2(lalias8)=yic2(lalias8)*rad1/rad2
                        zic2(lalias8)=zic2(lalias8)*rad1/rad2
                     endif
                  endif
               enddo
            endif
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
         elseif(ioption.eq.2) then
            if(ihextyp(ih1).eq.ifelmqud) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=3
                  itet1(itetoff(ntet)+3)=4
            elseif(ihextyp(ih1).eq.ifelmtri) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=3
            elseif(ihextyp(ih1).eq.ifelmhex) then
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmpri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=1
                  itet1(itetoff(ntet)+2)=2
                  itet1(itetoff(ntet)+3)=4
                  itet1(itetoff(ntet)+4)=5
                  itet1(itetoff(ntet)+5)=6
                  itet1(itetoff(ntet)+6)=8
               ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmpri
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=3
                  itet1(itetoff(ntet)+2)=4
                  itet1(itetoff(ntet)+3)=2
                  itet1(itetoff(ntet)+4)=7
                  itet1(itetoff(ntet)+5)=8
                  itet1(itetoff(ntet)+6)=6
            endif
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
 
C        ioption =  3 ==>  3 TETS PER PRISM, NO NEW POINTS
         elseif(ioption.eq.3) then
c            do i=1,3
c               ntet=ntet+1
c               itetclr(ntet)=ihexclr(ih1)
c               itettyp(ntet)=ifelmtet
c               itetoff(ntet)=itoff
c               jtetoff(ntet)=jtoff
c               itoff=itoff+nelmnen(itettyp(ntet))
c               jtoff=jtoff+nelmnef(itettyp(ntet))
c               itet1(itetoff(ntet)+1)=
c     *                          lalias(ipri3tet(1,i))
c               itet1(itetoff(ntet)+2)=
c     *                          lalias(ipri3tet(2,i))
c               itet1(itetoff(ntet)+3)=
c     *                          lalias(ipri3tet(3,i))
c               itet1(itetoff(ntet)+4)=
c     *                          lalias(ipri3tet(4,i))
c               do j=1,nelmnef(itettyp(ntet))
c                  jtet1(jtetoff(ntet)+j)=-1
c               enddo
c            enddo
             n1=lalias(1)
             n2=lalias(2)
             n3=lalias(3)
             n4=lalias(4)
             n5=lalias(5)
             n6=lalias(6)
             d15=(xic2(n1)-xic2(n5))**2
     *          +(yic2(n1)-yic2(n5))**2
     *          +(zic2(n1)-zic2(n5))**2
             d16=(xic2(n1)-xic2(n6))**2
     *          +(yic2(n1)-yic2(n6))**2
     *          +(zic2(n1)-zic2(n6))**2
             d24=(xic2(n2)-xic2(n4))**2
     *          +(yic2(n2)-yic2(n4))**2
     *          +(zic2(n2)-zic2(n4))**2
             d26=(xic2(n2)-xic2(n6))**2
     *          +(yic2(n2)-yic2(n6))**2
     *          +(zic2(n2)-zic2(n6))**2
             d34=(xic2(n3)-xic2(n4))**2
     *          +(yic2(n3)-yic2(n4))**2
     *          +(zic2(n3)-zic2(n4))**2
             d35=(xic2(n3)-xic2(n5))**2
     *          +(yic2(n3)-yic2(n5))**2
     *          +(zic2(n3)-zic2(n5))**2
             if(d15.lt.d24.or.(d15.eq.d24.and.n1.gt.n2))then
               if(d26.lt.d35.or.(d26.eq.d35.and.n2.gt.n3))then
                 jcc=1
               else
                 if(d34.lt.d16.or.(d34.eq.d16.and.n3.gt.n1))then
                   jcc=2
                 else
                   jcc=3
                 endif
               endif
             else
               if(d26.lt.d35.or.(d26.eq.d35.and.n2.gt.n3))then
                 if(d34.lt.d16.or.(d34.eq.d16.and.n3.gt.n1))then
                   jcc=4
                 else
                   jcc=5
                 endif
               else
                 jcc=6
               endif
             endif
           do i=1,3
             ntet=ntet+1
             itetnorm(ntet) = i
             itetclr(ntet)=ihexclr(ih1)
             itettyp(ntet)=ifelmtet
             itetoff(ntet)=itoff
             jtetoff(ntet)=jtoff
             itoff=itoff+nelmnen(itettyp(ntet))
             jtoff=jtoff+nelmnef(itettyp(ntet))
               itet1(itetoff(ntet)+1)=
     *                          lalias(ipri3tet(1,i,jcc))
               itet1(itetoff(ntet)+2)=
     *                          lalias(ipri3tet(2,i,jcc))
               itet1(itetoff(ntet)+3)=
     *                          lalias(ipri3tet(3,i,jcc))
               itet1(itetoff(ntet)+4)=
     *                          lalias(ipri3tet(4,i,jcc))
               do j=1,nelmnef(itettyp(ntet))
                  jtet1(jtetoff(ntet)+j)=-1
               enddo
           enddo
 
C        ioption =  4 ==>  4 TRIANGLES PER QUAD, 1 NEW POINT PER QUAD
         elseif(ioption.eq.4) then
            if(nsdtopohex.eq.2) then
               if(ihextyp(ih1).eq.ifelmqud) then
                  lalias(15)=npstart+ih1
                  ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtri
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=15
                     itet1(itetoff(ntet)+2)=1
                     itet1(itetoff(ntet)+3)=2
                  ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtri
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=15
                     itet1(itetoff(ntet)+2)=2
                     itet1(itetoff(ntet)+3)=3
                  ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtri
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=15
                     itet1(itetoff(ntet)+2)=3
                     itet1(itetoff(ntet)+3)=4
                  ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtri
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=15
                     itet1(itetoff(ntet)+2)=4
                     itet1(itetoff(ntet)+3)=1
                  imt1(lalias(15))=0
                  itp1(lalias(15))=0
                  icr1(lalias(15))=0
                  iign(lalias(15))=0
                  xic2(lalias(15))=0.0
                  yic2(lalias(15))=0.0
                  zic2(lalias(15))=0.0
                  itp=0
                  icr=9999999
                  ign=0
                  ict=0
                  do i=1,nelmnen(ihextyp(ih1))
                     i1=ihexnn1(ihexoff(ih1)+i)
                     itp=max(itp,itp1(i1))
                     icr=min(icr,icr1(i1))
                     ign=max(ign,iign(i1))
                     xr=xic2(i1)
                     yr=yic2(i1)
                     zr=zic2(i1)
                     xic2(lalias(15))=xic2(lalias(15))+xr
                     yic2(lalias(15))=yic2(lalias(15))+yr
                     zic2(lalias(15))=zic2(lalias(15))+zr
                     rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  enddo
                  imt1(lalias(15))=ihexclr(ih1)
                  itp1(lalias(15))=itp
                  icr1(lalias(15))=icr
                  iign(lalias(15))=1+ign
                  xic2(lalias(15))=xic2(lalias(15)) /
     *                             nelmnen(ihextyp(ih1))
                  yic2(lalias(15))=yic2(lalias(15)) /
     *                             nelmnen(ihextyp(ih1))
                  zic2(lalias(15))=zic2(lalias(15)) /
     *                             nelmnen(ihextyp(ih1))
                  rad1=rad1/dble(nelmnen(ihextyp(ih1)))
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  if(iradavg.eq.1) then
                     xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                     yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                     zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
                  endif
               elseif(ihextyp(ih1).eq.ifelmtri) then
                  ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtri
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=1
                     itet1(itetoff(ntet)+2)=2
                     itet1(itetoff(ntet)+3)=3
               endif
               do it=ntstart+1,ntet
                  do i=1,nelmnen(itettyp(it))
                     itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
                  enddo
               enddo
            elseif(nsdtopohex.eq.3) then
               lalias(7)=npstart+ih1
               icount=0
               do i=1,nelmnef(ihextyp(ih1))
                  if(ielmface0(i,ihextyp(ih1)) .eq.
     *               nelmnen(ifelmqud)) then
                     icount=icount+1
                     lalias(icount+nelmnen(ihextyp(ih1)))=
     *                  ktet(jhexoff(ih1)+i)
                     do j=1,ielmface0(i,ihextyp(ih1))
                        jp1=j+1
                        if(jp1.gt.ielmface0(i,ihextyp(ih1))) jp1=1
                        ntet=ntet+1
                        itetclr(ntet)=ihexclr(ih1)
                        itettyp(ntet)=ifelmtet
                        itetoff(ntet)=itoff
                        jtetoff(ntet)=jtoff
                        itoff=itoff+nelmnen(itettyp(ntet))
                        jtoff=jtoff+nelmnef(itettyp(ntet))
                        itet1(itetoff(ntet)+1)=7
                        itet1(itetoff(ntet)+2)=
     *                     ielmface1(j,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+3)=
     *                     ielmface1(jp1,i,ihextyp(ih1))
                        itet1(itetoff(ntet)+4)=i+nelmnen(ihextyp(ih1))
                     enddo
                  elseif(ielmface0(i,ihextyp(ih1)).eq.
     *                   nelmnen(ifelmtri)) then
                        ntet=ntet+1
                        itetclr(ntet)=ihexclr(ih1)
                        itettyp(ntet)=ifelmtet
                        itetoff(ntet)=itoff
                        jtetoff(ntet)=jtoff
                        itoff=itoff+nelmnen(itettyp(ntet))
                        jtoff=jtoff+nelmnef(itettyp(ntet))
                        itet1(itetoff(ntet)+1)=7
                        itet1(itetoff(ntet)+2)=itet1(itetoff(ih1)+
     *                                      ielmface1(1,i,ihextyp(ih1)))
                        itet1(itetoff(ntet)+3)=itet1(itetoff(ih1)+
     *                                      ielmface1(2,i,ihextyp(ih1)))
                        itet1(itetoff(ntet)+4)=itet1(itetoff(ih1)+
     *                                      ielmface1(3,i,ihextyp(ih1)))
                  endif
               enddo
               imt1(lalias(7))=0
               itp1(lalias(7))=0
               icr1(lalias(7))=0
               iign(lalias(7))=0
               xic2(lalias(7))=0.0
               yic2(lalias(7))=0.0
               zic2(lalias(7))=0.0
               itp=0
               icr=9999999
               ign=0
               ict=0
               do i=1,nelmnen(ihextyp(ih1))
                  i1=ihexnn1(ihexoff(ih1)+i)
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  ign=max(ign,iign(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(7))=xic2(lalias(7))+xr
                  yic2(lalias(7))=yic2(lalias(7))+yr
                  zic2(lalias(7))=zic2(lalias(7))+zr
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               imt1(lalias(7))=ihexclr(ih1)
               itp1(lalias(7))=itp
               icr1(lalias(7))=icr
               iign(lalias(7))=1+ign
               xic2(lalias(7))=xic2(lalias(7))/nelmnen(ihextyp(ih1))
               yic2(lalias(7))=yic2(lalias(7))/nelmnen(ihextyp(ih1))
               zic2(lalias(7))=zic2(lalias(7))/nelmnen(ihextyp(ih1))
               rad1=rad1/dble(nelmnen(ihextyp(ih1)))
               rad2=sqrt(xic2(lalias(7))**2+
     *                   yic2(lalias(7))**2+
     *                   zic2(lalias(7))**2)
               if(iradavg.eq.1) then
                  xic2(lalias(7))=xic2(lalias(7))*rad1/rad2
                  yic2(lalias(7))=yic2(lalias(7))*rad1/rad2
                  zic2(lalias(7))=zic2(lalias(7))*rad1/rad2
               endif
               icount=0
               do i=1,nelmnef(ihextyp(ih1))
                  if(ielmface0(i,ihextyp(ih1)) .eq.
     *               nelmnen(ifelmqud)) then
                     icount=icount+1
                     lalias8=lalias(icount+nelmnen(ihextyp(ih1)))
                     ict=0
                     imt1(lalias8)=imt1(
     *                              ihexnn1(
     *                                 ihexoff(ih1)+
     *                                 ielmface1(1,i,ihextyp(ih1))))
                     itp1(lalias8)=0
                     icr1(lalias8)=0
                     iign(lalias8)=0
                     xic2(lalias8)=0.0
                     yic2(lalias8)=0.0
                     zic2(lalias8)=0.0
                     if(ifvels) then
                        vels(1,lalias8)=0.0
                        vels(2,lalias8)=0.0
                        vels(3,lalias8)=0.0
                     endif
                     if(ifdens) then
                        dens(lalias8)=0.0
                     endif
                     if(ifpres) then
                        pres(lalias8)=0.0
                     endif
                     rad1=0.0
                     itp=0
                     icr=999999
                     ign=0
                     do j=1,ielmface0(i,ihextyp(ih1))
                        i1=ihexnn1(ihexoff(ih1)+
     *                             ielmface1(j,i,ihextyp(ih1)))
                        imt=imt1(i1)
                        if(imt1(lalias8).eq.imt) ict=ict+1
                        itp=max(itp,itp1(i1))
                        icr=min(icr,icr1(i1))
                        ign=max(ign,iign(i1))
                        xr=xic2(i1)
                        yr=yic2(i1)
                        zr=zic2(i1)
                        xic2(lalias8)=xic2(lalias8)+xr
                        yic2(lalias8)=yic2(lalias8)+yr
                        zic2(lalias8)=zic2(lalias8)+zr
                        rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                     enddo
                     itp1(lalias8)=itp
                     icr1(lalias8)=icr
                     iign(lalias8)=1+ign
                     if(ict.ne.ielmface0(i,ihextyp(ih1))) then
                        if(jhexnn1(jhexoff(ih1)+i).eq.mbndry) then
                           jt=0
                           jf=0
                        elseif(jhexnn1(jhexoff(ih1)+i).gt.0 .and.
     *                         jhexnn1(jhexoff(ih1)+i).lt.mbndry) then
                           jt=1+(jhexnn1(jhexoff(ih1)+i)-1)/
     *                           nefhex
                           jf=jhexnn1(jhexoff(ih1)+i) -
     *                           nefhex*(jt-1)
                        elseif(jhexnn1(jhexoff(ih1)+i).gt.mbndry) then
                           jt=1+(jhexnn1(jhexoff(ih1)+i)-mbndry-1)/
     *                             nefhex
                           jf=jhexnn1(jhexoff(ih1)+i) -
     *                           nefhex*(jt-1) -
     *                              mbndry
                        endif
                        imtel1=ihexclr(ih1)
                        if(jt.eq.0) then
                           imt1(lalias8)=imtel1
                        else
                           jhex=jt
                           if(jhex.gt.0.and.jhex.le.numhex) then
                              imtel2=ihexclr(jt)
                           else
                              imtel2=imtel1
                           endif
                           if(imtel1.eq.imtel2) then
                              imt1(lalias8)=imtel1
                           else
                              if(iwcnt.le.20 .or. idebug.gt.0) then
                                write(wlog,9080) ih1,i,lalias8
                                call writloga(
     *                             'default',0,wlog,0,iwerr)
 9080                           format("Error in assigning node color:",
     *                         " element=",i10," face=",i3," node=",i10)
                              endif
                              iwcnt=iwcnt+1
                              imt1(lalias8)=imtel1
                          endif
                        endif
                     endif
                     xic2(lalias8)=xic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     yic2(lalias8)=yic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     zic2(lalias8)=zic2(lalias8) /
     *                             ielmface0(i,ihextyp(ih1))
                     rad1=rad1/ielmface0(i,ihextyp(ih1))
                     rad2=sqrt(xic2(lalias8)**2+
     *                         yic2(lalias8)**2+
     *                         zic2(lalias8)**2)
                     if(iradavg.eq.1) then
                        xic2(lalias8)=xic2(lalias8)*rad1/rad2
                        yic2(lalias8)=yic2(lalias8)*rad1/rad2
                        zic2(lalias8)=zic2(lalias8)*rad1/rad2
                     endif
                  endif
               enddo
               do it=ntstart+1,ntet
                  do i=1,nelmnen(itettyp(it))
                     itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
                  enddo
               enddo
            endif
 
C        ioption =  5 ==>  5 TETS PER HEX, NO NEW POINTS
         elseif(ioption.eq.5) then
            if(ih1.eq.1) then
               do ih2=1,numhex
                  do i=1,nelmnef(itettyp(ih2))
                     if(jhexnn1(jhexoff(ih2)+i).eq.mbndry) then
                        jh=0
                        jf=0
                     elseif(jhexnn1(jhexoff(ih2)+i).gt.0 .and.
     *                      jhexnn1(jhexoff(ih2)+i).lt.mbndry) then
                        jh=1+(jhexnn1(jhexoff(ih2)+i)-1)/
     *                           nefhex
                        jf=jhexnn1(jhexoff(ih2)+i) -
     *                        nefhex*(jh-1)
                     elseif(jhexnn1(jhexoff(ih2)+i).gt.mbndry) then
                        jh=1+(jhexnn1(jhexoff(ih2)+i)-mbndry-1)/
     *                          nefhex
                        jf=jhexnn1(jhexoff(ih2)+i) -
     *                        nefhex*(jh-1) -
     *                           mbndry
                     endif
                     if(jh.le.0.or.jh.gt.numhex) then
                        ip1=ihexnn1(ihexoff(ih2)+
     *                              ielmface1(1,i,ihextyp(ih2)))
                        ip2=ihexnn1(ihexoff(ih2)+
     *                              ielmface1(2,i,ihextyp(ih2)))
                        ip3=ihexnn1(ihexoff(ih2)+
     *                              ielmface1(3,i,ihextyp(ih2)))
                        ip4=ihexnn1(ihexoff(ih2)+
     *                              ielmface1(4,i,ihextyp(ih2)))
                        xfacei=0.25d+00*(xic(ip1)+xic(ip2)+
     *                                   xic(ip3)+xic(ip4))
                        yfacei=0.25d+00*(yic(ip1)+yic(ip2)+
     *                                   yic(ip3)+yic(ip4))
                        zfacei=0.25d+00*(zic(ip1)+zic(ip2)+
     *                                   zic(ip3)+zic(ip4))
                        do ih3=1,numhex
                           do j=1,nelmnef(ihextyp(ih3))
                              jp1=ihexnn1(ihexoff(ih3) +
     *                                    ielmface1(1,j,ihextyp(ih3)))
                              jp2=ihexnn1(ihexoff(ih3) +
     *                                    ielmface1(2,j,ihextyp(ih3)))
                              jp3=ihexnn1(ihexoff(ih3) +
     *                                    ielmface1(3,j,ihextyp(ih3)))
                              jp4=ihexnn1(ihexoff(ih3) +
     *                                    ielmface1(4,j,ihextyp(ih3)))
                              xfacej=0.25d+00*(xic(jp1)+xic(jp2)+
     *                                         xic(jp3)+xic(jp4))
                              yfacej=0.25d+00*(yic(jp1)+yic(jp2)+
     *                                         yic(jp3)+yic(jp4))
                              zfacej=0.25d+00*(zic(jp1)+zic(jp2)+
     *                                         zic(jp3)+zic(jp4))
                              dist=(xfacej-xfacei)**2+
     *                             (yfacej-yfacei)**2+
     *                             (zfacej-zfacei)**2
                              if(ih2.ne.ih3.and.dist.lt.xfacdist) then
                                 jhexnn1(jhexoff(ih2)+i)=
     *                               nelmnef(ihextyp(ih3))*(ih3-1)+j
                                 jhexnn1(jhexoff(ih3)+j)=
     *                               nelmnef(ihextyp(ih2))*(ih2-1)+i
                              endif
                           enddo
                        enddo
                     endif
                  enddo
               enddo
               length=numhex
               call mmgetblk("ihexclr5",isubname,ipihexclr5,length,2,
     *                       icscode)
               do ih=1,numhex
                  ihexclr5(ih)=0
               enddo
               ihexclr5(1)=1
 400           continue
                  icount=0
                  do ih=1,numhex
                     if(ihexclr5(ih).eq.0) then
                        do i=1,nelmnef(ihextyp(ih))
                           if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                              jh=0
                              jf=0
                           elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                            jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                              jh=1+(jhexnn1(jhexoff(ih)+i)-1)/
     *                             nefhex
                              jf=jhexnn1(jhexoff(ih)+i) -
     *                              nefhex*(jh-1)
                           elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                              jh=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                                nefhex
                              jf=jhexnn1(jhexoff(ih)+i) -
     *                              nefhex*(jh-1) -
     *                                 mbndry
                           endif
                           if(jh.gt.0.and.jh.le.numhex) then
                              if(ihexclr5(ih).ne.0) then
                                 if(ihexclr5(jh).eq.ihexclr5(ih)) then
                                    write(wlog,9060) ih,i,jh,
     *                                                  ihexclr5(ih),
     *                                                  ihexclr5(jh)
                                    call writloga('default',0,wlog,
     *                                            0,iwerr)
 9060                               format("Hex5 color error: ",5i10)
                                 endif
                              else
                                 if(ihexclr5(jh).eq.1) then
                                    icount=icount+1
                                    ihexclr5(ih)=2
                                 elseif(ihexclr5(jh).eq.2) then
                                    icount=icount+1
                                    ihexclr5(ih)=1
                                 endif
                              endif
                           endif
                        enddo
                     endif
                  enddo
               if(icount.gt.0) goto 400
C**************call mmrelblk("ihexclr5",isubname,ipihexclr5,icscode)
               do ih=1,numhex
                  if(ihexclr5(ih).le.0) then
                     write(wlog,9070) ih,ihexclr5(ih)
                     call writloga('default',0,wlog,0,iwerr)
 9070                format("Hex5 with no color: ",2i10)
                  endif
               enddo
            endif
            do i=1,5
               ntet=ntet+1
               itetclr(ntet)=ihexclr(ih1)
               itettyp(ntet)=ifelmtet
               itetoff(ntet)=itoff
               jtetoff(ntet)=jtoff
               itoff=itoff+nelmnen(itettyp(ntet))
               jtoff=jtoff+nelmnef(itettyp(ntet))
               itet1(itetoff(ntet)+1)=
     *                          lalias(ihex5tet(1,i,ihexclr5(ih1)))
               itet1(itetoff(ntet)+2)=
     *                          lalias(ihex5tet(2,i,ihexclr5(ih1)))
               itet1(itetoff(ntet)+3)=
     *                          lalias(ihex5tet(3,i,ihexclr5(ih1)))
               itet1(itetoff(ntet)+4)=
     *                          lalias(ihex5tet(4,i,ihexclr5(ih1)))
               do j=1,nelmnef(itettyp(ntet))
                  jtet1(jtetoff(ntet)+j)=-1
               enddo
            enddo
 
C        ioption =  6 ==>  6 TETS PER HEX, NO NEW POINTS
         elseif(ioption.eq.6) then
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 7'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
            do i=1,6
               ntet=ntet+1
               itetclr(ntet)=ihexclr(ih1)
               itettyp(ntet)=ifelmtet
               itetoff(ntet)=itoff
               jtetoff(ntet)=jtoff
               itoff=itoff+nelmnen(itettyp(ntet))
               jtoff=jtoff+nelmnef(itettyp(ntet))
               itet1(itetoff(ntet)+1)=
     *                          lalias(ihex6tet(1,i))
               itet1(itetoff(ntet)+2)=
     *                          lalias(ihex6tet(2,i))
               itet1(itetoff(ntet)+3)=
     *                          lalias(ihex6tet(3,i))
               itet1(itetoff(ntet)+4)=
     *                          lalias(ihex6tet(4,i))
               do j=1,nelmnef(itettyp(ntet))
                  jtet1(jtetoff(ntet)+j)=-1
               enddo
            enddo
 
C        ioption = 14 ==> 14 TETS PER PRISM, 4 NEW POINTS (1 + 3 FACES)
         elseif(ioption.eq.14) then
            lalias(10)=npstart+ih1
            icount=0
            do i=1,nelmnef(ihextyp(ih1))
               if(ielmface0(i,ihextyp(ih1)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias(icount+nelmnen(ihextyp(ih1)))=
     *               ktet(jhexoff(ih1)+i)
                  do j=1,ielmface0(i,ihextyp(ih1))
                     jp1=j+1
                     if(jp1.gt.ielmface0(i,ihextyp(ih1))) jp1=1
                     ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtet
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=10
                     itet1(itetoff(ntet)+2)=ielmface1(j,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+3)=
     *                  ielmface1(jp1,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+4)=icount+nelmnen(ihextyp(ih1))
                  enddo
               elseif(ielmface0(i,ihextyp(ih1)).eq.
     *                nelmnen(ifelmtri)) then
                     ntet=ntet+1
                     itetclr(ntet)=ihexclr(ih1)
                     itettyp(ntet)=ifelmtet
                     itetoff(ntet)=itoff
                     jtetoff(ntet)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntet))
                     jtoff=jtoff+nelmnef(itettyp(ntet))
                     itet1(itetoff(ntet)+1)=10
                     itet1(itetoff(ntet)+2)=
     *                                      ielmface1(1,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+3)=
     *                                      ielmface1(2,i,ihextyp(ih1))
                     itet1(itetoff(ntet)+4)=
     *                                      ielmface1(3,i,ihextyp(ih1))
               endif
            enddo
            imt1(lalias(10))=0
            itp1(lalias(10))=0
            icr1(lalias(10))=0
            iign(lalias(10))=0
            xic2(lalias(10))=0.0
            yic2(lalias(10))=0.0
            zic2(lalias(10))=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih1))
               i1=ihexnn1(ihexoff(ih1)+i)
               itp=max(itp,itp1(i1))
               icr=min(icr,icr1(i1))
               ign=max(ign,iign(i1))
               xr=xic2(i1)
               yr=yic2(i1)
               zr=zic2(i1)
               xic2(lalias(10))=xic2(lalias(10))+xr
               yic2(lalias(10))=yic2(lalias(10))+yr
               zic2(lalias(10))=zic2(lalias(10))+zr
               rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
            enddo
            imt1(lalias(10))=ihexclr(ih1)
            itp1(lalias(10))=itp
            icr1(lalias(10))=icr
            iign(lalias(10))=1+ign
            xic2(lalias(10))=xic2(lalias(10))/nelmnen(ihextyp(ih1))
            yic2(lalias(10))=yic2(lalias(10))/nelmnen(ihextyp(ih1))
            zic2(lalias(10))=zic2(lalias(10))/nelmnen(ihextyp(ih1))
            rad1=rad1/dble(nelmnen(ihextyp(ih1)))
            rad2=sqrt(xic2(lalias(10))**2+
     *                yic2(lalias(10))**2+
     *                zic2(lalias(10))**2)
            if(iradavg.eq.1) then
               xic2(lalias(10))=xic2(lalias(10))*rad1/rad2
               yic2(lalias(10))=yic2(lalias(10))*rad1/rad2
               zic2(lalias(10))=zic2(lalias(10))*rad1/rad2
            endif
            icount=0
            do i=1,nelmnef(ihextyp(ih1))
               if(ielmface0(i,ihextyp(ih1)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias8=lalias(icount+nelmnen(ihextyp(ih1)))
                  ict=0
                  imt1(lalias8)=imt1(ihexnn1(ihexoff(ih1)+
     *                                    ielmface1(1,i,ihextyp(ih1))))
                  itp1(lalias8)=0
                  icr1(lalias8)=0
                  iign(lalias8)=0
                  xic2(lalias8)=0.0
                  yic2(lalias8)=0.0
                  zic2(lalias8)=0.0
                  if(ifvels) then
                     vels(1,lalias8)=0.0
                     vels(2,lalias8)=0.0
                     vels(3,lalias8)=0.0
                  endif
                  if(ifdens) then
                     dens(lalias8)=0.0
                  endif
                  if(ifpres) then
                     pres(lalias8)=0.0
                  endif
                  rad1=0.0
                  itp=0
                  icr=999999
                  ign=0
                  do j=1,ielmface0(i,ihextyp(ih1))
                     i1=ihexnn1(ihexoff(ih1)+
     *                          ielmface1(j,i,ihextyp(ih1)))
                     imt=imt1(i1)
                     if(imt1(lalias8).eq.imt) ict=ict+1
                     itp=max(itp,itp1(i1))
                     icr=min(icr,icr1(i1))
                     ign=max(ign,iign(i1))
                     xr=xic2(i1)
                     yr=yic2(i1)
                     zr=zic2(i1)
                     xic2(lalias8)=xic2(lalias8)+xr
                     yic2(lalias8)=yic2(lalias8)+yr
                     zic2(lalias8)=zic2(lalias8)+zr
                     rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  enddo
                  itp1(lalias8)=itp
                  icr1(lalias8)=icr
                  iign(lalias8)=1+ign
                  if(ict.ne.ielmface0(i,ihextyp(ih1))) then
                     if(jhexnn1(jhexoff(ih1)+i).eq.mbndry) then
                        jt=0
                        jf=0
                     elseif(jhexnn1(jhexoff(ih1)+i).gt.0 .and.
     *                      jhexnn1(jhexoff(ih1)+i).lt.mbndry) then
                        jt=1+(jhexnn1(jhexoff(ih1)+i)-1)/
     *                        nefhex
                        jf=jhexnn1(jhexoff(ih1)+i) -
     *                        nefhex*(jt-1)
                     elseif(jhexnn1(jhexoff(ih1)+i).gt.mbndry) then
                        jt=1+(jhexnn1(jhexoff(ih1)+i)-mbndry-1)/
     *                          nefhex
                        jf=jhexnn1(jhexoff(ih1)+i) -
     *                        nefhex*(jt-1) -
     *                           mbndry
                     endif
                     imtel1=ihexclr(ih1)
                     if(jt.eq.0) then
                        imt1(lalias8)=imtel1
                     else
                        jhex=jt
                        if(jhex.gt.0.and.jhex.le.numhex) then
                           imtel2=ihexclr(jt)
                        else
                           imtel2=imtel1
                        endif
                        if(imtel1.eq.imtel2) then
                           imt1(lalias8)=imtel1
                        else
                           if(iwcnt.le.20 .or. idebug.gt.0) then
                             write(wlog,9080) ih1,i,lalias8
                             call writloga('default',0,wlog,0,iwerr)
                           endif
                           iwcnt=iwcnt+1
                           imt1(lalias8)=imtel1
                       endif
                     endif
                  endif
                  xic2(lalias8)=xic2(lalias8)/ielmface0(i,ihextyp(ih1))
                  yic2(lalias8)=yic2(lalias8)/ielmface0(i,ihextyp(ih1))
                  zic2(lalias8)=zic2(lalias8)/ielmface0(i,ihextyp(ih1))
                  rad1=rad1/ielmface0(i,ihextyp(ih1))
                  rad2=sqrt(xic2(lalias8)**2+
     *                      yic2(lalias8)**2+
     *                      zic2(lalias8)**2)
                  if(iradavg.eq.1) then
                     xic2(lalias8)=xic2(lalias8)*rad1/rad2
                     yic2(lalias8)=yic2(lalias8)*rad1/rad2
                     zic2(lalias8)=zic2(lalias8)*rad1/rad2
                  endif
               endif
            enddo
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
 
C        ioption = 18 ==> 18 TETS PER PRISM, 6 NEW POINTS (1 + 5 FACES)
         elseif(ioption.eq.18) then
            lalias(15)=npstart+ih1
            do i=1,nelmnef(ihextyp(ih1))
               lalias(i+nelmnen(ihextyp(ih1)))=ktet(jhexoff(ih1)+i)
               do j=1,ielmface0(i,ihextyp(ih1))
                  jp1=j+1
                  if(jp1.gt.ielmface0(i,ihextyp(ih1))) jp1=1
                  ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=15
                  itet1(itetoff(ntet)+2)=ielmface1(j,i,ihextyp(ih1))
                  itet1(itetoff(ntet)+3)=ielmface1(jp1,i,ihextyp(ih1))
                  itet1(itetoff(ntet)+4)=i+nelmnen(ihextyp(ih1))
               enddo
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih1))
               i1=ihexnn1(ihexoff(ih1)+i)
               itp=max(itp,itp1(i1))
               icr=min(icr,icr1(i1))
               ign=max(ign,iign(i1))
               xr=xic2(i1)
               yr=yic2(i1)
               zr=zic2(i1)
               xic2(lalias(15))=xic2(lalias(15))+xr
               yic2(lalias(15))=yic2(lalias(15))+yr
               zic2(lalias(15))=zic2(lalias(15))+zr
               rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
            enddo
            imt1(lalias(15))=ihexclr(ih1)
            itp1(lalias(15))=itp
            icr1(lalias(15))=icr
            iign(lalias(15))=1+ign
            xic2(lalias(15))=xic2(lalias(15))/nelmnen(ihextyp(ih1))
            yic2(lalias(15))=yic2(lalias(15))/nelmnen(ihextyp(ih1))
            zic2(lalias(15))=zic2(lalias(15))/nelmnen(ihextyp(ih1))
            rad1=rad1/dble(nelmnen(ihextyp(ih1)))
            rad2=sqrt(xic2(lalias(15))**2+
     *                yic2(lalias(15))**2+
     *                zic2(lalias(15))**2)
            if(iradavg.eq.1) then
               xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
               yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
               zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
            endif
            do i=1,nelmnef(ihextyp(ih1))
               lalias8=lalias(i+nelmnen(ihextyp(ih1)))
               ict=0
               imt1(lalias8)=imt1(ihexnn1(ihexoff(ih1)+
     *                                    ielmface1(1,i,ihextyp(ih1))))
               itp1(lalias8)=0
               icr1(lalias8)=0
               iign(lalias8)=0
               xic2(lalias8)=0.0
               yic2(lalias8)=0.0
               zic2(lalias8)=0.0
               if(ifvels) then
                  vels(1,lalias8)=0.0
                  vels(2,lalias8)=0.0
                  vels(3,lalias8)=0.0
               endif
               if(ifdens) then
                  dens(lalias8)=0.0
               endif
               if(ifpres) then
                  pres(lalias8)=0.0
               endif
               rad1=0.0
               itp=0
               icr=999999
               ign=0
               do j=1,ielmface0(i,ihextyp(ih1))
                  i1=ihexnn1(ihexoff(ih1)+ielmface1(j,i,ihextyp(ih1)))
                  imt=imt1(i1)
                  if(imt1(lalias8).eq.imt) ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  ign=max(ign,iign(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias8)=xic2(lalias8)+xr
                  yic2(lalias8)=yic2(lalias8)+yr
                  zic2(lalias8)=zic2(lalias8)+zr
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               itp1(lalias8)=itp
               icr1(lalias8)=icr
               iign(lalias8)=1+ign
               if(ict.ne.ielmface0(i,ihextyp(ih1))) then
                  if(jhexnn1(jhexoff(ih1)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih1)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih1)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih1)+i)-1)/
     *                     nefhex
                     jf=jhexnn1(jhexoff(ih1)+i) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih1)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih1)+i)-mbndry-1)/
     *                       nefhex
                     jf=jhexnn1(jhexoff(ih1)+i) -
     *                     nefhex*(jt-1) -
     *                        mbndry
                  endif
                  imtel1=ihexclr(ih1)
                  if(jt.eq.0) then
                     imt1(lalias8)=imtel1
                  else
                     jhex=jt
                     if(jhex.gt.0.and.jhex.le.numhex) then
                        imtel2=ihexclr(jt)
                     else
                        imtel2=imtel1
                     endif
                     if(imtel1.eq.imtel2) then
                        imt1(lalias8)=imtel1
                     else
                        if(iwcnt.le.20 .or. idebug.gt.0) then
                          write(wlog,9080) ih1,i,lalias8
                          call writloga('default',0,wlog,0,iwerr)
                        endif
                        iwcnt=iwcnt+1
                        imt1(lalias8)=imtel1
                    endif
                  endif
               endif
               xic2(lalias8)=xic2(lalias8)/ielmface0(i,ihextyp(ih1))
               yic2(lalias8)=yic2(lalias8)/ielmface0(i,ihextyp(ih1))
               zic2(lalias8)=zic2(lalias8)/ielmface0(i,ihextyp(ih1))
               rad1=rad1/ielmface0(i,ihextyp(ih1))
               rad2=sqrt(xic2(lalias8)**2+
     *                   yic2(lalias8)**2+
     *                   zic2(lalias8)**2)
               if(iradavg.eq.1) then
                  xic2(lalias8)=xic2(lalias8)*rad1/rad2
                  yic2(lalias8)=yic2(lalias8)*rad1/rad2
                  zic2(lalias8)=zic2(lalias8)*rad1/rad2
               endif
            enddo
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
 
C        ioption = 24 ==> 24 TETS PER HEX, 7 NEW POINTS (1 + 6 FACES)
         elseif(ioption.eq.24) then
            lalias(15)=npstart+ih1
            do i=1,nelmnef(ihextyp(ih1))
               lalias(i+nelmnen(ihextyp(ih1)))=ktet(jhexoff(ih1)+i)
               do j=1,ielmface0(i,ihextyp(ih1))
                  jp1=j+1
                  if(jp1.gt.ielmface0(i,ihextyp(ih1))) jp1=1
                  ntet=ntet+1
                  itetclr(ntet)=ihexclr(ih1)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=itoff
                  jtetoff(ntet)=jtoff
                  itoff=itoff+nelmnen(itettyp(ntet))
                  jtoff=jtoff+nelmnef(itettyp(ntet))
                  itet1(itetoff(ntet)+1)=15
                  itet1(itetoff(ntet)+2)=ielmface1(j,i,ihextyp(ih1))
                  itet1(itetoff(ntet)+3)=ielmface1(jp1,i,ihextyp(ih1))
                  itet1(itetoff(ntet)+4)=i+nelmnen(ihextyp(ih1))
               enddo
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih1))
               i1=ihexnn1(ihexoff(ih1)+i)
               itp=max(itp,itp1(i1))
               icr=min(icr,icr1(i1))
               ign=max(ign,iign(i1))
               xr=xic2(i1)
               yr=yic2(i1)
               zr=zic2(i1)
               xic2(lalias(15))=xic2(lalias(15))+xr
               yic2(lalias(15))=yic2(lalias(15))+yr
               zic2(lalias(15))=zic2(lalias(15))+zr
               rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
            enddo
            imt1(lalias(15))=ihexclr(ih1)
            itp1(lalias(15))=itp
            icr1(lalias(15))=icr
            iign(lalias(15))=1+ign
            xic2(lalias(15))=xic2(lalias(15))/nelmnen(ihextyp(ih1))
            yic2(lalias(15))=yic2(lalias(15))/nelmnen(ihextyp(ih1))
            zic2(lalias(15))=zic2(lalias(15))/nelmnen(ihextyp(ih1))
            rad1=rad1/dble(nelmnen(ihextyp(ih1)))
            rad2=sqrt(xic2(lalias(15))**2+
     *                yic2(lalias(15))**2+
     *                zic2(lalias(15))**2)
            if(iradavg.eq.1) then
               xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
               yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
               zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
            endif
            do i=1,nelmnef(ihextyp(ih1))
               lalias8=lalias(i+nelmnen(ihextyp(ih1)))
               ict=0
               imt1(lalias8)=imt1(ihexnn1(ihexoff(ih1)+
     *                                    ielmface1(1,i,ihextyp(ih1))))
               itp1(lalias8)=0
               icr1(lalias8)=0
               iign(lalias8)=0
               xic2(lalias8)=0.0
               yic2(lalias8)=0.0
               zic2(lalias8)=0.0
            if(ifvels) then
               vels(1,lalias8)=0.0
               vels(2,lalias8)=0.0
               vels(3,lalias8)=0.0
            endif
            if(ifdens) then
               dens(lalias8)=0.0
            endif
            if(ifpres) then
               pres(lalias8)=0.0
            endif
               rad1=0.0
               itp=0
               icr=999999
               ign=0
               do j=1,ielmface0(i,ihextyp(ih1))
                  i1=ihexnn1(ihexoff(ih1)+ielmface1(j,i,ihextyp(ih1)))
                  imt=imt1(i1)
                  if(imt1(lalias8).eq.imt) ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  ign=max(ign,iign(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias8)=xic2(lalias8)+xr
                  yic2(lalias8)=yic2(lalias8)+yr
                  zic2(lalias8)=zic2(lalias8)+zr
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               itp1(lalias8)=itp
               icr1(lalias8)=icr
               iign(lalias8)=1+ign
               if(ict.ne.ielmface0(i,ihextyp(ih1))) then
                  if(jhexnn1(jhexoff(ih1)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih1)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih1)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih1)+i)-1)/
     *                     nefhex
                     jf=jhexnn1(jhexoff(ih1)+i) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih1)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih1)+i)-mbndry-1)/
     *                       nefhex
                     jf=jhexnn1(jhexoff(ih1)+i) -
     *                     nefhex*(jt-1) -
     *                        mbndry
                  endif
                  imtel1=ihexclr(ih1)
                  if(jt.eq.0) then
                     imt1(lalias8)=imtel1
                  else
                     jhex=jt
                     if(jhex.gt.0.and.jhex.le.numhex) then
                        imtel2=ihexclr(jt)
                     else
                        imtel2=imtel1
                     endif
                     if(imtel1.eq.imtel2) then
                        imt1(lalias8)=imtel1
                     else
                        if(iwcnt.le.20 .or. idebug.gt.0) then
                          write(wlog,9101)ih,i,lalias8
 9101                     format("Error in assigning node color:",
     *                    " element=",i10," face=",i3," node=",i10)
                          call writloga('default',0,wlog,0,iwerr)
                        endif
                        iwcnt=iwcnt+1
                        imt1(lalias8)=imtel1
                    endif
                  endif
               endif
               xic2(lalias8)=xic2(lalias8)/ielmface0(i,ihextyp(ih1))
               yic2(lalias8)=yic2(lalias8)/ielmface0(i,ihextyp(ih1))
               zic2(lalias8)=zic2(lalias8)/ielmface0(i,ihextyp(ih1))
               rad1=rad1/ielmface0(i,ihextyp(ih1))
               rad2=sqrt(xic2(lalias8)**2+
     *                   yic2(lalias8)**2+
     *                   zic2(lalias8)**2)
               if(iradavg.eq.1) then
                  xic2(lalias8)=xic2(lalias8)*rad1/rad2
                  yic2(lalias8)=yic2(lalias8)*rad1/rad2
                  zic2(lalias8)=zic2(lalias8)*rad1/rad2
               endif
            enddo
            do it=ntstart+1,ntet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=lalias(itet1(itetoff(it)+i))
               enddo
            enddo
         endif
      enddo
      length=npoints+numhex+nefhex*numhex
      call mmgetblk("ialiasp",isubname,ipialiasp,length,2,icscode)
      do i=1,npoints+numhex+nefhex*numhex
         ialiasp(i)=i
      enddo
      do i=1,numhex
         do j=1,nelmnee(ihextyp(i))
            i1=ialiasp(ihexnn1(ihexoff(i)+ielmedge1(1,j,ihextyp(i))))
            i2=ialiasp(ihexnn1(ihexoff(i)+ielmedge1(2,j,ihextyp(i))))
            dist=(xic2(i1)-xic2(i2))**2 +
     *           (yic2(i1)-yic2(i2))**2 +
     *           (zic2(i1)-zic2(i2))**2
            i1=max(i1,i2)
            if(dist.le.xfacdist) then
               do k=1,nelmnen(ihextyp(i))
                  i2=ihexnn1(ihexoff(i)+k)
                  dist=(xic2(i1)-xic2(i2))**2 +
     *                 (yic2(i1)-yic2(i2))**2 +
     *                 (zic2(i1)-zic2(i2))**2
                  if(dist.le.xfacdist) then
                     i3=max(i1,i2)
                     i2=i1+i2-i3
                     ialiasp(i2)=ialiasp(i3)
                  endif
               enddo
            endif
         enddo
      enddo
 
c     IF NEW POINTS ADDED WITH NEW ELEMENTS
      if(ioption.eq.-1.or.
     *   ioption.eq.0.or.
     *   ioption.eq.14.or.
     *   ioption.eq.18.or.
     *   ioption.eq.24) then
         do ih=1,numhex
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.ifelmqud) then
                  i1=ktet(jhexoff(ih)+i)
                  if(i1.gt.0) then
                     if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                        jt=0
                        jf=0
                     elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                      jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                        jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                        jf=jhexnn1(jhexoff(ih)+i) -
     *                        nefhex*(jt-1)
                     elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                        jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                          nefhex
                        jf=jhexnn1(jhexoff(ih)+i) -
     *                        nefhex*(jt-1) -
     *                           mbndry
                     endif
                     if(jt.gt.0.and.jt.le.numhex) then
                        i2=ktet(jhexoff(jt)+jf)
                        dist=(xic2(i1)-xic2(i2))**2 +
     *                       (yic2(i1)-yic2(i2))**2 +
     *                       (zic2(i1)-zic2(i2))**2
                        if(dist.le.xfacdist) then
                           i3=max(i1,i2)
                           i2=i1+i2-i3
                           ialiasp(i2)=ialiasp(i3)
                           do k=1,nelmnef(ihextyp(jt))
                              i2=ktet(jhexoff(jt)+k)
                              dist=(xic2(i1)-xic2(i2))**2 +
     *                             (yic2(i1)-yic2(i2))**2 +
     *                             (zic2(i1)-zic2(i2))**2
                              if(dist.le.xfacdist) then
                                 i3=max(i1,i2)
                                 i2=i1+i2-i3
                                 ialiasp(i2)=ialiasp(i3)
                              endif
                           enddo
                        endif
                     endif
                     i1=ktet(jhexoff(ih)+i)
                     do k=1,nelmnef(ihextyp(ih))
                        i2=ktet(jhexoff(ih)+k)
                        dist=(xic2(i1)-xic2(i2))**2 +
     *                       (yic2(i1)-yic2(i2))**2 +
     *                       (zic2(i1)-zic2(i2))**2
                        if(dist.le.xfacdist) then
                           i3=max(i1,i2)
                           i2=i1+i2-i3
                           ialiasp(i2)=ialiasp(i3)
                        endif
                        do l=1,ielmface0(k,ihextyp(ih))
                           i2=ihexnn1(ihexoff(ih)+
     *                                ielmface1(l,k,ihextyp(ih)))
                           dist=(xic2(i1)-xic2(i2))**2 +
     *                          (yic2(i1)-yic2(i2))**2 +
     *                          (zic2(i1)-zic2(i2))**2
                           if(dist.le.xfacdist) then
                              i3=max(i1,i2)
                              i2=i1+i2-i3
                              ialiasp(i2)=ialiasp(i3)
                           endif
                        enddo
                     enddo
                  endif
               endif
            enddo
         enddo
         do ih=1,numhex
            if(ihextyp(ih).eq.ifelmtri) then
            elseif(ihextyp(ih).eq.ifelmtet) then
            else
               i1=npstart+ih
               do k=1,nelmnen(ihextyp(ih))
                  i2=ihexnn1(ihexoff(ih)+k)
                  dist=(xic2(i1)-xic2(i2))**2 +
     *                 (yic2(i1)-yic2(i2))**2 +
     *                 (zic2(i1)-zic2(i2))**2
                  if(dist.le.xfacdist) then
                     i3=max(i1,i2)
                     i2=i1+i2-i3
                     ialiasp(i2)=ialiasp(i3)
                  endif
               enddo
               do k=1,nelmnef(ihextyp(ih))
                  i2=ktet(jhexoff(ih)+k)
                  if(i2.gt.0) then
                     dist=(xic2(i1)-xic2(i2))**2 +
     *                    (yic2(i1)-yic2(i2))**2 +
     *                    (zic2(i1)-zic2(i2))**2
                     if(dist.le.xfacdist) then
                        i3=max(i1,i2)
                        i2=i1+i2-i3
                        ialiasp(i2)=ialiasp(i3)
                     endif
                  endif
               enddo
               do k=1,nelmnef(ihextyp(ih))
                  i1=npstart+ih
                  if(jhexnn1(jhexoff(ih)+k).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih)+k).gt.0 .and.
     *                   jhexnn1(jhexoff(ih)+k).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+k)-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+k) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih)+k).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+k)-mbndry-1)/
     *                       nefhex
                     jf=jhexnn1(jhexoff(ih)+k) -
     *                     nefhex*(jt-1) -
     *                        mbndry
                  endif
                  if(jt.gt.0) then
                     i2=npstart+jt
                     dist=(xic2(i1)-xic2(i2))**2 +
     *                    (yic2(i1)-yic2(i2))**2 +
     *                    (zic2(i1)-zic2(i2))**2
                     if(dist.le.xfacdist) then
                        i3=max(i1,i2)
                        i2=i1+i2-i3
                        ialiasp(i2)=ialiasp(i3)
                        do l=1,nelmnef(ihextyp(ih))
                           i1=ktet(jhexoff(ih)+l)
                           do m=1,nelmnef(ihextyp(jt))
                              i2=ktet(jhexoff(jt)+m)
                              if(i2.gt.0) then
                                  dist=(xic2(i1)-xic2(i2))**2 +
     *                                 (yic2(i1)-yic2(i2))**2 +
     *                                 (zic2(i1)-zic2(i2))**2
                                  if(dist.le.xfacdist) then
                                     i3=max(i1,i2)
                                     i2=i1+i2-i3
                                     ialiasp(i2)=ialiasp(i3)
                                 endif
                              endif
                           enddo
                        enddo
                     endif
                  endif
               enddo
            endif
         enddo
      endif
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 8'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      npoints1=npoints+numhex+nefhex*numhex
      do i=1,npoints1
         ict=0
         i1=i
 200     continue
         ict=ict+1
         if(ict.gt.npoints1) then
            write(wlog,9090) i,i1,ialiasp(i1)
            call writloga('default',0,wlog,0,iwerr)
 9090       format("Infinite loop on alias list: ",3i10)
            stop
         endif
         if(i1.ne.ialiasp(i1)) then
            i1=ialiasp(i1)
            goto 200
         else
            ialiasp(i)=i1
         endif
      enddo
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 9'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
C  get work space for cmo_interpolate
       nelmadd1=0
       nelmadd2=0
      if(ioption.eq.-1) then
          nelmadd1=1+nelmnef(ifelmnew)
          nelmadd2=nelmnen(ifelmnew)
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd1*nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      elseif(ioption.eq.0) then
          nelmadd1=1+nelmnef(ifelmnew)
          nelmadd2=nelmnen(ifelmnew)
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd1*nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      elseif(ioption.eq.4) then
          nelmadd1=1
          nelmadd2=nenhex
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      elseif(ioption.eq.14) then
          nelmadd1=1+3
          nelmadd2=nelmnen(ifelmpyr)
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd1*nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      elseif(ioption.eq.18) then
          nelmadd1=1+nelmnef(ifelmpri)
          nelmadd2=nelmnen(ifelmpri)
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd1*nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      elseif(ioption.eq.24) then
          nelmadd1=1+nelmnef(ifelmhex)
          nelmadd2=nenhex
          length=nelmadd1*numhex
          call mmgetblk('list',isubname,iplist,length,1,ier)
          length=nelmadd1*nelmadd2*numhex
          call mmgetblk('ilist',isubname,ipilist,length,1,ier)
          call mmgetblk('xweight',isubname,ipxweight,length,2,ier)
      endif
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 10'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
      do ih=1,numhex
         if(nelmadd1.gt.0) then
            do i=1,nelmadd1
               list(nelmadd1*(ih-1)+i)=0
               do j=1,nelmadd2
                  ilist(nelmadd1*nelmadd2*(ih-1)+nelmadd2*(i-1)+j)=1
                  xweight(nelmadd1*nelmadd2*(ih-1)+nelmadd2*(i-1)+j)=0.0
               enddo
            enddo
         endif
         do i=1,nelmnen(ihextyp(ih))
            lalias(i)=ihexnn1(ihexoff(ih)+i)
         enddo
         if(ioption.eq.-1) then
            lalias(15)=npstart+ih
            list(nelmadd1*(ih-1)+1)=lalias(15)
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ifvels) then
               vels(1,lalias(15))=0.0
               vels(2,lalias(15))=0.0
               vels(3,lalias(15))=0.0
            endif
            if(ifdens) then
               dens(lalias(15))=0.0
            endif
            if(ifpres) then
               pres(lalias(15))=0.0
            endif
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih))
               i1=ihexnn1(ihexoff(ih)+i)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ilist((ih-1)*nelmadd2+i)=i1
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  if(ifvels) then
                     vels(1,lalias(15))=vels(1,lalias(15))+vels(1,i1)
                     vels(2,lalias(15))=vels(2,lalias(15))+vels(2,i1)
                     vels(3,lalias(15))=vels(3,lalias(15))+vels(3,i1)
                  endif
                  if(ifdens) then
                     dens(lalias(15))=dens(lalias(15))+dens(i1)
                  endif
                  if(ifpres) then
                     pres(lalias(15))=pres(lalias(15))+pres(i1)
                  endif
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  xweight((ih-1)*nelmadd2+i)=1.0d+00
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ih)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               if(ifvels) then
                  vels(1,lalias(15))=vels(1,lalias(15))/dble(ict)
                  vels(2,lalias(15))=vels(2,lalias(15))/dble(ict)
                  vels(3,lalias(15))=vels(3,lalias(15))/dble(ict)
               endif
               if(ifdens) then
                  dens(lalias(15))=dens(lalias(15))/dble(ict)
               endif
               if(ifpres) then
                  pres(lalias(15))=pres(lalias(15))/dble(ict)
               endif
            endif
         elseif(ioption.eq.0) then
            lalias(15)=npstart+ih
            list(nelmadd1*(ih-1)+1)=lalias(15)
            icount=0
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias(icount+nelmnen(ihextyp(ih)))=
     *               ktet(jhexoff(ih)+i)
               endif
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ifvels) then
               vels(1,lalias(15))=0.0
               vels(2,lalias(15))=0.0
               vels(3,lalias(15))=0.0
            endif
            if (ifdens) then
               dens(lalias(15))=0.0
            endif
            if (ifpres) then
               pres(lalias(15))=0.0
            endif
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih))
               i1=ihexnn1(ihexoff(ih)+i)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ilist(nelmadd1*nelmadd2*(ih-1)+i)=i1
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  if(ifvels) then
                     vels(1,lalias(15))=vels(1,lalias(15))+vels(1,i1)
                     vels(2,lalias(15))=vels(2,lalias(15))+vels(2,i1)
                     vels(3,lalias(15))=vels(3,lalias(15))+vels(3,i1)
                  endif
                  if(ifdens) then
                     dens(lalias(15))=dens(lalias(15))+dens(i1)
                  endif
                  if(ifpres) then
                     pres(lalias(15))=pres(lalias(15))+pres(i1)
                  endif
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  xweight(nelmadd1*nelmadd2*(ih-1)+i)=1.0d+00
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ih)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               if(ifvels) then
                  vels(1,lalias(15))=vels(1,lalias(15))/dble(ict)
                  vels(2,lalias(15))=vels(2,lalias(15))/dble(ict)
                  vels(3,lalias(15))=vels(3,lalias(15))/dble(ict)
               endif
               if(ifdens) then
                  dens(lalias(15))=dens(lalias(15))/dble(ict)
               endif
               if(ifpres) then
                  pres(lalias(15))=pres(lalias(15))/dble(ict)
               endif
            endif
            icount=0
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias8=lalias(icount+nelmnen(ihextyp(ih)))
                  list(nelmadd1*(ih-1)+1+i)=lalias8
                  if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                         nefhex
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                          nefhex*(jt-1) -
     *                           mbndry
                  endif
                  ict=0
                  imt1(lalias8)=imt1(ihexnn1(ihexoff(ih)+
     *                               ielmface1(1,i,ihextyp(ih))))
                  iign(lalias8)=0
                  xic2(lalias8)=0.0
                  yic2(lalias8)=0.0
                  zic2(lalias8)=0.0
                  if(ifvels) then
                     vels(1,lalias8)=0.0
                     vels(2,lalias8)=0.0
                     vels(3,lalias8)=0.0
                  endif
                  if(ifdens) then
                     dens(lalias8)=0.0
                  endif
                  if(ifpres) then
                     pres(lalias8)=0.0
                  endif
                  rad1=0.0
                  itp=0
                  icr=9999999
                  ign=0
                  ict1=0
                  do j=1,ielmface0(i,ihextyp(ih))
                     i1=ihexnn1(ihexoff(ih)+
     *                          ielmface1(j,i,ihextyp(ih)))
C*****               if(ialiasp(i1).eq.i1) then
                     if(i1.eq.i1) then
                        ilist(nelmadd1*nelmadd2*(ih-1)+
     *                        nelmadd2*i+j)=i1
                        ict1=ict1+1
                        xr=xic2(i1)
                        yr=yic2(i1)
                        zr=zic2(i1)
                        imt=imt1(i1)
                        itp=max(itp,itp1(i1))
                        icr=min(icr,icr1(i1))
                        ign=max(ign,iign(i1))
                        if(imt1(lalias8).eq.imt) ict=ict+1
                        xic2(lalias8)=xic2(lalias8)+xr
                        yic2(lalias8)=yic2(lalias8)+yr
                        zic2(lalias8)=zic2(lalias8)+zr
                        if(ifvels) then
                           vels(1,lalias8)=vels(1,lalias8)+vels(1,i1)
                           vels(2,lalias8)=vels(2,lalias8)+vels(2,i1)
                           vels(3,lalias8)=vels(3,lalias8)+vels(3,i1)
                        endif
                        if(ifdens) then
                           dens(lalias8)=dens(lalias8)+dens(i1)
                        endif
                        if(ifpres) then
                           pres(lalias8)=pres(lalias8)+pres(i1)
                        endif
                        rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                        xweight(nelmadd1*nelmadd2*(ih-1)+
     *                          nelmadd2*i+j)=1.0d+00
                     endif
                  enddo
                  if(ict1.gt.0) then
                     itp1(lalias8)=itp
                     icr1(lalias8)=icr
                     iign(lalias8)=1+ign
                     if(ict.ne.ielmface0(i,ihextyp(ih))) then
                        imtel1=ihexclr(ih)
                        if(jt.eq.0) then
                           imt1(lalias8)=imtel1
                        else
                           jhex=jt
                           if(jhex.gt.0.and.jhex.le.numhex) then
                              imtel2=ihexclr(jt)
                           else
                              imtel2=imtel1
                           endif
                           if(imtel1.eq.imtel2) then
                              imt1(lalias8)=imtel1
                           else
                             if (iwcnt.le.20 .or. idebug.gt.0) then
                               write(wlog,9100) ih,i,lalias8,
     *                         (ihexnn1(ihexoff(ih)+
     *                              ielmface1(k,i,ihextyp(ih))),k=1,4)
                               call writloga(
     *                            'default',0,wlog,0,iwerr)
                             endif
                             iwcnt=iwcnt+1
                             imt1(lalias8)=imtel1
                           endif
                        endif
                     endif
                     xic2(lalias8)=xic2(lalias8)/dble(ict1)
                     yic2(lalias8)=yic2(lalias8)/dble(ict1)
                     zic2(lalias8)=zic2(lalias8)/dble(ict1)
                     if(iradavg.eq.1) then
                        rad1=rad1/dble(ict1)
                        rad2=sqrt(xic2(lalias8)**2+
     *                            yic2(lalias8)**2+
     *                            zic2(lalias8)**2)
                        xic2(lalias8)=xic2(lalias8)*rad1/rad2
                        yic2(lalias8)=yic2(lalias8)*rad1/rad2
                        zic2(lalias8)=zic2(lalias8)*rad1/rad2
                     endif
                     if(ifvels) then
                        vels(1,lalias8)=vels(1,lalias8)/dble(ict)
                        vels(2,lalias8)=vels(2,lalias8)/dble(ict)
                        vels(3,lalias8)=vels(3,lalias8)/dble(ict)
                     endif
                     if(ifdens) then
                        dens(lalias8)=dens(lalias8)/dble(ict)
                     endif
                     if(ifpres) then
                        pres(lalias8)=pres(lalias8)/dble(ict)
                     endif
                  endif
               endif
            enddo
         elseif(ioption.eq.2) then
         elseif(ioption.eq.4) then
            lalias(15)=npstart+ih
            list(nelmadd1*(ih-1)+1)=lalias(15)
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ifvels) then
               vels(1,lalias(15))=0.0
               vels(2,lalias(15))=0.0
               vels(3,lalias(15))=0.0
            endif
            if(ifdens) then
               dens(lalias(15))=0.0
            endif
            if(ifpres) then
               pres(lalias(15))=0.0
            endif
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih))
               i1=ihexnn1(ihexoff(ih)+i)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ilist((ih-1)*nelmadd2+i)=i1
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  if(ifvels) then
                     vels(1,lalias(15))=vels(1,lalias(15))+vels(1,i1)
                     vels(2,lalias(15))=vels(2,lalias(15))+vels(2,i1)
                     vels(3,lalias(15))=vels(3,lalias(15))+vels(3,i1)
                  endif
                  if(ifdens) then
                     dens(lalias(15))=dens(lalias(15))+dens(i1)
                  endif
                  if(ifpres) then
                     pres(lalias(15))=pres(lalias(15))+pres(i1)
                  endif
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  xweight((ih-1)*nelmadd2+i)=1.0d+00
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ih)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               if(ifvels) then
                  vels(1,lalias(15))=vels(1,lalias(15))/dble(ict)
                  vels(2,lalias(15))=vels(2,lalias(15))/dble(ict)
                  vels(3,lalias(15))=vels(3,lalias(15))/dble(ict)
               endif
               if(ifdens) then
                  dens(lalias(15))=dens(lalias(15))/dble(ict)
               endif
               if(ifpres) then
                  pres(lalias(15))=pres(lalias(15))/dble(ict)
               endif
            endif
         elseif(ioption.eq.5) then
         elseif(ioption.eq.6) then
         elseif(ioption.eq.14) then
            lalias(15)=npstart+ih
            list(nelmadd1*(ih-1)+1)=lalias(15)
            icount=0
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias(icount+nelmnen(ihextyp(ih)))=
     *               ktet(jhexoff(ih)+i)
               endif
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ifvels) then
               vels(1,lalias(15))=0.0
               vels(2,lalias(15))=0.0
               vels(3,lalias(15))=0.0
            endif
            if (ifdens) then
               dens(lalias(15))=0.0
            endif
            if (ifpres) then
               pres(lalias(15))=0.0
            endif
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih))
               i1=ihexnn1(ihexoff(ih)+i)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ilist(nelmadd1*nelmadd2*(ih-1)+i)=i1
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  if(ifvels) then
                     vels(1,lalias(15))=vels(1,lalias(15))+vels(1,i1)
                     vels(2,lalias(15))=vels(2,lalias(15))+vels(2,i1)
                     vels(3,lalias(15))=vels(3,lalias(15))+vels(3,i1)
                  endif
                  if(ifdens) then
                     dens(lalias(15))=dens(lalias(15))+dens(i1)
                  endif
                  if(ifpres) then
                     pres(lalias(15))=pres(lalias(15))+pres(i1)
                  endif
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  xweight(nelmadd1*nelmadd2*(ih-1)+i)=1.0d+00
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ih)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               if(ifvels) then
                  vels(1,lalias(15))=vels(1,lalias(15))/dble(ict)
                  vels(2,lalias(15))=vels(2,lalias(15))/dble(ict)
                  vels(3,lalias(15))=vels(3,lalias(15))/dble(ict)
               endif
               if(ifdens) then
                  dens(lalias(15))=dens(lalias(15))/dble(ict)
               endif
               if(ifpres) then
                  pres(lalias(15))=pres(lalias(15))/dble(ict)
               endif
            endif
            icount=0
            do i=1,nelmnef(ihextyp(ih))
               if(ielmface0(i,ihextyp(ih)).eq.nelmnen(ifelmqud)) then
                  icount=icount+1
                  lalias8=lalias(icount+nelmnen(ihextyp(ih)))
                  list(nelmadd1*(ih-1)+1+i)=lalias8
                  if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                     jt=0
                     jf=0
                  elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                   jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                     nefhex*(jt-1)
                  elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                     jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                         nefhex
                     jf=jhexnn1(jhexoff(ih)+i) -
     *                          nefhex*(jt-1) -
     *                           mbndry
                  endif
                  ict=0
                  imt1(lalias8)=imt1(ihexnn1(ihexoff(ih)+
     *                               ielmface1(1,i,ihextyp(ih))))
                  iign(lalias8)=0
                  xic2(lalias8)=0.0
                  yic2(lalias8)=0.0
                  zic2(lalias8)=0.0
                  if(ifvels) then
                     vels(1,lalias8)=0.0
                     vels(2,lalias8)=0.0
                     vels(3,lalias8)=0.0
                  endif
                  if(ifdens) then
                     dens(lalias8)=0.0
                  endif
                  if(ifpres) then
                     pres(lalias8)=0.0
                  endif
                  rad1=0.0
                  itp=0
                  icr=9999999
                  ign=0
                  ict1=0
                  do j=1,ielmface0(i,ihextyp(ih))
                     i1=ihexnn1(ihexoff(ih)+
     *                          ielmface1(j,i,ihextyp(ih)))
C*****               if(ialiasp(i1).eq.i1) then
                     if(i1.eq.i1) then
                        ilist(nelmadd1*nelmadd2*(ih-1)+
     *                        nelmadd2*i+j)=i1
                        ict1=ict1+1
                        xr=xic2(i1)
                        yr=yic2(i1)
                        zr=zic2(i1)
                        imt=imt1(i1)
                        itp=max(itp,itp1(i1))
                        icr=min(icr,icr1(i1))
                        ign=max(ign,iign(i1))
                        if(imt1(lalias8).eq.imt) ict=ict+1
                        xic2(lalias8)=xic2(lalias8)+xr
                        yic2(lalias8)=yic2(lalias8)+yr
                        zic2(lalias8)=zic2(lalias8)+zr
                        if(ifvels) then
                           vels(1,lalias8)=vels(1,lalias8)+vels(1,i1)
                           vels(2,lalias8)=vels(2,lalias8)+vels(2,i1)
                           vels(3,lalias8)=vels(3,lalias8)+vels(3,i1)
                        endif
                        if(ifdens) then
                           dens(lalias8)=dens(lalias8)+dens(i1)
                        endif
                        if(ifpres) then
                           pres(lalias8)=pres(lalias8)+pres(i1)
                        endif
                        rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                        xweight(nelmadd1*nelmadd2*(ih-1)+
     *                          nelmadd2*i+j)=1.0d+00
                     endif
                  enddo
                  if(ict1.gt.0) then
                     itp1(lalias8)=itp
                     icr1(lalias8)=icr
                     iign(lalias8)=1+ign
                     if(ict.ne.ielmface0(i,ihextyp(ih))) then
                        imtel1=ihexclr(ih)
                        if(jt.eq.0) then
                           imt1(lalias8)=imtel1
                        else
                           jhex=jt
                           if(jhex.gt.0.and.jhex.le.numhex) then
                              imtel2=ihexclr(jt)
                           else
                              imtel2=imtel1
                           endif
                           if(imtel1.eq.imtel2) then
                              imt1(lalias8)=imtel1
                           else
                             if (iwcnt.le.20 .or. idebug.gt.0) then
                               write(wlog,9100) ih,i,lalias8,
     *                         (ihexnn1(ihexoff(ih)+
     *                              ielmface1(k,i,ihextyp(ih))),k=1,4)
                               call writloga(
     *                            'default',0,wlog,0,iwerr)
                             endif
                             iwcnt=iwcnt+1
                             imt1(lalias8)=imtel1
                           endif
                        endif
                     endif
                     xic2(lalias8)=xic2(lalias8)/dble(ict1)
                     yic2(lalias8)=yic2(lalias8)/dble(ict1)
                     zic2(lalias8)=zic2(lalias8)/dble(ict1)
                     if(iradavg.eq.1) then
                        rad1=rad1/dble(ict1)
                        rad2=sqrt(xic2(lalias8)**2+
     *                            yic2(lalias8)**2+
     *                            zic2(lalias8)**2)
                        xic2(lalias8)=xic2(lalias8)*rad1/rad2
                        yic2(lalias8)=yic2(lalias8)*rad1/rad2
                        zic2(lalias8)=zic2(lalias8)*rad1/rad2
                     endif
                     if(ifvels) then
                        vels(1,lalias8)=vels(1,lalias8)/dble(ict)
                        vels(2,lalias8)=vels(2,lalias8)/dble(ict)
                        vels(3,lalias8)=vels(3,lalias8)/dble(ict)
                     endif
                     if(ifdens) then
                        dens(lalias8)=dens(lalias8)/dble(ict)
                     endif
                     if(ifpres) then
                        pres(lalias8)=pres(lalias8)/dble(ict)
                     endif
                  endif
               endif
            enddo
         elseif(ioption.eq.18.or.ioption.eq.24) then
            lalias(15)=npstart+ih
            list(nelmadd1*(ih-1)+1)=lalias(15)
            do i=1,nelmnef(ihextyp(ih))
               lalias(i+nelmnen(ihextyp(ih)))=ktet(jhexoff(ih)+i)
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            iign(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            if(ifvels) then
               vels(1,lalias(15))=0.0
               vels(2,lalias(15))=0.0
               vels(3,lalias(15))=0.0
            endif
            if (ifdens) then
               dens(lalias(15))=0.0
            endif
            if (ifpres) then
               pres(lalias(15))=0.0
            endif
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,nelmnen(ihextyp(ih))
               i1=ihexnn1(ihexoff(ih)+i)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ilist(nelmadd1*nelmadd2*(ih-1)+i)=i1
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  if(ifvels) then
                     vels(1,lalias(15))=vels(1,lalias(15))+vels(1,i1)
                     vels(2,lalias(15))=vels(2,lalias(15))+vels(2,i1)
                     vels(3,lalias(15))=vels(3,lalias(15))+vels(3,i1)
                  endif
                  if(ifdens) then
                     dens(lalias(15))=dens(lalias(15))+dens(i1)
                  endif
                  if(ifpres) then
                     pres(lalias(15))=pres(lalias(15))+pres(i1)
                  endif
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  xweight(nelmadd1*nelmadd2*(ih-1)+i)=1.0d+00
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ih)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               iign(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               if(ifvels) then
                  vels(1,lalias(15))=vels(1,lalias(15))/dble(ict)
                  vels(2,lalias(15))=vels(2,lalias(15))/dble(ict)
                  vels(3,lalias(15))=vels(3,lalias(15))/dble(ict)
               endif
               if(ifdens) then
                  dens(lalias(15))=dens(lalias(15))/dble(ict)
               endif
               if(ifpres) then
                  pres(lalias(15))=pres(lalias(15))/dble(ict)
               endif
            endif
            do i=1,nelmnef(ihextyp(ih))
               lalias8=lalias(i+nelmnen(ihextyp(ih)))
               list(nelmadd1*(ih-1)+1+i)=lalias8
               if(jhexnn1(jhexoff(ih)+i).eq.mbndry) then
                  jt=0
                  jf=0
               elseif(jhexnn1(jhexoff(ih)+i).gt.0 .and.
     *                jhexnn1(jhexoff(ih)+i).lt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-1)/nefhex
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                  nefhex*(jt-1)
               elseif(jhexnn1(jhexoff(ih)+i).gt.mbndry) then
                  jt=1+(jhexnn1(jhexoff(ih)+i)-mbndry-1)/
     *                      nefhex
                  jf=jhexnn1(jhexoff(ih)+i) -
     *                       nefhex*(jt-1) -
     *                        mbndry
               endif
               ict=0
               imt1(lalias8)=imt1(ihexnn1(ihexoff(ih)+
     *                                    ielmface1(1,i,ihextyp(ih))))
               iign(lalias8)=0
               xic2(lalias8)=0.0
               yic2(lalias8)=0.0
               zic2(lalias8)=0.0
               if(ifvels) then
                  vels(1,lalias8)=0.0
                  vels(2,lalias8)=0.0
                  vels(3,lalias8)=0.0
               endif
               if(ifdens) then
                  dens(lalias8)=0.0
               endif
               if(ifpres) then
                  pres(lalias8)=0.0
               endif
               rad1=0.0
               itp=0
               icr=9999999
               ign=0
               ict1=0
               do j=1,ielmface0(i,ihextyp(ih))
                  i1=ihexnn1(ihexoff(ih)+
     *                       ielmface1(j,i,ihextyp(ih)))
C*****            if(ialiasp(i1).eq.i1) then
                  if(i1.eq.i1) then
                     ilist(nelmadd1*nelmadd2*(ih-1)+
     *                     nelmadd2*i+j)=i1
                     ict1=ict1+1
                     xr=xic2(i1)
                     yr=yic2(i1)
                     zr=zic2(i1)
                     imt=imt1(i1)
                     itp=max(itp,itp1(i1))
                     icr=min(icr,icr1(i1))
                     ign=max(ign,iign(i1))
                     if(imt1(lalias8).eq.imt) ict=ict+1
                     xic2(lalias8)=xic2(lalias8)+xr
                     yic2(lalias8)=yic2(lalias8)+yr
                     zic2(lalias8)=zic2(lalias8)+zr
                     if(ifvels) then
                        vels(1,lalias8)=vels(1,lalias8)+vels(1,i1)
                        vels(2,lalias8)=vels(2,lalias8)+vels(2,i1)
                        vels(3,lalias8)=vels(3,lalias8)+vels(3,i1)
                     endif
                     if(ifdens) then
                        dens(lalias8)=dens(lalias8)+dens(i1)
                     endif
                     if(ifpres) then
                        pres(lalias8)=pres(lalias8)+pres(i1)
                     endif
                     rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                     xweight(nelmadd1*nelmadd2*(ih-1)+
     *                       nelmadd2*i+j)=1.0d+00
                  endif
               enddo
               if(ict1.gt.0) then
                  itp1(lalias8)=itp
                  icr1(lalias8)=icr
                  iign(lalias8)=1+ign
                  if(ict.ne.ielmface0(i,ihextyp(ih))) then
                     imtel1=ihexclr(ih)
                     if(jt.eq.0) then
                        imt1(lalias8)=imtel1
                     else
                        jhex=jt
                        if(jhex.gt.0.and.jhex.le.numhex) then
                           imtel2=ihexclr(jt)
                        else
                           imtel2=imtel1
                        endif
                        if(imtel1.eq.imtel2) then
                           imt1(lalias8)=imtel1
                        else
                          if (iwcnt.le.20 .or. idebug.gt.0) then
                            write(wlog,9100) ih,i,lalias8,
     *                      (ihexnn1(ihexoff(ih)+
     *                              ielmface1(k,i,ihextyp(ih))),k=1,4)
                            call writloga('default',0,wlog,0,iwerr)
 9100                       format("Error in assigning node color:",
     *                            " element=",i10," face=",i3,
     *                            " node=",i3," face nodes: ",4i10)
                          endif
                          iwcnt=iwcnt+1
                          imt1(lalias8)=imtel1
                        endif
                     endif
                  endif
                  xic2(lalias8)=xic2(lalias8)/dble(ict1)
                  yic2(lalias8)=yic2(lalias8)/dble(ict1)
                  zic2(lalias8)=zic2(lalias8)/dble(ict1)
                  if(iradavg.eq.1) then
                     rad1=rad1/dble(ict1)
                     rad2=sqrt(xic2(lalias8)**2+
     *                         yic2(lalias8)**2+
     *                         zic2(lalias8)**2)
                     xic2(lalias8)=xic2(lalias8)*rad1/rad2
                     yic2(lalias8)=yic2(lalias8)*rad1/rad2
                     zic2(lalias8)=zic2(lalias8)*rad1/rad2
                  endif
                  if(ifvels) then
                     vels(1,lalias8)=vels(1,lalias8)/dble(ict)
                     vels(2,lalias8)=vels(2,lalias8)/dble(ict)
                     vels(3,lalias8)=vels(3,lalias8)/dble(ict)
                  endif
                  if(ifdens) then
                     dens(lalias8)=dens(lalias8)/dble(ict)
                  endif
                  if(ifpres) then
                     pres(lalias8)=pres(lalias8)/dble(ict)
                  endif
               endif
            enddo
         endif
      enddo
 
      if (iwcnt.gt.0) then
         write(wlog,'(i10,a)') iwcnt,
     *   ' Total errors assigning node color.'
         call writloga('default',0,wlog,0,iwerr)
      endif
 
C     Check Point
      if(idebug .gt. 0)then
         write(wlog,'(a)')'Check Point 11'
         call writloga('default',0,wlog,0,iwerr)
         call mmverify()
      endif
C
C  CALL CMO_INTERPOLATE IF POINTS HAVE BEEN ADDED TO FIX UP NEW POINTS
C  JUST WORK ON ADDED ATTRIBUTES
C
      if (ioption.eq.-1.or.
     *    ioption.eq.0.or.
     *    ioption.eq.4.or.
     *    ioption.eq.14.or.
     *    ioption.eq.18.or.
     *    ioption.eq.24) then
         length1=nelmadd1*numhex
         call cmo_interpolate(cmotet,cmotet,cuser,
     *        length1,nelmadd2,list,ilist,xweight,ier)
      endif
      length=ntet
      call mmgetblk("itdel",isubname,ipitdel,length,2,icscode)
      ntdel=0
      ntneg=0
      do i=1,ntet
         itdel(i)=0
      enddo
      do it=1,numhex
         do i=1,nelmnen(ihextyp(it))
            ihexnn1(ihexoff(it)+i)=ialiasp(ihexnn1(ihexoff(it)+i))
         enddo
      enddo
      do i=1,ntet
         do j=1,nelmnen(itettyp(i))
            i1=itet1(itetoff(i)+j)
            itet1(itetoff(i)+j)=ialiasp(i1)
            xicvol(j)=xic2(i1)
            yicvol(j)=yic2(i1)
            zicvol(j)=zic2(i1)
         enddo
C        CHECK VOLUMES for NEW XYZ
         call volume_element(itettyp(i),
     *                       xicvol,yicvol,zicvol,
     *                       voltet)
         if(voltet.lt.-xfacvol) then
            ntneg=ntneg+1
         endif
         if(voltet.le.xfacvol) then
            ntdel=ntdel+1
            itdel(i)=1
         else
            itdel(i)=0
            voltot=voltot+voltet
         endif
      enddo
      if(ntdel.gt.0) then
         write(wlog,9110) ntet,ntdel
         call writloga('default',0,wlog,0,iwerr)
 9110    format("Deleting tets: ",2i10)
      endif
      if(ntneg.gt.0) then
         write(wlog,9120) ntet,ntneg
         call writloga('default',0,wlog,0,iwerr)
 9120    format("Negative volume tets: ",2i10)
      endif
 
C DELETE TAGGED TETS
      if (iremove_vol.eq.1) then
        mbndry_old=mbndry
        mbndry_new=mbndry
        itp1_boundary=ifitprfl
        call cmo_set_info('nelements',cmotet,ntet,1,1,ierror)
        call addmesh_delete(cmotet,
     *                    mbndry_old,
     *                    mbndry_new,
     *                    itp1_boundary,
     *                    ipitdel,
     *                    ierror)
        call cmo_get_info('nelements',cmotet,ntet,lencmo,itpcmo,ierror)
      else
        write(wlog,'(a,i10)')
     *  '  WARNING: Volumes le zero not removed, nelements= ',ntet
        call writloga('default',0,wlog,0,iwerr)
      endif
      call mmrelblk("itdel",isubname,ipitdel,icscode)
 
C FIND AND UPDATE COLORS OF MERGED POINTS
      numtet=ntet
      if(icheckpt.eq.1) then
 300     continue
         do i=1,npoints
            ialiasp(i)=0
         enddo
         do it=1,numtet
            do i=1,nelmnen(itettyp(it))
               ialiasp(itet1(itetoff(it)+i))=itet1(itetoff(it)+i)
            enddo
         enddo
	 idup=0
 
c        if removing volumes, call filter to compress
         if (iremove_vol .eq. 1) then
           call filter_htt_pts(npoints,xic2,yic2,zic2,xfacdist,ialiasp)
         endif
         do i1=1,npoints
            if((ialiasp(i1) .ne. i1) .and.
     1         (ialiasp(i1) .ne. 0 ))then
              idup=idup+1
            endif
         enddo
         if(idup.le.0) then
            write(wlog,9140)
            call writloga('default',0,wlog,0,iwerr)
 9140       format("No duplicate points")
	 else
            write(wlog,9150) idup
            call writloga('default',0,wlog,0,iwerr)
 9150       format("Number of duplicate points: ",i10)
            do it=1,numtet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=ialiasp(itet1(itetoff(it)+i))
               enddo
            enddo
            do it=1,numhex
               do i=1,nelmnen(ihextyp(it))
                  ihexnn1(ihexoff(it)+i)=ialiasp(ihexnn1(ihexoff(it)+i))
               enddo
            enddo
            goto 300
         endif
      else
        write(wlog,'(a)')
     * '  WARNING icheckpt=0: Node fields may be incorrect.'
        call writloga('default',0,wlog,0,iwerr)
      endif
C
 
      call cmo_set_info('nnodes',cmotet,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmotet,numtet,1,1,ierror)
      call geniee_cmo(cmotet)
C
C
C
C FIND and REMOVE DUPLICATE POINTS
      if(iremove_dup.eq.1) then
         do i=1,npoints
            ialiasp(i)=0
         enddo
         do it=1,numtet
            do i=1,nelmnen(itettyp(it))
               ialiasp(itet1(itetoff(it)+i))=itet1(itetoff(it)+i)
            enddo
         enddo
         ict=0
         do i=1,npoints
            if(ialiasp(i).ne.0) then
               ict=ict+1
               ialiasp(i)=ict
            endif
         enddo
         if(ict.lt.npoints) then
            if(natt.gt.0) then
               do iatt=1,natt
                  len1=icharlnf(cattlen(iatt))
                  if(cattlen(iatt)(1:len1).eq.'nnodes') then
                     if(iattyp(iatt).ne.0.and.
     *                      cattinterp(iatt)(1:8).ne.'constant') then
                        call cmo_get_info(cattname(iatt),cmotet,ipatt,
     *                              lenitet,icmotype,ier)
                        if(ier.eq.0) then
                           nvals=iattrank(iatt)
                           call mmfindbk(cattname(iatt),cmotet,
     *                                   ipatt,iattlen,icscode)
                           do i=1,iattlen
                              i1=ialiasp(i)
                              if(i1.gt.0) then
                                 do j=1,nvals
                                    if(iattyp(iatt).eq.1) then
                                       iiatt(i1+j-1)=iiatt(i+j-1)
                                    else
                                       xatt(i1+j-1)=xatt(i+j-1)
                                    endif
                                 enddo
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            endif
            do i=1,npoints
               i1=ialiasp(i)
               if(i1.gt.0) then
                  imt1(i1)=imt1(i)
                  itp1(i1)=itp1(i)
                  icr1(i1)=icr1(i)
                  iign(i1)=iign(i)
                  xic2(i1)=xic2(i)
                  yic2(i1)=yic2(i)
                  zic2(i1)=zic2(i)
               endif
            enddo
            do it=1,numtet
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=ialiasp(itet1(itetoff(it)+i))
               enddo
            enddo
            do i=1,npstart
               i1=ialiasp(i)
               if(i1.gt.0) then
                  imt1hex(i1)=imt1hex(i)
                  itp1hex(i1)=itp1hex(i)
                  icr1hex(i1)=icr1hex(i)
                  xhex(i1)=xhex(i)
                  yhex(i1)=yhex(i)
                  zhex(i1)=zhex(i)
               endif
            enddo
            do it=1,numhex
               do i=1,nelmnen(ihextyp(it))
                  ihexnn1(ihexoff(it)+i)=ialiasp(ihexnn1(ihexoff(it)+i))
               enddo
            enddo
         endif
         npoints=ict
         do i=1,npoints
            ialiasp(i)=i
         enddo
      else
        write(wlog,'(a,i10)')
     *  '  WARNING: Duplicate points not removed, nnodes = ',npoints
        call writloga('default',0,wlog,0,iwerr)
      endif
C
      call mmrelblk("ktet",isubname,ipktet,icscode)
C
      do i=1,npoints
         xic(i)=xic2(i)
         yic(i)=yic2(i)
         zic(i)=zic2(i)
      enddo
      call mmrelblk("xic2",isubname,ipxic2,icscode)
      call mmrelblk("yic2",isubname,ipyic2,icscode)
      call mmrelblk("zic2",isubname,ipzic2,icscode)
C
C     .................................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
      length=npoints
      call mmgetblk('idone',isubname,ipidone,length,2,icscode)
C
      do i=1,npoints
         itp1(i)=0
         idone(i)=0
      enddo
      cmotype='tet'
      do it=1,numtet
         do i=1,nelmnef(itettyp(it))
            index=jtetoff(it)+i
            if(jtet1(index).le.0.or.jtet1(index).ge.mbndry) then
               do j=1,ielmface0(i,itettyp(it))
                  j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                  itp1(j1)=ifitprfl
               enddo
            endif
         enddo
      enddo
      do it=1,numtet
         do i=1,nelmnef(itettyp(it))
            index=jtetoff(it)+i
            if(jtet1(index).gt.0.and.jtet1(index).lt.mbndry) then
               jt=1+(jtet1(index)-1)/nef
               jf=jtet1(index)-nef*(jt-1)
               if(itetclr(it).ne.itetclr(jt)) then
                  do j=1,ielmface0(i,itettyp(it))
                     j1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     if(idone(j1).eq.0) then
                        idone(j1)=1
                        if(itp1(j1).eq.ifitpinb) then
                        elseif(itp1(j1).eq.ifitpfre) then
                        elseif(itp1(j1).eq.ifitprfl) then
                           itp1(j1)=ifitpinb
                        else
                           itp1(j1)=ifitpini
                        endif
                     endif
                  enddo
                  jtet1(jtetoff(it)+i )=jtet1(jtetoff(it)+i)+mbndry
                  jtet1(jtetoff(jt)+jf)=jtet1(jtetoff(jt)+jf)+mbndry
               endif
            endif
         enddo
      enddo
C
C     ***************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL REAL NODES.
C          IREAL1 = 1  -> Real Node.
C          IREAL1 = 0  -> Not a real node.
      length=npoints
      call mmgetblk('ireal1',isubname,ipireal1,length,2,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call cmo_get_info('itp1',cmotet,ipitp1,lenitp1,icmotype,ierror)
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierr)
         if(ierr.ne.0) call x3d_error(isubname,'unpacktp')
C
C     ***************************************************************
C
      do i=1,npoints
         ialiasp(i)=0
      enddo
      do it=1,numtet
         do i=1,nelmnen(itettyp(it))
            ialiasp(itet1(itetoff(it)+i))=itet1(itetoff(it)+i)
         enddo
      enddo
      do i1=1,npoints
         if(ireal1(i1).eq.1.and.ialiasp(i1).eq.0) then
            itp1(i1)=ifitpdud
         endif
      enddo
 9998 continue
      goto 9999
 9999 continue
      call cmo_set_info('nnodes',cmotet,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmotet,numtet,1,1,ierror)
      ipointi = 1
      ipointj = npoints
      call cmo_set_info('ipointi',cmotet,ipointi,1,1,ier)
               if (ier .ne. 0) call x3d_error(isubname,'set_ipointi')
      call cmo_set_info('ipointj',cmotet,ipointj,1,1,ier)
               if (ier .ne. 0) call x3d_error(isubname,'set_ipointj')
 
      ifatal = 0
8888  call mmrelprt(isubname,ierror)
      if (ifatal.gt.0) then
        write(wlog,'(a)')'hextotet exiting with fatal errors.'
        call writloga('default',0,wlog,0,iwerr)
      endif
 
      return
      end
c     END hextotet_hybrid()
 
*dk,cmo_getattributes
      subroutine cmo_getattributes(cmo,natt,cattlen,iattyp,iattrank,
     *                           cattinterp,cattname,ier)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         RETRIEVE TYPES LENGTHS INTERPOLATIONS AND POINTERS FOR
C            ALL USER ADDED ATTRIBUTES.
C
C      INPUT ARGUMENTS -
C
C     cmo       name of mesh object
C
C
C      OUTPUT ARGUMENTS -
C
C         natt - number of attributes
C         cattlen - array of attribute lengths
C         iatttyp - array of attribute types (1=integer,2=real)
C         iattrank - array of attribute ranks
C         cattinterp - array of attribute interpolation types
C         ier - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
C
      character*132 wlog
C
C
C#######################################################################
C
      character*(*) cmo
      integer iattyp(*),iattrank(*),natt,ier
      character*32 cattlen(*), cattinterp(*)
C
C#######################################################################
C
      character*32  ctype,  ctabinterp, cname, clength,cpers,cio,crank
      character*32 cattname (*)
      integer i, icmo, nattstrt,icmo_index, j,ilen,ityp,icscode,
     *  ierror_return,index
C
      integer  nmcmoatt
C
      integer iattlen
C
C
C#######################################################################
C
C     WARNING! MAGIC NUMBER ALERT! See comment below for why this is set
C     here. There are 65 default attributes, so to get the user-added
C     attributes, we start looking at index 66.
      data nattstrt /66/
C
C
 
      ier=0
      icmo_index=0
C
C.... Check to see if the CMO exists.
C
      call cmo_get_index(cmo,icmo,ier)
C
      if(icmo.eq.0) then
C
         ier=1
         write(wlog,'(a,a)')
     *      'mesh_object name does not exist: ',cmo
         call writloga('default',0,wlog,0,ier)
         goto 9999
C
      endif
C
C....    Get the number of attributes
c
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   icscode)
      if(icscode.ne.0)
     *   call x3d_error('hextotet_hybrid ','get number_of_attributes')
C
C  there are 65 basic attributes - user added attributes will have numbers
C  greater than 65
 
C
      natt=0
      if(nmcmoatt.lt.nattstrt) go to 9999
      natt=nmcmoatt-nattstrt+1
      do i=nattstrt,nmcmoatt
c
c  get name of attribute
c
            call cmo_get_attribute_name(cmo,i,
     *                     cname,icscode)
 
            j=i-nattstrt+1
            cattname(j)=cname
C
C.... get type      INTERPOLATION Field.
C
            call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *       clength,ctabinterp,cpers,cio,ierror_return)
            cattinterp(j)=ctabinterp
 
            if(ctype(1:2).eq.'VD'.or.ctype(1:2).eq.'vd') iattyp(j)=2
            if(ctype(1:2).eq.'VI'.or.ctype(1:2).eq.'vi') iattyp(j)=1
            if(ctype(1:1).eq.'I'.or.ctype(1:1).eq.'i') iattyp(j)=0
 
            cattlen(j)=clength
            call cmo_get_length(cname,cmo,iattlen,iattrank(j),ier)
C
      enddo
 9999 continue
      return
      end
      subroutine filter_htt_pts(nnodes,xic,yic,zic,dsmin,ialiasin)
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
C         ialiasin - THE NAMES OF EACH POINT. IF THIS IS NOT AN
C                     INDENITY THEN THIS POINT HAS BEEN FOUND
C                     TO BE CLOSE THE POINT INDEX CONTAINED IN
C                     ARRAY.
C
C      CHANGE HISTORY -
C
C        $Log: hextotet_hybrid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
c     subroutine arguments
      integer nnodes
      real*8 xic(nnodes), yic(nnodes), zic(nnodes)
      real*8 dsmin
      integer ialiasin(nnodes)
C
C#######################################################################
C
      pointer (ipncoord_bin, ncoord_bin)
      pointer (ipncoord_off, ncoord_off)
      pointer (ipialias1, ialias1)
      integer ncoord_bin(1000000), ncoord_off(1000000)
      integer ialias1(1000000)
C
      character*32 isubname
      character*182 wlog
C
      integer  i1, i2,    i, j, k,
     *        ix, iy, iz, ixpe, ixme, iype, iyme, izpe, izme,
     *        ixmin, ixmax, iymin, iymax, izmin, izmax,
     *        ncube, nx, ny, nz, nxyz, isum,
     *        ibin, ibinpe, ibinme, ibin1, ibin2
      real*8 xnoise, dsmin1, dsminsq
      integer ierror, length,  icscode, iwerr
      real*8 xmin1, ymin1, zmin1, xmax1, ymax1, zmax1,
     *       xavg1, yavg1, zavg1, dx1, dy1, dz1, distance
      real*8 alargenumber
      parameter (alargenumber=1.d+30)
C
C
C#######################################################################
C
      integer icc
      icc(i,j,k)=i+(j-1+(k-1)*(ny-1))*(nx-1)
C
C#######################################################################
C
C
      isubname='filter_htt_pts'
C
C     ******************************************************************
C     SET THE RETURN ERROR CODE.
C
      ierror = 0
C
      xnoise=1.0d-99
C
C     ******************************************************************
C
      dsminsq=dsmin**2+xnoise
C
C     ******************************************************************
C     FIND THE MAXIMUM AND MINIMUM EXTENTS OF MESHES 1 AND 2.
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
C
C  If spread is less than input minimum distance
C  use only one bin in that dimension
C
      if(dx1.lt.dsmin) then
         dx1=0.0
      else
         xmin1=xmin1-0.01*dx1
         xmax1=xmax1+0.01*dx1
         dx1=abs(xmax1-xmin1)
      endif
 
      if(dy1.lt.dsmin)  then
         dy1=0.0
      else
         ymin1=ymin1-0.01*dy1
         ymax1=ymax1+0.01*dy1
         dy1=abs(ymax1-ymin1)
      endif
      if(dz1.lt.dsmin) then
         dz1=0.0
      else
         zmin1=zmin1-0.01*dz1
         zmax1=zmax1+0.01*dz1
         dz1=abs(zmax1-zmin1)
      endif
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
      nxyz=nx*ny*nz
      length=nxyz
      call mmgetblk('ncoord_bin',isubname,ipncoord_bin,length,1,icscode)
      call mmgetblk('ncoord_off',isubname,ipncoord_off,length,1,icscode)
      do i=1,nxyz
         ncoord_bin(i)=0
         ncoord_off(i)=0
      enddo
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
         ibin=icc(ix,iy,iz)
         ibinpe=icc(ixpe,iype,izpe)
         ibinme=icc(ixme,iyme,izme)
         ncoord_bin(ibin)=ncoord_bin(ibin)+1
         if(ibinpe.ne.ibin) then
            ncoord_bin(ibinpe)=ncoord_bin(ibinpe)+1
         endif
         if(ibinme.ne.ibin) then
            ncoord_bin(ibinme)=ncoord_bin(ibinme)+1
         endif
      enddo
      isum=0
      do i=1,nxyz
         if(ncoord_bin(i).gt.0) then
            ncoord_off(i)=isum
            isum=isum+ncoord_bin(i)
         endif
         ncoord_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('ialias1',isubname,ipialias1,length,1,icscode)
      do i=1,length
         ialias1(i)=0.0
      enddo
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
         ibin=icc(ix,iy,iz)
         ibinpe=icc(ixpe,iype,izpe)
         ibinme=icc(ixme,iyme,izme)
         ncoord_bin(ibin)=ncoord_bin(ibin)+1
         ialias1(ncoord_off(ibin)+ncoord_bin(ibin))=i1
         if(ibinpe.ne.ibin) then
            ncoord_bin(ibinpe)=ncoord_bin(ibinpe)+1
            ialias1(ncoord_off(ibinpe)+ncoord_bin(ibinpe))=i1
C***              print *,"pe-bin: ",i1,ibin,ibinpe,ibinme
         endif
         if(ibinme.ne.ibin) then
            ncoord_bin(ibinme)=ncoord_bin(ibinme)+1
            ialias1(ncoord_off(ibinme)+ncoord_bin(ibinme))=i1
C***              print *,"me-bin: ",i1,ibin,ibinpe,ibinme
         endif
      enddo
      do i1=1,nnodes
       if(ialiasin(i1).eq.i1) then
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
         if(ncoord_bin(ibin1).gt.0) then
C*****      ixmin=max(1,ix-1)
C*****      ixmax=min(nx,ix+1)
C*****      iymin=max(1,iy-1)
C*****      iymax=min(ny,iy+1)
C*****      izmin=max(1,iz-1)
C*****      izmax=min(nx,iz+1)
            ixmin=ix
            ixmax=ix
            iymin=iy
            iymax=iy
            izmin=iz
            izmax=iz
            do iz=izmin,izmax
               do iy=iymin,iymax
                  do ix=ixmin,ixmax
                     ibin2=icc(ix,iy,iz)
                     do j=ncoord_off(ibin2)+1,
     *                  ncoord_off(ibin2)+ncoord_bin(ibin2)
                        i2=ialias1(j)
                        if((i1.lt.i2) .and. (i2 .gt. 0)) then
                           distance=(xic(i1)-xic(i2))**2 +
     *                              (yic(i1)-yic(i2))**2 +
     *                              (zic(i1)-zic(i2))**2
                           if(distance.lt.dsminsq) then
                              ialiasin(i1)=ialias1(j)
                              if(ibin1.ne.ibin2) then
        write(wlog,'(a)')'Needed to go outside default bin:'
        call writloga('default',0,wlog,0,iwerr)
        write(wlog,'(i8,i8,i10,i10,i10)')ibin1,ibin2,j,i1,i2
        call writloga('default',0,wlog,0,iwerr)
 
                              endif
                           endif
                        endif
                     enddo
                  enddo
               enddo
            enddo
         endif
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
