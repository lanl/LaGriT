 
      subroutine intersect_cmo(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Intersects two CMO objects, finding the line(s) of
C        intersection.
C
C     INPUT ARGUMENTS -
C
C        imsgin  - INTEGER VALUES FROM COMMAND LINE
C        xmsgin  - FLOAT VALUES FROM COMMAND LINE
C        cmsgin  - CHARACTER VALUES FROM COMMAND LINE
C        msgtype - DATA TYPE OF EACH TOKEN
C        nwds    - NUMBER OF TOKENS PASSED TO THIS ROUTINE
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - error returned (zero if no errors)
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C        $Log: intersect_cmo.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   21 Apr 2000 07:05:44   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.11   Mon Sep 21 16:50:06 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.10   Sun Jun 07 12:51:58 1998   gable
CPVCS    Modifications to check for valid input. If input cmo
CPVCS    is hybrid quad/tri or quad then convert to triangles.
CPVCS    If input cmo is invalid (i.e. line, hex, tet,...)
CPVCS    elements do nothing and return.
CPVCS
CPVCS       Rev 1.9   Mon Mar 23 11:54:38 1998   dcg
CPVCS    set ipointi and ipointj for new cmo
CPVCS
CPVCS       Rev 1.8   Fri Oct 03 11:01:52 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:52:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Wed Jul 24 17:32:30 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.5   Mon Jun 03 10:52:42 1996   jxf
CPVCS    cmo_derive added.
CPVCS
CPVCS       Rev 1.3   Tue Mar 05 12:52:50 1996   dcg
CPVCS    remove int1, icn1
CPVCS
CPVCS       Rev 1.2   12/05/95 08:25:24   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.1   11/07/95 17:19:22   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.0   06/07/95 16:52:04   dcg
CPVCS    Initial revision.
C
      implicit none
      include "local_element.h"
      include "chydro.h"
      integer iresize
      integer nplen, PC_UNUSED
      parameter (nplen=1000000)
      character*132 logmess
      character*132 dotask_command
C
C  Pointers for temporary storage for lists to pass to cmo_interpolate
       pointer(iplist,list)
       pointer(ipilist,ilist)
       pointer(ipxweight,xweight)
       integer list(3*nplen), ilist(3,3*nplen)
       real*8 xweight(3,3*nplen)
C
      integer icharlnf, mbndry
      integer ierr1,nwds,jerr,len,ity,
     *        ierror,idelete,length,icscode,i,j,k,m,numpa,numpb,
     *        kpts(2),ifoundit(2),ptnumber(2),jfoundit,nlist,
     *        it,node1,three,itindex,jtindex,
     *        jt,jf,
     *        if_all_tri_a, if_all_tri_b,
     *        if_new_cmoa,  if_new_cmob,
     *        if_cmoa_valid,if_cmob_valid
 
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds),epsilone,da1,da2,da3,db1,db2,db3,temp,
     *       dl,dm,dn,qx1a(2),qy1a(2),qz1a(2),qx1b(2),qy1b(2),qz1b(2),
     *       frac,ti1,ti2,tj1,tj2,xpt1(2),ypt1(2),zpt1(2),
     *       triarea,qx1,qy1,qz1,qx2,qy2,qz2,qx3,qy3,qz3,
     *       area1,area2,area3,area4
      character*32 cmsgin(nwds),cmoain,cmobin,cmoout,isubname,
     *  cglobal,cdefault
C
C   Definitions for incoming (existing) cmoain
C
      pointer (ipimt1a, imt1a)
      pointer (ipitp1a, itp1a)
      pointer (ipicr1a, icr1a)
      pointer (ipisn1a, isn1a)
      integer imt1a(nplen), itp1a(nplen),
     *        icr1a(nplen), isn1a(nplen)
      pointer (ipxica, xica)
      pointer (ipyica, yica)
      pointer (ipzica, zica)
      real*8 xica(nplen), yica(nplen), zica(nplen)
      pointer (ipitetclra, itetclra)
      pointer (ipitettypa, itettypa)
      pointer (ipiteta, iteta)
      pointer (ipjteta, jteta)
      integer itetclra(nplen)
      integer itettypa(nplen)
      integer iteta(3,nplen) , jteta(3,nplen)
      integer icmotypa,lenimt1a,lenitp1a,
     *        lenicr1a,lenisn1a,lenxica,lenyica,lenzica,
     *        lenitetclra,lenitettypa,leniteta,lenjteta,mbndrya,lengtha,
     *        npointsa, ntetsa
      pointer (ipbbaxmin, bbaxmin)
      pointer (ipbbaxmax, bbaxmax)
      pointer (ipbbaymin, bbaymin)
      pointer (ipbbaymax, bbaymax)
      pointer (ipbbazmin, bbazmin)
      pointer (ipbbazmax, bbazmax)
      real*8 bbaxmin(nplen),bbaxmax(nplen),bbaymin(nplen),
     *       bbaymax(nplen),bbazmin(nplen),bbazmax(nplen)
      real*8 globaxmin,globaxmax,globaymin,globaymax,
     *       globazmin,globazmax
      pointer (ipaa, aa)
      pointer (ipba, ba)
      pointer (ipca, ca)
      pointer (ipda, da)
      real*8 aa(nplen),ba(nplen),ca(nplen),da(nplen)
C
C   Definitions for incoming (existing) cmobin
C
      pointer (ipimt1b, imt1b)
      pointer (ipitp1b, itp1b)
      pointer (ipicr1b, icr1b)
      pointer (ipisn1b, isn1b)
      integer imt1b(nplen), itp1b(nplen),
     *        icr1b(nplen), isn1b(nplen)
      pointer (ipxicb, xicb)
      pointer (ipyicb, yicb)
      pointer (ipzicb, zicb)
      real*8 xicb(nplen), yicb(nplen), zicb(nplen)
      pointer (ipitetclrb, itetclrb)
      pointer (ipitettypb, itettypb)
      pointer (ipitetb, itetb)
      pointer (ipjtetb, jtetb)
      integer itetclrb(nplen)
      integer itettypb(nplen)
      integer itetb(3,nplen) , jtetb(3,nplen)
      integer icmotypb,lenimt1b,lenitp1b,
     *        lenicr1b,lenisn1b,lenxicb,lenyicb,lenzicb,
     *        lenitetclrb,lenitettypb,lenitetb,lenjtetb,mbndryb,lengthb,
     *        npointsb,ntetsb
      pointer (ipbbbxmin, bbbxmin)
      pointer (ipbbbxmax, bbbxmax)
      pointer (ipbbbymin, bbbymin)
      pointer (ipbbbymax, bbbymax)
      pointer (ipbbbzmin, bbbzmin)
      pointer (ipbbbzmax, bbbzmax)
      pointer (ipbbbnolap, bbbnolap)
      real*8 bbbxmin(nplen),bbbxmax(nplen),bbbymin(nplen),
     *       bbbymax(nplen),bbbzmin(nplen),bbbzmax(nplen)
      integer bbbnolap(nplen)
      real*8 globbxmin,globbxmax,globbymin,globbymax,
     *       globbzmin,globbzmax
      pointer (ipab, ab)
      pointer (ipbb, bb)
      pointer (ipcb, cb)
      pointer (ipdb, db)
      real*8 ab(nplen),bb(nplen),cb(nplen),db(nplen)
C
C   Definitions for outgoing (existing) cmoout
C
      pointer (ipimt1c, imt1c)
      pointer (ipitp1c, itp1c)
      pointer (ipicr1c, icr1c)
      pointer (ipisn1c, isn1c)
      integer imt1c(nplen), itp1c(nplen),
     *        icr1c(nplen), isn1c(nplen)
      pointer (ipxicc, xicc)
      pointer (ipyicc, yicc)
      pointer (ipzicc, zicc)
      real*8 xicc(nplen), yicc(nplen), zicc(nplen)
      pointer (ipitetclrc, itetclrc)
      pointer (ipitettypc, itettypc)
      pointer (ipitetoffc, itetoffc)
      pointer (ipjtetoffc, jtetoffc)
      pointer (ipitetc1, itetc1)
      pointer (ipjtetc1, jtetc1)
      integer itetclrc(nplen),itettypc(nplen),itetoffc(nplen),
     *        jtetoffc(nplen)
      integer itetc1(nplen),
     *        jtetc1(nplen)
      integer icmotypc,lenimt1c,lenitp1c,nefcmo,
     *        lenicr1c,lenisn1c,lenxicc,lenyicc,lenzicc,
     *        lenitetclrc,lenitetc1,lenjtetc1,mbndryc,npointsc,
     *        npointscmax,ntetsc,ntetscmax,lenitettypc,lenitetoffc,
     *        lenjtetoffc,lenmbndryc,ipointi,ipointj
      real*8 alargenumber
      data alargenumber/1.d+99/
C
      pointer (ipidone, idone)
      integer idone(nplen)
C
C   statement function for the area of a triangle:
C
      triarea(qx1,qy1,qz1,qx2,qy2,qz2,qx3,qy3,qz3) =
     *  0.5*sqrt(
     *  ((qz2-qz1)*(qy3-qy1)-(qy2-qy1)*(qz3-qz1)) *
     *  ((qz2-qz1)*(qy3-qy1)-(qy2-qy1)*(qz3-qz1)) +
     *  ((qx2-qx1)*(qz3-qz1)-(qz2-qz1)*(qx3-qx1)) *
     *  ((qx2-qx1)*(qz3-qz1)-(qz2-qz1)*(qx3-qx1)) +
     *  ((qy2-qy1)*(qx3-qx1)-(qx2-qx1)*(qy3-qy1)) *
     *  ((qy2-qy1)*(qx3-qx1)-(qx2-qx1)*(qy3-qy1)) )
C
C
      ierr1 = 0
      isubname = 'intersect_cmo'
      cglobal='global'
      cdefault='default'
      epsilone=1.0e-8
      PC_UNUSED = 10.0
      iresize = 0
C
C   Check for character data names for cmoain, cmobin, and cmoout.
C
      if( (msgtype(1).ne.3) .or. (msgtype(2).ne.3) .or.
     *    (msgtype(3).ne.3) ) then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Bad value for INTERFACE CMO name.  Must be character data.'
         call writloga('default',0,logmess,0,ierr1)
         return
      endif
C
      if_new_cmoa = 0
      if_new_cmob = 0
C
      cmoain = cmsgin(2)
      cmobin = cmsgin(3)
      cmoout = cmsgin(1)
C
C     Jump back into the code here if hextotet had to be called for one or
C     both of the input cmos.
C
  225 continue
C
      if_cmoa_valid = 1
      if_cmob_valid = 1
C
      if ( cmoain .eq. '-cmo-' ) call cmo_get_name(cmoain,jerr)
C
      call cmo_get_info('mbndry',cmoain,mbndrya,lengtha,icmotypa,
     *			jerr)
      call cmo_get_info('imt1',cmoain,ipimt1a,lenimt1a,icmotypa,jerr)
      call cmo_get_info('itp1',cmoain,ipitp1a,lenitp1a,icmotypa,jerr)
      call cmo_get_info('icr1',cmoain,ipicr1a,lenicr1a,icmotypa,jerr)
      call cmo_get_info('isn1',cmoain,ipisn1a,lenisn1a,icmotypa,jerr)
      call cmo_get_info('xic',cmoain,ipxica,lenxica,icmotypa,jerr)
      call cmo_get_info('yic',cmoain,ipyica,lenyica,icmotypa,jerr)
      call cmo_get_info('zic',cmoain,ipzica,lenzica,icmotypa,jerr)
      call cmo_get_info('itetclr',cmoain,ipitetclra,lenitetclra,
     *			icmotypa,jerr)
      call cmo_get_info('itettyp',cmoain,ipitettypa,lenitettypa,
     *			icmotypa,jerr)
      call cmo_get_info('itet',cmoain,ipiteta,leniteta,icmotypa,jerr)
      call cmo_get_info('jtet',cmoain,ipjteta,lenjteta,icmotypa,jerr)
C
      if ( cmobin .eq. '-cmo-' ) call cmo_get_name(cmobin,jerr)
C
      call cmo_get_info('mbndry',cmobin,mbndryb,lengthb,icmotypb,
     *			jerr)
      call cmo_get_info('imt1',cmobin,ipimt1b,lenimt1b,icmotypb,jerr)
      call cmo_get_info('itp1',cmobin,ipitp1b,lenitp1b,icmotypb,jerr)
      call cmo_get_info('icr1',cmobin,ipicr1b,lenicr1b,icmotypb,jerr)
      call cmo_get_info('isn1',cmobin,ipisn1b,lenisn1b,icmotypb,jerr)
      call cmo_get_info('xic',cmobin,ipxicb,lenxicb,icmotypb,jerr)
      call cmo_get_info('yic',cmobin,ipyicb,lenyicb,icmotypb,jerr)
      call cmo_get_info('zic',cmobin,ipzicb,lenzicb,icmotypb,jerr)
      call cmo_get_info('itetclr',cmobin,ipitetclrb,lenitetclrb,
     *			icmotypb,jerr)
      call cmo_get_info('itettyp',cmobin,ipitettypb,lenitettypb,
     *			icmotypb,jerr)
      call cmo_get_info('itet',cmobin,ipitetb,lenitetb,icmotypb,jerr)
      call cmo_get_info('jtet',cmobin,ipjtetb,lenjtetb,icmotypb,jerr)
C
      call cmo_get_info('nnodes',cmoain,npointsa,len,ity,ierror)
      call cmo_get_info('nelements',cmoain,ntetsa,len,ity,ierror)
C
      call cmo_get_info('nnodes',cmobin,npointsb,len,ity,ierror)
      call cmo_get_info('nelements',cmobin,ntetsb,len,ity,ierror)
C
      if(cmoout .eq. '-def-') then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Need a name for the output mesh object.'
         call writloga('default',0,logmess,0,ierr1)
         return
      endif
C
C   Check that both input cmo's are all triangles.
C   If they are hybrid (quad,tri) or all quad use hextotet
C   to create triangle cmo's for intersection and release
C   the triangle cmo's before exiting subroutine.
C
C   Also test if cmo is something other than quads or triangles,
C   If invalid input, report that and kick out of the intersect command.
C
      if_all_tri_a = 0
      do i = 1,ntetsa
         if(itettypa(i) .ne. ifelmtri)then
            if_all_tri_a = if_all_tri_a + 1
            if(itettypa(i) .ne. ifelmqud)if_cmoa_valid = -1
         endif
      enddo
      if(if_all_tri_a .ne. 0)then
         write(logmess,'(a)')
     *     'Notice: Input cmo_a is not all triangles.'
         call writloga('default',0,logmess,0,ierr1)
      endif
      if_all_tri_b = 0
      do i = 1,ntetsb
         if(itettypb(i) .ne. ifelmtri)then
            if_all_tri_b = if_all_tri_b + 1
            if(itettypb(i) .ne. ifelmqud)if_cmob_valid = -1
         endif
      enddo
      if(if_all_tri_b .ne. 0)then
         write(logmess,'(a)')
     *     'Notice: Input cmo_b is not all triangles.'
         call writloga('default',0,logmess,0,ierr1)
      endif
      if((if_all_tri_a .ne. 0).or.(if_all_tri_b .ne. 0))then
C
C     Kick out if something other than quads and/or triangles.
C
         if((if_cmoa_valid .eq. -1).or.(if_cmob_valid .eq. -1))then
            if(if_cmoa_valid .eq. -1)then
              write(logmess,'(a)')
     *       'ERROR: Invalid input, first cmo not all quad and/or tri.'
              call writloga('default',0,logmess,0,ierr1)
            endif
            if(if_cmob_valid .eq. -1)then
              write(logmess,'(a)')
     *       'ERROR: Invalid input, second cmo not all quad and/or tri.'
              call writloga('default',0,logmess,0,ierr1)
            endif
            write(logmess,'(a)')
     *      'ERROR: No action or output from INTERSECT command'
            call writloga('default',0,logmess,0,ierr1)
            return
          endif
C
         write(logmess,'(a)')
     *     'Notice: Calling hextotet to convert quads to triangles'
         call writloga('default',0,logmess,0,ierr1)
         if(if_all_tri_a .ne. 0)then
            dotask_command =
     *           'hextotet/2/cmo_tri_a/'//cmoain(1:icharlnf(cmoain))//
     *            ' ; finish '
            call dotaskx3d(dotask_command,ierror)
            cmoain = 'cmo_tri_a'
            if_new_cmoa = 1
         endif
         if(if_all_tri_b .ne. 0)then
            dotask_command =
     *           'hextotet/2/cmo_tri_b/'//cmobin(1:icharlnf(cmobin))//
     *            ' ; finish '
            call dotaskx3d(dotask_command,ierror)
            if_new_cmob = 1
            cmobin = 'cmo_tri_b'
         endif
 
C
C         Jump back up to the top and refresh all the pointers to use
C         the new mesh object created by hextotet.
         go to 225
      endif
C
C   Create the new cmo.
C
      call cmo_exist(cmoout,ierror)
C
C   ierror.eq.0 means that the cmo already exists.
C
      if(ierror.eq.0) call cmo_release(cmoout,idelete)
      call cmo_derive(cmoout,cmoain,ierror)
C
C   Make room for twice the existing number of points and tets.
C   Will need to check later to ensure that this is big enough,
C   as points are added.
C
      npointscmax = npointsa + npointsb
      ntetscmax = ntetsa + ntetsb
      call cmo_set_info('nnodes',cmoout,npointscmax,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetscmax,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,1,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,3,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,2,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,2,1,1,ierror)
C
      call cmo_newlen(cmoout,ierror)
C
      call cmo_get_info('imt1',cmoout,ipimt1c,lenimt1c,icmotypc,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1c,lenitp1c,icmotypc,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1c,lenicr1c,icmotypc,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1c,lenisn1c,icmotypc,ierror)
      call cmo_get_info('xic',cmoout,ipxicc,lenxicc,icmotypc,ierror)
      call cmo_get_info('yic',cmoout,ipyicc,lenyicc,icmotypc,ierror)
      call cmo_get_info('zic',cmoout,ipzicc,lenzicc,icmotypc,ierror)
      call cmo_get_info('mbndry',cmoout,mbndryc,lenmbndryc,icmotypc,
     *                  ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclrc,lenitetclrc,
     *			icmotypc,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypc,lenitettypc,
     *			icmotypc,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffc,lenitetoffc,
     *			icmotypc,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffc,lenjtetoffc,
     *			icmotypc,ierror)
      call cmo_get_info('itet',cmoout,ipitetc1,lenitetc1,icmotypc,
     *                  ierror)
      call cmo_get_info('jtet',cmoout,ipjtetc1,lenjtetc1,icmotypc,
     *                  ierror)
C
C   Get some memory to hold bounding boxes of each tet in cmoain and
C   cmobin.  Then fill in the bounding box info.
C
      call mmgetblk('bbaxmin',isubname,ipbbaxmin,ntetsa,2,icscode)
      call mmgetblk('bbaxmax',isubname,ipbbaxmax,ntetsa,2,icscode)
      call mmgetblk('bbaymin',isubname,ipbbaymin,ntetsa,2,icscode)
      call mmgetblk('bbaymax',isubname,ipbbaymax,ntetsa,2,icscode)
      call mmgetblk('bbazmin',isubname,ipbbazmin,ntetsa,2,icscode)
      call mmgetblk('bbazmax',isubname,ipbbazmax,ntetsa,2,icscode)
      call mmgetblk('bbbxmin',isubname,ipbbbxmin,ntetsb,2,icscode)
      call mmgetblk('bbbxmax',isubname,ipbbbxmax,ntetsb,2,icscode)
      call mmgetblk('bbbymin',isubname,ipbbbymin,ntetsb,2,icscode)
      call mmgetblk('bbbymax',isubname,ipbbbymax,ntetsb,2,icscode)
      call mmgetblk('bbbzmin',isubname,ipbbbzmin,ntetsb,2,icscode)
      call mmgetblk('bbbzmax',isubname,ipbbbzmax,ntetsb,2,icscode)
      call mmgetblk('aa',isubname,ipaa,ntetsa,2,icscode)
      call mmgetblk('ba',isubname,ipba,ntetsa,2,icscode)
      call mmgetblk('ca',isubname,ipca,ntetsa,2,icscode)
      call mmgetblk('da',isubname,ipda,ntetsa,2,icscode)
      call mmgetblk('ab',isubname,ipab,ntetsb,2,icscode)
      call mmgetblk('bb',isubname,ipbb,ntetsb,2,icscode)
      call mmgetblk('cb',isubname,ipcb,ntetsb,2,icscode)
      call mmgetblk('db',isubname,ipdb,ntetsb,2,icscode)
 
C
C
C  get working space for interpolation
C
      call mmgetblk('list',isubname,iplist,3*(npointsa+npointsb),
     *              2,icscode)
      call mmgetblk('ilist',isubname,ipilist,3*(npointsa+npointsb),
     *              2,icscode)
      call mmgetblk('xweight',isubname,ipxweight,
     *              3*(npointsa+npointsb),2,icscode)
C
      npointsc = 0
      ntetsc = 0
      itindex = 0
      jtindex = 0
      nlist = 0
C
      globaxmin = alargenumber
      globaymin = globaxmin
      globazmin = globaxmin
      globaxmax = -globaxmin
      globaymax = -globaxmin
      globazmax = -globaxmin
      do i=1,ntetsa
         aa(i) = PC_UNUSED
         bbaxmin(i) = min(xica(iteta(1,i)),xica(iteta(2,i)),
     *                      xica(iteta(3,i)))
         bbaxmax(i) = max(xica(iteta(1,i)),xica(iteta(2,i)),
     *                      xica(iteta(3,i)))
         if ( (bbaxmax(i) - bbaxmin(i)) .lt. epsilone ) then
            bbaxmax(i) = bbaxmax(i) + epsilone
            bbaxmin(i) = bbaxmin(i) - epsilone
         endif
         bbaymin(i) = min(yica(iteta(1,i)),yica(iteta(2,i)),
     *                      yica(iteta(3,i)))
         bbaymax(i) = max(yica(iteta(1,i)),yica(iteta(2,i)),
     *                      yica(iteta(3,i)))
         if ( (bbaymax(i) - bbaymin(i)) .lt. epsilone ) then
            bbaymax(i) = bbaymax(i) + epsilone
            bbaymin(i) = bbaymin(i) - epsilone
         endif
         bbazmin(i) = min(zica(iteta(1,i)),zica(iteta(2,i)),
     *                      zica(iteta(3,i)))
         bbazmax(i) = max(zica(iteta(1,i)),zica(iteta(2,i)),
     *                      zica(iteta(3,i)))
         if ( (bbazmax(i) - bbazmin(i)) .lt. epsilone ) then
            bbazmax(i) = bbazmax(i) + epsilone
            bbazmin(i) = bbazmin(i) - epsilone
         endif
         if ( bbaxmin(i) .lt. globaxmin ) globaxmin = bbaxmin(i)
         if ( bbaymin(i) .lt. globaymin ) globaymin = bbaymin(i)
         if ( bbazmin(i) .lt. globazmin ) globazmin = bbazmin(i)
         if ( bbaxmax(i) .gt. globaxmax ) globaxmax = bbaxmax(i)
         if ( bbaymax(i) .gt. globaymax ) globaymax = bbaymax(i)
         if ( bbazmax(i) .gt. globazmax ) globazmax = bbazmax(i)
      enddo
C
      globbxmin = alargenumber
      globbymin = globbxmin
      globbzmin = globbxmin
      globbxmax = -globbxmin
      globbymax = -globbxmin
      globbzmax = -globbxmin
      do i=1,ntetsb
         ab(i) = PC_UNUSED
         bbbxmin(i) = min(xicb(itetb(1,i)),xicb(itetb(2,i)),
     *                      xicb(itetb(3,i)))
         bbbxmax(i) = max(xicb(itetb(1,i)),xicb(itetb(2,i)),
     *                      xicb(itetb(3,i)))
         if ( (bbbxmax(i) - bbbxmin(i)) .lt. epsilone ) then
            bbbxmax(i) = bbbxmax(i) + epsilone
            bbbxmin(i) = bbbxmin(i) - epsilone
         endif
         bbbymin(i) = min(yicb(itetb(1,i)),yicb(itetb(2,i)),
     *                      yicb(itetb(3,i)))
         bbbymax(i) = max(yicb(itetb(1,i)),yicb(itetb(2,i)),
     *                      yicb(itetb(3,i)))
         if ( (bbbymax(i) - bbbymin(i)) .lt. epsilone ) then
            bbbymax(i) = bbbymax(i) + epsilone
            bbbymin(i) = bbbymin(i) - epsilone
         endif
         bbbzmin(i) = min(zicb(itetb(1,i)),zicb(itetb(2,i)),
     *                      zicb(itetb(3,i)))
         bbbzmax(i) = max(zicb(itetb(1,i)),zicb(itetb(2,i)),
     *                      zicb(itetb(3,i)))
         if ( (bbbzmax(i) - bbbzmin(i)) .lt. epsilone ) then
            bbbzmax(i) = bbbzmax(i) + epsilone
            bbbzmin(i) = bbbzmin(i) - epsilone
         endif
         if ( bbbxmin(i) .lt. globbxmin ) globbxmin = bbbxmin(i)
         if ( bbbymin(i) .lt. globbymin ) globbymin = bbbymin(i)
         if ( bbbzmin(i) .lt. globbzmin ) globbzmin = bbbzmin(i)
         if ( bbbxmax(i) .gt. globbxmax ) globbxmax = bbbxmax(i)
         if ( bbbymax(i) .gt. globbymax ) globbymax = bbbymax(i)
         if ( bbbzmax(i) .gt. globbzmax ) globbzmax = bbbzmax(i)
      enddo
C
C   Now get a list of cmobin tets that have bboxes that lie outside
C   the global bbox for cmoain.  These tets cannot possibly intersect
C   any tets in cmoain.
C
      call mmgetblk('bbbnolap',isubname,ipbbbnolap,ntetsb,2,icscode)
C
      do i=1,ntetsb
         bbbnolap(i) = 0
         if ( (globaxmax .lt. bbbxmin(i)) .or.
     *        (bbbxmax(i) .lt. globaxmin) .or.
     *        (globaymax .lt. bbbymin(i)) .or.
     *        (bbbymax(i) .lt. globaymin) .or.
     *        (globazmax .lt. bbbzmin(i)) .or.
     *        (bbbzmax(i) .lt. globazmin) ) bbbnolap(i) = 1
      enddo
C
C   Compare extents of all tet faces in cmoain against extents of all
C   tet faces in cmobin that have bbnolap .eq. 0 to find potentially
C   intersecting faces.
C
      do i=1,ntetsa
C   First see if the extent of this cmoain face overlaps the global
C   extent of cmobin.
C
         if ( (globbxmax .lt. bbaxmin(i)) .or.
     *        (bbaxmax(i) .lt. globbxmin) .or.
     *        (globbymax .lt. bbaymin(i)) .or.
     *        (bbaymax(i) .lt. globbymin) .or.
     *        (globbzmax .lt. bbazmin(i)) .or.
     *        (bbazmax(i) .lt. globbzmin) ) then
C
C   No overlap so do nothing.
C
         else
C
C   Potential overlap, so test against each face in cmobin.
C
            do j=1,ntetsb
               if ( bbbnolap(j) .eq.0 ) then
C
C   Do these faces have overlapping extents?
C
                  if ( (bbbxmax(j) .lt. bbaxmin(i)) .or.
     *                 (bbaxmax(i) .lt. bbbxmin(j)) .or.
     *                 (bbbymax(j) .lt. bbaymin(i)) .or.
     *                 (bbaymax(i) .lt. bbbymin(j)) .or.
     *                 (bbbzmax(j) .lt. bbazmin(i)) .or.
     *                 (bbazmax(i) .lt. bbbzmin(j)) ) then
C
C   No overlap so do nothing.
C
                  else
C
C   Do further testing.  Does face "a" cross the plane of face "b"?
C   Find the signed distance of each vertex of face "a" to the plane
C   of face "b".  If they all have the same sign, they are all on the
C   same side and thus there can be no intersection of these two faces.
C
C   Get the plane coefficients for face "b" if they haven't been
C   gotten already.  If the value of the first coefficient is
C   PC_UNUSED, we need to get them.  Note that PC_UNUSED .eq. 10.0
C   while the "a" coefficient will always be .le. 1.0 since it
C   represents the x coefficient of the unit normal.  The factor 0.1
C   is subtracted so that the .gt. test will never fail on roundoff
C   error.
C
                     if ( ab(j) .gt. (PC_UNUSED - 0.1) )
     *                  call get_plane_coeffs(
     *           xicb(itetb(1,j)),yicb(itetb(1,j)),zicb(itetb(1,j)),
     *           xicb(itetb(2,j)),yicb(itetb(2,j)),zicb(itetb(2,j)),
     *           xicb(itetb(3,j)),yicb(itetb(3,j)),zicb(itetb(3,j)),
     *           ab(j),bb(j),cb(j),db(j))
                     da1 = ab(j)*xica(iteta(1,i)) +
     *                     bb(j)*yica(iteta(1,i)) +
     *                     cb(j)*zica(iteta(1,i)) + db(j)
C
                     da2 = ab(j)*xica(iteta(2,i)) +
     *                     bb(j)*yica(iteta(2,i)) +
     *                     cb(j)*zica(iteta(2,i)) + db(j)
C
                     da3 = ab(j)*xica(iteta(3,i)) +
     *                     bb(j)*yica(iteta(3,i)) +
     *                     cb(j)*zica(iteta(3,i)) + db(j)
C
                     if ( ( (da1 .gt. epsilone) .and.
     *                      (da2 .gt. epsilone) .and.
     *                      (da3 .gt. epsilone) ) .or.
     *                    ( (da1 .lt. -epsilone) .and.
     *                      (da2 .lt. -epsilone) .and.
     *                      (da3 .lt. -epsilone) ) ) then
C
C   Do nothing -- all 3 vertices are on the same side.
C
                     else if ( (abs(da1) .lt.epsilone) .and.
     *                         (abs(da2) .lt.epsilone) .and.
     *                         (abs(da3) .lt.epsilone) ) then
C
C   Do nothing -- the triangles are coplanar.
C
                     else
C
C   Now do the same thing with face "b" vis-a-vis face "a".
C
                        if ( aa(i) .gt. (PC_UNUSED - 0.1) )
     *                  call get_plane_coeffs(
     *           xica(iteta(1,i)),yica(iteta(1,i)),zica(iteta(1,i)),
     *           xica(iteta(2,i)),yica(iteta(2,i)),zica(iteta(2,i)),
     *           xica(iteta(3,i)),yica(iteta(3,i)),zica(iteta(3,i)),
     *           aa(i),ba(i),ca(i),da(i))
                        db1 = aa(i)*xicb(itetb(1,j)) +
     *                        ba(i)*yicb(itetb(1,j)) +
     *                        ca(i)*zicb(itetb(1,j)) + da(i)
C
                        db2 = aa(i)*xicb(itetb(2,j)) +
     *                        ba(i)*yicb(itetb(2,j)) +
     *                        ca(i)*zicb(itetb(2,j)) + da(i)
C
                        db3 = aa(i)*xicb(itetb(3,j)) +
     *                        ba(i)*yicb(itetb(3,j)) +
     *                        ca(i)*zicb(itetb(3,j)) + da(i)
C
                        if ( ( (db1 .gt. epsilone) .and.
     *                         (db2 .gt. epsilone) .and.
     *                         (db3 .gt. epsilone) ) .or.
     *                       ( (db1 .lt. -epsilone) .and.
     *                         (db2 .lt. -epsilone) .and.
     *                         (db3 .lt. -epsilone) ) ) then
C
C   Do nothing -- all 3 vertices are on the same side.
C
                        else
C
C   Find the direction cosines of the line of intersection of the
C   planes of face "a" and face "b".
C
                           dl = ba(i)*cb(j) - ca(i)*bb(j)
                           dm = ca(i)*ab(j) - aa(i)*cb(j)
                           dn = aa(i)*bb(j) - ba(i)*ab(j)
                           temp = sqrt(dl*dl + dm*dm + dn*dn)
                           dl = dl/temp
                           dm = dm/temp
                           dn = dn/temp
C
C   Now get the points where wach face's edges cross the plane of
C   the other face.  There will be two such points for each face.
C   First test to see if the crossing is at a vertex.
C
C   face "a":
C
                           numpa = 0
                           if ( abs(da1) .lt. epsilone ) then
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(1,i))
                              qy1a(numpa) = yica(iteta(1,i))
                              qz1a(numpa) = zica(iteta(1,i))
                           endif
                           if ( abs(da2) .lt. epsilone ) then
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(2,i))
                              qy1a(numpa) = yica(iteta(2,i))
                              qz1a(numpa) = zica(iteta(2,i))
                           endif
                           if ( abs(da3) .lt. epsilone ) then
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(3,i))
                              qy1a(numpa) = yica(iteta(3,i))
                              qz1a(numpa) = zica(iteta(3,i))
                           endif
C
C   If fewer than two intersections have been found, test each edge
C   for crossing, until two have been found.
C
                           if ( (numpa .lt. 2) .and.
     *                          ( ( (da1 .ge. epsilone) .and.
     *                              (da2 .le. -epsilone) ) .or.
     *                            ( (da1 .le. -epsilone) .and.
     *                              (da2 .ge. epsilone) ) ) ) then
                              frac = abs(da1)/(abs(da1) + abs(da2))
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(1,i)) +
     *                     frac*(xica(iteta(2,i))-xica(iteta(1,i)))
                              qy1a(numpa) = yica(iteta(1,i)) +
     *                     frac*(yica(iteta(2,i))-yica(iteta(1,i)))
                              qz1a(numpa) = zica(iteta(1,i)) +
     *                     frac*(zica(iteta(2,i))-zica(iteta(1,i)))
                           endif
                           if ( (numpa .lt. 2) .and.
     *                          ( ( (da1 .ge. epsilone) .and.
     *                              (da3 .le. -epsilone) ) .or.
     *                            ( (da1 .le. -epsilone) .and.
     *                              (da3 .ge. epsilone) ) ) ) then
                              frac = abs(da1)/(abs(da1) + abs(da3))
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(1,i)) +
     *                     frac*(xica(iteta(3,i))-xica(iteta(1,i)))
                              qy1a(numpa) = yica(iteta(1,i)) +
     *                     frac*(yica(iteta(3,i))-yica(iteta(1,i)))
                              qz1a(numpa) = zica(iteta(1,i)) +
     *                     frac*(zica(iteta(3,i))-zica(iteta(1,i)))
                           endif
                           if ( (numpa .lt. 2) .and.
     *                          ( ( (da2 .ge. epsilone) .and.
     *                              (da3 .le. -epsilone) ) .or.
     *                            ( (da2 .le. -epsilone) .and.
     *                              (da3 .ge. epsilone) ) ) ) then
                              frac = abs(da2)/(abs(da2) + abs(da3))
                              numpa = numpa + 1
                              qx1a(numpa) = xica(iteta(2,i)) +
     *                     frac*(xica(iteta(3,i))-xica(iteta(2,i)))
                              qy1a(numpa) = yica(iteta(2,i)) +
     *                     frac*(yica(iteta(3,i))-yica(iteta(2,i)))
                              qz1a(numpa) = zica(iteta(2,i)) +
     *                     frac*(zica(iteta(3,i))-zica(iteta(2,i)))
                           endif
C
C   Now do the same for face "b":
C
                           numpb = 0
                           if ( abs(db1) .lt. epsilone ) then
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(1,j))
                              qy1b(numpb) = yicb(itetb(1,j))
                              qz1b(numpb) = zicb(itetb(1,j))
                           endif
                           if ( abs(db2) .lt. epsilone ) then
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(2,j))
                              qy1b(numpb) = yicb(itetb(2,j))
                              qz1b(numpb) = zicb(itetb(2,j))
                           endif
                           if ( abs(db3) .lt. epsilone ) then
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(3,j))
                              qy1b(numpb) = yicb(itetb(3,j))
                              qz1b(numpb) = zicb(itetb(3,j))
                           endif
C
C   If fewer than two intersections have been found, test each edge
C   for crossing, until two have been found.
C
                           if ( (numpb .lt. 2) .and.
     *                          ( ( (db1 .ge. epsilone) .and.
     *                              (db2 .le. -epsilone) ) .or.
     *                            ( (db1 .le. -epsilone) .and.
     *                              (db2 .ge. epsilone) ) ) ) then
                              frac = abs(db1)/(abs(db1) + abs(db2))
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(1,j)) +
     *                     frac*(xicb(itetb(2,j))-xicb(itetb(1,j)))
                              qy1b(numpb) = yicb(itetb(1,j)) +
     *                     frac*(yicb(itetb(2,j))-yicb(itetb(1,j)))
                              qz1b(numpb) = zicb(itetb(1,j)) +
     *                     frac*(zicb(itetb(2,j))-zicb(itetb(1,j)))
                           endif
                           if ( (numpb .lt. 2) .and.
     *                          ( ( (db1 .ge. epsilone) .and.
     *                              (db3 .le. -epsilone) ) .or.
     *                            ( (db1 .le. -epsilone) .and.
     *                              (db3 .ge. epsilone) ) ) ) then
                              frac = abs(db1)/(abs(db1) + abs(db3))
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(1,j)) +
     *                     frac*(xicb(itetb(3,j))-xicb(itetb(1,j)))
                              qy1b(numpb) = yicb(itetb(1,j)) +
     *                     frac*(yicb(itetb(3,j))-yicb(itetb(1,j)))
                              qz1b(numpb) = zicb(itetb(1,j)) +
     *                     frac*(zicb(itetb(3,j))-zicb(itetb(1,j)))
                           endif
                           if ( (numpb .lt. 2) .and.
     *                          ( ( (db2 .ge. epsilone) .and.
     *                              (db3 .le. -epsilone) ) .or.
     *                            ( (db2 .le. -epsilone) .and.
     *                              (db3 .ge. epsilone) ) ) ) then
                              frac = abs(db2)/(abs(db2) + abs(db3))
                              numpb = numpb + 1
                              qx1b(numpb) = xicb(itetb(2,j)) +
     *                     frac*(xicb(itetb(3,j))-xicb(itetb(2,j)))
                              qy1b(numpb) = yicb(itetb(2,j)) +
     *                     frac*(yicb(itetb(3,j))-yicb(itetb(2,j)))
                              qz1b(numpb) = zicb(itetb(2,j)) +
     *                     frac*(zicb(itetb(3,j))-zicb(itetb(2,j)))
                           endif
C
C   Order the intersections of face "a" and face "b" along the line of
C   intersection.  Use a non-zero direction cosine to do this.  Using
C   the parametric equations for a line, x = qx1 + dl*t, y = qy1 + dm*t,
C   z = qz1 + dn*t, find the distance, t, for each of the two points
C   per face, and then sort them in ascending values of t.
C
C   First, find a direction cosine appreciably .gt. 0.0 in magnitude.
C
                           ti1 = 0.0
                           if ( abs(dl) .gt. 0.3 ) then
                              if ( numpa .eq. 1 ) then
                                 ti2 = 0.0
                              else
                                 ti2 = (qx1a(2) - qx1a(1))/dl
                              endif
                              tj1 = (qx1b(1) - qx1a(1))/dl
                              if ( numpb .eq. 1 ) then
                                 tj2 = tj1
                              else
                                 tj2 = (qx1b(2) - qx1a(1))/dl
                              endif
                           else if ( abs(dm) .gt. 0.3 ) then
                              if ( numpa .eq. 1 ) then
                                 ti2 = 0.0
                              else
                                 ti2 = (qy1a(2) - qy1a(1))/dm
                              endif
                              tj1 = (qy1b(1) - qy1a(1))/dm
                              if ( numpb .eq. 1 ) then
                                 tj2 = tj1
                              else
                                 tj2 = (qy1b(2) - qy1a(1))/dm
                              endif
                           else
                              if ( numpa .eq. 1 ) then
                                 ti2 = 0.0
                              else
                                 ti2 = (qz1a(2) - qz1a(1))/dn
                              endif
                              tj1 = (qz1b(1) - qz1a(1))/dn
                              if ( numpb .eq. 1 ) then
                                 tj2 = tj1
                              else
                                 tj2 = (qz1b(2) - qz1a(1))/dn
                              endif
                           endif
C
C   Sort the ti's and then the tj's.
C
                           if ( ti2 .lt. ti1 ) then
                              temp = ti1
                              ti1 = ti2
                              ti2 = temp
                              temp = qx1a(1)
                              qx1a(1) = qx1a(2)
                              qx1a(2) = temp
                              temp = qy1a(1)
                              qy1a(1) = qy1a(2)
                              qy1a(2) = temp
                              temp = qz1a(1)
                              qz1a(1) = qz1a(2)
                              qz1a(2) = temp
                           endif
                           if ( tj2 .lt. tj1 ) then
                              temp = tj1
                              tj1 = tj2
                              tj2 = temp
                              temp = qx1b(1)
                              qx1b(1) = qx1b(2)
                              qx1b(2) = temp
                              temp = qy1b(1)
                              qy1b(1) = qy1b(2)
                              qy1b(2) = temp
                              temp = qz1b(1)
                              qz1b(1) = qz1b(2)
                              qz1b(2) = temp
                           endif
C
                           kpts(1) = 0
                           if ( (abs(ti1-ti2) .gt.epsilone) .and.
     *                          (abs(tj1-tj2) .gt.epsilone) )
     *                        call overlap(ti1,ti2,tj1,tj2,kpts)
C
C   kpts(1) will be either 1 or 3.  kpts(2) will be either 2 or 4.
C
                           if ( kpts(1) .ne. 0 ) then
                               if ( kpts(1) .eq. 1 ) then
                                 xpt1(1) = qx1a(1)
                                 ypt1(1) = qy1a(1)
                                 zpt1(1) = qz1a(1)
                              else
                                 xpt1(1) = qx1b(1)
                                 ypt1(1) = qy1b(1)
                                 zpt1(1) = qz1b(1)
                              endif
                              if ( kpts(2) .eq. 2 ) then
                                 xpt1(2) = qx1a(2)
                                 ypt1(2) = qy1a(2)
                                 zpt1(2) = qz1a(2)
                              else
                                 xpt1(2) = qx1b(2)
                                 ypt1(2) = qy1b(2)
                                 zpt1(2) = qz1b(2)
                              endif
C
C   Now look to see if either or both of these points need to be
C   added to the point list for the output CMO.
C
                              do k=1,2
                                 ifoundit(k) = 0
                                 m = 1
                                 do while ( (ifoundit(k) .eq. 0) .and.
     *                                      (m .lt. (npointsc + 1)) )
                                    if ( (abs(xicc(m)-xpt1(k)) .lt.
     *                                    epsilone) .and.
     *                                   (abs(yicc(m)-ypt1(k)) .lt.
     *                                    epsilone) .and.
     *                                   (abs(zicc(m)-zpt1(k)) .lt.
     *                                    epsilone) ) then
                                       ifoundit(k) = 1
                                       ptnumber(k) = m
C
C   If the found point has a different material number than the
C   current point, it is an interface point.  Set the itp1 to 2.
C
                                       if ( imt1c(m) .ne.
     *                                      imt1a(iteta(1,i)) )
     *                                   itp1c(m) = 2
                                    endif
                                    m = m + 1
                                 enddo
                                 if ( ifoundit(k) .eq. 0 ) then
                                    npointsc = npointsc + 1
                                    xicc(npointsc) = xpt1(k)
                                    yicc(npointsc) = ypt1(k)
                                    zicc(npointsc) = zpt1(k)
                                    icr1c(npointsc) = 0
                                    isn1c(npointsc) = 0
                                    imt1c(npointsc) = imt1a(iteta(1,i))
                                    itp1c(npointsc) = 0
                                    ptnumber(k) = npointsc
C
C   Interpolate the node quantities.
C
                                   area1 = triarea(
     *            xicc(npointsc),yicc(npointsc),zicc(npointsc),
     *            xica(iteta(2,i)),yica(iteta(2,i)),zica(iteta(2,i)),
     *            xica(iteta(3,i)),yica(iteta(3,i)),zica(iteta(3,i)))
                                   area2 = triarea(
     *            xicc(npointsc),yicc(npointsc),zicc(npointsc),
     *            xica(iteta(1,i)),yica(iteta(1,i)),zica(iteta(1,i)),
     *            xica(iteta(3,i)),yica(iteta(3,i)),zica(iteta(3,i)))
                                   area3 = triarea(
     *            xicc(npointsc),yicc(npointsc),zicc(npointsc),
     *            xica(iteta(1,i)),yica(iteta(1,i)),zica(iteta(1,i)),
     *            xica(iteta(2,i)),yica(iteta(2,i)),zica(iteta(2,i)))
                                   area4 = area1 + area2 + area3
                                   area1 = area1/area4
                                   area2 = area2/area4
                                   area3 = area3/area4
C
C  Put newpoints and neighbors and weights into arrays
C  for later call to cmo_interpolate
C
                                   nlist=nlist+1
                                   list(nlist)=npointsc
                                   ilist(1,nlist)=iteta(1,i)
                                   ilist(2,nlist)=iteta(2,i)
                                   ilist(3,nlist)=iteta(3,i)
                                   xweight(1,nlist)=area1
                                   xweight(2,nlist)=area2
                                   xweight(3,nlist)=area3
                                     endif
C
                              enddo
C
C   Add a new tet if needed.  If either point has ifoundit .eq. 0
C   then this has to be a new tet.
C
                              if ( (ifoundit(1) .eq. 0) .or.
     *                             (ifoundit(2) .eq. 0) ) then
                                 ntetsc = ntetsc + 1
               itetoffc(ntetsc) = itindex
               jtetoffc(ntetsc) = jtindex
               itindex = itindex + nelmnen(ifelmlin)
               jtindex = jtindex + nelmnef(ifelmlin)
                                 itetc1(2*(ntetsc-1)+1) = ptnumber(1)
                                 itetc1(2*(ntetsc-1)+2) = ptnumber(2)
                                 itetclrc(ntetsc) = itetclra(i)
                                 itettypc(ntetsc) = ifelmlin
                              else
C
C   Check to see if the tet already exists.
C
                                 jfoundit = 0
                                 m = 1
                                 do while ( (jfoundit .eq. 0) .and.
     *                                   (m .lt. (ntetsc + 1)) )
                             if (((itetc1(2*(m-1)+1) .eq. ptnumber(1))
     *                                 .and.
     *                            (itetc1(2*(m-1)+2) .eq. ptnumber(2)))
     *                                 .or.
     *                           ((itetc1(2*(m-1)+1) .eq. ptnumber(2))
     *                                 .and.
     *                            (itetc1(2*(m-1)+2) .eq. ptnumber(1))))
     *                                 jfoundit = 1
                                    m = m + 1
                                 enddo
                                 if ( jfoundit .eq. 0 ) then
                                    ntetsc = ntetsc + 1
               itetoffc(ntetsc) = itindex
               jtetoffc(ntetsc) = jtindex
               itindex = itindex + nelmnen(ifelmlin)
               jtindex = jtindex + nelmnef(ifelmlin)
                                    itetc1(2*(ntetsc-1)+1) =
     *                                  ptnumber(1)
                                    itetc1(2*(ntetsc-1)+2) =
     *                                  ptnumber(2)
                                    itetclrc(ntetsc) = itetclra(i)
                                    itettypc(ntetsc) = ifelmlin
                                 endif
C
C   end "if ( (ifoundit(1) .eq. 0) .or. ", etc.
C
                              endif
C
                              if ((npointscmax-npointsc) .lt. 2) then
                                 iresize = 1
                                 npointscmax = npointscmax*2
                              endif
                              if ((ntetscmax-ntetsc) .lt. 2) then
                                 iresize = 1
                                 ntetscmax = ntetscmax*2
                              endif
                              if ( iresize .eq. 1 ) then
                                 iresize = 0
      call cmo_set_info('nnodes',cmoout,npointscmax,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetscmax,1,1,ierror)
C
      call cmo_newlen(cmoout,ierror)
C
      call cmo_get_info('imt1',cmoout,ipimt1c,lenimt1c,icmotypc,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1c,lenitp1c,icmotypc,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1c,lenicr1c,icmotypc,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1c,lenisn1c,icmotypc,ierror)
      call cmo_get_info('xic',cmoout,ipxicc,lenxicc,icmotypc,ierror)
      call cmo_get_info('yic',cmoout,ipyicc,lenyicc,icmotypc,ierror)
      call cmo_get_info('zic',cmoout,ipzicc,lenzicc,icmotypc,ierror)
      call cmo_get_info('mbndry',cmoout,mbndryc,lenmbndryc,icmotypc,
     *                  ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclrc,lenitetclrc,
     *			icmotypc,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypc,lenitettypc,
     *			icmotypc,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffc,lenitetoffc,
     *			icmotypc,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffc,lenjtetoffc,
     *			icmotypc,ierror)
      call cmo_get_info('itet',cmoout,ipitetc1,lenitetc1,icmotypc,
     *                  ierror)
      call cmo_get_info('jtet',cmoout,ipjtetc1,lenjtetc1,icmotypc,
     *                  ierror)
C
C   end "if (iresize .eq. 1)"
C
                              endif
C
C   end "if ( kpts(1) .ne. 0 ) then ", etc.
C
                           endif
C
C   end "if ( (abs(ti1-ti2) .gt.epsilone) ", etc.
C
                        endif
C
C   end "if ( ( (db1 .gt. epsilone) .and. ", etc.
C
                     endif
C
C   end "if ( ( (da1 .gt. epsilone) .and. ", etc.
C
                  endif
C
C   end "if ( (bbbxmax(j) .lt. bbaxmin(i)) ", etc.
C
               endif
C
C   end "do j=1,ntetsb"
C
            enddo
C
C   end "if ( (globbxmax .lt. bbaxmin(i)) .or. ", etc.
C
         endif
C
C   end "do i=1,ntetsa"
C
      enddo
C
C   If no new tets were found, this is an empty part.
C
      if(ntetsc.eq.0) then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Nothing found for new mesh object.'
          call writloga('default',0,logmess,0,ierr1)
        goto 9999
      endif
C
C   Figure out the new jtet array.
C
C
C   Update the new cmo.
C
      call cmo_set_info('nnodes',cmoout,npointsc,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetsc,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
C
C
C   Figure out the new jtet array.
C
      call geniee_cmo(cmoout)
 
C  Use cmo_interpolate to fill output cmo
      three = 3
      call cmo_interpolate(cmoout,cmoain,'nnodes',nlist,three,
     x     list,ilist,xweight,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate',
     *                               ierror)
C
C
C     ...............................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
C
      call cmo_get_info('nnodes',cmoout,npointsc,length,
     *                   icmotypc,ierror)
      call cmo_get_info('nelements',cmoout,ntetsc,length,
     *                   icmotypc,ierror)
      call cmo_get_info('mbndry',cmoout,mbndry,length,
     *                   icmotypc,ierror)
      call cmo_get_info('faces_per_element',cmoout,nefcmo,length,
     *                   icmotypc,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1c,lenitp1c,
     *                   icmotypc,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1c,lenicr1c,
     *                   icmotypc,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclrc,lenitetclrc,
     *                   icmotypc,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypc,lenitettypc,
     *                   icmotypc,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffc,lenitetoffc,
     *                   icmotypc,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffc,lenjtetoffc,
     *                   icmotypc,ierror)
      call cmo_get_info('itet',cmoout,ipitetc1,lenitetc1,
     *                   icmotypc,ierror)
      call cmo_get_info('jtet',cmoout,ipjtetc1,lenjtetc1,
     *                   icmotypc,ierror)
C
      length=npointsc
      call mmgetblk('idone',isubname,ipidone,length,2,icscode)
      do i=1,npointsc
         idone(i)=0
         itp1c(i)=0
      enddo
C
      do it=1,ntetsc
         do i=1,nelmnef(itettypc(it))
            if (jtetc1(jtetoffc(it)+i).ge.mbndryc) then
               do j=1,ielmface0(i,itettypc(it))
                  node1 = itetc1(itetoffc(it)+
     *                           ielmface1(j,i,itettypc(it)))
                  itp1c(node1)=ifitprfl
               enddo
            endif
         enddo
      enddo
C
      do it=1,ntetsc
         do i=1,nelmnef(itettypc(it))
 
            if (jtetc1(jtetoffc(it)+i).gt.0.and.
     *          jtetc1(jtetoffc(it)+i).lt.mbndryc) then
               jt=1+(jtetc1(jtetoffc(it)+i)-1)/nefcmo
               jf=jtetc1(jtetoffc(it)+i)-nefcmo*(it-1)
               if(itetclra(it).ne.itetclra(jt)) then
                  do j=1,ielmface0(i,itettypc(it))
                     node1=itetc1(itetoffc(it)+
     *                            ielmface1(j,i,itettypc(it)))
                     if(idone(node1).eq.0) then
                        idone(node1)=1
                        if(itp1c(node1).eq.ifitprfl) then
                           itp1c(node1)=ifitpinb
                        else
                           itp1c(node1)=ifitpini
                        endif
                     endif
                  enddo
               endif
            endif
         enddo
      enddo
C
      do i=1,npointsc
         if(itp1c(i).eq.ifitpint.or.itp1c(i).eq.ifitpini) icr1a(i)=0
      enddo
C
 
C
C   Update the new cmo.
C
      call cmo_set_info('nnodes',cmoout,npointsc,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetsc,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
C
C  Update ipointi and ipointj
      ipointi=1
      ipointj=npointsc
      call set_info_i('ipointi',cmoout,cglobal,cdefault,ipointi,
     *     ierror)
      call set_info_i('ipointj',cmoout,cglobal,cdefault,ipointj,
     *     ierror)
 
C
 9999 continue
C
C      If hextotet had to be called, release the cmo's created by hextotet.
C
         if(if_new_cmoa .ne. 0)then
            dotask_command ='cmo/release/cmo_tri_a ; finish '
            call dotaskx3d(dotask_command,ierror)
         endif
         if(if_new_cmob .ne. 0)then
            dotask_command ='cmo/release/cmo_tri_b ; finish'
            call dotaskx3d(dotask_command,ierror)
         endif
C
      call mmrelprt(isubname,icscode)
      return
      end
      subroutine get_plane_coeffs(x1,y1,z1,x2,y2,z2,x3,y3,z3,a,b,c,d)
C
C #####################################################################
C
C     PURPOSE -
C
C        Computes the coefficients of the plane equation:
C        ax + by + cz + d = 0, given three points.
C
C     INPUT ARGUMENTS -
C
C        x1,y1,z1 - COORDINATES OF THE FIRST POINT
C        x2,y2,z2 - COORDINATES OF THE SECOND POINT
C        x3,y3,z3 - COORDINATES OF THE THIRD POINT
C
C     OUTPUT ARGUMENTS -
C
C        a,b,c,d  - PLANE EQUATI0N COEFFICIENTS
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,a,b,c,d
      real*8 temp
      a = ((z1 - z2)*(y3 - y2) - (y1 - y2)*(z3 - z2))
      b = ((x1 - x2)*(z3 - z2) - (x3 - x2)*(z1 - z2))
      c = ((y1 - y2)*(x3 - x2) - (x1 - x2)*(y3 - y2))
      temp = sqrt(a*a + b*b + c*c)
      a = a/temp
      b = b/temp
      c = c/temp
      d = -(a*x1 + b*y1 + c*z1)
      return
      end
      subroutine overlap(ti1,ti2,tj1,tj2,kpts)
C
C #####################################################################
C
C     PURPOSE -
C
C        Returns endpoints of a line segment consisting of the
C        overlapping portion of two colinear segments.
C
C     INPUT ARGUMENTS -
C
C        ti1,ti2 - PARAMETERIZED DISTANCE ALONG THE LINE OF THE
C                  ENDPOINTS OF THE FIRST SEGMENT
C        tJ1,tJ2 - PARAMETERIZED DISTANCE ALONG THE LINE OF THE
C                  ENDPOINTS OF THE SECOND SEGMENT
C
C     OUTPUT ARGUMENTS -
C
C        kpts    - SEQUENCE OF THE OVERLAPPING SEGMENT:
C                  1 = ti1, 2 = ti2, 3 = tj1, 4 = tj2
C                  0 = NO OVERLAP
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C
      implicit none
      real*8 ti1,ti2,tj1,tj2,epsilone
      integer kpts(2)
C
C   Put a zero in kpts(1).  If this is returned, it means that
C   there is no overlap.  If kpts(i) .eq. 1, 2, 3, or 4, this means
C   that, respectively, ti1, ti2, ti3, or ti4 is the point in
C   question.
C
      kpts(1) = 0
      epsilone=1.0e-8
C
C   There are 13 ways that two line segments can overlap.  Here
C   they are:
C
C    Case 1:
C    ti1______________ti2
C                              tj1____________tj2
C   Nothing to do here.
C
C    Case 2:
C    ti1______________|ti2
C                  tj1|____________tj2
C   They just touch.  Do nothing.
C
C    Case 10:
C                  ti1|____________ti2
C    tj1______________|tj2
C   They just touch.  Do nothing.
C
C    Case 11:
C                              ti1____________ti2
C    tj1_____________tj2
C   Do nothing.
C
C    Case 3:
C    ti1_____________ti2
C          tj1______________tj2
C
      if ( ((ti2 - tj1) .gt. epsilone) .and.
     *     ((tj2 - ti2) .gt. epsilone) .and.
     *     ((tj1 - ti1) .gt. epsilone) ) then
         kpts(1) = 3
         kpts(2) = 2
C
C   Case 4:
C   ti1|____________ti2
C   tj1|____________________tj2
C
      else if ( (abs(ti1 - tj1) .le. epsilone) .and.
     *         ((tj2 - ti2) .gt. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 2
C
C  Case 5:
C   ti1__________|ti2
C       tj1______|tj2
C
      else if ( ((tj1 - ti1) .gt. epsilone) .and.
     *           (abs(ti2 - tj2) .le. epsilone) ) then
         kpts(1) = 3
         kpts(2) = 2
C
C  Case 6:
C   ti1|__________|ti2
C   tj1|__________|tj2
C
      else if ( (abs(ti1 - tj1) .le. epsilone) .and.
     *          (abs(ti2 - tj2) .le. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 2
C
C  Case 7:
C   ti1|____________ti2
C   tj1|______tj2
C
      else if ( (abs(ti1 - tj1) .le. epsilone) .and.
     *         ((ti2 - tj2) .gt. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 4
C
C Case 8:
C       ti1________|ti2
C   tj1____________|tj2
C
      else if ( ((ti1 - tj1) .gt. epsilone) .and.
     *          (abs(ti2 - tj2) .le. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 2
C
C  Case 9:
C         ti1____________ti2
C   tj1__________tj2
C
      else if ( ((ti1 - tj1) .gt. epsilone) .and.
     *          ((ti2 - tj2) .gt. epsilone) .and.
     *          ((tj2 - ti1) .gt. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 4
C
C  Case 12:
C   ti1____________________ti2
C       tj1___________tj2
C
      else if ( ((tj1 - ti1) .gt. epsilone) .and.
     *         ((ti2 - tj2) .gt. epsilone) ) then
         kpts(1) = 3
         kpts(2) = 4
C
C  Case 13:
C        ti1____ti2
C   tj1_______________tj2
C
      else if ( ((ti1 - tj1) .gt. epsilone) .and.
     *          ((tj2 - ti2) .gt. epsilone) ) then
         kpts(1) = 1
         kpts(2) = 2
C
      endif
      return
      end
 
 
 
 
 
 
 
 
 
 
