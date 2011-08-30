      subroutine connect(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C     PURPOSE -
C
C        MAIN NEAREST-NEIGHBOR ROUTINE.  THIS ROUTINE FIRST CONNECTS THE
C        MESH DISREGARDING THE INTERFACES.  AFTERWARDS, POINTS ARE ADDED
C        TO BREAK MULTIMATERIAL CONNECTIONS.
C
C        FORMAT: SEARCH/delaunay/first_point/last_point/stride/
C                    coordinates of enclosing tet
C
C     INPUT ARGUMENTS -
C
C        imsgin   ]
C        xmsgin   ] - INPUT MESSAGE.
C        cmsgin   ]
C        msgtype  ]
C
C
C     OUTPUT ARGUMENTS -
C
C        THE NODE AND FACE CONNECTIVITY MATRIX FOR THE CMO.
C
C
C     CHANGE HISTORY -
C
C        $Log: connect.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.16   23 Jun 2006 11:08:08   tam
CPVCS    previous version did not have shiftl defined as external
CPVCS    
CPVCS       Rev 1.15   08 Feb 2006 14:38:14   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.14   28 Jul 2005 15:21:48   gable
CPVCS    Add check_interface option.
CPVCS    
CPVCS       Rev 1.13   17 Nov 2004 14:05:30   dcg
CPVCS    calculate the number of materials correctly
CPVCS    
CPVCS       Rev 1.12   30 Sep 2004 09:41:16   dcg
CPVCS    use ior in place of .or. with integers
CPVCS    
CPVCS       Rev 1.11   08 Jan 2002 08:34:36   dcg
CPVCS    correct spelling error
CPVCS    
CPVCS       Rev 1.10   09 Mar 2001 13:11:02   dcg
CPVCS    allow user to change circumsphere test epsilon by reading in a factor
CPVCS    look for mesh object attribut 'circumsphere_factor'
CPVCS    add one more connect step with test value half of previous
CPVCS    
CPVCS       Rev 1.9   16 Aug 2000 13:23:44   dcg
CPVCS    fix memory problem with failure lists
CPVCS    
CPVCS       Rev 1.8   21 Apr 2000 07:04:50   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.7   Tue Apr 04 15:01:40 2000   dcg
CPVCS    set 'small' based on epsilonr*fudge_factor - this
CPVCS    quantity is used in the delaunay sphere test
CPVCS    
CPVCS       Rev 1.6   28 Mar 2000 14:09:10   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.5   Thu Feb 17 08:37:46 2000   dcg
CPVCS    check for virtual nodes and set matlst correctly
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 08:47:32 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Wed Feb 02 13:31:22 2000   dcg
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:47:42   dcg
CPVCS    
CPVCS       Rev 1.1   05 Jan 2000 17:32:16   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.12   Tue Oct 12 16:45:44 1999   dcg
CPVCS    check for valid mbndry and reset if needed
CPVCS
CPVCS       Rev 1.11   Tue Aug 10 10:12:00 1999   dcg
CPVCS    release temp storage (imts1) when no longer needed
CPVCS
CPVCS       Rev 1.10   Tue Jun 08 16:56:04 1999   dcg
CPVCS    better test for no multi-material testing
CPVCS
CPVCS       Rev 1.9   Fri Jan 22 11:32:12 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.8   Fri Dec 04 16:38:14 1998   dcg
CPVCS    refresh pointers after call to remove_bigtet
CPVCS
CPVCS       Rev 1.7   Thu Jul 02 14:02:10 1998   dcg
CPVCS    allow more than 32 materials
CPVCS
CPVCS       Rev 1.6   Mon Nov 24 16:31:50 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.5   Fri Oct 03 17:23:34 1997   dcg
CPVCS    fix looping in multi-material - quit if all points
CPVCS    that were attempted to be added failed
CPVCS
CPVCS       Rev 1.4   Thu Aug 28 15:27:52 1997   dcg
CPVCS    fix memory management error with updating npoints and ntets
CPVCS
CPVCS       Rev 1.1   Mon Aug 18 15:55:34 1997   dcg
CPVCS    set itetoff, jtetoff and itettyp
CPVCS
CPVCS       Rev 1.0   Mon Aug 18 14:54:58 1997   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.52   Mon Jul 14 10:20:28 1997   dcg
CPVCS    orient bigtet wrt longer sides of the enclosing box
CPVCS    allow user to specify bigtet coordinates on command line
CPVCS    relax volume test for tets which contain at least
CPVCS    one bigtet vertex
CPVCS
CPVCS       Rev 1.51   Mon Apr 14 16:55:30 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.50   Wed Apr 02 13:25:20 1997   dcg
CPVCS    get correct material information for virtual
CPVCS    interface points
CPVCS
CPVCS       Rev 1.49   Fri Mar 07 09:27:36 1997   dcg
CPVCS    use epsilonl for smalfrac in volume test
CPVCS
CPVCS       Rev 1.48   Thu Mar 06 21:53:54 1997   het
CPVCS    Write "dudded" points to a file.
CPVCS
CPVCS       Rev 1.47   Tue Feb 25 21:20:36 1997   het
CPVCS    Correct a memory management error with the "itemp1()" array.
CPVCS
CPVCS       Rev 1.46   Fri Jan 31 09:41:06 1997   dcg
CPVCS    set default isrchopt and point range correctly
CPVCS
CPVCS       Rev 1.45   Thu Jan 30 13:54:22 1997   dcg
CPVCS    Implement option to force only Delaunay tetrahedralization
CPVCS    Activate with search/delaunay command - switch is
CPVCS    logical variable ifdelaun
CPVCS
CPVCS       Rev 1.44   Fri Jan 24 13:59:12 1997   het
CPVCS    Add the SPH bailout "goto", but comment it out.
CPVCS
CPVCS       Rev 1.43   Wed Nov 13 09:59:14 1996   dcg
CPVCS    put in more point insertion failure tests
CPVCS
CPVCS       Rev 1.42   Tue Nov 05 10:06:10 1996   dcg
CPVCS    separate integer,real and charater variable in common
CPVCS
CPVCS       Rev 1.41   Fri Nov 01 16:33:00 1996   dcg
CPVCS    fix reordering of connection list
CPVCS    use complete tet search if iaddpass ne 0
CPVCS    put in more error traps
CPVCS
CPVCS       Rev 1.40   Thu Oct 10 11:27:18 1996   dcg
CPVCS    take care of case with no points to add when
CPVCS    multimaterial connection is detected
CPVCS
CPVCS       Rev 1.39   Mon Oct 07 16:14:48 1996   dcg
CPVCS    fix two typos icc to iic and npstnew to nptsnew
CPVCS    also move mmnewlen call for matlst to use nlstptl
CPVCS    instead of nlstcns
CPVCS
CPVCS       Rev 1.38   Thu Sep 26 14:22:58 1996   dcg
CPVCS    correct memory error with lstplt
CPVCS
CPVCS       Rev 1.37   Wed Sep 11 10:10:20 1996   dcg
CPVCS    initialize rtestmin to 1.e+99 (1/smalarea not sufficient)
CPVCS
CPVCS       Rev 1.36   Mon Sep 09 14:34:34 1996   dcg
CPVCS    put back do 100, do 200 full npoint by ntet search
CPVCS    for enclosing circumsphere if idelaun.eq.0 (breaking
CPVCS    multimaterial phase) or istep > 3 (nstepdgn).
CPVCS
CPVCS       Rev 1.35   Fri Sep 06 17:03:32 1996   dcg
CPVCS    replace n squared search for cavity tets with
CPVCS    walking algorithm and neighbor based search
CPVCS
CPVCS       Rev 1.34   Thu Aug 29 15:57:26 1996   dcg
CPVCS    fix memory management problems with multimaterial connections
CPVCS
CPVCS       Rev 1.33   Tue Aug 27 14:21:42 1996   dcg
CPVCS    replace loop on pairs of points for multimaterial
CPVCS    connecting with calls to routines that process at pairs (ifaceptv)
CPVCS
CPVCS       Rev 1.32   Mon Jul 08 16:05:44 1996   dcg
CPVCS    fix error when checking for interface points in multimaterial tets
CPVCS
CPVCS       Rev 1.31   Tue May 21 17:12:56 1996   dcg
CPVCS    delete statement: ntets=ntets-nlstnew
CPVCS    when incrementing memory add space for 500 extra tets
CPVCS
CPVCS       Rev 1.30   Thu May 16 10:27:52 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.29   Mon May 06 12:27:18 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.28   Wed Mar 06 16:36:10 1996   dcg
CPVCS    initialize imtmax to 1
CPVCS
CPVCS       Rev 1.27   Tue Mar 05 13:46:36 1996   dcg
CPVCS    remove int1, icn1
CPVCS
CPVCS       Rev 1.26   Thu Feb 22 17:13:56 1996   dcg
CPVCS    add call to cmo_interpolate for points added to
CPVCS    break multi-material connections
CPVCS
CPVCS       Rev 1.25   Fri Feb 02 14:22:06 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.24   Tue Jan 23 09:50:30 1996   dcg
CPVCS    replace cray dependent shift and mask code when
CPVCS    checking for interface points
CPVCS
CPVCS       Rev 1.23   Tue Jan 23 09:27:40 1996   het
CPVCS    Correct the jtet1 test for hybrid grids.
CPVCS
CPVCS       Rev 1.22   12/05/95 08:23:26   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.20   11/08/95 09:33:58   dcg
CPVCS    replace calls to mmgasblk with mmgetblk
CPVCS
CPVCS       Rev 1.19   11/07/95 17:22:18   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.18   10/16/95 10:28:48   het
CPVCS    No change.
CPVCS
CPVCS       Rev 1.17   09/07/95 14:11:22   dcg
CPVCS    refresh pointers after cmo_newlen call
CPVCS
CPVCS       Rev 1.15   08/29/95 11:51:48   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.14   08/23/95 06:57:48   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.13   08/22/95 06:50:14   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.12   06/20/95 15:40:26   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.11   06/07/95 15:30:44   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.10   05/26/95 13:19:52   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.9   05/11/95 13:54:18   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.8   05/01/95 08:33:40   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.7   03/31/95 09:09:00   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.6   03/30/95 05:00:20   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.5   03/23/95 15:07:32   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.4   02/18/95 06:56:36   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.3   02/16/95 15:54:28   dcg
CPVCS
CPVCS       Rev 1.2   02/16/95 10:23:56   dcg
CPVCS    compatiblity changes for ibm platform
CPVCS
CPVCS       Rev 1.1   02/16/95 07:37:30   het
CPVCS    Corrected an error with calling nn3dn_bin
CPVCS
CPVCS       Rev 1.0   02/14/95 14:35:22   dcg
CPVCS    Original version
C
C#######################################################################
C
      implicit none
 
      integer nwds,ierr1
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
C
      integer ier,leni,icmotype,npoints,ntets,iout,lout,itype,
     * ipt1,ipt2,ipt3,
     * ifield,ipttyp,mpno,length,incpts,
     * lencns,ics,nmatfal1,nmatfal2,
     * it,i,j,nmatmax,ii,ierr,k,
     * iunit,nen,nef,ierror,nerr,
     * nlsttts,n,imtval,
     * matindex,iimt,i1,nlstptls,nlstptln
C
      include 'cmo.h'
      include 'chydro.h'
      include 'consts.h'
      include 'search.h'
      include 'local_element.h'
      include 'geom_lg.h'
C
C#######################################################################
C
      integer intconn, intpass,intpasmx,npoints_save,
     * icscode,ntetmax,lastimt

      integer ismax, icharlnf
      integer rimat,mpary,lstptl

      real*8 delx,dely,delz,crossx,crossy,crossz,sumsq,
     * cvmgt
      pointer (iprimat  ,rimat(1000000)     )
      pointer (ipmpary ,mpary(1000000)     )
      pointer (iplstptl,lstptl(1000000)    )
      real*8 circumsphere_factor

C        *** POINTERS RELATED TO MASS POINTS.
      real*8 r,rout
      pointer(ipout,out)
      real*8 out(*)
c
      pointer (ipnimts ,nimts(1000000)      )
      pointer (ipimts1 ,imts1(1000000)      )
      pointer (ipmatlst,matlst(1000000)    )
C
      pointer (ipitetoff,itetoff(1000000)  )
      pointer (ipjtetoff,jtetoff(1000000)  )
      integer  jtetoff,itetoff,
     *     nimts,imts1,matlst

C        *** POINTERS RELATED TO MREGIONS AND SURFACES.
      logical itsttp, ifp1,ifp2,ifp3,ifp4
      logical lif_search_intrface_edges
      real*8 xcoords(12),vvbarmin

      data cglobal,cdefault/'global','default'/

      integer shiftl, ior
      external shiftl

      character*8 cpart,cnnodes
      character*8 cglobal,cdefault
      character*32 isubname,blkname,cout
      character*32 cpt1,cpt2,cpt3,geom_name
      character*132 logmess
C
C#######################################################################
C     MACROS.
C
      delx(i,j)=xl(j,it)-xl(i,it)
      dely(i,j)=yl(j,it)-yl(i,it)
      delz(i,j)=zl(j,it)-zl(i,it)
      crossx(i,j)=delyb(i,it)*delzb(j,it)-delzb(i,it)*delyb(j,it)
      crossy(i,j)=-(delxb(i,it)*delzb(j,it)-delzb(i,it)*delxb(j,it))
      crossz(i,j)=delxb(i,it)*delyb(j,it)-delyb(i,it)*delxb(j,it)
      sumsq(i)=xl(i,it)**2+yl(i,it)**2+zl(i,it)**2
 
C
C#######################################################################
C
      ntetmax=0
      isubname='connect'
      blkname='nn3dn'
      cpart='part'
      cnnodes='nnodes'
C     counter for number of errors encountered
      nerr = 0
C
      call cmo_get_name(cmo,ier)
      if (ier.ne.0) then
         nerr = nerr+1
         write(logmess,'(a,a,a)')'Error: ',
     *   isubname(1:icharlnf(isubname)),
     *   '     > can not get cmo name.'
         call writloga('default',0,logmess,1,icscode)
         ierr1 = 1
         goto 9999
      endif

      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call cmo_get_attinfo('circumsphere_factor',cmo,iout,
     *           circumsphere_factor,cout,ipout,lout,itype,ierror)
      if(ierror.ne.0) circumsphere_factor=1.0
C
      call cmo_get_intinfo('nnodes',cmo,npoints,leni,icmotype,ier)

      if (ier.ne.0) then
         nerr = nerr+1
         call list_errc(nerr,isubname,
     *       'can not get node info from cmo: ',cmo)
         ierr1 = ier
         goto 9999
       endif

       if (npoints.le.1) then
         nerr = nerr+1
         call list_erri(nerr,isubname,
     *        'invalid number of nodes: ',npoints)
         ierr1 = 1
         goto 9999
       endif


      call cmo_get_intinfo('nelements',cmo,ntets,leni,icmotype,ier)
      call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
      npoints_save=npoints
c
c  check value of mbndry
c
      call set_mbndry()
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
C
C    SET POINT-INDEX BOUNDARIES.
C
 
      ipt1=1
      ipt2=npoints
      ipt3=1
      ipttyp=0
      ifield=2
      if (nwds.eq.ifield) then
         ipt1=1
         ipt2=npoints
         ipt3=1
      elseif(nwds.eq.ifield+1) then
         ipt1=imsgin(ifield+1)
         ipt2=npoints
         ipt3=1
      elseif(nwds.eq.ifield+2) then
         ipt1=imsgin(ifield+1)
         ipt2=imsgin(ifield+2)
         ipt3=1
      elseif(nwds.ge.ifield+3) then
         if(msgtype(ifield+1).eq.1) then
            ipt1=imsgin(ifield+1)
            ipt2=imsgin(ifield+2)
            ipt3=imsgin(ifield+3)
         elseif(msgtype(ifield+1).eq.3) then
            ipttyp=1
            cpt1=cmsgin(ifield+1)
            cpt2=cmsgin(ifield+2)
            cpt3=cmsgin(ifield+3)
         endif
      endif
C
C     ******************************************************************C
C     SET SOME VARIABLES.
C
      iremove=1
      idud=1
      iaddpts=1
C        *** iremove: FLAG FOR REMOVING THE ENCLOSING TETRAHEDRON.
C        *** idud:    FLAG FOR DUDDING THE NONCONNECTABLE POINTS.
C        *** iaddpts: FLAG FOR ADDING MASS POINTS TO ELIMINATE
C        ***          SMALL-VOLUME TETRAHEDRA.
      ntets=0
      ntetexcl=ntets
      itetstrt=ntets+1
      ittstrt1=4*ntets+1
C        *** ntetexcl=EXISTING NUMBER OF TETRAHEDRA TO BE EXCLUDED FROM
C        ***          THE SEARCH PROCESS.
C        *** itetstrt=STARTING INDEX OF TETRAHEDRA TO BE INCLUDED IN THE
C        ***          SEARCH PROCESS.
C        *** ittstrt1=STARTING FACE INDEX OF TETRAHEDRA TO BE INCLUDED
C        ***          IN THE SEARCH PROCESS.
C     ******************************************************************
C
C     GET SOME INITIAL MEMORY (RELATED TO THE TOTAL NUMBER OF
C     TETRAHEDRA).
C
      ntetmax=max(ntetmax,7*npoints+ntets)
      call cmo_set_info('nelements',cmo,ntetmax,1,1,ierr)
      call cmo_newlen(cmo,ierr)
      ntetmaxl=ntetmax-ntetexcl
      call mmgetblk('vol',blkname,ipvol,ntetmax,2,icscode)
      call mmgetblk('xvor',blkname,ipxvor,ntetmax,2,icscode)
      call mmgetblk('yvor',blkname,ipyvor,ntetmax,2,icscode)
      call mmgetblk('zvor',blkname,ipzvor,ntetmax,2,icscode)
      call mmgetblk('lsttts',blkname,iplsttts,ntetmax,1,icscode)
C
C  access mesh object
C
      nnodes=npoints+4
      call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
      call cmo_newlen(cmo,ierr)
      call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
      lstptlen=npoints+4
      call mmgetblk('mpary',blkname,ipmpary,npoints,1,icscode)
      call mmgetblk('lstptl',blkname,iplstptl,lstptlen,1,icscode)
      mpno=npoints
      if(ipttyp.eq.0)then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,npoints,isetwd,itp1)
      else
         call pntlimc(cpt1,cpt2,cpt3,ipmpary,mpno,npoints,isetwd,itp1)
      endif
      nlstptl=mpno
      do 10 ii=1,mpno
         lstptl(ii)=mpary(ii)
   10 continue
      call mmgetblk('lstfail',blkname,iplstfal,lstptlen,1,icscode)
C
C     SET THE REGION NAMES, OFFSETS, AND NUMBER OF ELEMENTS FOR ALL
C     REGIONS.
C
      if(nmregs.gt.0) then
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,
     *        ierror)
         call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierror)
         call mmfindbk('mregdef',geom_name,ipmregdef,length,ierror)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      endif
 
C
C     SET THE SURFACE NAMES AND OFFSETS FOR ALL SURFACES.
C
      if(nsurf.gt.0) then
         call mmfindbk('csall',geom_name,ipcsall,length,ierror)
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      endif
C     __________________________________________________________________
C
C     GET MEMORY FOR THE MATERIAL-LIST ARRAY.
C     count the number of different materials
C
      length=npoints
      call mmgetblk('rimat',blkname,iprimat,length,1,icscode)
      do i=1,npoints
        rimat(i)=imt1(i)
      enddo
      call hpsorti(npoints,rimat)
      nmatmax=1
      j=lastimt
      do i=2,npoints
         if(rimat(i) .gt.lastimt) then
             lastimt=rimat(i)
             nmatmax=nmatmax+1
         endif
      enddo
      call mmrelblk('rimat',blkname,iprimat,icscode)
      matblks=max((nmatmax+31)/32,1)
C        *** NUMBER OF BLOCKS OF THE MATERIAL ARRAY.  EACH BLOCK WILL
C        *** HANDLE UP TO 32 MATERIALS.
      lenmatmx=(6*(npoints+4))/5
C        *** LENGTH OF EACH MATERIAL-LIST-ARRAY BLOCK.
      call mmgetblk('matlst',blkname,ipmatlst,matblks*lenmatmx,1,ics)
C
C     ******************************************************************
C     INITIALIZE SOME MORE VARIABLES.
C
      nstepdgn=4
C        *** MAXIMUM NUMBER OF ATTEMPTS TO BE MADE TO CONNECT UP THE
C        *** MESH DURING DEGENERACY CONNECTIONS WITHOUT ADDING MORE
C        *** POINTS.
      small=epsilonr*100000.*circumsphere_factor
C        *** EFFECTIVE EPSILONS FOR COMPARISONS.
      call get_epsilon('epsilonl',smalfrac)
C        *** SMALL NUMBER FOR COMPARING PERCENT DIFFERENCES.
C     GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', smldistp)
cccccc      smldistp=small*max(boxsizex,boxsizey,boxsizez)
C        *** SMALL DISTANCE (PERMANENT) TO BE USED IN DETERMINING
C        *** POINT TYPES, MATERIAL TYPES, AND INTERFACE LOCATIONS.
      vvbarmin=1.0e-07
C        *** MINIMUM ACCEPTABLE RATIO OF TETRAHEDRON VOLUME TO THE
C        *** MAXIMUM OF AVERAGE TETRAHEDRON VOLUME ASSOCIATED WITH THE
C        *** FOUR VERTEX MASS POINTS, BELOW WHICH MASS POINTS WILL BE
C        *** ADDED TO ELIMINATE SMALL-VOLUME TETRAHEDRA.
      iaddpass=0
C        *** PASS NUMBER WHILE ADDING POINTS TO ELIMINATE SMALL-
C        *** VOLUME TETRAHEDRA.
      idrastic=0
C        *** DESPARATION FLAG FOR ADDING THE OFFENDING POINTS.
      intpass=0
C        *** PASS NUMBER WHILE ADDING INTERFACE POINTS IN ORDER TO
C        *** BREAK MULTIMATERIAL CONNECTIONS.
      intconn=0
C        *** NUMBER OF MULTIMATERIAL CONNECTIONS ENCOUNTERED IN THE
C        *** PROBLEM.
      intpasmx=20
C        *** MAXIMUM NUMBER OF PASSES TO BE MADE FOR ADDING INTERFACE
C        *** POINTS IN ORDER TO BREAK MULTIMATERIAL CONNECTIONS.
      lencns=0
C        *** LENGTH OF THE ARRAY CONTAINING THE MULTIMATERIAL-
C        *** CONNECTION LIST.
      idelaun=1
C        *** FLAG FOR DELAUNAY TETRAHEDRALIZATION.
C      force delaunay always
C                                    disable merge and drastic step.
C     ******************************************************************
C
C     GET MATERIAL NUMBER FOR INTERFACE POINTS.
C
      imatint=0
      do i=1,nmregs
        if(cmregs(i).eq.'intrface') imatint=matregs(i)
      enddo
C
C
C     ******************************************************************
C
C     EXCLUDE INACTIVE POINTS FROM THE POINT LIST.
C
      do  ii=1,nlstptl
         lstptl(ii)=cvmgt(lstptl(ii),0,
     $    (itp1(lstptl(ii)).ge.ifitpst1.and.
     $     itp1(lstptl(ii)).le.ifitpen1)
     $    .or.
     $    (itp1(lstptl(ii)).ge.ifitpst2.and.
     $     itp1(lstptl(ii)).le.ifitpen2))
      enddo
      call kmprsn(nlstptl,lstptl(1),1,lstptl(1),1,lstptl(1),1,nlstptln)
      nlstptl=nlstptln
      nlstptls=nlstptl
C        *** SAVE THE NUMBER OF POINTS IN THIS ORIGINAL LIST, CURRENTLY
C        *** FOR NO OTHER PURPOSE THAN TO BE CONSISTENT WITH THE POINT-
C        *** ADDITION LOGICS.
C
C     ******************************************************************
C
C     SET UP A MATERIAL-LIST ARRAY, WHOSE EACH BIT DETERMINES WHETHER
C     A POINT IS ASSOCIATED WITH THE MATERIAL CORRESPONDING TO THE BIT
C     NUMBER.
C
C     __________________________________________________________________
C
C     SET UP THE MATERIAL-LIST ARRAY.
C
      imtmax=1
      do i1=1,npoints
         imtmax=max(imtmax,imt1(i1))
      enddo
      length=npoints
      call mmgetblk('nimts',blkname,ipnimts,npoints,1,ics)
      length=npoints*max(imtmax,nmregs)
      call mmgetblk('imts1',blkname,ipimts1,length,1,ics)
      nmatfal1=0
      nmatfal2=0
      if(nmregs.gt.0) then
         call ifaceregv(xic,yic,zic,npoints,
     $                  smldistp,
     *                  imts1,nimts,
     *                  ierr)
      else
         do i1=1,npoints
            nimts(i1)=1
            imts1(i1)=imt1(i1)
         enddo
      endif
      do  i=1,npoints
         if(nimts(i).eq.0.and.itsttp('virtual',itp1(i))) then
               matindex=((imt1(i)-1)/32)*lenmatmx
               matlst(matindex+i)=ior(matlst(matindex+i),
     $          shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1))
         elseif(itsttp('intrface',itp1(i))) then
            if(nimts(i).gt.0) then
               do 23 iimt=1,nimts(i)
                  imtval=imts1(iimt+nmregs*(i-1))
                  matindex=((imtval-1)/32)*lenmatmx
                  i1=shiftl(1,imtval-((imtval-1)/32)*32-1)
                  matlst(matindex+i)=ior(matlst(matindex+i),i1)
   23          continue
            else
               nmatfal1=nmatfal1+1
            endif
         elseif((itp1(i).ge.ifitpst1.and.itp1(i).le.ifitpen1)
     $          .or.
     $          (itp1(i).ge.ifitpst2.and.itp1(i).le.ifitpen2)
     $         ) then
            if(imt1(i).ne.0) then
               matindex=((imt1(i)-1)/32)*lenmatmx
               i1=shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1)
               matlst(matindex+i)=ior(matlst(matindex+i),i1)
            else
               nmatfal2=nmatfal2+1
            endif
         endif
      enddo
      call mmrelblk('imts1',blkname,ipimts1,ics)
      if(nmatfal1.ne.0) then
         write(logmess,25) nmatfal1
   25    format(' Cannot find materials for',i10,' interface points.')
         call writloga(cdefault,1,logmess,0,ierr)
      endif
      if(nmatfal2.ne.0) then
         write(logmess,26) nmatfal2
   26    format(' No materials associated with',i10,
     $          ' noninterface points.')
         call writloga(cdefault,0,logmess,0,ierr)
      endif
C
C     ******************************************************************
C
C     ENCLOSE APPROPRIATE POINTS IN A BIG TETRAHEDRON IF NO MESH EXISTS.
C     OTHERWISE, CHANGE MASS-POINT NUMBERS OF POINTS ASSOCIATED WITH THE
C     SURROUNDING TETRAHEDRON.
C
      n=0
      if (nwds.ge.ifield+4) then
         n=12
         do j=1,12
            xcoords(j)=xmsgin(ifield+3+j)
         enddo
      endif
      call make_big_tet(npoints,ntets,xcoords,n)
      write(logmess,"(' Coordinates of enclosing tetrahedron are: ')")
      call writloga(cdefault,0,logmess,0,ierr)
      do j=1,4
        write(logmess,'(7x,3(1x,d14.5))') xbigtet(j),ybigtet(j),
     *        zbigtet(j)
        call writloga(cdefault,0,logmess,0,ierr)
      enddo
C
C     CHECK IF ADDING POINTS IS TURNED OFF
C     CHECK IF EXTRA SEARCH FOR CONNECTIONS BETWEEN INTRFACE NODE IS TURNED ON
C
      lifadd=.true.
      lif_search_intrface_edges =.false.

      incpts=0
      if(cmsgin(nwds)(1:icharlnf(cmsgin(nwds))).eq.'noadd')
     *      lifadd=.false.
      if(cmsgin(nwds)(1:icharlnf(cmsgin(nwds))).eq.'check_interface')
     *      lif_search_intrface_edges = .true.
     
      if (ibigtet.gt.0) then
         xic (ibigtet  )=xbigtet(1)
         yic (ibigtet  )=ybigtet(1)
         zic (ibigtet  )=zbigtet(1)
         itp1(ibigtet  )=ifitpini
         imt1(ibigtet  )=0
         xic (ibigtet+1)=xbigtet(2)
         yic (ibigtet+1)=ybigtet(2)
         zic (ibigtet+1)=zbigtet(2)
         itp1(ibigtet+1)=ifitpini
         imt1(ibigtet+1)=0
         xic (ibigtet+2)=xbigtet(3)
         yic (ibigtet+2)=ybigtet(3)
         zic (ibigtet+2)=zbigtet(3)
         itp1(ibigtet+2)=ifitpini
         imt1(ibigtet+2)=0
         xic (ibigtet+3)=xbigtet(4)
         yic (ibigtet+3)=ybigtet(4)
         zic (ibigtet+3)=zbigtet(4)
         itp1(ibigtet+3)=ifitpini
         imt1(ibigtet+3)=0
         do n=1,matblks
            matindex=(n-1)*lenmatmx
            matlst(matindex+ibigtet  )=-1
            matlst(matindex+ibigtet+1)=-1
            matlst(matindex+ibigtet+2)=-1
            matlst(matindex+ibigtet+3)=-1
         enddo
         incpts=4
      else
         incpts=0
      endif
C
C        *** ALL matlst BITS FOR THE BIG-TETRAHEDRON POINTS ARE SET,
C        *** SO THAT A CONNECTION ORIGINATING FROM THESE POINTS WILL
C        *** NEVER BE CONSIDERED A MULTIMATERIAL CONNECTION DURING
C        *** THE SECOND INTERFACE PASS, AND ALL SUBSEQUENT PASSES.
C
C     ******************************************************************
C
C     CALCULATE VOLUMES, VORONOI POINTS
C              *** SIX TIMES THE TRUE TETRAHEDRON VOLUME.
C              *** TWICE THE COORDINATES OF THE VORONOI POINT.
C
      if(ibigtet.gt.0) then
         call volume_tet(xic(ibigtet),yic(ibigtet),zic(ibigtet),
     *   xic(ibigtet+1),yic(ibigtet+1),zic(ibigtet+1),
     *   xic(ibigtet+2),yic(ibigtet+2),zic(ibigtet+2),
     *   xic(ibigtet+3),yic(ibigtet+3),zic(ibigtet+3),
     *   vol(1))
         vol(1)=6.*vol(1)
         call voronoi_center(xic(ibigtet),yic(ibigtet),zic(ibigtet),
     *   xic(ibigtet+1),yic(ibigtet+1),zic(ibigtet+1),
     *   xic(ibigtet+2),yic(ibigtet+2),zic(ibigtet+2),
     *   xic(ibigtet+3),yic(ibigtet+3),zic(ibigtet+3),
     *  xvor(1),yvor(1),zvor(1),r)
         xvor(1)=2.*xvor(1)
         yvor(1)=2.*yvor(1)
         zvor(1)=2.*zvor(1)
      endif
C
C     ******************************************************************
C
C     Do initial grid generation step
C
      nlsttts=0
      istep=1

C     Add some non-terminating error checking
      if (npoints.le.0) then 
       nerr=nerr+1
       call list_err(nerr,isubname,'initial step npoints 0')
      endif
      if (ntets.le.0) then 
       nerr=nerr+1
       call list_err(nerr,isubname,'initial step ntets 0')
      endif
      if (ntetmax.le.0) then 
       nerr=nerr+1
       call list_err(nerr,isubname,'initial step ntetmax 0')
      endif
      
      call delaunay_connect(npoints,ntets,epsilon,
     *  ntetmax,nlsttts)

      if (npoints.le.0) then
        write(logmess,'(a,i5)')
     *  'WARNING: connect has 0 nodes for initial work. ',npoints
        call writloga('default',0,logmess,0,icscode)
      endif

      if (ntets.le.0) then
        write(logmess,'(a,i5)')
     *  'WARNING: connect has 0 elements for initial work. ',ntets
        call writloga('default',0,logmess,0,icscode)
      endif
C
C  refresh pointers
C
      call mmfindbk('lstptl',blkname,iplstptl,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk lstptl')
      call mmfindbk('xvor',blkname,ipxvor,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk xvor')
      call mmfindbk('yvor',blkname,ipyvor,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk yvor')
      call mmfindbk('zvor',blkname,ipzvor,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk zvor')
      call mmfindbk('lsttts',blkname,iplsttts,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk lsttts')
      call mmfindbk('vol',blkname,ipvol,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk vol')
      call mmfindbk('lstfail',blkname,iplstfal,lennew,icscode)
      if (icscode.ne.0)call x3d_warn(isubname,'mmfindblk lstfail')

      call cmo_set_info('nelements',cmo,ntets,1,1,icscode)
      call cmo_set_info('nnodes',cmo,npoints,1,1,icscode)
C
C  find and break multimaterial connections
C
C  current multi_material breaking algorithm needs the enclosing bigtet
C  if  a new algorithm is written that does not need the enclosing tet
C  uncomment out the next two statements
c      call remove_bigtet()
c      ibigtet=0
C
C  Skip adding points if instructed on command line
C
      if(.not.lifadd) go to 9900
      if(.not.lif_search_intrface_edges)then
      call multi_material(0)
      else
      call multi_material(1)
      endif
C
C
      call mmfindbk('lstfail',blkname,iplstfal,lennew,icscode)
      call mmfindbk('lstptl',blkname,iplstptl,lennew,icscode)
      call cmo_get_info('nelements',cmo,ntets,icmotype,leni,icscode)
      call cmo_get_info('nnodes',cmo,npoints,icmotype,leni,icscode)
C
C     ******************************************************************
C
C     REMOVE ALL TETRAHEDRA ASSOCIATED WITH THE BIG TETRAHEDRON AND FILL
C     HOLES.
C
 9900 if(iremove.ne.0.and.ibigtet.gt. 0) then
 
         call remove_bigtet()
         call cmo_get_info('nelements',cmo,ntets,leni,icmotype,ierr)
         call cmo_get_info('nnodes',cmo,npoints,icmotype,leni,icscode)
         call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ierr)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
C        _______________________________________________________________
C        IN CASE SOME POINTS COULD NOT BE CONNECTED EARLIER, TRY TO
C        CONNECT THEM AGAIN WITH THE SURROUNDING TETRAHEDRON REMOVED,
C        AND/OR ADD POINTS TO ELIMINATE SMALL TETRAHEDRA.
C
C     Look for points flagged earlier as unconnected
C     Try them again now the bigtet has been removed
C
         nlstfail=0
         do i=1,npoints
            if(itp1(i).ge.1000) then
               itp1(i)=itp1(i)-1000
               nlstfail=nlstfail+1
               if(nlstfail.gt.lennew) then
                  call mmnewlen('lstfail',blkname,iplstfal,
     *                   nlstfail+100,icscode)
                  call mmnewlen('lstptl',blkname,iplstptl,
     *                   nlstfail+100,icscode)
                  lennew=nlstfail+100
               endif
               lstfail(nlstfail)=i
            endif
         enddo
         if(nlstfail.ne.0) then
            do  i=1,nlstfail
               lstptl(i)=lstfail(i)
            enddo
            nlstptl=nlstfail
 
         else
            nlstptl=0
         endif
      endif
      istep=1
      if (nlstptl.ne.0) then
         ntetmax=ntets
         call delaunay_connect(npoints,ntets,
     *      epsilon,ntetmax,nlsttts)

        if (ntets.le.0) call x3d_warn(isubname,'set info 0 elements')
        if (npoints.le.0) call x3d_warn(isubname,'set info 0 points')

         call cmo_set_info('nelements',cmo,ntets,1,1,icscode)
         call cmo_set_info('nnodes',cmo,npoints,1,1,icscode)
      endif
C     ******************************************************************
C
C     DUD-OUT POINTS THAT HAVE NO TETRAHEDRA ASSOCIATED WITH THEM.
C
      call cmo_get_info('nnodes',cmo,npoints,icmotype,leni,icscode)
      if(npoints+incpts.gt.lstptlen) then
         lstptlen=npoints+incpts
         call mmnewlen('lstptl',blkname,iplstptl,lstptlen,icscode)
      endif
C
C     Look for points flagged earlier as unconnected--reset type
C
      do i=1,npoints
         if(itp1(i).ge.1000) itp1(i)=itp1(i)-1000
         lstptl(i)=cvmgt(i,0,(itp1(i).ge.ifitpst1.and.
     $                           itp1(i).le.ifitpen1)
     $                          .or.
     $                          (itp1(i).ge.ifitpst2.and.
     $                           itp1(i).le.ifitpen2)
     $                     )
      enddo
      do  it=itetstrt,ntets
         lstptl(itet(1,it))=0
         lstptl(itet(2,it))=0
         lstptl(itet(3,it))=0
         lstptl(itet(4,it))=0
      enddo
      call kmprsn(npoints,lstptl(1),1,lstptl(1),1,lstptl(1),1,
     *               nlstptl)
      if(nlstptl.ne.0) then
         write(logmess,9930) nlstptl
 9930     format(' Dudding',i7,
     $             ' points that have no associated tetrahedra.')
         call writloga(cdefault,1,logmess,0,ierr)
         if(idebug.ge.6) then
            iunit=-1
            call hassign(iunit,'nn3dn_dud.inp',ierr)
            if (iunit.lt.0 .or. ierr.lt.0) then
            call x3d_warn(isubname,'hassign bad file unit')
            else
            write(iunit,*) nlstptl,0,0,0,0
            endif
         endif
         do  i=1,nlstptl
            k=lstptl(i)
            if(idebug.ge.6 .and. iunit.gt.0)
     *         write(iunit,*) i,xic(k),yic(k),zic(k),k,imt1(k)
            write(logmess,9940) xic(k),yic(k),zic(k),k,imt1(k)
 9940          format(3e15.7,x,2i7)
            if(i.le.30)then
               call writloga(cdefault,0,logmess,0,ierr)
               call writloga('bat',0,logmess,0,ierr)
            else
               call writloga('bat',0,logmess,0,ierr)
            endif
         enddo
         if(idebug.ge.6 .and. iunit.gt.0) close(iunit)
         do  ii=1,nlstptl
            itp1(lstptl(ii))=ifitpdud
         enddo
      endif
 
C
C
C     ******************************************************************
C
C     WRITE THE MESH-COMPLETION MESSAGE.
C
      if (nlstptl.ne.0) then
        write(logmess,'(a)')
     *  ' The mesh is complete but could not include all points.'
        call writloga(cdefault,1,logmess,0,ierr)
        write(logmess,'(a,i10)')
     *  ' Number of points that could not be connected: ',nlstptl
        call writloga(cdefault,0,logmess,1,ierr)
      else
        write(logmess,9980)
 9980   format(' The mesh is now complete!')
        call writloga(cdefault,1,logmess,1,ierr)
      endif
C
C
C     ******************************************************************
C
C     RELEASE THE TEMPORARY MEMORY BACK TO THE MEMORY MANAGER.
C
      call mmrelprt(blkname,icscode)
C
C
C     ******************************************************************
C
C     ADD mbndry TO jtet OF BOUNDARY-TETRAHEDRON FACES.
C
      do it=itetstrt,ntets
               ifp1=itsttp('intrface',itp1(itet(1,it)))
               ifp2=itsttp('intrface',itp1(itet(2,it)))
               ifp3=itsttp('intrface',itp1(itet(3,it)))
               ifp4=itsttp('intrface',itp1(itet(4,it)))
         jtet(1,it)=cvmgt(mbndry+jtet(1,it),jtet(1,it),
     $                    jtet(1,it).eq.0.or.
     $                    (ifp2.and.ifp3.and.ifp4.and.
     $                     (jtet(1,it).gt.0.and.
     $                      jtet(1,it).lt.mbndry            )
     $                    )
     $                   )
         jtet(2,it)=cvmgt(mbndry+jtet(2,it),jtet(2,it),
     $                    jtet(2,it).eq.0.or.
     $                    (ifp1.and.ifp3.and.ifp4.and.
     $                     (jtet(2,it).gt.0.and.
     $                      jtet(2,it).lt.mbndry            )
     $                    )
     $                   )
         jtet(3,it)=cvmgt(mbndry+jtet(3,it),jtet(3,it),
     $                    jtet(3,it).eq.0.or.
     $                    (ifp1.and.ifp2.and.ifp4.and.
     $                     (jtet(3,it).gt.0.and.
     $                      jtet(3,it).lt.mbndry            )
     $                    )
     $                   )
         jtet(4,it)=cvmgt(mbndry+jtet(4,it),jtet(4,it),
     $                    jtet(4,it).eq.0.or.
     $                    (ifp1.and.ifp2.and.ifp3.and.
     $                     (jtet(4,it).gt.0.and.
     $                      jtet(4,it).lt.mbndry            )
     $                    )
     $                   )
      enddo
C
C     ******************************************************************
C     SET THE ERROR FLAG TO NORMAL.
C
      ierr1=0
C
C     ******************************************************************
C
C
C     ******************************************************************
C
      call cmo_get_name(cmo,icscode)
      call cmo_set_info('nnodes',cmo,npoints,1,1,icscode)
      call cmo_set_info('nelements',cmo,ntets,1,1,icscode)
      call set_info_i('ipointj',cmo,cglobal,cdefault,npoints,ier)
      call set_info_i('ipointi',cmo,cglobal,cdefault,npoints_save,
     *   ier)
 
      call set_mbndry()
 
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
      call cmo_get_info('nodes_per_element',cmo,
     *                        nen,leni,icmotype,ier)
      call cmo_get_info('faces_per_element',cmo,
     *                        nef,leni,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
C
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
C
      do it=1,ntets
         itettyp(it)=ifelmtet
         itetoff(it)=nen*(it-1)
         jtetoff(it)=nef*(it-1)
         itetclr(it)=imt1(itet1(itetoff(it)+1))
         do i=1,nef
            if(jtet1(jtetoff(it)+i).le.0) then
               jtet1(jtetoff(it)+i)=mbndry
            endif
         enddo
      enddo
C
 9999 continue

      if (ierr1.ne.0 .or. nerr.gt.0) then

        if (ierr1.ne.0) then 
          call list_erri(nerr+1,isubname,
     *       'ending with ierr1 flag:',ierr1)
        endif

        call list_err_end(nerr,isubname)

      endif

      return
      end
