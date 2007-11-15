      subroutine connect2d_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C     PURPOSE -
C
C        MAIN NEAREST-NEIGHBOR ROUTINE.  THIS ROUTINE FIRST CONNECTS THE
C        MESH DISREGARDING THE INTERFACES.  AFTERWARDS, POINTS ARE ADDED
C        TO BREAK MULTIMATERIAL CONNECTIONS.
C
C        FORMAT: connect
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
C        $Log: connect2d_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   08 Feb 2006 14:38:14   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.11   02 Aug 2005 08:09:58   gable

CPVCS    Changed ifadd to lifadd.

CPVCS    

CPVCS       Rev 1.10   15 Nov 2004 10:35:20   dcg

CPVCS    replace .or. with function ior

CPVCS    

CPVCS       Rev 1.9   30 Jan 2001 11:27:12   dcg

CPVCS    allow connecting a subset of nodes

CPVCS    

CPVCS       Rev 1.8   30 Jan 2001 08:24:40   dcg

CPVCS    pass input big triangle coordinates to the make_bigtri routine

CPVCS    

CPVCS       Rev 1.7   21 Apr 2000 07:04:24   gable

CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.

CPVCS    

CPVCS       Rev 1.6   28 Mar 2000 14:09:08   dcg

CPVCS    remove include 'machine.h'

CPVCS    

CPVCS       Rev 1.5   Thu Feb 17 08:37:42 2000   dcg

CPVCS    check for virtual nodes and set matlst correctly

CPVCS    

CPVCS       Rev 1.4   Tue Feb 08 13:52:26 2000   dcg

CPVCS    

CPVCS       Rev 1.3   Wed Feb 02 13:35:06 2000   dcg

CPVCS    

CPVCS       Rev 1.2   13 Jan 2000 14:47:40   dcg

CPVCS    

CPVCS       Rev 1.1   05 Jan 2000 17:32:14   dcg

CPVCS     

CPVCS
CPVCS       Rev 1.3   Tue Nov 30 13:14:52 1999   dcg
CPVCS    set nen to 3 for triangle meshes
CPVCS
CPVCS       Rev 1.2   Fri Nov 12 15:31:06 1999   dcg
CPVCS    check for collinear points
CPVCS
CPVCS       Rev 1.1   Wed Sep 01 12:21:46 1999   dcg
CPVCS    delete references to if4
CPVCS
CPVCS       Rev 1.0   Fri Mar 19 13:14:36 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      character*132 logmess
C
      integer ier,leni,icmotype,npoints,ntets,icscode,
     * ipt1,ipt2,ipt3,iout,lout,itype,
     * nwds,ifield,ipttyp,mpno,length,incpts,
     * lencns,ics,nmatfal1,nmatfal2,
     * it,i,j,nmatmax,ii,ierr,k,
     * iunit,nen,nef,nnodes,mbndry,
     * nlsttts,n,imtval,
     * matindex,iimt,i1,nlstptls,nlstptln
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*32 cmsgin(nwds),cmo
      pointer (ipitet,itet)
      pointer (ipitet,itet1)
      integer itet(3,*),itet1(*)
      pointer (ipjtet,jtet)
      pointer (ipjtet,jtet1)
      integer jtet(3,*),jtet1(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipisetwd,isetwd)
      integer isetwd(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitetclr,itetclr), (ipitettyp,itettyp)
      integer itetclr(*),itettyp(*)
      pointer (ipjtetoff,jtetoff), (ipitetoff,itetoff)
      integer jtetoff(*),itetoff(*)
C
      include 'chydro.h'
      include 'consts.h'
      include 'search.h'
      include 'local_element.h'
      include 'geom_lg.h'
C
      integer shiftl, ior
      external shiftl
C
C#######################################################################
C
      integer intconn, intpass,intpasmx,npoints_save,
     * ierr1,ntetmax,ierror
      integer ismax, icharlnf
      real*8 delx,dely,delz,crossx,crossy,crossz,sumsq,
     * cvmgt
      pointer (iprimat  ,rimat(*)     )
      pointer (ipmpary ,mpary(*)     )
      pointer (iplstptl,lstptl(*)    )
      real*8 rimat,rout
      pointer(ipout,out)
      real*8 out(*)
      integer mpary,lstptl
C        *** POINTERS RELATED TO MASS POINTS.
      character*8 cpart,cnnodes
      real*8 r
c
      pointer (ipnimts ,nimts(*)      )
      pointer (ipimts1 ,imts1(*)      )
      pointer (ipmatlst,matlst(*)    )
      integer  nimts,imts1,matlst
      character*32 isubname
      character*32 cpt1,cpt2,cpt3,geom_name
      character*8 cglobal,cdefault
      logical itsttp, ifp1,ifp2,ifp3
      real*8 xcoords(12),vvbarmin
      data cglobal,cdefault/'global','default'/
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
      isubname='nn3dn'
      cpart='part'
      cnnodes='nnodes'
C
      call cmo_get_name(cmo,ier)
C
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call cmo_get_info('nnodes',cmo,npoints,leni,icmotype,ier)
      call cmo_get_info('nelements',cmo,ntets,leni,icmotype,ier)
      call set_mbndry()
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
      call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
      npoints_save=npoints
C
C    SET POINT-INDEX BOUNDARIES.
C
 
      ipt1=1
      ipt2=npoints
      ipt3=1
      ipttyp=0
      ifield=2
      if (nwds.le.ifield) then
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
      call mmgetblk('vol',isubname,ipvol,ntetmax,2,icscode)
      call mmgetblk('xvor',isubname,ipxvor,ntetmax,2,icscode)
      call mmgetblk('yvor',isubname,ipyvor,ntetmax,2,icscode)
      call mmgetblk('zvor',isubname,ipzvor,ntetmax,2,icscode)
      call mmgetblk('lsttts',isubname,iplsttts,ntetmax,1,icscode)
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
      call mmgetblk('mpary',isubname,ipmpary,npoints,1,icscode)
      call mmgetblk('lstptl',isubname,iplstptl,lstptlen,1,icscode)
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
      call mmgetblk('lstfail',isubname,iplstfal,lstptlen,1,icscode)
C
C     SET THE REGION NAMES, OFFSETS, AND NUMBER OF ELEMENTS FOR ALL
C     REGIONS.
C
      if(nmregs.gt.0) then
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,
     *     ierror)
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
c
C
C     GET MEMORY FOR THE MATERIAL-LIST ARRAY.
C
      if(nmregs.ne.0) then
         nmatmax=nmregs
      else
         length=npoints
         call mmgetblk('rimat',isubname,iprimat,length,2,icscode)
         do 15 i=1,npoints
            rimat(i)=float(imt1(i))
   15    continue
         nmatmax=int(rimat(ismax(npoints,rimat(1),1))+0.1)
         do 16 i=1,npoints
            imt1(i)=int(rimat(i)+0.1)
   16    continue
         call mmrelblk('rimat',isubname,iprimat,icscode)
      endif
      matblks=max((nmatmax+31)/32,1)
C        *** NUMBER OF BLOCKS OF THE MATERIAL ARRAY.  EACH BLOCK WILL
C        *** HANDLE UP TO 32 MATERIALS.
      lenmatmx=(6*(npoints+4))/5
C        *** LENGTH OF EACH MATERIAL-LIST-ARRAY BLOCK.
      call mmgetblk('matlst',isubname,ipmatlst,matblks*lenmatmx,1,ics)
C
C     ******************************************************************
C     INITIALIZE SOME MORE VARIABLES.
C
      nstepdgn=3
C        *** MAXIMUM NUMBER OF ATTEMPTS TO BE MADE TO CONNECT UP THE
C        *** MESH DURING DEGENERACY CONNECTIONS WITHOUT ADDING MORE
C        *** POINTS.
      small=3.25e-10
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
      call mmgetblk('nimts',isubname,ipnimts,npoints,1,ics)
      length=npoints*max(imtmax,nmregs)
      call mmgetblk('imts1',isubname,ipimts1,length,1,ics)
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
               matlst(matindex+i)=ior(matlst(matindex+i)
     $          ,shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1))
         elseif(itsttp('intrface',itp1(i))) then
            if(nimts(i).gt.0) then
               do 23 iimt=1,nimts(i)
                  imtval=imts1(iimt+nmregs*(i-1))
                  matindex=((imtval-1)/32)*lenmatmx
                  matlst(matindex+i)=ior(matlst(matindex+i)
     $             ,shiftl(1,imtval-((imtval-1)/32)*32-1))
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
               matlst(matindex+i)=ior(matlst(matindex+i)
     $          ,shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1))
            else
               nmatfal2=nmatfal2+1
            endif
         endif
      enddo
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
         do j=1,9
            xcoords(j)=xmsgin(ifield+3+j)
         enddo
      endif
      call make_bigtri_lg(npoints,ntets,xcoords,nwds,ierr1)
      if(ierr1.ne.0) go to 9999
      write(logmess,"(' Coordinates of enclosing triangle are: ')")
      call writloga(cdefault,0,logmess,0,ierr)
      do j=1,3
        write(logmess,'(10x,3(d12.5))') xbigtet(j),ybigtet(j),
     *        zbigtet(j)
        call writloga(cdefault,0,logmess,0,ierr)
      enddo
C
C     CHECK IF ADDING POINTS IS TURNED OFF
C
      lifadd=.true.
      incpts=0
      if(nwds.eq.18) then
         if(cmsgin(18)(1:icharlnf(cmsgin(18))).eq.'noadd')
     *      lifadd=.false.
      endif
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
         do n=1,matblks
            matindex=(n-1)*lenmatmx
            matlst(matindex+ibigtet  )=-1
            matlst(matindex+ibigtet+1)=-1
            matlst(matindex+ibigtet+2)=-1
         enddo
         incpts=3
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
         call volume_tri(xic(ibigtet),yic(ibigtet),zic(ibigtet),
     *   xic(ibigtet+1),yic(ibigtet+1),zic(ibigtet+1),
     *   xic(ibigtet+2),yic(ibigtet+2),zic(ibigtet+2),
     *   vol(1))
         call voronoi_center_2d(xic(ibigtet),yic(ibigtet),zic(ibigtet),
     *   xic(ibigtet+1),yic(ibigtet+1),zic(ibigtet+1),
     *   xic(ibigtet+2),yic(ibigtet+2),zic(ibigtet+2),
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
      istep=1
      call delaunay_connect2d_lg(npoints,ntets,epsilon,
     *  ntetmax,nlsttts)
 
C
C  refresh pointers
C
      call mmfindbk('lstptl',isubname,iplstptl,lennew,icscode)
      call mmfindbk('xvor',isubname,ipxvor,lennew,icscode)
      call mmfindbk('yvor',isubname,ipyvor,lennew,icscode)
      call mmfindbk('zvor',isubname,ipzvor,lennew,icscode)
      call mmfindbk('lsttts',isubname,iplsttts,lennew,icscode)
      call mmfindbk('vol',isubname,ipvol,lennew,icscode)
      call mmfindbk('lstfail',isubname,iplstfal,lennew,icscode)
      call cmo_set_info('nelements',cmo,ntets,1,1,icscode)
      call cmo_set_info('nnodes',cmo,npoints,1,1,icscode)
C
C  find and break multimaterial connections
C
C  current multi_material breaking algorithm needs the enclosing bigtet
C  if  a new algorithm is written that does not need the enclosing tet
C  uncomment out the next two statements
c      call remove_bigtri_lg()
c      ibigtet=0
C
C  Skip adding points if instructed on command line
C
      if(.not.lifadd) go to 9900
      call multi_material2d_lg()
C
C
      call cmo_get_info('nelements',cmo,ntets,icmotype,leni,icscode)
      call cmo_get_info('nnodes',cmo,npoints,icmotype,leni,icscode)
C
C     ******************************************************************
C
C     REMOVE ALL TETRAHEDRA ASSOCIATED WITH THE BIG TETRAHEDRON AND FILL
C     HOLES.
C
 9900 if(iremove.ne.0.and.ibigtet.gt. 0) then
 
         call remove_bigtri_lg()
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
         call delaunay_connect2d_lg (npoints,ntets,
     *      epsilon,ntetmax,nlsttts)
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
         call mmnewlen('lstptl',isubname,iplstptl,lstptlen,icscode)
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
            write(iunit,*) nlstptl,0,0,0,0
         endif
         do  i=1,nlstptl
            k=lstptl(i)
            if(idebug.ge.6)
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
         if(idebug.ge.6) close(iunit)
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
      write(logmess,9980)
 9980 format(' The mesh is now complete!')
      call writloga(cdefault,1,logmess,1,ierr)
C
C
C     ******************************************************************
C
C     RELEASE THE TEMPORARY MEMORY BACK TO THE MEMORY MANAGER.
C
      call mmrelprt(isubname,icscode)
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
         jtet(1,it)=cvmgt(mbndry+jtet(1,it),jtet(1,it),
     $                    jtet(1,it).eq.0.or.
     $                    (ifp2.and.ifp3.and.
     $                     (jtet(1,it).gt.0.and.
     $                      jtet(1,it).lt.mbndry            )
     $                    )
     $                   )
         jtet(2,it)=cvmgt(mbndry+jtet(2,it),jtet(2,it),
     $                    jtet(2,it).eq.0.or.
     $                    (ifp1.and.ifp3.and.
     $                     (jtet(2,it).gt.0.and.
     $                      jtet(2,it).lt.mbndry            )
     $                    )
     $                   )
         jtet(3,it)=cvmgt(mbndry+jtet(3,it),jtet(3,it),
     $                    jtet(3,it).eq.0.or.
     $                    (ifp1.and.ifp2.and.
     $                     (jtet(3,it).gt.0.and.
     $                      jtet(3,it).lt.mbndry            )
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
 
      call set_mbndry()
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
 
      call set_info_i('ipointj',cmo,cglobal,cdefault,npoints,ier)
      call set_info_i('ipointi',cmo,cglobal,cdefault,npoints_save,
     *   ier)
c
c  nen should be 3 for triangle mesh
c
      nen=3
      call cmo_set_info('nodes_per_element',cmo,
     *                        nen,1,1,ier)
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
         itettyp(it)=ifelmtri
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
      return
      end
