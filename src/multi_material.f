      subroutine multi_material(intrface_search)
C
C#######################################################################
C
C     PURPOSE -
C       look for multimaterial connections in existing mesh
C       attempt to break these connections by adding points
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/multi_material_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.11   26 Jul 2006 15:31:46   gable
CPVCS    Recover previous edits, remove PC type line returns, merge rev 1.9 1.10.
CPVCS    
CPVCS       Rev 1.9   28 Jul 2005 15:22:54   gable
CPVCS    Add check_interface option. Made some changes to screen and
CPVCS    outx3dgen output. check_interface option results in a larger
CPVCS    number of candidate edges searched for material interface crossing
CPVCS    but there are situations where that is required to get correct
CPVCS    results.
CPVCS    
CPVCS       Rev 1.8   30 Sep 2004 09:45:12   dcg
CPVCS    use ior in place of .or. with integers
CPVCS    
CPVCS       Rev 1.7   10 Apr 2001 13:15:18   dcg
CPVCS    use iand function in place of .and.
CPVCS    
CPVCS       Rev 1.6   Wed Apr 05 13:34:48 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.4   Thu Feb 17 08:37:32 2000   dcg
CPVCS    check for virtual nodes and set matlst correctly
CPVCS    
CPVCS       Rev 1.3   02 Feb 2000 17:15:32   dcg
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:48:16   dcg
CPVCS    
CPVCS       Rev 1.1   05 Jan 2000 17:33:06   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.17   Mon Aug 30 15:58:42 1999   dcg
CPVCS    refresh cmo pointers after call to delaunay connect
CPVCS
CPVCS       Rev 1.16   Tue Aug 10 10:12:28 1999   dcg
CPVCS    update length of matlst correctly
CPVCS
CPVCS       Rev 1.15   Tue May 18 14:31:06 1999   dcg
CPVCS    make sure icontab entries occur in increasing
CPVCS    surface number order
CPVCS
CPVCS       Rev 1.14   Mon Nov 16 16:59:40 1998   dcg
CPVCS    fix bug that caused interface/boundary point types to
CPVCS    be set incorrectly
CPVCS
CPVCS       Rev 1.13   Wed Aug 19 15:11:12 1998   dcg
CPVCS    increment storage for icontab if new entries are
CPVCS    required because added points fall on combination of
CPVCS    surface not previously encountered
CPVCS
CPVCS       Rev 1.12   Tue Aug 11 16:30:58 1998   dcg
CPVCS    fix problem with number of nodes incorrect , if fail in
CPVCS    placing all nodes
CPVCS
CPVCS       Rev 1.11   Thu Jul 02 14:02:24 1998   dcg
CPVCS    allow more than 32 materials
CPVCS
CPVCS       Rev 1.10   Fri Jun 19 09:40:20 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.9   Fri May 29 14:46:52 1998   dcg
CPVCS    add changes required by closed_surfaces implementation
CPVCS
CPVCS       Rev 1.8   Fri May 22 13:13:58 1998   dcg
CPVCS    reset nnodes if no new points added
CPVCS
CPVCS
CPVCS       Rev 1.7   Mon Mar 16 15:35:10 1998   dcg
CPVCS    set point type for interface points on external
CPVCS    boundaries that are added to break multi-material
CPVCS    connections
CPVCS
CPVCS       Rev 1.6   Wed Dec 17 12:14:20 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.5   Mon Nov 24 16:34:24 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.3   Wed Oct 08 15:44:18 1997   dcg
CPVCS    add logical declarations
CPVCS
CPVCS       Rev 1.0   Mon Aug 18 14:57:24 1997   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'cmo.h'
      include 'chydro.h'
      include 'consts.h'
      include 'machine.h'
      include 'geom_lg.h'
      character*32 geom_name
      character*32 isubname
      integer shiftl,popcnt
      external shiftl
      logical itsttp,icrmatch
      real*8 cvmgt,cvmgz
      character*132 logmess
C
      integer icscode,iout,lout,itype,idum1,
     *  leni,icmotype,ierr,ip1,ip2,nn,imtval,
     *  i,iimt,i1,length,nmatfal1,nmatfal2,matindex,
     *  it,itetl1,itetl2,itetl3,itetl4,nconbnd,
     *  nlstttsn,icount,lencns,nlstcns,nlstcnsn,
     *  nintfail,nptsnew,intpasmx,intpass,
     *  itestmax,itestmin,lng,len1,iic,iic1,iic2,iface,j,k,
     *  npoints,nmatmax,ntets,nlsttts,iit,n,
     *  intconn,nlstptls,ii,lenmatn,iaddint,ntetmax, nmatmax_m1
      integer intrface_search
      integer ifdebug
 
      real*8 d1,d2,rout
      pointer(ipout,out)
      real*8 out(*)
      logical ifp1,ifp2,ifp3,ifp4,fail
C
      pointer (ipisurftst,isurftst)
      integer isurftst(*)
      pointer (ipicsurf,icsurf)
      integer icsurf(50,*)
      pointer (iplstptl,lstptl(1000000)    )
      integer lstptl
 
      character*8 cnnodes,ickin
 
      pointer (ipnimts ,nimts(1000000)      )
      pointer (ipimts1 ,imts1(1000000)      )
      integer nimts,imts1
      pointer (ipicontab, icontab)
      integer icontab(50,1000000)
C
      pointer (ipmatlst,matlst(1000000)    )
      integer matlst
C
      pointer (ipx1, x1(1000000))
      pointer (ipy1, y1(1000000))
      pointer (ipz1, z1(1000000))
      pointer (ipx2, x2(1000000))
      pointer (ipy2, y2(1000000))
      pointer (ipz2, z2(1000000))
      pointer (ipxi, xi(1000000))
      pointer (ipyi, yi(1000000))
      pointer (ipzi, zi(1000000))
      pointer (iplist,list(1000000))
      pointer (ipnconew,nconew(1000000))
      pointer (ipilist,ilist(2,1000000))
      pointer (ipweight,weight(2,1000000))
      integer list,ilist,nconew,incpts,ns
      real*8 x1,y1,z1,x2,y2,z2,xi,yi,zi,weight
      pointer (ipint1,int1    )
      integer int1(1000000)
C
      data isubname/'multimat'/
      data intpasmx/20/
      data cnnodes/'nnodes'/
      data ickin/'eq'/
C
C     ******************************************************************
C  
C  If big tet exists ibigtet > 0 then need to save
C  room for extra 4 points
C
      incpts=0
      if(ibigtet.gt.0) incpts=4
C
C  ACCESS the MESH OBJECT
C
      call cmo_get_name(cmo,ierr)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,icscode)
      call cmo_get_info('nnodes',cmo,nnodes,leni,itype,ierr)
      call cmo_get_info('nelements',cmo,ntets,leni,itype,ierr)
      call cmo_get_info('idebug',cmo,ifdebug,leni,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,leni,itype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,leni,itype,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,leni,itype,ierr)
      call cmo_get_info('xic',cmo,ipxic,leni,itype,ierr)
      call cmo_get_info('yic',cmo,ipyic,leni,itype,ierr)
      call cmo_get_info('zic',cmo,ipzic,leni,itype,ierr)
      call cmo_get_info('itet',cmo,ipitet,leni,itype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,leni,itype,ierr)
      call cmo_get_info('nconbnd',cmo,nconbnd,leni,itype,ierr)
      call cmo_get_info('icontab',cmo,ipicontab,leni,itype,ierr)
      npoints=nnodes
C
C     GET THE REGION, SURFACE NAMES, OFFSETS, AND NUMBER FOR ALL
C     REGIONS,SURFACES
C
      if(nmregs.gt.0) then
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierr)
         call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,ierr)
         call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierr)
         call mmfindbk('mregdef',geom_name,ipmregdef,length,ierr)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierr)
      endif
      if(nsurf.gt.0) then
         call mmfindbk('csall',geom_name,ipcsall,length,ierr)
         call mmfindbk('istype',geom_name,ipistype,length,ierr)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierr)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierr)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierr)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierr)
      endif
C
C  LOOK FOR MATERIAL LIST IF NOT THERE CREATE IT
C
      if(nmregs.ne.0) then
         nmatmax=nmregs
      else
         do  i=1,npoints
            nmatmax=max(1,imt1(i))
            imt1(i)=max(1,imt1(i))
         enddo
      endif
      nmatmax_m1 = nmatmax - 1
C     
C        *** NUMBER OF BLOCKS OF THE MATERIAL ARRAY.  EACH BLOCK WILL
C        *** HANDLE UP TO 32 MATERIALS.
      matblks=max((nmatmax+31)/32,1)
C        *** LENGTH OF EACH MATERIAL-LIST-ARRAY BLOCK.
      lenmatmx=(6*(npoints+incpts))/5
      call mmfindbk('matlst',nname,ipmatlst,length,icscode)
      if (icscode.eq.0) then
         if(matblks*lenmatmx.gt.length)
     *      call mmnewlen('matlst',nname,ipmatlst,matblks*lenmatmx,
     *         ierr)
      else
         call mmgetblk('matlst',nname,ipmatlst,matblks*lenmatmx,1,
     *         ierr)
C
C     SET UP THE MATERIAL-LIST ARRAY.
C
         imtmax=nmatmax
         length=npoints
         call mmgetblk('nimts',isubname,ipnimts,npoints,1,icscode)
         length=npoints*max(imtmax,nmregs)
         call mmgetblk('imts1',isubname,ipimts1,length,1,icscode)
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
         do i=1,npoints
            if(nimts(i).eq.0.and.itsttp('virtual',itp1(i))) then
               if(imt1(i).ne.0) then
                  matindex=((imt1(i)-1)/32)*lenmatmx
c                  matlst(matindex+i)=ior(matlst(matindex+i),
c     $               shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1))
                   i1=shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1)
                   matlst(matindex+i)=ior(matlst(matindex+i),i1)
               else
                  nmatfal2=nmatfal2+1
               endif
            elseif(itsttp('intrface',itp1(i))) then
               if(nimts(i).gt.0) then
                  do iimt=1,nimts(i)
                    imtval=imts1(iimt+nmregs*(i-1))
                    matindex=((imtval-1)/32)*lenmatmx
c                    matlst(matindex+i)=ior(matlst(matindex+i),
c     $                 shiftl(1,imtval-((imtval-1)/32)*32-1))
                     i1=shiftl(1,imtval-((imtval-1)/32)*32-1)
                     matlst(matindex+i)=ior(matlst(matindex+i),i1)
                  enddo
               else
                  nmatfal1=nmatfal1+1
               endif
            elseif((itp1(i).ge.ifitpst1.and.itp1(i).le.ifitpen1)
     $          .or.
     $          (itp1(i).ge.ifitpst2.and.itp1(i).le.ifitpen2)
     $           ) then
               if(imt1(i).ne.0) then
                  matindex=((imt1(i)-1)/32)*lenmatmx
c                  matlst(matindex+i)=ior(matlst(matindex+i),
c     $               shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1))
                   i1=shiftl(1,imt1(i)-((imt1(i)-1)/32)*32-1)
                   matlst(matindex+i)=ior(matlst(matindex+i),i1)
               else
                  nmatfal2=nmatfal2+1
               endif
            endif
         enddo
         if(nmatfal1.ne.0) then
            write(logmess,25) nmatfal1
   25       format(' Cannot find materials for',i10,
     *           ' interface points.')
            call writloga('default',1,logmess,0,ierr)
         endif
         if(nmatfal2.ne.0) then
            write(logmess,26) nmatfal2
   26       format(' No materials associated with',i10,
     $          ' noninterface points.')
            call writloga('default',0,logmess,0,ierr)
         endif
      endif
C
C     ******************************************************************
C
C     CHECK FOR MULTIMATERIAL TETRAHEDRA, AND IF NECESSARY, ADD POINTS
C     TO ELIMINATE THE MULTIMATERIAL CONNECTIONS.  MULTIPLE PASSES ARE
C     MADE THROUGH THIS SECTION.
C
C     PASS 1:             POINTS ARE ADDED TO BREAK NONINTERFACE
C                         MULTIMATERIAL CONNECTIONS.
C
C     PASSES 2--intpasmx: SOME CONNECTIONS ORIGINATING FROM INTERFACE
C                         POINTS MAY ALSO BE MULTIMATERIAL.  POINTS ARE
C                         ADDED TO BREAK ALL SUCH CONNECTIONS, AND ANY
C                         NEW MULTIMATERIAL CONNECTIONS THAT MIGHT HAVE
C                         BEEN INTRODUCED IN THE PREVIOUS PASSES.
C
      lencns=npoints
      intpass=0
      call mmfindbk('lstcns1',nname,iplstcns1,leni,icscode)
      if (icscode.ne.0) then
         call mmgetblk('lstcns1',nname,iplstcns1,lencns,1,icscode)
         call mmgetblk('lstcns2',nname,iplstcns2,lencns,1,icscode)
      else
      call mmfindbk('lstcns2',nname,iplstcns2,leni,icscode)
         if (leni.lt.lencns) then
           call mmnewlen('lstcns1',nname,iplstcns1,lencns,icscode)
           call mmnewlen('lstcns2',nname,iplstcns2,lencns,icscode)
         endif
      endif
      call mmfindbk('lsttts',nname,iplsttts,leni,icscode)
      if (icscode.ne.0) then
         call mmgetblk('lsttts',nname,iplsttts,ntets,1,icscode)
      else
         if (leni.lt.ntets)
     *   call mmnewlen('lsttts',nname,iplsttts,ntets,icscode)
      endif
      iaddint=1
      if(iaddint.ne.0) then
 8990    continue
         intpass=intpass+1
         if(intpass.gt.intpasmx) then
            write(logmess,8991) intpasmx
 8991       format(' Maximum number of multi-material iterations',
     *         ' exceeded ',i5)
            call writloga('default',0,logmess,0,ierr)
            go to 9999
         endif
         idelaun=0
C
C        MAKE A LIST OF MULTIMATERIAL TETRAHEDRA.
C
         if(intpass.eq.1) then
C
C           PASS 1.  MAKE A LIST OF TETRAHEDRA THAT ARE DEFINITELY
C           MULTIMATERIAL.  THIS LIST EXCLUDES INTERFACE TETRAHEDRA.
C
            do it=1,ntetexcl
               lsttts(it)=0
            enddo
            call mmgetblk('int1',isubname,ipint1,npoints+incpts,
     *         1,ierr)
            if(ierr.ne.0) call x3d_error(isubname,'mmgetblk')
            call unpacktp('intrface','set',npoints+incpts,ipitp1,
     *                     ipint1,icscode)
            do 9005 it=itetstrt,ntets
               itetl1=itet(1,it)
               itetl2=itet(2,it)
               itetl3=itet(3,it)
               itetl4=itet(4,it)
               itestmax=0
               itestmin=99999
               if(int1(itetl1).eq.0) then
                  itestmin=min(itestmin,imt1(itetl1))
                  itestmax=max(itestmax,imt1(itetl1))
               endif
               if(int1(itetl2).eq.0) then
                  itestmin=min(itestmin,imt1(itetl2))
                  itestmax=max(itestmax,imt1(itetl2))
               endif
               if(int1(itetl3).eq.0) then
                  itestmin=min(itestmin,imt1(itetl3))
                  itestmax=max(itestmax,imt1(itetl3))
               endif
               if(int1(itetl4).eq.0) then
                  itestmin=min(itestmin,imt1(itetl4))
                  itestmax=max(itestmax,imt1(itetl4))
               endif
               if(itestmin.eq.99999) itestmin=0
               lsttts(it)=cvmgt(0,it,itestmax.eq.itestmin)
 9005       continue
            call kmprsn(ntets,lsttts(1),1,lsttts(1),1,lsttts(1),1,
     $                  nlsttts)
C
         else
C
C           ............................................................
C           PASSES 2--intpasmx.  MAKE A LIST OF ALL MULTIMATERIAL
C           TETRAHEDRA (INTERFACE MULTIMATERIAL TETRAHEDRA AND ANY
C           REMAINING MULTIMATERIAL TETRAHEDRA).
C
C           FIRST MAKE A PRELIMINARY LIST THAT INCLUDES ALL INTERFACE
C           TETRAHEDRA IN ADDITION TO ANY OTHER MULTIMATERIAL
C           TETRAHEDRA.
C
            do  it=1,ntetexcl
               lsttts(it)=0
            enddo
            do it=itetstrt,ntets
               itetl1=itet(1,it)
               itetl2=itet(2,it)
               itetl3=itet(3,it)
               itetl4=itet(4,it)
               ifp1=itsttp('intrface',itp1(itetl1))
               ifp2=itsttp('intrface',itp1(itetl2))
               ifp3=itsttp('intrface',itp1(itetl3))
               ifp4=itsttp('intrface',itp1(itetl4))
               lsttts(it)=cvmgt(it,0,max(imt1(itetl1),imt1(itetl2),
     $                                    imt1(itetl3),imt1(itetl4)).ne.
     $                               min(imt1(itetl1),imt1(itetl2),
     $                                    imt1(itetl3),imt1(itetl4))
     $                               .or.
     $                               ifp1.or.ifp2.or.ifp3.or.ifp4 )
            enddo
            call kmprsn(ntets,lsttts(1),1,lsttts(1),1,lsttts(1),1,
     $                  nlsttts)
C
C           NOW ELIMINATE TETRAHEDRA FROM THE LIST THAT ARE NOT
C           MULTIMATERIAL BY VIRTUE OF THE FACT THAT THERE IS AT LEAST
C           ONE COMMON MATERIAL SHARED BY THE POINT PAIR.
C cwg          ONE COMMON MATERIAL SHARED BY ALL FOUR POINTS.
C
            do iit=1,nlsttts
               lsttts(iit)=-lsttts(iit)
            enddo
            do  n=1,matblks
               matindex=(n-1)*lenmatmx
               do  iit=1,nlsttts
                  itetl1=itet(1,iabs(lsttts(iit)))
                  itetl2=itet(2,iabs(lsttts(iit)))
                  itetl3=itet(3,iabs(lsttts(iit)))
                  itetl4=itet(4,iabs(lsttts(iit)))
                  ifp1=itsttp('intrface',itp1(itetl1))
                  ifp2=itsttp('intrface',itp1(itetl2))
                  ifp3=itsttp('intrface',itp1(itetl3))
                  ifp4=itsttp('intrface',itp1(itetl4))
C
C      Return positive element number if smallest number of common materials is zero.
C      Return negative element number if smallest number of common materials is non-zero.
C
                  lsttts(iit)=cvmgz(iabs(lsttts(iit)),lsttts(iit),
     $             min(popcnt(iand(matlst(matindex+itetl1),
     $                         matlst(matindex+itetl2))),
     $                  popcnt(iand(matlst(matindex+itetl1),
     $                         matlst(matindex+itetl3))),
     $                  popcnt(iand(matlst(matindex+itetl1),
     $                         matlst(matindex+itetl4))),
     $                  popcnt(iand(matlst(matindex+itetl2),
     $                         matlst(matindex+itetl3))),
     $                  popcnt(iand(matlst(matindex+itetl2),
     $                         matlst(matindex+itetl4))),
     $                  popcnt(iand(matlst(matindex+itetl3),
     $                         matlst(matindex+itetl4)))
     $                 )
     $                             )

C
C   If imt value of all nodes in an element is nmatmax then
C   keep it in the list. This means we keep elements that have all interface nodes
C   and the imt value is nmatmax. The assumption is that intrface nodes have
C   imt = nmatmax
C
                  if(intrface_search .eq. 1)then
                  if((imt1(itetl1).eq.nmatmax).and.
     1               (imt1(itetl2).eq.nmatmax).and.
     2               (imt1(itetl3).eq.nmatmax).and.
     3               (imt1(itetl4).eq.nmatmax))then
                         lsttts(iit) = iabs(lsttts(iit))
                  endif
                  endif
c                endif
               enddo
            enddo
C
C Compress the lsttts list keeping only the positive values
C
            call kmprsp(nlsttts,lsttts(1),1,lsttts(1),1,lsttts(1),1,
     $                  nlstttsn)
            nlsttts=nlstttsn
         endif
C
C
C        SKIP THE REST OF THE LOGIC IF THERE ARE NO MULTIMATERIAL
C        TETRAHEDRA.
C
         if(nlsttts.eq.0) goto 9490
C
C        _______________________________________________________________
C
C        INCREMENT MEMORY.
C
 
         if(6*nlsttts.gt.lencns) then
            lencns=nlsttts
            call mmnewlen('lstcns1',nname,iplstcns1,6*nlsttts,
     $                    icscode)
            call mmnewlen('lstcns2',nname,iplstcns2,6*nlsttts,
     $                    icscode)
         endif
C
C        _______________________________________________________________
C
C        MAKE A LIST OF MULTIMATERIAL CONNECTIONS.
C
         if(intpass.eq.1) then
C
C           ............................................................
C           PASS 1.  MAKE A LIST OF NONINTERFACE MULTIMATERIAL
C           CONNECTIONS.
C
            do iit=1,nlsttts
               itetl1=itet(1,lsttts(iit))
               itetl2=itet(2,lsttts(iit))
               itetl3=itet(3,lsttts(iit))
               itetl4=itet(4,lsttts(iit))
               ifp1=itsttp('intrface',itp1(itetl1))
               ifp2=itsttp('intrface',itp1(itetl2))
               ifp3=itsttp('intrface',itp1(itetl3))
               ifp4=itsttp('intrface',itp1(itetl4))
               if(imt1(itetl1).eq.imt1(itetl2) .or.
     $             ifp1.or.ifp2) then
                  lstcns1(6*iit-5)=0
                  lstcns2(6*iit-5)=0
               else
                  lstcns1(6*iit-5)=min(itetl1,itetl2)
                  lstcns2(6*iit-5)=max(itetl1,itetl2)
               endif
               if(imt1(itetl1).eq.imt1(itetl3) .or.
     $             ifp1.or.ifp3) then
                  lstcns1(6*iit-4)=0
                  lstcns2(6*iit-4)=0
               else
                  lstcns1(6*iit-4)=min(itetl1,itetl3)
                  lstcns2(6*iit-4)=max(itetl1,itetl3)
               endif
               if(imt1(itetl1).eq.imt1(itetl4) .or.
     $             ifp1.or.ifp4) then
                  lstcns1(6*iit-3)=0
                  lstcns2(6*iit-3)=0
               else
                  lstcns1(6*iit-3)=min(itetl1,itetl4)
                  lstcns2(6*iit-3)=max(itetl1,itetl4)
               endif
               if(imt1(itetl2).eq.imt1(itetl3) .or.
     $             ifp2.or.ifp3) then
                  lstcns1(6*iit-2)=0
                  lstcns2(6*iit-2)=0
               else
                  lstcns1(6*iit-2)=min(itetl2,itetl3)
                  lstcns2(6*iit-2)=max(itetl2,itetl3)
               endif
               if(imt1(itetl2).eq.imt1(itetl4) .or.
     $             ifp2.or.ifp4) then
                  lstcns1(6*iit-1)=0
                  lstcns2(6*iit-1)=0
               else
                  lstcns1(6*iit-1)=min(itetl2,itetl4)
                  lstcns2(6*iit-1)=max(itetl2,itetl4)
               endif
               if(imt1(itetl3).eq.imt1(itetl4) .or.
     $             ifp3.or.ifp4) then
                  lstcns1(6*iit  )=0
                  lstcns2(6*iit  )=0
               else
                  lstcns1(6*iit  )=min(itetl3,itetl4)
                  lstcns2(6*iit  )=max(itetl3,itetl4)
               endif
            enddo
C
         else
C
C           ............................................................
C           PASSES 2--intpasmx.  MAKE A LIST OF INTERFACE
C           MULTIMATERIAL CONNECTIONS, AND ANY REMAINING
C           MULTIMATERIAL CONNECTIONS.
C
            do iit=1,nlsttts
               itetl1=itet(1,lsttts(iit))
               itetl2=itet(2,lsttts(iit))
               itetl3=itet(3,lsttts(iit))
               itetl4=itet(4,lsttts(iit))
               lstcns1(6*iit-5)=min(itetl1,itetl2)
               lstcns2(6*iit-5)=max(itetl1,itetl2)
               lstcns1(6*iit-4)=min(itetl1,itetl3)
               lstcns2(6*iit-4)=max(itetl1,itetl3)
               lstcns1(6*iit-3)=min(itetl1,itetl4)
               lstcns2(6*iit-3)=max(itetl1,itetl4)
               lstcns1(6*iit-2)=min(itetl2,itetl3)
               lstcns2(6*iit-2)=max(itetl2,itetl3)
               lstcns1(6*iit-1)=min(itetl2,itetl4)
               lstcns2(6*iit-1)=max(itetl2,itetl4)
               lstcns1(6*iit  )=min(itetl3,itetl4)
               lstcns2(6*iit  )=max(itetl3,itetl4)
            enddo
            
            do  n=1,matblks
               matindex=(n-1)*lenmatmx
               do  iit=1,nlsttts
                  itetl1=itet(1,lsttts(iit))
                  itetl2=itet(2,lsttts(iit))
                  itetl3=itet(3,lsttts(iit))
                  itetl4=itet(4,lsttts(iit))
                  lstcns1(6*iit-5)=cvmgz(lstcns1(6*iit-5),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl2))
     $                                    )
     $                                  )
                  lstcns2(6*iit-5)=cvmgz(lstcns2(6*iit-5),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl2))
     $                                    )
     $                                  )
                  
                  lstcns1(6*iit-4)=cvmgz(lstcns1(6*iit-4),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl3))
     $                                    )
     $                                  )
                  lstcns2(6*iit-4)=cvmgz(lstcns2(6*iit-4),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl3))
     $                                    )
     $                                  )
                  lstcns1(6*iit-3)=cvmgz(lstcns1(6*iit-3),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
                  lstcns2(6*iit-3)=cvmgz(lstcns2(6*iit-3),0,
     $                              popcnt(iand(matlst(matindex+itetl1),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
                  lstcns1(6*iit-2)=cvmgz(lstcns1(6*iit-2),0,
     $                              popcnt(iand(matlst(matindex+itetl2),
     $                                     matlst(matindex+itetl3))
     $                                    )
     $                                  )
                  lstcns2(6*iit-2)=cvmgz(lstcns2(6*iit-2),0,
     $                              popcnt(iand(matlst(matindex+itetl2),
     $                                     matlst(matindex+itetl3))
     $                                    )
     $                                  )
                  lstcns1(6*iit-1)=cvmgz(lstcns1(6*iit-1),0,
     $                              popcnt(iand(matlst(matindex+itetl2),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
                  lstcns2(6*iit-1)=cvmgz(lstcns2(6*iit-1),0,
     $                              popcnt(iand(matlst(matindex+itetl2),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
                  lstcns1(6*iit  )=cvmgz(lstcns1(6*iit  ),0,
     $                              popcnt(iand(matlst(matindex+itetl3),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
                  lstcns2(6*iit  )=cvmgz(lstcns2(6*iit  ),0,
     $                              popcnt(iand(matlst(matindex+itetl3),
     $                                     matlst(matindex+itetl4))
     $                                    )
     $                                  )
C
C   When intrface_search is turned on, keep connections between pairs
C   of nodes with imt = nmatmax. This will result in a lot of edges
C   along an interface being candidates and no interface will be found
C   but it will insure that edges between interfaces that also cross
C   an interface will be detected.
C
               if(intrface_search .eq. 1)then
                  if((imt1(itetl1).eq.nmatmax).and.
     1               (imt1(itetl2).eq.nmatmax))then
                     lstcns1(6*iit-5)=min(itetl1,itetl2)
                     lstcns2(6*iit-5)=max(itetl1,itetl2)
                  endif
                  if((imt1(itetl1).eq.nmatmax).and.
     1               (imt1(itetl3).eq.nmatmax))then
                     lstcns1(6*iit-4)=min(itetl1,itetl3)
                     lstcns2(6*iit-4)=max(itetl1,itetl3)
                  endif
                  if((imt1(itetl1).eq.nmatmax).and.
     1               (imt1(itetl4).eq.nmatmax))then
                     lstcns1(6*iit-3)=min(itetl1,itetl4)
                     lstcns2(6*iit-3)=max(itetl1,itetl4)
                  endif
                  if((imt1(itetl2).eq.nmatmax).and.
     1               (imt1(itetl3).eq.nmatmax))then
                     lstcns1(6*iit-2)=min(itetl2,itetl3)
                     lstcns2(6*iit-2)=max(itetl2,itetl3)
                  endif
                  if((imt1(itetl2).eq.nmatmax).and.
     1               (imt1(itetl4).eq.nmatmax))then
                     lstcns1(6*iit-1)=min(itetl2,itetl4)
                     lstcns2(6*iit-1)=max(itetl2,itetl4)
                  endif
                  if((imt1(itetl3).eq.nmatmax).and.
     1               (imt1(itetl4).eq.nmatmax))then
                     lstcns1(6*iit-0)=min(itetl3,itetl4)
                     lstcns2(6*iit-0)=max(itetl3,itetl4)
                  endif
               endif
               enddo
            enddo
         endif
         call kmprsn(6*nlsttts,lstcns1(1),1,lstcns1(1),1,lstcns1(1),1,
     $               nlstcns)
         call kmprsn(6*nlsttts,lstcns2(1),1,lstcns2(1),1,lstcns2(1),1,
     $               nlstcns)
C
C        _______________________________________________________________
C
C        SKIP THE REST OF THE LOGIC IF THERE ARE NO MULTIMATERIAL
C        CONNECTIONS.
C
         if(nlstcns.eq.0) goto 9490
C
C        _______________________________________________________________
C
C        ELIMINATE DUPLICATE CONNECTIONS FROM THE CONNECTION LIST.
C
         do  iic1=1,nlstcns
            if(lstcns1(iic1).ne.0) then
               do  iic2=iic1+1,nlstcns
                  if(lstcns1(iic1).eq.lstcns1(iic2).and.
     $               lstcns2(iic1).eq.lstcns2(iic2)
     $              ) then
                     lstcns1(iic2)=0
                     lstcns2(iic2)=0
                  endif
               enddo
            endif
         enddo
         call kmprsn(nlstcns,lstcns1(1),1,lstcns1(1),1,lstcns1(1),1,
     $               nlstcnsn)
         call kmprsn(nlstcns,lstcns2(1),1,lstcns2(1),1,lstcns2(1),1,
     $               nlstcnsn)
         nlstcns=nlstcnsn

       write(logmess,9170) nlstcns
 9170  format(' Candidate connections that may cross boundaries ='
     1 ,i10)
       call writloga('default',0,logmess,0,ierr)
C
C        _______________________________________________________________
C
C        INCREASE MEMORY, IF NECESSARY.
C
         nnodes=npoints+incpts+nlstcns*2
         call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
         call cmo_newlen(cmo,ierr)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
         call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierr)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
         call mmfindbk('lstptl',nname,iplstptl,lstptlen,ierr)
         if(nlstcns.gt.lstptlen) then
            lstptlen=nlstcns
            call mmnewlen('lstptl',nname,iplstptl,lstptlen,
     $                    icscode)
            call mmnewlen('lstfail',nname,iplstfal,lstptlen,
     $                    icscode)
         else
           call mmfindbk('lstfail',nname,iplstfal,lstptlen,ierr)
         endif
C
C        _______________________________________________________________
C
C        ADD INTERFACE POINTS TO BREAK MULTIMATERIAL CONNECTIONS.
C
         nlstptl=0
         nintfail=0
         call mmgetblk('nconew',isubname,ipnconew,3*nlstcns,1,ierr)
         call mmgetblk('xi',isubname,ipxi,nlstcns*3,2,ierr)
         call mmgetblk('yi',isubname,ipyi,nlstcns*3,2,ierr)
         call mmgetblk('zi',isubname,ipzi,nlstcns*3,2,ierr)
         call mmgetblk('x1',isubname,ipx1,nlstcns,2,ierr)
         call mmgetblk('y1',isubname,ipy1,nlstcns,2,ierr)
         call mmgetblk('z1',isubname,ipz1,nlstcns,2,ierr)
         call mmgetblk('x2',isubname,ipx2,nlstcns,2,ierr)
         call mmgetblk('y2',isubname,ipy2,nlstcns,2,ierr)
         call mmgetblk('z2',isubname,ipz2,nlstcns,2,ierr)
         do iic=1,nlstcns
            x1(iic)=xic(lstcns1(iic))
            y1(iic)=yic(lstcns1(iic))
            z1(iic)=zic(lstcns1(iic))
            x2(iic)=xic(lstcns2(iic))
            y2(iic)=yic(lstcns2(iic))
            z2(iic)=zic(lstcns2(iic))
         enddo
         call ifaceptv(x1,y1,z1,x2,y2,z2,nlstcns,
     $                   smldistp,
     $                   ipxi,ipyi,ipzi,nptsnew,ipnconew,ierr)
         do j=1,nlstcns
            do k=1,nptsnew
               if(nconew(k).eq.j) go to 9176
            enddo
            if(ifdebug .gt. 0)then
            write(logmess,9190) lstcns1(j),lstcns2(j)
            call writloga('default',0,logmess,0,ierr)
            endif
 9176    continue
         enddo
         icount=0
         call mmrelblk('x1',isubname,ipx1,ierr)
         call mmrelblk('x2',isubname,ipx2,ierr)
         call mmrelblk('y1',isubname,ipy1,ierr)
         call mmrelblk('z1',isubname,ipz1,ierr)
         call mmrelblk('y2',isubname,ipy2,ierr)
         call mmrelblk('z2',isubname,ipz2,ierr)
         if(nptsnew.eq.0 ) then
             nnodes=npoints
             call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
            go to 9209
         endif
         call mmgetblk('weight',isubname,ipweight,nptsnew*2,2,ierr)
         call mmgetblk('ilist',isubname,ipilist,nptsnew*2,1,ierr)
         call mmgetblk('list',isubname,iplist,nptsnew,1,ierr)
C        INCREASE MEMORY, IF NECESSARY.
C
         if(npoints+nptsnew+incpts.gt.nnodes) then
            nnodes=nnodes+nptsnew+10+npoints
            call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
            call cmo_newlen(cmo,ierr)
            call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
            call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
            call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierr)
            call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
            call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
            call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
            call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
            call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
         endif
         do iic=1,nptsnew
C
C  set up arguments for cmo_interpolate - weight by distance
C  between connection being broken
C
            icount=icount+1
            list(icount)=npoints+icount
            ilist(1,icount)=lstcns1(nconew(iic))
            ilist(2,icount)=lstcns2(nconew(iic))
            d1=1./sqrt((xi(icount)-xic(lstcns1(nconew(iic))))**2+
     *                    (yi(icount)-yic(lstcns1(nconew(iic))))**2+
     *                    (zi(icount)-zic(lstcns1(nconew(iic))))**2)
            d2=1./sqrt((xi(icount)-xic(lstcns2(nconew(iic))))**2+
     *                    (yi(icount)-yic(lstcns2(nconew(iic))))**2+
     *                    (zi(icount)-zic(lstcns2(nconew(iic))))**2)
C
            weight(1,icount)=d1/(d1+d2)
            weight(2,icount)=d2/(d1+d2)
         enddo
         call cmo_interpolate(cmo,cmo,cnnodes,icount,2,list,ilist,
     *                               weight,ierr)
         do iic=1,icount
            xic(npoints+iic)=xi(iic)
            yic(npoints+iic)=yi(iic)
            zic(npoints+iic)=zi(iic)
            itp1(npoints+iic)=ifitpini
            imt1(npoints+iic)=imatint
         enddo
         call mmgetblk('surftst',isubname,ipisurftst,icount,1,icscode)
         call mmgetblk('icsurf',isubname,ipicsurf,icount*50,1,icscode)
C
C  check to see if point is on reflective or free boundary surfaces
C   or if on a constrained interface must set icr values
C
         do ns=1,nsurf
            if(ibtype(ns)(1:7).eq.'reflect') then
               call surftstv(xic(npoints+1),yic(npoints+1),
     *                    zic(npoints+1),icount,smalfrac,cmo,
     *                    istype(ns),surfparam(offsparam(ns)+1),
     *                    sheetnm(ns),ickin,isurftst)
               do iic=1,icount
                  if(isurftst(iic).eq.1) then
                     icsurf(1,iic)=icsurf(1,iic)+1
                     icsurf(icsurf(1,iic)+1,iic)=ns
                     if(itp1(npoints+iic).eq.ifitpirf) then
                     elseif(itp1(npoints+iic).eq.ifitpifb) then
                        itp1(npoints+iic)=ifitpirf
                     else
                        itp1(npoints+iic)=ifitpirb
                     endif
                  endif
               enddo
            elseif(ibtype(ns)(1:4).eq.'free') then
               call surftstv(xic(npoints+1),yic(npoints+1),
     *                    zic(npoints+1),icount,smalfrac,cmo,
     *                    istype(ns),surfparam(offsparam(ns)+1),
     *                    sheetnm(ns),ickin,isurftst)
               do iic=1,icount
                  if(isurftst(iic).eq.1) then
                     if(itp1(npoints+iic).eq.ifitpirf) then
                     elseif(itp1(npoints+iic).eq.ifitpirb) then
                        itp1(npoints+iic)=ifitpirf
                     else
                        itp1(npoints+iic)=ifitpifb
                     endif
                  endif
               enddo
            elseif(ibtype(ns)(1:8).eq.'intrcons') then
               call surftstv(xic(npoints+1),yic(npoints+1),
     *                    zic(npoints+1),icount,smalfrac,cmo,
     *                    istype(ns),surfparam(offsparam(ns)+1),
     *                    sheetnm(ns),ickin,isurftst)
               do iic=1,icount
                  if(isurftst(iic).eq.1) then
                     icsurf(1,iic)=icsurf(1,iic)+1
                     icsurf(icsurf(1,iic)+1,iic)=ns
                  endif
               enddo
            endif
         enddo
c
c  use accumulated icr value to set value for contrained surfaces
C
         do iic=1,icount
            n=icsurf(1,iic)
            icr1(npoints+iic)=0
            if (n.gt.0) then
               do j = 1,nconbnd
                  fail = .false.
                  if(icontab(1,j).eq.n) then
                     do nn = 1, n
                        if(icontab(2+nn,j).ne.icsurf(1+nn,iic)) then
                           fail=.true.
                        endif
                     enddo
                     if(.not.fail) then
                        icr1(npoints+iic)=j
                        go to 18
                     endif
                  else
                     fail=.true.
                  endif
               enddo
               nconbnd=nconbnd+1
               call cmo_set_info('nconbnd',cmo,nconbnd,1,1,ierr)
               call mmgetlen(ipicontab,leni,ierr)
               if(leni.lt.50*nconbnd)
     *           call mmincblk('icontab',cmo,ipicontab,500,ierr)
               icontab(1,nconbnd)=n
               icontab(2,nconbnd)=max(0,3-n)
               do nn = 1,n
                  icontab(2+nn,nconbnd)=icsurf(1+nn,iic)
               enddo
            call isort(icontab(3,nconbnd),idum1,n,1)
               icr1(npoints+iic)= nconbnd
            endif
 18         if(n.gt.1) then
C  if point type set to reflect or free then skip this
C  section - if not and point is on more than 2 constrained
C  interfaces it means that we have a closed surfaces condition
c  in this case all boundaries are constrained interfaces
C  check that the end nodes of connection being broken have
C  the same point types and icr values - use this info
C  to set the node itp value
               ip1=ilist(1,iic)
               ip2=ilist(2,iic)
               if(itsttp('reflect',itp1(ip1)).and.
     *            itsttp('reflect',itp1(ip2)).and.
     *            icrmatch(ip1,ip2,ipicr1,ipicontab)) then
                  itp1(npoints+iic)=ifitpirb
                  if(itsttp('free',itp1(ip1)).and.
     *               itsttp('free',itp1(ip2))) then
                     itp1(npoints+iic)=ifitprfb
                  elseif(itsttp('free',itp1(ip1)).and.
     *                   itsttp('free',itp1(ip2))) then
                     itp1(npoints+iic)=ifitpifb
                  endif
               else
                  itp1(npoints+iic)=ifitpini
               endif
            endif
         enddo
         call mmrelblk('surftst',isubname,ipisurftst,icscode)
         write(logmess,9180) icount
 9180    format(' Adding',i10,
     *          ' points to break multimaterial connections')
         call writloga('default',0,logmess,0,ierr)
         icount=0
         do iic=1,nptsnew
            write(logmess,9182) lstcns1(nconew(iic)),
     *                          lstcns2(nconew(iic)),
     *                          npoints+iic
            call writloga('bat',0,logmess,0,ierr)
 9182       format ('For connection ',2i10,' added point ',i10)
            if(nconew(iic).eq.0 ) then
               nintfail=nintfail+1
               if(ifdebug .gt. 0)then
               write(logmess,9190) lstcns1(nconew(iic)),
     *             lstcns2(nconew(iic))
 9190          format(' Cannot locate interface between points',i10,
     $                ' and',i10,'.')
               call writloga('bat',0,logmess,0,ierr)
               endif
            else
               icount=icount+1
            endif
         enddo
C
C  reorder connections to match points returned
C
         call mmgetlen(iplstcns1,leni,ierr)
         if(nptsnew.gt.leni) then
            call mmnewlen('lstcns1',nname,iplstcns1,nptsnew,ierr)
            call mmnewlen('lstcns2',nname,iplstcns2,nptsnew,ierr)
         endif
         do iic=1,nptsnew
            ilist(1,iic)=lstcns1(nconew(iic))
            ilist(2,iic)=lstcns2(nconew(iic))
         enddo
         do iic=1,nptsnew
            lstcns1(iic)=ilist(1,iic)
            lstcns2(iic)=ilist(2,iic)
         enddo
 
 9209    call mmrelblk('nconew',isubname,ipnconew,ierr)
         call mmrelblk('xi',isubname,ipxi,ierr)
         call mmrelblk('yi',isubname,ipyi,ierr)
         call mmrelblk('zi',isubname,ipzi,ierr)
         call mmrelblk('list',isubname,iplist,ierr)
         call mmrelblk('ilist',isubname,ipilist,ierr)
         call mmrelblk('weight',isubname,ipweight,ierr)
         call mmrelblk('icsurf',isubname,ipicsurf,ierr)
         nlstptl=icount
         if(nintfail.ne.0) then
            write(logmess,9210) nintfail
 9210       format(' Cannot locate interface between',i10,
     $             ' point pairs.')
            call writloga('default',0,logmess,0,ierr)
         endif
C
C        _______________________________________________________________
C        SKIP THE REST OF THE LOGIC IF NO POINTS ARE TO BE ADDED.
C
 9220    if(nlstptl.eq.0) goto 9490
C
C        _______________________________________________________________
C        UPDATE THE lstptl ARRAY.
C
         if(nlstptl.gt.lstptlen) then
            lstptlen=nlstptl
            call mmnewlen('lstptl',nname,iplstptl,lstptlen,
     $                    icscode)
         endif
         do i=1,nlstptl
            lstptl(i)=npoints+i
         enddo
         nlstptls=nlstptl
C           *** SAVE THE NUMBER OF POINTS IN THIS ORIGINAL LIST
C           *** FOR PRINTING A MESSAGE.
C
         call ifacerfl(npoints+1,nlstptl,smldistp)
C
C        _______________________________________________________________
C        WRITE A MESSAGE ABOUT THE ADDITION OF POINTS.
C
         write(logmess,9300) lstptl(1),lstptl(nlstptl)
 9300    format(' Points',i7,' -',i7,' are being added',
     $          ' to break multimaterial connections.')
         call writloga('default',1,logmess,0,ierr)
C
C        _______________________________________________________________
C        ACCUMULATE THE TOTAL NUMBER OF CONNECTIONS ENCOUNTERED
C        IN THE PROBLEM.
C
         intconn=intconn+nlstptl
C
C        _______________________________________________________________
C        MODIFY THE MATERIAL-LIST ARRAY.
C
         nmatfal1=0
         call mmfindbk('nimts',isubname,ipnimts,lng,ierr)
         if(ierr.eq.0) then
            if(lng.lt.nlstptl)
     *         call mmnewlen('nimts',isubname,ipnimts,nlstptl,ierr)
         else
            call mmgetblk('nimts',isubname,ipnimts,nlstptl,1,ierr)
         endif
         call mmfindbk('imts1',isubname,ipimts1,lng,ierr)
         len1=nlstptl*max(imtmax,nmregs)
         if(ierr.eq.0) then
            if(lng.lt.len1)
     *         call mmnewlen('imts1',isubname,ipimts1,len1,ierr)
         else
            call mmgetblk('imts1',isubname,ipimts1,len1,1,ierr)
         endif
         call ifaceregv(xic(npoints+1),yic(npoints+1),
     $                    zic(npoints+1),nlstptl,smldistp,
     *                    imts1,nimts,
     *                    ierr)
C
         if(npoints+incpts+nlstptl.gt.lenmatmx) then
            lenmatn=(6*(npoints+incpts+nlstptl))/5
            call mmnewlen('matlst',nname,ipmatlst,lenmatn,
     $                    icscode)
            do  n=matblks,2,-1
               do  ii=(n-1)*lenmatn+npoints,(n-1)*lenmatn+1,-1
                  matlst(ii)=matlst(ii-(n-1)*(lenmatn-lenmatmx))
               enddo
            enddo
            do  n=1,matblks
               do  ii=(n-1)*lenmatn+npoints+1,n*lenmatn
                  matlst(ii)=0
               enddo
            enddo
            lenmatmx=lenmatn
         else
            do  n=1,matblks
               matindex=(n-1)*lenmatmx
               if(incpts.gt.0) then
                  matlst(matindex+npoints+1)=0
                  matlst(matindex+npoints+2)=0
                  matlst(matindex+npoints+3)=0
                  matlst(matindex+npoints+4)=0
               endif
            enddo
         endif
         do  i=1,nlstptl
            if(nimts(i).ne.0) then
               do  iimt=1,nimts(i)
                  imtval=imts1(iimt+nmregs*(i-1))
                  matindex=((imtval-1)/32)*lenmatmx
                  n=shiftl(1,imtval-((imtval-1)/32)*32-1)
                  matlst(matindex+i+npoints)=
     $              ior(matlst(matindex+i+npoints),n)
c
c     $              shiftl(1,imtval-((imtval-1)/32)*32-1))
               enddo
            else
               nmatfal1=nmatfal1+1
            endif
         enddo
C
         if(nmatfal1.ne.0) then
            write(logmess,9340) nmatfal1
 9340       format(' Cannot find materials for',i10,
     $             ' new interface points.')
            call writloga('default',1,logmess,0,ierr)
         endif
C
C        _______________________________________________________________
C
C        CHANGE THE MASS-POINT NUMBERS OF POINTS ASSOCIATED WITH
C        THE BIG TETRAHEDRON.
C
         if (ibigtet.gt.0) then
            do iface=ittstrt1,4*ntets
               itetl1=itet1(iface)
               itet1(iface)=cvmgt(itet1(iface),itet1(iface)+nlstptl,
     $                         itet1(iface).lt.ibigtet)
            enddo
            ibigtet=npoints+nlstptl+1
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
            do  n=1,matblks
               matindex=(n-1)*lenmatmx
               matlst(matindex+ibigtet  )=-1
               matlst(matindex+ibigtet+1)=-1
               matlst(matindex+ibigtet+2)=-1
               matlst(matindex+ibigtet+3)=-1
            enddo
         endif
C           *** THE BIG-TETRAHEDRON POINT TYPES ARE SET TO
C           *** INTERFACE TYPES BECAUSE THIS ALLOWS US TO EXCLUDE
C           *** CONNECTIONS ORIGINATING FROM THESE POINTS DURING
C           *** THE FIRST INTERFACE PASS.  ALSO, ALL matlst BITS
C           *** FOR THE BIG-TETRAHEDRON POINTS ARE SET, SO THAT A
C           *** CONNECTION ORIGINATING FROM THESE POINTS WILL
C           *** NEVER BE CONSIDERED A MULTIMATERIAL CONNECTION
C           *** DURING THE SECOND INTERFACE PASS, AND ALL
C           *** SUBSEQUENT PASSES.
C
C        _______________________________________________________________
C
C        INCREMENT npoints.
C
         npoints=npoints+nlstptl
C
C        _______________________________________________________________
C
C        TRY TO CONNECT THESE POINTS.
C
         istep=1
         ntetmax=ntets
         call delaunay_connect(npoints,ntets,epsilon,
     *      ntetmax,nlsttts)
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierr)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierr)
         call cmo_newlen(cmo,ierr)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierr)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
         call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierr)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
         lencns=npoints
         call mmfindbk('lstcns1',nname,iplstcns1,leni,icscode)
         call mmfindbk('lstcns2',nname,iplstcns2,leni,icscode)
         if (leni.lt.lencns) then
           call mmnewlen('lstcns1',nname,iplstcns1,lencns,icscode)
           call mmnewlen('lstcns2',nname,iplstcns2,lencns,icscode)
         endif
         call mmfindbk('lsttts',nname,iplsttts,leni,icscode)
         if (leni.lt.ntets)
     *      call mmnewlen('lsttts',nname,iplsttts,ntets,icscode)
         if(nlstfail.ge.nlstptls.and.intpass.ne.1) then
C  if all points added failed then bail out
            write(logmess,'(a)') ' All added points failed'
            call writloga('default',0,logmess,0,ierr)
            go to 9999
         endif
         go to 8990
C
C        _______________________________________________________________
C
 9490    continue
C
C        _______________________________________________________________
C
C        IF WE GET TO THIS POINT, THEN NO INTERFACE POINTS ARE TO BE
C        ADDED DURING THIS PASS.  IF THIS IS THE FIRST PASS, GO TO THE
C        SECOND PASS.  IF THIS IS NOT THE FIRST PASS, THEN WE HAVE
C        SUCCESSFULLY ELIMINATED ALL MULTIMATERIAL CONNECTIONS WITHIN
C        THE MAXIMUM NUMBER OF PASSES ALLOWED; IN WHICH CASE, PRINT A
C        MESSAGE TO THAT EFFECT AND RESET THE DESIRED VARIABLES.
C
         if(intpass.eq.1) then
            goto 8990
         else
            write(logmess,9500)
 9500       format(' Successfully eliminated all multimaterial',
     $             ' connections.')
            call writloga('default',1,logmess,0,ierr)
            go to 9999
         endif
C
C        _______________________________________________________________
C
      endif
9999  call mmrelprt(isubname,ierr)
      return
      end
C
      logical function icrmatch (ip1,ip2,ipicr1,ipicontab)
      implicit none
      integer ip1,ip2,j,k,n1,n2,ic1,ic2
      pointer (ipicr1,icr1)
      integer icr1(*)
      pointer (ipicontab,icontab)
      integer icontab(50,*)
      icrmatch=.false.
      ic1=icr1(ip1)
      ic2=icr1(ip2)
      if (ic1.eq.0.or.ic2.eq.0) go to 9999
      if (ic1.eq.ic2) then
         icrmatch=.true.
         go to 9999
      endif
      n1=icontab(1,icr1(ip1))
      n2=icontab(1,icr1(ip2))
      if (n1.ge.n2) then
         do j=1,n2
            do k =1,n1
               if(icontab(2+k,ic1).eq.icontab(2+j,ic2)) then
                  icrmatch=.true.
                  go to 9999
               endif
            enddo
         enddo
      else
        do j=1,n1
            do k =1,n2
               if(icontab(2+k,ic1).eq.icontab(2+j,ic2)) then
                  icrmatch=.true.
                  go to 9999
               endif
            enddo
         enddo
      endif
 9999 continue
      return
      end
