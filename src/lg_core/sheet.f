      subroutine sheet(xmsgin,cmsgin,imsgin,msgtype,nwds,cmoout)
C
C########################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE DEFINES A BOUNDARY SURFACE OF A SHEET BY GENERATING
C     TRIANGULAR FACETS FROM GRIDDED INPUT.
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C        ipstb     - POINTER TO STORAGE BLOCK DATA ARRAY
C
C      OUTPUT ARGUMENTS
C
C        cmoout   -  name of output cmo
C
C     CHANGE HISTORY -
C
C        $Log: sheet.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.11   29 Aug 2007 16:29:22   gable
CPVCS    Modified so that if the input sheet MO is empty, code continues without
CPVCS    a mmgetblk crash.
CPVCS    
CPVCS       Rev 1.10   24 Jan 2007 16:38:02   tam
CPVCS    changed all dotaskx3d to dotask
CPVCS    
CPVCS       Rev 1.9   24 Jan 2007 16:23:46   tam
CPVCS    changed delatt with -def- to DELATT with cmoout
CPVCS    to make sure dotask operated on correct cmo
CPVCS    
CPVCS       Rev 1.8   Wed Apr 05 13:35:02 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.7   Thu Mar 30 08:08:42 2000   dcg
CPVCS    implicit none change
CPVCS
CPVCS       Rev 1.6   Wed Mar 29 17:24:56 2000   dcg
CPVCS    get rid of reference to ign1 - no longer a default attribute
CPVCS
CPVCS       Rev 1.5   Wed Mar 29 17:20:40 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.4   28 Mar 2000 14:09:26   dcg
CPVCS    remove include 'machine.h'
CPVCS
CPVCS       Rev 1.3   Thu Feb 03 08:57:30 2000   dcg
CPVCS
CPVCS       Rev 1.2   13 Jan 2000 14:48:28   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   04 Jan 2000 16:48:00   dcg
CPVCS
CPVCS
CPVCS       Rev 1.0   Thu Dec 23 14:22:56 1999   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.32   Thu Mar 04 11:21:20 1999   gable
CPVCS    Modified dotask commands because parser was not
CPVCS    treating  a / / string as default cmo input. Added
CPVCS    -def- to dotask commands.
CPVCS
CPVCS       Rev 1.31   Fri Apr 17 10:10:54 1998   gable
CPVCS    By default the surface cmo created will minimize memory.
CPVCS    A data statement variable if_delatt give control over
CPVCS    which cmo variables are kept or deleted.
CPVCS    if_delatt = 0 will delete isetwd, ialias, imt1, itp1, icr1, isn1,
CPVCS    itetclr
CPVCS    if_delatt ge 3 will delete nothing.
CPVCS
CPVCS       Rev 1.30   Fri Feb 27 12:04:22 1998   gable
CPVCS    Add set_info for edges_per_element in the output cmo.
CPVCS    Up to now it was using the default value of 6 because
CPVCS    it was defaulting to parameters for a tet cmo.
CPVCS
CPVCS       Rev 1.29   Mon Apr 14 17:01:30 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.28   Mon Nov 18 11:01:14 1996   dcg
CPVCS    remove character literals from argument lists
CPVCS
CPVCS       Rev 1.27   Wed Jun 19 10:27:34 1996   het
CPVCS    Restore the name of the original CMO after creating the sheet CMO.
CPVCS
CPVCS       Rev 1.26   Mon Feb 26 14:57:16 1996   dcg
CPVCS    add finish to dotasks
CPVCS
CPVCS       Rev 1.25   Wed Feb 14 17:30:24 1996   kuprat
CPVCS    Changed io attribute on LINKT and SBOX to 'x'.
CPVCS
CPVCS       Rev 1.24   Tue Feb 13 14:39:58 1996   ahmed
CPVCS    add the k-D tree to the sheet cmo
CPVCS
CPVCS       Rev 1.23   Fri Dec 22 14:21:40 1995   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.22   12/05/95 08:23:18   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.20   11/07/95 17:26:20   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.19   11/07/95 11:29:00   het
CPVCS    Modify the 2D triangle refinement algorithms.
CPVCS
CPVCS       Rev 1.18   10/19/95 06:20:20   het
CPVCS    Use the call to hsbgetd to stuff the CMO name into storage block form
CPVCS
CPVCS       Rev 1.17   10/18/95 17:10:38   het
CPVCS    Fix an error with nstbout
CPVCS
CPVCS       Rev 1.16   10/18/95 12:20:02   het
CPVCS    Allow greater that 8 character names for sheets in the surface command.
CPVCS
CPVCS       Rev 1.15   08/28/95 11:22:20   ahmed
CPVCS    sheet surfaces input as mesh objects
CPVCS
C
C
C########################################################################
C
      implicit none
C
      include "chydro.h"
      include "local_element.h"
      integer lenptr
C
      parameter (lenptr=1000000)
C
C########################################################################
 
      pointer (ipxin, xin)
      pointer (ipyin, yin)
      pointer (ipzin, zin)
      pointer (ipitmpnd, itmpnd)
C
      real*8 xin(lenptr),yin(lenptr),zin(lenptr)
      integer itmpnd(lenptr)
C
      integer idatin(128),idattyp(128), nwds
      real*8 xmsgin(nwds),xdatin(128)
      integer imsgin(nwds), msgtype(nwds)
      character*132 logmess
      character*132 cbuff
      character*(*) cmsgin(nwds)
      character*80 imessage
      character*32 isubname,input
      character*32 cmoin,cmoout,cmocurrent
      character*32 mdatin(128)
C
C     SET POINTERS FOR INCOMING cmo
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisetwd, isetwd)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet1)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
C
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
      integer imt1(lenptr),itp1(lenptr),itetclr(lenptr),
     &        itettyp(lenptr),itet1(lenptr),
     &        itetoff(lenptr),jtetoff(lenptr),isetwd(lenptr)
 
C
C     SET POINTERS FOR OUTGOING cmo
C
      pointer (ipimt1o, imt1o)
      pointer (ipitp1o, itp1o)
      pointer (ipitetclro, itetclro)
      pointer (ipitettypo, itettypo)
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
      pointer (ipiteto, itet1o)
      pointer (ipjteto, jtet1o)
      pointer (ipitetoffo, itetoffo)
      pointer (ipjtetoffo, jtetoffo)
C
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox)
C
      real*8 xico(lenptr),yico(lenptr),zico(lenptr),sbox(lenptr)
      integer imt1o(lenptr),itp1o(lenptr),itetclro(lenptr),
     &        itettypo(lenptr),itet1o(lenptr),jtet1o(lenptr),
     &        itetoffo(lenptr),jtetoffo(lenptr),linkt(lenptr)
C
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      integer nedge_bin(lenptr), nedge_off(lenptr)
      pointer (ipitface, itface)
      integer itface(lenptr)
C
      pointer (ipitflag, itflag)
      integer itflag(lenptr)
c
      integer isum,j1,ifsum,length,nsd,ipointi,ipointj,iq,i5,iquad,
     *   i1,i2,i3,jtoff,itoff,itri,npoints,mbndry,i,icnt,
     *   iunit,j,nx,ny,lenmm1,len,icharlnf,j2,j3,modesum2,
     *   k,modesum4,modesum1,modemin,modemax,jt,nodesum3,leninput,
     *   nodesum1,nodesum2,nodesum4,ierr,modesum3,nodemin,idelete,
     *   nodemax,i4,index,it,npts,iflag,ntets,
     *   nsdtopo,nen,nef,nsdgeom,nnodes,icscode,ndata,
     *   nelements,ilen,icmotype,ierror,ics,if_delatt,itcount
      real*8 z5,y5,x5,x4,y4,z4,area,c,b,a,x2,y2,z2,areatol,areamax,
     *  x1,y1,z1,degtorad,xmag2,xdot,c2,b2,a2,a1,b1,c1,xmag1,x3,y3,z3
 
C
C     This data value will create a minimal cmo by default.
C     The larger the value of if_delatt, the larger your cmo memory.
C     if_delatt = 0 will delete
C     isetwd, ialias, imt1, itp1, icr1, isn1, ign1, xtetwd, itetclr
C     Some parts of the code may need some attributes.
C     if_delatt = 1 will delete
C     isetwd, ialias, icr1, isn1, ign1, xtetwd
C     if_delatt = 2 will delete
C     isetwd, ialias, ign1, xtetwd
C     if_delatt ge 3 will delete nothing
      data if_delatt / 0 /
C
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ierror = 0
      cmoin = '-cmo-'
      cmoout = '-none-'
C
C     *******************************************************************
C     SET THE MEMORY MANAGED PARTITION NAME.
C
      isubname='sheet'
C
C     *******************************************************************
C     SAVE THE NAME OF THE CURRENT MESH OBJECT
C
      call cmo_get_name(cmocurrent,ierror)
C
C     *******************************************************************
C     GET INPUT TYPE AND INCOMING cmo INFO.
C
      iflag=0
      if (msgtype(5).eq.1) then
         iflag=1
         input='quad_points'
         cmoout = cmsgin(2)
      elseif (msgtype(5).eq.3) then
         cmoin=cmsgin(5)
         call cmo_exist(cmoin,ierror)
         if(ierror.eq.0) then
            iflag=2
            cmoout = cmsgin(2)
**------------------------------------------------------------
**            if (cmoin(1:icharlnf(cmoin)).eq.'-cmo-') then
**               call cmo_get_name(cmoin,ierror)
**            endif
**------------------------------------------------------------
            call cmo_get_info('nelements',cmoin,
     &                         nelements,ilen,icmotype,ierror)
            call cmo_get_info('ndimensions_geom',cmoin,
     &                         nsdgeom,ilen,icmotype,ierror)
            call cmo_get_info('ndimensions_topo',cmoin,
     &                         nsdtopo,ilen,icmotype,ierror)
            call cmo_get_info('nodes_per_element',cmoin,
     &                         nen,ilen,icmotype,ierror)
            call cmo_get_info('faces_per_element',cmoin,
     &                         nef,ilen,icmotype,ierror)
            if (nsdtopo.eq.2 .and.
     &          nen.eq.nelmnen(ifelmqud) .and.
     &          nef.eq.nelmnef(ifelmqud)) then
                input='quad_cmo'
            elseif (nen.eq.nelmnen(ifelmtri) .and.
     &          nef.eq.nelmnef(ifelmtri)) then
                input='tri_cmo'
            elseif (nen.eq.nelmnen(ifelmhyb) .and.
     &          nef.eq.nelmnef(ifelmhyb)) then
                input='hyb_cmo'
            else
               write(logmess,9000) cmoin(1:icharlnf(cmoin))
 9000          format("Sheet CMO has the wrong dimensionality: ",a)
               call writloga('default',1,logmess,0,ierror)
               goto 9999
            endif
         else
            call fexist(cmsgin(5),ierror)
            if (ierror.eq.0) then
               write(logmess,9010) cmsgin(5)(1:icharlnf(cmsgin(5)))
 9010          format(' The QUAD-POINT file, ',a,', does not exist.')
               call writloga('default',1,logmess,0,ierror)
               ierror = 1
               goto 9999
            else
               iflag=3
               input='quad_file'
               cmoout = cmsgin(2)
            endif
         endif
      endif
      if (iflag.eq.0) then
         write(logmess,9020) cmsgin(5)(1:icharlnf(cmsgin(5)))
 9020    format('An unknown sheet type : ',a)
         call writloga('default',1,logmess,0,ierror)
         ierror = 1
         goto 9999
      endif
C
      if (cmoout .eq. '-none-') then
         ierror=1
         write(logmess,'(a)') 'Need a name for the output cmo SHEET.'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     *******************************************************************
C
      pie=acos(-1.0d+00)
      degtorad=pie/180.
      len=icharlnf(input)
C
      if (input(1:len).eq.'tri_cmo' .or.
     &    input(1:len).eq.'quad_cmo' .or.
     &    input(1:len).eq.'hyb_cmo') then
C
C        ****************************************************************
C        GET THE SHEET DATA FROM THE cmo.
C
         call cmo_get_info('nnodes',cmoin,nnodes,ilen,icmotype,ierror)
         call cmo_get_info('nelements',cmoin,
     &                      nelements,ilen,icmotype,ierror)
         call cmo_get_info('ndimensions_geom',cmoin,
     &                      nsdgeom,ilen,icmotype,ierror)
         call cmo_get_info('ndimensions_topo',cmoin,
     &                      nsdtopo,ilen,icmotype,ierror)
         call cmo_get_info('nodes_per_element',cmoin,
     &                      nen,ilen,icmotype,ierror)
         call cmo_get_info('faces_per_element',cmoin,
     &                      nef,ilen,icmotype,ierror)
         call cmo_get_info('itp1',cmoin,ipitp1,ilen,icmotype,ierror)
         call cmo_get_info('imt1',cmoin,ipimt1,ilen,icmotype,ierror)
         call cmo_get_info('isetwd',cmoin,ipisetwd,ilen,icmotype,ierror)
         call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
         call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
         call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
         call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,icmotype
     &                     ,ierror)
         call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,icmotype
     &                      ,ierror)
         call cmo_get_info('itetoff',cmoin,ipitetoff,ilen,icmotype
     &                      ,ierror)
         call cmo_get_info('jtetoff',cmoin,ipjtetoff,ilen,icmotype
     &                      ,ierror)
         call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
         if (nsdtopo.eq.2 .and.
     &       nen.eq.nelmnen(ifelmqud) .and.
     &       nef.eq.nelmnef(ifelmqud)) then
            input='quad_cmo'
         elseif (nen.eq.nelmnen(ifelmtri) .and.
     &           nef.eq.nelmnef(ifelmtri)) then
            input='tri_cmo'
         elseif (nen.eq.nelmnen(ifelmhyb) .and.
     &           nef.eq.nelmnef(ifelmhyb)) then
            input='hyb_cmo'
         endif
         nx=nnodes
         ny=1
         npts=nnodes
C        ****************************************************************
C        Create an empty mesh object and exit
C        if mesh object has 0 nodes or elements.
C
         if((nnodes .eq. 0).or.(nelements .eq. 0))then
         if(nnodes .eq. 0)then
            write(logmess,'(a)')
     1       'SURFACE:WARNING Sheet mesh object has 0 nodes.'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         endif
         if(nelements .eq. 0)then
            write(logmess,'(a)')
     1       'SURFACE:WARNING Sheet mesh object has 0 elements.'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         endif
         call cmo_exist(cmoout,ierror)
         if(ierror.eq.0) call cmo_release(cmoout,idelete)
         call cmo_create(cmoout,ierror)
            write(logmess,'(a)')
     1       'SURFACE:WARNING Empty surface mesh object created.'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
         go to 9999
         endif
             
C
C        ****************************************************************
C        GET MEMORY FOR INPUT DATA GRID.
C
         lenmm1=nnodes
         call mmgetblk('xin',isubname,ipxin,lenmm1,2,icscode)
         call mmgetblk('yin',isubname,ipyin,lenmm1,2,icscode)
         call mmgetblk('zin',isubname,ipzin,lenmm1,2,icscode)
C
C        ****************************************************************
C        GET POINT DATA FOR cmo.
C
         do i=1,nnodes
            xin(i)=xic(i)
            yin(i)=yic(i)
            zin(i)=zic(i)
         enddo
C
      elseif (input.eq.'quad_points') then
C
C           *************************************************************
C           READ IN DATA GRID POINTS.
C
            nx=imsgin(5)
            ny=imsgin(6)
C
C           *************************************************************
C           GET MEMORY FOR INPUT DATA GRID.
C
            lenmm1=nx*ny
            call mmgetblk('xin',isubname,ipxin,lenmm1,2,icscode)
            call mmgetblk('yin',isubname,ipyin,lenmm1,2,icscode)
            call mmgetblk('zin',isubname,ipzin,lenmm1,2,icscode)
C
            icnt=0
            do i=1,nx*ny
               icnt=icnt+1
               imessage='Reading triplet data coordinates.'
               call getcmds(128,idatin,xdatin,mdatin,idattyp,ndata,
     &                      imessage)
 
               xin(icnt)=xdatin(1)
               yin(icnt)=xdatin(2)
               zin(icnt)=xdatin(3)
            enddo
            npts=icnt
C
      elseif (input.eq.'quad_file') then
C
C           *************************************************************
C           READ IN DATA GRID POINTS FROM A FILE.
C
            write(logmess,'(a,a)') 'Read sheet data from a file: ',
     &                              cmsgin(5)
            call writloga('default',0,logmess,0,ierror)
C
            iunit=-1
            call hassign(iunit,cmsgin(5),ierror)
            if (iunit.lt.0 .or. ierror.lt.0) then
              call x3d_error(isubname,'hassign bad file unit')
              goto 9999
            endif

            read(iunit,*) nx,ny
C
C           *************************************************************
C           GET MEMORY FOR INPUT DATA GRID.
C
            lenmm1=nx*ny
            call mmgetblk('xin',isubname,ipxin,lenmm1,2,icscode)
            call mmgetblk('yin',isubname,ipyin,lenmm1,2,icscode)
            call mmgetblk('zin',isubname,ipzin,lenmm1,2,icscode)
C
            icnt=0
            do j=1,ny
               do i=1,nx
                  icnt=icnt+1
                  read(iunit,*) x1,y1,z1
                  xin(icnt)=x1
                  yin(icnt)=y1
                  zin(icnt)=z1
               enddo
            enddo
            close(iunit)
            npts=icnt
      endif
C
C     *******************************************************************
C     CHECK THAT THE NO. OF POINTS ENTERED IS NX*NY.
C
      if (npts .ne. nx*ny) then
         write(logmess,9030)
 9030    format(' Warning, the no. of points entered does not ',
     &          'equal nx*ny.')
         call writloga('default',0,logmess,0,ierror)
         write(logmess,9004) nx,ny,npts
 9004    format('    No. points=',i8,3x,'nx=',i5,3x,'ny=',i5)
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         goto 9999
      endif
C
C     *******************************************************************
C     SET LENGTHS OF THE NEW cmo.
C
      leninput=icharlnf(input)
      if(input(1:leninput).eq.'hyb_cmo') then
         npoints=nnodes
         ntets=nelements
      elseif(input(1:leninput).eq.'tri_cmo') then
         npoints=nnodes
         ntets=nelements
      elseif(input(1:leninput).eq.'quad_cmo') then
         npoints=nnodes+nelements
         ntets=4*nelements
      elseif(input(1:leninput).eq.'quad_points' .or.
     &   input(1:leninput).eq.'quad_file')  then
         npoints=nx*ny+(nx-1)*(ny-1)
         ntets=4*(nx-1)*(ny-1)
      endif
C
C     *******************************************************************
C     GET MEMORY FOR itmpnd AND INITIALIZE  IT.
C
      lenmm1=npoints
      call mmgetblk('itmpnd',isubname,ipitmpnd,lenmm1,2,icscode)
      do i=1,npoints
         itmpnd(i)=21
      enddo
C
C     *******************************************************************
C     CREATE THE NEW cmo.
C
      call cmo_exist(cmoout,ierror)
C
C     ierror.eq.0 MEANS THAT THE cmo ALREADY EXISTS.
C
      if(ierror.eq.0) call cmo_release(cmoout,idelete)
C
      call cmo_create(cmoout,ierror)
C
C     *******************************************************************
C     ADD ATTRIBUTES FOR k-D TREE OF THE OUTGOING cmo.
C
      cbuff='cmo/addatt/' //
     &      '/' //
     &      'v2' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//2.0' //
     &      ' ; finish'
      call dotask(cbuff,ierror)
      cbuff='cmo/addatt/' //
     &      '/' //
     &      'linkt' //
     &      '/VINT' //
     &      '/v2/nelements//permanent/x/0.0' //
     &      ' ; finish'
      call dotask(cbuff,ierror)
C
      cbuff='cmo/addatt/' //
     &      '/' //
     &      'v12' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//12.0' //
     &      ' ; finish'
      call dotask(cbuff,ierror)
      cbuff='cmo/addatt/' //
     &      '/' //
     &      'sbox' //
     &      '/VDOUBLE' //
     &      '/v12/nelements/linear/permanent/x/0.0' //
     &      ' ; finish'
      call dotask(cbuff,ierror)
C
      call cmo_set_info('nnodes',cmoout,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntets,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,2,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,3,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,3,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,3,1,1,ierror)
      call cmo_set_info('edges_per_element',cmoout,3,1,1,ierror)
C
      call cmo_newlen(cmoout,ierror)
C
      call cmo_get_info('mbndry',cmoout,mbndry,ilen,icmotype,ierror)
C
      call cmo_get_info('imt1',cmoout,ipimt1o,ilen,icmotype,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1o,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoout,ipxico,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoout,ipyico,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoout,ipzico,ilen,icmotype,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclro,ilen,
     &                   icmotype,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypo,ilen,
     &                   icmotype,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffo,ilen,
     &                   icmotype,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffo,ilen,
     &                   icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteto,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmoout,ipjteto,ilen,icmotype,ierror)
      call cmo_get_info('linkt',cmoout,iplinkt,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoout,ipsbox,ilen,icmotype,ierror)
C
C     *******************************************************************
C     BUILD TRIANGLES FROM EACH TYPE OF INPUT.
C
      areamax=0.0d+00
      areatol=1.0e-12
      if (input(1:leninput).eq.'hyb_cmo') then
C
         itri=0
         itoff=0
         jtoff=0
         do it=1,nelements
            index=itetoff(it)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            x1=xin(i1)
            y1=yin(i1)
            z1=zin(i1)
            x2=xin(i2)
            y2=yin(i2)
            z2=zin(i2)
            x3=xin(i3)
            y3=yin(i3)
            z3=zin(i3)
C
            xico(i1)=x1
            yico(i1)=y1
            zico(i1)=z1
            xico(i2)=x2
            yico(i2)=y2
            zico(i2)=z2
            xico(i3)=x3
            yico(i3)=y3
            zico(i3)=z3
C
            imt1o(i1)=imt1(i1)
            imt1o(i2)=imt1(i2)
            imt1o(i3)=imt1(i3)
C
            a=(y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
            b=(x3-x1)*(z2-z1) - (x2-x1)*(z3-z1)
            c=(x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
            area=0.5d+00 * sqrt(a**2+b**2+c**2)
            areamax=max(areamax,area)
C
            if (area.gt.areatol) then
               itri=itri+1
               itetclro(itri)=itetclr(it)
               itettypo(itri)=ifelmtri
               itetoffo(itri)=itoff
               jtetoffo(itri)=jtoff
C
               itet1o(itoff+1)=i1
               itet1o(itoff+2)=i2
               itet1o(itoff+3)=i3
               itoff=itoff+nelmnen(itettypo(itri))
               jtoff=jtoff+nelmnef(itettypo(itri))
C
               itmpnd(i1)=itp1(i1)
               itmpnd(i2)=itp1(i2)
               itmpnd(i3)=itp1(i3)
            endif
         enddo
         nnodes=npoints
         nelements=itri
C
      elseif (input(1:leninput).eq.'tri_cmo') then
C
C     *******************************************************************
C     BUILD TRIANGLES FROM EACH TYPE OF INPUT.
C
         itri=0
         itoff=0
         jtoff=0
         do it=1,nelements
            index=itetoff(it)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            x1=xin(i1)
            y1=yin(i1)
            z1=zin(i1)
            x2=xin(i2)
            y2=yin(i2)
            z2=zin(i2)
            x3=xin(i3)
            y3=yin(i3)
            z3=zin(i3)
C
            xico(i1)=x1
            yico(i1)=y1
            zico(i1)=z1
            xico(i2)=x2
            yico(i2)=y2
            zico(i2)=z2
            xico(i3)=x3
            yico(i3)=y3
            zico(i3)=z3
C
            imt1o(i1)=imt1(i1)
            imt1o(i2)=imt1(i2)
            imt1o(i3)=imt1(i3)
C
            a=(y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
            b=(x3-x1)*(z2-z1) - (x2-x1)*(z3-z1)
            c=(x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
            area=0.5d+00 * sqrt(a**2+b**2+c**2)
            areamax=max(areamax,area)
C
            if (area.gt.areatol) then
               itri=itri+1
               itetclro(itri)=itetclr(it)
               itettypo(itri)=ifelmtri
               itetoffo(itri)=itoff
               jtetoffo(itri)=jtoff
C
               itet1o(itoff+1)=i1
               itet1o(itoff+2)=i2
               itet1o(itoff+3)=i3
               itoff=itoff+nelmnen(itettypo(itri))
               jtoff=jtoff+nelmnef(itettypo(itri))
C
               itmpnd(i1)=itp1(i1)
               itmpnd(i2)=itp1(i2)
               itmpnd(i3)=itp1(i3)
            endif
         enddo
         nnodes=npoints
         nelements=itri
C
      elseif (input(1:leninput).eq.'quad_cmo') then
C
C            ************************************************************
C            LOOP THROUGH THE GRID TO CREATE TRIANGULAR FACETS.
C            FOUR FACETS ARE GENERATED PER CELL.
C
             itri=0
             itoff=0
             jtoff=0
             i5=nnodes
             do iquad=1,nelements
                index=itetoff(iquad)
                i1=itet1(index+1)
                i2=itet1(index+2)
                i3=itet1(index+3)
                i4=itet1(index+4)
                i5=i5+1
C
                x1=xin(i1)
                y1=yin(i1)
                z1=zin(i1)
                x2=xin(i2)
                y2=yin(i2)
                z2=zin(i2)
                x3=xin(i3)
                y3=yin(i3)
                z3=zin(i3)
                x4=xin(i4)
                y4=yin(i4)
                z4=zin(i4)
                x5=0.25d+00*(x1+x2+x3+x4)
                y5=0.25d+00*(y1+y2+y3+y4)
                z5=0.25d+00*(z1+z2+z3+z4)
C
                xico(i1)=x1
                yico(i1)=y1
                zico(i1)=z1
                xico(i2)=x2
                yico(i2)=y2
                zico(i2)=z2
                xico(i3)=x3
                yico(i3)=y3
                zico(i3)=z3
                xico(i4)=x4
                yico(i4)=y4
                zico(i4)=z4
                xico(i5)=x5
                yico(i5)=y5
                zico(i5)=z5
C
                imt1o(i1)=imt1(i1)
                imt1o(i2)=imt1(i2)
                imt1o(i3)=imt1(i3)
                imt1o(i4)=imt1(i4)
                imt1o(i5)=itetclr(iquad)
C
C               ---------------------------------------------------------
C               TRIANGULATE EACH QUAD TO FOUR TRIANGLES.
C
                a=(y2-y1)*(z5-z1) - (y5-y1)*(z2-z1)
                b=(x5-x1)*(z2-z1) - (x2-x1)*(z5-z1)
                c=(x2-x1)*(y5-y1) - (x5-x1)*(y2-y1)
                area=0.5d+00 * sqrt(a**2+b**2+c**2)
                areamax=max(areamax,area)
C
                if (area.gt.areatol) then
                   itri=itri+1
                   itetclro(itri)=itetclr(iquad)
                   itettypo(itri)=ifelmtri
                   itetoffo(itri)=itoff
                   jtetoffo(itri)=jtoff
C
                   itet1o(itoff+1)=i1
                   itet1o(itoff+2)=i2
                   itet1o(itoff+3)=i5
                   itoff=itoff+nelmnen(itettypo(itri))
                   jtoff=jtoff+nelmnef(itettypo(itri))
C
                   itmpnd(i1)=itp1(i1)
                   itmpnd(i2)=itp1(i2)
                   itmpnd(i5)=min(itp1(i1),itp1(i2),itp1(i3),itp1(i4))
                endif
C
                a=(y3-y2)*(z5-z2) - (y5-y2)*(z3-z2)
                b=(x5-x2)*(z3-z2) - (x3-x2)*(z5-z2)
                c=(x3-x2)*(y5-y2) - (x5-x2)*(y3-y2)
                area=0.5d+00 * sqrt(a**2+b**2+c**2)
                areamax=max(areamax,area)
C
                if (area.gt.areatol) then
                   itri=itri+1
                   itetclro(itri)=itetclr(iquad)
                   itettypo(itri)=ifelmtri
                   itetoffo(itri)=itoff
                   jtetoffo(itri)=jtoff
C
                   itet1o(itoff+1)=i2
                   itet1o(itoff+2)=i3
                   itet1o(itoff+3)=i5
                   itoff=itoff+nelmnen(itettypo(itri))
                   jtoff=jtoff+nelmnef(itettypo(itri))
C
                   itmpnd(i2)=itp1(i2)
                   itmpnd(i3)=itp1(i3)
                   itmpnd(i5)=min(itp1(i1),itp1(i2),itp1(i3),itp1(i4))
                endif
C
                a=(y4-y3)*(z5-z3) - (y5-y3)*(z4-z3)
                b=(x5-x3)*(z4-z3) - (x4-x3)*(z5-z3)
                c=(x4-x3)*(y5-y3) - (x5-x3)*(y4-y3)
                area=0.5d+00 * sqrt(a**2+b**2+c**2)
                areamax=max(areamax,area)
C
                if (area.gt.areatol) then
                   itri=itri+1
                   itetclro(itri)=itetclr(iquad)
                   itettypo(itri)=ifelmtri
                   itetoffo(itri)=itoff
                   jtetoffo(itri)=jtoff
C
                   itet1o(itoff+1)=i3
                   itet1o(itoff+2)=i4
                   itet1o(itoff+3)=i5
                   itoff=itoff+nelmnen(itettypo(itri))
                   jtoff=jtoff+nelmnef(itettypo(itri))
C
                   itmpnd(i3)=itp1(i3)
                   itmpnd(i4)=itp1(i4)
                   itmpnd(i5)=min(itp1(i1),itp1(i2),itp1(i3),itp1(i4))
                endif
C
                a=(y4-y1)*(z5-z1) - (y5-y1)*(z4-z1)
                b=(x5-x1)*(z4-z1) - (x4-x1)*(z5-z1)
                c=(x4-x1)*(y5-y1) - (x5-x1)*(y4-y1)
                area=0.5d+00 * sqrt(a**2+b**2+c**2)
                areamax=max(areamax,area)
C
                if (area.gt.areatol) then
                   itri=itri+1
                   itetclro(itri)=itetclr(iquad)
                   itettypo(itri)=ifelmtri
                   itetoffo(itri)=itoff
                   jtetoffo(itri)=jtoff
C
                   itet1o(itoff+1)=i4
                   itet1o(itoff+2)=i1
                   itet1o(itoff+3)=i5
                   itoff=itoff+nelmnen(itettypo(itri))
                   jtoff=jtoff+nelmnef(itettypo(itri))
C
                   itmpnd(i1)=itp1(i1)
                   itmpnd(i4)=itp1(i4)
                   itmpnd(i5)=min(itp1(i1),itp1(i2),itp1(i3),itp1(i4))
                endif
C
              enddo
              nnodes=npoints
              nelements=itri
C
      elseif (input(1:leninput).eq.'quad_points' .or.
     &        input(1:leninput).eq.'quad_file') then
C
C            ************************************************************
C            LOOP THROUGH THE GRID TO CREATE TRIANGULAR FACETS.  FOUR
C            FACETS ARE GENERATED PER CELL BY ADDING A NEW AVERAGED
C            VERTEX AT THE CENTER OF EACH QUAD.
C
             itri=0
             itoff=0
             jtoff=0
             iq=0
             i5=nx*ny
             do j=1,ny-1
                do i=1,nx-1
                   iq=iq+1
C
C                  ------------------------------------------------------
C                  GET THE GRID POINTS FOR EACH CELL.
C
                   i1=iq
                   i2=iq+1
                   i3=iq+1+nx
                   i4=iq+nx
                   i5=i5+1
C
                   x1=xin(i1)
                   y1=yin(i1)
                   z1=zin(i1)
                   x2=xin(i2)
                   y2=yin(i2)
                   z2=zin(i2)
                   x3=xin(i3)
                   y3=yin(i3)
                   z3=zin(i3)
                   x4=xin(i4)
                   y4=yin(i4)
                   z4=zin(i4)
                   x5=0.25d+00*(x1+x2+x3+x4)
                   y5=0.25d+00*(y1+y2+y3+y4)
                   z5=0.25d+00*(z1+z2+z3+z4)
C
                   xico(i1)=x1
                   yico(i1)=y1
                   zico(i1)=z1
                   xico(i2)=x2
                   yico(i2)=y2
                   zico(i2)=z2
                   xico(i3)=x3
                   yico(i3)=y3
                   zico(i3)=z3
                   xico(i4)=x4
                   yico(i4)=y4
                   zico(i4)=z4
                   xico(i5)=x5
                   yico(i5)=y5
                   zico(i5)=z5
C
                   imt1o(i1)=1
                   imt1o(i2)=1
                   imt1o(i3)=1
                   imt1o(i4)=1
                   imt1o(i5)=1
C
C                  ------------------------------------------------------
C                  TRIANGULATE EACH QUAD TO FOUR TRIANGLES.
C
                   a=(y2-y1)*(z5-z1) - (y5-y1)*(z2-z1)
                   b=(x5-x1)*(z2-z1) - (x2-x1)*(z5-z1)
                   c=(x2-x1)*(y5-y1) - (x5-x1)*(y2-y1)
                   area=0.5d+00 * sqrt(a**2+b**2+c**2)
                   areamax=max(areamax,area)
C
                   if (area.gt.areatol) then
                      itri=itri+1
                      itetclro(itri)=1
                      itettypo(itri)=ifelmtri
                      itetoffo(itri)=itoff
                      jtetoffo(itri)=jtoff
C
                      itet1o(itoff+1)=i1
                      itet1o(itoff+2)=i2
                      itet1o(itoff+3)=i5
                      itoff=itoff+nelmnen(itettypo(itri))
                      jtoff=jtoff+nelmnef(itettypo(itri))
C
                      itmpnd(i1)=0
                      itmpnd(i2)=0
                      itmpnd(i5)=0
                   endif
C
                   a=(y3-y2)*(z5-z2) - (y5-y2)*(z3-z2)
                   b=(x5-x2)*(z3-z2) - (x3-x2)*(z5-z2)
                   c=(x3-x2)*(y5-y2) - (x5-x2)*(y3-y2)
                   area=0.5d+00 * sqrt(a**2+b**2+c**2)
                   areamax=max(areamax,area)
C
                   if (area.gt.areatol) then
                      itri=itri+1
                      itetclro(itri)=1
                      itettypo(itri)=ifelmtri
                      itetoffo(itri)=itoff
                      jtetoffo(itri)=jtoff
C
                      itet1o(itoff+1)=i2
                      itet1o(itoff+2)=i3
                      itet1o(itoff+3)=i5
                      itoff=itoff+nelmnen(itettypo(itri))
                      jtoff=jtoff+nelmnef(itettypo(itri))
C
                      itmpnd(i2)=0
                      itmpnd(i3)=0
                      itmpnd(i5)=0
                   endif
C
                   a=(y4-y3)*(z5-z3) - (y5-y3)*(z4-z3)
                   b=(x5-x3)*(z4-z3) - (x4-x3)*(z5-z3)
                   c=(x4-x3)*(y5-y3) - (x5-x3)*(y4-y3)
                   area=0.5d+00 * sqrt(a**2+b**2+c**2)
                   areamax=max(areamax,area)
C
                   if (area.gt.areatol) then
                      itri=itri+1
                      itetclro(itri)=1
                      itettypo(itri)=ifelmtri
                      itetoffo(itri)=itoff
                      jtetoffo(itri)=jtoff
C
                      itet1o(itoff+1)=i3
                      itet1o(itoff+2)=i4
                      itet1o(itoff+3)=i5
                      itoff=itoff+nelmnen(itettypo(itri))
                      jtoff=jtoff+nelmnef(itettypo(itri))
C
                      itmpnd(i3)=0
                      itmpnd(i4)=0
                      itmpnd(i5)=0
                   endif
C
                   a=(y4-y1)*(z5-z1) - (y5-y1)*(z4-z1)
                   b=(x5-x1)*(z4-z1) - (x4-x1)*(z5-z1)
                   c=(x4-x1)*(y5-y1) - (x5-x1)*(y4-y1)
                   area=0.5d+00 * sqrt(a**2+b**2+c**2)
                   areamax=max(areamax,area)
C
                   if (area.gt.areatol) then
                      itri=itri+1
                      itetclro(itri)=1
                      itettypo(itri)=ifelmtri
                      itetoffo(itri)=itoff
                      jtetoffo(itri)=jtoff
C
                      itet1o(itoff+1)=i4
                      itet1o(itoff+2)=i1
                      itet1o(itoff+3)=i5
                      itoff=itoff+nelmnen(itettypo(itri))
                      jtoff=jtoff+nelmnef(itettypo(itri))
C
                      itmpnd(i4)=0
                      itmpnd(i1)=0
                      itmpnd(i5)=0
                   endif
                enddo
              iq=iq+1
             enddo
             nnodes=npoints
             nelements=itri
C
      endif
C
C     *******************************************************************
C     IF THE MAXIMUM ELEMENT AREA IS "ZER0" THEN THERE MUST BE NO
C     TRIANGLES FOR THIS SURFACE. BAIL OUT OF THIS ROUTINE.
C
      if (areamax.lt.areatol) then
         ierror=1
         write(logmess,'(a)') 'The surface is degenerate.'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     *******************************************************************
C     SET THE CORRECT POINT TYPE FOR EACH NODE.
C
       do i=1,nnodes
          itp1o(i)=itmpnd(i)
       enddo
C
C   *******************************************************************
C   UPDATE  THE NEW cmo.
C
      ipointi=1
      ipointj=nnodes
      call cmo_set_info('ipointi',cmoout,ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_set_info('ipointj',cmoout,ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      call cmo_set_info('nnodes',cmoout,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nelements,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
C
C     ******************************************************************
C     REFRESH SOME POINTERS.
C
      call cmo_get_info('itp1',cmoout,ipitp1o,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteto,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmoout,ipjteto,ilen,icmotype,ierror)
C
C     ******************************************************************
C     SET NECESSARY INFO FOR THE OUTGOING cmo
C
      nsd=2
      nen=3
      nef=3
C
C     ******************************************************************
C
      length=nelements
      call mmgetblk('itflag',isubname,
     &              ipitflag,length,2,icscode)
      do it=1,nelements
         itflag(it)=1
      enddo
C
      length=3*nnodes
      call mmgetblk('nedge_bin',isubname,
     &              ipnedge_bin,length,2,icscode)
      call mmgetblk('nedge_off',isubname,
     &              ipnedge_off,length,2,icscode)
      do i=1,3*nnodes
         nedge_bin(i)=0
         nedge_off(i)=0
      enddo
      do it=1,nelements
         ifsum=0
         do i=1,nelmnen(itettypo(it))
            j1=itet1o(itetoffo(it)+i)
            ifsum=ifsum+j1
         enddo
         nedge_bin(ifsum)=nedge_bin(ifsum)+1
      enddo
      isum=0
      do i=1,3*nnodes
         if(nedge_bin(i).gt.0) then
            nedge_off(i)=isum
            isum=isum+nedge_bin(i)
         endif
         nedge_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('itface',isubname,ipitface,length,2,icscode)
      do i=1,length
         itface(i)=0
      enddo
      do it=1,nelements
         ifsum=0
         do i=1,nelmnen(itettypo(it))
            j1=itet1o(itetoffo(it)+i)
            ifsum=ifsum+j1
         enddo
         nedge_bin(ifsum)=nedge_bin(ifsum)+1
         itface(nedge_off(ifsum)+nedge_bin(ifsum))=it
      enddo
      do it=1,nelements
         nodemax=0
         nodemin=nnodes+1
         nodesum1=0
         nodesum4=1
         do i=1,nelmnen(itettypo(it))
            j1=itet1o(itetoffo(it)+i)
            nodemax=max(nodemax,j1)
            nodemin=min(nodemin,j1)
            nodesum1=nodesum1+j1
            nodesum4=nodesum4*j1
         enddo
         if(nedge_bin(nodesum1).eq.1) then
         else
            nodesum2=nodemax - nodesum1
            nodesum3=nodemin - nodesum1
            nodesum4=nodesum4 / (nodemax*nodemin)
            do j=1,nedge_bin(nodesum1)
               jt=itface(nedge_off(nodesum1)+j)
               if(jt.le.0.or.it.eq.jt) then
               else
                  modemax=0
                  modemin=nnodes+1
                  modesum1=0
                  modesum4=1
                  do k=1,nelmnen(itettypo(jt))
                     j1=itet1o(itetoffo(jt)+k)
                     modemax=max(modemax,j1)
                     modemin=min(modemin,j1)
                     modesum1=modesum1+j1
                     modesum4=modesum4*j1
                  enddo
                  modesum2=modemax - modesum1
                  modesum3=modemin - modesum1
                  modesum4=modesum4 / (modemax*modemin)
                  if((nodesum1-modesum1.eq.0).and.
     &               (nodesum2-modesum2.eq.0).and.
     &               (nodesum3-modesum3.eq.0).and.
     &               (nodesum4-modesum4.eq.0) ) then
                     itface(nedge_off(nodesum1)+j)=-1
                     i1=itet1o(itetoffo(it)+1)
                     i2=itet1o(itetoffo(it)+2)
                     i3=itet1o(itetoffo(it)+3)
                     x1=xic(i1)
                     y1=yic(i1)
                     z1=zic(i1)
                     x2=xic(i2)
                     y2=yic(i2)
                     z2=zic(i2)
                     x3=xic(i3)
                     y3=yic(i3)
                     z3=zic(i3)
                     a1=(y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
                     b1=(x3-x1)*(z2-z1) - (x2-x1)*(z3-z1)
                     c1=(x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
                     xmag1= sqrt(a1**2+b1**2+c1**2)
                     j1=itet1o(itetoffo(jt)+1)
                     j2=itet1o(itetoffo(jt)+2)
                     j3=itet1o(itetoffo(jt)+3)
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)
                     y2=yic(j2)
                     z2=zic(j2)
                     x3=xic(j3)
                     y3=yic(j3)
                     z3=zic(j3)
                     a2=(y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
                     b2=(x3-x1)*(z2-z1) - (x2-x1)*(z3-z1)
                     c2=(x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
                     xmag2= sqrt(a2**2+b2**2+c2**2)
                     xdot=(a1*a2+b1*b2+c1*c2)/(xmag1*xmag2+1.0d-99)
                     if((1.0-abs(xdot)).lt.1.0e-06) then
                        itflag(it)=0
                        itflag(jt)=0
                     endif
                  endif
               endif
            enddo
         endif
      enddo
      itcount=0
      itoff=0
      jtoff=0
      do it=1,nelements
         if(itflag(it).eq.1) then
             itcount=itcount+1
             itetclro(itcount)=itetclro(it)
             itettypo(itcount)=itettypo(it)
             itetoffo(itcount)=itoff
             jtetoffo(itcount)=jtoff
             do i=1,nelmnen(itettypo(it))
                itet1o(itoff+i)=itet1o(itetoffo(it)+i)
             enddo
             itoff=itoff+nelmnen(itettypo(it))
             jtoff=jtoff+nelmnef(itettypo(it))
         endif
      enddo
      nelements=itcount
C
      call cmo_set_info('nnodes',cmoout,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nelements,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
C
C     ******************************************************************
C     CONSTRUCT THE CONNECTIVITY MATRIX.
C
      do it=1,nelements
         do i=1,nelmnef(itettypo(it))
            jtet1o(jtetoffo(it)+i)=-1
         enddo
      enddo
C
C     *******************************************************************
C     GENERATE THE jtet CONNECTIVITY FOR THE OUTGOING cmo.
C
      call geniee_cmo(cmoout)
C
C     ******************************************************************
C     GET RID OF VARIABLES THAT ARE UNNECESSARY FOR SHEET CMO
C
C
      ilen=icharlnf(cmoout)
      if(if_delatt .lt. 3) then
         cbuff = 'log/tty/off ; finish'
         call dotask(cbuff,ierror)
 
      if(if_delatt .le. 2) then
C
C     isetwd
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' isetwd ; finish' 
        call dotask(cbuff,ierror)

      endif
      if(if_delatt .le. 2) then
C
C     ialias
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' ialias ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 0) then
C
C     imt1
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' imt1 ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 0) then
C
C     itp1
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' itp1 ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 1) then
C
C     icr1
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' icr1 ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 1) then
C
C     isn1
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' isn1 ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 2) then
C
C     xtetwd
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' xtetwd ; finish' 
        call dotask(cbuff,ierror)
      endif
      if(if_delatt .le. 1) then
C
C     itetclr
c
        cbuff='cmo/DELATT/' // cmoout(1:ilen) // ' itetclr ; finish' 
        call dotask(cbuff,ierror)
 
      endif
 
         cbuff = 'log/tty/on ; finish'
         call dotask(cbuff,ierror)
      endif
C
C     *******************************************************************
C     CREATE THE k-D TREE ARRAYS linkt and sbox.
C
      call kdtree(xico,yico,zico,itet1o,nelements,linkt,sbox,ierr)
 
C     *******************************************************************
C     RESTORE THE NAME OF THE ORIGINAL CURRENT MESH OBJECT
C
      call cmo_set_name(cmocurrent,ierror)
C
      goto 9999
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,ics)
 
C
      return
      end
