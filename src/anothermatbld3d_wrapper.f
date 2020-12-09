*dk,mikematmatbld3d_wrapper

      subroutine anothermatbld3d_wrapper
     x           (ifile,io_type,num_area_coef,ifcompress, ifhybrid)
 
C#######################################################################
C
C      PURPOSE -
C
C         CALL routines to build a sparse matrix for FEHM(bot).
C
C      INPUT ARGUMENTS -
C
C         ifile - Base file name (.stor is appended).
C         io_type - use io_type like io_job to write all or portions of
C                   work to different file types or attributes 
C                   note: until binary form is written, 1 = 3 = unformatted
C
C                 = 1 Output binary coefficient (stor) file.
C                 = 2 Output ascii coefficient (stor) file.
C                 = 3 Output unformatted coefficient (stor) file.
C
C                 = 5 write voronoi volumes to cmo attribute named ifile 
C                   skip coefficient work
C
C                 = 10 Write PFLOTRAN format (no 0 areas)
C                 = 12 Write PFLOTRAN format (include 0 areas)
C                 = 13 Write PFLOTRAN plus extra values such as Aij/dist 
C
C        --------------------------------------------------------------
C         NOTE!! current code only supports num_area_coef = 1
C         this value is passed into sparseMatrix routines
C         as component_of_interest global variable 
C           initialize3ddiffusionmat(num_area_coef,...
C           extractnegativecoefs(j,ipamat) where j = num_area_coef-1
C           getcomponentmatrixvalues(j,ipamat) where j = num_area_coef-1
C        --------------------------------------------------------------
C
C         num_area_coef = 1 = default hard-wired
C                           Output single component scalar area/distance
c                           coefficients THIS IS THE ONLY ONE WORKING.
C                       = 3 Output x,y,z  vector area/distance coefficients.
C                       = 4 Output scalar and vector area/distance
c                           coefficients.
C                       =-1 Output single component scalar area coefficients.
C                       =-3 Output x,y,z  vector area coefficients.
C                       =-4 Output scalar and vector area coefficients.
c
c          ifcompress   = 0 If no compression of the .stor file is desired.
C                       = 1 If compression is desired.
C      OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
c         The output file is in the FEHM .stor format
c         Refer to matbld3d_stor.f for more information.
c
c
c$Log: anothermatbld3d_wrapper.f,v $
cRevision 2.00  2007/11/05 19:45:46  spchu
cImport to CVS
c
CPVCS    
CPVCS       Rev 1.13   10 May 2001 13:52:24   jan
CPVCS    deleted embedded underscore
CPVCS    
CPVCS       Rev 1.12   19 Feb 2001 13:17:04   dcg
CPVCS    remove embedded underscores in procedure names - alpha compiler
CPVCS    wants to have double underscore after the names - this causes
CPVCS    incompatibility among platforms
CPVCS
CPVCS       Rev 1.11   Tue Nov 16 11:59:30 1999   murphy
CPVCS    Added more informational output.
CPVCS
CPVCS       Rev 1.10   Mon Sep 20 15:02:54 1999   murphy
CPVCS
CPVCS       Rev 1.9   Tue Aug 31 14:25:10 1999   murphy
CPVCS    Fixed more unformatted i/o woes.
CPVCS
CPVCS       Rev 1.8   Fri Jul 16 14:10:00 1999   murphy
CPVCS    Added code to print out informational information about negative coefficients,
CPVCS    compression ratio, and other arcane details necessary for the user.
CPVCS
CPVCS       Rev 1.7   Tue Jun 29 11:41:52 1999   murphy
CPVCS
CPVCS
CPVCS       Rev 1.6   Wed Jun 23 16:06:12 1999   murphy
CPVCS
CPVCS
CPVCS       Rev 1.5   Wed Jun 23 11:40:30 1999   murphy
CPVCS    Fixed another minor bug with unformatted output.
CPVCS
CPVCS       Rev 1.4   Wed Jun 23 10:38:12 1999   murphy
CPVCS    Fixed minor bug in unformatted output.
CPVCS
CPVCS       Rev 1.3   Wed Jun 16 11:19:22 1999   nnc
CPVCS    Fixed multiple variable declarations.
CPVCS
CPVCS       Rev 1.2   Mon Jun 14 11:01:32 1999   dcg
CPVCS    shorten name
CPVCS
CPVCS       Rev 1.1   Fri Jun 11 13:49:46 1999   murphy
CPVCS    Some DEC machines are too wimpy to handle long subroutine names.
CPVCS    So I shortened the names in anothermatbl3d.c and had to update
CPVCS    the calls.
CPVCS
CPVCS       Rev 1.0   Tue May 18 12:49:48 1999   murphy
CPVCS    Initial revision.
C#######################################################################
 
c--------------------------------------------------------------------------
c Declarations.
c--------------------------------------------------------------------------
 
      implicit none
      include "chydro.h"
      include "consts.h"
      include "local_element.h"
 
cccccccccc
c     Args
cccccccccc
      character*(*) ifile
      character*72  title_string
      integer io_type, num_area_coef, ifcompress, ifhybrid
 
ccccccccccc
c     Local
Ccccccccccc
 
c
c  Information from the CMO.
c
      integer nnodes, ilen,ityp,iout, ierror, ierr, icscode
      integer length, icmotype, ntets, lenxic, lenyic, lenzic,
     *     lenitet, lenjtet, mbndry, nef
 
C
      pointer (ipitet, itet)
      integer itet(*)
      pointer (ipjtet, jtet)
      integer jtet(*)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      integer itetoff(*),jtetoff(*)

      pointer (ipitettyp, itettyp)
      integer itettyp(*)
      pointer (ipirowdiag,irowdiag)
      integer irowdiag(*)
      pointer (ipitemp, itemp)
      integer itemp(*)
      pointer (ipitemp2, itemp2)
      integer itemp2(*)
      pointer (ipisn1, isn1)
      integer isn1(*)
      pointer (ipitp1, itp1)
      integer itp1(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipelts, elts)
      pointer (ipedges, edges)
      integer elts(*),edges(*)
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)

      pointer (ipout, out)
      integer out(*)
 
c
c Variables used in matrix computation and stats
c
 
      integer i,j,ii,jj,icnt 
      integer lenitp1, lenisn1,iflag 
      integer iip1,iip2,ip1,ip2,jstop,jstart,iinc,irow,jcol
      integer neq, neqp1, ncont,ncoefs, num_written_coefs
      integer nelts
      integer num_conn_max
      integer nsize,njerk
      integer ierrw, numnegs, numsuspectnegs, numzerocoefs
      integer entryprocessed
      integer istatus

      integer iunit, nextlun
      integer icharlnf

      pointer (ipimatptrs, imatptrs)
      integer imatptrs(*)
      pointer (ipnegrows, negrows)
      integer negrows(*)
      pointer (ipnegcols, negcols)
      integer negcols(*)

      pointer (ipij_ccoef, ij_ccoef)
      integer ij_ccoef(*)

      pointer (ipccoef, ccoef)
      real*8 ccoef(*)

      pointer (ipvolic, volic)
      real*8  volic(*)

      pointer (ipamat, amat)
      real*8  amat(*)

      pointer (ipfnegs, fnegs)
      real*8  fnegs(*)

      pointer (ipvalue,value)
      real*8  value(*)

      pointer (ipxtemp, xtemp)
      real*8 xtemp(*)

      pointer (iphybrid_factor, hybrid_factor)
      real*8 hybrid_factor(*)

      real*8 ave_con_node, volmin,volmax,voltot
      real*8 amatmin,amatmax,absamatmin,absamatmax

      real*8 compress_eps, dummy_eps

C     Text buffer for dotask
      character*256 cbuff
      character*132 dotask_command

      character*132 logmess
      character*24 string, fdate
      character*32 isubname
      character*32 cmo, ifilename
      character*32 att_name, cout
      character*32 io_string, coef_string, comp_string

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BEGIN begin
 
      compress_eps = 1.0e-8
      ave_con_node=0.0
      volmin=0.0
      volmax=0.0 
      voltot=0.0 
      istatus=0
      iunit = 0
      isubname='anothermatbld3d_wrapper'
      ifilename='-notset-'

C     catch old style, binary is now 3
      if (io_type.eq.1) io_type=3

      if (num_area_coef.ne.1) then
        logmess='AMatbld3d_stor: We only support scalar coefficients'
        call writloga('default', 0,logmess,0,ierror)
        goto 9999
      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCC
c     Get mesh information.
CCCCCCCCCCCCCCCCCCCCCCCCCCC
 
      call cmo_get_name(cmo,ierror)
      if (ierror .ne. 0) then
        write(logmess,'(a,a15)')"AMatbld3d_stor: get cmo Error: ",cmo
        call writloga('default',0,logmess,0,icscode)
        istatus = -1
        goto 9999
      endif

      call cmo_get_intinfo('idebug',cmo,idebug,ilen,ityp,ierr)
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)

      if (nnodes.le.0) then
        write(logmess,'(a,a)')
     *  "AMatbld3d_stor: cmo has no points: ",cmo(1:icharlnf(cmo))
        call writloga('default',1,logmess,1,ierrw)
        istatus = -1
        goto 9999
      endif
      if (ntets.le.0) then
        write(logmess,'(a,a)')
     *  "AMatbld3d_stor: cmo has no elements: ",cmo(1:icharlnf(cmo))
        call writloga('default',1,logmess,1,ierrw)
        istatus = -1
        goto 9999
      endif

      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                 nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,ityp,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,icmotype,ierror)
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,ityp,ierror)

C TAM
C     Check for user defined compression epsilon
C     itype -   The type of the data to be returned (I=1,R=2,C=3).
C                  (INT=1 or REAL=2, all vectors are 3)
C     error_return 0 means no error

      write(logmess,'(a,e14.7)')
     * "AMatbld3d_stor: Matrix compress_eps: ",compress_eps
      call writloga('default',1,logmess,0,icscode)

      call cmo_get_attinfo('compress_eps',cmo,iout,dummy_eps,
     *        cout,ipout,ilen,ityp, ierr)

      if (ierr.eq.0) then

         if (ityp.eq.2) then
           compress_eps = dummy_eps

           write(logmess,'(a,e14.7)')
     *    "AMatbld3d_stor: Matrix compress_eps set by user: ",
     *     compress_eps
           call writloga('default',0,logmess,0,icscode)

         else

           write(logmess,'(a,e14.7)')
     *    "AMatbld3d_stor: Matrix compress_eps unchanged: ",
     *     compress_eps
           call writloga('default',0,logmess,0,icscode)
           write(logmess,'(a)')
     *    "               compress_eps must be type REAL (2) "
           call writloga('default',0,logmess,0,icscode)
         endif

      endif

       call cmo_get_attinfo('epsilon',cmo,iout,dummy_eps,
     *        cout,ipout,ilen,ityp, ierr)
       write(logmess,'(a,e14.7)')
     * "AMatbld3d_stor: Local epsilon: ",dummy_eps
       call writloga('default',0,logmess,0,icscode)

C     add information so user knows which routine is being used.
      if (idebug.ne.0) then
      write(logmess,'(a)') "AMatbld3d_stor "
      call writloga('default',1,logmess,0,icscode)
      write(logmess,'(a,a)') "  cmo name         : ",cmo
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a,a)') "  file name        : ",ifile
      call writloga('default',0,logmess,0,icscode)

      if (io_type .eq. 2) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," fehm ascii"
      if (io_type .eq. 3) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," fehm unformatted"
      if (io_type .eq. 5) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," cmo attribute"
      if (io_type .eq. 10) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," pflotran, skip 0 areas"
      if (io_type .eq. 12) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," pflotran, keep 0 areas"
      if (io_type .eq. 13) write(logmess,'(a,i5,a)') 
     *  "  file type option: ",io_type," pflotran, extra values"

      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a,i5)') "  compress option : ",ifcompress
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a,i5)') "  coef option     : ",num_area_coef
      call writloga('default',0,logmess,1,icscode)
      endif

 
      call mmgetblk("iparent",isubname,ipiparent,nnodes,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "AMatbld3d_stor: mmgetblk failed: iparent ",icscode
        call writloga('default',0,logmess,0,icscode)
        istatus = icscode
        goto 9999
      endif
      call unpackpc(nnodes,itp1,isn1,iparent)
 
c
c elts and edges are used for get_elements_on_edge
c
      nsize = 100
      njerk = 1
 
      call mmgetblk('elts',isubname,ipelts,nsize,njerk,ierror)
      if (ierror .ne. 0) then
        write(logmess,'(a,i5)')
     *   "AMatbld3d_stor: mmgetblk failed: elts ",ierror
        call writloga('default',0,logmess,0,ierrw)
        istatus = ierror
        goto 9999
      endif

      call mmgetblk('edges',isubname,ipedges,nsize,njerk,ierror)
      if (ierror .ne. 0) then
        write(logmess,'(a,i5)')
     *   "AMatbld3d_stor: mmgetblk failed: edges ",ierror
        call writloga('default',0,logmess,0,ierrw)
        istatus = ierror
        goto 9999
      endif


CCCCCC prepare attribute CCCCCCCCCCCCCCCCCCCCCC
      if (io_type .eq. 5) then
      
        att_name = ifile
        call cmo_get_info(att_name,cmo,ipvalue,ilen,ityp,ierr)
        if (ierr.ne.0 ) then
           write(logmess,'(a,a,a)')
     *     'anothermatbld3d error: ',
     *     'Attribute to fill does not exist: ',att_name
           call writloga('default',0,logmess,0,ierr)
        endif

CCCCCC prepare file CCCCCCCCCCCCCCCCCCCCCC
      else 

c       Get next available unit number.
c       Use nextlun(), as nextlun1() can return a negative integer
        iunit = nextlun()

c       Open the file, use root ifile to make a name 
        if (io_type .ge. 10 ) then
            ifilename=ifile(1:icharlnf(ifile)) // '.uge'

c         this is verbose, maybe not a valid pflo format
          if (io_type .eq. 13 ) then
            ifilename=ifile(1:icharlnf(ifile)) // '.Vuge'
          endif
        else
            ifilename=ifile(1:icharlnf(ifile)) // '.stor'
        endif

        if(io_type.eq.3)then
         open(unit=iunit, file = ifilename, form = 'unformatted')
        else
         open(unit=iunit, file = ifilename, form = 'formatted')
        endif

      endif
 
CCCC WRITE header to file in FEHM format CCCCCCCCCCCCCCCCCCCCCC
C
C        Write header to the file.
c        We assume header 1st line is 72 characters long:
c
c formatted ascii
c fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
c 123456789012345678901234567890123456789012345678901234567890123456789012
c fehmstor ieeer8i4 LaGriT Sparse Matrix Voronoi Coefficients
c unformatted
c      possible compression types are none, coefs, graph, all
c      this version matbld3d compresses graph and or coefs (all for both)
c       matbld3d_stor with graph compression
c       06/03 09:57:46 20093-D 3-D Linear Diffusion Model (matbld3d_gstor)
c       matbld3d_stor with graph and area coefficient compression
c       06/03 09:57:46 20093-D 3-D Linear Diffusion Model (matbld3d_astor)
c 123456789012345678901234567890123456789012345678901234567890123456789012

C     Get a time stamp for file headers
      string = fdate()

c     FEHM BINARY header
      if(io_type .eq. 3)then

          write(title_string,'(a)')
     *'fehmstor ieeer8i4 LaGriT Sparse Matrix Voronoi Coefficients'
          write(iunit)title_string

          if (ifcompress.ne.0) then
          write(title_string,*)
     *      string,' 3-D Linear Diffusion Model (matbld3d_astor)'
          else
          write(title_string,*)
     *      string,' 3-D Linear Diffusion Model (matbld3d_gstor)'
          endif
          write(iunit)title_string


C     FEHM ASCII header
      elseif(io_type .eq. 2)then

           if (ifcompress.ne.0) then
           write(iunit,'(a)')
     *'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write(iunit,*)string,'3-D Linear Diffusion Model (matbld3d_astor)'
           endif

      endif 

C---------------------------------------------------------------------------
C     Build and output the sparse matrix.
C---------------------------------------------------------------------------
 
      neq=nnodes
      neqp1=neq+1

 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Call C functions to do the real work of building the matrix.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if (ifhybrid .eq. 1) then
        write(logmess,'(a)')
     *    'AMatbld3d_stor: Using hybrid median-Voronoi volumes'
        call writloga('default',1,logmess,0,icscode)

C       Create an attribute for storing the hybrid factor.
        write(cbuff, '(a,a,a)') 'cmo/addatt/', cmo,
     *      '/hybrid_factor/vdouble/scalar/nelements; finish'
        call dotask(cbuff, icscode)

        call cmo_get_info("hybrid_factor", cmo, iphybrid_factor, ilen,
     *      ityp, ierr)

C
C       Zero out the new attribute
C
        do i = 1, ntets
            hybrid_factor(i) = 0.0
        enddo
      endif

      if (idebug.gt.1) then
      write(logmess,'(a)') ">> AMatbld3d_stor: initialize3ddiffusionmat"
      call writloga('default',0,logmess,0,icscode)
      write(logmess,*) ">>  neq and ntets: ",neq,ntets
      call writloga('default',0,logmess,0,icscode)
      write(logmess,*) ">>  compress_eps: ",compress_eps
      call writloga('default',0,logmess,0,icscode)
      endif

C TAM pass compress_eps value
      call initialize3ddiffusionmat(num_area_coef,
     *     ifcompress, neq, xic(1), yic(1), zic(1),  ntets, itet(1),
     *     jtet(1), mbndry, ifhybrid, hybrid_factor(1), compress_eps)

C     call initialize3ddiffusionmat(num_area_coef,
C    *     ifcompress, neq, xic(1), yic(1), zic(1),  ntets, itet(1),
C    *     jtet(1), mbndry, ifhybrid, hybrid_factor(1))

      if (idebug.gt.1) then
      write(logmess,'(a)') "AMatbld3d_stor: check incident tets"
      call writloga('default',0,logmess,0,icscode)
      endif

C     For each each edge of every tet, decide if it has been processed.
C     If not, get all incident tets and compute the entry.

      do i=1,ntets
         do j=1,6
            iip1=itet(itetoff(i)+ielmedge1(1,j,itettyp(i)))
            iip2=itet(itetoff(i)+ielmedge1(2,j,itettyp(i)))
            ip1=iparent(iip1)
            ip2=iparent(iip2)
            if (entryprocessed(ip1,ip2).eq.0) then
 
               call get_elements_on_edge(i,j,nelts,ipelts,ipedges,
     *              ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,
     *              ipiparent,nef, mbndry)

               call computeentry(ip1, ip2, nelts,elts(1),edges(1))
            endif
         enddo
      enddo
      call finalscalar3ddiffusionmat()

      if (idebug.gt.1) then
      write(logmess,'(a)') "AMatbld3d_stor: print coeff information"
      call writloga('default',0,logmess,0,icscode)
      write(logmess,*)"mbndry and num_area_coef ",mbndry,num_area_coef
      call writloga('default',0,logmess,0,icscode)
      endif

 
cccccccccccccccccccccccccccccc
c print negative coefficients.
cccccccccccccccccccccccccccccc
c     note test is using maximum[component_of_interest]*epsilon
c     where SparseMatrix compress_eps = 1e-8 by default or user setatt

      j=num_area_coef - 1
      call extractnegativecoefs(j,numnegs,numsuspectnegs,numzerocoefs,
     *                              ipnegrows,ipnegcols,ipfnegs)
      if (numnegs.eq.0) then
         write(logmess,'(a,i10)')
     *   "AMatbld3d_stor: *****Zero Negative Coefficients ******"
         call writloga('default',0,logmess,0,ierrw)
 
C        change 10e-8*max so 10e-8 can be read from reporting
         write(logmess,'(a,i10)')
     *        "AMatbld3d_stor: Number of 'zero' (< compress_eps) coefs",
     *        numzerocoefs
         call writloga('default',0,logmess,0,ierrw)
 
 
      else
         write(logmess,'(a)')
     *        "AMatbld3d_stor: *****Negative Coefficients ******"
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i10)')
     *        "AMatbld3d_stor: Total Number of Negative Coefficients",
     *        numnegs
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i10)')
     *        "AMatbld3d_stor: Number of Significant Negative Coefs",
     *        numsuspectnegs
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i10)')
     *        "AMatbld3d_stor: Number of 'zero' (< 10e-8 *max) coefs",
     *        numzerocoefs
         call writloga('default',0,logmess,0,ierrw)
 
 
         do i=1,numsuspectnegs
            write(logmess,'(a,i8,a,i8,a,1pe15.7)')
     *      "Negative coef at row ",negrows(i),
     *      "  Column ",negcols(i)," with value ",fnegs(i)
            call writloga('default',0,logmess,0,ierrw)
         enddo

         if (ifhybrid .eq. 1 .and. numsuspectnegs .gt. 0) then
             write(logmess, '(a,a,a)')
     *       'AMatbld3d_stor: Warning: Hybridization was unable to ',
     *       'fix all negative coefficients. This mesh is probably ',
     *       'non-Delaunay.'
            call writloga('default',0,logmess,0,ierrw)
         endif

cccccccccccccccccccccccccccccc
c Create two new node vectors, ccoef, ij_ccoef
c Put the negative ij coefficient value into the two
c nodes connected to the ij edge.
c
c The vector ij_coef will assign the j index value to node i so that
c one can determine which edge is associated with the negtive coefficient
c that is assigned to nodes.
cccccccccccccccccccccccccccccc
C
C    Locate or allocate ccoef vector
C
      call mmfindbk('ccoef',cmo,ipccoef,ilen,ierror)
      if(ierror.ne.0) then
         dotask_command = 'cmo/addatt/' //
     >                     cmo(1:icharlnf(cmo)) //
     >                     '/' //
     >     'ccoef/VDOUBLE/scalar/nnodes/linear/permanent/afgx/0.0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >      call x3d_error(isubname,'ccoef')
         call mmfindbk('ccoef',cmo,ipccoef,ilen,ierror)
 
      else
        if (ilen.lt.nnodes) then
           write(logmess,'(a,2i10)')
     *         "AMatbld3d_stor: Odd condition (ilen.lt.nnodes)",
     *          ilen, nnodes
           call writloga('default',0,logmess,0,ierror)
           call cmo_newlen(cmo,ierror)
        endif
       endif
C
C     Locate or allocate ij_coef array
C
      call mmfindbk('ij_ccoef',cmo,ipij_ccoef,ilen,ierror)
      if(ierror.ne.0) then
         dotask_command = 'cmo/addatt/' //
     >                     cmo(1:icharlnf(cmo)) //
     >                     '/' //
     >     'ij_ccoef/VINT/scalar/nnodes/linear/permanent/afgx/0.0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >      call x3d_error(isubname,'ij_ccoef')
         call mmfindbk('ij_ccoef',cmo,ipij_ccoef,ilen,ierror)
      endif
c
c     Now fill the vectors
c
      do i = 1,nnodes
        ccoef(i) = 0.0
        ij_ccoef(i) = 0
      enddo
c
c     Only set values if the coef is more negative than the presently
c     assigned value.
c
      do i=1,numsuspectnegs
        if(fnegs(i) .lt. ccoef(negrows(i)))then
           ccoef(negrows(i)) = fnegs(i)
           ij_ccoef(negrows(i)) = negcols(i)
        endif
        
        if(fnegs(i) .lt. ccoef(negcols(i)))then
           ccoef(negcols(i)) = fnegs(i)
           ij_ccoef(negcols(i)) = negrows(i)
        endif

      enddo
      endif
c
c     Finished reporting negative coefficients
c
      call freenegcoefs()


 
cccccccccccccccccccccccccccc
C     Third line of file with matrix sizes
c     Write to file if io_type is less than 5
cccccccccccccccccccccccccccc

      call getmatrixsizes(num_written_coefs,ncoefs,num_conn_max)
      ncont=neqp1+ncoefs

      if(io_type .eq. 3) then
         write(iunit) num_written_coefs,neq,ncont,
     x                num_area_coef, num_conn_max
      elseif(io_type .eq. 2) then
         write(iunit,9010) num_written_coefs,neq,ncont,
     x                num_area_coef, num_conn_max
      endif
 
      length = ncont
 
c     ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Output some statistics about stuff on the third line.
c     ccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      ave_con_node = float(ncoefs)/float(neq)
 
      write(logmess,'(a,i8,a,i10)')
     *   "AMatbld3d_stor: npoints = ",neq,
     *   "  ncoefs = ",ncoefs
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,i10)')
     *"AMatbld3d_stor: Number of unique coefs =", num_written_coefs
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,i10)')
     *"AMatbld3d_stor: Maximum num. connections to a node = ",
     * num_conn_max
      call writloga('default',0,logmess,0,ierrw)
 
 
ccccccccccccccccccccccccccc
c  Output Voronoi volumes.
c  Write to file or attribute
c  Write in PFLOTRAN or FEHM format
ccccccccccccccccccccccccccc

      call getvoronoivolumes(ipvolic)

      if(io_type .eq. 3)then
         write(iunit) (volic(i),i=1,neq)

      elseif(io_type .eq. 2)then

         write(iunit,9000) (volic(i),i=1,neq)
 9000    format(5(1pe20.12))

      elseif(io_type .eq. 5)then

         do i = 1,neq
           value(i) = volic(i)
         enddo

      elseif (io_type .ge. 10) then

         call dump_pflo_stor_cells(iunit, io_type,
     *      xic,yic,zic,volic,nnodes)

      endif

c     cccccccccccccccccccccccccccccccccccccccccccccc
c     Compute some statistics about voronoi volumes.
c     cccccccccccccccccccccccccccccccccccccccccccccc

      volmin = volic(1)
      volmax = volic(1)
      voltot = volic(1)
      do i=2,neq
         if (volmin.gt.volic(i)) then
            volmin = volic(i)
         endif
         if (volmax.lt.volic(i)) then
            volmax = volic(i)
         endif
         voltot = voltot + volic(i)
      enddo
 
c     ccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Output some statistics about Voronoi Volume stuff.
c     ccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      write(logmess,'(a,1pe15.7)')
     *   "AMatbld3d_stor: Volume min = ",volmin
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "AMatbld3d_stor: Volume max = ",volmax
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,3(1pe15.7))')
     *   "AMatbld3d_stor: Total Volume: ",voltot
      call writloga('default',0,logmess,0,ierrw)

C     there may be a sneaky un-released pointer still in the code
C     this is just some output in case things go awry
      if (volic(1) .lt.0 .and. volmin.ge.0 ) then
         write(logmess,'(a)')
     *   "AMatbld3d_stor: WARNING: volume pointer may be stale. "
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,'(a,1pe15.7)')
     *   "First value is ",volic(1)
         call writloga('default',0,logmess,1,ierrw)
      endif
 
      call freevoronoivolumes()

cccccccccccccccccccccccccccccccccccccccccccccccccc
c GET and WRITE the matrix
c Output entries per row and  entries of each row.
c (Also known as occupied columns.)
cccccccccccccccccccccccccccccccccccccccccccccccccc
 
      call getentriesperrow(ipitemp)
      call getoccupiedcolumns(ipitemp2)
 
      if(io_type .eq. 3)then
         write(iunit) (itemp(i),i=1,neqp1), (itemp2(i),i=1,ncoefs)
      elseif(io_type .eq. 2)then
         write(iunit,9010) (itemp(i),i=1,neqp1)
         write(iunit,9010) (itemp2(i),i=1,ncoefs)
 9010    format(5i10)
      endif

C TAM
c     print*,"itemp ij_ptrs(neqp1) ",neqp1
c     print*,(itemp(i),i=1,neqp1)
c     print*,"itemp2 ij_list(ncoefs) ",ncoefs
c     print*,(itemp2(i),i=1,ncoefs)

C      these are needed later for pflotran format
       if (io_type .lt. 10) call freeentriesperrow()
       if (io_type .lt. 10) call freeoccupiedcolumns()
 
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Get the imatptrs, and the diagonals and print them out, with all
c     the extra George padding in the middle.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      call getmatrixpointers(ipimatptrs,ipirowdiag)
      njerk = 0
 
      if(io_type .eq. 3) then
         write(iunit)      (imatptrs(i),i=1,ncoefs), (njerk,i=1,neqp1)
         write(iunit)      (1+irowdiag(i)+neqp1,i=1,neq)
      elseif(io_type .eq. 2)then
         write(iunit,9010) (imatptrs(i),i=1,ncoefs), (njerk,i=1,neqp1)
         write(iunit,9010) (1+irowdiag(i)+neqp1,i=1,neq)
      endif

C     these are needed in the pflotran files 
      if (io_type .lt. 10) call freematrixpointers()


cccccccccccccccccccccccccccccccccccccccccccccc
c     FINSH WRITING with coef matrix values
c     Output the matrix values, fill array amat
c     Get them one component at a time because
c     of row-major vs. column-major problems.
cccccccccccccccccccccccccccccccccccccccccccccc
 
C    LOOP goes from 0 to 0, other options not implemented
C    This loop encompasses both the FEHM and PFLOTRAN coef output
C    so getcomponentmatrixvalues(j,ipamat) is called at top of loop
C    where j = component_of_interest 

      do j=0,num_area_coef-1
         call getcomponentmatrixvalues(j,ipamat)
 
c        ccccccccccccccccccccccccccccccccccccccccccccccccccc
c        Compute some statistics about the "amat" values.
c        ccccccccccccccccccccccccccccccccccccccccccccccccccc
         if (j.eq.num_area_coef-1) then
 
            absamatmin = abs(amat(1))
            absamatmax = abs(amat(1))
            amatmin = amat(1)
            amatmax = amat(1)
 
            do i=1,num_written_coefs
               if (absamatmin.gt.abs(amat(i))) then
                  absamatmin = abs(amat(i))
               endif
               if (absamatmax.lt.abs(amat(i))) then
                  absamatmax = abs(amat(i))
               endif
 
               if (amatmin.gt.amat(i)) then
                  amatmin = amat(i)
               endif
               if (amatmax.lt.amat(i)) then
                  amatmax = amat(i)
               endif

C debug        
C              if (idebug.gt.1) then
C                write(logmess,'(a,i8,a,i8,a,1pe15.7)')
C    *           "CCOEF at row ",i,
C    *           "  Column ",j," with value ",amat(i)
C                call writloga('default',0,logmess,0,ierrw)
C              endif

            enddo

c      report stats to screen output

            if(num_area_coef .gt. 0)then
               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: abs(Aij/xij) min = ",absamatmin
               call writloga('default',0,logmess,0,ierrw)


               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: abs(Aij/xij) max = ",absamatmax
               call writloga('default',0,logmess,0,ierrw)

               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: (Aij/xij) max = ",amatmax
               call writloga('default',0,logmess,0,ierrw)

               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: (Aij/xij) min = ",amatmin
               call writloga('default',0,logmess,0,ierrw)

            elseif(num_area_coef .lt. 0)then
               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: abs(Aij) min = ",absamatmin
               call writloga('default',0,logmess,0,ierrw)

               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: abs(Aij) max = ",absamatmax
               call writloga('default',0,logmess,0,ierrw)

               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: (Aij) min = ",amatmin
               call writloga('default',0,logmess,0,ierrw)

               write(logmess,'(a,1pe15.7)')
     *              "AMatbld3d_stor: (Aij) max = ",amatmax
               call writloga('default',0,logmess,0,ierrw)

            endif
       endif

c      ccccccccccccccccccccccccccccccccccccccccccccccccccc
C      WRITE PFLOTRAN format first so we can free pointers
       if (io_type .ge. 10 .and. io_type .lt.20) then

C
C    For writing pflotran files, need the following arrays:
C    neq=nnodes
C    itemp (neq+1) i= value-neq, j = 0 to (next value - current val) 
C    itemp2(coefs) node pairs, use itemp for i,j start and increments
C    amat(filled with call to getcomponentmatrixvalues(j,ipamat))
C    amatptrs(coefs) values are index into amat values
C          use itemp for i,j start and increments
C
C    These arrays can be passed as they are.
C      itemp(neqp1) = ij_ptrs = ptrs into ij edge list
C      itemp2(ncoefs) = ij_list = ij edge list
C      The area array needs to be created.
C      ij_area(nnodes) = Aij for each edge from amat and amatptrs

C         FILL xtemp with areas 1 thru nnodes
          ilen = ncoefs
          call mmgetblk('xtemp',isubname,ipxtemp,ilen,2,ierror)
          if (ierror.ne.0 .or. ilen.ne.ncoefs) then
            write(logmess,'(a,i5)')
     *    "AMatbld3d_stor PFLO: mmgetblk failed: xtemp ",ierror
            call writloga('default',1,logmess,1,ierrw)
            istatus = ierror
            goto 9999
          endif
      
          icnt=0
          do irow = 1,neq
             jstart = itemp(irow)-neq
             jstop = (itemp(irow+1)-itemp(irow))-1
             do jcol = 0, jstop
               icnt= icnt+1
               iip1 = jstart+jcol
c              we have to undo the negative coef here
c              2D creates a non-negative amat array
               xtemp(iip1) = -1 * amat(imatptrs(iip1))
             enddo
          enddo

          if (icnt .ne. ncoefs) 
     *    call x3d_error(isubname,"ij differs from ncoefs")

c         pass matbld values unaltered so they can be modified
c         in one place for both 2D and 3D output
          call dump_pflo_coefs(iunit,io_type,
     *           xic,yic,zic,
     *           itemp, itemp2, xtemp, amatmax,
     *           nnodes,ncoefs)

          call mmrelblk('xtemp',isubname,ipxtemp,ierror)

C         Now release pointers we are finished using
          call freevoronoivolumes()
          call freeentriesperrow()
          call freeoccupiedcolumns()
          call freematrixpointers()

       endif
C      end writing coefs for PFLOTRAN format
 
c      ccccccccccccccccccccccccccccccccccccccccccccccccccc
C      WRITE FEHM format 
       if(io_type .eq. 3)then
          write(iunit)      (amat(i),i=1,num_written_coefs)
       elseif(io_type .eq. 2)then
          write(iunit,9000) (amat(i),i=1,num_written_coefs)
       endif
C      END writing coefs for FEHM 
 
       call freematrixvalues()
      enddo
C     END loop writing coefficient for FEHM and PFLOTRAN 

 
 
ccccccccccc
c Shutdown! Finish and Report 
ccccccccccc

9999  continue

C convert options to readable strings and report
C
C io_type = 2 Output ascii coefficient (stor) file.
C         = 1 = 3 Output unformatted coefficient (stor) file.
C         = 5 no stor, write voronoi volumes to cmo attribute named ifile
C
C num_area_coef = 1 Output single component scalar area/distance coefficients.
C         = 3 Output x,y,z  vector area/distance coefficients.
C         = 4 Output scalar and vector area/distance coefficients.
C         =-1 Output single component scalar area coefficients.
C         =-3 Output x,y,z  vector area coefficients.
C         =-4 Output scalar and vector area coefficients.
c
c  ifcompress   = 0 If no compression of the .stor file is desired. (graph = gstor)
C               = 1 If compression is desired. (all = _astor)
C              (note matbld3d_stor writes _cstor and _nstor (coefs and none))

      io_string =  'not set                        '
      coef_string ='not set                        ' 
      comp_string ='not set                        ' 

      if (io_type.eq.1) io_string='unformatted'
      if (io_type.eq.2) io_string='ascii'
      if (io_type.eq.3) io_string='unformatted'
      if (io_type.eq.5) io_string='attribute'
      if (io_type.eq.10) io_string='PFLOTRAN'
      if (io_type.eq.12) io_string='PFLOTRAN (all)'
      if (io_type.eq.13) io_string='PFLOTRAN (verbose)'

      if (num_area_coef.eq.1) coef_string = 'scalar area/distance'
      if (num_area_coef.eq.3) coef_string = 'vector area/distance'
      if (num_area_coef.eq.4) 
     *    coef_string = 'scalar and vector area/distance'
      if (num_area_coef.eq. -1) coef_string = 'scalar area'
      if (num_area_coef.eq. -3) coef_string = 'vector area'
      if (num_area_coef.eq. -4) coef_string = 'scalar and vector area'

      if (ifcompress.eq. 1) 
     * comp_string='graph and coefficient values'
      if (ifcompress.eq. 0) 
     *comp_string= 'graph, not coefficient values'

C Report any errors
C
      if (istatus .ne. 0) then
        call mmprint()
        write(logmess,'(a,i5)')
     *  "AMatbld3d_stor: ERROR could not finish: ",istatus
        call writloga('default',1,logmess,0,ierrw)

        write(logmess,'(a,a32)')
     *  "Attempted sparse matrix with ",
     *  comp_string
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,a32)')
     *  "with coefficients written as ",
     *  coef_string
        call writloga('default',0,logmess,0,ierrw)

        if (ifcompress.eq.0) then
        write(logmess,'(a)')
     *  "  *** _gstor SPARSE COEFFICIENT MATRIX ERROR ***"
        call writloga('default',1,logmess,0,ierrw)
        else
        write(logmess,'(a)')
     *  "  *** _astor SPARSE COEFFICIENT MATRIX ERROR ***"
        call writloga('default',1,logmess,0,ierrw)
        endif
        write(logmess,'(a)')
     *  "*** INCOMPLETE or NO STOR FILE WRITTEN!! ***"
        call writloga('default',0,logmess,1,ierrw)

       else


        write(logmess,'(a,a32)')
     *  "AMatbld3d_stor Matrix coefficient values stored as ",
     *  coef_string 
        call writloga('default',0,logmess,0,ierrw)

        if (io_type .lt. 10) then

        write(logmess,'(a,a32)')
     *  "AMatbld3d_stor Matrix compression used for ",
     *  comp_string
        call writloga('default',0,logmess,0,ierrw)

        if (io_type .eq. 5) then
        write(logmess,'(a,a,a)')
     *  cmo(1:icharlnf(cmo)),
     *  " attribute with voronoi volumes created with name ",
     *  ifile(1:icharlnf(ifile)) 
        call writloga('default',0,logmess,0,ierrw)
        else
        write(logmess,'(a,a,a)')
     *  io_string(1:icharlnf(io_string)),
     *  " STOR file written with name ",
     *  ifilename(1:icharlnf(ifilename)) 
        call writloga('default',0,logmess,0,ierrw)
        endif
        
        if (ifcompress .eq. 0) then
        write(logmess,'(a)')
     *  "*** SPARSE COEFFICIENT MATRIX _gstor SUCCESSFUL ***"
        call writloga('default',1,logmess,1,ierrw)
        else
        write(logmess,'(a)')
     *  "*** SPARSE COEFFICIENT MATRIX _astor SUCCESSFUL ***"
        call writloga('default',1,logmess,1,ierrw)
        endif

        endif

        if (io_type .ge. 10) then
        write(logmess,'(a)')
     *  "*** SPARSE COEFFICIENT MATRIX for PFLOTRAN SUCCESSFUL ***"
        call writloga('default',1,logmess,1,ierrw)
        endif

      endif
C     end report

       write(logmess,'(a,a)')
     *  "3D Matrix Coefficient file written with name ",
     *  ifilename(1:icharlnf(ifilename))
        call writloga('default',0,logmess,1,ierrw)


      call killsparsematrix()
      call mmrelprt(isubname,icscode)
      if (iunit .ne. 0) close(iunit)
 
      return
      end

