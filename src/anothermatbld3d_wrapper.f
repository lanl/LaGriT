*dk,mikematmatbld3d_wrapper

      subroutine anothermatbld3d_wrapper
     x           (ifile,io_type,num_area_coef,ifcompress)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1999                              C
C This program was prepared by the Regents of the University of        C
C California at Los Alamos National Laboratory (the University) under  C
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). C
C The University has certain rights in the program pursuant to the     C
C contract and the program should not be copied or distributed outside C
C your organization. All rights in the program are reserved by the DOE C
C and the University. Neither the U.S. Government nor the University   C
C makes any warranty, express or implied, or assumes and liability or  C
C responsibility for the use of this software.                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
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
C         num_area_coef = 1 Output single component scalar area/distance
c                           coefficients.
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
      integer io_type, num_area_coef, ifcompress
 
ccccccccccc
c     Local
Ccccccccccc
 
c
c  Information from the CMO.
c
      integer nnodes, ilen,ityp,ierror, ierr, icscode
      integer length, icmotype, ntets, lenxic, lenyic, lenzic,
     x     lenitet, lenjtet, mbndry
 
      integer nef
 
      character*32 cmo, ifilename
      character*24 string, fdate
      character*32 isubname
C    
      character*32 io_string, coef_string, comp_string
      character*132 dotask_command
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer itet(4*1000000)
      pointer (ipitet, itet)
      integer jtet(4*1000000)
      pointer (ipjtet, jtet)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      pointer (ipitettyp, itettyp)
      pointer (ipirowdiag,irowdiag)
      integer irowdiag(1000000)
      integer itettyp(1000000)
      pointer (ipitemp, itemp)
      integer itemp(10000000)
      pointer (ipitemp2, itemp2)
      integer itemp2(10000000)
      pointer (ipisn1, isn1)
      integer isn1(1000000)
      pointer (ipitp1, itp1)
      integer itp1(1000000)
      pointer (ipiparent, iparent)
      integer iparent(1000000)
      pointer (ipelts, elts)
      pointer (ipedges, edges)
      integer elts(1000000),edges(1000000)
 
      integer itetoff(4*1000000),jtetoff(4*1000000)
      real *8 xic, yic, zic
      dimension xic(1000000), yic(1000000), zic(1000000)
 
c
c Variables used in matrix computation and stats
c
 
      integer i,j
      integer iunit, nextlun
      integer lenitp1, lenisn1 
      integer iip1, iip2, ip1, ip2
      integer neq, neqp1, ncont,ncoefs, num_written_coefs
      integer nelts
      integer num_conn_max
      integer nsize,njerk
      integer ierrw, numnegs, numsuspectnegs, numzerocoefs
      integer entryprocessed
      integer istatus

      integer icharlnf

      pointer (ipimatptrs, imatptrs)
      integer imatptrs(10000000)
      pointer (ipnegrows, negrows)
      integer negrows(1000000)
      pointer (ipnegcols, negcols)
      integer negcols(1000000)
 
      real*8 ave_con_node, volmin,volmax,voltot
      real*8 amatmin,amatmax,absamatmin,absamatmax

      pointer (ipccoef, ccoef)
      real*8 ccoef(*)
      pointer (ipij_ccoef, ij_ccoef)
      integer ij_ccoef(*)

c     real*8  volic
c     pointer (ipvolic,volic)
c     dimension volic(1000000)

      pointer (ipvolic, volic)
      real*8  volic(1000000)

      pointer (ipamat, amat)
      real*8  amat(1000000)

      pointer (ipfnegs, fnegs)
      real*8  fnegs(1000000)

      pointer (ipvalue,value)
      real*8  value(1000000)

c Variables for attribute in cmo

      character*32 att_name
      character*132 logmess
 
CCCCCCCCCCCCCCCCCCCCCCCCCCC
c     Get mesh information.
CCCCCCCCCCCCCCCCCCCCCCCCCCC
      ave_con_node=0.0
      volmin=0.0
      volmax=0.0 
      voltot=0.0 
 
      isubname='anothermatbld3d_wrapper'
      istatus=0

      if (num_area_coef.ne.1) then
        logmess='AMatbld3d_stor: We only support scalar coefficients'
        call writloga('default', 0,logmess,0,ierror)
        goto 9999
      endif
 
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

C     add information so user knows which routine is being used.
      if (idebug.ne.0) then
      write(logmess,'(a)') "AMatbld3d_stor "
      call writloga('default',1,logmess,0,icscode)
      write(logmess,'(a,a)') "  cmo name         : ",cmo
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a,a)') "  file name        : ",ifile
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a,i5)') "  file type option: ",io_type
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

 
C---------------------------------------------------------------------------
C     Build and output the sparse matrix.
C---------------------------------------------------------------------------

 
cccccccccccccccc
c     open file or prepare cmo attribute 
      iunit=0
      if (io_type .eq. 5) then
      
        att_name = ifile
        call cmo_get_info(att_name,cmo,ipvalue,ilen,ityp,ierr)
        if (ierr.ne.0 ) then
           write(logmess,'(a,a,a)')
     *     'anothermatbld3d error: ',
     *     'Attribute to fill does not exist: ',att_name
           call writloga('default',0,logmess,0,ierr)
        endif

      else 
cccccccccccccccc
c     write to file using root ifile to make a name 
        ifilename=ifile(1:icharlnf(ifile)) // '.stor'
c       Get next available unit number.
c       Use nextlun(), as nextlun1() can return a negative integer
        iunit = nextlun()
c       Open the file.
        if (io_type.eq.1) io_type=3
        if(io_type.eq.3)then
         open(unit=iunit, file = ifilename, form = 'unformatted')
        elseif(io_type .eq. 2)then
         open(unit=iunit, file = ifilename, form = 'formatted')
        endif
 
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

C       Get a time stamp for the file header, placed on the second line.
        string = fdate()
 
        if(io_type .eq. 3)then
c       BINARY header
          write(title_string,'(a)')
     1'fehmstor ieeer8i4 LaGriT Sparse Matrix Voronoi Coefficients'
          write(iunit)title_string

          if (ifcompress.ne.0) then
          write(title_string,*)
     1      string,' 3-D Linear Diffusion Model (matbld3d_astor)'
          else
          write(title_string,*)
     1      string,' 3-D Linear Diffusion Model (matbld3d_gstor)'
          endif
          write(iunit)title_string


        elseif(io_type .eq. 2)then
C       ASCII header

           if (ifcompress.ne.0) then
           write(iunit,'(a)')
     1'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write(iunit,*)string,'3-D Linear Diffusion Model (matbld3d_astor)'
            else
           write(iunit,'(a)')
     1'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write(iunit,*)string,'3-D Linear Diffusion Model (matbld3d_gstor)'
            endif

        endif
      endif
cccccccccccccccc
c end attribute or file io setup
 
      neq=nnodes
      neqp1=neq+1
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Call C functions to do the real work of building the matrix.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if (idebug.ne.0) then
      write(logmess,'(a)') "AMatbld3d_stor: initialize3ddiffusionmat"
      call writloga('default',1,logmess,0,icscode)
      write(logmess,*) "  neq and ntets: ",neq,ntets
      call writloga('default',0,logmess,0,icscode)
      endif

      call initialize3ddiffusionmat(num_area_coef,
     x     ifcompress, neq, xic(1), yic(1), zic(1),  ntets, itet(1))

      if (idebug.ne.0) then
      write(logmess,'(a)') "AMatbld3d_stor: check incident tets"
      call writloga('default',1,logmess,0,icscode)
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

      if (idebug.ne.0) then
      write(logmess,'(a)') "AMatbld3d_stor: print coeff information"
      call writloga('default',1,logmess,0,icscode)
      write(logmess,*)"mbndry and num_area_coef ",mbndry,num_area_coef
      call writloga('default',0,logmess,0,icscode)
      endif

 
cccccccccccccccccccccccccccccc
c print negative coefficients.
cccccccccccccccccccccccccccccc
      j=num_area_coef - 1
      call extractnegativecoefs(j,numnegs,numsuspectnegs,numzerocoefs,
     *                              ipnegrows,ipnegcols,ipfnegs)
      if (numnegs.eq.0) then
         write(logmess,'(a,i10)')
     *   "AMatbld3d_stor: *****Zero Negative Coefficients ******"
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i8)')
     *        "AMatbld3d_stor: Number of 'zero' (< 10e-8*max) coefs",
     *        numzerocoefs
         call writloga('default',0,logmess,0,ierrw)
 
 
      else
         write(logmess,'(a)')
     *        "AMatbld3d_stor: *****Negative Coefficients ******"
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i8)')
     *        "AMatbld3d_stor: Total Number of Negative Coefficients",
     *        numnegs
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i8)')
     *        "AMatbld3d_stor: Number of Significant Negative Coefs",
     *        numsuspectnegs
         call writloga('default',0,logmess,0,ierrw)
 
         write(logmess,'(a,i8)')
     *        "AMatbld3d_stor: Number of 'zero' (< 10e-8*max) coefs",
     *        numzerocoefs
         call writloga('default',0,logmess,0,ierrw)
 
 
         do i=1,numsuspectnegs
            write(logmess,'(a,i8,a,i8,a,1pe15.7)')
     *      "Negative coef at row ",negrows(i),
     *      "  Column ",negcols(i)," with value ",fnegs(i)
            call writloga('default',0,logmess,0,ierrw)
         enddo
cccccccccccccccccccccccccccccc
c Create two new node vectors, ccoef, ij_ccoef
c Put the negative ij coefficient value into the two
c nodes connected to the ij edge.
c
c The vector ij_coef will assign the j index value to node i so that
c one can determine which edge is associated with the neative coefficient
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
     *   "  n connections = ",ncoefs
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,i10)')
     *"AMatbld3d_stor: Number of written coefs =", num_written_coefs
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,i10)')
     *"AMatbld3d_stor: Maximum num. connections to a node = ",
     * num_conn_max
      call writloga('default',0,logmess,0,ierrw)
 
 
ccccccccccccccccccccccccccc
c  Output Voronoi volumes.
c  Write to file or attribute
ccccccccccccccccccccccccccc

      call getvoronoivolumes(ipvolic)

      if(io_type .eq. 3)then
         write(iunit)      (volic(i),i=1,neq)

      elseif(io_type .eq. 2)then
         write(iunit,9000) (volic(i),i=1,neq)
 9000    format(5(1pe20.12))

      elseif(io_type .eq. 5)then
         do i = 1,neq
           value(i) = volic(i)
         enddo

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

      if (volic(1) .lt.0 ) then
         write(logmess,'(a)')
     *   "AMatbld3d_stor: WARNING: volume pointer may be stale. "
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,'(a,1pe15.7)')
     *   "First value is ",volic(1)
         call writloga('default',0,logmess,1,ierrw)
      endif
 
      call freevoronoivolumes()

 
 
cccccccccccccccccccccccccccccccccccccccccccccccccc
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

      call freeentriesperrow()
      call freeoccupiedcolumns()
 
 
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
      call freematrixpointers()
 
cccccccccccccccccccccccccccccccccccccccccccccc
c     Output the matrix values.
c     Get them one component at a time because
c     of row-major vs. column-major problems.
cccccccccccccccccccccccccccccccccccccccccccccc
 
      do j=0,num_area_coef-1
         call getcomponentmatrixvalues(j,ipamat)
 
         if(io_type .eq. 3)then
            write(iunit)      (amat(i),i=1,num_written_coefs)
         elseif(io_type .eq. 2)then
            write(iunit,9000) (amat(i),i=1,num_written_coefs)
         endif
 
         if (j.eq.num_area_coef-1) then
 
c          ccccccccccccccccccccccccccccccccccccccccccccccccccc
c          Compute some statistics about the "amat" values.
c          ccccccccccccccccccccccccccccccccccccccccccccccccccc
 
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
 
 
            enddo
 
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
         call freematrixvalues()
      enddo


 
 
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
     *  "Compression used for ",
     *  comp_string
        call writloga('default',0,logmess,0,ierrw)

        write(logmess,'(a,a32)')
     *  "The area coefficient values were written as ",
     *  coef_string 
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
C     end report

      call killsparsematrix()
      call mmrelprt(isubname,icscode)
      if (iunit .ne. 0) close(iunit)
 
      return
      end
