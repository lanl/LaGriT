
C#######################################################################
C     Terry Miller tamiller@lanl.gov
C
C     Write coefficient matrix (stor) style values in PFLOTRAN format
C     Split into 2 subroutines for the 2 blocks of information
C
C     Note: The cells in PFLOTRAN are Voronoi cells with a Voronoi
C       volume for each node. Nodes are assumed to be on boundaries,
C       PFLOTRAN does not like nodes on boundaries. This is unresolved.
C
C     The Coefficient Matrix is created by matbld routines in
C     FEHM stor convention. The Voronoi Coefficients are divided
C     by distance, we mult by distance here so Aij is written.
C
C#######################################################################

C -----------------------------------------------------------------
      subroutine dump_pflo_stor_cells
     *      (iunit,io_type,xp,yp,zp,vorvol,nnodes)
C 
C#######################################################################
C The first block are the list of ids of cells and the 
C coordinates of cell centroids and the volumes of the cells.
C The PFLOTRAN cells are Voronoi volumes, one for each node
C
C CELLS <integer>    integer = # cells (N)
C id_1 x_1 y_1 z_1 volume_1
C id_2 x_2 y_2 z_2 volume_2
C ...
C ...
C id_N x_N y_N z_N volume_N
C
C#######################################################################
      implicit none

C     args
      integer iunit, io_type, nnodes
      real*8 xp(nnodes), yp(nnodes), zp(nnodes), vorvol(nnodes)

C     local variables

      integer*4 iunit4,lunget
      integer i,ics,n

      character*132 logmess
      character*32 isubname

C#######################################################################
C BEGIN begin
      isubname = "dump_pflo_stor_cells"

c     currently, there is no verbose version for CELL section
c     io_type is currently ignored
      if (io_type .eq. 13) then
        write(logmess,'(a,i10)')"PFLOTRAN modified to write verbose."
        call writloga('default',0,logmess,0,ics)
      endif
     
      if (iunit.eq.6) then
         write(iunit,'(a,i5)')" Writing to screen: ",iunit
      endif
    
      if (nnodes .gt. 0) then
        write(logmess,'(a,i10)')"PFLOTRAN total CELL nodes =   ",nnodes
        call writloga('default',0,logmess,0,ics)
      else
        write(logmess,'(a)')"PFLOTRAN Early Exit, 0 CELL nodes!"
        call writloga('default',0,logmess,1,ics)
        goto 9999
      endif

C     HEADING LINE
      write(iunit,'(a,1x,i10)')"CELLS ",nnodes

C     COORDINATES and VORONOI VOLUMES
      i=0
      do n = 1,nnodes
        write(iunit,9000) n,xp(n),yp(n),zp(n),vorvol(n)
        i=i+1
      enddo

      if (i .ne. nnodes) then
        write(logmess,'(a,i10)')"PFLOTRAN Early Exit, num written = ",i
        call writloga('default',0,logmess,1,ics)
      endif

c     print*," -Done writing cells: ",nnodes

 9000 format(i10,4(1x,1pe20.12))

 9999 return
      end

C#######################################################################

C -----------------------------------------------------------------
      subroutine dump_pflo_coefs
     *      (iunit,io_type,xp,yp,zp,ij_ptrs,ij_list,ij_area,
     *           amax,nnodes,ncoefs)

C#######################################################################
C arrays are passed in from matbld stor 2D or 3D and are unaltered
C need to reverse the negative are convention matbld
C need to undo the Aij divided by distance convention used in matbld
C
C
C
C The second block consists of a list of ids of the connecting cells (id_up, id_dn), 
C coordinates of the face centroid between the two connected cells and 
C areas of the faces.
C
C CONNECTIONS <integer>   integer = # connections (M)
C id_up_1 id_dn_1 x_1 y_1 z_1 area_1
C id_up_2 id_dn_2 x_2 y_2 z_2 area_2
C ...
C ...
C id_up_M id_dn_M x_M y_M z_M area_M
C
C#######################################################################
      implicit none

C     args
      integer iunit,io_type, nnodes, ncoefs
      real*8 xp(nnodes), yp(nnodes), zp(nnodes), ij_area(ncoefs),amax
      integer ij_ptrs(nnodes+1), ij_list(ncoefs)

C     local variables

      pointer (ipifilter, ifilter)
      integer ifilter(*)

      integer*4 iunit4,lunget
      integer if_filter,ics,n,i,j,idx,jcol,jstart,jstop,
     *        nconn,ndiag,nzero,icount,icscode,ierr,ierrw,
     *        ifirst

      real*8 xm, ym, zm, aij, aij_min, aij_max,
     *       astor, xij,yij,zij,xijdist,
     *       eps_local, eps_filter

      character*132 logmess
      character*32 isubname


C#######################################################################
C BEGIN begin

      isubname = "dump_pflo_coefs"
      eps_local = 1e-12
      aij_min = eps_local
      aij_max = eps_local
      if_filter = 1
      if (io_type .gt. 10) if_filter = 0

c     iunit4=lunget('tty')
c     iunit = iunit4
    
      if (iunit.eq.6) then
         write(iunit,'(a,i5)')" Writing to screen: ",iunit
      endif

      if (io_type .eq. 12) then
        write(logmess,'(a,i10)')"PFLOTRAN will not filter zero coefs."
        call writloga('default',0,logmess,0,ics)
      else if (io_type .eq. 13) then
        write(logmess,'(a,i10)')"PFLOTRAN modified to write verbose."
        call writloga('default',0,logmess,0,ics)
      else
        io_type=10
        write(logmess,'(a,i10)')"PFLOTRAN will filter zero coefs."
        call writloga('default',0,logmess,0,ics)
      endif

      if (nnodes .le. 0) then
        write(logmess,'(a)')"PFLOTRAN Early Exit, 0 Nodes."
        call writloga('default',1,logmess,1,ics)
        goto 9999
      endif
      if (ncoefs .le. 0) then
        write(logmess,'(a)')"PFLOTRAN Early Exit, 0 Coefs"
        call writloga('default',1,logmess,1,ics)
        goto 9999
      endif

C     Get epsilon to filter zero coefs
C     currently, amax can be 0. from matbld
      eps_filter = eps_local
      if (abs(amax) .gt. eps_local) 
     *     eps_filter = abs(amax)*eps_local

      write(logmess,'(a,1pe15.7)')
     * "PFLOTRAN coefficient from matbld Aij/Xij  max  = ",amax
      call writloga('default',0,logmess,0,ics)
 
      if (io_type .eq. 10) then
        write(logmess,'(a,1pe15.7,1pe15.7)')
     * "PFLOTRAN filter = epsilon * abs(max Aij/Xij) :",
     *   eps_local,abs(amax)
        call writloga('default',0,logmess,0,ics)

        write(logmess,'(a,1pe15.7)')
     * "PFLOTRAN coefficients: epsilon for zero filter = ",eps_filter
        call writloga('default',0,logmess,0,ics)
      endif

      nconn = 0
      nzero = 0
      ndiag = 0

C      Create ifilter array to filter (or not filter) coefs near zero 
C      Count nconn number of unique ij connections
C      These values are Aij/Xij from matbld
       call mmgetblk("ifilter",isubname,ipifilter,ncoefs,1,ics)
       if (ics .ne. 0) then
          write(logmess,'(a,i5)')
     *    "pflotran coef matrix: mmgetblk failed: ifilter ",ics
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')
     *    "pflotran unable filter zero coefficients."
          call writloga('default',0,logmess,0,icscode)
          if_filter = 0
       endif

C     These loops are all because we need to know the number
C     of ij connections that will be written, before we write
C     them. I count the number here while filling filter array.
C     i,j are the connections and idx is the 1-ncoefs index into coef matrix 
C     These loops are the same as used to write the file lines for each ij

      nconn = 0
      do i = 1,nnodes
        jstart = ij_ptrs(i)-nnodes
        jstop = (ij_ptrs(i+1)-ij_ptrs(i))-1
        do jcol = 0, jstop
          idx = jstart+jcol
          j = ij_list(idx)
          astor = ij_area(idx)

c         count the connections we will write
c         NOTE: this logic MUST be same as below where written
          if (j.gt.i) then
            nconn = nconn + 1
c           print*,' -count ij ',nconn,i,j

c           tag and count zero Aij/Zij less than epsilon
c           1 means filter, 0 means not zero, -1 is unfiltered zero
            ifilter(idx) = 0
            if (abs(astor) .lt. eps_filter) then
              if (if_filter.gt.0) then
                 ifilter(idx)=1
c                print*," -filter    astor ",i,j,astor,idx
              else
                 print*," -no filter astor ",i,j,astor,idx
                 ifilter(idx)= -1
              endif
              nzero = nzero+1
            else
c             print*," -not zero astor     ",i,j,astor,idx
            endif

          endif
        enddo
      enddo 
      print*," -count nconn",nconn
      print*," -count nzero",nzero
      print*," "

C     CONNECTIONS and AREAS
C     I J  Xij Yij Zij Aij
C     I J are the edge nodes
C     Xij Yij Zij Aij is the midpoint
C     Aij is the fehm astor mult by dist and mult by -1 

C     HEADING LINE
      if (io_type .eq. 13) then
        write(iunit,'(a,1x,i10,a)')"CONNECTIONS ",
     *  nconn,"  I J  Xm Ym Zm   Aij  Aij/dist "
      else if (io_type .eq. 12) then
        write(iunit,'(a,1x,i10)')"CONNECTIONS ",nconn
      else
        write(iunit,'(a,1x,i10)')"CONNECTIONS ",nconn-nzero
      endif

      icount = 0
      ifirst = 1

C     LOOP through the ccoef matrix rows and columns
      do i = 1,nnodes
        jstart = ij_ptrs(i)-nnodes
        jstop = (ij_ptrs(i+1)-ij_ptrs(i))-1

C       LOOP through each I,J pair and write values
        do jcol = 0, jstop
          idx = jstart+jcol
          j = ij_list(idx)
          astor = ij_area(idx)
          if (j.eq.i) ndiag=ndiag+1
          
C         WRITE the edge pairs once so j>i 
          if (j.gt.i) then

C          check if this coef is filtered
C          this includes 0 for non zero and -1 for unfiltered zero
           if (ifilter(idx).ne.1) then

C             COMPUTE midpoint of edge
              xm=(xp(j)+xp(i))*0.5
              ym=(yp(j)+yp(i))*0.5
              zm=(zp(j)+zp(i))*0.5

C             COMPUTE to area instead of area/dist
C             do not mult by -1 to undo the negative
C             2D does not pass negative, 3D does, so undo in matbld
C             area matrix in stor routines creates  Aij/xij from Aij
              xij = xp(j) - xp(i)
              yij = yp(j) - yp(i)
              zij = zp(j) - zp(i)
              xijdist = sqrt(xij**2 + yij**2 + zij**2)

C             this is where we undo fehm conventions
C             we assume astor has not been made negative
              aij = astor * xijdist
c             print*," -pflotran aij, astor ",aij,astor

              if (ifirst.eq.1) then
                  aij_min = aij
                  aij_max = aij
                  ifirst = 0
              else
                  aij_min = min(aij_min,aij)
                  aij_max = max(aij_max,aij)
              endif

C             WRITE the final values
C           default io_type is 10 and filters zero coefs lt eps 
C           io_type 12 writes zero areas
C           io_type 13 writes additional info 

            if (io_type.eq.10 ) then
              write(iunit,9001) i,j,xm,ym,zm,aij
              icount = icount + 1

            elseif (io_type .eq. 12 ) then
              write(iunit,9001) i,j,xm,ym,zm,aij
              icount = icount + 1

            elseif (io_type .eq. 13 ) then
              write(iunit,9003)i,j,xm,ym,zm,aij,astor
              icount = icount + 1

            endif
c           print*," -pflotran written:  ",i,j,astor,aij
        
          else
c           print*," -pflotran filtered: ",i,j,astor

C         end check for filtered output
          endif

c       end i < j skip
        else
c           print*," -pflotran skip i<=j: ",i,j
        endif
       
        enddo
      enddo
C     LOOP end for writing valid coefs 

       call mmrelblk("ifilter",isubname,ipifilter,ics)
       if(ics.ne.0) call x3d_error(isubname,"mmrelblk fail ifilter")

       write(logmess,'(a45,i10)')
     * "PFLOTRAN total matbld matrix coefficients   = ",ncoefs
       call writloga('default',0,logmess,0,ics)

       write(logmess,'(a45,i10)')
     * "PFLOTRAN matrix  i>j (written)              = ",nconn
       call writloga('default',0,logmess,0,ics)

       write(logmess,'(a45,i10)')
     *  "PFLOTRAN matrix  i<=j (not written)        = ",ncoefs-nconn
       call writloga('default',0,logmess,0,ics)

c      stats about zero coefs
       if (if_filter .gt. 0) then
         write(logmess,'(a45,i10)')
     * "PFLOTRAN zero coefs < epsilon (not written) = ",nzero
         call writloga('default',0,logmess,0,ics)
       else
         write(logmess,'(a45,i10)')
     * "PFLOTRAN zero coefs < epsilon    (written)  = ",nzero
         call writloga('default',0,logmess,0,ics)
       endif

c      total and final number of coefs written
       write(logmess,'(a45,i10)')
     * "PFLOTRAN total CONNECTIONS written         = ",icount
       call writloga('default',1,logmess,0,ics)

       if (io_type.eq.10 .and. icount.ne.nconn-nzero) then
         write(logmess,'(a)')
     * "PFLOTRAN Warning: ij count - nzero differs from num written."
         call writloga('default',1,logmess,0,ics)

       else if (icount .ne. nconn) then
         write(logmess,'(a)')
     * "PFLOTRAN Warning: ij count differs from num written."
         call writloga('default',1,logmess,0,ics)
       endif

       write(logmess,'(a45,1pe15.7)')
     * "PFLOTRAN coefficient  Aij min              = ",aij_min
       call writloga('default',0,logmess,0,ics)
       write(logmess,'(a45,1pe15.7)')
     * "PFLOTRAN coefficient  Aij max              = ",aij_max
       call writloga('default',0,logmess,0,ics)



 9001 format(i10,1x,i10,4(1x,1pe20.12))
 9003 format(i10,1x,i10,5(1x,1pe20.12))


 9999 return

      call mmrelprt(isubname,ics)

      end


