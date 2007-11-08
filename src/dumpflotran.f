      subroutine dumpflotran(ifile)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  PURPOSE
c     Dump a flotran file.
c
c  INPUT ARGUMENTS - 
c     ifile - Base file name (.flow is appended)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cPVCS
C$Log:   /pvcs.config/t3d/src/dumpflotran.f_a  $
CPVCS    
CPVCS       Rev 1.1   Tue Jun 08 10:43:06 1999   murphy
CPVCS    Converted to left-handed coordinate system.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                       The Flotran file format.
c
c Flotran, a cell-centered approach, expects a quadrilateral or 
c hexahedral mesh in the following format: 
c
c Comment lines:  Any comment line starts with colon (:).   
c
c First line: 
c  "GRID" followed by:  nx ny nz and the absolute orgin of the coordinates.  
c  where nx, ny, nz indicate the number of cells in the x,y,z directions.
c  If we are working with a quadralateral mesh, then nd=1
c  where d is the dimension we are ignoring.  The format is "left-handed"
c  with increasing z going down.
c
c
c The second line should read "phik" to signify the 
c the Material block follows.  
c   The material block specifies the material type of each cell. 
c   The format is we write it in is:
c      i i  j j  k k   mat# mat# mat# mat# 
c   for each cell.  
c
c   The i,j,k index each control volume from 1 to nx, 1 to ny, and 1 to nyz,
c   respectively.    The material block is terminated with a slash (/).  
c
c The line after the material block should contain the character string ":dxyz"
c to signify the start of the the Delta block.   The Delta block is  a set 
c of delta x's followed by delta y's followed by  and delta z's (10 per line)
c signifying the length of each control volume in each direction.    Again,
c if it is a quadralateral mesh, there is 1.0 in the ignored dimension.  
c
c Peter Lichtner gives an example of how the actual input file looks 
c (which differs from our first approximation because he wants information
c  not available from an avs file.)
c
c:      geometry  nx  ny  nz ivplwr ipvcal iout gravity pref tref  href
cGrid   XYZ        1   1 121    1      0      3    0      0    0    0
c:
cPhiK
c: i1 i2  j1  j2  k1  k2  iist ithrm  vb porm  permx   permy    permz
c   1  1  1   1   1  121   1     1    0.
c/
c:     igrid   rw      re
cDXYZ   0      0.      1.
c: (dx(i),i=1,nx)
c   1.
c:
c: (dy(j),j=1,ny)
c    1.
c:
c: (dz(k),k=1,nz)
c10.  10.  10.  10.  10.  10.  10.  10.  10.  10.
c10.  10.  10.  10.  10.  10.  10.  10.  10.  10.
c10.  10.  10.  10.   5.   5.   5.   5.   5.   5.
c 5.   5.   5.   5.   5.   5.   5.   5.   5.   5.
c 2.   2.   2.   2.   2.   2.   2.   2.   2.   2.
c 2.   2.   2.   2.   2.   2.   2.   2.   2.   2.
c 2.   2.   2.   1.5  1.   1.   1.   1.   1.   1.
c 1.   1.   1.   1.   1.   1.   1.   1.   1.   1.5
c 2.   2.   2.   2.   2.   2.   2.   2.   2.   2.
c 2.   2.   2.   2.   2.   2.   2.   2.   5.   5.
c 5.   5.   5.   5.   5.   5.   5.   5.  10.  10.
c10.  10.  10.  10.  10.  10.  10.  10.  10.  10.
c10.

      implicit none
      include "local_element.h"

      integer lenptr
      parameter (lenptr=1000000)

      integer iunit, nextlun

cccccccccc
c     Args
cccccccccc
      character*(*) ifile
      integer ierror
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)

      pointer (ipi_bin, i_bin)
      pointer (ipj_bin, j_bin)
      pointer (ipk_bin, k_bin)

      pointer (ipi_index, i_index)
      pointer (ipj_index, j_index)
      pointer (ipk_index, k_index)

      pointer (ipdxyz, dxyz)


c 
C     Element-Based Attributes
c
      pointer (ipitet, itet)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
C
      integer   itetclr(lenptr), itettyp(lenptr)
      integer   itet(4*lenptr), itetoff(lenptr)

      real*8 xic(10000000), yic(10000000), zic(10000000)

      real*8 dxyz(10000000)

      integer i_bin(10000000),j_bin(10000000),k_bin(10000000)
      integer i_index(10000000),j_index(10000000),k_index(10000000)
      integer ilen, itype, ier
      integer nnodes, ntets
      integer icharlnf
      integer i,j,k, nx,ny,nz
      integer ii,jj,kk
      integer jtype

      character*24 string, fdate 
      character*32 isubname, cmonam, cmonam2, ifilename 
      character*80 logmess 
      character*132 extrudestring

      isubname='dumpflotran'
c
      call cmo_get_name(cmonam,ier)
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,ntets,ilen,itype,ierror)
      call cmo_get_info('xic',cmonam,ipxic,ilen,itype,ier)
      call cmo_get_info('yic',cmonam,ipyic,ilen,itype,ier)
      call cmo_get_info('zic',cmonam,ipzic,ilen,itype,ier)

      call cmo_get_info('itetclr',cmonam,ipitetclr,ilen,itype,ierror)
      call cmo_get_info('itettyp',cmonam,ipitettyp,ilen,itype,ierror)
      call cmo_get_info('itet',cmonam,ipitet,ilen,itype,ierror)
      call cmo_get_info('itetoff',cmonam,ipitetoff,ilen,itype,ierror)

c
c Check itettyp to make sure we are dealing with all quads or all hexes.  
c

      jtype = itettyp(1)
      if ((jtype.ne.ifelmqud).and.(jtype.ne.ifelmhex)) then
         write(logmess,'(a)')
     &        'Error in dumpflotran: mesh is not a quad or hex'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif

      do i=2,ntets
         if (itettyp(i).ne.jtype)then
c     Write an error message.
            write(logmess,'(a)')
     &           'Error in dumpflotran: mesh is not all hex or all quad'
            call writloga('default',0,logmess,0,ierror)
            goto 9999
         endif
      enddo

c
c We want to sort the nodes two ways, by bins and by index.  
c We do the bins first.   
      call dotaskx3d('sort/xyz/bins ; finish',ierror)
c
c Get the i_bin, j_bin, k_bin arrays,  the results of sort/xyz/bins.
c To do so, we play the following memory management shuffle game.
c
      call cmo_get_info('i_index',cmonam,ipi_index,ilen,itype,ier)
      call cmo_get_info('j_index',cmonam,ipj_index,ilen,itype,ier)
      call cmo_get_info('k_index',cmonam,ipk_index,ilen,itype,ier)

c
c Allocate space for i_bin, j_bin, k_bin
c
      ilen = 1
      call mmgetblk('i_bin',isubname,ipi_bin,nnodes,ilen,ier)
      call mmgetblk('j_bin',isubname,ipj_bin,nnodes,ilen,ier)
      call mmgetblk('k_bin',isubname,ipk_bin,nnodes,ilen,ier)
      call cmo_get_info('i_bin',cmonam,ipi_bin,ilen,itype,ier)
      call cmo_get_info('j_bin',cmonam,ipj_bin,ilen,itype,ier)
      call cmo_get_info('k_bin',cmonam,ipk_bin,ilen,itype,ier)

c
c Set them equal to i_index, j_index, k_index
c      

      do i=1,nnodes
         i_bin(i) = i_index(i)
         j_bin(i) = j_index(i)
         k_bin(i) = k_index(i)
      enddo

c
c Now, get the real i_index, j_index, k_index
c

      call dotaskx3d('sort/xyz/index ; finish',ierror)
      call cmo_get_info('i_index',cmonam,ipi_index,ilen,itype,ier)
      call cmo_get_info('j_index',cmonam,ipj_index,ilen,itype,ier)
      call cmo_get_info('k_index',cmonam,ipk_index,ilen,itype,ier)



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Output the conversion into a .flow file.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



c Make a string containing the file name. 
      ifilename=ifile(1:icharlnf(ifile)) // '.flow'

c
c Get next available unit number.
c     Use nextlun(), as nextlun1() can return a negative integer
c

      iunit = nextlun()  
      string = fdate()
c     Open the file.
      open(unit=iunit, file = ifilename, form = 'formatted')
      write(iunit,*)':',string,'LaGriT Generated (dumpflowtran.f)'      
      nx=0
      ny=0
      nz=0
      do i=1,nnodes
         if (nx.le.i_bin(i)) then
            nx=i_bin(i)
         endif
         if (ny.le.j_bin(i)) then
            ny=j_bin(i)
         endif
         if (nz.le.k_bin(i)) then
            nz=k_bin(i)
         endif
      enddo
      
c
c Decrement by one because we are cell-based not node-based.
c
      nx = nx - 1
      ny = ny - 1 
      nz = nz - 1


      print *,nx,ny,nz,'which one is 0?'

      write(iunit,*)
     x ':Put nts coordinates coordinates here (They are now  0,0,0)'

      write(iunit,*)
     x ':geometry  nx  ny  nz ivplwr ipvcal iout gravity pref tref href'

      write(iunit,*)'GRID ',nx,ny,nz,0,0,0
c
c Write out phik and the material information.
c

cPhiK
c: i1 i2  j1  j2  k1  k2  iist ithrm  vb porm  permx   permy    permz
c   1  1  1   1   1  121   1     1    0.

      write(iunit,*)'PhiK'
      write(iunit,*)
     *' i1',' i2 ',' j1 ','j2 ','k1 ','k2 ','iist ','ithrm ','vb ',
     *'porm ','permx ','permy ','permz '

      do i=1,ntets
         ii=nnodes+1
         jj=nnodes+1
         kk=0

         do j=1,nelmnen(itettyp(i)) 
            k=itet(itetoff(i)+j) 
            if (ii.gt.i_bin(k)) then
               ii = i_bin(k)
            endif

            if (jj.gt.j_bin(k)) then
               jj = j_bin(k)
            endif

            if (kk.lt.k_bin(k)) then
               kk = k_bin(k)
            endif
         enddo 
         jtype = itetclr(i)
         kk = nz + 2 - kk
         write(iunit,*)ii,ii,jj,jj,kk,kk,jtype,jtype,jtype,jtype
      enddo

      call dotaskx3d('sort/xyz/index ; finish',ierror)

c
c  Allocate memory for the results.
c
      ilen=2
      call mmgetblk('dxyz',isubname,ipdxyz,nnodes,ilen,ier)
      call cmo_get_info('dxyz',cmonam,ipdxyz,ilen,itype,ier)


c
c Write out dxyz and the delta information.
c

      write(iunit,*)':    igrid   rw      re'
      write(iunit,*)'DXYZ   0      0.      1.'

      write(iunit,*)': (dx(i),i=1,nx)'
      if (nx.ge.1) then
         j=1
         do i=2,nnodes
            if (i_bin(i_index(i-1)).ne.i_bin(i_index(i))) then
               dxyz(j) = xic(i_index(i))-xic(i_index(i-1))
               j = j+1
            endif
         enddo
      else
         nx=1
         dxyz(1) = 1.0
      endif
      write (iunit,9000) (dxyz(i),i=1,nx)

      write(iunit,*)': (dy(i),i=1,ny)'
      if (ny.ge.1)  then
         j=1
         do i=2,nnodes
            if (j_bin(j_index(i-1)).ne.j_bin(j_index(i))) then
               dxyz(j) = yic(j_index(i))-yic(j_index(i-1))
               j = j+1
            endif
         enddo
      else
         ny=1
         dxyz(1) = 1.0
      endif
      write (iunit,9000)(dxyz(i),i=1,ny)


      write(iunit,*)': (dz(i),i=1,nz)'
      if (nz.ge.1) then
         j=1
         do i=nnodes,2,-1
            if (k_bin(k_index(i-1)).ne.k_bin(k_index(i))) then
               dxyz(j) = zic(k_index(i))-zic(k_index(i-1))
               j = j+1
            endif
         enddo
      else
         nz=1
         dxyz(1) = 1.0
      endif
      write (iunit,9000)(dxyz(i),i=1,nz)


 9000 format(10(1pe20.12))
      close(iunit)

 9999 return
      end
