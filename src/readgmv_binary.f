      subroutine readgmv_binary(ifile,ierror)
c
c #####################################################################
c
c     purpose -
c
c        create a gmv formatted file from the current mesh object
c
c     input arguments -
c
c        none
c
c     output arguments -
c
c        none
c
c     change history -
c
C        $Log: readgmv_binary.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   12 Jun 2003 11:53:06   tam
CPVCS    initialized ctype and cmoattnam to 32 blanks for opt SGI
CPVCS    
CPVCS       Rev 1.8   24 Mar 2003 14:38:00   gable
CPVCS    Recognize AMR mesh variables itetpar, itetkid, itetlev as VINT.
CPVCS    
CPVCS       Rev 1.7   18 Jul 2001 15:44:50   dcg
CPVCS    get rid of all lines having to do with random io calls
CPVCS    i.e call cposition, iadr calculation
CPVCS
CPVCS       Rev 1.5   29 Nov 2000 10:08:56   dcg
CPVCS    check if geometry exists
CPVCS
CPVCS       Rev 1.4   Wed Apr 12 15:06:16 2000   dcg
CPVCS    change nulls to blanks after cread ( nulls come from c io)
CPVCS
CPVCS       Rev 1.3   Wed Apr 05 13:34:56 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.2   Wed Feb 02 13:43:08 2000   dcg
CPVCS
CPVCS       Rev 1.1   Mon Jan 31 13:31:08 2000   dcg
CPVCS
CPVCS       Rev 1.0   Mon Jan 31 13:25:36 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.16   Tue Nov 30 17:08:12 1999   jtg
CPVCS    slightly cleaned up, nothing major
CPVCS
CPVCS       Rev 1.15   Thu Aug 05 14:03:34 1999   dcg
CPVCS    allow line type mesh object elements
CPVCS
CPVCS       Rev 1.14   Tue Jul 06 19:29:40 1999   jtg
CPVCS    modifed call to geniee (put at end)
CPVCS
CPVCS       Rev 1.13   Fri Jan 22 15:35:16 1999   dcg
CPVCS    change itetclr(ielements)=imat to itetclr(ielements)=1 (imat was not defined)
CPVCS
CPVCS       Rev 1.12   Fri Oct 31 10:49:46 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.11   Thu May 01 14:14:20 1997   dcg
CPVCS    read polygon data correctly (4 bytes at a time)
CPVCS
CPVCS       Rev 1.10   Mon Apr 14 16:57:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.9   Sun Feb 23 10:33:24 1997   het
CPVCS    Change to read binary AMR grids.
CPVCS
CPVCS       Rev 1.8   Mon Dec 02 08:54:18 1996   het
CPVCS    Read element size variable fields.
CPVCS
CPVCS       Rev 1.7   Wed Nov 13 13:48:48 1996   het
CPVCS    read connectivity for quad and tri GMV cells.
c
c ######################################################################
c
      implicit none
C
C args
      character*(*) ifile
      integer ierror
C
      integer charlength
      parameter (charlength=32)
C
      include "local_element.h"
      include 'geom_lg.h'
C
      pointer (ipimt1, imt1)
      integer imt1(*)
C
      pointer (ipitetclr, itetclr(*))
      pointer (ipitettyp, itettyp(*))
      pointer (ipitetoff, itetoff(*))
      pointer (ipjtetoff, jtetoff(*))
      pointer (ipitet, itet1(*))
      pointer (ipjtet, jtet1(*))
      integer itetclr,itettyp,itetoff,jtetoff,itet1,jtet1

C

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)

      pointer (ipout,out)
      real*8 out(*)
C
      real*4 xtime4
      pointer (ipxtemp4, xtemp4)
      real*4 xtemp4(*)
      pointer (ipitemp4, itemp4)
      integer*4 itemp4(*)

      integer*4 liste(100)

      integer*4 iunit4
      integer*4 nelements4,nnodes4,iflag4,maxclrpoint4,ihcycle4
C
      pointer (ipiarray, iarray)
      integer iarray(*)
      pointer (ipxarray, xarray)
      real*8 xarray(*)

      real*8 a,b,c,d,e,f,crosx,crosy,crosz,time,rout
      pointer (ipvels,vels)
      real*8 vels(3,*)

C     for defining sizes in gmv binary files
      integer  IEEE, ASCII, IEEEI4R4, IEEEI4R8, IEEEI8R4,
     * IEEEI8R8, IECXI4R4, IECXI4R8, IECXI8R4, IECXI8R8

      integer ihcycle, ivoronoi2d,ivoronoi3d,ipolydata,
     *  iflag_all,it,ird,lenc,index,lenout,icharlnb,
     *  ics,iout,iflag,itype,ifelmnew,nee,nsdtopo,nsdgeom,
     *  i,i1,i2,i3,i4,i5,i6,i7,i8,icount,ifound,
     *  itoff,jtoff,nelements,ftype,
     *  length,nnodes,iadr,ibytes,icharlnf,len1,maxclrpoint,
     *  iunit,ihybrid,ilen,ityp,ierr,ielements,
     *  j,leni,itp,lout,ierror_return,icscode,ierrwrt,
     *  ier
      integer nsd, nen, nef

      integer nbyte_total, idebug

      logical isgeom
c
      character*32 isubname,cmo,cmoattnam,ctype,cvelnm,
     *         clen,cinter,cpers,cio,crank
      character*32  cmotype,cout,geom_name,c32blank
      character*132 iline, iword1, iword2
      character*132 file, Fifile
      character*132 logmess
      character*8092 cbuff
c
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data ipolydata / 1 /
      data iflag_all / 0 /

      data IEEE / 0 /
      data ASCII / 1 /
      data IEEEI4R4 / 0 /
      data IEEEI4R8 / 2 /
      data IEEEI8R4 / 3 /
      data IEEEI8R8 / 4 /
      data IECXI4R4 / 5 /
      data IECXI4R8 / 6 /
      data IECXI8R4 / 7 /
      data IECXI8R8 / 8 /

c
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
c
C###############################################################
C begin

      isubname="readgmv_binary"
c     initialize strings going into cread
      c32blank="                                "
      cmoattnam=c32blank
      ctype=c32blank
      nbyte_total = 0
      idebug = 0
C
      len1=icharlnf(ifile)
      Fifile=ifile(1:len1) // 'F'
      file=ifile(1:len1) // char(0)
      iunit=-1
      call hassign(iunit,Fifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        write(logmess,*) 'WARNING: can not get file no: ' 
     *   // Fifile  
        call writloga('default',0,logmess,0,ierror)
        goto 9998 
      endif
      iunit4=iunit
      call cassignr(iunit4,file,ierror)
C
C tam
C     Removed calls to termcode since we really
C     do not want to stop the program
C     we just want to exit the routine trying to read gmv
C     also moved error reporting from cread to end with rest
C     instead of using a goto statement back up to 98 here. 

C     Look for first header word 'gmvinput' 
      iadr=0
      ibytes=8

      nbyte_total = nbyte_total + ibytes
      call cread(iunit4,iword1,ibytes,ierror)
      if (ierror .ne.0) go to 9998

      if(iword1(1:8).eq.'gmvinput') then

C       Check second header word for gmv format tag
C       These tags are found in various gmv formats  
C       and can be used to recognize and parse new gmv formats
C       as described in sample I/O files found on GMV web site
C       Original lagrit type is IEEE / 0 / same as IEEEI4R4 / 0 /

        ibytes=8
        nbyte_total = nbyte_total + ibytes
        call cread(iunit,iword2,ibytes,ierror)
        if(ierror.ne.0) go to 9998 

        ftype = -1
        if (iword2(1:5).eq. 'ascii') ftype=ASCII
        if (iword2(1:6).eq. ' ascii') ftype=ASCII
        if (iword2(1:7).eq. '  ascii') ftype=ASCII
        if (iword2(1:8).eq. '   ascii') ftype=ASCII
        if (iword2(1:4).eq. 'ieee') ftype=IEEEI4R4
        if (iword2(1:5).eq.' ieee') ftype=IEEEI4R4
        if (iword2(1:8).eq.'ieeei4r4') ftype=IEEEI4R4
        if (iword2(1:9).eq.' ieeei4r4') ftype=IEEEI4R4
        if (iword2(1:8).eq.'ieeei4r8') ftype=IEEEI4R8
        if (iword2(1:9).eq.' ieeei4r8') ftype=IEEEI4R8
        if (iword2(1:8).eq.'ieeei8r4') ftype=IEEEI8R4
        if (iword2(1:9).eq.' ieeei8r4') ftype=IEEEI8R4
        if (iword2(1:8).eq.'ieeei8r8') ftype=IEEEI8R8
        if (iword2(1:9).eq.' ieeei8r8') ftype=IEEEI8R8
        if (iword2(1:8).eq.'iecxi4r4') ftype=IECXI4R4
        if (iword2(1:9).eq.' iecxi4r4') ftype=IECXI4R4
        if (iword2(1:8).eq.'iecxi4r8') ftype=IECXI4R8
        if (iword2(1:9).eq.' iecxi4r8') ftype=IECXI4R8
        if (iword2(1:8).eq.'iecxi8r4') ftype=IECXI8R4
        if (iword2(1:9).eq.' iecxi8r4') ftype=IECXI8R4
        if (iword2(1:8).eq.'iecxi8r8') ftype=IECXI8R8
        if (iword2(1:9).eq.' iecxi8r8') ftype=IECXI8R8

C       FORMAT TYPE NOT FOUND
        if (ftype .eq. -1) then
          write(logmess,'(a,a)')
     *    "WARNING: Invalid GMV file type: ", iword2(1:8) 
          call writloga('default',0,logmess,0,ierror)
          write(logmess,'(a,a)')"Must be of file type: ",    
     *    " ascii, ieee, or iecx " 
          call writloga('default',0,logmess,0,ierror)

          call cclose(iunit4)
          goto 9999

C       FORMAT TYPE NOT SUPPORTED
        elseif (ftype .gt. IEEEI4R4 ) then
          write(logmess,'(a,a)')"Unsupported GMV file type: ",    
     *    iword2(1:8) 
          call writloga('default',0,logmess,0,ierror)
          call cclose(iunit4)
          goto 9999

C       FORMAT TYPE RECOGNIZED 
        else
          write(logmess,'(a,a,a)') "Reading GMV binary file "
     >    ,iword1(1:8),iword2(1:8) 
          call writloga('default',0,logmess,0,ierror)
        endif

C     IF word 1 is not gmvinput
      else
         write(logmess,'(a)')
     *   "Not a GMV file, gmvinput token not found."
         call writloga('default',0,logmess,0,ierror)
         call cclose(iunit4)
         goto 9999
      endif

C
C     done checking header for tag gmvinput and format ieee 
C     continue reading file

      ihybrid=0
C
 100  continue
      ibytes=8
      nbyte_total = nbyte_total + ibytes
      call cread(iunit4,iword1,ibytes,ierror)
      if(ierror.ne.0) go to 9998 

      if(iword1(1:6).eq.'endgmv') goto 9999
C
      if(iword1(1:5).eq.'nodes') then
C
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,nnodes4,ibytes,ierror)
         if(ierror.ne.0) go to 9998 
         nnodes = nnodes4
C
         call cmo_get_name(cmo,icscode)
         call cmo_set_info('nnodes',cmo,nnodes,1,1,icscode)
C
         if(nnodes.gt.0) then
C
            call cmo_newlen(cmo,icscode)
C
            call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
            call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
            call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
C
            length=nnodes
            call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,icscode)
C
            ibytes=4*nnodes
            nbyte_total = nbyte_total + ibytes
            call cread(iunit4,xtemp4,ibytes,ierror)
            if(ierror.ne.0) go to 9998 

            do i=1,nnodes
               xic(i)=xtemp4(i)
            enddo
            ibytes=4*nnodes
            nbyte_total = nbyte_total + ibytes
            call cread(iunit4,xtemp4,ibytes,ierror)
            if (ierror.ne.0 ) go to 9998
            do i=1,nnodes
               yic(i)=xtemp4(i)
            enddo
            ibytes=4*nnodes
            nbyte_total = nbyte_total + ibytes 
            call cread(iunit4,xtemp4,ibytes,ierror)
            if (ierror.ne.0 ) go to 9998
            do i=1,nnodes
               zic(i)=xtemp4(i)
            enddo
C
            call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
C
         endif
         goto 100
      elseif(iword1(1:5).eq.'cells') then
C
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,nelements4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         nelements=nelements4
C
         call cmo_get_name(cmo,icscode)
         call cmo_set_info('nnodes',cmo,nnodes,1,1,icscode)
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
C
         if(nelements.gt.0) then
            itoff=0
            jtoff=0
            do ielements=1,nelements
               ibytes=8
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,ctype,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
            call nulltoblank_lg(ctype,charlength)
               ifound=0
               if(ctype(1:3).eq.'lin') then
                  ibytes=4*(1+nelmnen(ifelmlin))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  do i=2,3
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,4i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,3)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
               elseif(ctype(1:3).eq.'tri') then
                  ibytes=4*(1+nelmnen(ifelmtri))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  i3=liste(4)
                  do i=2,4
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,4i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,4)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
               elseif(ctype(1:4).eq.'quad') then
                  ibytes=4*(1+nelmnen(ifelmqud))
                  nbyte_total = nbyte_total + ibytes 
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  i3=liste(4)
                  i4=liste(5)
                  do i=2,5
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,4i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,5)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
               elseif(ctype(1:3).eq.'tet') then
                  ibytes=4*(1+nelmnen(ifelmtet))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  i3=liste(4)
                  i4=liste(5)
                  do i=2,5
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,4i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,5)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
                  if(i1.eq.i4) ctype='tri'
               elseif(ctype(1:3).eq.'pyr') then
                  ibytes=4*(1+nelmnen(ifelmpyr))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i5=liste(2)
                  i1=liste(3)
                  i4=liste(4)
                  i3=liste(5)
                  i2=liste(6)
                  do i=2,6
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,9i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,6)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
               elseif(ctype(1:3).eq.'pri') then
                  ibytes=4*(1+nelmnen(ifelmpri))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  i3=liste(4)
                  i4=liste(5)
                  i5=liste(6)
                  i6=liste(7)
                  do i=2,7
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,9i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,7)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
               elseif(ctype(1:3).eq.'hex') then
                  ibytes=4*(1+nelmnen(ifelmhex))
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,liste,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  icount=liste(1)
                  i1=liste(2)
                  i2=liste(3)
                  i3=liste(4)
                  i4=liste(5)
                  i5=liste(6)
                  i6=liste(7)
                  i7=liste(8)
                  i8=liste(9)
                  do i=2,9
                     if(liste(i).lt.1.or.liste(i).gt.nnodes) then
                        write(logmess,'(a,9i10)') 'Illegal NN: ',
     *                                            ielements,
     *                                            (liste(j),j=2,9)
                        call writloga('default',0,logmess,0,ierror)
                     endif
                  enddo
                  if(i1.eq.i5.and.i2.eq.i6 .and.
     *                  i3.eq.i7.and.i4.eq.i8) ctype='quad'
               else
                  write(logmess,'(a,a)')
     *               'Unsupported GMV element type: ',ctype
                  call writloga('default',0,logmess,0,ierrwrt)
               endif
               if(ifound.eq.0.and.ielements.eq.1) then
                  if(ctype(1:3).eq.'lin') then
                        cmotype='lin'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=2
                        nen=nelmnen(ifelmlin)
                        nef=nelmnef(ifelmlin)
                        nee=nelmnee(ifelmlin)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:3).eq.'tri') then
                        cmotype='tri'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=2
                        nen=nelmnen(ifelmtri)
                        nef=nelmnef(ifelmtri)
                        nee=nelmnee(ifelmtri)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:4).eq.'quad') then
                        cmotype='quad'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=2
                        nen=nelmnen(ifelmqud)
                        nef=nelmnef(ifelmqud)
                        nee=nelmnee(ifelmqud)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:3).eq.'tet') then
                        cmotype='tet'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmtet)
                        nef=nelmnef(ifelmtet)
                        nee=nelmnee(ifelmtet)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:3).eq.'pyr') then
                        cmotype='pyr'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmpyr)
                        nef=nelmnef(ifelmpyr)
                        nee=nelmnee(ifelmpyr)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:3).eq.'pri') then
                        cmotype='pri'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmpri)
                        nef=nelmnef(ifelmpri)
                        nee=nelmnee(ifelmpri)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  elseif(ctype(1:3).eq.'hex') then
                        cmotype='hex'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmhex)
                        nef=nelmnef(ifelmhex)
                        nee=nelmnee(ifelmhex)
                        call cmo_set_info('nnodes',cmo,
     *                                    nnodes,1,1,ierror)
                        call cmo_set_info('nelements',cmo,
     *                                    nelements,1,1,ierror)
                        call cmo_set_info('ndimensions_geom',cmo,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmo,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmo,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmo,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmo,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmo,ierror)
                        call cmo_get_info('itetclr',cmo,
     *                                    ipitetclr,leni,itp,ier)
                        call cmo_get_info('itettyp',cmo,
     *                                    ipitettyp,leni,itp,ier)
                        call cmo_get_info('itetoff',cmo,
     *                                    ipitetoff,leni,itp,ier)
                        call cmo_get_info('jtetoff',cmo,
     *                                    ipjtetoff,leni,itp,ier)
                        call cmo_get_info('itet',cmo,
     *                                    ipitet,leni,itp,ier)
                        call cmo_get_info('jtet',cmo,
     *                                    ipjtet,leni,itp,ier)
                  endif
               endif
               if(ctype(1:3).eq.'tri') then
                  ifelmnew=ifelmtri
               elseif(ctype(1:4).eq.'line') then
                  ifelmnew=ifelmlin
               elseif(ctype(1:4).eq.'quad') then
                  ifelmnew=ifelmqud
               elseif(ctype(1:3).eq.'tet') then
                  ifelmnew=ifelmtet
               elseif(ctype(1:3).eq.'pyr') then
                  ifelmnew=ifelmpyr
               elseif(ctype(1:3).eq.'pri') then
                  ifelmnew=ifelmpri
               elseif(ctype(1:3).eq.'hex') then
                  ifelmnew=ifelmhex
               endif
               if(ihybrid.eq.0 .and.
     *            (nelmnen(ifelmnew).ne.nen .or.
     *             nelmnef(ifelmnew).ne.nef)) then
                  ihybrid=1
                  nen=nelmnen(ifelmhyb)
                  nef=nelmnef(ifelmhyb)
                  nee=nelmnee(ifelmhyb)
                  call cmo_set_info('nodes_per_element',cmo,
     *                              nen,1,1,ierror)
                  call cmo_set_info('faces_per_element',cmo,
     *                              nef,1,1,ierror)
                  call cmo_set_info('edges_per_element',cmo,
     *                              nee,1,1,ierror)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_info('itetclr',cmo,
     *                              ipitetclr,leni,itp,ier)
                  call cmo_get_info('itettyp',cmo,
     *                              ipitettyp,leni,itp,ier)
                  call cmo_get_info('itetoff',cmo,
     *                              ipitetoff,leni,itp,ier)
                  call cmo_get_info('jtetoff',cmo,
     *                              ipjtetoff,leni,itp,ier)
                  call cmo_get_info('itet',cmo,
     *                              ipitet,leni,itp,ier)
                  call cmo_get_info('jtet',cmo,
     *                              ipjtet,leni,itp,ier)
               endif
               if(ctype(1:3).eq.'lin') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmlin
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  itoff=itoff+nelmnen(ifelmlin)
                  jtoff=jtoff+nelmnef(ifelmlin)
               elseif(ctype(1:3).eq.'tri') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmtri
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  itoff=itoff+nelmnen(ifelmtri)
                  jtoff=jtoff+nelmnef(ifelmtri)
               elseif(ctype(1:4).eq.'quad') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmqud
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  jtet1(jtoff+4)=0
                  itoff=itoff+nelmnen(ifelmqud)
                  jtoff=jtoff+nelmnef(ifelmqud)
               elseif(ctype(1:3).eq.'tet') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmtet
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  jtet1(jtoff+4)=0
                  itoff=itoff+nelmnen(ifelmtet)
                  jtoff=jtoff+nelmnef(ifelmtet)
               elseif(ctype(1:3).eq.'pyr') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmpyr
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  itet1(itoff+5)=i5
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  jtet1(jtoff+4)=0
                  jtet1(jtoff+5)=0
                  itoff=itoff+nelmnen(ifelmpyr)
                  jtoff=jtoff+nelmnef(ifelmpyr)
               elseif(ctype(1:3).eq.'pri') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmpri
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  itet1(itoff+5)=i5
                  itet1(itoff+6)=i6
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  jtet1(jtoff+4)=0
                  jtet1(jtoff+5)=0
                  itoff=itoff+nelmnen(ifelmpri)
                  jtoff=jtoff+nelmnef(ifelmpri)
               elseif(ctype(1:3).eq.'hex') then
                  itetclr(ielements)=1
                  itettyp(ielements)=ifelmhex
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  itet1(itoff+5)=i5
                  itet1(itoff+6)=i6
                  itet1(itoff+7)=i7
                  itet1(itoff+8)=i8
                  jtet1(jtoff+1)=0
                  jtet1(jtoff+2)=0
                  jtet1(jtoff+3)=0
                  jtet1(jtoff+4)=0
                  jtet1(jtoff+5)=0
                  jtet1(jtoff+6)=0
                  itoff=itoff+nelmnen(ifelmhex)
                  jtoff=jtoff+nelmnef(ifelmhex)
               else

               write(logmess,'(a,i10,i5)') "Bad element type: ",
     *         ielements,itype 
               call writloga('default',0,logmess,0,ierror)

               endif
            enddo
CCC         call geniee_cmo(cmo)
CCCC        ! moved to end
         endif
         goto 100
C
      elseif(iword1(1:8).eq.'velocity') then
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,iflag4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         iflag=iflag4
         if(iflag.eq.1) then
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            if(nnodes.gt.0) then
               call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *                        ipout,lout,itype,ierror_return)
 
               if(ier.ne.0) cvelnm='vels'
               call cmo_get_info(cvelnm,cmo,ipvels,ilen,ityp,ierr)
               if(ierr.ne.0) then
                 cbuff ='cmo/addatt/-def-/vels/VDOUBLE/vector/' //
     *                  'nnodes/linear/permanent/gxa/0.0 ;  ' //
     *                  'finish'
                 call dotask(cbuff,ier)
                 call cmo_get_info('vels',cmo,ipvels,ilen,ityp,ierr)
               endif
               length=nnodes
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nnodes
                  vels(1,i)=xtemp4(i)
               enddo
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nnodes
                  vels(2,i)=xtemp4(i)
               enddo
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nnodes
                  vels(3,i)=xtemp4(i)
               enddo
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            endif
         else
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            if(nelements.gt.0) then
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            endif
         endif
         goto 100
C
      elseif(iword1(1:8).eq.'variable') then
C
 110     continue
         ibytes=8
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,cmoattnam,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
            call nulltoblank_lg(cmoattnam,charlength)
         if(cmoattnam(1:7).eq.'endvars') goto 100
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,iflag4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         iflag=iflag4
         if(iflag.eq.1) then
            call cmo_get_info('nnodes',cmo,
     *                        nnodes,ilen,itype,icscode)
            len1=icharlnb(cmoattnam)
            do j=1,len1
               if(cmoattnam(j:j).eq.' ') then
                  cmoattnam(j:j)='_'
               endif
            enddo
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *          clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               length=nnodes
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  do i=1,nnodes
                     iarray(i)=xtemp4(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  do i=1,nnodes
                     xarray(i)=xtemp4(i)
                  enddo
               endif
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nnodes/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               length=nnodes
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nnodes
                  xarray(i)=xtemp4(i)
               enddo
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            endif
         elseif(iflag.eq.0) then
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            len1=icharlnb(cmoattnam)
            do j=1,len1
               if(cmoattnam(j:j).eq.' ') then
                  cmoattnam(j:j)='_'
               endif
            enddo
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *         clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  do i=1,nelements
                     iarray(i)=xtemp4(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  do i=1,nelements
                     xarray(i)=xtemp4(i)
                  enddo
               endif
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            else
C
C    Special case for AMR variables
C
              if((cmoattnam(1:7).eq.'itetpar') .or. 
     *           (cmoattnam(1:7).eq.'itetkid') .or. 
     *           (cmoattnam(1:7).eq.'itetlev'))then 
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VINT' //
     *               '/scalar/nelements/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nelements
                  iarray(i)=nint(xtemp4(i))
               enddo
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
C
C    END Special case for AMR variables
C
              else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nelements/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nelements
                  xarray(i)=xtemp4(i)
               enddo
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
              endif
            endif
         else
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            len1=icharlnb(cmoattnam)
            do j=1,len1
               if(cmoattnam(j:j).eq.' ') then
                  cmoattnam(j:j)='_'
               endif
            enddo
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
 
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *          clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  do i=1,nelements
                     iarray(i)=xtemp4(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  do i=1,nelements
                     xarray(i)=xtemp4(i)
                  enddo
               endif
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nelements/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               length=nelements
               call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,xtemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do i=1,nelements
                  xarray(i)=xtemp4(i)
               enddo
               call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
            endif
         endif
         goto 110
      elseif(iword1(1:5).eq.'flags') then
C
 
 120     continue
         ibytes=8
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,cmoattnam,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
            call nulltoblank_lg(cmoattnam,charlength)
         if(cmoattnam(1:7).eq.'endflag') goto 100
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,maxclrpoint4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
            maxclrpoint=maxclrpoint4
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,iflag4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
            iflag=iflag4
         do i=1,maxclrpoint
            iword2=' '
            ibytes=8
            nbyte_total = nbyte_total + ibytes
            call cread(iunit4,iword2,ibytes,ierror)
            if (ierror.ne.0 ) go to 9998
         enddo
         if(iflag.eq.1) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *         clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  length=nnodes
                  call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &               ,icscode)
                  ibytes=4*nnodes
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,itemp4,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  do it=1,nnodes
                      iarray(it)=itemp4(it)
                  enddo
                  call mmrelblk('itemp4',isubname,ipitemp4,icscode)

               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  length=nnodes
                  call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
                  ibytes=4*nnodes
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,xtemp4,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  do i=1,nnodes
                     xarray(i)=xtemp4(i)
                  enddo
                  call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
               endif
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VINT' //
     *               '/scalar/nnodes/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
               length=nnodes
               call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &            ,icscode)
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,itemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
                  do it=1,nnodes
                     iarray(it)=itemp4(it)
                  enddo
                  call mmrelblk('itemp4',isubname,ipitemp4,icscode)
            endif
         elseif(iflag.eq.0) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *         clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  length=nelements
                  call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &               ,icscode)
                  ibytes=4*nelements
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,itemp4,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                   do it=1,nelements
                      iarray(it)=itemp4(it)
                   enddo
                   call mmrelblk('itemp4',isubname,ipitemp4,icscode)
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  length=nelements
                  call mmgetblk('xtemp4',isubname,ipxtemp4,length,2,ics)
                  ibytes=4*nelements
                  nbyte_total = nbyte_total + ibytes
                  call cread(iunit4,xtemp4,ibytes,ierror)
                  if (ierror.ne.0 ) go to 9998
                  do i=1,nelements
                     xarray(i)=xtemp4(i)
                  enddo
                  call mmrelblk('xtemp4',isubname,ipxtemp4,icscode)
               endif
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VINT' //
     *               '/scalar/nelements/linear/permanent/gxa/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
               length=nelements
               call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &            ,icscode)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,itemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do it=1,nelements
                   iarray(it)=itemp4(it)
               enddo
               call mmrelblk('itemp4',isubname,ipitemp4,icscode)
            endif
         endif
C
         goto 120
      elseif(iword1(1:8).eq.'material') then
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      isgeom=.true.
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      if(ierror.ne.0) then
             isgeom=.false.
             write(logmess,'(a)') ' no geometry for mesh object'
             call writloga('default',0,logmess,0,ierrwrt)
      endif
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      if(ierror.ne.0) isgeom=.false.
C
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,maxclrpoint4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         maxclrpoint=maxclrpoint4
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,iflag4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         do i=1,maxclrpoint
            iword2=' '
            ibytes=8
            nbyte_total = nbyte_total + ibytes
            call cread(iunit4,iword2,ibytes,ierror)
            if (ierror.ne.0 ) go to 9998
               call nulltoblank_lg(iword2,charlength)
            len1=icharlnb(iword2)
            do j=1,len1
               if(iword2(j:j).eq.' ') then
                  iword2(j:j)='_'
               endif
            enddo
            if(isgeom) then
              do j=1,nmregs
                if(cmregs(j).eq.iword2(1:len1)) matregs(j)=i
              enddo
            endif
         enddo
         if(iflag.eq.1) then
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            if(nnodes.gt.0) then
               call cmo_get_info('nelements',cmo,
     *                           nelements,ilen,itype,icscode)
               call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,leni,itp,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,leni,itp,ier)
               length=nnodes
               call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &            ,icscode)
               ibytes=4*nnodes
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,itemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do it=1,nnodes
                  imt1(it)=itemp4(it)
               enddo
               do it=1,nelements
                  itetclr(it)=imt1(itet1(itetoff(it)+1))
               enddo
               call mmrelblk('itemp4',isubname,ipitemp4,icscode)
            endif
         elseif(iflag.eq.0) then
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
            if(nelements.gt.0) then
               length=nelements
               call mmgetblk('itemp4',isubname,ipitemp4,length,1
     &            ,icscode)
               ibytes=4*nelements
               nbyte_total = nbyte_total + ibytes
               call cread(iunit4,itemp4,ibytes,ierror)
               if (ierror.ne.0 ) go to 9998
               do it=1,nelements
                  itetclr(it)=itemp4(it)
               enddo
               call mmrelblk('itemp4',isubname,ipitemp4,icscode)
            endif
         endif
         goto 100
C
      elseif(iword1(1:8).eq.'polygons') then
         len1=icharlnf(iword1)
         ird=1
         ibytes=4
         dowhile(ird.eq.1)
             nbyte_total = nbyte_total + ibytes
             call cread(iunit4,iword1,ibytes,ierror)
             if (ierror.ne.0 ) go to 9998
             if(iword1(1:4).eq.'endp') then
                nbyte_total = nbyte_total + ibytes
                call cread(iunit4,iword1,ibytes,ierror)
                if (ierror.ne.0 ) go to 9998
                if(iword1(1:3).eq.'oly') ird=0
             endif
         enddo
         goto 100
C
      elseif(iword1(1:7).eq.'cycleno') then
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,ihcycle4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         ihcycle = ihcycle4
 
         call set_global('ihcycle',
     *                   ihcycle,rout,cout,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_global')
         goto 100
      elseif(iword1(1:8).eq.'probtime') then
         ibytes=4
         nbyte_total = nbyte_total + ibytes
         call cread(iunit4,xtime4,ibytes,ierror)
         if (ierror.ne.0 ) go to 9998
         time=xtime4
 
         call set_global('time',iout,
     *                   time,cout,2,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_global')
         goto 100

C     this is not always an error - skip and continue
C     often encounter spaces or ?? characters
C     need to check that all tags are parsed correctly
C     and that write gmv is writing binary correctly
C     check variable polygons cycleno probtime
      else

C        write(logmess,9000) iword1(1:icharlnf(iline))
C        call writloga('default',0,logmess,0,ierr)
C9000    format('GMV binary input not recognized:  ',a)
         goto 100
      endif
C
      
 9998 continue

      if (ierror .ne. 0) then
         write(logmess,98) file(1:icharlnf(file)),nbyte_total
   98    format('ERROR reading GMV binary file: ',a32,
     >   " near byte ",i10)
         call writloga('default',0,logmess,0,ierror)
      endif

 9999 continue

      call cmo_get_info('nnodes',cmo,
     *                   nnodes,ilen,itype,ierror)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilen,itype,icscode)

      if (ierror.ne. 0) then
         write(logmess,'(a,a)')
     *   "Error: Problem setting up cmo ",cmo 
         call writloga('default',1,logmess,0,ierr)

      else if (nelements.gt.0) then
          call dotask('geniee; finish',ierror)
      endif

      write(logmess,90) nnodes,nelements
   90 format('Nodes: ',i10 ' Elements: ',i10)
      call writloga('default',0,logmess,0,ierror)

      write(logmess,91) nbyte_total,file(1:icharlnf(file))
   91 format('Done reading GMV binary total bytes: ',i10,
     >" file:  ",a)
      call writloga('default',0,logmess,1,ierror)

      return
      end
