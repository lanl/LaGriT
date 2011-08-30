      subroutine readgmv_ascii(ifile,ierror)
c
c #####################################################################
c
c     purpose -
c
c        read a gmv formatted file into the current mesh object
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
C        $Log: readgmv_ascii.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   22 Jul 2004 14:24:02   gable
CPVCS    Found error in tet and hex connectivity. Insert code to invert
CPVCS    connectivity so that tet and hex are not inside out. Also added
CPVCS    simdate and codename keyword to header of ascii GMV files. These
CPVCS    keywords are used in the read/gmv code now to detect the new
CPVCS    corrected connectivity.
CPVCS    
CPVCS       Rev 1.3   24 Mar 2003 14:36:22   gable
CPVCS    Fixed format so that ascii input can exceed 1 million nodes.
CPVCS    Also recognize AMR mesh variables itetpar, itetkid, itetlev
CPVCS    as VINT.
CPVCS    
CPVCS       Rev 1.2   27 Nov 2002 10:13:08   gable
CPVCS    Added option of reading 'lin' type elements in gmvfreeformat.
CPVCS    Fixed error in read/gmv with line type elements. Error/typo
CPVCS    did not fine keyword 'lin'.
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:34:54 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.0   02 Feb 2000 17:10:14   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.15   Tue Dec 14 10:24:38 1999   dcg
CPVCS    new option read/gmvfreeformat for ascii gmv files to
CPVCS    be read with read(*) type read statements
CPVCS
CPVCS       Rev 1.14   Tue Nov 30 17:05:36 1999   jtg
CPVCS    added in ability to read line type elements.
CPVCS    Also default jtet changed to 0 instead of -1 in anticipation
CPVCS    of future mbndry=0 convention change.
CPVCS
CPVCS       Rev 1.13   Mon Jul 12 16:48:28 1999   dcg
CPVCS    delete unneeded replacement statement
CPVCS
CPVCS       Rev 1.12   Thu Jul 08 10:47:58 1999   dcg
CPVCS    replace read(iunit,*) with formatted reads
CPVCS
CPVCS       Rev 1.11   Tue Jul 06 19:29:08 1999   jtg
CPVCS    modifed call to geniee (put at end)
CPVCS
CPVCS       Rev 1.10   Fri Jul 02 14:37:40 1999   dcg
CPVCS    add end-of-file test to read
CPVCS    skip blank lines
CPVCS    clean up memory management calls
CPVCS    remove unused pointer statements
CPVCS
CPVCS       Rev 1.9   Fri Oct 31 10:49:40 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:57:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Dec 02 08:54:00 1996   het
CPVCS    Read element size variable fields.
CPVCS
CPVCS       Rev 1.6   Wed Nov 13 13:48:02 1996   het
CPVCS    read connectivity for quad and tri GMV cells.
c
c ######################################################################
c
      implicit none
C
      character*132 logmess
C
c
      include 'geom_lg.h'
      include "local_element.h"
c
      character*(*) ifile
      integer iflag_all,ipolydata,ivoronoi3d,ivoronoi2d,ihcycle,
     *  ics,j,ifprm,len2,imore,icount,maxclrpoint,lenc,
     *  lenout,length,ierror_return,lout,iout,imat,
     *  ifelmnew,leni,nee,nsdtopo,nsdgeom,i1,i2,i3,i4,i5,i6,i7,i8,
     *  ifound,itoff,jtoff,ierr,ilen,ityp,nelements,icscode,nnodes,
     *  icharlnf,len1,ihybrid,iunit,ielements,ierror,ier
      integer if_invert_ele, if_lagrit, if_invert_hit
      integer icharlnb,iflag,index,itp,ierrwrt,itype,i
      real*8 time,rout,a,b,c,d,e,f,crosx,crosy,crosz
      character*32 cinter,cpers,cio,clen,crank,cout,iword2
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipvels,vels)
      real*8 vels(3,*)
C
      real*8 xic(*), yic(*), zic(*)
C
      integer nsd, nen, nef
      pointer (ipitetclr, itetclr(*))
      pointer (ipitettyp, itettyp(*))
      pointer (ipitetoff, itetoff(*))
      pointer (ipjtetoff, jtetoff(*))
      pointer (ipitet, itet1(*))
      pointer (ipjtet, jtet1(*))
      integer itetclr,itettyp,itetoff,jtetoff,itet1,jtet1
C
      pointer (ipxarray, xarray(*))
      pointer (ipiarray, iarray(*))
      real*8 xarray
      integer iarray
      pointer (ipxtemp,xtemp)
      real*8 xtemp(*)
      pointer (ipout,out)
      real*8 out(*)
C
      character*132 iline, iword
      character*32 isubname,cmo,cmoattnam,ctype,geom_name
 
      character*32   cmotype, cvelnm
      character*8092 cbuff
 
c
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data ipolydata / 1 /
      data iflag_all / 0 /
c
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
c
      isubname="readgmv_ascii"
 
C
C     Due to a past bug in IO of tets, both write and read
C     of tets was inverted. To fix this we have corrected
C     the connectivity output in dumpgmv_hybrid_nosb.f. However,
C     to detect old vs. new files we now include the GMV keywords
C     'simdate' and 'codename' as part of the GMV output. It will
C     be assumed that if the simdate keyword exist, then tets do
C     not need to be inverted. If simdate does not exist in the
C     GMV file, then tet connectivity must be inverted.
C
      if_invert_ele = 1 
      if_lagrit     = 0 
      if_invert_hit = 0 
C
      iunit=-1
      call hassign(iunit,ifile,ierror) 
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        write(logmess,*) 'WARNING: file not read: '
     1     // ifile(1:icharlnf(ifile))
        call writloga('default',0,logmess,1,ierr)
        ierror = -1
        goto 9999
      endif
C
      read(iunit,'(a80)') iline
      if(iline(1:14).ne.'gmvinput ascii') then
         print *,"Not a GMV dump"
         close(iunit)
         goto 9999
      endif
C
      ihybrid=0
C
 100  continue
      call cmo_get_name(cmo,icscode)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      read(iunit,'(a80)',err=9998,end=9999) iline
      if(iline(1:80).eq.' ') go to 100
      if(iline(1:6).eq.'endgmv') goto 9999
C
      if(iline(1:5).eq.'nodes') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) nnodes
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
            read(iunit,"(10(e14.5))") (xic(i),i=1,nnodes)
            read(iunit,"(10(e14.5))") (yic(i),i=1,nnodes)
            read(iunit,"(10(e14.5))") (zic(i),i=1,nnodes)
C
         endif
         goto 100
      elseif(iline(1:8).eq.'codename') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iword,*) iline
         if(iline(1:6) .eq. 'LaGriT')if_lagrit = 1
         goto 100
      elseif(iline(1:8).eq.'simdate') then
C
C     Do not invert tet connectivity. If simdate keyword
C     exists and this file also contains 'codename LaGriT'.
C
         if_invert_ele = 0 
         goto 100
      elseif(iline(1:5).eq.'cells') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) nelements
         len1=max(nnodes,nelements)
         call mmgetblk('xtemp',isubname,ipxtemp,len1,2,icscode)
C
         call cmo_get_name(cmo,icscode)
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
C
         if(nelements.gt.0) then
            itoff=0
            jtoff=0
            do ielements=1,nelements
               ifound=0
               read(iunit,'(a80)') iline
               len1=icharlnf(iline)
               ctype=iline(1:len1)
c              iline=iline((len1+1):len(iline))
               if(nnodes .lt. 1000000)then
               if(ctype(1:3).eq.'lin') then
                  read(iline,"(9x,i4,8i7)") itype,i1,i2
               elseif(ctype(1:3).eq.'tri') then
                  read(iline,"(9x,i4,8i7)") itype,i1,i2,i3
               elseif(ctype(1:4).eq.'quad') then
                  read(iline,"(9x,i4,8i7)") itype,i1,i2,i3,i4
               elseif(ctype(1:3).eq.'tet') then
                  if(if_invert_ele .eq. 0)then
                    read(iline,"(9x,i4,8i7)") itype,i1,i3,i2,i4
                  else
                    read(iline,"(9x,i4,8i7)") itype,i1,i2,i3,i4
                    if_invert_hit = 1
                  endif
                  if(i1.eq.i4) ctype='tri'
               elseif(ctype(1:3).eq.'pyr') then
                  read(iline,"(9x,i4,8i7)") itype,i5,i1,i4,i3,i2
               elseif(ctype(1:3).eq.'pri') then
                  read(iline,"(9x,i4,8i7)") itype,i1,i2,i3,i4,i5,i6
               elseif(ctype(1:3).eq.'hex') then
                  if(if_invert_ele .eq. 0)then
                  read(iline,"(9x,i4,8i7)") itype,i5,i6,i7,i8,i1,i2,i3,
     *                        i4
                  else
                  read(iline,"(9x,i4,8i7)") itype,i1,i2,i3,i4,i5,i6,i7,
     *                        i8
                    if_invert_hit = 1
                  endif
                  if(i1.eq.i5.and.i2.eq.i6 .and.
     *                  i3.eq.i7.and.i4.eq.i8) ctype='quad'
               else
                  write(logmess,'(a,a)')
     *               'Unsupported GMV element type: ',ctype
                  call writloga('default',0,logmess,0,ierrwrt)
               endif
               elseif(nnodes .ge. 1000000)then
               if(ctype(1:3).eq.'lin') then
                  read(iline,"(9x,i4,8i10)") itype,i1,i2
               elseif(ctype(1:3).eq.'tri') then
                  read(iline,"(9x,i4,8i10)") itype,i1,i2,i3
               elseif(ctype(1:4).eq.'quad') then
                  read(iline,"(9x,i4,8i10)") itype,i1,i2,i3,i4
               elseif(ctype(1:3).eq.'tet') then
                  if(if_invert_ele .eq. 0)then
                    read(iline,"(9x,i4,8i10)") itype,i1,i3,i2,i4
                  else
                    read(iline,"(9x,i4,8i10)") itype,i1,i2,i3,i4
                    if_invert_hit = 1
                  endif
                  if(i1.eq.i4) ctype='tri'
               elseif(ctype(1:3).eq.'pyr') then
                  read(iline,"(9x,i4,8i10)") itype,i5,i1,i4,i3,i2
               elseif(ctype(1:3).eq.'pri') then
                  read(iline,"(9x,i4,8i10)") itype,i1,i2,i3,i4,i5,i6
               elseif(ctype(1:3).eq.'hex') then
                  if(if_invert_ele .eq. 0)then
                  read(iline,"(9x,i4,8i10)") itype,i5,i6,i7,i8,i1,i2,i3,
     *                        i4
                  else
                  read(iline,"(9x,i4,8i10)") itype,i1,i2,i3,i4,i5,i6,i7,
     *                        i8
                    if_invert_hit = 1
                  endif
                  if(i1.eq.i5.and.i2.eq.i6 .and.
     *                  i3.eq.i7.and.i4.eq.i8) ctype='quad'
               else
                  write(logmess,'(a,a)')
     *               'Unsupported GMV element type: ',ctype
                  call writloga('default',0,logmess,0,ierrwrt)
               endif
               endif
               if(ifound.eq.0.and.ielements.eq.1) then
                  if(ctype(1:3).eq.'lin') then
                        cmotype='lin'
                        nsd=2
                        nsdgeom=3
                        nsdtopo=1
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
               if(ctype(1:3).eq.'lin') then
                  ifelmnew=ifelmlin
               elseif(ctype(1:3).eq.'tri') then
                  ifelmnew=ifelmtri
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
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
                  print *,"Bad element type: ",ielements,itype
               endif
            enddo
CCC         call geniee_cmo(cmo)
CCCC        ! moved to end....
         endif
         goto 100
C
      elseif(iline(1:8).eq.'velocity') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) iflag
         if(iflag.eq.1) then
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            if(nnodes.gt.0) then
               call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *                        ipout,lout,itype,ierror_return)
 
               if(ier.ne.0) cvelnm='vels'
               call cmo_get_info(cvelnm,cmo,ipvels,ilen,ityp,ierr)
               if(ierr.ne.0) then
                 cbuff =     'cmo/addatt/-def-/vels/VDOUBLE/vector/' //
     *                  'nnodes/linear/permanent/gx/0.0 ;  ' //
     *                  'finish'
                 call dotask(cbuff,ier)
                 call cmo_get_info('vels',cmo,ipvels,ilen,ityp,ierr)
               endif
               read(iunit,"(4(1x,e22.14))") (vels(1,i),i=1,nnodes)
               read(iunit,"(4(1x,e22.14))") (vels(2,i),i=1,nnodes)
               read(iunit,"(4(1x,e22.14))") (vels(3,i),i=1,nnodes)
            endif
         else
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            if(nelements.gt.0) then
               length=nelements
               call mmgetblk('xarray',isubname,
     *                       ipxarray,length,2,icscode)
               read(iunit,"(4(1x,e22.14))") (xarray(i),i=1,nelements)
               read(iunit,"(4(1x,e22.14))") (xarray(i),i=1,nelements)
               read(iunit,"(4(1x,e22.14))") (xarray(i),i=1,nelements)
               call mmrelblk('xarray',isubname,ipxarray,icscode)
            endif
         endif
         goto 100
C
      elseif(iline(1:8).eq.'variable') then
C
 110     continue
         read(iunit,'(a80)') iline
         if(iline(1:7).eq.'endvars') goto 100
         len1=icharlnf(iline)
         cmoattnam=iline(1:len1)
         iline=iline((len1+1):len(iline))
         read(iline,*) iflag
         if(iflag.eq.1) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               ctype=' '
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *         clen,cinter,cpers,cio,ierror_return)
 
 
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  read(iunit,"(10(e14.5))") (xtemp(i),i=1,nnodes)
                 do i=1,nnodes
                   iarray(i)=nint(xtemp(i))
                 enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  read(iunit,"(10(e14.5))") (xarray(i),i=1,nnodes)
               endif
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               read(iunit,"(10(e14.5))") (xarray(i),i=1,nnodes)
            endif
         elseif(iflag.eq.0) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *         clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  read(iunit,"(10(e14.5))") (xtemp(i),i=1,nelements)
                 do i=1,nnodes
                   iarray(i)=nint(xtemp(i))
                 enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  read(iunit,"(10(e14.5))") (xarray(i),i=1,nelements)
               endif
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nelements/linear/permanent/gxaf/0.0' //
     *               ' ; finish '
               call dotask(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               read(iunit,"(10(e14.5))") (xarray(i),i=1,nelements)
            endif
         else
            length=nelements
            call mmgetblk('xarray',isubname,ipxarray,length,2,icscode)
            read(iunit,"(10(e14.5))") (xarray(i),i=1,nelements)
            call mmrelblk('xarray',isubname,ipxarray,icscode)
         endif
         goto 110
      elseif(iline(1:5).eq.'flags') then
C
 
 120     continue
 
         read(iunit,'(a80)') iline
         if(iline(1:7).eq.'endflag') goto 100
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) maxclrpoint,iflag
         icount=0
         dowhile(icount.lt.maxclrpoint)
            read(iunit,'(a80)') iline
            imore=0
            dowhile(imore.eq.0)
               icount=icount+1
               len1=icharlnf(iline)
               len2=icharlnb(iline)
               iword=iline(1:len1)
               if(ifprm.eq.0) then
 
               do j=1,nmregs
                 if(cmregs(j).eq.iword2) matregs(j)=i
               enddo
 
               endif
               if(len2.gt.len1) then
                  iline=iline((len1+1):len2)
               else
                  imore=1
               endif
            enddo
         enddo
         if(iflag.eq.1) then
            length=nnodes
            call mmgetblk('iarray',isubname,ipiarray,length,2,icscode)
            read(iunit,"(20i5)") (iarray(i),i=1,nnodes)
            call mmrelblk('iarray',isubname,ipiarray,icscode)
         elseif(iflag.eq.0) then
            length=nelements
            call mmgetblk('iarray',isubname,ipiarray,length,2,icscode)
            read(iunit,"(20i5)") (iarray(i),i=1,nelements)
            call mmrelblk('iarray',isubname,ipiarray,icscode)
         endif
C
         goto 120
      elseif(iline(1:8).eq.'material') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) maxclrpoint,iflag
         icount=0
         dowhile(icount.lt.maxclrpoint)
            read(iunit,'(a80)') iline
            imore=0
            dowhile(imore.eq.0)
               icount=icount+1
               len1=icharlnf(iline)
               len2=icharlnb(iline)
               iword=iline(1:len1)
               if(ifprm.eq.0) then
               do j=1,nmregs
                if(cmregs(j).eq.iword2) matregs(j)=i
               enddo
               endif
               if(len2.gt.len1) then
                  iline=iline((len1+1):len2)
               else
                  imore=1
               endif
            enddo
         enddo
         if(iflag.eq.1) then
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            if(nnodes.gt.0) then
               length=nnodes
               call mmgetblk('iarray',isubname,ipiarray,length,2,ics)
               read(iunit,"(20i5)") (iarray(i),i=1,nnodes)
               call mmrelblk('iarray',isubname,ipiarray,icscode)
            endif
         elseif(iflag.eq.0) then
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
            if(nelements.gt.0) then
               read(iunit,"(20i5)") (itetclr(i),i=1,nelements)
            endif
         endif
         goto 100
C
      elseif(iline(1:8).eq.'polygons') then
         len1=icharlnf(iline)
         dowhile(iline(1:len1).ne.'endpoly')
            read(iunit,'(a80)') iline
            len1=icharlnf(iline)
         enddo
         goto 100
C
      elseif(iline(1:7).eq.'cycleno') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) ihcycle
         call set_global('ihcycle',
     *                   ihcycle,rout,cout,1,icscode)
 
         if (icscode .ne. 0) call x3d_error(isubname,'set_ihcycle')
      elseif(iline(1:8).eq.'probtime') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) time
 
        call set_global('time',iout,
     *                   time,cout,2,icscode)
 
 
         if (icscode .ne. 0) call x3d_error(isubname,'set_time')
      else
         write(logmess,9000) iline(1:icharlnf(iline))
         call writloga('default',1,logmess,0,ierr)
 9000    format('ASCII GMV input not recognized:  ',a)
         goto 100
      endif
C
      goto 9998
 9998 continue
      goto 9999
 9999 continue
C
C     Print a warning message to the screen
C     if it is an old, pre-bug fix, ascii, tet, gmv file
C
      if(if_invert_hit .eq. 1)then
      if(if_invert_ele .eq. 0)then
c       write(logmess,'(a)')
c      1   'Read LaGriT GMV file with correct connectivity'
c       call writloga('default',0,logmess,0,ierrwrt)
      elseif(if_invert_ele .eq. 1)then
      write(logmess,'(a)')
     1   'WARNING:LaGriT GMV file with inverted hex/tet connectivity'
      call writloga('default',0,logmess,0,ierrwrt)
      write(logmess,'(a)')
     1   'WARNING:hex connectivity will be read as 5 6 7 8 1 2 3 4'
      call writloga('default',0,logmess,0,ierrwrt)
      write(logmess,'(a)')
     1   'WARNING:tet connectivity will be read as 1 3 2 4'
      call writloga('default',0,logmess,0,ierrwrt)
      endif
      endif
      
      call mmrelprt(isubname,icscode)

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

      write(logmess,91) ifile(1:icharlnf(ifile))
   91 format('Done reading GMV ascii file: ',a)
      call writloga('default',0,logmess,1,ierror)

      return
      end
c
c
c
c
c
      subroutine readgmv_asciistar(ifile,ierror)
c
c #####################################################################
c
c     purpose -
c
c        read a gmv formatted file into the current mesh object
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
C        $Log: readgmv_ascii.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.11   Tue Jul 06 19:29:08 1999   jtg
CPVCS    modifed call to geniee (put at end)
CPVCS
CPVCS       Rev 1.10   Fri Jul 02 14:37:40 1999   dcg
CPVCS    add end-of-file test to read
CPVCS    skip blank lines
CPVCS    clean up memory management calls
CPVCS    remove unused pointer statements
CPVCS
CPVCS       Rev 1.9   Fri Oct 31 10:49:40 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:57:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Dec 02 08:54:00 1996   het
CPVCS    Read element size variable fields.
CPVCS
CPVCS       Rev 1.6   Wed Nov 13 13:48:02 1996   het
CPVCS    read connectivity for quad and tri GMV cells.
c
c ######################################################################
c
      implicit none
C
      character*132 logmess
c
      include 'geom_lg.h'
      include "local_element.h"
C
      integer iflag_all,ipolydata,ivoronoi3d,ivoronoi2d,ihcycle,
     *  ics,j,ifprm,len2,imore,icount,maxclrpoint,lenc,
     *  lenout,length,ierror_return,lout,iout,imat,
     *  ifelmnew,leni,nee,nsdtopo,nsdgeom,i1,i2,i3,i4,i5,i6,i7,i8,
     *  ifound,itoff,jtoff,ierr,ilen,ityp,nelements,icscode,nnodes,
     *  icharlnf,len1,ihybrid,iunit,ielements,ier,ierror
      integer icharlnb,iflag,index,itp,ierrwrt,itype,i
      real*8 time,rout,a,b,c,d,e,f,crosx,crosy,crosz
      character*32 cinter,cpers,cio,crank,clen
c
      character*(*) ifile
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipvels,vels)
      real*8 vels(3,*)
C
      real*8 xic(*), yic(*), zic(*)
C
      integer nsd, nen, nef
      pointer (ipitetclr, itetclr(*))
      pointer (ipitettyp, itettyp(*))
      pointer (ipitetoff, itetoff(*))
      pointer (ipjtetoff, jtetoff(*))
      pointer (ipitet, itet1(*))
      pointer (ipjtet, jtet1(*))
      integer itetclr,itettyp,itetoff,jtetoff,itet1,jtet1
C
C
      pointer (ipxarray, xarray(*))
      pointer (ipiarray, iarray(*))
      real*8 xarray
      integer iarray
      pointer (ipout,out)
      real*8 out(*)
C
      character*132 iline, iword
      character*32 isubname,cmo,cmoattnam,ctype,geom_name
 
      character*32   cmotype, cvelnm
      character*8092 cbuff
 
c
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data ipolydata / 1 /
      data iflag_all / 0 /
c
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
c
      isubname="readgmv_asciistar"
 
 
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        write(logmess,*) 'WARNING: file not read: '
     1     // ifile(1:icharlnf(ifile))
        call writloga('default',0,logmess,1,ierr)
        ierror = -1
        goto 9999
      endif
C
      read(iunit,'(a80)') iline
      if(iline(1:14).ne.'gmvinput ascii') then
         print *,"Not a GMV dump"
         close(iunit)
         goto 9999
      endif
C
      ihybrid=0
C
 100  continue
      call cmo_get_name(cmo,icscode)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      read(iunit,'(a80)',err=9998,end=9999) iline
      if(iline(1:80).eq.' ') go to 100
      if(iline(1:6).eq.'endgmv') goto 9999
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
C
      if(iline(1:5).eq.'nodes') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) nnodes
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
            read(iunit,*) (xic(i),i=1,nnodes)
            read(iunit,*) (yic(i),i=1,nnodes)
            read(iunit,*) (zic(i),i=1,nnodes)
C
         endif
         goto 100
      elseif(iline(1:5).eq.'cells') then
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) nelements
C
         call cmo_get_name(cmo,icscode)
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
C
         if(nelements.gt.0) then
            itoff=0
            jtoff=0
            do ielements=1,nelements
               ifound=0
               read(iunit,'(a80)') iline
               len1=icharlnf(iline)
               ctype=iline(1:len1)
               iline=iline((len1+1):len(iline))
               if(ctype(1:3).eq.'lin') then
                  read(iline,*) itype,i1,i2
               elseif(ctype(1:3).eq.'tri') then
                  read(iline,*) itype,i1,i2,i3
               elseif(ctype(1:4).eq.'quad') then
                  read(iline,*) itype,i1,i2,i3,i4
               elseif(ctype(1:3).eq.'tet') then
                  read(iline,*) itype,i1,i2,i3,i4
                  if(i1.eq.i4) ctype='tri'
               elseif(ctype(1:3).eq.'pyr') then
                  read(iline,*) itype,i5,i1,i4,i3,i2
               elseif(ctype(1:3).eq.'pri') then
                  read(iline,*) itype,i1,i2,i3,i4,i5,i6
               elseif(ctype(1:3).eq.'hex') then
                  read(iline,*) itype,i1,i2,i3,i4,i5,i6,i7,i8
                  if(i1.eq.i5.and.i2.eq.i6 .and.
     *                  i3.eq.i7.and.i4.eq.i8) ctype='quad'
               else
                  write(logmess,'(a,a)')
     *               'Unsupported GMV element type: ',ctype
                  call writloga('default',0,logmess,0,ierrwrt)
               endif
               if(ifound.eq.0) then
                  if(ctype(1:3).eq.'lin') then
                     if(ielements.eq.1) then
                        cmotype='lin'
                        nsd=2
                        nsdgeom=3
                        nsdtopo=1
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
                     endif
                  elseif(ctype(1:3).eq.'tri') then
                     if(ielements.eq.1) then
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
                     endif
                  endif
               endif
               if(ifound.eq.0) then
                  if(ctype(1:4).eq.'quad') then
                     if(ielements.eq.1) then
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
                     endif
                  endif
               endif
               if(ifound.eq.0) then
                  if(ctype(1:3).eq.'tet') then
                     if(ielements.eq.1) then
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
                     endif
                  endif
               endif
               if(ifound.eq.0) then
                  if(ctype(1:3).eq.'pyr') then
                     if(ielements.eq.1) then
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
                     endif
                  endif
               endif
               if(ifound.eq.0) then
                  if(ctype(1:3).eq.'pri') then
                     if(ielements.eq.1) then
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
                     endif
                  endif
               endif
               if(ifound.eq.0) then
                  if(ctype(1:3).eq.'hex') then
                     if(ielements.eq.1) then
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
               endif
               if(ctype(1:3).eq.'lin') then
                  ifelmnew=ifelmlin
               elseif(ctype(1:3).eq.'tri') then
                  ifelmnew=ifelmtri
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
                  itetclr(ielements)=imat
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
                  itetclr(ielements)=imat
                  itettyp(ielements)=ifelmtri
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  itoff=itoff+nelmnen(ifelmtri)
                  jtoff=jtoff+nelmnef(ifelmtri)
               elseif(ctype(1:4).eq.'quad') then
                  itetclr(ielements)=imat
                  itettyp(ielements)=ifelmqud
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  jtet1(jtoff+4)=-1
                  itoff=itoff+nelmnen(ifelmqud)
                  jtoff=jtoff+nelmnef(ifelmqud)
               elseif(ctype(1:3).eq.'tet') then
                  itetclr(ielements)=imat
                  itettyp(ielements)=ifelmtet
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  jtet1(jtoff+4)=-1
                  itoff=itoff+nelmnen(ifelmtet)
                  jtoff=jtoff+nelmnef(ifelmtet)
               elseif(ctype(1:3).eq.'pyr') then
                  itetclr(ielements)=imat
                  itettyp(ielements)=ifelmpyr
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  itet1(itoff+5)=i5
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  jtet1(jtoff+4)=-1
                  jtet1(jtoff+5)=-1
                  itoff=itoff+nelmnen(ifelmpyr)
                  jtoff=jtoff+nelmnef(ifelmpyr)
               elseif(ctype(1:3).eq.'pri') then
                  itetclr(ielements)=imat
                  itettyp(ielements)=ifelmpri
                  itetoff(ielements)=itoff
                  jtetoff(ielements)=jtoff
                  itet1(itoff+1)=i1
                  itet1(itoff+2)=i2
                  itet1(itoff+3)=i3
                  itet1(itoff+4)=i4
                  itet1(itoff+5)=i5
                  itet1(itoff+6)=i6
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  jtet1(jtoff+4)=-1
                  jtet1(jtoff+5)=-1
                  itoff=itoff+nelmnen(ifelmpri)
                  jtoff=jtoff+nelmnef(ifelmpri)
               elseif(ctype(1:3).eq.'hex') then
                  itetclr(ielements)=imat
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
                  jtet1(jtoff+1)=-1
                  jtet1(jtoff+2)=-1
                  jtet1(jtoff+3)=-1
                  jtet1(jtoff+4)=-1
                  jtet1(jtoff+5)=-1
                  jtet1(jtoff+6)=-1
                  itoff=itoff+nelmnen(ifelmhex)
                  jtoff=jtoff+nelmnef(ifelmhex)
               else
                  print *,"Bad element type: ",ielements,itype
               endif
            enddo
CCC         call geniee_cmo(cmo)
CCCC        ! moved to end....
         endif
         goto 100
C
      elseif(iline(1:8).eq.'velocity') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) iflag
         if(iflag.eq.1) then
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *                        ipout,lout,itype,ierror_return)
            if(nnodes.gt.0) then
               call cmo_get_info(cvelnm,cmo,ipvels,ilen,ityp,ierr)
               if(ierr.ne.0) then
                 cbuff =     'cmo/addatt/-def-/vels/VDOUBLE/vector/' //
     *                  'nnodes/linear/permanent/gx/0.0 ;  ' //
     *                  'finish'
                 call dotask(cbuff,ier)
                 call cmo_get_info('vels',cmo,ipvels,ilen,ityp,ierr)
               endif
               read(iunit,*) (vels(1,i),i=1,nnodes)
               read(iunit,*) (vels(2,i),i=1,nnodes)
               read(iunit,*) (vels(3,i),i=1,nnodes)
            endif
         else
            call cmo_get_name(cmo,icscode)
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            if(nelements.gt.0) then
               length=nelements
               call mmgetblk('xarray',isubname,
     *                       ipxarray,length,2,icscode)
               read(iunit,*) (xarray(i),i=1,nelements)
               read(iunit,*) (xarray(i),i=1,nelements)
               read(iunit,*) (xarray(i),i=1,nelements)
               call mmrelblk('xarray',isubname,ipxarray,icscode)
            endif
         endif
         goto 100
C
      elseif(iline(1:8).eq.'variable') then
C
 110     continue
         read(iunit,'(a80)') iline
         if(iline(1:7).eq.'endvars') goto 100
         len1=icharlnf(iline)
         cmoattnam=iline(1:len1)
         iline=iline((len1+1):len(iline))
         read(iline,*) iflag
         if(iflag.eq.1) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
              ctype=' '
              call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *          clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  read(iunit,*) (iarray(i),i=1,nnodes)
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  read(iunit,*) (xarray(i),i=1,nnodes)
               endif
            else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *               ' ; finish '
               call dotaskx3d(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               read(iunit,*) (xarray(i),i=1,nnodes)
            endif
         elseif(iflag.eq.0) then
            call mmfindbk(cmoattnam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               ctype=' '
               call cmo_get_attparam(cmoattnam,cmo,index,ctype,crank,
     *          clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
                  read(iunit,*) (iarray(i),i=1,nelements)
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
                  read(iunit,*) (xarray(i),i=1,nelements)
               endif
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
     *               '/scalar/nelements/linear/permanent/gxaf/0.0' //
     *               ' ; finish '
               call dotaskx3d(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipiarray,lenout,icscode)
            call mmgetblk('xarray',isubname,ipxarray,length,2,icscode)
               read(iunit,*) (xarray(i),i=1,nelements)
               do i=1,nelements
                  iarray(i)=nint(xarray(i))
               enddo
            call mmrelblk('xarray',isubname,ipxarray,icscode)
C
C    END Special case for AMR variables
C
              else
               cbuff='cmo/addatt/' //
     *               cmo(1:icharlnf(cmo)) //
     *               '/' //
     *               cmoattnam(1:icharlnf(cmoattnam)) //
     *               '/VDOUBLE' //
     *               '/scalar/nelements/linear/permanent/gxaf/0.0' //
     *               ' ; finish '
               call dotaskx3d(cbuff,ierror)
               call mmfindbk(cmoattnam,cmo,ipxarray,lenout,icscode)
               read(iunit,*) (xarray(i),i=1,nelements)
              endif
            endif
         else
            length=nelements
            call mmgetblk('xarray',isubname,ipxarray,length,2,icscode)
            read(iunit,*) (xarray(i),i=1,nelements)
            call mmrelblk('xarray',isubname,ipxarray,icscode)
         endif
         goto 110
      elseif(iline(1:5).eq.'flags') then
C
 
 120     continue
         read(iunit,'(a80)') iline
         if(iline(1:7).eq.'endflag') goto 100
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) maxclrpoint,iflag
         icount=0
         dowhile(icount.lt.maxclrpoint)
            read(iunit,'(a80)') iline
            imore=0
            dowhile(imore.eq.0)
               icount=icount+1
               len1=icharlnf(iline)
               len2=icharlnb(iline)
               iword=iline(1:len1)
               if(ifprm.eq.0.and.nmregs.gt.0) then
                  len1=icharlnf(iline)
                  do j=1,nmregs
                   if(cmregs(j).eq.iword) matregs(j)=i
                  enddo
               endif
               if(len2.gt.len1) then
                  iline=iline((len1+1):len2)
               else
                  imore=1
               endif
            enddo
         enddo
         if(iflag.eq.1) then
            length=nnodes
            call mmgetblk('xarray',isubname,ipxarray,length,2,icscode)
            read(iunit,*) (xarray(i),i=1,nnodes)
            call mmrelblk('xarray',isubname,ipxarray,icscode)
         elseif(iflag.eq.0) then
            length=nelements
            call mmgetblk('xarray',isubname,ipxarray,length,2,icscode)
            read(iunit,*) (xarray(i),i=1,nelements)
            call mmrelblk('xarray',isubname,ipxarray,icscode)
         endif
C
         goto 120
      elseif(iline(1:8).eq.'material') then
         call mmfindbk('cmregs',cmo,ipcmregs,length,ierror)
         call mmfindbk('matregs',cmo,ipmatregs,length,ierror)
C
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) maxclrpoint,iflag
         icount=0
         dowhile(icount.lt.maxclrpoint)
            read(iunit,'(a80)') iline
            imore=0
            dowhile(imore.eq.0)
               icount=icount+1
               len1=icharlnf(iline)
               len2=icharlnb(iline)
               iword=iline(1:len1)
               if(ifprm.eq.0.and.nmregs.gt.0) then
                  do j=1,nmregs
                   if(cmregs(j).eq.iword) matregs(j)=i
                  enddo
               endif
               if(len2.gt.len1) then
                  iline=iline((len1+1):len2)
               else
                  imore=1
               endif
            enddo
         enddo
         if(iflag.eq.1) then
            call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
            if(nnodes.gt.0) then
               length=nnodes
               call mmgetblk('xarray',isubname,ipxarray,length,2,ics)
               read(iunit,*) (xarray(i),i=1,nnodes)
               call mmrelblk('xarray',isubname,ipxarray,icscode)
            endif
         elseif(iflag.eq.0) then
            call cmo_get_info('nelements',cmo,
     *                        nelements,ilen,itype,icscode)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
            if(nelements.gt.0) then
               read(iunit,*) (itetclr(i),i=1,nelements)
            endif
         endif
         goto 100
C
      elseif(iline(1:8).eq.'polygons') then
         len1=icharlnf(iline)
         dowhile(iline(1:len1).ne.'endpoly')
            read(iunit,'(a80)') iline
            len1=icharlnf(iline)
         enddo
         goto 100
C
      elseif(iline(1:7).eq.'cycleno') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) ihcycle
 
         call set_global('ihcycle',
     *                   ihcycle,0., ' ',1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
      elseif(iline(1:8).eq.'probtime') then
         len1=icharlnf(iline)
         iline=iline((len1+1):len(iline))
         read(iline,*) time
 
         call set_global('time',0,
     *                   time,' ',2,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_r')
      else
         write(logmess,9000) iline(1:icharlnf(iline))
         call writloga('default',1,logmess,0,ierr)
 9000    format('GMV input not recognized:  ',a)
         goto 100
      endif
C
      goto 9998
 9998 continue
      goto 9999
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

      write(logmess,91) ifile(1:icharlnf(ifile))
   91 format('Done reading GMV asciistar file: ',a)
      call writloga('default',0,logmess,1,ierror)

      return
      end
