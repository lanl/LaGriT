      subroutine readavs(ifile,
     *                   iopt_nodes,
     *                   iopt_elements,
     *                   iopt_values,
     *                   ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE READS AN AVS UCD TYPE FILE
C
C     INPUT ARGUMENTS -
C
C        ifile         - INPUT AVS FILE NAME. 
C        iopt_nodes    - OPTION FOR READING (=1) OR IGNORING (=0) THE
C                           NODES IN THE THE AVS FILE.
C        iopt_elements - OPTION FOR READING (=1) OR IGNORING (=0) THE
C                           ELEMENTS IN THE THE AVS FILE.
C        iopt_values   - OPTION FOR READING (=1) OR IGNORING (=0) THE
C                           NODE-VALUES IN THE THE AVS FILE.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - RETURN ERROR CODE (== 0 ==> OK, <> 0 ==> AN ERROR)
C
C      CHANGE HISTORY -
C$Log: readavs.f,v $
CRevision 2.00  2007/11/09 20:03:59  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.9   15 Mar 2006 08:56:04   gable
CPVCS    Fixed some cases where there is only some stuff in the AVS
CPVCS    file like nodes and node attribute but no element or element
CPVCS    attribute, etc. Also added the call to cmo/status at the
CPVCS    end. The option to call cmo/status/-cmo-/brief does not
CPVCS    work so right now the screen output is the brief info
CPVCS    for all existing MO's. If cmo/status is fixed the dotask at
CPVCS    the end can be changed.
CPVCS    
CPVCS       Rev 1.8   18 Oct 2005 16:04:00   gable
CPVCS    Extend input file name length to 132 characters.
CPVCS    Fix error when number of node attributes is zero
CPVCS    and number of element attributes is non-zero. Code
CPVCS    expects a zero (0) in the file where attributes
CPVCS    are read in but then continues on to element attributes.
CPVCS    
CPVCS       Rev 1.7   11 Sep 2001 13:04:24   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   24 Aug 2001 11:33:50   gable
CPVCS    Added ability to read in element attributes, real or integer.
CPVCS    Added error checking to insure first line of AVS file is
CPVCS    consistent with file descriptor information just above
CPVCS    attribute data.
CPVCS    
CPVCS       Rev 1.5   21 Apr 2000 07:07:42   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.4   07 Apr 2000 08:43:46   gable
CPVCS    Old version would eliminate elements with itetclr .le. 0
CPVCS    Changed it so only elements with negative itetclr are eliminated.
CPVCS    
CPVCS       Rev 1.4   07 Apr 2000 08:36:52   gable
CPVCS    Old version would eliminate elements with itetclr .le. 0
CPVCS    Changed it so only elements with negative itetclr are eliminated.
CPVCS    
CPVCS       Rev 1.3   Thu Mar 23 09:04:10 2000   dcg
CPVCS    check for type of existing attribute when reading in field values
CPVCS    
CPVCS       Rev 1.2   17 Feb 2000 17:37:26   jtg
CPVCS    entire if (iseticr1.eq.0) ... endif removed as the critical lines
CPVCS    setting icr1 are long since commented out.
CPVCS    
CPVCS       Rev 1.1   Thu Feb 17 10:27:36 2000   dcg
CPVCS    check for no elements on file
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 13:46:12 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.49   Thu Nov 11 17:46:44 1999   gable
CPVCS    No change.
CPVCS
CPVCS       Rev 1.48   Thu Nov 11 17:43:50 1999   gable
CPVCS    Make changes to allow node numbering to be zero
CPVCS    based rather than one based.
CPVCS
CPVCS       Rev 1.47   Fri Jul 23 14:07:04 1999   dcg
CPVCS    implicit none
CPVCS    use pointer arrays for integer*8 pointer arrays
CPVCS
CPVCS       Rev 1.46   Thu Jul 01 02:33:40 1999   jtg
CPVCS    changes made so nonexistant cmo, illegal element types
CPVCS    do not crash code. Illegal element types are ignored,
CPVCS    nonexistant cmo return with error message.
CPVCS
CPVCS       Rev 1.45   Wed Jun 30 15:39:38 1999   jtg
CPVCS    fixed pyramid back: Rev 1.44 was incorrect
CPVCS
CPVCS       Rev 1.44   Thu Jun 17 11:33:32 1999   jtg
CPVCS    !NO! order of nodes for pyr made !NOT! consistent with blockcom.f
CPVCS
CPVCS       Rev 1.43   Thu Jun 17 09:30:58 1999   dcg
CPVCS    fix bug when reading pyramid elements - removed unneeded line
CPVCS
CPVCS       Rev 1.42   Sun Jun 07 12:11:00 1998   gable
CPVCS    Fixed bug in read pt and line element type.
CPVCS
CPVCS       Rev 1.41   Tue Apr 14 11:10:06 1998   kmb
CPVCS    changed attributes to use integer, real types.
CPVCS
CPVCS       Rev 1.40   Thu Jul 03 09:58:44 1997   dcg
CPVCS    skip loop that unless type is 'tet' -invalid for
CPVCS    other element types
CPVCS
CPVCS       Rev 1.39   Mon Apr 14 16:57:24 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.38   Thu Mar 06 21:47:34 1997   het
CPVCS    Fix an error with "nef".
CPVCS
CPVCS       Rev 1.37   Tue Dec 03 12:59:48 1996   het
CPVCS    Change the integer int() function to nint().
CPVCS
CPVCS       Rev 1.36   Wed Nov 13 13:15:44 1996   dcg
CPVCS    add change history field
C
C
C
C#######################################################################
C
      implicit none
      pointer (ipisdatptr,isdatptr)
C  32 bit address declaration
      integer isdatptr(*)
C  64 bit address declaration
C      integer*8 isdatptr(*)
      pointer (ipisdatptr_e,isdatptr_e)
C  32 bit address declaration
      integer isdatptr_e(*)
C  64 bit address declaration
C      integer*8 isdatptr_e(*)
c      include 'pointer_arrays.h'
C
      character*132 logmess
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include "chydro.h"
      include "local_element.h"
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000)
      integer itet1(10000000), jtet1(10000000)
      real*8   xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipireal1 , ireal1  )
      integer ireal1(1000000)
C
      pointer (ipidone, idone)
      integer idone(1000000)
C
C ######################################################################
C
      character*(*) ifile
      character*8 itype
      character*32 isubname, cmonam, cmoatt, cmotype
      character*132 cline, cbuff,ctype
C
C     Node based arrays
C
      pointer (ipout,data)
      real*8 data(*)
      pointer (ipiarray, iarray)
      pointer (ipxarray, xarray)
      integer iarray(1000000)
      real*8 xarray(1000000)
      pointer (ipivalues, ivalues)
      integer ivalues(1000000)
      pointer (ipxvalues, xvalues)
      real*8 xvalues(1000000)
      pointer (ipiatt_type, iatt_type)
      integer iatt_type(1000000)
C
C     Element based arrays
C
      pointer (ipiarray_e, iarray_e)
      pointer (ipxarray_e, xarray_e)
      integer iarray_e(1000000)
      real*8 xarray_e(1000000)
      pointer (ipivalues_e, ivalues_e)
      integer ivalues_e(1000000)
      pointer (ipxvalues_e, xvalues_e)
      real*8 xvalues_e(1000000)
      pointer (ipiatt_type_e, iatt_type_e)
      integer iatt_type_e(1000000)
C      
      integer icharlnf
C$$      integer ismax,ismin
C
C$$      real*8 xref(6),yref(6),zref(6)
C$$     * ,volface(6)
      integer nnodes,ilen,ity,ier,mbndry,isetimt1,
     * isetitp1,iseticr1,isetisn1,npoints_start,ihybrid,
     * ifirst_line,icount,ipoints,ielements,itoff,jtoff,
     * lenfile,jf,jt,node1,imt1max,
     * imt1min,ih,numtet1,numtet,j,ic,len1,i,
     * nvalues1,length,i1,i2,i3, i4,i5,i6,i7,i8,ifelmnew,
     * istart,ifound,imat,it,nen,nef,nee,nsdgeom,nsdtopo,
     * nsd,nelements,iline,i1max,i1min,nvalues,npoints,
     * nelemavs,ierr1,ierr,iopt_values,iopt_elements,
     * iopt_nodes,ierrdum,itetshift,icscode,lenout,
     * ierror,itp,itypeout
      integer nelements_start, icount_e, nvalues_e, nvalues1_e
C$$     * ,if2,if3,if4,j1,jndex,index
      integer if_zero_based, i_offset
    
      integer iunit
      integer*4 iunit4

      real*8 x1,y1,z1
C$$     * ,ymin1,ymax1,xmax1,xmin1,zmax1,zmin1,xdiff,xavg
C$$     * ,dx,x2,y2,z2,x3,y3,z3,x4,y4,z4,dy,dz,zavg,zdiff,yavg,ydiff
C#######################################################################
C
C
C     GET CMO NAME
C        (IF CMO DOES NOT EXIST, BAIL OUT)
C
      isubname='readavs'
      call cmo_get_name(cmonam,ierror)
      if (ierror.ne.0 .or. cmonam(1:8).eq.'-notset-') then
         lenfile=icharlnf(cmonam)
         write(logmess,9001) cmonam(1:lenfile)
 9001    format('The target mesh , ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierr)
         ! call x3d_error(isubname,'AVS READ FAILED')
         goto 9999
      endif
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,ity,ier)
      call cmo_get_info('nelements',cmonam,nelements,ilen,ity,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,ity,ier)
C
C     ******************************************************************
C
C
      isetimt1=0
      isetitp1=0
      iseticr1=0
      isetisn1=0
C
      npoints_start=nnodes
      nelements_start=nelements
C
      ihybrid=0
      ifirst_line=0
      icount=0
      icount_e=0
      ipoints=npoints_start
      ielements=0
      itoff=0
      jtoff=0
C
C
C     ******************************************************************
C
C     CHECK TO SEE IF THE AVS FILE EXISTS, IF NOT THEN BAIL OUT.
C
      lenfile=icharlnf(ifile)
      call fexist(ifile(1:lenfile),ierr1)
      if(ierr1.eq.0) then
         write(logmess,9000) ifile(1:lenfile)
 9000    format('The AVS file, ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierr)
         goto 9999
      endif
C
C
C     ******************************************************************
C
C     ASSIGN THE FILE TO THE NEXT AVAILABLE LOGICAL UNIT NUMBER.
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (ierror.ne.0 .or. iunit.lt.0) then
         write(logmess,*) 'hassign bad file no for '
     &         //ifile(1:lenfile)
         call writloga('default',1,logmess,0,ierr)
         goto 9999
      endif

C
C
C     ******************************************************************
C
C
 100  continue
      icount=icount+1
      iunit4 = iunit
      read(iunit4,'(a132)') cline
      if(cline(1:3).eq.'run') goto 9998
      if(cline(1:6).eq.'finish') goto 9998
      if(cline(1:1).eq.'#') goto 100
      if(ifirst_line.eq.0) then
         ifirst_line=icount
         read(cline,*) npoints,nelemavs,nvalues,nvalues_e
         i1min=npoints
         i1max=0
         goto 100
      elseif(ipoints.lt.npoints) then
 
C read in the AVS points
 
c
c     Some AVS files use zero based arrays. This code assumes the
c     nodes are in order, there are no holes in the list, and the
c     list starts with entry 1. Put in code to deal with the case
c     where the list starts with entry 0.
c
      if_zero_based = -1
         if(npoints.gt.0) then
            do iline=1,npoints
               read(cline,*) i1,x1,y1,z1
               if(if_zero_based .lt. 0)then
c
c              Check value of i1 on first pass through this loop
c
               if_zero_based = i1
               if(if_zero_based .ge. 2)then
                  write(logmess,9000) if_zero_based
 9003             format
     1   ('ERROR: The AVS files first node number is, ',i10)
                  call writloga('default',1,logmess,0,ierr)
 9004             format
     1   ('ERROR:Only 0 or 1 based sequential node numbering supported')
                  call writloga('default',1,logmess,0,ierr)
                  goto 9999
               else
                  if(if_zero_based .eq. 0)then
                     i_offset = 1
                  else
                     i_offset = 0
                  endif
               endif
               endif
 
 
 
               ! this code assumes i1(ipoint)=i1min:i1min+npoints-1
               ! and only uses i1 info to set itetshift
               i1min=min(i1min,i1+i_offset)
               i1max=max(i1max,i1+i_offset)
               ipoints=ipoints+1
               if(ipoints.eq.1) then
                  npoints=npoints_start+npoints
                  call cmo_set_info('nnodes',cmonam,npoints,1,1,ierror)
                  call cmo_newlen(cmonam,ierror)
                  call cmo_get_info('imt1',cmonam,
     *                              ipimt1,ilen,itp,ier)
                  call cmo_get_info('itp1',cmonam,
     *                              ipitp1,ilen,itp,ier)
                  call cmo_get_info('icr1',cmonam,
     *                              ipicr1,ilen,itp,ier)
                  call cmo_get_info('xic',cmonam,
     *                              ipxic,ilen,itp,ierror)
                  call cmo_get_info('yic',cmonam,
     *                              ipyic,ilen,itp,ierror)
                  call cmo_get_info('zic',cmonam,
     *                              ipzic,ilen,itp,ierror)
               endif
               imt1(ipoints)=0
               itp1(ipoints)=0
               xic(ipoints)=x1
               yic(ipoints)=y1
               zic(ipoints)=z1
               if(npoints.gt.1.and.ipoints.lt.npoints) then
 101              continue
                  icount=icount+1
                  read(iunit,'(a132)') cline
                  if(cline(1:1).eq.'#') goto 101
               endif
            enddo
         endif
         if(iopt_nodes.eq.0) then
            npoints=0
         endif
 
C read in the AVS elements
 
         if(nelemavs.gt.0) then
 200        continue
               read(iunit,'(a132)') cline
               if(cline(1:1).eq.'#') goto 200
            if (iopt_elements.eq.1) then
 
C set defaults in case read in only bad elements
 
               nnodes=npoints
               nelements=nelemavs
               call cmo_set_info('nnodes',cmonam,
     *                            nnodes,1,1,ierror)
               call cmo_set_info('nelements',cmonam,
     *                            nelements,1,1,ierror)
               cmotype='tet'
               nsd=3
               nsdgeom=3
               nsdtopo=3
               nen=nelmnen(ifelmtet)
               nef=nelmnef(ifelmtet)
               nee=nelmnee(ifelmtet)
               call cmo_set_info('ndimensions_geom',cmonam,
     *                            nsdgeom,1,1,ierror)
               call cmo_set_info('ndimensions_topo',cmonam,
     *                            nsdtopo,1,1,ierror)
               call cmo_set_info('nodes_per_element',cmonam,
     *                            nen,1,1,ierror)
               call cmo_set_info('faces_per_element',cmonam,
     *                            nef,1,1,ierror)
               call cmo_set_info('edges_per_element',cmonam,
     *                            nee,1,1,ierror)
               call cmo_newlen(cmonam,ierror)
               call cmo_get_info('itetclr',cmonam,
     *                            ipitetclr,ilen,itp,ier)
               call cmo_get_info('itettyp',cmonam,
     *                            ipitettyp,ilen,itp,ier)
               call cmo_get_info('itetoff',cmonam,
     *                            ipitetoff,ilen,itp,ier)
               call cmo_get_info('jtetoff',cmonam,
     *                            ipjtetoff,ilen,itp,ier)
               call cmo_get_info('itet',cmonam,
     *                            ipitet,ilen,itp,ier)
               call cmo_get_info('jtet',cmonam,
     *                            ipjtet,ilen,itp,ier)
            endif
            do iline=1,nelemavs
               ielements=ielements+1
               if(iopt_elements.eq.1) then
                  read(cline,*) it,imat
                  it = it + i_offset
 
C     find the AVS element type
 
                  ifound=0
                  itype='BAD'
                  istart=1
                  dowhile(istart.lt.len(cline).and.ifound.eq.0)
                     if (cline(istart:istart+2).eq.'pt') then
                        ifound=1
                        itype=cline(istart:istart+2)
                        cline=cline(istart+3:len(cline))
                     elseif (cline(istart:istart+4).eq.'line') then
                        ifound=2
                        itype=cline(istart:istart+4)
                        cline=cline(istart+5:len(cline))
                     elseif (cline(istart:istart+3).eq.'tri') then
                        ifound=3
                        itype=cline(istart:istart+3)
                        cline=cline(istart+4:len(cline))
                     elseif (cline(istart:istart+4).eq.'quad') then
                        ifound=4
                        itype=cline(istart:istart+4)
                        cline=cline(istart+5:len(cline))
                     elseif (cline(istart:istart+3).eq.'tet') then
                        ifound=5
                        itype=cline(istart:istart+3)
                        cline=cline(istart+4:len(cline))
                     elseif (cline(istart:istart+3).eq.'pyr') then
                        ifound=6
                        itype=cline(istart:istart+3)
                        cline=cline(istart+4:len(cline))
                     elseif (cline(istart:istart+5).eq.'prism') then
                        ifound=7
                        itype=cline(istart:istart+5)
                        cline=cline(istart+6:len(cline))
                     elseif (cline(istart:istart+3).eq.'hex') then
                        ifound=8
                        itype=cline(istart:istart+3)
                        cline=cline(istart+4:len(cline))
                     else
                        istart=istart+1
                     endif
                  enddo
                  if (ifound.eq.0) then
                      ielements=ielements-1
                      nelements=nelements-1
                      goto 103
                  endif
 
C     initialize lagrit mesh element info
 
                  if (ielements.eq.1) then
 
                     if(itype(1:2).eq.'pt') then
                        cmotype='pnt'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=1
                        nen=nelmnen(ifelmpnt)
                        nef=nelmnef(ifelmpnt)
                        nee=nelmnee(ifelmpnt)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     elseif (itype(1:4).eq.'line') then
                        cmotype='lin'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=1
                        nen=nelmnen(ifelmlin)
                        nef=nelmnef(ifelmlin)
                        nee=nelmnee(ifelmlin)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     else if(itype(1:3).eq.'tri') then
                        cmotype='tri'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=2
                        nen=nelmnen(ifelmtri)
                        nef=nelmnef(ifelmtri)
                        nee=nelmnee(ifelmtri)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     elseif (itype(1:4).eq.'quad') then
                        cmotype='quad'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=2
                        nen=nelmnen(ifelmqud)
                        nef=nelmnef(ifelmqud)
                        nee=nelmnee(ifelmqud)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     elseif (itype(1:3).eq.'tet') then
                        cmotype='tet'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmtet)
                        nef=nelmnef(ifelmtet)
                        nee=nelmnee(ifelmtet)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     elseif (itype(1:3).eq.'pyr') then
                        cmotype='pyr'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmpyr)
                        nef=nelmnef(ifelmpyr)
                        nee=nelmnee(ifelmpyr)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
 
                     elseif(itype(1:5).eq.'prism') then
                        cmotype='prism'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmpri)
                        nef=nelmnef(ifelmpri)
                        nee=nelmnee(ifelmpri)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
                     elseif(itype(1:3).eq.'hex') then
                        cmotype='hex'
                        nsd=3
                        nsdgeom=3
                        nsdtopo=3
                        nen=nelmnen(ifelmhex)
                        nef=nelmnef(ifelmhex)
                        nee=nelmnee(ifelmhex)
                        call cmo_set_info('ndimensions_geom',cmonam,
     *                                    nsdgeom,1,1,ierror)
                        call cmo_set_info('ndimensions_topo',cmonam,
     *                                    nsdtopo,1,1,ierror)
                        call cmo_set_info('nodes_per_element',cmonam,
     *                                    nen,1,1,ierror)
                        call cmo_set_info('faces_per_element',cmonam,
     *                                    nef,1,1,ierror)
                        call cmo_set_info('edges_per_element',cmonam,
     *                                    nee,1,1,ierror)
                        call cmo_newlen(cmonam,ierror)
                        call cmo_get_info('itetclr',cmonam,
     *                                    ipitetclr,ilen,itp,ier)
                        call cmo_get_info('itettyp',cmonam,
     *                                    ipitettyp,ilen,itp,ier)
                        call cmo_get_info('itetoff',cmonam,
     *                                    ipitetoff,ilen,itp,ier)
                        call cmo_get_info('jtetoff',cmonam,
     *                                    ipjtetoff,ilen,itp,ier)
                        call cmo_get_info('itet',cmonam,
     *                                    ipitet,ilen,itp,ier)
                        call cmo_get_info('jtet',cmonam,
     *                                    ipjtet,ilen,itp,ier)
                     endif
                  endif
 
C     set the itettyp for the current element
 
                  if(itype(1:2).eq.'pt') then
                     ifelmnew=ifelmpnt
                  elseif(itype(1:4).eq.'line') then
                     ifelmnew=ifelmlin
                  elseif(itype(1:3).eq.'tri') then
                     ifelmnew=ifelmtri
                  elseif(itype(1:4).eq.'quad') then
                     ifelmnew=ifelmqud
                  elseif(itype(1:3).eq.'tet') then
                     ifelmnew=ifelmtet
                  elseif(itype(1:3).eq.'pyr') then
                     ifelmnew=ifelmpyr
                  elseif(itype(1:3).eq.'pri') then
                     ifelmnew=ifelmpri
                  elseif(itype(1:3).eq.'hex') then
                     ifelmnew=ifelmhex
                  endif
                  if(ihybrid.eq.0 .and. ifound.ne.0 .and.
     *               (nelmnen(ifelmnew).ne.nen .or.
     *                nelmnef(ifelmnew).ne.nef)) then
                     ihybrid=1
                     nen=nelmnen(ifelmhyb)
                     nef=nelmnef(ifelmhyb)
                     nee=nelmnee(ifelmhyb)
                     call cmo_set_info('nodes_per_element',cmonam,
     *                                 nen,1,1,ierror)
                     call cmo_set_info('faces_per_element',cmonam,
     *                                 nef,1,1,ierror)
                     call cmo_set_info('edges_per_element',cmonam,
     *                                 nee,1,1,ierror)
                     call cmo_newlen(cmonam,ierror)
                     call cmo_get_info('itetclr',cmonam,
     *                                 ipitetclr,ilen,itp,ier)
                     call cmo_get_info('itettyp',cmonam,
     *                                 ipitettyp,ilen,itp,ier)
                     call cmo_get_info('itetoff',cmonam,
     *                                 ipitetoff,ilen,itp,ier)
                     call cmo_get_info('jtetoff',cmonam,
     *                                 ipjtetoff,ilen,itp,ier)
                     call cmo_get_info('itet',cmonam,
     *                                 ipitet,ilen,itp,ier)
                     call cmo_get_info('jtet',cmonam,
     *                                 ipjtet,ilen,itp,ier)
                  endif
 
C     set the itet relation for the current element
 
103               if(itype(1:2).eq.'pt') then
c         'pnt': note avs order  = lagrit order
                     read(cline,*)  i1
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmpnt
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i1+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
 
                  elseif(itype(1:4).eq.'line') then
c         'lin': note avs order  = lagrit order
                     read(cline,*)  i1,i2
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmlin
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i1+i_offset
                     itet1(itetoff(ielements)+2)=i2+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
 
                  elseif(itype(1:3).eq.'tri') then
c         'tri': note avs order  = lagrit order
                     read(cline,*)  i1,i2,i3
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmtri
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i1+i_offset
                     itet1(itetoff(ielements)+2)=i2+i_offset
                     itet1(itetoff(ielements)+3)=i3+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
 
                  elseif(itype(1:4).eq.'quad') then
c         'qud': note avs order  = lagrit order
                     read(cline,*)  i1,i2,i3,i4
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmqud
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i1+i_offset
                     itet1(itetoff(ielements)+2)=i2+i_offset
                     itet1(itetoff(ielements)+3)=i3+i_offset
                     itet1(itetoff(ielements)+4)=i4+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
                     jtet1(jtetoff(ielements)+4)=-1
 
                  elseif(itype(1:3).eq.'tet') then
c         'tet': note avs order  1 2 4 3 = lagrit order 1 2 3 4
                     read(cline,*)  i1,i2,i4,i3
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmtet
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i1+i_offset
                     itet1(itetoff(ielements)+2)=i2+i_offset
                     itet1(itetoff(ielements)+3)=i3+i_offset
                     itet1(itetoff(ielements)+4)=i4+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
                     jtet1(jtetoff(ielements)+4)=-1
 
                  elseif(itype(1:3).eq.'pyr') then
c         'pyr': note avs order  1 2 3 4 5 = lagrit order 2 3 4 5 1
                     read(cline,*)  i1,i2,i3,i4,i5
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmpyr
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i2+i_offset
                     itet1(itetoff(ielements)+2)=i3+i_offset
                     itet1(itetoff(ielements)+3)=i4+i_offset
                     itet1(itetoff(ielements)+4)=i5+i_offset
                     itet1(itetoff(ielements)+5)=i1+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
                     jtet1(jtetoff(ielements)+4)=-1
                     jtet1(jtetoff(ielements)+5)=-1
 
                  elseif(itype(1:5).eq.'prism') then
c         'pri': note avs order  1 2 3 4 5 6 = lagrit order 4 5 6 1 2 3
                     read(cline,*)  i1,i2,i3,i4,i5,i6
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmpri
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i4+i_offset
                     itet1(itetoff(ielements)+2)=i5+i_offset
                     itet1(itetoff(ielements)+3)=i6+i_offset
                     itet1(itetoff(ielements)+4)=i1+i_offset
                     itet1(itetoff(ielements)+5)=i2+i_offset
                     itet1(itetoff(ielements)+6)=i3+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
                     jtet1(jtetoff(ielements)+4)=-1
                     jtet1(jtetoff(ielements)+5)=-1
 
                  elseif(itype(1:3).eq.'hex') then
c         'hex': note avs order  1 2 3 4 5 6 7 8 = lagrit order 5 6 7 8 1 2 3 4
                     read(cline,*)  i1,i2,i3,i4,i5,i6,i7,i8
                     itetclr(ielements)=imat
                     itettyp(ielements)=ifelmhex
                     itetoff(ielements)=itoff
                     jtetoff(ielements)=jtoff
                     itoff=itoff+nelmnen(itettyp(ielements))
                     jtoff=jtoff+nelmnef(itettyp(ielements))
                     itet1(itetoff(ielements)+1)=i5+i_offset
                     itet1(itetoff(ielements)+2)=i6+i_offset
                     itet1(itetoff(ielements)+3)=i7+i_offset
                     itet1(itetoff(ielements)+4)=i8+i_offset
                     itet1(itetoff(ielements)+5)=i1+i_offset
                     itet1(itetoff(ielements)+6)=i2+i_offset
                     itet1(itetoff(ielements)+7)=i3+i_offset
                     itet1(itetoff(ielements)+8)=i4+i_offset
                     jtet1(jtetoff(ielements)+1)=-1
                     jtet1(jtetoff(ielements)+2)=-1
                     jtet1(jtetoff(ielements)+3)=-1
                     jtet1(jtetoff(ielements)+4)=-1
                     jtet1(jtetoff(ielements)+5)=-1
                     jtet1(jtetoff(ielements)+6)=-1
 
                  else
c         itype unsupported
                     print *,"Bad element type: ",iline,'  ',itype
                  endif
 
               endif
 
c     get the next line
 
               if(nelemavs.gt.1.and.iline.lt.nelemavs) then
 102              continue
                  icount=icount+1
                  read(iunit,'(a132)') cline
                  if(cline(1:1).eq.'#') goto 102
               endif
 
            enddo
         endif
         if(nelemavs.eq.0) nelements=0
         if(iopt_elements.eq.0) then
            nelemavs=0
         elseif(iopt_elements.eq.1) then
            nelemavs=nelements
         endif
 
c read in the field values
         print *, nvalues, nvalues_e 
         if((nvalues.le.0) .and. (nvalues_e.le.0)) then
C
C     All done, there are not node attributes or element attributes.
C

            go to 9998
         elseif((nvalues.le.0) .and. (nvalues_e.gt.0)) then
C
C     In this case there are no node attributes but element attributes will follow.
C     Still expect a zero in the file here.
C
            read(iunit,*) nvalues1
            if ((nvalues1 .ne. 0).and.(nvalues1 .eq. nvalues_e)) then
            write(logmess,290) 
            call writloga('default',0,logmess,0,ierr)
            write(logmess,291) 
            call writloga('default',0,logmess,0,ierr)
            write(logmess,292) 
            call writloga('default',0,logmess,0,ierr)
 290        format('READAVS WARNING: File header says nnode_att = 0')
 291        format
     1   ('READAVS WARNING: 0 not read after element connectivity')
 292        format
     1   ('READAVS WARNING: Assume nelement_att is in this position')
C           Make a guess that this was really nvalues1_e
               backspace(iunit)
            endif
C           and now jump forward to the place where element attribute info is read.
C
         elseif(nvalues.gt.0) then
            length=nvalues
            call mmgetblk('ivalues',isubname,ipivalues,length,2,icscode)
            call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
            call mmgetblk('isdatptr',isubname,
     *                    ipisdatptr,length,2,icscode)
            call mmgetblk('iatt_type',isubname,
     *                    ipiatt_type,length,2,icscode)
 300        continue
               read(iunit,'(a132)') cline
               if(cline(1:1).eq.'#') goto 300
            backspace(iunit)
            read(iunit,*) nvalues1,(ivalues(i),i=1,nvalues1)
            if(nvalues1.gt.0) then
            if(nvalues1 .ne. nvalues)then
            write(logmess,305) nvalues
 305        format('READAVS ERROR: File header says ',
     1              i10,' node attributes')
            call writloga('default',1,logmess,0,ierr)
            write(logmess,306) nvalues1
 306        format('READAVS ERROR: Data header says ',
     1              i10,' node attributes')
            call writloga('default',1,logmess,0,ierr)
            goto 9999
            endif
               do i=1,nvalues1
                  icount=icount+1
 310              continue
                     read(iunit,'(a132)') cline
                     if(cline(1:1).eq.'#') goto 310
                  len1=min(len(cline),icharlnf(cline))
                  ic=1
                  dowhile(ic.le.len1.and.cline(ic:ic).ne.',')
                     ic=ic+1
                  enddo
                  ic=ic-1
                  cmoatt=cline(1:ic)
                  ctype=cline(ic+2:ic+9)
                  if(cmoatt(1:ic).eq.'xmt1') then
                     isetimt1=1
                     cmoatt='imt1'
                  elseif(cmoatt(1:ic).eq.'imt1') then
                     isetimt1=1
                  elseif(cmoatt(1:ic).eq.'itp1') then
                     isetitp1=1
                  elseif(cmoatt(1:ic).eq.'icr1') then
                     iseticr1=1
                  elseif(cmoatt(1:ic).eq.'isn1') then
                     isetisn1=1
                  endif
                  call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                  if(icscode.eq.0) then
                     call mmgettyp(ipout,itypeout,icscode)
                     if (itypeout.eq.1) then
                        isdatptr(i)=ipout
                        iatt_type(i)=1
                     elseif(itypeout.eq.2) then
                        isdatptr(i)=ipout
                        iatt_type(i)=2 
                     else
                        write(logmess,'(a,a)')'Illegal attribute type ',
     *                       cmoatt(1:icharlnf(cmoatt))
                        call writloga('default',0,logmess,0,icscode)
                        go to 9998
                     endif                  
                  else
                    if(ctype(1:icharlnf(ctype)).eq.' integer') then
                        cbuff='cmo/addatt/' //
     *                     cmonam(1:icharlnf(cmonam)) //
     *                     '/' //
     *                     cmoatt(1:icharlnf(cmoatt)) //
     *                     '/VINT' //
     *                     '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *                     ' ; finish '
                     call dotaskx3d(cbuff,ierror)
                     call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                     isdatptr(i)=ipout
                     iatt_type(i)=1
                    else
                     cbuff='cmo/addatt/' //
     *                     cmonam(1:icharlnf(cmonam)) //
     *                     '/' //
     *                     cmoatt(1:icharlnf(cmoatt)) //
     *                     '/VDOUBLE' //
     *                     '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *                     ' ; finish '
                     call dotaskx3d(cbuff,ierror)
                     call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                     isdatptr(i)=ipout
                     iatt_type(i)=2
                     endif
                  endif
               enddo
               do i=npoints_start+1,npoints
                  icount=icount+1
 320              continue
                     read(iunit,'(a132)') cline
                     if(cline(1:1).eq.'#') goto 320
                  backspace(iunit)
                  read(iunit,*) i1,(xvalues(j),j=1,nvalues)
                  do j=1,nvalues
                     if(iatt_type(j).eq.1) then
                        ipiarray=isdatptr(j)
                        iarray(i)=nint(xvalues(j))
                     else
                        ipxarray=isdatptr(j)
                        xarray(i)=xvalues(j)
                     endif
                  enddo
               enddo
            endif
         endif
         if(iopt_values.eq.0) then
            nvalues=0
         endif
      endif
C
C ******* End Reading Node Attributes ***********
C
C ******* Begin Reading Element Attributes ***********
C 
         if(nvalues_e.le.0) then
C           Then we are done.
            go to 9998
C
         elseif(nvalues_e.gt.0) then
            length=nvalues_e
            call mmgetblk('ivalues_e',isubname,
     *                    ipivalues_e,length,2,icscode)
            call mmgetblk('xvalues_e',isubname,
     *                    ipxvalues_e,length,2,icscode)
            call mmgetblk('isdatptr_e',isubname,
     *                    ipisdatptr_e,length,2,icscode)
            call mmgetblk('iatt_type_e',isubname,
     *                    ipiatt_type_e,length,2,icscode)
 400        continue
               read(iunit,'(a132)') cline
               if(cline(1:1).eq.'#') goto 400
            backspace(iunit)
            read(iunit,*) nvalues1_e,(ivalues_e(i),i=1,nvalues1_e)
            if(nvalues1_e .ne. nvalues_e)then
            write(logmess,405) nvalues_e
 405        format('READAVS ERROR: File header says ',
     1              i10,' element attributes')
            call writloga('default',1,logmess,0,ierr)
            write(logmess,406) nvalues1_e
 406        format('READAVS ERROR: Data header says ',
     1              i10,' element attributes')
            call writloga('default',1,logmess,0,ierr)
            goto 9999
            endif
            if(nvalues1_e.gt.0) then
               do i=1,nvalues1_e
                  icount_e=icount_e+1
 410              continue
                     read(iunit,'(a132)') cline
                     if(cline(1:1).eq.'#') goto 410
                  len1=min(len(cline),icharlnf(cline))
                  ic=1
                  dowhile(ic.le.len1.and.cline(ic:ic).ne.',')
                     ic=ic+1
                  enddo
                  ic=ic-1
                  cmoatt=cline(1:ic)
                  ctype=cline(ic+2:ic+9)
                  call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                  if(icscode.eq.0) then
                     call mmgettyp(ipout,itypeout,icscode)
                     if (itypeout.eq.1) then
                        isdatptr_e(i)=ipout
                        iatt_type_e(i)=1
                     elseif(itypeout.eq.2) then
                        isdatptr_e(i)=ipout
                        iatt_type_e(i)=2 
                     else
                        write(logmess,'(a,a)')'Illegal attribute type ',
     *                       cmoatt(1:icharlnf(cmoatt))
                        call writloga('default',0,logmess,0,icscode)
                        go to 9998
                     endif                  
                  else
                    if(ctype(1:icharlnf(ctype)).eq.' integer') then
                        cbuff='cmo/addatt/' //
     *                     cmonam(1:icharlnf(cmonam)) //
     *                     '/' //
     *                     cmoatt(1:icharlnf(cmoatt)) //
     *                     '/VINT' //
     *                 '/scalar/nelements/linear/permanent/gxaf/0.0' //
     *                     ' ; finish '
                     call dotaskx3d(cbuff,ierror)
                     call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                     isdatptr_e(i)=ipout
                     iatt_type_e(i)=1
                    else
                     cbuff='cmo/addatt/' //
     *                     cmonam(1:icharlnf(cmonam)) //
     *                     '/' //
     *                     cmoatt(1:icharlnf(cmoatt)) //
     *                     '/VDOUBLE' //
     *                 '/scalar/nelements/linear/permanent/gxaf/0.0' //
     *                     ' ; finish '
                     call dotaskx3d(cbuff,ierror)
                     call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
                     isdatptr_e(i)=ipout
                     iatt_type_e(i)=2
                     endif
                  endif
               enddo
               do i=nelements_start+1,nelemavs
                  icount_e=icount_e+1
 420              continue
                     read(iunit,'(a132)') cline
                     if(cline(1:1).eq.'#') goto 420
                  backspace(iunit)
                  read(iunit,*) i1,(xvalues_e(j),j=1,nvalues_e)
                  do j=1,nvalues_e
                     if(iatt_type_e(j).eq.1) then
                        ipiarray_e=isdatptr_e(j)
                        iarray_e(i)=nint(xvalues_e(j))
                     else
                        ipxarray_e=isdatptr_e(j)
                        xarray_e(i)=xvalues_e(j)
                     endif
                  enddo
               enddo
            endif
         endif

C ******* End Reading Element Attributes ***********
 9998 continue
C
      close(iunit)
C
      call cmo_get_info('imt1',cmonam,ipimt1,ilen,itp,ier)
      call cmo_get_info('itp1',cmonam,ipitp1,ilen,itp,ier)
      call cmo_get_info('icr1',cmonam,ipicr1,ilen,itp,ier)
      call cmo_get_info('isn1',cmonam,ipisn1,ilen,itp,ier)
      call cmo_get_info('xic',cmonam,ipxic,ilen,itp,ierror)
      call cmo_get_info('yic',cmonam,ipyic,ilen,itp,ierror)
      call cmo_get_info('zic',cmonam,ipzic,ilen,itp,ierror)
C
      if(nelemavs.le.0) then
         numtet=nelemavs
      else
         if(i1min.ne.1) then
            itetshift=1-i1min
         else
            itetshift=0
         endif
         itoff=0
         jtoff=0
         numtet1=0
         do ih=1,nelemavs
            if(itetclr(ih).ge.0) then
               numtet1=numtet1+1
               itetclr(numtet1)=itetclr(ih)
               itettyp(numtet1)=itettyp(ih)
               itetoff(numtet1)=itoff
               jtetoff(numtet1)=jtoff
               itoff=itoff+nelmnen(itettyp(numtet1))
               jtoff=jtoff+nelmnef(itettyp(numtet1))
               do i=1,nelmnen(itettyp(ih))
                  itet1(itetoff(numtet1)+i)=itet1(itetoff(ih)+i) +
     *                                      itetshift
               enddo
            endif
         enddo
         nelemavs=numtet1
         numtet=nelemavs
C
         if(isetimt1.eq.0) then
            imt1min=99999999
            imt1max=0
            do it=1,numtet
               imt1min=min(imt1min,itetclr(it))
               imt1max=max(imt1max,itetclr(it))
            enddo
            if(imt1min.lt.0) then
               imt1min=1-imt1min
            else
               imt1min=0
            endif
            do it=1,numtet
               itetclr(it)=itetclr(it)+imt1min
               do i=1,nelmnen(itettyp(it))
                  imt1(itet1(itetoff(it)+i))=itetclr(it)
               enddo
            enddo
            do i1=1,npoints
               if(imt1(i1).le.0) imt1(i1)=imt1max+1
            enddo
         endif
C
         call cmo_get_name(cmonam,ierror)
         call cmo_set_info('nnodes',cmonam,npoints,1,1,ier)
         call cmo_set_info('nelements',cmonam,numtet,1,1,ier)
         call set_mbndry()
         call cmo_get_intinfo('mbndry',cmonam,mbndry,
     *                  ilen,itp,ierror)
C
C
C        Fill the jtetoff array
C
         call set_jtetoff()
C
         cbuff='geniee ; finish'
         call dotaskx3d(cbuff,ierror)
C
C        ...............................................................
C        SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
         if(isetitp1.eq.0) then
C
            length=npoints
            call mmgetblk('idone',isubname,ipidone,length,2,icscode)
C
            do i=1,npoints
               idone(i)=0
               itp1(i)=0
            enddo
C
            do it=1,numtet
               do i=1,nelmnef(itettyp(it))
                  if (jtet1(jtetoff(it)+i).ge.mbndry) then
                     do j=1,ielmface0(i,itettyp(it))
                        node1 = itet1(itetoff(it)+
     *                              ielmface1(j,i,itettyp(it)))
                        itp1(node1)=ifitprfl
                     enddo
                  endif
               enddo
            enddo
C
            do it=1,numtet
               do i=1,nelmnef(itettyp(it))
                  if (jtet1(jtetoff(it)+i).gt.0.and.
     *                   jtet1(jtetoff(it)+i).lt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                     jf=jtet1(jtetoff(it)+i)-nef*(it-1)
                     if(itetclr(it).ne.itetclr(jt)) then
                        do j=1,ielmface0(i,itettyp(it))
                           node1=itet1(itetoff(it)+
     *                               ielmface1(j,i,itettyp(it)))
                           if(idone(node1).eq.0) then
                              idone(node1)=1
                              if(itp1(node1).eq.ifitprfl) then
                                 itp1(node1)=ifitpinb
                              else
                                 itp1(node1)=ifitpini
                              endif
                           endif
                        enddo
                     endif
                  endif
               enddo
            enddo
            call mmrelblk('idone',isubname,ipidone,icscode)
         endif
C$$C
C$$C try to set icr1 assuming box
C$$C
C$$         if(iseticr1.eq.0) then
C$$            index=ismin(npoints,xic,1)
C$$            xmin1=xic(index)
C$$            index=ismax(npoints,xic,1)
C$$            xmax1=xic(index)
C$$            xdiff=xmax1-xmin1
C$$            xavg=0.5d+00*(xmax1+xmin1)
C$$C
C$$            index=ismin(npoints,yic,1)
C$$            ymin1=yic(index)
C$$            index=ismax(npoints,yic,1)
C$$            ymax1=yic(index)
C$$            ydiff=ymax1-ymin1
C$$            yavg=0.5d+00*(ymax1+ymin1)
C$$C
C$$            index=ismin(npoints,zic,1)
C$$            zmin1=zic(index)
C$$            index=ismax(npoints,zic,1)
C$$            zmax1=zic(index)
C$$            zdiff=zmax1-zmin1
C$$            zavg=0.5d+00*(zmax1+zmin1)
C$$C
C$$            xref(1)=xavg
C$$            yref(1)=yavg
C$$            zref(1)=zavg+10.0*zdiff
C$$            xref(2)=xavg
C$$            yref(2)=yavg
C$$            zref(2)=zavg-10.0*zdiff
C$$            xref(3)=xavg
C$$            yref(3)=yavg-10.0*ydiff
C$$            zref(3)=zavg
C$$            xref(4)=xavg+10.0*xdiff
C$$            yref(4)=yavg
C$$            zref(4)=zavg
C$$            xref(5)=xavg
C$$            yref(5)=yavg+10.0*ydiff
C$$            zref(5)=zavg
C$$            xref(6)=xavg-10.0*xdiff
C$$            yref(6)=yavg
C$$            zref(6)=zavg
C$$C
C$$            do i=1,npoints
C$$C*****   ******icr1(i)=0
C$$            enddo
C$$            do it=1,numtet
C$$               do i=1,nelmnef(itettyp(it))
C$$                  index=jtetoff(it)+i
C$$                  if((jtet1(index).le.0.or.jtet1(index).eq.mbndry) .and.
C$$     *             itettyp(it).eq.ifelmtet) then
C$$                     if2=itet1(itetoff(it) +
C$$     *                   ielmface1(1,i,itettyp(it)))
C$$                     if3=itet1(itetoff(it) +
C$$     *                   ielmface1(2,i,itettyp(it)))
C$$                     if4=itet1(itetoff(it) +
C$$     *                   ielmface1(3,i,itettyp(it)))
C$$                     x2=xic(if2)
C$$                     y2=yic(if2)
C$$                     z2=zic(if2)
C$$                     x3=xic(if3)
C$$                     y3=yic(if3)
C$$                     z3=zic(if3)
C$$                     x4=xic(if4)
C$$                     y4=yic(if4)
C$$                     z4=zic(if4)
C$$                     dx= ((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
C$$                     dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
C$$                     dz= ((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
C$$                     do j=1,nef
C$$                        volface(j)=-((x3-xref(j))*dx+
C$$     *                               (y3-yref(j))*dy+
C$$     *                               (z3-zref(j))*dz)
C$$                     enddo
C$$                     index=ismax(nef,volface,1)
C$$                     jndex=nef*(it-1)
C$$                     if(cmotype(1:3).eq.'tet') then
C$$                        do j=1,3
C$$                           j1=itet1(jndex+ielmface1(j,i,ifelmtet))
C$$C*****   ******************icr1(j1)=index
C$$                        enddo
C$$                     elseif(cmotype(1:3).eq.'hex') then
C$$                        do j=1,4
C$$                           j1=itet1(jndex+ielmface1(j,i,ifelmhex))
C$$C*****   ******************icr1(j1)=index
C$$                        enddo
C$$                     endif
C$$                  endif
C$$               enddo
C$$            enddo
C$$         endif
C
      endif
C
      if(nelemavs.gt.0) then
         length=npoints
         call mmgetblk('idone',isubname,ipidone,length,2,icscode)
C
C        ***************************************************************
C        SET UP AN ARRARY THAT IDENTIFIES THE ALL REAL NODES.
C             IREAL1 = 1  -> Real Node.
C             IREAL1 = 0  -> Not a real node.
         length=npoints
         call mmgetblk('ireal1',isubname,ipireal1,length,2,icscode)
            if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
         call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrdum)
            if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
C        ***************************************************************
C
         do i1=1,npoints
            idone(i1)=0
         enddo
         do it=1,numtet
            do i=1,nelmnen(itettyp(it))
               i1=itet1(itetoff(it)+i)
               idone(i1)=idone(i1)+1
            enddo
         enddo
         do i1=1,npoints
            if(ireal1(i1).eq.1.and.idone(i1).eq.0) then
               itp1(i1)=ifitpdud
            endif
         enddo
         call mmrelblk('idone',isubname,ipidone,icscode)
      endif
C
      call cmo_get_name(cmonam,ierror)
      call cmo_set_info('nnodes',cmonam,npoints,1,1,ier)
      call cmo_set_info('nelements',cmonam,numtet,1,1,ier)
      call set_mbndry()
C
C      This does not work due to a bug in the cmo/status
C      command. Leave this here for now and take out comments
C      at a later date if/when cmo/status is fixed.
C
C      cbuff='cmo/status/-cmo-/brief ; finish '
C      call dotaskx3d(cbuff,ierror)
       cbuff='cmo/status/brief ; finish '
       call dotaskx3d(cbuff,ierror)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
