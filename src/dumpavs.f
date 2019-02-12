      subroutine dumpavs(ifile,cmo,
     *               nsd,nen,nef,
     *               nnodes,nelements,mbndry,
     *               ihcycle,time,dthydro,
     *               iopt_points,iopt_elements,
     *               iopt_values_node,iopt_values_elem,io_format)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES AN OUTPUT FILE IN AVS UCD FORMAT.
C
C     INPUT ARGUMENTS -
C
C        iopt_points      = 0 Do not output node coordinate information
C        iopt_points      = 1 Output node coordinate information node#, x, y, z (DEFAULT)
C        iopt_points      = 2 Output node coordinates information without node number in first column, x, y, z
C        iopt_elements    = 0 Do not output element connectivity information
C        iopt_elements    = 1 Output element connectivity information (DEFAULT)
C        iopt_elements    = 3 Output coordinate and element pt for AVS UCD pt file 
C        iopt_values_node = 0 Do not output node attribute information
C        iopt_values_node = 1 Output node attribute information (DEFAULT)
C        iopt_values_node = 2 Output node attribute information without node number in first column
C        iopt_values_elem = 0 Do not output element attribute information
C        iopt_values_elem = 1 Output element attribute information (DEFAULT)
C        iopt_values_elem = 2 Output element attribute information without node number in first column
C
C        io_format = 1   All attributes are written as real numbers. (dump/avs/...)
C        io_format = 2   Attributes are written as real and integer (slower method) (dump/avs2/...).
C
C        No longer supported but will still write file, replaced with iopt flags
C        io_format = 3   Node Attributes are written as real and integer, header info lines start with #
C        io_format = 4   Element Attributes are written as real and integer, header info lines start with #
C
C        Note all flags 2 will produce non-standard AVS files.
C        Note files without coordinate section is non-standard.
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: dumpavs.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   03 Jan 2007 07:11:20   tam
CPVCS    initialized ch_string with length to avoid compiler complaints
CPVCS    
CPVCS       Rev 1.9   26 Jul 2006 10:57:00   gable
CPVCS    Added options to output without node number and/or element number
CPVCS    as the first column of output. This produces non-standard AVS files
CPVCS    that in general will not be readable by AVS or read/avs. This is
CPVCS    useful for creating tabular output of node and element attributes
CPVCS    without node number and element number.
CPVCS    
CPVCS       Rev 1.8   22 Nov 2005 11:26:42   gable
CPVCS    Fixed case where there are nodes and node attributes but
CPVCS    zero elements.
CPVCS    
CPVCS       Rev 1.7   03 Nov 2005 11:27:32   gable
CPVCS    Fixed problem with the case of output of only element attributes
CPVCS    with no node coordinate, no connectivity, no node attribute info.
CPVCS    Also modified format so the the node number and element counter
CPVCS    are written with only as many digits as necessary based on the
CPVCS    number of nodes and number of elements.
CPVCS    
CPVCS       Rev 1.6   12 Oct 2005 15:25:16   gable
CPVCS    Added avs2 option with will output node and element attributes
CPVCS    as real or integer. avs option outputs all node and element attributes
CPVCS    as real. Code will test the max(abs(attribute)) and format integers to
CPVCS    use only as many columns as are needed.
CPVCS    
CPVCS       Rev 1.5   30 Sep 2004 14:30:12   gable
CPVCS    Update dumpavs to support full control of
CPVCS    node,element,node_attribute,element_attribue output
CPVCS    by using 0 or 1 in tokens 5,6,7,8. Also change node
CPVCS    and element attribute output to support up to 100
CPVCS    columns of data in spread sheet like format.
CPVCS    
CPVCS       Rev 1.4   11 Sep 2001 13:02:22   gable
CPVCS    Change format of itetclr output. Used to be i5,
CPVCS    changed to i8 which should do until there are
CPVCS    more than 99,999,999 itetclr values.
CPVCS    
CPVCS       Rev 1.3   11 Jul 2001 16:57:48   tam
CPVCS    fixed mmgetblk error for case with 0 nnode attributes
CPVCS    fixed usage of iopt_points, iopt_element, iopt_values
CPVCS    
CPVCS       Rev 1.2   29 Feb 2000 14:54:48   gable
CPVCS    RVG Fixed a bug when dumping out element based attributes.
CPVCS    Instead of using ioffse for filling in the xvalues array it is using ioffs. 
CPVCS    
CPVCS       Rev 1.1   Tue Feb 08 16:41:18 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 14:43:18 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.29   Wed Oct 06 16:32:20 1999   dcg
CPVCS    declare iadr as a pointer
CPVCS
CPVCS       Rev 1.28   Wed Sep 08 12:33:54 1999   gable
CPVCS    Added error checking so that if the cmo has no
CPVCS    nodes and no elements it will return rather then
CPVCS    crash with a mmgetblk error.
CPVCS
CPVCS       Rev 1.27   Tue Feb 02 12:04:30 1999   dcg
CPVCS    be sure to close file before leaving
CPVCS
CPVCS       Rev 1.26   Tue Jan 19 11:37:36 1999   gable
CPVCS    Fixed error in node and element attribute output
CPVCS    when there are more than 7 attributes. Error caused
CPVCS    attribute 8, 12, 16, etc. to not be output.
CPVCS
CPVCS       Rev 1.25   Tue Dec 22 16:39:02 1998   dcg
CPVCS    accept ranks other than 1
CPVCS
CPVCS       Rev 1.24   Tue Dec 22 12:16:30 1998   dcg
CPVCS    dump out nelement length arrays
CPVCS    use implicit none
CPVCS    test ranges of values of coordinates (should be between
CPVCS    1.e-30 and 1.e+30.
CPVCS
CPVCS       Rev 1.23   Thu Nov 12 10:25:38 1998   gable
CPVCS    Changes to attribute format.
CPVCS    Hardwired so that the -def- attribute is not include in output.
CPVCS
CPVCS       Rev 1.22   Tue Apr 14 11:08:18 1998   kmb
CPVCS    changed attribute type so attributes can be integers, reals, or units.
CPVCS
CPVCS       Rev 1.21   Mon Apr 14 16:43:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.20   Fri Dec 20 11:48:56 1996   dcg
CPVCS    use itetoff to get index to nodes in itet array
CPVCS
CPVCS       Rev 1.19   Mon Nov 18 10:29:40 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.18   Thu Jun 27 09:08:46 1996   gable
CPVCS    Output of prism element connectivity list went past 72
CPVCS    spaces resulting in incorrect connectivity output. Added
CPVCS    continuation line.
CPVCS
CPVCS       Rev 1.17   Tue Apr 30 07:27:28 1996   het
CPVCS    Add the option for dumping just points, elements or values.
CPVCS
CPVCS       Rev 1.16   Wed Mar 06 16:01:28 1996   het
CPVCS    Add attributes to the AVS file.
CPVCS
CPVCS       Rev 1.15   Mon Mar 04 11:12:44 1996   dcg
CPVCS     remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.14   Tue Feb 06 11:51:42 1996   dcg
CPVCS    remove references to uic,vic,wic,pic,ric,eic
CPVCS
CPVCS       Rev 1.13   Mon Jan 29 22:15:26 1996   het
CPVCS    Dump the isn1 array to the AVS file
CPVCS
CPVCS       Rev 1.12   07/14/95 10:18:52   het
CPVCS    Make output format consistent for FEHMN/AVS
CPVCS
CPVCS       Rev 1.11   06/22/95 08:35:52   het
CPVCS    Use Carl's version of dumpavs for imt1, itp1, icr1 fields
CPVCS
CPVCS       Rev 1.9   03/03/95 11:07:50   dcg
CPVCS     Remove hardwired 2dmesh object
CPVCS
CPVCS       Rev 1.8   02/21/95 16:33:36   het
CPVCS    Correct an error deciding to write tets
CPVCS
CPVCS       Rev 1.7   02/16/95 07:35:24   het
CPVCS    Correct format errors in dump and read commands
CPVCS
CPVCS       Rev 1.6   02/01/95 11:30:08   het
CPVCS    Write the coordinates always as a 3D triplet
CPVCS
CPVCS       Rev 1.5   01/31/95 18:45:44   het
CPVCS    Correct an error in the 2d-dumpavs routine
CPVCS
CPVCS       Rev 1.4   01/04/95 22:02:04   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   01/03/95 16:44:26   het
CPVCS    Correct the format of AVS dumps for Carl.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/21/94 08:51:02   het
CPVCS    Corrected the order of the node numbers for hexes. The order
CPVCS        is reversed from the way the code stores them.
CPVCS
CPVCS       Rev 1.1   12/11/94 17:55:12   het
CPVCS    Fixed errors related to reading AT&T avs files.
CPVCS
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
C
      character ifile*(*)
      character*32 cmo
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)

      integer imt1(*), itp1(*), icr1(*), isn1(*)
      real*8 xic(*), yic(*), zic(*)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipisetwd, isetwd)
      integer isetwd(*)
      integer itet(*), jtet(*)
      integer  itypout
C
      pointer (ipxvalues, xvalues)
      real*8 xvalues(*)
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      integer icmo_pointer(*)
      REAL*8 xcmo_pointer(*)
      pointer (iadr,dat)
      real*8 dat(*)
C
      integer  nmcmoatt
      pointer (ipcnames,cnames)
      pointer (ipranks,iranks)
      pointer (ipidx,idx)
      pointer (ipidxe,idxe)
      pointer (ipoffs,ioffs)
      pointer (ipoffse,ioffse)
      pointer (iplengths,clengths)
      pointer (ipioflags,cioflags)
      character*32 cnames(*),clengths(*),cioflags(*)
      integer iranks(*),ioffs(*),ioffse(*),idx(*),idxe(*)
C
      character*32 isubname
      character*32  ctype , cinterp, crank,cpers
      character*132 logmess
      real*8 a,b,c,time,dthydro,cutoff,maxval
      real*8 cinterpolate
      integer icharlnf
      integer ilen,ityp,ierr,ivoronoi2d,ivoronoi3d,mmlength,
     *  ierror_return,irank,lent,nvalues1,length,len2,i8,i7,
     *  i6,i5,i4,i3,i2,i1,index,it,j,iflag,len1,i,nvalues,
     *  icscode,ierror,iunit,iopt_points,iopt_elements,
     *  iopt_values_node,iopt_values_elem,io_format,
     *  io_format_node, io_format_elem,
     *  mbndry,nelements,nnodes,ihcycle,nvaluese,lenval,lvalues,
     *  lvaluese,izero,k,irowlen,irowlene, ncolumn_max
      integer nnodes_io,nelements_io,nvalues_node,nvalues_elem
      integer nsd, nen, nef, nsdgeom,nsdtopo,nee
      integer ielements, itoff, jtoff
C
      pointer (ipitype_flag,itype_flag)
      pointer (ipimin_vint, imin_vint)
      pointer (ipimax_vint, imax_vint)
      pointer (ipn_int_i,   n_int_i)
      integer itype_flag(*),imin_vint(*),imax_vint(*),n_int_i(*)
      integer n_real, n_dec, n_int, n_space, n_char_string
      integer if_clear
      real*8 r8
c
      character*4096 ch_string 
C
      character*8  ch_elem1
      character*22 ch_elem2
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data cutoff/1.e-30/
      data maxval/1.e+30/
      data izero/0/
C
C ######################################################################
C begin

      it = 0
      lenval = 0

      if((nnodes .eq. 0) .and. (nelements .eq. 0))then
          write(logmess,'(a)')'WARNING: dumpavs'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: nnodes=0 nelements = 0'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: No output'
          call writloga('default',0,logmess,0,icscode)
          return
      endif
      isubname="dumpavs"
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        write(logmess,'(a)')'WARNING: No output'
        call writloga('default',0,logmess,0,icscode)
        return
      endif

C
C  get information from  mesh object
C
      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
C     ******************************************************************
C


C     write AVS UCD pt elements for node mesh with 0 elements 
      if(iopt_elements .eq. 3)then

         if (nnodes.gt.0 .and. nelements.gt.0 ) then
           write(logmess,'(a)')'WARNING: AVS UCD PT No output'
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a)') 
     *    'Expecting points but mesh has elements and connectivity.'
           call writloga('default',0,logmess,0,ierr)
           return
         endif

         nelements = nnodes
         io_format = 2
         iopt_elements = 3
         io_format_node = 0
         io_format_elem = 0

      elseif(io_format .eq. 3)then
         io_format = 2
         io_format_node = 1
         io_format_elem = 0
      elseif(io_format .eq. 4)then
         io_format = 2
         io_format_node = 0
         io_format_elem = 1
      else
         io_format_node = 0
         io_format_elem = 0
      endif


C debug
C     print*,"nnodes  = ",nnodes
C     print*,"nelements  = ",nelements
C     print*,"io format, node, elem = ",
C    *        io_format,io_format_node,io_format_elem
C     print*,"iopt_points       = ",iopt_points
C     print*,"iopt_elements     = ",iopt_elements
C     print*,"iopt_values_node  = ",iopt_values_node
C     print*,"iopt_values_elem  = ",iopt_values_elem

C
C     ******************************************************************
C     Count the number of AVS fields to write
C
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   icscode)
C
      nvalues=0
      nvaluese=0
      lvalues=0
      lvaluese=0
      lenval=0
      irowlen=0
      irowlene=0
 
c     Hardwire turning off output of -def- field.
c
      call dotaskx3d
     1 ('cmo/modatt/-def-/-def-/ioflag/x;finish',icscode)
      call mmgetblk('cnames',isubname,ipcnames,nmcmoatt*8,1,icscode)
      call mmgetblk('clengths',isubname,iplengths,nmcmoatt*8,1,icscode)
      call mmgetblk('iranks',isubname,ipranks,nmcmoatt,1,icscode)
      call mmgetblk('idx',isubname,ipidx,nmcmoatt,1,icscode)
      call mmgetblk('idxe',isubname,ipidxe,nmcmoatt,1,icscode)
      call mmgetblk('ioffs',isubname,ipoffs,nmcmoatt,1,icscode)
      call mmgetblk('ioffse',isubname,ipoffse,nmcmoatt,1,icscode)
      call mmgetblk('cioflags',isubname,ipioflags,nmcmoatt*8,1,icscode)
c
      call mmgetblk
     1   ('itype_flag',isubname,ipitype_flag,nmcmoatt,1,icscode)
      call mmgetblk
     1   ('imin_vint', isubname,ipimin_vint, nmcmoatt,1,icscode)
      call mmgetblk
     1   ('imax_vint', isubname,ipimax_vint, nmcmoatt,1,icscode)
      call mmgetblk
     1   ('n_int_i',   isubname,ipn_int_i,   nmcmoatt,1,icscode)
C
c
      ioffs(1)=0
      ioffse(1)=0
c
      do i=1,nmcmoatt
C
C
C....    NAME Field.
C
         call cmo_get_attribute_name(cmo,i,
     *                     cnames(i),icscode)
C
         call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *    clengths(i),cinterp,cpers,cioflags(i),ierror_return)
         call cmo_get_info(crank,cmo,iranks(i),ilen,ityp,icscode)
C
         len1=icharlnf(cioflags(i))
         iflag=0
         ioffs(1)=0
         do j=1,len1
            if(cioflags(i)(j:j).eq.'a'.and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
               iflag=1
               nvalues=nvalues+1
               idx(nvalues)=i
               lvalues=lvalues+iranks(i)
               ioffs(i)=irowlen
               irowlen=irowlen+iranks(i)
            endif
         enddo
         do j=1,len1
            if(cioflags(i)(j:j).eq.'a'.and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nelements') then
               iflag=1
               nvaluese=nvaluese+1
               idxe(nvaluese)=i
               lvaluese=lvaluese+iranks(i)
               ioffse(i)=irowlene
               irowlene=irowlene+iranks(i)
            endif
         enddo
      enddo
      if ((nvalues+nvaluese)*32 .gt. 4096) then
        write(logmess,"(a)")
     1  'ERROR dumpavs: Number of output attributes not supported '
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR dumpavs: Increase size of ch_string character string '
        call writloga('default',0,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR dumpavs: No Action '
        call writloga('default',0,logmess,1,ierr)
        goto 9999
      endif

C
C     write header at top of file only if writing node coordinates
C     num_nodes, num_elems, num_node_att, num_elem_att, num_mdata
C       num_mdata is model data and is not used and is set to 0
C       iopt_points toggle node coordinates
C       iopt_elements toggle element connectivity
C       iopt_values_node toggle node attributes 
C       iopt_values_elem toggle element attributes 

      if(iopt_points .ge. 1)then
         nnodes_io = nnodes
      else
         nnodes_io = izero
      endif
      if(iopt_elements .ge. 1)then
         nelements_io = nelements
      else
         nelements_io = izero
      endif
      if(iopt_values_node .ge. 1)then
         nvalues_node = nvalues
      else
         nvalues_node = izero
      endif
      if(iopt_values_elem .ge. 1)then
         nvalues_elem = nvaluese
      else
         nvalues_elem = izero
      endif
      if((io_format_node .ne. 0) .or. (io_format_elem .ne. 0))then
       write(iunit,9029)'# ',
     *   nnodes_io,nelements_io,nvalues_node,nvalues_elem,izero
      else
       write(iunit,9030)
     *   nnodes_io,nelements_io,nvalues_node,nvalues_elem,izero
      endif
     
 9029 format(a2,5(i10,1x))
 9030 format(5(i10,1x))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Write the point coordinates
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(iopt_points .ge. 1) then

         if(iopt_points .eq. 1)then
C
C        Output node#, x,y,z
C         
         ch_string(1:23) = '(i03.03,1x,3(1pe20.12))'
      if(nnodes .ge. 99)
     1   ch_string(1:23) = '(i04.04,1x,3(1pe20.12))'
      if(nnodes .ge. 999)
     1   ch_string(1:23) = '(i05.05,1x,3(1pe20.12))'
      if(nnodes .ge. 9999)
     1   ch_string(1:23) = '(i06.06,1x,3(1pe20.12))'
      if(nnodes .ge. 99999)
     1   ch_string(1:23) = '(i07.07,1x,3(1pe20.12))'
      if(nnodes .ge. 999999)
     1   ch_string(1:23) = '(i08.08,1x,3(1pe20.12))'
      if(nnodes .ge. 9999999)
     1   ch_string(1:23) = '(i09.09,1x,3(1pe20.12))'
      if(nnodes .ge. 99999999)
     1   ch_string(1:23) = '(i10.10,1x,3(1pe20.12))'
      if(nnodes .ge. 999999999)
     1   ch_string(1:23) = '(i11.11,1x,3(1pe20.12))'
     
         elseif(iopt_points .eq. 2)then
C
C        Output x,y,z without node number in first column
C
         ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 99)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 9999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 99999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 999999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 9999999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 99999999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
      if(nnodes .ge. 999999999)
     1   ch_string(1:23) = '(3(1pe20.12))          '
     
         endif
         do i=1,nnodes
            a=xic(i)
            b=yic(i)
            c=zic(i)
            if (abs(a).lt.cutoff) a=0.0
            if (abs(b).lt.cutoff) b=0.0
            if (abs(c).lt.cutoff) c=0.0
            if (a.gt.maxval) then
               write(logmess,9035) i,a
 9035          format(' x coordinate number ',i10,
     *                ' reset to maximum of 1.e+30 ',e12.5)
               call writloga('default',0,logmess,0,icscode)
               a=maxval
            endif
            if (a.lt.-maxval) then
               write(logmess,9035) i,a
               call writloga('default',0,logmess,0,icscode)
               a=-maxval
            endif
            if (b.gt.maxval) then
               write(logmess,9035) i,b
               call writloga('default',0,logmess,0,icscode)
               b=maxval
            endif
            if (b.lt.-maxval) then
               write(logmess,9035) i,b
               call writloga('default',0,logmess,0,icscode)
               b=-maxval
            endif
            if (abs(c).gt.maxval) then
               write(logmess,9035) i,c
               call writloga('default',0,logmess,0,icscode)
               c=maxval
            endif
            if (c.lt.-maxval) then
               write(logmess,9035) i,c
               call writloga('default',0,logmess,0,icscode)
               c=-maxval
            endif
            if(iopt_points .eq. 1)then
            write(iunit,ch_string(1:23)) i,a,b,c
            elseif(iopt_points .eq. 2)then
            write(iunit,ch_string(1:23))a,b,c
            endif
C
C 9040       format(i10.10,1x,3(1pe20.12))
C
         enddo
      endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Write the connectivity
C
C        Option to write 0 element point file at pt elements
C        This allows paraview to read and display the points
C        Set nelements to nnodes
C        Set material id to 1
C        Set connectivity to single integer value of node_id
C        Note the mesh attributes related to elements do not exist
C        Format:
C        1  1   pt   1
C        2  1   pt   2
C        3  1   pt   3
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     
      if(iopt_elements .ge. 1) then
C
C Since this is node connectivity all connectivity
C entries will be positive. We don't have to leave room for negative signs.
C
         if(iopt_elements.eq.1 .or. iopt_elements.eq.3) then
C
C        Output element#, material_id, element_type, connectivity
C         
         ch_elem1 = '(i03.03,'
      if(nelements .ge. 99)
     1   ch_elem1 = '(i04.04,'
      if(nelements .ge. 999)
     1   ch_elem1 = '(i05.05,'
      if(nelements .ge. 9999)
     1   ch_elem1 = '(i06.06,'
      if(nelements .ge. 99999)
     1   ch_elem1 = '(i07.07,'
      if(nelements .ge. 999999)
     1   ch_elem1 = '(i08.08,'
      if(nelements .ge. 9999999)
     1   ch_elem1 = '(i09.09,'
      if(nelements .ge. 99999999)
     1   ch_elem1 = '(i10.10,'
      if(nelements .ge. 999999999)
     1   ch_elem1 = '(i11.11,'

         elseif(iopt_elements .eq. 2)then

         ch_elem1 = '(       '

         endif
         
         ch_elem2 = '1x,i8,1x,a5,8(1x,i02))'
      if(nnodes .ge. 99)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i03))'
      if(nnodes .ge. 999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i04))'
      if(nnodes .ge. 9999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i05))'
      if(nnodes .ge. 99999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i06))'
      if(nnodes .ge. 999999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i07))'
      if(nnodes .ge. 9999999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i08))'
      if(nnodes .ge. 99999999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i09))'
      if(nnodes .ge. 999999999)
     1   ch_elem2 = '1x,i8,1x,a5,8(1x,i10))'

      ch_string(1:30) = ch_elem1(1:8)//ch_elem2(1:22)

C debug
C        print*,"ch_elem1: ",ch_elem1
C        print*,"ch_elem2: ",ch_elem2
C        print*,"ch_string: ",ch_string(1:30)
          
         if(iopt_elements.eq.1 .or. iopt_elements.eq.3)then
         do it=1,nelements

C           write connectivity for pt mesh with 0 elements
            if(iopt_elements .eq. 3) then

               write(iunit,ch_string(1:30)) it,1,'pt',it

            elseif(itettyp(it).eq.ifelmpnt) then

               index=itetoff(it)
               i1=itet(1+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'pt',i1

            elseif(itettyp(it).eq.ifelmlin) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'line',
     *                                      i1,i2
            elseif(itettyp(it).eq.ifelmtri) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'tri',
     *                                      i1,i2,i3
            elseif(itettyp(it).eq.ifelmqud) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'quad',
     *                                      i1,i2,i3,i4
            elseif(itettyp(it).eq.ifelmtet) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'tet',
     *                                      i1,i2,i4,i3
            elseif(itettyp(it).eq.ifelmpyr) then
               index=itetoff(it)
               i2=itet(1+index)
               i3=itet(2+index)
               i4=itet(3+index)
               i5=itet(4+index)
               i1=itet(5+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'pyr',
     *                                      i1,i2,i3,i4,i5
            elseif(itettyp(it).eq.ifelmpri) then
               index=itetoff(it)
               i4=itet(1+index)
               i5=itet(2+index)
               i6=itet(3+index)
               i1=itet(4+index)
               i2=itet(5+index)
               i3=itet(6+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'prism',
     *                                      i1,i2,i3,i4,i5,i6
            elseif(itettyp(it).eq.ifelmhex) then
               index=itetoff(it)
               i1=itet(5+index)
               i2=itet(6+index)
               i3=itet(7+index)
               i4=itet(8+index)
               i5=itet(1+index)
               i6=itet(2+index)
               i7=itet(3+index)
               i8=itet(4+index)
               write(iunit,ch_string(1:30)) it,itetclr(it),'hex',
     *                                      i1,i2,i3,i4,i5,i6,i7,i8
            endif
         enddo
         elseif(iopt_elements .eq. 2)then
         do it=1,nelements
            if(itettyp(it).eq.ifelmpnt) then
               index=itetoff(it)
               i1=itet(1+index)
               write(iunit,ch_string(1:30)) itetclr(it),'pt',
     *                                      i1
            elseif(itettyp(it).eq.ifelmlin) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               write(iunit,ch_string(1:30)) itetclr(it),'line',
     *                                      i1,i2
            elseif(itettyp(it).eq.ifelmtri) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               write(iunit,ch_string(1:30)) itetclr(it),'tri',
     *                                      i1,i2,i3
            elseif(itettyp(it).eq.ifelmqud) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,ch_string(1:30)) itetclr(it),'quad',
     *                                      i1,i2,i3,i4
            elseif(itettyp(it).eq.ifelmtet) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,ch_string(1:30)) itetclr(it),'tet',
     *                                      i1,i2,i4,i3
            elseif(itettyp(it).eq.ifelmpyr) then
               index=itetoff(it)
               i2=itet(1+index)
               i3=itet(2+index)
               i4=itet(3+index)
               i5=itet(4+index)
               i1=itet(5+index)
               write(iunit,ch_string(1:30)) itetclr(it),'pyr',
     *                                      i1,i2,i3,i4,i5
            elseif(itettyp(it).eq.ifelmpri) then
               index=itetoff(it)
               i4=itet(1+index)
               i5=itet(2+index)
               i6=itet(3+index)
               i1=itet(4+index)
               i2=itet(5+index)
               i3=itet(6+index)
               write(iunit,ch_string(1:30)) itetclr(it),'prism',
     *                                      i1,i2,i3,i4,i5,i6
            elseif(itettyp(it).eq.ifelmhex) then
               index=itetoff(it)
               i1=itet(5+index)
               i2=itet(6+index)
               i3=itet(7+index)
               i4=itet(8+index)
               i5=itet(1+index)
               i6=itet(2+index)
               i7=itet(3+index)
               i8=itet(4+index)
               write(iunit,ch_string(1:30)) itetclr(it),'hex',
     *                                      i1,i2,i3,i4,i5,i6,i7,i8
            endif
         enddo
         endif
      endif
 9050          format(i10.10,1x,i8,1x,a5,8(1x,i10))
 9052          format(i8,1x,a5,8(1x,i10))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Write the node and/or element attributes
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      if (iopt_values_node.eq.0 .or. nvalues.eq.0) go to 9995
      if (iopt_values_node.ge.1) then
         if(io_format_node .eq. 0)then
         write(iunit,9060) nvalues,(iranks(idx(i)),i=1,nvalues)
 9060    format(i5.5,100(1x,i2))
         elseif(io_format_node .eq. 1)then
         write(iunit,9061)'# ', nvalues,(iranks(idx(i)),i=1,nvalues)
 9061    format(a,i5.5,100(1x,i2))
         endif
         do i=1,nmcmoatt
            len1=icharlnf(cioflags(i))
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
                  len2=icharlnf(cnames(i))
                  call mmfindbk(cnames(i),cmo,iadr,length,icscode)
                  call mmgettyp(iadr,itypout,icscode)
                  if(io_format_node .eq. 0)then
                  if(itypout.eq.1) then
                   write(iunit,9070) cnames(i)(1:len2),', integer '
                  elseif(itypout.eq.2) then
                   write(iunit,9070) cnames(i)(1:len2),', real '
                  else
                   write(iunit,9070) cnames(i)(1:len2),', no units'
                  endif
                  elseif(io_format_node .eq. 1)then
C
C Prepend lines with #
C
                  if(itypout.eq.1) then
                   write(iunit,9071)'# ', cnames(i)(1:len2),', integer '
                  elseif(itypout.eq.2) then
                   write(iunit,9071)'# ', cnames(i)(1:len2),', real '
                  else
                   write(iunit,9071)'# ', cnames(i)(1:len2),', no units'
                  endif
                  endif
 9070             format(a,a)
 9071             format(a,a,a)
               endif
            enddo
         enddo
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Write the node attributes
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        If there are no node attributes, skip to elem attributes
         lenval=lvalues*nnodes
         call mmgetblk('xvalues',isubname,ipxvalues,lenval,2,icscode)
         if (iopt_values_node.eq.0 .or. lenval.eq.0) then
           lenval = 0
           go to 9995
         endif

         nvalues1=0
         do i=1,nmcmoatt
            len1=icharlnf(cioflags(i))
            iflag=0
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
                  iflag=1
                  nvalues1=nvalues1+1
               endif
            enddo
            if(iflag.eq.1) then
 
C
               call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *         clengths(i),cinterp,cpers,cioflags(i),ierror_return)
               lent=icharlnf(ctype)
               if(ctype(1:lent).eq.'VINT') then
C
                  itype_flag(nvalues1) = 1
                  call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
C
                  mmlength=irank*length
C
                  call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
C
                  if(ierror_return.ne.0) then
                     call x3d_error(isubname,'mmfindbk')
                  else
                     if(length.eq.nnodes) then
                        do j=1,length
                          do k=1,irank
                            xvalues(ioffs(i)+irowlen*(j-1)+k)=
     *                        icmo_pointer(irank*(j-1)+k)
                              imin_vint(nvalues1)=
     *                           min(imin_vint(nvalues1),
     *                           icmo_pointer(irank*(j-1)+k))
                              imax_vint(nvalues1)=
     *                           max(imax_vint(nvalues1),
     *                           icmo_pointer(irank*(j-1)+k))
                          enddo
                        enddo
                        imax_vint(nvalues1)=
     *                       max(abs(imin_vint(nvalues1)),
     *                           abs(imax_vint(nvalues1)))
                     else
                        write(logmess,9100) cnames(i),
     *                                      length,irank
 9100                   format("Invalid length for AVS variable: ",
     *                         a,' length=',i10,' rank=',i10)
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               elseif(ctype(1:lent).eq.'VDOUBLE') then
C
                  itype_flag(nvalues1) = 2
                  call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
C
                  mmlength=irank*length
C
                  call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
                  if(ierror_return.ne.0) then
                     call x3d_error(isubname,'mmfindbk')
                  else
                     if(length.eq.nnodes) then
                        do j=1,length
                          do k=1,irank
                            xvalues(ioffs(i)+irowlen*(j-1)+k)=
     *                        cinterpolate('function',cinterp,
     *                        xcmo_pointer(irank*(j-1)+k))
                          enddo
                        enddo
                     elseif(length.eq.nelements) then
c                        write(logmess,9100) cname(i),
c     *                                      length,irank
c                        call x3d_error(isubname,'Illegal cmo_name')
                     else
                        write(logmess,9100) cnames(i),
     *                                      length,irank
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               endif
            endif
         enddo
         
         if(io_format .eq. 1)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Output only reals.
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         ncolumn_max = 100
         
         if(iopt_values_node .eq. 1)then
         
         do i1=1,nnodes
            write(iunit,9080) i1,
     *        (xvalues(i+nvalues1*(i1-1)),i=1,min(ncolumn_max,nvalues1))
 9080       format(i10.10,100(1x,1pe19.12))
            if(nvalues1.gt. ncolumn_max) then
               do j=ncolumn_max+1,nvalues1,ncolumn_max
                  write(iunit,9085)
     *             (xvalues(i+nvalues1*(i1-1)),
     *              i=j,min(j+ncolumn_max-1,nvalues1))
 9085             format(100(1x,1pe19.12))
               enddo
            endif
         enddo
         
         elseif(iopt_values_node .eq. 2)then
         
         do i1=1,nnodes
            write(iunit,9081)
     *        (xvalues(i+nvalues1*(i1-1)),i=1,min(ncolumn_max,nvalues1))
 9081       format(100(1pe19.12,1x))
            if(nvalues1.gt. ncolumn_max) then
               do j=ncolumn_max+1,nvalues1,ncolumn_max
                  write(iunit,9085)
     *             (xvalues(i+nvalues1*(i1-1)),
     *              i=j,min(j+ncolumn_max-1,nvalues1))
               enddo
            endif
         enddo
         
         endif
         
         elseif(io_format .eq. 2)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Output real and integer by packing a character string.
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         n_real   = 20
         n_dec    = 12
         n_space  =  0
         if_clear =  0

         do i = 1, nvalues1
            n_int_i(i) = nint(alog10( real(imax_vint(i)+1))) + 3
            if(n_int_i(i) .le. 3)n_int_i(i) = 3
         enddo
         do i1=1,nnodes
            if(iopt_values_node .eq. 1)then
            
            n_int  = nint( alog10( real(nnodes)+1 ) ) + 3
            n_char_string = 0
            r8 = i1
            call string_add_entry_r8
     1     (ch_string,n_char_string,r8, 1,n_real,n_dec,n_int,n_space)
            elseif(iopt_values_node .eq. 2)then
C
            n_char_string = 0
C
            endif
            do i = 1, nvalues1
               n_int = n_int_i(i)
               call string_add_entry_r8
     1        (ch_string,n_char_string,xvalues(i+nvalues1*(i1-1)),
     2         itype_flag(i),n_real,n_dec,n_int,n_space)
            enddo
            call string_write(ch_string, n_char_string, iunit, if_clear)
         enddo
         endif
         
      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Write the element attributes
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     If there are no elements to write, skip this part
 9995 if (iopt_values_elem.eq.0 .or. nvaluese.eq.0) go to 9999
      if(iopt_values_elem.ge.1) then
         if(io_format_elem .eq. 0)then
         write(iunit,9060) nvaluese,(iranks(idxe(i)),i=1,nvaluese)
         elseif(io_format_elem .eq. 1)then
         write(iunit,9061)'# ', nvaluese,(iranks(idx(i)),i=1,nvaluese)
         endif
         do i=1,nmcmoatt
 
            len1=icharlnf(cioflags(i))
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nelements')
     *             then
                  len2=icharlnf(cnames(i))
                  call mmfindbk(cnames(i),cmo,iadr,length,icscode)
                  call mmgettyp(iadr,itypout,icscode)
                  if(io_format_elem .eq. 0)then
                  if(itypout.eq.1) then
                   write(iunit,9070) cnames(i)(1:len2),', integer '
                  elseif(itypout.eq.2) then
                   write(iunit,9070) cnames(i)(1:len2),', real '
                  else
                   write(iunit,9070) cnames(i)(1:len2),', no units'
                  endif
                  elseif(io_format_elem .eq. 1)then
                  if(itypout.eq.1) then
                   write(iunit,9071)'# ', cnames(i)(1:len2),', integer '
                  elseif(itypout.eq.2) then
                   write(iunit,9071)'# ', cnames(i)(1:len2),', real '
                  else
                   write(iunit,9071)'# ', cnames(i)(1:len2),', no units'
                  endif
                  endif
               endif
            enddo
         enddo
C
        if(lvaluese*nelements.gt.lenval) then
          if ((lenval.eq.0) .or. (iopt_values_node.eq.0)) then
           lenval=lvaluese*nelements
           call mmgetblk('xvalues',isubname,ipxvalues,lenval,2,icscode)
          else
            lenval=lvaluese*nelements
            call mmincblk('xvalues',isubname,ipxvalues,lenval,icscode)
          endif
        endif
C
         nvalues1=0
         do i=1,nmcmoatt
 
            len1=icharlnf(cioflags(i))
            iflag=0
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nelements')
     *            then
                  iflag=1
                  nvalues1=nvalues1+1
               endif
            enddo
            if(iflag.eq.1) then
            call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *         clengths(i),cinterp,cpers,cioflags(i),ierror_return)
c
               lent=icharlnf(ctype)
               if(ctype(1:lent).eq.'VINT') then
C
                  itype_flag(nvalues1) = 1
                  call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
C
                  mmlength=irank*length
C
                  call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
C
                  if(ierror_return.ne.0) then
                     call x3d_error(isubname,'mmfindbk')
                  else
                     if(length.eq.nelements) then
                        do j=1,length
                          do k=1,irank
                            xvalues(ioffse(i)+irowlene*(j-1)+k)=
     *                        icmo_pointer(irank*(j-1)+k)
                              imin_vint(nvalues1)=
     *                           min(imin_vint(nvalues1),
     *                           icmo_pointer(irank*(j-1)+k))
                              imax_vint(nvalues1)=
     *                           max(imax_vint(nvalues1),
     *                           icmo_pointer(irank*(j-1)+k))
                          enddo
                        enddo
                        imax_vint(nvalues1)=
     *                       max(abs(imin_vint(nvalues1)),
     *                           abs(imax_vint(nvalues1)))
                     else
                        write(logmess,9100) cnames(i), length,irank
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               elseif(ctype(1:lent).eq.'VDOUBLE') then
C
                  itype_flag(nvalues1) = 2
                  call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
C
                  mmlength=irank*length
C
                  call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
                  if(ierror_return.ne.0) then
                     call x3d_error(isubname,'mmfindbk')
                  else
                     if(length.eq.nelements) then
                        do j=1,length
                          do k=1,irank
                            xvalues(ioffse(i)+irowlene*(j-1)+k)=
     *                       cinterpolate('function',cinterp,
     *                       xcmo_pointer(irank*(j-1)+k))
                          enddo
                        enddo
                     else
                        write(logmess,9100) cnames(i),
     *                                      length,irank
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               endif
            endif
         enddo
         if(io_format .eq. 1)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Output only reals.
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         ncolumn_max = 100
         
         if(iopt_values_elem .eq. 1)then
         
         do i1=1,nelements
            write(iunit,9080) i1,
     *       (xvalues(i+nvalues1*(i1-1)),i=1,min(ncolumn_max,nvalues1))
            if(nvalues1.gt.ncolumn_max) then
               do j=ncolumn_max+1,nvalues1,ncolumn_max
                  write(iunit,9085)
     *               (xvalues(i+nvalues1*(i1-1))
     *                ,i=j,min(j+ncolumn_max-1,nvalues1))
               enddo
            endif
         enddo

         elseif(iopt_values_elem .eq. 2)then

         do i1=1,nelements
            write(iunit,9081)
     *       (xvalues(i+nvalues1*(i1-1)),i=1,min(ncolumn_max,nvalues1))
            if(nvalues1.gt.ncolumn_max) then
               do j=ncolumn_max+1,nvalues1,ncolumn_max
                  write(iunit,9085)
     *               (xvalues(i+nvalues1*(i1-1))
     *                ,i=j,min(j+ncolumn_max-1,nvalues1))
               enddo
            endif
         enddo

         endif
         
         elseif(io_format .eq. 2)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        Output real and integer by packing a character string.
C         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         n_real = 20
         n_dec  = 12
         n_space = 0
         if_clear = 0

         do i = 1, nvalues1
            n_int_i(i) = nint(alog10( real(imax_vint(i)+1))) + 3
            if(n_int_i(i) .le. 3)n_int_i(i) = 3
         enddo
         do i1=1,nelements
         
            if(iopt_values_elem .eq. 1)then
            
            n_int  = nint( alog10( real(nelements)+1 ) ) + 3
            n_char_string = 0
            r8 = i1
            call string_add_entry_r8
     1     (ch_string,n_char_string,r8, 1,n_real,n_dec,n_int,n_space)
            elseif(iopt_values_elem .eq. 2)then
C
            n_char_string = 0
C
            endif
            do i = 1, nvalues1
               n_int = n_int_i(i)
               call string_add_entry_r8
     1        (ch_string,n_char_string,xvalues(i+nvalues1*(i1-1)),
     2         itype_flag(i),n_real,n_dec,n_int,n_space)
            enddo
            call string_write(ch_string, n_char_string, iunit, if_clear)
         enddo
       endif
      endif
C
 9999 close(iunit)
      continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end

      subroutine string_add_entry_r8
     1   (ch_string,n_char_string,
     3    r8, itype,
     4    n_real,
     5    n_dec,
     6    n_int,
     7    n_space)
C
C#######################################################################
C
C     PURPOSE -
C
C     INPUT ARGUMENTS -
C
C     OUTPUT ARGUMENTS -
C     
C     LIMITATIONS -
C
C#######################################################################
C
      implicit none
c
      character*(*) ch_string
      character*5   ch_format_i
      character*5   ch_format_l
      character*3   ch_format_r
      
      character*2 ch_format_num(30)
      character*3 ch_format_numrp(30)
      character*2 ch_format_type(4)
      character*1 ch_format_lp
      character*1 ch_format_rp
      character*1 ch_format_p
      
      data ch_format_numrp(1) / '01)' /
      data ch_format_numrp(2) / '02)' /
      data ch_format_numrp(3) / '03)' /
      data ch_format_numrp(4) / '04)' /
      data ch_format_numrp(5) / '05)' /
      data ch_format_numrp(6) / '06)' /
      data ch_format_numrp(7) / '07)' /
      data ch_format_numrp(8) / '08)' /
      data ch_format_numrp(9) / '09)' /
      data ch_format_numrp(10) / '10)' /
      data ch_format_numrp(11) / '11)' /
      data ch_format_numrp(12) / '12)' /
      data ch_format_numrp(13) / '13)' /
      data ch_format_numrp(14) / '14)' /
      data ch_format_numrp(15) / '15)' /
      data ch_format_numrp(16) / '16)' /
      data ch_format_numrp(17) / '17)' /
      data ch_format_numrp(18) / '18)' /
      data ch_format_numrp(19) / '19)' /
      data ch_format_numrp(20) / '20)' /
      data ch_format_numrp(21) / '21)' /
      data ch_format_numrp(22) / '22)' /
      data ch_format_numrp(23) / '23)' /
      data ch_format_numrp(24) / '24)' /
      data ch_format_numrp(25) / '25)' /
      data ch_format_numrp(26) / '26)' /
      data ch_format_numrp(27) / '27)' /
      data ch_format_numrp(28) / '28)' /
      data ch_format_numrp(29) / '29)' /
      data ch_format_numrp(30) / '30)' /

      data ch_format_num(1) / '01' /
      data ch_format_num(2) / '02' /
      data ch_format_num(3) / '03' /
      data ch_format_num(4) / '04' /
      data ch_format_num(5) / '05' /
      data ch_format_num(6) / '06' /
      data ch_format_num(7) / '07' /
      data ch_format_num(8) / '08' /
      data ch_format_num(9) / '09' /
      data ch_format_num(10) / '10' /
      data ch_format_num(11) / '11' /
      data ch_format_num(12) / '12' /
      data ch_format_num(13) / '13' /
      data ch_format_num(14) / '14' /
      data ch_format_num(15) / '15' /
      data ch_format_num(16) / '16' /
      data ch_format_num(17) / '17' /
      data ch_format_num(18) / '18' /
      data ch_format_num(19) / '19' /
      data ch_format_num(20) / '20' /
      data ch_format_num(21) / '21' /
      data ch_format_num(22) / '22' /
      data ch_format_num(23) / '23' /
      data ch_format_num(24) / '24' /
      data ch_format_num(25) / '25' /
      data ch_format_num(26) / '26' /
      data ch_format_num(27) / '27' /
      data ch_format_num(28) / '28' /
      data ch_format_num(29) / '29' /
      data ch_format_num(30) / '30' /

      data ch_format_type(1) / '(i' /
      data ch_format_type(2) / '(e' /
      data ch_format_type(3) / '(a' /
      data ch_format_type(4) / '(f' /

      data ch_format_lp / '(' /
      data ch_format_rp / ')' /
      data ch_format_p  / '.' /

      integer itype, n_real, n_dec, n_int, n_space, n_char_string
      real*8 r8
      integer length_string, length_filled, istart, istop, i
      integer len
      integer icharlnf, icharlnb
c
      length_filled = n_char_string
      istart = length_filled + 1

      if((n_space .gt. 0).and.(length_filled .ne. 0))then
         istart = length_filled + 1
         do i = 1, n_space
            write(ch_string(istart:istart),'(a1)')' '
            istart = istart  + 1
         enddo
         length_filled = istart - 1
      endif

      if(itype .eq. 1)then
         istop = istart + n_int - 1
         write(ch_string(istart:istop),
     *         ch_format_type(1) // 
     *         ch_format_numrp(n_int) 
     *         )nint(r8)
      elseif(itype .eq. 2)then
         istop = istart + n_real - 1
         write(ch_string(istart:istop),
     *         ch_format_type(2) // 
     *         ch_format_num(n_real) // 
     *         ch_format_p // 
     *         ch_format_numrp(n_dec) 
     *         )r8
      elseif(itype .eq. 3)then

      elseif(itype .eq. 4)then
         istop = istart + n_real - 1
         write(ch_string(istart:istop),
     *         ch_format_type(4) // 
     *         ch_format_num(n_real) // 
     *         ch_format_p // 
     *         ch_format_numrp(n_dec)
     *         )r8
      endif
      
      n_char_string = istop
      return
      end


      subroutine string_write(ch_string, n_char_string, lu, if_clear)
C
C#######################################################################
C
C     PURPOSE -
C
C     Write character string to ascii output file
C
C     INPUT ARGUMENTS -
C
C     ch_string = character string for output
C     lu        = logical unit number of for output
C     if_clear  = flag to indicate if:
C     If if_clear = 1 fill the string with blanks
C     If if_clear .ne. 1 do not alter string
C
C     OUTPUT ARGUMENTS - (None)
C     
C     LIMITATIONS -
C
C     This will work for strings up to length_filled = 9,999,999
C#######################################################################
C
      implicit none
      character*(*) ch_string
      character*8 ch_format
      character*1 ch_left, ch_right
      integer n_char_string, lu, if_clear
      integer length_filled
      integer icharlnf, icharlnb

      ch_left  = '('
      ch_right = ')'
      
      length_filled = n_char_string

      ch_format(1:1) = 'a'
      write(ch_format(2:8),'(i7.7)')length_filled
      write(lu,
     *      ch_left//
     *      ch_format//
     *      ch_right
     *      )ch_string(1:length_filled)

      if(if_clear .eq. 1)call string_clear(ch_string)

      return
      end
      subroutine string_clear(ch_string)
C
C#######################################################################
C
C     PURPOSE -
C
C     Fill a character string with blanks.
C
C     INPUT ARGUMENTS -
C
C     ch_string = character string
C
C     OUTPUT ARGUMENTS - (None)
C     
C#######################################################################
C
      implicit none
      character*(*) ch_string
      integer length_string, i
      integer len
      
      length_string = len(ch_string)
      do i = 1, length_string
         write(ch_string(i:i),'(a1)')' '
      enddo
      return
      end
      
