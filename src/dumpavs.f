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
C        Nov 2026: restructured (the AVS cell types and vertex orders
C        from avs_cell_table, shared with readavs; computed number
C        widths; one loop for the vertex and the cell attributes). The
C        files written are byte-identical to the earlier ones
C        (test/suite/test_avs_io/golden).
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
      integer nsd, nen, nef, nnodes, nelements, mbndry, ihcycle
      real*8 time, dthydro
      integer iopt_points, iopt_elements, iopt_values_node,
     *        iopt_values_elem, io_format
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet)
      integer itetclr(*), itettyp(*), itetoff(*), itet(*)
C
      pointer (ipxvalues, xvalues)
      real*8 xvalues(*)
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      integer icmo_pointer(*)
      real*8 xcmo_pointer(*)
      pointer (iadr, dat)
      real*8 dat(*)
C
      pointer (ipcnames, cnames)
      pointer (ipranks, iranks)
      pointer (iplengths, clengths)
      pointer (ipioflags, cioflags)
      pointer (ipisel, isel)
      pointer (ipioffv, ioffv)
      character*32 cnames(*), clengths(*), cioflags(*)
      integer iranks(*), isel(*), ioffv(*)
C
      pointer (ipitype_flag, itype_flag)
      pointer (ipimin_vint, imin_vint)
      pointer (ipimax_vint, imax_vint)
      pointer (ipn_int_i, n_int_i)
      integer itype_flag(*), imin_vint(*), imax_vint(*), n_int_i(*)
C
      character*32 isubname, ctype, cinterp, crank, cpers, clen
      character*132 logmess
      character*8 cname
      character*4096 ch_string
      character*30 cfmt_point, cfmt_cell
      character*2 chash
      real*8 xyz(3), cutoff, maxval, r8, cinterpolate
      integer icharlnf, avs_width
      integer i, j, k, ic, it, iindex, ierr, icscode, ierror_return,
     *  ilen, ityp, iunit, nmcmoatt, nvalues, nvaluese, nelem,
     *  io_fmt, io_format_node, io_format_elem, nnodes_io,
     *  nelements_io, nvalues_node, nvalues_elem, iw, ivw, isec,
     *  nent, iopt, ihash, nsel, irowlen, len2, itypout, irank,
     *  length, mmlength, lent, ncolumn_max, n_real, n_dec, n_int,
     *  n_space, n_char_string, if_clear, lname, ifelm, nvert,
     *  njtet, lperm(8), ioff, ivert(8), kc, ktab(0:20), ifk
      character*8 ctab(8)
      integer ltab(8), ntab(8), ptab(8,8), njk
      character*1 cxyz(3)
      data cxyz / 'x', 'y', 'z' /
C
      data cutoff/1.e-30/
      data maxval/1.d+30/
C
C ######################################################################
C begin
C
      if ((nnodes .eq. 0) .and. (nelements .eq. 0)) then
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
      call hassign(iunit,ifile,ierr)
      if (iunit.lt.0 .or. ierr.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        write(logmess,'(a)')'WARNING: No output'
        call writloga('default',0,logmess,0,icscode)
        return
      endif
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
C
C     The options. io_format 3 and 4 (att_node, att_elem) start the
C     header lines with "#". The arguments are not changed: local
C     copies (nelem, io_fmt).
C
      nelem = nelements
      io_fmt = io_format
      io_format_node = 0
      io_format_elem = 0
      if (iopt_elements .eq. 3) then
C        pt cells for a mesh of vertices only
         if (nnodes.gt.0 .and. nelements.gt.0) then
           write(logmess,'(a)')'WARNING: AVS UCD PT No output'
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a)')
     *    'Expecting points but mesh has elements and connectivity.'
           call writloga('default',0,logmess,0,ierr)
           close(iunit)
           return
         endif
         nelem = nnodes
         io_fmt = 2
      elseif (io_format .eq. 3) then
         io_fmt = 2
         io_format_node = 1
      elseif (io_format .eq. 4) then
         io_fmt = 2
         io_format_elem = 1
      endif
C
C     ******************************************************************
C     The attributes written: ioflag holds 'a'; length nnodes (vertex
C     attributes) or nelements (cell attributes).
C
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   icscode)
C
c     Hardwire turning off output of -def- field.
      call dotaskx3d
     1 ('cmo/modatt/-def-/-def-/ioflag/x;finish',icscode)
      call mmgetblk('cnames',isubname,ipcnames,nmcmoatt*8,1,icscode)
      call mmgetblk('clengths',isubname,iplengths,nmcmoatt*8,1,icscode)
      call mmgetblk('cioflags',isubname,ipioflags,nmcmoatt*8,1,icscode)
      call mmgetblk('iranks',isubname,ipranks,nmcmoatt,1,icscode)
      call mmgetblk('isel',isubname,ipisel,nmcmoatt,1,icscode)
      call mmgetblk('ioffv',isubname,ipioffv,nmcmoatt,1,icscode)
      call mmgetblk
     1   ('itype_flag',isubname,ipitype_flag,nmcmoatt,1,icscode)
      call mmgetblk
     1   ('imin_vint', isubname,ipimin_vint, nmcmoatt,1,icscode)
      call mmgetblk
     1   ('imax_vint', isubname,ipimax_vint, nmcmoatt,1,icscode)
      call mmgetblk
     1   ('n_int_i',   isubname,ipn_int_i,   nmcmoatt,1,icscode)
C
      nvalues=0
      nvaluese=0
      do i=1,nmcmoatt
         call cmo_get_attribute_name(cmo,i,cnames(i),icscode)
         call cmo_get_attparam(cnames(i),cmo,iindex,ctype,crank,
     *    clengths(i),cinterp,cpers,cioflags(i),ierror_return)
         call cmo_get_info(crank,cmo,iranks(i),ilen,ityp,icscode)
C
C        Vectors (rank 3) are not written; see the history of this
C        file for the format they would need.
         if (crank(1:6) .eq. "vector") then
            write(logmess,"(a)")
     1     'ERROR dumpavs: vector output not supported.'
            call writloga('default',1,logmess,0,ierr)
            write(logmess,"(a)")
     1     'convert vector to scalars with cmo/addatt'
            call writloga('default',0,logmess,0,ierr)
            write(logmess,"(a)")
     1     'ERROR dumpavs: No Action '
            call writloga('default',0,logmess,1,ierr)
            goto 9999
         endif
         if (index(cioflags(i),'a') .gt. 0) then
            if (clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes')
     *         nvalues=nvalues+1
            if (clengths(i)(1:icharlnf(clengths(i))).eq.'nelements')
     *         nvaluese=nvaluese+1
         endif
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
C     ******************************************************************
C     The header: vertices, cells, vertex attributes, cell
C     attributes, model data (always 0); a part not written counts 0.
C
      nnodes_io = 0
      nelements_io = 0
      nvalues_node = 0
      nvalues_elem = 0
      if (iopt_points .ge. 1) nnodes_io = nnodes
      if (iopt_elements .ge. 1) nelements_io = nelem
      if (iopt_values_node .ge. 1) nvalues_node = nvalues
      if (iopt_values_elem .ge. 1) nvalues_elem = nvaluese
      if ((io_format_node .ne. 0) .or. (io_format_elem .ne. 0)) then
         write(iunit,9029) '# ',
     *     nnodes_io,nelements_io,nvalues_node,nvalues_elem,0
      else
         write(iunit,9030)
     *     nnodes_io,nelements_io,nvalues_node,nvalues_elem,0
      endif
 9029 format(a2,5(i10,1x))
 9030 format(5(i10,1x))
C
C     ******************************************************************
C     The vertices: number (iopt_points 1, as wide as needed), x, y, z.
C     |value| below 1e-30 is written as 0, beyond 1e30 as +-1e30.
C
      if (iopt_points .ge. 1) then
         iw = avs_width(nnodes)
         if (iopt_points .eq. 1) then
            write(cfmt_point,'(a,i2.2,a,i2.2,a)') '(i',iw,'.',iw,
     *         ',1x,3(1pe20.12))'
         else
            cfmt_point = '(3(1pe20.12))'
         endif
         do i=1,nnodes
            xyz(1)=xic(i)
            xyz(2)=yic(i)
            xyz(3)=zic(i)
            do k=1,3
               if (abs(xyz(k)).lt.cutoff) xyz(k)=0.0
               if (abs(xyz(k)).gt.maxval) then
                  write(logmess,9035) cxyz(k),i,xyz(k)
 9035             format(1x,a1,' coordinate of vertex ',i10,
     *                   ' written as +-1.e+30: ',e12.5)
                  call writloga('default',0,logmess,0,icscode)
                  xyz(k)=sign(maxval,xyz(k))
               endif
            enddo
            if (iopt_points .eq. 1) then
               write(iunit,cfmt_point) i,xyz(1),xyz(2),xyz(3)
            elseif (iopt_points .eq. 2) then
               write(iunit,cfmt_point) xyz(1),xyz(2),xyz(3)
            endif
         enddo
      endif
C
C     ******************************************************************
C     The cells: number (iopt_elements 1 and 3), material, AVS type,
C     vertices in AVS order (avs_cell_table). iopt_elements 3 writes
C     one pt cell per vertex (material 1), for a mesh of vertices
C     only (Paraview draws them). Cells of other types are not
C     written.
C
      if (iopt_elements .ge. 1) then
C        the table once: ktab(LaGriT type) = its entry, 0 for none
         do kc=0,20
            ktab(kc)=0
         enddo
         do kc=1,8
            call avs_cell_table(kc,ctab(kc),ltab(kc),ifk,ntab(kc),
     *                          njk,ptab(1,kc))
            if (ifk.ge.0 .and. ifk.le.20) ktab(ifk)=kc
         enddo
         iw = avs_width(nelem)
         ivw = avs_width(nnodes) - 1
         if (iopt_elements.eq.1 .or. iopt_elements.eq.3) then
            write(cfmt_cell,'(a,i2.2,a,i2.2,a)') '(i',iw,'.',iw,','
         else
            cfmt_cell = '(       '
         endif
         write(cfmt_cell(9:30),'(a,i2.2,a)') '1x,i8,1x,a5,8(1x,i',ivw,
     *      '))'
         do it=1,nelem
            if (iopt_elements .eq. 3) then
               write(iunit,cfmt_cell) it,1,'pt',it
            else
               ic = 0
               if (itettyp(it).ge.0 .and. itettyp(it).le.20)
     *            ic = ktab(itettyp(it))
               if (ic .gt. 0) then
                  nvert = ntab(ic)
                  lname = ltab(ic)
                  ioff = itetoff(it)
                  do k=1,nvert
                     ivert(k) = itet(ioff+ptab(k,ic))
                  enddo
                  if (iopt_elements .eq. 1) then
                     write(iunit,cfmt_cell) it,itetclr(it),
     *                  ctab(ic)(1:lname),(ivert(k),k=1,nvert)
                  elseif (iopt_elements .eq. 2) then
                     write(iunit,cfmt_cell) itetclr(it),
     *                  ctab(ic)(1:lname),(ivert(k),k=1,nvert)
                  endif
               endif
            endif
         enddo
      endif
C
C     ******************************************************************
C     The vertex attributes (isec 1), then the cell attributes (isec
C     2): the count and ranks, one "name, type" line each, then one
C     row per vertex or cell (iopt 1: with its number; 2: without).
C     avs1 (io_format 1) writes every value as a real; avs2 (2)
C     writes integers as integers, as wide as the largest |value|.
C     imin_vint and imax_vint carry over from the vertex to the cell
C     attributes, so the widths of cell attribute k also cover vertex
C     attribute k (as before; kept so that files do not change).
C
      do isec=1,2
         if (isec .eq. 1) then
            clen = 'nnodes'
            nent = nnodes
            iopt = iopt_values_node
            ihash = io_format_node
         else
            clen = 'nelements'
            nent = nelem
            iopt = iopt_values_elem
            ihash = io_format_elem
         endif
         nsel = 0
         irowlen = 0
         do i=1,nmcmoatt
            if (index(cioflags(i),'a').gt.0 .and.
     *          clengths(i)(1:icharlnf(clengths(i))).eq.
     *          clen(1:icharlnf(clen))) then
               nsel = nsel+1
               isel(nsel) = i
               ioffv(nsel) = irowlen
               irowlen = irowlen + iranks(i)
            endif
         enddo
         if (iopt.eq.0 .or. nsel.eq.0) goto 500
C
         chash = ' '
         if (ihash .eq. 1) chash = '# '
         if (ihash .eq. 0) then
            write(iunit,9060) nsel,(iranks(isel(i)),i=1,nsel)
         else
            write(iunit,9061) chash, nsel,(iranks(isel(i)),i=1,nsel)
         endif
 9060    format(i5.5,100(1x,i2))
 9061    format(a,i5.5,100(1x,i2))
         do i=1,nsel
            len2=icharlnf(cnames(isel(i)))
            call mmfindbk(cnames(isel(i)),cmo,iadr,length,icscode)
            call mmgettyp(iadr,itypout,icscode)
            if (itypout.eq.1) then
               ctype = ', integer '
               lent = 10
            elseif (itypout.eq.2) then
               ctype = ', real '
               lent = 7
            else
               ctype = ', no units'
               lent = 10
            endif
            if (ihash .eq. 0) then
               write(iunit,9070) cnames(isel(i))(1:len2),ctype(1:lent)
            else
               write(iunit,9071) chash, cnames(isel(i))(1:len2),
     *            ctype(1:lent)
            endif
 9070       format(a,a)
 9071       format(a,a,a)
         enddo
C
C        the values, one row of irowlen per vertex or cell
         if (irowlen*nent .le. 0) goto 500
         if (isec .eq. 1) then
            call mmgetblk('xvalues',isubname,ipxvalues,irowlen*nent,2,
     *                    icscode)
         else
            call mmgetblk('xvaluese',isubname,ipxvalues,irowlen*nent,
     *                    2,icscode)
         endif
         do i=1,nsel
            call cmo_get_attparam(cnames(isel(i)),cmo,iindex,ctype,
     *         crank,clengths(isel(i)),cinterp,cpers,cioflags(isel(i)),
     *         ierror_return)
            lent=icharlnf(ctype)
            if (ctype(1:lent).ne.'VINT' .and.
     *          ctype(1:lent).ne.'VDOUBLE') goto 400
            if (ctype(1:lent).eq.'VINT') then
               itype_flag(i) = 1
            else
               itype_flag(i) = 2
            endif
            call cmo_get_length(cnames(isel(i)),cmo,length,irank,
     *                          ierror_return)
            mmlength=irank*length
            call mmfindbk(cnames(isel(i)),cmo,ipcmo_pointer,mmlength,
     *                    ierror_return)
            if (ierror_return.ne.0) then
               call x3d_error(isubname,'mmfindbk')
               goto 400
            endif
            if (length.ne.nent) then
               if (isec.eq.1 .and. itype_flag(i).eq.2 .and.
     *             length.eq.nelem) goto 400
               call x3d_error(isubname,'Illegal cmo_name')
               goto 400
            endif
            ioff = ioffv(i)
            if (itype_flag(i) .eq. 1) then
               do j=1,length
                  do k=1,irank
                     xvalues(ioff+irowlen*(j-1)+k)=
     *                  icmo_pointer(irank*(j-1)+k)
                     imin_vint(i)=min(imin_vint(i),
     *                  icmo_pointer(irank*(j-1)+k))
                     imax_vint(i)=max(imax_vint(i),
     *                  icmo_pointer(irank*(j-1)+k))
                  enddo
               enddo
               imax_vint(i)=max(abs(imin_vint(i)),abs(imax_vint(i)))
            else
               do j=1,length
                  do k=1,irank
                     xvalues(ioff+irowlen*(j-1)+k)=
     *                  cinterpolate('function',cinterp,
     *                  xcmo_pointer(irank*(j-1)+k))
                  enddo
               enddo
            endif
 400        continue
         enddo
C
         if (io_fmt .eq. 1) then
C           avs1: every value a real, 100 to a line
            ncolumn_max = 100
            do it=1,nent
               if (iopt .eq. 1) then
                  write(iunit,9080) it,
     *              (xvalues(i+irowlen*(it-1)),
     *               i=1,min(ncolumn_max,irowlen))
               elseif (iopt .eq. 2) then
                  write(iunit,9081)
     *              (xvalues(i+irowlen*(it-1)),
     *               i=1,min(ncolumn_max,irowlen))
               endif
               if (irowlen .gt. ncolumn_max) then
                  do j=ncolumn_max+1,irowlen,ncolumn_max
                     write(iunit,9085)
     *                (xvalues(i+irowlen*(it-1)),
     *                 i=j,min(j+ncolumn_max-1,irowlen))
                  enddo
               endif
            enddo
 9080       format(i10.10,100(1x,1pe19.12))
 9081       format(100(1pe19.12,1x))
 9085       format(100(1x,1pe19.12))
         elseif (io_fmt .eq. 2) then
C           avs2: integers as integers, packed into ch_string
            n_real   = 20
            n_dec    = 12
            n_space  =  0
            if_clear =  0
            do i = 1, nsel
               n_int_i(i) = nint(alog10( real(imax_vint(i)+1))) + 3
               if (n_int_i(i) .le. 3) n_int_i(i) = 3
            enddo
            do it=1,nent
               n_char_string = 0
               if (iopt .eq. 1) then
                  n_int  = nint( alog10( real(nent)+1 ) ) + 3
                  r8 = it
                  call string_add_entry_r8
     1             (ch_string,n_char_string,r8,1,n_real,n_dec,n_int,
     2              n_space)
               endif
               do i = 1, nsel
                  n_int = n_int_i(i)
                  call string_add_entry_r8
     1             (ch_string,n_char_string,xvalues(i+irowlen*(it-1)),
     2              itype_flag(i),n_real,n_dec,n_int,n_space)
               enddo
               call string_write(ch_string, n_char_string, iunit,
     *                           if_clear)
            enddo
         endif
 500     continue
      enddo
C
 9999 close(iunit)
      call mmrelprt(isubname,icscode)
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
      
