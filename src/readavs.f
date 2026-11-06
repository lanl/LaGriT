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
C         Nov 2026: restructured: the cell types and vertex orders
C         from avs_cell_table (shared with dumpavs), one line reader
C         (avs_getline: comments, errors, long lines), one routine for
C         the vertex and the cell attributes (avs_read_atts), each
C         line read once. Meshes read are the same as before
C         (test/suite/test_avs_io).
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
C
      include "chydro.h"
      include "local_element.h"
C
      character*(*) ifile
      integer iopt_nodes, iopt_elements, iopt_values, ierror
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
      integer imt1(*), itp1(*), icr1(*), isn1(*), itet1(*), jtet1(*)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*), itetoff(*), jtetoff(*)
      pointer (ipireal1, ireal1)
      integer ireal1(*)
      pointer (ipidone, idone)
      integer idone(*)
      pointer (ipidrop, idrop)
      integer idrop(*)
C
      character*132 logmess
      character*32 isubname, cmonam
      character*200 cbuff
      character*24 csect
      character*32 cbadtype
      character*8 cname
C     one line of the file (avs_getline): cline(1:nline)
      character*1024 cline
      integer nline, ios, ilong
C
      integer icharlnf
      integer nnodes, nelements, mbndry, ilen, ity, ier, ierr, ierr1,
     *  itp, icscode, lenfile, iunit, isetimt1, isetitp1, iseticr1,
     *  isetisn1, npoints, nelemavs, nvalues, nvalues_e, nelemfile,
     *  i, j, k, i1, ih, it, jt, jf, node1, iline, imat, ifound,
     *  istart, ifelm, ifelmnew, ihybrid, itoff, jtoff, ielements,
     *  nen, nef, nee, nsdgeom, nsdtopo, if_zero_based, i_offset,
     *  i1min, i1max, nbad, ibadfirst, lname, nvert, njtet, lperm(8),
     *  ivert(8), kk, numtet, numtet1, itetshift, imt1min, imt1max,
     *  length, ierrdum, nvalues1, ifk, njk
      character*8 ctab(8)
      integer ltab(8), ntab(8), jtab(8), ftab(8), ptab(8,8)
      real*8 x1, y1, z1
      integer ipos, ierrp, iv(2)
      real*8 rv(3)
C
C ######################################################################
C
      isubname='readavs'
      ierror=0
      call cmo_get_name(cmonam,ierror)
      if (ierror.ne.0 .or. cmonam(1:8).eq.'-notset-') then
         lenfile=icharlnf(cmonam)
         write(logmess,9001) cmonam(1:lenfile)
 9001    format('The target mesh , ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierr)
         goto 9999
      endif
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,ity,ier)
      call cmo_get_info('nelements',cmonam,nelements,ilen,ity,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,ity,ier)
C
C     The mesh object must be empty: read / avs does not add to a mesh
C     (addmesh / append joins two meshes).
C
      if (nnodes.gt.0 .or. nelements.gt.0) then
         write(logmess,'(a,a,a)') 'ERROR read/avs: mesh object ',
     *      cmonam(1:icharlnf(cmonam)),' is not empty'
         call writloga('default',1,logmess,0,ierr)
         write(logmess,'(a)')
     *      'ERROR read/avs: read into a new mesh object; '//
     *      'addmesh / append joins meshes'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)') 'ERROR read/avs: nothing read'
         call writloga('default',0,logmess,1,ierr)
         ierror=1
         goto 9999
      endif
C
      isetimt1=0
      isetitp1=0
      iseticr1=0
      isetisn1=0
      ihybrid=0
      ielements=0
      itoff=0
      jtoff=0
      nbad=0
      ibadfirst=0
      nelemfile=0
      nelemavs=0
      npoints=0
      nvalues=0
      nvalues_e=0
      i_offset=0
      i1min=0
      i1max=0
      nen=0
      nef=0
C
C     ******************************************************************
C     Open the file.
C
      lenfile=icharlnf(ifile)
      call fexist(ifile(1:lenfile),ierr1)
      if(ierr1.eq.0) then
         write(logmess,9000) ifile(1:lenfile)
 9000    format('The AVS file, ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierr)
         goto 9999
      endif
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (ierror.ne.0 .or. iunit.lt.0) then
         write(logmess,*) 'hassign bad file no for '
     &         //ifile(1:lenfile)
         call writloga('default',1,logmess,0,ierr)
         goto 9999
      endif
C
C     ******************************************************************
C     The header: vertices, cells, vertex and cell attributes. A first
C     line "run" or "finish" ends the file.
C
      csect='header'
      call avs_getline(iunit,cline,nline,ilong,ios)
      if (ios.ne.0) goto 9990
      if(cline(1:3).eq.'run') goto 9998
      if(cline(1:6).eq.'finish') goto 9998
      read(cline(1:nline),*,err=9990,end=9990)
     *   npoints,nelemavs,nvalues,nvalues_e
      nelemfile=nelemavs
      i1min=npoints
      i1max=0
C
C     Nothing more is read without vertices.
      if (npoints.le.0) goto 600
C
C     ******************************************************************
C     The vertices, numbered from 0 or from 1, one after another (the
C     cells refer to them by these numbers).
C
      csect='vertices'
      if_zero_based = -1
      do iline=1,npoints
         call avs_getline(iunit,cline,nline,ilong,ios)
         if (ios.ne.0) goto 9990
C        avs_get_ints and avs_get_reals (avs_fast.c) read the plain
C        lines; anything else, the list-directed read as before
         ipos=1
         call avs_get_ints(cline,nline,ipos,1,iv,ierrp)
         if (ierrp.eq.0) call avs_get_reals(cline,nline,ipos,3,rv,ierrp)
         if (ierrp.eq.0) then
            i1=iv(1)
            x1=rv(1)
            y1=rv(2)
            z1=rv(3)
         else
            read(cline(1:nline),*,err=9990,end=9990) i1,x1,y1,z1
         endif
         if(if_zero_based .lt. 0)then
            if_zero_based = i1
            if(if_zero_based .ge. 2 .or. if_zero_based .lt. 0)then
               write(logmess,9003) if_zero_based
 9003          format('ERROR read/avs: the first vertex number is ',i10)
               call writloga('default',1,logmess,0,ierr)
               write(logmess,9004)
 9004          format
     1   ('ERROR read/avs: number the vertices 0, 1, 2, ... or 1, 2, 3,'
     2    ' ...')
               call writloga('default',0,logmess,0,ierr)
               goto 9992
            endif
            i_offset = 1 - if_zero_based
         endif
         if(i1 .ne. if_zero_based+iline-1) then
            write(logmess,9005) i1, if_zero_based+iline-1
 9005       format('ERROR read/avs: vertex number ',i10,
     1             ' where ',i10,' was expected')
            call writloga('default',1,logmess,0,ierr)
            write(logmess,9004)
            call writloga('default',0,logmess,0,ierr)
            goto 9992
         endif
         i1min=min(i1min,i1+i_offset)
         i1max=max(i1max,i1+i_offset)
         if(iline.eq.1) then
            call cmo_set_info('nnodes',cmonam,npoints,1,1,ierror)
            call cmo_newlen(cmonam,ierror)
            call cmo_get_info('imt1',cmonam,ipimt1,ilen,itp,ier)
            call cmo_get_info('itp1',cmonam,ipitp1,ilen,itp,ier)
            call cmo_get_info('icr1',cmonam,ipicr1,ilen,itp,ier)
            call cmo_get_info('xic',cmonam,ipxic,ilen,itp,ierror)
            call cmo_get_info('yic',cmonam,ipyic,ilen,itp,ierror)
            call cmo_get_info('zic',cmonam,ipzic,ilen,itp,ierror)
         endif
         imt1(iline)=0
         itp1(iline)=0
         xic(iline)=x1
         yic(iline)=y1
         zic(iline)=z1
      enddo
      if(iopt_nodes.eq.0) then
         npoints=0
      endif
C
C     ******************************************************************
C     The cells: number, material, AVS type, vertices (avs_cell_table
C     gives the types and vertex orders). The type is the first of
C     the names followed by a blank on the line. A cell of another
C     type is skipped (WARNING below), and so is its row of cell
C     attributes.
C
      csect='cells'
      do kk=1,8
         call avs_cell_table(kk,ctab(kk),ltab(kk),ftab(kk),ntab(kk),
     *                       jtab(kk),ptab(1,kk))
      enddo
      if(nelemavs.gt.0) then
         if (iopt_elements.eq.1) then
C           tets until the first cell says otherwise
            nelements=nelemavs
            call cmo_set_info('nnodes',cmonam,npoints,1,1,ierror)
            call cmo_set_info('nelements',cmonam,nelements,1,1,ierror)
            nen=nelmnen(ifelmtet)
            nef=nelmnef(ifelmtet)
            nee=nelmnee(ifelmtet)
            call avs_cmo_shape(cmonam,3,3,nen,nef,nee,ipitetclr,
     *         ipitettyp,ipitetoff,ipjtetoff,ipitet,ipjtet)
         endif
         do iline=1,nelemavs
            call avs_getline(iunit,cline,nline,ilong,ios)
            if (ios.ne.0) goto 9990
            ielements=ielements+1
            if (iopt_elements.ne.1) goto 295
            ipos=1
            call avs_get_ints(cline,nline,ipos,2,iv,ierrp)
            if (ierrp.eq.0) then
               it=iv(1)
               imat=iv(2)
            else
               read(cline(1:nline),*,err=9990,end=9990) it,imat
            endif
            ifound=0
            istart=1
            do while (istart.lt.nline .and. ifound.eq.0)
C              every name starts with l, h, p, q or t
               if (index('lhpqt',cline(istart:istart)).eq.0) goto 208
               do kk=1,8
                  if (cline(istart:istart+ltab(kk)-1).eq.
     *                ctab(kk)(1:ltab(kk))) then
                     if (cline(istart+ltab(kk):istart+ltab(kk)).eq.' ')
     *                  then
                        ifound=kk
                        lname=ltab(kk)
                        goto 210
                     endif
                  endif
               enddo
C              "pri", as other codes write a prism
               if (cline(istart:istart+3).eq.'pri ') then
                  ifound=7
                  lname=3
                  goto 210
               endif
 208           continue
               istart=istart+1
            enddo
 210        continue
            if (ifound.ne.0) then
               ifelm=ftab(ifound)
               nvert=ntab(ifound)
               njtet=jtab(ifound)
               do k=1,8
                  lperm(k)=ptab(k,ifound)
               enddo
            endif
            if (ifound.eq.0) then
               ielements=ielements-1
               nelements=nelements-1
               if (nbad.eq.0) then
                  call mmgetblk('idrop',isubname,ipidrop,
     *                          nelemfile,1,icscode)
                  do i=1,nelemfile
                     idrop(i)=0
                  enddo
                  ibadfirst=iline
                  read(cline(1:nline),*,err=204,end=204) it,imat,
     *               cbadtype
                  goto 205
 204              cbadtype='?'
 205              continue
               endif
               nbad=nbad+1
               idrop(iline)=1
               goto 295
            endif
C
C           the mesh object's cell shape: from the first cell; hybrid
C           as soon as a cell of another size follows
            if (ielements.eq.1) then
               nen=nelmnen(ifelm)
               nef=nelmnef(ifelm)
               nee=nelmnee(ifelm)
               nsdtopo=3
               if (ifelm.eq.ifelmpnt .or. ifelm.eq.ifelmlin) nsdtopo=1
               if (ifelm.eq.ifelmtri .or. ifelm.eq.ifelmqud) nsdtopo=2
               call avs_cmo_shape(cmonam,3,nsdtopo,nen,nef,nee,
     *            ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,ipjtet)
            endif
            ifelmnew=ifelm
            if(ihybrid.eq.0 .and.
     *         (nelmnen(ifelmnew).ne.nen .or.
     *          nelmnef(ifelmnew).ne.nef)) then
               ihybrid=1
               nen=nelmnen(ifelmhyb)
               nef=nelmnef(ifelmhyb)
               nee=nelmnee(ifelmhyb)
               call avs_cmo_shape(cmonam,0,0,nen,nef,nee,
     *            ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,ipjtet)
            endif
C
            ipos=istart+lname
            call avs_get_ints(cline,nline,ipos,nvert,ivert,ierrp)
            if (ierrp.ne.0)
     *         read(cline(istart+lname:nline),*,err=9990,end=9990)
     *            (ivert(k),k=1,nvert)
            itetclr(ielements)=imat
            itettyp(ielements)=ifelm
            itetoff(ielements)=itoff
            jtetoff(ielements)=jtoff
            itoff=itoff+nelmnen(ifelm)
            jtoff=jtoff+nelmnef(ifelm)
            do k=1,nvert
               itet1(itetoff(ielements)+lperm(k))=ivert(k)+i_offset
            enddo
            do k=1,njtet
               jtet1(jtetoff(ielements)+k)=-1
            enddo
 295        continue
         enddo
      endif
      if (nbad.gt.0) then
         write(logmess,'(a,i10,a)') 'WARNING read/avs: ',nbad,
     *      ' cells of unknown type skipped'
         call writloga('default',1,logmess,0,ierr)
         write(logmess,'(a,i10,a,a)')
     *      'WARNING read/avs: the first is cell ',ibadfirst,
     *      ' of the file, type ',cbadtype(1:icharlnf(cbadtype))
         call writloga('default',0,logmess,1,ierr)
      endif
      if(nelemavs.eq.0) nelements=0
      if(iopt_elements.eq.0) then
         nelemavs=0
      elseif(iopt_elements.eq.1) then
         nelemavs=nelements
      endif
C
C     ******************************************************************
C     The vertex attributes. With none but cell attributes to come,
C     the section is the line "0" (a file without it is read too).
C
      csect='vertex attributes'
      if((nvalues.le.0) .and. (nvalues_e.le.0)) then
         goto 9998
      elseif((nvalues.le.0) .and. (nvalues_e.gt.0)) then
         read(iunit,*,err=9990,end=9990) nvalues1
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
            backspace(iunit)
         endif
      else
         call avs_read_atts(iunit,cmonam,'nnodes',nvalues,npoints,
     *      0,idrop,isetimt1,isetitp1,iseticr1,isetisn1,csect,ierr)
         if (ierr.eq.1) goto 9990
         if (ierr.eq.2) goto 9992
         if (ierr.eq.3) goto 9998
      endif
      if(iopt_values.eq.0) then
         nvalues=0
      endif
C
C     ******************************************************************
C     The cell attributes: one row per cell of the file (the rows of
C     skipped cells are read and dropped).
C
 600  continue
      csect='cell attributes'
      if(nvalues_e.gt.0) then
         if (nelemavs.le.0) nelemfile=0
         call avs_read_atts(iunit,cmonam,'nelements',nvalues_e,
     *      nelemfile,nbad,idrop,isetimt1,isetitp1,iseticr1,isetisn1,
     *      csect,ierr)
         if (ierr.eq.1) goto 9990
         if (ierr.eq.2) goto 9992
      endif
C
C     ******************************************************************
C     The mesh: compress the cells, the vertex materials and types,
C     jtet (geniee), dudded vertices.
C
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
C
C     A line that cannot be read, or the end of the file too early.
C
 9990 continue
      write(logmess,'(a,a)')
     *   'ERROR read/avs: a line cannot be read, or the file ends, in ',
     *   csect(1:len_trim(csect))
      call writloga('default',1,logmess,0,ierr)
C
C     Errors after the file is opened: close it and empty the mesh
C     object (it may hold part of the file).
C
 9992 continue
      close(iunit)
      call cmo_set_info('nnodes',cmonam,0,1,1,ier)
      call cmo_set_info('nelements',cmonam,0,1,1,ier)
      write(logmess,'(a,a,a)') 'ERROR read/avs: nothing read; ',
     *   cmonam(1:icharlnf(cmonam)),' is empty'
      call writloga('default',0,logmess,1,ierr)
      ierror=1
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
C
      subroutine avs_rank_warn(cwhere,cname,irank)
C
C     read / avs: a WARNING that an attribute of rank irank is skipped
C
      implicit none
      character*(*) cwhere, cname
      integer irank, icharlnf, ierr
      character*132 logmess
      write(logmess,'(a,a,a,a,a,i3,a)') 'WARNING read/avs: ',
     *   cwhere,' attribute ',cname(1:icharlnf(cname)),' has rank',
     *   irank,'; only rank 1 is read: skipped'
      call writloga('default',0,logmess,0,ierr)
      return
      end
C
      subroutine avs_getline(iunit, cline, nline, ilong, ios)
C
C     read / avs: the next line of the file that does not start with
C     "#", in cline(1:nline) (blanks follow it). A line that fills
C     cline (it may be longer) gives ilong = 1: backspace reads it
C     again from the file. ios: 0, or the iostat of the end of the
C     file or a read error.
C
      implicit none
      integer iunit, nline, ilong, ios
      character*(*) cline
 10   continue
      ilong=0
      read(iunit,'(a)',iostat=ios) cline
      if (ios.ne.0) then
         nline=0
         return
      endif
      if (cline(1:1).eq.'#') goto 10
      nline=len_trim(cline)
      if (nline.eq.len(cline)) ilong=1
      return
      end
C
      subroutine avs_cmo_shape(cmonam, nsdgeom, nsdtopo, nen, nef, nee,
     *   ipitetclr, ipitettyp, ipitetoff, ipjtetoff, ipitet, ipjtet)
C
C     read / avs: give mesh object cmonam cells of nen vertices, nef
C     faces and nee edges (and the dimensions, if nsdgeom > 0), and
C     return its cell arrays.
C
      implicit none
      character*(*) cmonam
      integer nsdgeom, nsdtopo, nen, nef, nee
      integer ipitetclr, ipitettyp, ipitetoff, ipjtetoff, ipitet,
     *        ipjtet
      integer ilen, itp, ier, ierror
      if (nsdgeom.gt.0) then
         call cmo_set_info('ndimensions_geom',cmonam,nsdgeom,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmonam,nsdtopo,1,1,ierror)
      endif
      call cmo_set_info('nodes_per_element',cmonam,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmonam,nef,1,1,ierror)
      call cmo_set_info('edges_per_element',cmonam,nee,1,1,ierror)
      call cmo_newlen(cmonam,ierror)
      call cmo_get_info('itetclr',cmonam,ipitetclr,ilen,itp,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,ilen,itp,ier)
      call cmo_get_info('itetoff',cmonam,ipitetoff,ilen,itp,ier)
      call cmo_get_info('jtetoff',cmonam,ipjtetoff,ilen,itp,ier)
      call cmo_get_info('itet',cmonam,ipitet,ilen,itp,ier)
      call cmo_get_info('jtet',cmonam,ipjtet,ilen,itp,ier)
      return
      end
C
      subroutine avs_read_atts(iunit, cmonam, clen, nexp, nrows, nbad,
     *   idrop, isetimt1, isetitp1, iseticr1, isetisn1, csect, ierr)
C
C     read / avs: one attribute section, vertex (clen 'nnodes') or
C     cell ('nelements'): the count and ranks, one "name, type" line
C     each, then nrows rows. A row of a skipped cell (idrop(row) = 1,
C     when nbad > 0) is read and dropped. An attribute the mesh
C     object lacks is made: VINT if its type is "integer" (with or
C     without a blank after the comma), else VDOUBLE. Ranks other than
C     1 are skipped (WARNING). For vertex attributes, xmt1 is read as
C     imt1, and isetimt1 ... isetisn1 say which of imt1, itp1, icr1
C     and isn1 the file gave.
C
C     ierr: 0 done; 1 a line cannot be read or the file ends (the
C     caller reports it, with csect); 2 an error reported here; 3 an
C     attribute of a type other than integer or real (reading stops,
C     the mesh is kept, as before).
C
      implicit none
      integer iunit, nexp, nrows, nbad, isetimt1, isetitp1, iseticr1,
     *        isetisn1, ierr
      character*(*) cmonam, clen, csect
      integer idrop(*)
C
      pointer (ipout, iout)
      integer iout(*)
      pointer (ipiarray, iarray)
      pointer (ipxarray, xarray)
      integer iarray(*)
      real*8 xarray(*)
      pointer (ipirank, irank)
      pointer (ipiatt, iatt_type)
      pointer (ipidat, isdatptr)
      pointer (ipxrow, xrow)
      integer irank(*), iatt_type(*), isdatptr(*)
      real*8 xrow(*)
C
      character*1024 cline
      character*132 logmess, ctype
      character*200 cbuff
      character*32 isubname, cmoatt, cwhere
      integer ipos, iv(1), iindex
      character*32 cptype, cprank, cplen, cpint, cppers, cpio
      integer nline, ilong, ios, nvals, i, j, k, ic, len1, nsum, irow,
     *  lenout, icscode, itypeout, ierror, i1, iwhere
      integer icharlnf
C
      isubname='avs_read_atts'
      ierr=0
      if (clen(1:6).eq.'nnodes') then
         cwhere='vertex'
         iwhere=1
      else
         cwhere='cell'
         iwhere=2
      endif
C
C     the count and the ranks
      call avs_getline(iunit,cline,nline,ilong,ios)
      if (ios.ne.0) goto 900
      call mmgetblk('irank',isubname,ipirank,max(1,nexp),1,icscode)
      read(cline(1:nline),*,iostat=ios) nvals,(irank(i),i=1,nexp)
      if (ios.ne.0 .or. ilong.ne.0) then
         backspace(iunit)
         read(iunit,*,err=900,end=900) nvals
         if (nvals.ne.nexp) goto 800
         backspace(iunit)
         read(iunit,*,err=900,end=900) nvals,(irank(i),i=1,nvals)
      endif
 800  continue
      if (nvals.ne.nexp) then
         write(logmess,805) nexp, cwhere(1:icharlnf(cwhere))
 805     format('READAVS ERROR: File header says ',i10,1x,a,
     1          ' attributes')
         call writloga('default',1,logmess,0,ierror)
         write(logmess,806) nvals, cwhere(1:icharlnf(cwhere))
 806     format('READAVS ERROR: Data header says ',i10,1x,a,
     1          ' attributes')
         call writloga('default',1,logmess,0,ierror)
         ierr=2
         goto 999
      endif
C
C     the names: make the attributes the mesh object lacks
      call mmgetblk('iatt_type',isubname,ipiatt,nvals,1,icscode)
      call mmgetblk('isdatptr',isubname,ipidat,nvals,1,icscode)
      do i=1,nvals
         call avs_getline(iunit,cline,nline,ilong,ios)
         if (ios.ne.0) goto 900
         len1=min(nline,icharlnf(cline))
         ic=1
         do while (ic.le.len1 .and. cline(ic:ic).ne.',')
            ic=ic+1
         enddo
         ic=ic-1
         cmoatt=cline(1:ic)
C        the type after the comma, with or without a space
         if (ic+2.le.len(cline)) then
            ctype=' '//adjustl(cline(ic+2:))
         else
            ctype=' '
         endif
         ctype(9:)=' '
         if (iwhere.eq.1) then
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
         endif
         iatt_type(i)=0
         if (irank(i).ne.1) then
            call avs_rank_warn(cwhere(1:icharlnf(cwhere)),cmoatt,
     *                         irank(i))
            goto 150
         endif
         call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
         if (icscode.eq.0) then
C           an attribute of that name, of the other length (a cell
C           attribute named like a vertex attribute, or the reverse):
C           skipped, since its values would not fit
            call cmo_get_attparam(cmoatt,cmonam,iindex,cptype,cprank,
     *         cplen,cpint,cppers,cpio,ierror)
            if (ierror.eq.0 .and. cplen(1:icharlnf(cplen)).ne.
     *          clen(1:icharlnf(clen))) then
               write(logmess,'(a,a,a,a,a)') 'WARNING read/avs: ',
     *            cwhere(1:icharlnf(cwhere)),' attribute ',
     *            cmoatt(1:icharlnf(cmoatt)),
     *            ' exists with the other length: skipped'
               call writloga('default',0,logmess,0,icscode)
               goto 150
            endif
         else
            if (ctype(1:icharlnf(ctype)).eq.' integer') then
               cbuff='cmo/addatt/' // cmonam(1:icharlnf(cmonam)) //
     *               '/' // cmoatt(1:icharlnf(cmoatt)) // '/VINT' //
     *               '/scalar/' // clen(1:icharlnf(clen)) //
     *               '/linear/permanent/gxaf/0.0 ; finish '
            else
               cbuff='cmo/addatt/' // cmonam(1:icharlnf(cmonam)) //
     *               '/' // cmoatt(1:icharlnf(cmoatt)) // '/VDOUBLE' //
     *               '/scalar/' // clen(1:icharlnf(clen)) //
     *               '/linear/permanent/gxaf/0.0 ; finish '
            endif
            call dotaskx3d(cbuff,ierror)
            call mmfindbk(cmoatt,cmonam,ipout,lenout,icscode)
         endif
         call mmgettyp(ipout,itypeout,icscode)
         if (itypeout.ne.1 .and. itypeout.ne.2) then
            write(logmess,'(a,a)')'Illegal attribute type ',
     *         cmoatt(1:icharlnf(cmoatt))
            call writloga('default',0,logmess,0,icscode)
            ierr=3
            goto 999
         endif
         isdatptr(i)=ipout
         iatt_type(i)=itypeout
 150     continue
      enddo
C
C     the rows: one line each (or more: then read from the file)
      nsum=0
      do j=1,nvals
         nsum=nsum+irank(j)
      enddo
      call mmgetblk('xrow',isubname,ipxrow,max(1,nsum),2,icscode)
      irow=0
      do i=1,nrows
         call avs_getline(iunit,cline,nline,ilong,ios)
         if (ios.ne.0) goto 900
         ios=1
         if (ilong.eq.0) then
            ipos=1
            call avs_get_ints(cline,nline,ipos,1,iv,ios)
            if (ios.eq.0)
     *         call avs_get_reals(cline,nline,ipos,nsum,xrow,ios)
            if (ios.ne.0)
     *         read(cline(1:nline),*,iostat=ios) i1,(xrow(j),j=1,nsum)
         endif
         if (ios.ne.0) then
            backspace(iunit)
            read(iunit,*,err=900,end=900) i1,(xrow(j),j=1,nsum)
         endif
         if (nbad.gt.0) then
            if (idrop(i).ne.0) goto 250
         endif
         irow=irow+1
         k=0
         do j=1,nvals
            k=k+irank(j)
            if (iatt_type(j).eq.1) then
               ipiarray=isdatptr(j)
               iarray(irow)=nint(xrow(k))
            elseif (iatt_type(j).eq.2) then
               ipxarray=isdatptr(j)
               xarray(irow)=xrow(k)
            endif
         enddo
 250     continue
      enddo
      goto 999
C
 900  ierr=1
 999  call mmrelprt(isubname,icscode)
      return
      end
