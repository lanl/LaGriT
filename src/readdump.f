      subroutine readdump(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                    ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Reads all the Dumps.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: readdump.f,v $
C         Revision 2.00  2007/11/09 20:03:59  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.43   18 Oct 2005 16:06:04   gable
CPVCS    Extend input file name length to 132 characters.
CPVCS    
CPVCS       Rev 1.42   22 Oct 2003 10:11:58   gable
CPVCS    Added GoCad option.
CPVCS    
CPVCS       Rev 1.41   29 Jun 2000 17:49:04   bap
CPVCS    Added logic to close both C and Fortran Files after they 
CPVCS    were read. If the files were already closed, it does nothing.
CPVCS    
CPVCS       Rev 1.40   21 Apr 2000 07:07:54   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.39   22 Mar 2000 10:44:28   gable
CPVCS    Added calls to setsize after all input formats
CPVCS    except lagrit and lagrit_old
CPVCS    
CPVCS       Rev 1.38   21 Mar 2000 12:59:02   dcg
CPVCS    if mesh objects already exist, add to list don't replace
CPVCS
CPVCS       Rev 1.37   Mon Feb 28 09:31:26 2000   dcg
CPVCS    add lagritold option
CPVCS
CPVCS       Rev 1.36   Tue Feb 08 14:24:14 2000   dcg
CPVCS    replace local_element
CPVCS
CPVCS       Rev 1.35   08 Feb 2000 08:58:30   dcg
CPVCS    make implicit none
CPVCS    remove unused .h includes
CPVCS
CPVCS       Rev 1.34   Tue Dec 14 10:23:44 1999   dcg
CPVCS    new option read/gmvfreeformat for ascii gmv files to
CPVCS    be read with read(*) type read statements
CPVCS
CPVCS       Rev 1.33   Fri Dec 03 11:55:04 1999   jtg
CPVCS    return if illegal read so doesn't crash on set_info_i
CPVCS
CPVCS       Rev 1.32   Tue Nov 30 16:39:18 1999   jtg
CPVCS    made lgt synonymous with lagrit
CPVCS
CPVCS       Rev 1.31   Wed Nov 24 13:32:38 1999   dcg
CPVCS    add read/ruby option
CPVCS
CPVCS       Rev 1.30   Fri Nov 05 13:27:42 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.29   Wed Aug 18 16:51:40 1999   dcg
CPVCS    use ierror_return
CPVCS
CPVCS       Rev 1.28   Wed Jul 14 15:09:52 1999   dcg
CPVCS    Detect if ascii or binary file - look for errors on input and
CPVCS    allow for some mistakes
CPVCS
CPVCS       Rev 1.27   Tue Jul 06 21:19:40 1999   jtg
CPVCS    make changes so that gmv dump will not crash code if
CPVCS    file does not exist, is empty, or there is no mesh object
CPVCS
CPVCS       Rev 1.26   Tue May 11 16:49:42 1999   dcg
CPVCS    set defaults for lagrit dumps (ascii) is default
CPVCS
CPVCS       Rev 1.25   Tue Mar 09 15:04:22 1999   dcg
CPVCS    read in cmo and pset, eset info for lagrit dumps
CPVCS
CPVCS       Rev 1.24   Fri Mar 05 11:16:10 1999   dcg
CPVCS    dump/lagrit generates new format dump
CPVCS
CPVCS       Rev 1.22   Fri Sep 18 11:04:18 1998   dcg
CPVCS    restore read_sheetij
CPVCS
CPVCS       Rev 1.20   Mon Sep 14 12:23:58 1998   tam
CPVCS    added read/sheetij option
CPVCS
CPVCS       Rev 1.19   Mon Aug 31 12:27:36 1998   dcg
CPVCS    remove unused and undocumented options
CPVCS
CPVCS       Rev 1.18   Mon Jan 26 11:02:02 1998   llt
CPVCS    Added readvrml call.
CPVCS
CPVCS       Rev 1.16   Thu Mar 06 21:48:28 1997   het
CPVCS    Add the dumprage and dumprtt dump formats.
CPVCS
CPVCS       Rev 1.15   Sun Feb 23 10:39:14 1997   het
CPVCS    Add the qlink read command.
CPVCS
CPVCS       Rev 1.14   Thu Oct 10 08:39:26 1996   het
CPVCS    Modify the parameter definition for the call read_iges().
CPVCS
CPVCS       Rev 1.13   Thu Feb 22 13:51:18 1996   dcg
CPVCS    remove call to dumpx3d_att
CPVCS
CPVCS       Rev 1.12   Tue Jan 30 17:19:00 1996   dcg
CPVCS    change call to readgmv_ascii from readgmv
CPVCS
CPVCS       Rev 1.11   Wed Jan 24 06:06:56 1996   het
CPVCS    Add the readnurbs_iges_grid routine
CPVCS
CPVCS       Rev 1.10   Tue Jan 23 09:33:10 1996   het
CPVCS    Add the read/iges files.
CPVCS
CPVCS       Rev 1.9   Fri Dec 22 14:19:36 1995   het
CPVCS    Fix an error with readavs
CPVCS
CPVCS       Rev 1.8   11/17/95 15:23:00   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.7   11/16/95 17:08:40   het
CPVCS    Create the getregv1 routine as a special case.
CPVCS
CPVCS       Rev 1.6   09/11/95 14:40:32   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.5   08/29/95 12:48:28   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.4   08/29/95 12:17:06   het
CPVCS    Add the cmowrk storage block for each CMO
CPVCS
CPVCS       Rev 1.3   07/31/95 13:37:34   dcg
CPVCS    change format to read/option
CPVCS
CPVCS       Rev 1.2   07/18/95 16:58:06   dcg
CPVCS    ibm changes
CPVCS
CPVCS       Rev 1.1   07/17/95 16:11:48   dcg
CPVCS    original version
C
C#######################################################################
C
C     implicit none
      implicit real*8 (a-h, o-z)
      include 'local_element.h'
C
C#######################################################################
C
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds), nnwds, icmsg
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C.... Subroutine name for memory management and errors.
C
      character*32 isubname
      character*8 cglobal, cdefault,iomode
C
C.... Mesh Object Data.
C
      character*32 cmo
C
C#######################################################################
C
      character*40 idsb
      character*32 ioption, ctest1
      character*132 ifile
      integer lenidsb
C
      character*132 logmess
C
C#######################################################################
C
      character*32 iword
C
      integer icharln
C
C#######################################################################
C
C     Variables for allowing the 3-token form of read
C     To add more filetypes for the short-form 3-token read,
C     follow these easy steps: Update filetypes and fileoptions
C     to arrays of the new larger size, and update numtypes 
C     similarly. Then, add the new file extensions and read options
C     that you want to support into the arrays.
      integer numtypes
      character*8 filetypes(6)
      character*32 fileoptions(6)
      integer typeindex
      character*132 filename
      character*8 fileext
      integer namelen
      integer extlen
      integer extindex
      character*128 newcommand
C
      filetypes(1) = 'inp'
      fileoptions(1) = 'avs'
      filetypes(2) = 'avs'
      fileoptions(2) = 'avs'
      filetypes(3) = 'lg'
      fileoptions(3) = 'lagrit'
      filetypes(4) = 'lagrit'
      fileoptions(4) = 'lagrit'
      filetypes(5) = 'gmv'
      fileoptions(5) = 'gmv'
      filetypes(6) = 'ts'
      fileoptions(6) = 'gocad'
      numtypes = 6
C#######################################################################
C
C     Define the subroutine name for memory management and errors.
C
      isubname='readdump'
      cglobal='global'
      cdefault='default'
C
C
      idsb = cmsgin(1)
      lenidsb=icharln(idsb)
      ioption=' '
      len1=1
      nnwds=nwds
      icmsg=3
      ierror_return=0
C
C
      if (nwds .eq. 3) then
C***Find the index of the '.' in the file name         
         extindex = index(cmsgin(2), '.', .TRUE.)
C***If there is no '.', it is not valid, so try other syntax         
         if (extindex .eq. 0) then
             write(logmess,'(a)') 'Second argument is not a filename. '
     *                        // 'Try again.'
             call writloga('default',1,logmess,0,ierr)
         else 
             filename = cmsgin(2)
             namelen = icharln(filename)
             fileext = filename(extindex+1:namelen)
             extlen = icharln(fileext)
C***Convert file extension to lowercase
             extindex = 1
             do while (extindex .le. icharln(fileext))
                 if (fileext(extindex:extindex).ge."A" 
     *                  .and. fileext(extindex:extindex).le."Z") then
                     fileext(extindex:extindex) = 
     *                   achar(iachar(fileext(extindex:extindex))+32)
                 endif
                 extindex = extindex + 1
             enddo
             typeindex = 1
C***Check lowercase extension against list of extensions
             do
                 if(fileext.eq.filetypes(typeindex))then
                     ioption = fileoptions(typeindex)
                     exit
                 endif
                 typeindex = typeindex + 1
                 if (typeindex > numtypes) then
                     write(logmess,'(a)') 'Unrecognized filetype for '
     *                                // '3-token read.'  
                     call writloga('default',1,logmess,0,ierr)
                     exit
                 endif
             enddo
C***Construct 4-token command and call it instead, if we
C***found a well-formed 3-token call.
             if (.not.ioption.eq.' ') then
                 newcommand = "read / " 
     *                      // ioption(1:icharln(ioption)) // " / " 
     *                      // filename(1:icharln(filename)) // " / " 
     *                      // cmsgin(3)(1:icharln(cmsgin(3)))
     *                      // "; finish"
                 call dotask(newcommand, ierror)
                 if (ierror .ne. 0) then
                     write(logmess, '(a)') 'ERROR! See above.'
                 endif
                 goto 9000
             endif
          endif
      endif

C***The rest of the code is for handling all other cases
      if(idsb(1:lenidsb).eq.'read') then
         ioption=cmsgin(2)
         len1=icharlnf(ioption)
         nnwds=nwds-1
         if (nwds.le.2) then
            ifile='-def-'
         else
            ifile=cmsgin(3)
         endif
         lenfile=icharlnf(ifile)
         icmsg=4
      else
         ifile=cmsgin(2)
         lenfile=icharlnf(ifile)
      endif
C
      if(ioption(1:len1).eq.'lagrit' .or. ioption(1:len1).eq.'lgt')then
         if(nwds.le.3) then
             cmo='-all-'
             iomode='ascii'
         elseif(nwds.le.4) then
             iomode='ascii'
             cmo=cmsgin(4)
             if (cmsgin(4)(1:5).eq.'-def-')  cmo='-all-'
             if (cmsgin(4)(1:5).eq.'ascii')  cmo='-all-'
             if (cmsgin(4)(1:6).eq.'binary') then
                cmo='-all-'
                iomode='binary'
             endif
         elseif(nwds.le.5) then
             if (cmsgin(4)(1:5).eq.'-def-')  cmo='-all-'
             cmo=cmsgin(4)
             if(cmsgin(5)(1:6).eq.'binary') then
                iomode='binary'
             else
                iomode='ascii'
             endif
         endif
C
         call read_lagrit(ifile(1:lenfile),cmo,iomode,ierror_return)
      elseif(ioption(1:len1).eq.'lagritold' )then
         if(nwds.le.3) then
             cmo='-all-'
             iomode='ascii'
         elseif(nwds.le.4) then
             iomode='ascii'
             if (cmsgin(4)(1:5).eq.'-def-')  cmo='-all-'
             if (cmsgin(4)(1:5).eq.'ascii')  cmo='-all-'
             if (cmsgin(4)(1:6).eq.'binary') then
                cmo='-all-'
                iomode='ascii'
             endif
         elseif(nwds.le.5) then
             if (cmsgin(4)(1:5).eq.'-def-')  cmo='-all-'
             if(cmsgin(5)(1:6).eq.'binary') then
                iomode='binary'
             else
                iomode='ascii'
             endif
         endif
C
         call read_lagrit_old(ifile(1:lenfile),cmo,iomode,ierror_return)
C
C
      elseif(ioption(1:len1).eq.'ruby') then
         nbinx=5
         nbiny=5
         nbinz=5
         cmo='3dmesh'
         if(nwds.ge.4) cmo=cmsgin(4)
         if(nwds.ge.7) then
           nbinx=imsgin(5)
           nbiny=imsgin(6)
           nbinz=imsgin(7)
         endif
         call readruby_lg(ifile,cmo,nbinx,nbiny,nbinz,ierror)
         call setsize()
      elseif(idsb(1:lenidsb).eq.'readdatex' .or.
     *     ioption(1:len1).eq.'datex'
     *     .or.idsb(1:lenidsb).eq.'readsimul'.or.
     *     ioption(1:len1).eq.'simul')  then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         call cmo_set_name(cmo,ierror)
         call cmo_set_info('nnodes',cmo,0,1,1,ierror)
         call cmo_set_info('nelements',cmo,0,1,1,ierror)
         call cmo_set_info('ndimensions_geom',cmo,1,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmo,1,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmo,1,1,1,ierror)
         call cmo_set_info('faces_per_element',cmo,1,1,1,ierror)
         call cmo_newlen(cmo,ierror)
         call readdatex(ifile(1:lenfile),ierror_return)
         call setsize()
C
      elseif(idsb(1:lenidsb).eq.'readgmv' .or.
     *       ioption(1:len1).eq.'gmv') then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         nsdgeom=3
         nsdtopo=3
         nen=nelmnen(ifelmtet)
         nef=nelmnen(ifelmtet)
         call cmo_set_name(cmo,ierror)
         if (ierror.ne.0) then
            write(logmess,*) 'GMV read error: no mesh object'
            call writloga('default',1,logmess,0,ierr)
            return
         endif
         call cmo_set_info('nnodes',cmo,0,1,1,ierror)
         call cmo_set_info('nelements',cmo,0,1,1,ierror)
         call cmo_set_info('ndimensions_geom',cmo,nsdgeom,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmo,nsdtopo,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
         call cmo_newlen(cmo,ierror)
         call fexist(ifile(1:lenfile),ierr1)
         if(ierr1.eq.0) then
            write(logmess,*) 'GMV read error: cannot find file '
     &         //ifile(1:lenfile)
            call writloga('default',1,logmess,0,ierr)
            return
         endif
         iunit=-1
         call hassign(iunit,ifile,ierror)
         read(iunit,'(a32)',err=900) iword
         goto 901
900      write(logmess,*) 'GMV read error: empty file'
         call writloga('default',1,logmess,0,ierr)
         return
901      continue
         close(iunit)
         if(iword(1:8).eq.'gmvinput'.and.iword(9:12).eq.'ieee') then
            call readgmv_binary(ifile(1:lenfile),ierror_return)
         else
            call readgmv_ascii(ifile(1:lenfile),ierror_return)
         endif
         call setsize()
C
      elseif(idsb(1:lenidsb).eq.'readgmvfreeformat' .or.
     *       ioption(1:len1).eq.'gmvfreeformat') then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         nsdgeom=3
         nsdtopo=3
         nen=nelmnen(ifelmtet)
         nef=nelmnen(ifelmtet)
         call cmo_set_name(cmo,ierror)
         if (ierror.ne.0) then
            write(logmess,*) 'GMV read error: no mesh object'
            call writloga('default',1,logmess,0,ierr)
            return
         endif
         call cmo_set_info('nnodes',cmo,0,1,1,ierror)
         call cmo_set_info('nelements',cmo,0,1,1,ierror)
         call cmo_set_info('ndimensions_geom',cmo,nsdgeom,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmo,nsdtopo,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
         call cmo_newlen(cmo,ierror)
         call fexist(ifile(1:lenfile),ierr1)
         if(ierr1.eq.0) then
            write(logmess,*) 'GMV read error: cannot find file '
     &         //ifile(1:lenfile)
            call writloga('default',1,logmess,0,ierr)
            return
         endif
         iunit=-1
         call hassign(iunit,ifile,ierror)
         read(iunit,'(a32)',err=910) iword
         goto 911
910      write(logmess,*) 'GMV read error: empty file'
         call writloga('default',1,logmess,0,ierr)
         return
911      continue
         close(iunit)
         if(iword(1:8).eq.'gmvinput'.and.iword(9:12).eq.'ieee') then
            call readgmv_binary(ifile(1:lenfile),ierror_return)
         else
            call readgmv_asciistar(ifile(1:lenfile),ierror_return)
         endif
         call setsize()
C
C     Read a GOCAD format TSURF file.
C     See: http://gocad.ensg.inpl-nancy.fr/IMG/papers/Fileform.fm.html
C
      elseif(ioption(1:len1).eq.'gocad') then
C
         call read_gocad_tsurf(imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
         call setsize()
C
      elseif(idsb(1:lenidsb).eq.'readavs' .or.
     *       ioption(1:len1).eq.'avs') then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_set_info('nnodes',cmo,0,1,1,ierror)
               call cmo_set_info('nelements',cmo,0,1,1,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         if (nnwds.le.3) then
            iopt_points=1
            iopt_elements=1
            iopt_values=1
         elseif (nnwds.le.4) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_points=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_points=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_points=1
            endif
            iopt_elements=1
            iopt_values=1
         elseif (nnwds.le.5) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_points=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_points=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_points=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_elements=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_elements=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_elements=1
            endif
            iopt_values=1
         elseif (nnwds.le.6) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_points=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_points=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_points=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_elements=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_elements=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_elements=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_values=max(0,min(1,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_values=max(zero,min(one,xmsgin(ist1)))
            else
               iopt_values=1
            endif
         endif
         call cmo_set_name(cmo,ierror)
         call readavs(ifile(1:lenfile),
     *                iopt_points,
     *                iopt_elements,
     *                iopt_values,
     *                ierror_return)
C
         call setsize()
C
      elseif(ioption(1:len1).eq.'iges_grid') then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_set_info('nnodes',cmo,0,1,1,ierror)
               call cmo_set_info('nelements',cmo,0,1,1,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         ksi=0
         ksj=0
         kli=0
         if (nnwds.le.3) then
            ist1=ist1+1
            ist1=ist1+1
            ist1=ist1+1
            iopt_nurbs=1
            iopt_nurbl=0
            iopt_nurbp=0
         elseif (nnwds.le.4) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_nurbs=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbs=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbs=1
            endif
            ist1=ist1+1
            ist1=ist1+1
            iopt_nurbl=0
            iopt_nurbp=0
         elseif (nnwds.le.5) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_nurbs=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbs=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbs=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_nurbl=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbl=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbl=0
            endif
            ist1=ist1+1
            iopt_nurbp=1
         elseif (nnwds.le.6) then
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_nurbs=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbs=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbs=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_nurbl=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbl=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbl=0
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_nurbp=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbp=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbp=0
            endif
         else
            ist1=5
            if(msgtype(ist1).eq.1) then
               iopt_nurbs=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbs=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbs=1
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_nurbl=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbl=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbl=0
            endif
            ist1=ist1+1
            if(msgtype(ist1).eq.1) then
               iopt_nurbp=max(0,min(2,imsgin(ist1)))
            elseif(msgtype(ist1).eq.2) then
               iopt_nurbp=max(zero,min(two,xmsgin(ist1)))
            else
               iopt_nurbp=0
            endif
            ist1=ist1+1
            ksi=imsgin(ist1)
            ist1=ist1+1
            ksj=imsgin(ist1)
            ist1=ist1+1
            kli=imsgin(ist1)
         endif
         iopt_nurbp=0
         call cmo_set_name(cmo,ierror)
         call readnurbs_iges_grid(ifile(1:lenfile),
     *                            iopt_nurbs,
     *                            iopt_nurbl,
     *                            iopt_nurbp,
     *                            ksi,ksj,kli,
     *                            ierror_return)
C
         call setsize()
      elseif(idsb(1:lenidsb).eq.'readdcm' .or.
     *       ioption(1:len1).eq.'dcm') then
C
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         call cmo_set_name(cmo,ierror)
         call readdcm(ifile(1:lenfile),ierror_return)
         call setsize()
 
C
c      elseif(ioption(1:len1).eq.'free') then
C
c         if (nnwds.eq.2) then
c            call cmo_get_name(cmo,ierror)
c            call cmo_exist(cmo,ierror)
c            if(ierror.eq.0) then
c               call cmo_release(cmo,ierror)
c            endif
c         else
c            cmo=cmsgin(icmsg)
c         endif
c         call cmo_set_name(cmo,ierror)
c         call readfree(imsgin,xmsgin,cmsgin,msgtype,nwds,
c     *                 ierror_return)
 
 
      elseif(idsb(1:lenidsb).eq.'readsheetij' .or.
     *       ioption(1:len1) .eq. 'sheetij') then
 
         call cmo_get_name(cmo,ierror)
         call cmo_exist(cmo,ierror)
         if(ierror.eq.0) then
            call cmo_release(cmo,ierror)
         endif
 
         call cmo_set_name(cmo,ierror)
         call read_sheetij(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *      ierror_return)
         call setsize()
 
 
 
      elseif(idsb(1:lenidsb).eq.'readngp' .or.
     *       ioption(1:len1).eq.'ngp')then
C
         if (nnwds.le.2) then
            ctest1='tet'
         else
            ctest1=cmsgin(icmsg)
         endif
         if (nnwds.eq.3) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg+1)
         endif
         call cmo_set_name(cmo,ierror)
         if(ctest1(1:3).eq.'tet') then
            call readngptet(ifile(1:lenfile),ierror_return)
         elseif(ctest1(1:3).eq.'hex') then
            call readngphex(ifile(1:lenfile),ierror_return)
         elseif(ctest1(1:3).eq.'quad') then
C*****         call readngpquad(ifile(1:lenfile),ierror_return)
         elseif(ctest1(1:3).eq.'tri') then
            call readngptri(ifile(1:lenfile),ierror_return)
         endif
         call setsize()
C
c      elseif(ioption(1:len1).eq.'qlink')then
C
c         call readq(ifile(1:lenfile),ierror)
C
C
c      elseif(ioption(1:len1).eq.'bmlink')then
C
c         call readbm(ifile(1:lenfile),ierror_return)
      elseif(idsb(1:lenidsb).eq.'readvrml' .or.
     *       ioption(1:len1).eq.'vrml') then
         if (nnwds.eq.2) then
            call cmo_get_name(cmo,ierror)
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo,ierror)
            endif
         else
            cmo=cmsgin(icmsg)
         endif
         call cmo_set_name(cmo,ierror)
 
         call readvrml(ifile(1:lenfile),ierror_return)
         call setsize()
C
      elseif(( ioption(1:len1) .eq. 'zone') .or. 
     1       ( ioption(1:len1) .eq. 'zonn')) then
C
C     FEHM zone/zonn file format
C
         call read_fehm_zone(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *      ierror_return)
C
      else
C
         write(logmess,'(a,a)') 'Invalid READ Option ', idsb(1:lenidsb)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,a,a)') 'Invalid READ Option ', 
     1                           idsb(1:lenidsb),  ioption(1:len1)
         call writloga('default',0,logmess,0,ierrw)
         ierror_return=-1
         goto 9000
C
      endif
C
C     ******************************************************************
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      ipointi=1
      ipointj=npoints
      call set_info_i('ipointi',cmo,cglobal,cdefault,ipointi,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call set_info_i('ipointj',cmo,cglobal,cdefault,ipointj,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
C     close up the file that was opened.
C
C     try to get file unit with normal file name (this means it was a 
C     fortran file open)
      iunit=lunget(ifile(1:lenfile))
      if (iunit.ne.-1) then
         close(iunit)
      else
C     try to get file unit with filename + F (this means it was a C file
C     open)
         iunit=lunget(ifile(1:lenfile) // 'F')
         if (iunit.ne.-1) then
            close(iunit)
            call cclose(iunit)
         else
C
C           Do Nothing: The file's already closed.
         endif
      endif
C
9000  return
      end
