c     **************************************************************
c     IMPORTANT NOTE:
c     File preprocessing is performed below in the event that the 
c     user does not have ExodusII built.
c 
c     Note that this file *must* be compiled with the -cpp flag to enable 
c     pre-processing.
c     Note also that this is not necessarily a portable solution to 
c     other compilers, i.e. ifort.
c
c     USAGE:
c
c     To enable ExodusII output:
c        gfortran $(FFLAGS) -cpp -o dumpexodusII.o dumpexodusII.f
c
c     To disable ExodusII output:
c        gfortran $(FFLAGS) -cpp -DNOEXODUS -o dumpexodusII.o dumpexodusII.f
c
c     For more information, see:
c        https://stackoverflow.com/a/41234703/5150303
c     **************************************************************

#ifdef NOEXODUS

c     **************************************************************
c     code substitution when not using ExodusII libraries

      subroutine dumpexodusII(
     >               imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)

      implicit none

      include 'lagrit.h'

      integer nwds
      integer ierror, ierr
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

      character*132 logmess

      write(logmess,'(a)')'ExodusII exiting: '
      call writloga('default',1,logmess,0,ierr)
      write(logmess,'(a)')'Not available in this version.'
      call writloga('default',0,logmess,2,ierr)
      
      return
      end

#else

c     **************************************************************
c     subroutine dumpexodusII
c     written originally for ExodusII 5 syntax
c     updated May 2015 for ExodusII 6 syntax

      subroutine dumpexodusII(
     >               imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)

C     dump/exo/ ifile / cmoname
C               default: single argument for output filename
C
C     dump/exo/ ifile / cmoname / facesets / files / file1, file2, ...filen
C               output filename
C               import facesets from files with tag based on order 1 to n
C
C     Comments from Carl Gable on syntax:
C
C     Dump ExodusII file with no vertex set, element set or face set information:
C       dump / exo / file_name/ mo_name 
C       dump / exodusii / file_name/ mo_name 
C       dump / exodusII / file_name/ mo_name 
C
C     Dump ExodusII file with vertex set, element set and face set info:
C       dump / exo / file_name/ mo_name / psets / eltsets / facesets 
C
C     Dump ExodusII file with vertex set, element set and face set info:
C       dump / exo / file_name/ mo_name / psets / eltsets / facesets file1 file2 filen
C
C     Dump ExodusII file with vertex set, element set and no face set information:
C       dump / exo / file_name/ mo_name / psets / eltsets
C
C     Dump ExodusII file with no vertex set,  no element set and face set info:
C       dump / exo / file_name/ mo_name / / / facesets file1 file2 filen
C
C     Write a mesh object to a file in the Exodus II format. 
C     The keyword pests as token 5 will cause all psets (lists of vertex numbers) 
C     associated with the mesh object to be written to the ExodusII output file. 
C     The keyword eltsets as token 6 will cause all eltsets (lists of cell numbers) 
C     associated with the mesh object to be written to the ExodusII output file.  
C     If face set information is being provided from files (file1 file2 filen) the 
C     format of the file is written in AVS UCD cell attribute format. 
C     The first column is the global cell number, the second column is the local face number
C
C     ### IMPORTANT ###################
C     The cells MUST be pre-sorted by itetclr material values
C     Exodus will reorder elements internally, we do not want the
C     cells to be re-ordered. If they are, faceset correlation to
C     the cell numbers will no longer be correct.
C     Check that facesets are correct by reading the exo file into GMV
C     and selecting Surfaces under Display.
C     sort based on itetclr values and cell location
C     xmed, ymed, zmed will arrange into columns (after itetclr sort)
C
C     createpts / median
C     sort / mo_pri / index / ascending / ikey / itetclr xmed ymed zmed
C     reorder / mo_pri / ikey
C       dump / gmv / out_tmp_sort1.gmv / mo_pri
C       cmo / DELATT / mo_pri / ikey
C     sort / mo_pri / index / ascending / ikey / zic yic xic
C     reorder / mo_pri / ikey
C     ################################     
c
c     **************************************************************
c
c     WARNING: works only for 1 element type per material 
c     But if we sorted based on itetcolor and a subsort based on 
c     itettyp, then it could be made to work for hybrid meshes
c
c     WARNING: some work arrays for writing sets are hard coded sizes
c     1000 is used for number of side sets which is reasonable to 
c     expect but should be changed to allocated arrays 
c    
c     **************************************************************
c
      implicit none

      include 'lagrit.h'
      include 'exodusII.inc'

c     arguments
c     character*(*) ifile

      integer nbitsmax
      parameter (nbitsmax=32)
      integer nwds
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror

c     local variables

c     I am unable to successfully use blockcom.h so I am redefining some
c     vars in place of nelmnen and nelmnef - quan

      integer nelnodes(8)
      data nelnodes / 1, 2, 3, 4, 4, 5, 6, 8 /
      integer nelfaces(8)
      data nelfaces / 0, 1, 3, 4, 4, 5, 5, 6 /

      integer i,j,k,ii,j1, k1, k2, idx, idx1, idx2, ioffset
      integer ierr, ierrw
      integer len_cmo_name, iblk_num, if_debug_local, idebug
      integer icharlnf,eltyp
      integer ierror_return
      integer nnodes, nelements, nen, mbndry
      integer iout,loutx,louty,loutz,loutk,itype
      integer lout, length, ityp
      integer nsdgeom, ndimtopo
      integer status
      integer ilen, llen_string, llen_line, ilen_word
      integer id_elem, nelblocks, iblockel
      integer inumqarec
      integer wsize,fsize
      integer maxelblk
      integer nouter1, nouter2, nnouter
      integer nsidesets1, nsidesets2, nsidesets
      integer neltsets, npsets, nfiles
      integer n11, n12, n13, n21, n22, n23
      integer fnodes_j(4), fnodes_k(4)
      integer nfnk, nfnj, nf, nn
      integer nnj, nnk, col_j, col_k, nmat
      integer offset, ibeg, iend
      integer elem_id, iblk_id
      integer idexo, icompws, iows, inumatt, num_qa_rec
      integer idexi, in_compws, in_ws, in_numatt, in_num
      integer iunit, iflag, ncount, ival, ival2
      integer setcount, index, mpno, set_id


      integer num_att_vec(2)
      integer qarecvec(3)

      integer*4 iunit4

      real*8  t1xyz(3,3), t2xyz(3,3), lxyz1(3,2), lxyz2(3,2)
      real*8  cosang, cosang_flat
      real api,vers
      
      logical done
      logical output_psets, output_eltsets, output_facesets,
     *   import_facesets, auto_facesets, auto_nodesets

      pointer (ipflist, flist)
      character*72 flist(nwds)

      character*72 ifile, ifile2, filename, cmo_name
      character*24 now
      character*33 qarec_text(4),coor_names_text(3)
      character*33 name_nod_var0(500), name_elem_var0(500)
      character*33 root3, str, str1, str2, str3, str4
      character*132 root,root2,isubname
      character*3 blk_num
      character*41 lentitle
      character*2 lenebprop1
      character*5 lenconnect_varid
      character*132 cbuff, logmess
      character*32 option

      character*(MXSTLN) qa_record(4,1)
      character*(MXSTLN) cname
      character*8 eltyp_str
      character*8 iword, iword2
      

c     Mapping between local node nums from LaGriT to Exodus II

      integer lag2exo_nmap(8,8) 
      data lag2exo_nmap / 1, 0, 0, 0, 0, 0, 0, 0, ! Point
     &                    1, 2, 0, 0, 0, 0, 0, 0, ! Line
     &                    1, 2, 3, 0, 0, 0, 0, 0, ! Tri
     &                    1, 2, 3, 4, 0, 0, 0, 0, ! Quad
     &                    1, 2, 3, 4, 0, 0, 0, 0, ! Tet
     &                    0, 0, 0, 0, 0, 0, 0, 0, ! Pyramid
     &                    1, 2, 3, 4, 5, 6, 0, 0, ! Wedge/Prism
     &                    1, 2, 3, 4, 5, 6, 7, 8/ ! Hex

c     Mapping between local face nums from LaGriT to Exodus II

      integer lag2exo_fmap(6,8) 
      data lag2exo_fmap / 1, 0, 0, 0, 0, 0, ! Point
     &                    1, 2, 0, 0, 0, 0, ! Line
     &                    2, 3, 1, 0, 0, 0, ! Tri
     &                    1, 2, 3, 4, 0, 0, ! Quad
     &                    2, 3, 1, 4, 0, 0, ! Tet
     &                    0, 0, 0, 0, 0, 0, ! Pyramid
     &                    4, 5, 1, 2, 3, 0, ! Wedge/Prism
     &                    5, 6, 1, 2, 3, 4/ ! Hex


C
C     Memory manager variables
C
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      integer itet(*),itetoff(*)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(*),jtetoff(*)
      pointer (ipikey_utr,ikey_utr)
      integer ikey_utr(*)
      pointer (ipitp1, itp1)
      integer itp1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      pointer (ipitetp,itetp)
      integer itetp(*)
      pointer (ipeltsetnames1,eltsetnames1)
      character*32 eltsetnames1(*)
      pointer (ippsetnames1,psetnames1)
      character*32 psetnames1(*)
      character*32 cpt1, cpt2, cpt3
      pointer (ippmpary1, pmpary1)
      integer pmpary1(*)
      pointer (ipempary1, empary1)
      integer empary1(*)
      pointer (ipexo_elt_ary, exo_elt_ary)
      integer exo_elt_ary(*)
      pointer (ipelem_map, elem_map)
      integer elem_map(*)
      pointer (ipxtetwd, xtetwd)
      integer xtetwd(*)
      pointer (ipisetwd, isetwd)
      integer isetwd(*)

c
c     Local temporary memory managed arrays
c
C
      pointer (ipnumelblk,numelblk)
      integer numelblk(*)       ! Number of elements in block
      pointer (ipelcolblk,elcolblk)
      integer elcolblk(*)       ! Color or material ID of elements in block
      pointer (ipeltypblk,eltypblk)
      integer eltypblk(*)       ! Type of elements in block
      pointer (ipbegidxblk,begidxblk)
      integer begidxblk(*)      ! Beginning index of elements in block


c     these should be allocated, but 1000 should be more than enough sidesets
      integer nfaces_ss1(1000), nfaces_ss2(1000), nfaces_ss(1000)
      integer sideset_tag1(1000), sideset_tag2(1000), sideset_tag(1000)
      integer nnodes_ns(1000)
      integer nodeset_tag(1000)

      pointer (ipnsidesetmat,nsidesetmat)
      integer nsidesetmat(*)
      pointer (ipmatcols,matcols)
      integer matcols(*)

      pointer (ipouterelems1,outerelems1)
      integer outerelems1(*)
      pointer (ipouterelems2,outerelems2)
      integer outerelems2(*)
      pointer (ipouterfaces1,outerfaces1)
      integer outerfaces1(*)
      pointer (ipouterfaces2,outerfaces2)
      integer outerfaces2(*)

c     these were integer 4 to work with fortran calls
c     changed for ExodusII6 and greater
      integer elconblk(*)
      integer sselemlist(*)
      integer ssfacelist(*)
      integer outernodes(*)

      pointer (ipelconblk,elconblk)
      pointer (ipsselemlist,sselemlist)
      pointer (ipssfacelist,ssfacelist)
      pointer (ipouternodes,outernodes)

      integer fmarked(*)
      integer nmarked(*)

      pointer (ipfmarked,fmarked)
      pointer (ipnmarked,nmarked)

C
C     Wrapper function for the Exodus excre function
C
c      integer excre_wrapper
c      external excre_wrapper


c**********************************************************
C     Begin dumpexodusII formerly known as dumpnetcdf

C     There are SETUP and WRITE SECTIONS in this file.

C     Initialize flags for output sets
      output_psets = .false.
      output_eltsets = .false.
      output_facesets = .false.
      import_facesets = .false.
      auto_facesets = .false.
      auto_nodesets = .false.

c     for extra output from this routine if compiled non-zero 
c     also will keep tty output on if not zero
      if_debug_local = 0
      idebug = 0
      ierror_return = 0

c
      isubname ='dumpexodusII'
      lentitle= 'LaGriT to ExodusII output'
      lenebprop1='ID'

      cosang_flat = -0.707      ! Approximately 135 degrees

C     api=4.5700002
C     vers=4.5700002
C     Update 2017 - should be dynamic instead of hard coded
C     Output from run:
C     global attributes:
C      api_version = 7.03f ;
C      version = 7.03f ;
C      floating_point_word_size = 8 ;
C      file_size = 1 ;
C      maximum_name_length = 32 ;
C      int64_status = 0 ;
C      title = "LaGriT to Exodus" ;

      api=7.03
      vers=7.03
      wsize=8
      fsize=0
      llen_string=33
      llen_line=81
      inumqarec=1


c**********************************************************
c
c
C ----Parse the input commands --------------------------------     
C     Do some error checking on options
C

C     default - output ifile and cmoname must be provided
C     dump/exo/ ifile / cmoname / option

      if (msgtype(3).eq. 3) then
         ifile = cmsgin(3)
      else
         write(logmess,"(a,a)") isubname,
     *   " ERROR : filename not given."
         call writloga('default',0,logmess,1,ierr)
         ierror_return = -1
         go to 9999
      endif

      if (msgtype(4).eq. 3) then
         cmo_name = cmsgin(4)
      else
         write(logmess,"(a,a)") isubname,
     *   " ERROR : cmo name not given."
         call writloga('default',0,logmess,1,ierr)
         ierror_return = -1
      endif
      

C     No psets and eltsets: dump/exo/ outfile/ cmoname / / / facesets
C     No facesets: dump/exo/ outfile/ cmoname / psets / eltsets /
C     dump/exo/ outfile/ cmoname / psets / eltsets / facesets 
C     dump/exo/ outfile/ cmoname / psets / eltsets / facesets file1,file2,...filen

C     Check for pset and eltset options
      if (msgtype(5).eq.3 .and. cmsgin(5)(1:5).eq.'psets') then
         output_psets = .true.
      endif

      if (msgtype(6).eq.3 .and. cmsgin(6)(1:7).eq.'eltsets') then
         output_eltsets = .true.
      endif


C     Check for faceset option

      if (msgtype(7).eq.3 .and. cmsgin(7)(1:8).eq.'facesets') then
         auto_facesets = .true.
         output_facesets = .true.
         if (nwds.gt.7) then
            auto_facesets = .false.
            import_facesets = .true.
	    call mmgetblk("flist", isubname, ipflist, 72*nwds, 1, ierr)
            if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk flist')

            nfiles = 0
            do ii = 8,nwds
              nfiles = nfiles+1
              flist(nfiles) = cmsgin(ii)

              write(logmess,"(a,i7,1x,a)") 
     *        "got ",nfiles,flist(nfiles)
               call writloga('default',0,logmess,0,ierr)

              ilen = icharlnf(flist(nfiles))
              call fexist(flist(nfiles)(1:ilen),ierr)

              if(ierr.eq.0) then
                write(logmess,"(a,a)") 
     *      "ERROR: Missing facesets file: ",flist(nfiles)(1:ilen)
                call writloga('default',1,logmess,1,ierr)
                ierror_return = -2
                goto 9999
              endif

            enddo
         endif
      endif


C-----Done parsing input ------------------------------------

c     Get CMO info

C     set cmo and ierror_return in case things blow up
      call cmo_exist(cmo_name,ierr)
      if(ierr.ne.0) then
        write(logmess,'(a)')
     &  'dump/exo Warning: cannot find default mesh object.'
        call writloga('default',0,logmess,0,ierrw)
        ierror_return = -3
        go to 9999
      endif
      len_cmo_name=icharlnf(cmo_name)

C     set debug level based on user or code defintions
      call cmo_get_info('idebug',cmo_name,idebug,ilen,ityp,ierr)
      if (idebug .gt. 0) if_debug_local = idebug
      if (if_debug_local .gt. 0) then
         write(logmess,'(a,i5)')
     &  'dump/exo idebug set: ',if_debug_local
        call writloga('default',1,logmess,1,ierrw)
      endif

      filename=ifile(1:icharlnf(ifile))
      write(logmess,"(a,a,a,a,a,a)")'ExodusII: ',
     & 'Start writing to file: ',filename(1:icharlnf(filename)),
     & ' using cmo: ',cmo_name(1:len_cmo_name)
      call writloga('default',1,logmess,1,ierr)

c     Turn off screen output of dotask commands
c     but leave on for debug mode

      if(if_debug_local.eq.0) call writset('stat','tty','off',ierr)

      ierr=0
      cbuff = ' '
      write(cbuff,*) 'cmo/set_id/' // cmo_name(1:len_cmo_name)//
     &               '/ element / e_num_temp; finish'
      call dotask(cbuff,ierr)

c     Sort the elements according to their itetclr (material ID)

      cbuff = ' '
      write(cbuff,*) 'sort/' //cmo_name(1:len_cmo_name)//
     &               '/index/ascending/ikey_utr/itetclr/e_num_temp',
     &               ';finish'
      call dotask(cbuff,ierr)
      if (ierr.ne.0) then
      write(logmess,"(a)")
     *    "Error performing sort/cmo"
          call writloga('default',0,logmess,1,ierr)
      endif

      cbuff = ' '
      write(cbuff,*) 'cmo/DELATT/' // cmo_name(1:len_cmo_name)//
     &               '/ e_num_temp; finish'
      call dotask(cbuff,ierr)

c
c     For Exodus II output reset ioflag of itetclr
c TAM - why are we resetting ioflag ?
c      write(cbuff,*) 'cmo/modatt/' // cmo_name(1:len_cmo_name)//
c     &               '/ itetclr / ioflag / lga ; finish'
c      call dotask(cbuff,ierr)


c     Retrieve detailed info about mesh including coordinates, connectivity

      call cmo_get_info('xic',cmo_name,ipxic,lout,itype,ierr)
      call cmo_get_info('yic',cmo_name,ipyic,lout,itype,ierr)
      call cmo_get_info('zic',cmo_name,ipzic,lout,itype,ierr)
      call cmo_get_info('imt1',cmo_name,ipimt1,lout,itype,ierr)
      call cmo_get_info('itp1',cmo_name,ipitp1,lout,itype,ierr)
      call cmo_get_info('itetclr',cmo_name,
     *                  ipitetclr,lout,itype,ierr)
      call cmo_get_info('itettyp',cmo_name,
     *                  ipitettyp,lout,itype,ierr)
      call cmo_get_info('itet',cmo_name,
     *                  ipitet,lout,itype,ierr)
      call cmo_get_info('itetoff',cmo_name,
     *                  ipitetoff,lout,itype,ierr)
      call cmo_get_info('jtet',cmo_name,ipjtet,lout,itype,ierr)
      call cmo_get_info('jtetoff',cmo_name,ipjtetoff,lout,itype,ierr)
      call cmo_get_info('ikey_utr',cmo_name,ipikey_utr,lout,itype,
     *                  ierr)
      call cmo_get_info('nnodes',cmo_name,nnodes,lout,itype,
     *                  ierr)
      call cmo_get_info('nelements',cmo_name,nelements,lout,
     *                  itype,ierr)
      call cmo_get_info('ndimensions_geom',cmo_name,
     *                        nsdgeom,lout,itype,ierr)
      call cmo_get_info('ndimensions_topo',cmo_name,
     *                        ndimtopo,lout,itype,ierr)
      call cmo_get_info('mbndry',cmo_name,mbndry,lout,itype,ierr)
      call cmo_get_info('xtetwd',cmo_name,ipxtetwd,lout,itype,ierr)
      call cmo_get_info('isetwd',cmo_name,ipisetwd,lout,itype,ierr)



c-------------------------------------------------------------------
c     PREPROCESS LAGRIT DATA for EXODUSII calls


c     SECTION SETUP - COUNT ELEMENT SETS AND POINT SETS

C     print*
C     print*, 'Counting number of psets and eltsets'
      if (output_psets .eqv. .true.) then
         npsets = 0
         call mmfindbk('psetnames',cmo_name,
     *        ippsetnames1,lout,ierr)
         do i=1,nbitsmax
            if (psetnames1(i) .ne.' '.and.
     *          psetnames1(i)(1:5).ne.'-def-')
     *         npsets=npsets + 1
         enddo
         length = max(nnodes, nelements)
         call mmgetblk('pmpary1',isubname,ippmpary1,length,2,ierr)
      else
         npsets = 0
      endif
C      print *, 'Finish counting psets'
C      print *, 'Number of psets: ', npsets


      if (output_eltsets .eqv. .true.) then
         neltsets = 0
         call mmfindbk('eltsetnames',cmo_name,
     *        ipeltsetnames1,lout,ierr)
         do i=1,nbitsmax
            if (eltsetnames1(i) .ne.' '.and.
     *          eltsetnames1(i)(1:5).ne.'-def-')
     *         neltsets=neltsets + 1
         enddo
         length = max(nnodes, nelements)
         call mmgetblk('empary1',isubname,ipempary1,length,2,ierr)
      else
         neltsets = 0
      endif
C      print *, 'Finish counting eltsets'
C      print *, 'Number of eltsets: ', neltsets
C      print*


c     SECTION SETUP - MAP ELEMENT IDS BETWEEN EXODUS AND LAGRIT

      length = max(nnodes, nelements)
      call mmgetblk('elem_map',isubname,ipelem_map,length,2,ierr)
      do i=1, nelements
         elem_map(ikey_utr(i)) = i
      enddo


c     SECTION SETUP - PREPARE ELEMENT BLOCKS

c     There is an element block for each itetclr 1 thru N
c     First count the blocks, then allocate an array, then fill the array.
c     nblocks is number of blocks
c     blockinfo has number of elements per block
c     There is currently no attrib for these blocks


      nmat = 1                  ! There is at least one material
      nelblocks=1               ! There is at least one block
      iblockel=1
      do i=2,nelements
         id_elem=ikey_utr(i)
         iblockel=iblockel+1
         if (itetclr(ikey_utr(i)).ne.itetclr(ikey_utr(i-1))) then
            nmat = nmat + 1
            nelblocks=nelblocks+1
            iblockel=0
         endif
      enddo

      call mmgetblk
     *   ('numelblk',isubname,ipnumelblk,nelblocks,1,ierr)
      call mmgetblk
     *   ('elcolblk',isubname,ipelcolblk,nelblocks,1,ierr)
      call mmgetblk
     *   ('eltypblk',isubname,ipeltypblk,nelblocks,1,ierr)
      call mmgetblk
     *   ('begidxblk',isubname,ipbegidxblk,nelblocks,1,ierr)

      call mmgetblk
     &     ('matcols',isubname,ipmatcols,nmat,1,ierr)

      
      nmat = 1
      nelblocks=1
      numelblk(1) = 1           ! There is at least one element in the block
      elcolblk(1) = itetclr(ikey_utr(1))
      eltypblk(1) = itettyp(ikey_utr(1))
      begidxblk(1) = 1
      matcols(1) = itetclr(ikey_utr(1))

      iblockel=1
      do i=2,nelements
         iblockel=iblockel+1
         if (itetclr(ikey_utr(i)).ne.itetclr(ikey_utr(i-1))) then
            
c           start a new block with new material

            nelblocks=nelblocks+1 
            nmat = nmat + 1
            
            numelblk(nelblocks) = 1
            elcolblk(nelblocks) = itetclr(ikey_utr(i))
            eltypblk(nelblocks) = itettyp(ikey_utr(i))
            begidxblk(nelblocks) = i
            matcols(nmat) = itetclr(ikey_utr(i))
            
            iblockel=1
            
         else
            numelblk(nelblocks) = iblockel
         endif
      enddo

c     Maximum number of elements in any block

      maxelblk=0
      do i = 1, nelblocks
         if (numelblk(i) .gt. maxelblk) maxelblk = numelblk(i)
      enddo


C     SECTION SETUP Side Sets 


c     form a list of outer faces - outerfaces1,outerelems1

      nouter1 = 0
      do i = 1, nelements
         elem_id = ikey_utr(i)
         eltyp = itettyp(elem_id)
         do j = 1, nelfaces(eltyp)  

c           No element on other side of this face - outer element

            if (jtet(jtetoff(elem_id)+j) .eq. mbndry) then
               nouter1 = nouter1 + 1
            endif

         enddo
      enddo

      call mmgetblk
     &     ('outerelems1',isubname,ipouterelems1,nouter1,1,ierr)
      call mmgetblk
     &     ('outerfaces1',isubname,ipouterfaces1,nouter1,1,ierr)


c     collect all the outer faces

      nouter1 = 0
      do i = 1, nelements
         elem_id = ikey_utr(i)
         eltyp = itettyp(elem_id)
         do j = 1, nelfaces(eltyp)

            if (jtet(jtetoff(elem_id)+j) .eq. mbndry) then
               nouter1 = nouter1 + 1
               outerelems1(nouter1) = i ! possible for 2 or more faces of 
               outerfaces1(nouter1) = j ! same element to be outer faces
            endif

         enddo
      enddo

c     Automated Version to find faces for Side Sets:

c     form lists outerfaces2,outerelems2 containing faces
c     grouped according to contiguous sets separated by sharp angles
c
c     Then we will repopulate outerfaces1,outerelems1 from outerfaces2,
c     outerelems2 by forming sub-groups of faces with the same material IDs
c     We assume there will be a maximum of 1000 sidesets

      if (output_facesets .eqv. .true.) then
	if ( auto_facesets .eqv. .false.) then
	   call mmgetblk
     &        ('sselemlist',isubname,ipsselemlist,3*nouter1,1,ierr)
	   call mmgetblk
     &        ('ssfacelist',isubname,ipssfacelist,3*nouter1,1,ierr)
	   goto 2001
	else
           goto 2000
        endif
      else
	nsidesets = 0
	goto 4000
      endif


c     Auto Break up these faces first according to sharp edges 
c     (or edges according to sharp corners)

 2000 print *, 'Warning: Side Sets detected automatically.'
      print *, "This process may take awhile."
         write(logmess,"(a)")
     *  "Warning: Side Sets detected automatically."
         call writloga('default',1,logmess,0,ierr)
         write(logmess,"(a)")
     *  "This process may take awhile."
         call writloga('default',0,logmess,1,ierr)

      call mmgetblk
     &     ('fmarked',isubname,ipfmarked,nouter1,1,ierr)
      call mmgetblk
     &     ('outerelems2',isubname,ipouterelems2,nouter1,1,ierr)
      call mmgetblk
     &     ('outerfaces2',isubname,ipouterfaces2,nouter1,1,ierr)

c     We will reuse the sselemlist and ssfacelist for the final
c     sideset storage so allocate the larger amount
      call mmgetblk
     &     ('sselemlist',isubname,ipsselemlist,3*nouter1,1,ierr)
      call mmgetblk
     &     ('ssfacelist',isubname,ipssfacelist,3*nouter1,1,ierr)


      fmarked(1:nouter1) = 0

      nsidesets2 = 0
      nouter2 = 0

      do i = 1, nouter1
         if (fmarked(i) .eq. 1) cycle


c        start a new sideset

         sselemlist(1:nouter1) = 0
         ssfacelist(1:nouter1) = 0

         nsidesets2 = nsidesets2 + 1
         sideset_tag2(nsidesets2) = 100 + nsidesets2

c        add face to temporary sideset list

         sselemlist(1) = outerelems1(i)
         ssfacelist(1) = outerfaces1(i)
         nf = 1
         j = 1

c        also add to sorted outer face list

         nouter2 = nouter2 + 1
         outerelems2(nouter2) = outerelems1(i)
         outerfaces2(nouter2) = outerfaces1(i)


         done = .false.
         do while (done .eqv. .false.)

c           get the nodes of this face

            elem_id = ikey_utr(sselemlist(j))
            eltyp = itettyp(elem_id)
            nen = nelnodes(eltyp)

            call get_face_nodes(ssfacelist(j), nfnj, fnodes_j,
     &           eltyp, nen, itet(itetoff(elem_id)+1))

            do j1 = 1, nfnj

               n11 = fnodes_j(j1)
               n12 = fnodes_j(1 + mod(j1,nfnj))
               n13 = fnodes_j(1 + mod(j1+1,nfnj))


c              search the rest of the elements to find an adjacent face

               do k = i+1, nouter1 
               
                  if (fmarked(k) .eq. 1) cycle

                  elem_id = ikey_utr(outerelems1(k))
                  eltyp = itettyp(elem_id)
                  nen = nelnodes(eltyp)

                  call get_face_nodes(outerfaces1(k), nfnk, fnodes_k,
     &                 eltyp, nen, itet(itetoff(elem_id)+1))

c                 do they share an edge?

                  do k1 = 1, nfnk
                     
                     n21 = fnodes_k(k1)
                     n22 = fnodes_k(1 + mod(k1,nfnk))
                     n23 = fnodes_k(1 + mod(k1+1,nfnk))


                     if (ndimtopo .eq. 2) then

c                       Common node?

                        if (n12 .eq. n21) then

                           lxyz1(1,1) = xic(n12)
                           lxyz1(2,1) = yic(n12)
                           lxyz1(3,1) = zic(n12)
                           lxyz1(1,2) = xic(n11)
                           lxyz1(2,2) = yic(n11)
                           lxyz1(3,2) = zic(n11)

                           lxyz2(1,1) = xic(n21)
                           lxyz2(2,1) = yic(n21)
                           lxyz2(3,1) = zic(n21)
                           lxyz2(1,2) = xic(n22)
                           lxyz2(2,2) = yic(n22)
                           lxyz2(3,2) = zic(n22)

                           call get_int_ang(lxyz1,lxyz2,cosang)
                           
                           if (cosang .lt. cosang_flat) then
                              
c                             Not a sharp edge (3D)
c                             Include this adjacent face in this sideset

                              nf = nf + 1
                              sselemlist(nf) = outerelems1(k)
                              ssfacelist(nf) = outerfaces1(k)
                              
                              nouter2 = nouter2 + 1
                              outerelems2(nouter2) = outerelems1(k)
                              outerfaces2(nouter2) = outerfaces1(k)
                              
                              fmarked(k) = 1 ! mark as processed
                              
                           endif

                        endif

                     else

c                       Common edge?

                        if (n11 .eq. n22 .and. n12 .eq. n21) then

c                          Check dihedral angle between the two faces

                           t1xyz(1,1) = xic(n11)
                           t1xyz(2,1) = yic(n11)
                           t1xyz(3,1) = zic(n11)
                           t1xyz(1,2) = xic(n12)
                           t1xyz(2,2) = yic(n12)
                           t1xyz(3,2) = zic(n12)
                           t1xyz(1,3) = xic(n13)
                           t1xyz(2,3) = yic(n13)
                           t1xyz(3,3) = zic(n13)
                           
                           t2xyz(1,1) = xic(n21)
                           t2xyz(2,1) = yic(n21)
                           t2xyz(3,1) = zic(n21)
                           t2xyz(1,2) = xic(n22)
                           t2xyz(2,2) = yic(n22)
                           t2xyz(3,2) = zic(n22)
                           t2xyz(1,3) = xic(n23)
                           t2xyz(2,3) = yic(n23)
                           t2xyz(3,3) = zic(n23)
                           
                           
                           call get_dihed_ang(t1xyz,t2xyz,cosang)

                           if (cosang .lt. cosang_flat) then
                              
c                             Not a sharp edge (3D)
c                             Include this adjacent face in this sideset

                              nf = nf + 1
                              sselemlist(nf) = outerelems1(k)
                              ssfacelist(nf) = outerfaces1(k)
                              
                              nouter2 = nouter2 + 1
                              outerelems2(nouter2) = outerelems1(k)
                              outerfaces2(nouter2) = outerfaces1(k)
                           
                              fmarked(k) = 1 ! mark as processed

                           endif

                        endif
                        
                     endif
                     
                  enddo         ! do k1 = 1, nfnk
                  
               enddo            ! do j1 = 1, nfnj
               
               
            enddo               ! do k = i+1, nouter1

            j = j + 1

            if (j .gt. nf) done = .true.

         enddo                  ! do while (done .eq. .false.)

         nfaces_ss2(nsidesets2) = nf

      enddo                     ! do i = 1, nouter1
      
      fmarked(1:nouter1) = 0





c     Auto -  further subdivide the sidesets based on material IDs of elements
      call mmgetblk
     &     ('nsidesetmat',isubname,ipnsidesetmat,nmat,1,ierr)
      nsidesetmat(1:nmat) = 0

      
      nsidesets1 = 0

      if (nelblocks .gt. 1) then ! i.e. there is more than 1 material
         nouter1 = 0
         offset = 0
         
         do i = 1, nsidesets2
            
            ibeg = offset + 1
            iend = offset + nfaces_ss2(i)
            
            do j = ibeg, iend
               
               if (fmarked(j) .eq. 1) cycle
               
c              start a new sideset
               
               sselemlist(1:nouter1) = 0
               ssfacelist(1:nouter1) = 0
               
               nsidesets1 = nsidesets1 + 1

               elem_id = ikey_utr(outerelems2(j))

               col_j = itetclr(elem_id)
               do k = 1, nmat
                  if (matcols(k) .eq. col_j) exit
               enddo
               nsidesetmat(k) = nsidesetmat(k) + 1

c              TODO if possible, remove the 10000 multiplier 
c              The multiplier for block id was removed for ExoduII6
               sideset_tag1(nsidesets1) = 10000*k + nsidesetmat(k)

c              add face to temporary sideset list
c              strictly speaking, we don't need this list in this section

               sselemlist(1) = outerelems2(j)
               ssfacelist(1) = outerfaces2(j)
               nf = 1
               
c              also add to sorted outer face list

               nouter1 = nouter1 + 1
               outerelems1(nouter1) = outerelems2(j)
               outerfaces1(nouter1) = outerfaces2(j)

               fmarked(j) = 1   ! Mark as processed
               

c              search the rest of the elements in this sideset to find
c              other faces in the same material

               do k = j+1, iend
                        
                  if (fmarked(k) .eq. 1) cycle
                        
                  elem_id = ikey_utr(outerelems2(k))
                  col_k = itetclr(elem_id)
                  
                  if (col_j .eq. col_k) then
                              
                     nf = nf + 1
                     sselemlist(nf) = outerelems2(k)
                     ssfacelist(nf) = outerfaces2(k)
                     
                     nouter1 = nouter1 + 1
                     outerelems1(nouter1) = outerelems2(k)
                     outerfaces1(nouter1) = outerfaces2(k)
                     
                     fmarked(k) = 1 ! mark as processed

                  endif

               enddo            ! do k = j+1, iend

               nfaces_ss1(nsidesets1) = nf

            enddo               ! do j = ibeg, iend

            offset = offset + nfaces_ss2(i)
            
         enddo                  ! do i = 1, nouter2
      endif                     ! if there is more than one material



c     Auto - form the final sideset info
c     In addition to the above two sidesets, all the outer faces of the
c     mesh will be made into one sideset (the first one)

      sideset_tag(1) = 1
      nfaces_ss(1) = nouter2
      nsidesets = 1
      
      do j = 1, nouter2
         sselemlist(j) = outerelems2(j)
         ssfacelist(j) = outerfaces2(j)
      enddo

      offset = nouter1          ! offset for sselemlist
      ibeg = 0                  ! offset for outerelems2
      do i = 1, nsidesets2
         nsidesets = nsidesets + 1
         sideset_tag(nsidesets) = sideset_tag2(i)
         nfaces_ss(nsidesets) = nfaces_ss2(i)
         
         do j = 1, nfaces_ss2(i)
            sselemlist(offset+j) = outerelems2(ibeg+j)
            ssfacelist(offset+j) = outerfaces2(ibeg+j)
         enddo

         offset = offset + nfaces_ss2(i)
         ibeg = ibeg + nfaces_ss2(i)
      enddo

      ibeg = 0
      do i = 1, nsidesets1
         nsidesets = nsidesets + 1
         sideset_tag(nsidesets) = sideset_tag1(i)
         nfaces_ss(nsidesets) = nfaces_ss1(i)
         
         do j = 1, nfaces_ss1(i)
            sselemlist(offset+j) = outerelems1(ibeg+j)
            ssfacelist(offset+j) = outerfaces1(ibeg+j)
         enddo

         offset = offset + nfaces_ss1(i)
         ibeg = ibeg + nfaces_ss1(i)
      enddo
      print *, 'Done preparing automated sidesets!'
      print*


c     ASSUME THERE ARE AS MANY PSETS AS FACESETS - EACH PSET IS
c     JUST A COLLECTION OF THE UNIQUE NODES OF A FACESET
      
c     assuming each mesh face has a max of four nodes, allocate the most
c     conservative amount of space for list of outer nodes

      if ((output_psets .eqv. .true.) .and. (npsets .eq. 0)) then
      print *, 'WARNING: You have not defined any psets'
      print *, 'Trying to define psets automatically'
      print *, 'Assume there are as many psets as facesets.'
      print *, 'Each pset is just a collection of'
      print *, 'the unique nodes of a faceset.'
      nnouter = 0
      do i = 1, nsidesets
         nnouter = nnouter + nfaces_ss(i)
      enddo
      nnouter = 4*nnouter 
      
      call mmgetblk
     &     ('outernodes',isubname,ipouternodes,nnouter,1,ierr)
      
      call mmgetblk
     &     ('nmarked',isubname,ipnmarked,nnodes,1,ierr)


      offset = 0
      ibeg = 1
      do i = 1, nsidesets         
         nodeset_tag(i) = sideset_tag(i)

         nmarked(1:nnodes) = 0

         iend = ibeg+nfaces_ss(i)-1
         nn = 0
         do j = ibeg, iend

            elem_id = ikey_utr(sselemlist(j))
            eltyp = itettyp(elem_id)
            nen = nelnodes(eltyp)
            call get_face_nodes(ssfacelist(j), nnj, fnodes_j,
     &           eltyp, nen, itet(itetoff(elem_id)+1))

            do j1 = 1, nfnj
               if (nmarked(fnodes_j(j1)) .eq. 0) then

c                 add to nodeset
            
                  nn = nn + 1
                  outernodes(offset + nn) = fnodes_j(j1)
                  nmarked(fnodes_j(j1)) = 1

               endif                  
            enddo               ! do j1 = 1, nfnj

         enddo                  ! do j = ibeg, iend

         nnodes_ns(i) = nn
         offset = offset + nn

         ibeg = ibeg + nfaces_ss(i)

      enddo                     ! do i = 1, nsideset
      npsets = nsidesets
      auto_nodesets = .true.
      endif
      goto 4000
      

C     SECTION SETUP - User Defined Side Sets 

C     option to import facesets from lagrit faceset files
C     this section updates facesets information
C     nsidesets - number of sets = nfiles
C     sideset_tag(1:nfiles) - id number tag to assign
C     nfaces_ss( )  - indexes into big arrays 
C     sselemlist( ) - indexed big array with all elem sets 
C     ssfacelist( )  - indexed big array with all face sets)

 2001 print*,'Exodus FACESETS imported from files. ',nfiles
      
      offset = 0
      ibeg=0
      do i = 1, nfiles

        iunit=-1
        ifile2 = flist(i)
        ilen = icharlnf(ifile2)
        call hassign(iunit,ifile2,ierr)
        iunit4 = iunit
        if (ierr.lt.0 .or. iunit.lt.0) then
            write(logmess,*) 'ERROR: file not opened: '
     &         //ifile2(1:ilen)
            call writloga('default',1,logmess,0,ierr)
            ierr = -1
            go to 9000
        endif

C       look for top of file text with following lines:
C       ....
C       idelem1, integer 
C       idface1, integer 

        iflag = 0
        ncount = 0  
        do while (iflag.eq.0) 
          read(iunit4,'(a8)',err=9000) iword
          read(iunit4,'(a8)',err=9000) iword2
          if (iword(1:6).eq.'idelem' .and.
     &      iword2(1:6).eq.'idface') iflag=1
        enddo

C       read and save values from current file
        if ( iflag .eq. 1) then
  110       continue
            read(iunit4,*,end=3000) ival,ival2
            ncount= ncount+1

            if (ncount .gt. nouter1) then
              print*,'Warning: array size too small.'
              print*,'ncount: ',ncount,' size: ',nouter1
            endif

            outerelems1(ncount) = ival
            outerfaces1(ncount) = ival2
            go to 110        
        endif

 3000  close(iunit)

       if (ncount .le. 0) then
         write(logmess,*) 'Warning: No idelem idface tags in file: '
     &       //ifile2(1:ilen)
         call writloga('default',1,logmess,1,ierr)
         print*,'Warning: No idelem, idface in file: ',ifile2(1:ilen)
       endif

C     ********** now write to faceset arrays *******
      iend = offset + ncount
      if (iend .gt. nouter1*3) then
        print*,'Warning: ncount may be greater than array size.'
      endif

      print*,'Total read: ',iend
      print*,'Current offset: ',offset
      print*,'Set tag: ',i,' nfaces: ',ncount

      sideset_tag(i) = i 
      nfaces_ss(i) = ncount 
      do j = 1, ncount 
         sselemlist(offset+j) = outerelems1(j)
         ssfacelist(offset+j) = outerfaces1(j)
      enddo

      print*,'first: ',sselemlist(offset+1),
     &         ssfacelist(offset+1)
      print*,'last:  ',sselemlist(offset+ncount),
     &         ssfacelist(offset+ncount)

      offset = offset + nfaces_ss(i)
      print*,'Set new offset: ',offset

C     ********** done writing to faceset arrays *******

C     end loop through nfiles
      enddo
      nsidesets = nfiles

 9000 if (ierr .lt. 0) then
         write(logmess,*) 'Warning: Could not read facesets file: '
     &       //ifile2(1:ilen)
         call writloga('default',1,logmess,0,ierr)
         write(logmess,*) 'Warning: No facesets defined.'
         call writloga('default',0,logmess,1,ierr)
        print*, 'Could not read facesets file ',ifile2(1:ilen)
	print*, 'No facesets will be defined'
        nsidesets = 0
      endif
      continue



C     -------------------------------------------------
C     SETUP DONE  
C     SKIP to HERE to WRITE 

 4000 icompws=8
      iows = 8
      status = 0

C     Turn on screen output to allow important information
      call writset('stat','tty','on',ierr)    


c The function ex_create or (EXCRE for Fortran) creates a new EXODUS II file and returns an
c ID that can subsequently be used to refer to the file.
c
c An application code can compute either 4- or 8-byte values and can designate that the values
c be stored in the EXODUS II file as either 4- or 8-byte numbers; conversion between the 4- and
c 8-byte values is performed automatically by the API routines. Thus, there are four possible
c combinations of compute word size and storage (or I/O) word size.
c
c In case of an error, ex_create returns a negative number; EXCRE returns a nonzero error
c number in IERR. Possible causes of errors include:
c -  Passing a file name that includes a directory that does not exist.
c -  Specifying a file name of a file that exists and also specifying a no clobber option.
c -  Attempting to create a file in a directory without permission to create files there.
c -  Passing an invalid file mode.
c

C INTEGER FUNCTION EXCRE (PATH, ICMODE, ICOMPWS, IOWS, IERR)
C CHARACTER*(*) PATH (R) The file name of the new EXODUS II file.
C INTEGER ICMODE (R) Clobber mode. Use one of the following predefined constants:
C   - EXNOCL create the new file only if the given file name does not already exist.
C   - EXCLOB create the new file, overwrite if exists.
C   - EXNORM Create a normal (32-bit offset) model.
C   - EXLARG To create a model which can store datasets larger than 2 gigabytes.
C   - EXNET4 To create a model using the HDF5-based netcdf-4 output. (Future capability)
C   - EXNOSH Do not open the underlying netCDF file in share mode.
C INTEGER ICOMPWS (RW) The word size in bytes (0, 4 or 8) of the (REAL) program variables
C INTEGER IOWS (R) The word size in bytes (4 or 8) of the (REAL) data stored in file.
C INTEGER IERR (W) Returned error code. If no errors occurred, 0 is returned.
c
c     icompws is the "computer word size." It should probably not be
c     hard-coded here. Likewise for iows, the IO word size.


C     SECTION WRITE - Create ExodusII File 

      idexo = excre (filename, EXCLOB, icompws, iows, status)


      if (if_debug_local .gt. 0) then
            write(logmess,'(a,a)')
     &       'EXCRE Create ExodusII File: ',filename
            call writloga('default',1,logmess,0,ierr)
            write(logmess,'(2x,a,i7)')
     &      'File ID:        ',idexo
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i15)')
     &      'Word Size Program:   ',icompws
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i15)')
     &      'Word Size Data:      ',iows
            call writloga('default',0,logmess,0,ierr)
      endif


      if (status.ne.0) then 
        call exerr(isubname,
     & " ERROR opening ExodusII file: ",status)
        ierror_return = status
        go to 9999
      endif

 
C     -------------------------------------------------
C     SECTION WRITE Initialize ExodusII data 
 
      write(logmess,"('Title: LAGRIT TO EXODUSII')")
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,"('number of dimension:      ',i10)") nsdgeom
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of nodes:          ',i10)") nnodes
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of elements:       ',i10)") nelements
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of edges:          ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of edge blocks:    ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of element blocks: ',i10)") nelblocks
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of face blocks:    ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of node sets:      ',i10)") npsets
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of edge sets:      ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of element sets:   ',i10)") neltsets
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of side sets:      ',i10)") nsidesets
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of face sets:      ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of node maps:      ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of edge maps:      ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of face maps:      ',i10)") 0
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,"('number of element maps:   ',i10)") 0
      call writloga('default',0,logmess,1,ierrw)
      
      call exo_lg_ini(idexo, nsdgeom, nnodes, 
     &     nelements, nelblocks, npsets, 
     &     nsidesets, neltsets, status)

      if (status.ne.0) then 
         call exerr(isubname,
     &   " ERROR writing ExodusII init parameters.",status)
         ierror_return = status
         go to 9999
      endif


C     -------------------------------------------------
C     SECTION WRITE QA Information 

c     Put some QA info - problem name, date, time etc.

      num_qa_rec = 1

      qa_record(1,1) = cmo_name(1:MXSTLN)
      qa_record(2,1) = "probname"
      qa_record(3,1) = "Today"
      qa_record(4,1) = "Time"

      call EXPQA(idexo, num_qa_rec, qa_record, status)
      if (status.ne.0) then 
          call exerr(isubname,
     &    " ERROR writing ExodusII date/time. ",status)
          ierror_return = status
          print*,'EXPQA DONE.',status
          go to 9999
      endif


C     -------------------------------------------------
C     SECTION WRITE Coordinates

       if (if_debug_local .gt. 0) then
            write(logmess,'(a,i10)')
     &       'EXCRE Write Coordinates: ',nnodes
            call writloga('default',1,logmess,0,ierr)
       endif

      call EXPCOR(idexo, xic, yic, zic, status)
      if (status.ne.0) then 
          call exerr(isubname,
     &    " ERROR writing ExodusII coordinates. ",status)
          ierror_return = status
          print*,'EXPCOR DONE.',status
          go to 9999
      endif

C     -------------------------------------------------
C     SECTION WRITE Element Blocks
      

C The function ex_put_elem_block (or EXPELB for Fortran) writes the parameters used to
C describe an element block. In case of an error, ex_put_elem_block returns a negative number; 
C a warning will return a positive number. 
C EXPELB returns a nonzero error (negative) or warning (positive) number in C IERR. 
C Possible causes of errors include:
C  -  data file not properly opened.
C  -  data file opened for read only.
C  -  data file not initialized properly with call to ex_put_init (EXPINI for Fortran).
C  -  an element block with the same ID has already been specified.
C  -  the number of element blocks specified in the call to ex_put_init (EXPINI for
C     Fortran) has been exceeded.

C SUBROUTINE EXPELB (IDEXO, IDELB, NAMELB, NUMELB, NUMLNK, NUMATR, IERR)
C 
C INTEGER IDEXO (R) EXODUS file ID
C INTEGER IDELB (R) The element block ID. 
C CHARACTER*MXSTLN NAMELB (R) The type of elements in the element block.
C     For historical reasons, this string should be all upper case.
C INTEGER NUMELB (R) The number of elements in the element block.
C INTEGER NUMLNK (R) The number of nodes per element in the element block.
C INTEGER NUMATR (R) The number of attributes per element in the element block.
C                    (These attrib arrays are not currently used)
C INTEGER IERR (W)  Returned error code. If no errors occurred, 0 is returned.
C 
C There is an Element Block for each itetclr (material) value 1 thru N


      call mmgetblk
     &     ('elconblk',isubname,ipelconblk,8*maxelblk,1,ierr)
      if (ierr .ne.0) then
         print*,'ERROR from mmgetblk elconblk: ',ierr
         ierror_return = ierr
         go to 9999
      endif

      do i=1,nelblocks

c        set element attribute array to 0
c        it is not used, so do not write it
         inumatt = 0

c        element block info

c        there is no reason to make this large id number
c        iblk_id = elcolblk(i)*10000
         iblk_id = elcolblk(i)

         eltyp=eltypblk(i)
         if (eltyp.eq.8) then
            eltyp_str='HEX'
         else if (eltyp.eq.7) then
            eltyp_str='WEDGE'
         else if (eltyp.eq.6) then
            write(logmess,"(a)")
     *           "Warning: element type not supported by Exodus"          
            call writloga('default',0,logmess,0,ierr)
            write(logmess,"(a)")
     *           "Write out elements as polyhedra - Not implemented"
            stop 2
         else if (eltyp.eq.5) then
            eltyp_str='TETRA'
         else if (eltyp.eq.4) then
            eltyp_str='QUAD'
         else if (eltyp.eq.3) then
            eltyp_str='TRI3'
         else if (eltyp.eq.2) then
            eltyp_str='BEAM'
         endif

         if (if_debug_local .gt. 0) then
            write(logmess,'(a)')
     &       'EXPELB write block elements: '
            call writloga('default',1,logmess,0,ierr)
            write(logmess,'(2x,a,a)')
     &      'Block Type:        ',eltyp_str
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i7)')
     &      'Block ID:        ',iblk_id
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i12)')
     &      'Block Elements:  ',numelblk(i)
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i5)')
     &      'Element Nodes:     ',nelnodes(eltyp)
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i5)')
     &      'Block attrib:      ',inumatt
            call writloga('default',0,logmess,0,ierr)
         endif

         call EXPELB(idexo, iblk_id, eltyp_str, 
     &        numelblk(i), nelnodes(eltyp), 
     &        inumatt, status)

         if (status.lt.0) then 
              call exerr(isubname,
     &       "ERROR writing ExodusII element block. ",status)
             ierror_return = status
             go to 9999
         endif
         if (status.gt.0) then  
              call exerr(isubname,
     &       "Warning writing ExodusII element block. ",status)
             status = 0
         endif

C     SECTION WRITE Element block attrib (not implemented)

C     Note  element attrib is turned off by EXPELB argument inumatt = 0 
C
C     may need attrib written with 0 values for backward compatiblity
C     EXPEAT is where the attribute data (ie attrib1) is written.
C     EXPEAN is where the attribute name (ie attrib1_name) is written.
C
C     Suggested syntax:
C       cmo/addatt/ mo / exo_attrib / VINT / scalar/ scalar /constant/permanent/ 0
C       cmo/setatt/ mo / exo_attrib / [flag value]
C
C     check for mesh object attribute exo_attrib
C     If exo_attrib is -1, do not write to file, this will save space. (current default)
C     If exo_attrib = 0, call exodus to write attrib with 0's (for backward compatibility)
C     If exo_attrib is 1, fill attrib with to itetclr (block id) 
C



C        SECTION WRITE Element Connectivity

c        ecolnblk array - 8 is the maximum number of nodes per element
c        maxelblk is the maximum number of elements in any block

c        fill arrays for exodus ordering based on sort key ikey_utr
         k = 0
         do j = begidxblk(i), begidxblk(i)+numelblk(i)-1
            elem_id = ikey_utr(j)               
            offset = itetoff(elem_id)
            do k1 = 1, nelnodes(eltyp)
               k2 = lag2exo_nmap(k1,eltyp)
               elconblk(k+k2) = itet(offset+k1)
            enddo               
            k = k + nelnodes(eltyp)
         enddo

       if (if_debug_local .gt. 0) then
            write(logmess,'(a)')
     &       'EXPELC write block connectivity: '
            call writloga('default',1,logmess,0,ierr)
            write(logmess,'(2x,a,i7)')
     &      'Block ID:        ',iblk_id
            call writloga('default',0,logmess,0,ierr)
            write(logmess,'(2x,a,i12,i12)')
     &      'Block index     ',begidxblk(i), begidxblk(i)+numelblk(i)-1
            call writloga('default',0,logmess,0,ierr)
         endif


         call EXPELC(idexo, iblk_id, elconblk, status)

         if (status.ne.0) then 
            call exerr(isubname,
     &      " ERROR writing ExodusII element connectivity. ",status)
            ierror_return = status
            go to 9999
         endif

      enddo


C     SECTION WRITE node sets (pset) (if defined) 

      if (auto_nodesets .eqv. .true.) then

      ibeg = 1
      do i = 1, npsets

         call EXPNP(idexo, nodeset_tag(i), nnodes_ns(i), 0, status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " ERROR writing ExodusII node sets. ",status)
            ierror_return = status
            go to 9999
         endif

         call EXPNS(idexo, nodeset_tag(i), outernodes(ibeg), status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " ERROR writing ExodusII outer nodes. ",status)
            ierror_return = status
            go to 9999
         endif
         
         ibeg = ibeg + nnodes_ns(i)
      enddo

      else
      if (npsets .ne. 0) then
      set_id = 0

      write(logmess,'(a,i10,a)')'WRITING EXODUS NODE SETS:',
     &   npsets, ' sets in total'

      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a32,a20,a25)')'Nodeset Names',
     &   'Set ID','# nodes in set'
      call writloga('default',0,logmess,0,ierr)

      do j = 1, nbitsmax
         if((psetnames1(j) .ne.' ')   .and.
     *      (psetnames1(j)(1:5) .ne. '-def-')) then
            set_id = set_id + 1
            cpt1='pset'
            cpt2='get'
            cpt3=psetnames1(j)
            mpno=nnodes
            call pntlimc(cpt1,cpt2,cpt3,ippmpary1,mpno,
     *          nnodes,isetwd,itp1)

            print*, 'Writing to EXO file nodeset no. ', set_id
            print*, 'Nodeset name: ', trim(cpt3)

C           EX_NODE_SET = EXNSET = 2 = node set property code
            status = 0
            ilen = icharlnf(trim(cpt3))

c          call c routine
c          void exo_put_sets_
c          const int_ptrsize *idexo, const int_ptrsize *type,
c          const char *name, const int_ptrsize *nlen,
c          const int_ptrsize *sid, const int_ptrsize *nentry,
c          const int_ptrsize *num_df,
c          const int_ptrsize *set_entry_list, int_ptrsize *status
c          where int_ptrsize is usually integer 8

            call exo_put_sets(idexo, EXNSET, trim(cpt3), 
     *         ilen, set_id, mpno, 0, pmpary1, status)

            if (status .eq. 0) then
               write(logmess,'(a32,i20,i25)')trim(cpt3),
     &         set_id, mpno
               call writloga('default',0,logmess,0,ierr)
            else
               write(logmess,'(a,i10)')'ERROR in writing nodeset',
     &         status
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
      enddo
      endif
      endif

C     SECTION WRITE element sets (eltset) (if defined) 

      if (neltsets .ne. 0) then
      set_id = 0

      write(logmess,'(a,i10,a)')'WRITING EXODUS ELEMENT SETS:',
     &   neltsets, ' sets in total'
      call writloga('default',1,logmess,0,ierr)
      write(logmess,'(a32,a20,a25)')'Elemset Names',
     &   'Set ID','# elements in set'
      call writloga('default',0,logmess,0,ierr)
      do j = 1, nbitsmax
         if((eltsetnames1(j) .ne.' ')   .and.
     *      (eltsetnames1(j)(1:5) .ne. '-def-')) then
            set_id = set_id + 1
            cpt1='eset'
            cpt2='get'
            cpt3=eltsetnames1(j)
            mpno=nelements
            call eltlimc(cpt1,cpt2,cpt3,ipempary1,mpno,
     *          nelements,xtetwd)
            call mmgetblk('exo_elt',isubname,ipexo_elt_ary, 
     *        mpno,2,ierr)
            do i = 1, mpno
              exo_elt_ary(i) = elem_map(empary1(i))
            enddo

C           EX_ELEM_SET = NOT FOUND in new exodusII.inc !!
            call exo_put_sets(idexo, EX_ELEM_SET, trim(cpt3), 
     *         icharlnf(trim(cpt3)), set_id, mpno, 
     *         0, exo_elt_ary, status)
            if (status .eq. 0) then
               write(logmess,'(a32,i20,i25)')trim(cpt3),
     &         set_id, mpno
               call writloga('default',0,logmess,0,ierr)
            else
               write(logmess,'(a,i10)')'ERROR in writing elemset',
     &         status
               call writloga('default',0,logmess,0,ierr)
            endif
            call mmrelblk('exo_elt',isubname,ipexo_elt_ary, ierr)
         endif
      enddo
      endif


C     SECTION WRITE Side Sets (if defined) 

c     Side Set transform LaGriT local face numbers to Exodus II local face numbers

      offset = 0
      do i = 1, nsidesets
         do j = 1, nfaces_ss(i)
            k = offset + j
            elem_id = ikey_utr(sselemlist(k))
            eltyp = itettyp(elem_id)
            ssfacelist(k) = lag2exo_fmap(ssfacelist(k),eltyp)
         enddo
         offset = offset + nfaces_ss(i)
      enddo

c
c The function ex_put_side_set_param (or EXPSP for Fortran) writes the side set ID and the
c number of sides (faces on 3-d element types; edges on 2-d element types) which describe a
c single side set, and the number of distribution factors on the side set. Because each side of a
c side set is completely defined by an element and a local side number, the number of sides is
c equal to the number of elements in a side set.
c In case of an error, ex_put_side_set_param returns a negative number; a warning will return
c a positive number. EXPSP returns a nonzero error (negative) or warning (positive) number in
c IERR. Possible causes of errors include:
c    - data file not properly opened with call to ex_create or ex_open (EXCRE or EXOPEN
c      for Fortran).
c    - data file opened for read only.
c    - data file not initialized properly with call to ex_put_init (EXPINI for Fortran).
c    - the number of side sets specified in the call to ex_put_init (EXPINI for Fortran)
c      was zero or has been exceeded.
c    - a side set with the same ID has already been stored.

C SUBROUTINE EXPSP (IDEXO, IDESS, NSESS, NDESS, IERR)
C INTEGER IDEXO (R) EXODUS file ID from previous call to EXCRE or EXOPEN.
C INTEGER IDESS (R) The side set ID.
C INTEGER NSESS (R) The number of sides (faces or edges) in the side set.
C INTEGER NDESS (R) The number of distribution factors on the side set.
C INTEGER IERR (W) Returned error code. 0 for no errors. 

      ibeg = 1
      do i = 1, nsidesets

         call EXPSP(idexo, sideset_tag(i), nfaces_ss(i), 0, status)
         if (status.lt.0) then 
            call exerr(isubname,
     &      " ERROR writing ExodusII sideset faces. ",status)
            ierror_return = status
            go to 9999
         endif
         if (status.gt.0) then
            call exerr(isubname,
     &      " Warning writing ExodusII sideset faces. ",status)
            status = 0
         endif

         call EXPSS(idexo, sideset_tag(i), sselemlist(ibeg),
     &        ssfacelist(ibeg), status)
         if (status.lt.0) then
            call exerr(isubname,
     &      " ERROR writing ExodusII sideset elements. ",status)
            ierror_return = status
            go to 9999
         endif
         if (status.gt.0) then
            call exerr(isubname,
     &      " Warning writing ExodusII sideset elements. ",status)
            status = 0
         endif

         
C     change to writlog for output to log files
c     print*,'------------------------------------------'
c     print*,'EXPSS loop: ',i
c     print*,'sideset tag: ',sideset_tag(i),'nfaces: ',nfaces_ss(i)
c     print*,'index starting at ',ibeg
c     print*,'   nfaces_ss :',nfaces_ss(i)
c     print*,'   sselemlist :',sselemlist(ibeg)
c     print*,'   ssfacelist :',ssfacelist(ibeg)
c     print*,'------------------------------------------'

c        write SS sideset information for logfiles

         if (i .eq.1) then
           write(logmess,'(a)')
     &     '------------------------------------------'
           call writloga('default',1,logmess,0,ierr)
            write(logmess,'(a)') 'EXPSS loop: '
            call writloga('default',0,logmess,0,ierr)
         endif

         write(logmess,'(2x,i7,a,1x,i12,a,1x,i12)')
     &     i,' Side Set tag: ',sideset_tag(i),
     &     ' Faces: ',nfaces_ss(i)
         call writloga('default',0,logmess,0,ierr)

         if (if_debug_local .ne. 0) then
             print*,'   nfaces_ss : ',nfaces_ss(i)
             print*,'   sselemlist :',sselemlist(ibeg)
             print*,'   ssfacelist :',ssfacelist(ibeg)
         endif

         ibeg = ibeg + nfaces_ss(i)
      enddo

      if (nsidesets .gt. 0) then
         write(logmess,'(a)')
     &     '------------------------------------------'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a,i7,a,i14,a,i14)')
     &     'Done ExodusII Side Sets Total: ',nsidesets
         call writloga('default',0,logmess,1,ierr)

         if ( auto_facesets .eqv. .true.) then
           write(logmess,"(a)")
     *     "Side Sets detected automatically."
           call writloga('default',0,logmess,1,ierr)
         endif
      endif


C     SECTION DONE with ExodusII file

c     Close the ExodusII file
      call EXCLOS(idexo, status)
      if (status.ne.0) then 
           call exerr(isubname,
     &      " ERROR closing ExodusII file. ",status)
            ierror_return = status
            go to 9999
       endif

c
c     For netCDF output reset ioflag of itetclr
c     TAM - why are we changing ioflag for itetclr ?
c
c     write(cbuff,*) 'cmo/modatt/' // cmo_name(1:len_cmo_name)//
c    &               '/ itetclr / ioflag / l ; finish'
c     call dotask(cbuff,ierr)
c

 9999 continue

C     CLEANUP and RETURN 

c     Turn on screen output to allow important information
      call writset('stat','tty','on',ierr)

      if(if_debug_local .gt. 1) call mmprint()

      
      if(if_debug_local .ne. 0) then 
         write(logmess,'(a,i5)')
     &     'dumpexodusII debug mode ON set to: ',if_debug_local
         call writloga('default',0,logmess,0,ierr)
      endif

C     release all memory
      call mmrelprt(isubname, ierr)

      ierror = ierror_return
      if (ierror .ne. 0) then
         write(logmess,"(a,i5)")
     *   "ExodusII dump exiting with ERROR flag: ",ierror
         call writloga('default',1,logmess,1,ierr)
      else
         write(logmess,"(a,a,a,a,a,a)")'ExodusII: ',
     &      'Done writing to ExodusII file: ',
     &      filename(1:icharlnf(filename)),
     &      ' using cmo: ',cmo_name(1:len_cmo_name)
            call writloga('default',1,logmess,1,ierr)
      endif


      return
      end

C     DONE dumpexodusII() 

c     *******************************************************************
c
c     Get nodes of a local face of an element
c
c     *******************************************************************

      subroutine get_face_nodes(faceid, nfn, fnodes, eltype, nelnodes, 
     &     elnodes)

      integer faceid
      integer nfn, fnodes(4), eltype, nelnodes
      integer elnodes(nelnodes)
      integer fnode_tmpl(4,6,4), nfn_tmpl(6,4)
      data fnode_tmpl 
     &     /2,3,4,0, 1,4,3,0, 1,2,4,0, 1,3,2,0, 0,0,0,0, 0,0,0,0,
     &      1,4,3,2, 1,2,5,0, 2,3,5,0, 3,4,5,0, 4,1,5,0, 0,0,0,0,
     &      1,3,2,0, 4,5,6,0, 1,2,5,4, 2,3,6,5, 1,4,6,3, 0,0,0,0,
     &      1,4,3,2, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7, 1,5,8,4/
      data nfn_tmpl
     &     /3,3,3,3,0,0,
     &      4,3,3,3,0,0,
     &      3,3,4,4,4,0,
     &      4,4,4,4,4,4/
      integer i

      if (eltype .eq. 3) then   ! for tris, Face i is opp node i
         nfn = 2
         fnodes(1) = elnodes(1 + mod(faceid,nelnodes))
         fnodes(2) = elnodes(1 + mod(faceid+1,nelnodes))
      else if (eltype .eq. 4) then
         nfn = 2
         fnodes(1) = elnodes(faceid)
         fnodes(2) = elnodes(1 + mod(faceid,nelnodes))
      else
         nfn = nfn_tmpl(faceid,eltype-4)
            
         do i = 1, nfn
            fnodes(i) = elnodes(fnode_tmpl(i,faceid,eltype-4))
         enddo
      endif

      return
      end



c     ****************************************************************
c
c     Get dihedral angle between two triangles
c     
c     Assume that triangles are given as (p1,p2,p3) and (p2,p1,p4)
c
c     ****************************************************************
      

      subroutine get_dihed_ang(txyz1, txyz2, cosang)
      real*8 txyz1(3,3), txyz2(3,3)
      real*8 cosang
      real*8 v1(3), v2(3), n1(3), n2(3), t1(3), t2(3), t1_len, t2_len


c     edge vecs for triangle 1

      do i = 1, 3
         v1(i) = txyz1(i,2)-txyz1(i,1)
         v2(i) = txyz1(i,3)-txyz1(i,1)
      enddo

c     cross product of vec1 and vec2 is normal to t1

      n1(1) = v1(2)*v2(3)-v1(3)*v2(2)
      n1(2) = v1(3)*v2(1)-v1(1)*v2(3)
      n1(3) = v1(1)*v2(2)-v1(2)*v2(1)

c     cross product of n1 and v1 is normal to v1 in the plane of the t1

      t1(1) = n1(2)*v1(3)-n1(3)*v1(2)
      t1(2) = n1(3)*v1(1)-n1(1)*v1(3)
      t1(3) = n1(1)*v1(2)-n1(2)*v1(1)


      t1_len = sqrt(t1(1)*t1(1)+t1(2)*t1(2)+t1(3)*t1(3))



c     edge vecs for triangle 2

      do i = 1, 3
         v1(i) = txyz2(i,2)-txyz2(i,1)
         v2(i) = txyz2(i,3)-txyz2(i,1)
      enddo

c     cross product of vec1 and vec2

      n2(1) = v1(2)*v2(3)-v1(3)*v2(2)
      n2(2) = v1(3)*v2(1)-v1(1)*v2(3)
      n2(3) = v1(1)*v2(2)-v1(2)*v2(1)

c     cross product of n2 and v1 is normal to v1 in the plane of the t2

      t2(1) = n2(2)*v1(3)-n2(3)*v1(2)
      t2(2) = n2(3)*v1(1)-n2(1)*v1(3)
      t2(3) = n2(1)*v1(2)-n2(2)*v1(1)


      t2_len = sqrt(t2(1)*t2(1)+t2(2)*t2(2)+t2(3)*t2(3))


      cosang = (t1(1)*t2(1)+t1(2)*t2(2)+t1(3)*t2(3))/(t1_len*t2_len)

      return
      end




c     ****************************************************************
c
c     Get included angle between two lines going ccw from line 1 to line 2
c     Assumption is point 1 of both lines is the common point
c
c     ****************************************************************
      

      subroutine get_int_ang(lxyz1, lxyz2, cosang)

C     changed dimension to be same as used (3,2)
C     real*8 lxyz1(3,3) 
C     real*8 lxyz2(3,3)
      real*8 lxyz1(3,2) 
      real*8 lxyz2(3,2)
      real*8 cosang
      real*8 v1(3), v2(3), v1_len, v2_len


c     vectors representing both lines (v1 and v2)

      do i = 1, 3
         v1(i) = lxyz1(i,2)-lxyz1(i,1)
         v2(i) = lxyz2(i,2)-lxyz2(i,1)
      enddo

      v1_len = sqrt(v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3))

      v2_len = sqrt(v2(1)*v2(1)+v2(2)*v2(2)+v2(3)*v2(3))


      cosang = (v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3))/(v1_len*v2_len)

      return
      end

c     ***** END EXODUSII PREPROCESSING *****
#endif
c     ***** END EXODUSII PREPROCESSING *****
