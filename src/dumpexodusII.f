
c     **************************************************************
c     subroutine dumpexodusII(ifiles)

      subroutine dumpexodusII(
     >               imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)

C     dump/exo/ ifile / cmoname
C               default: single argument for output filename
C
C     dump/exo/ ifile / cmoname / facesets / files / file1, file2, ...filen
C               output filename
C               import facesets from files with tag based on order 1 to n
C     
c
c     **************************************************************
c
c     WARNING: works only for 1 element type per material 
c     But if we sorted based on itetcolor and a subsort based on 
c     itettyp, then it could be made to work for hybrid meshes
c
      implicit none

      include 'exodusII.inc'
      include 'lagrit.h'

c     arguments
c     character*(*) ifile

      integer nwds
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror

c     local variables

c     I am unable to successfully use blockcom.h so I am redefining some
c     vars in place of nelmnen and nelmnef

      integer nelnodes(8)
      data nelnodes / 1, 2, 3, 4, 4, 5, 6, 8 /
      integer nelfaces(8)
      data nelfaces / 0, 1, 3, 4, 4, 5, 5, 6 /

      integer i,j,k,ii,j1, k1, k2, idx, idx1, idx2, ioffset
      integer ierr, ierrw
      integer len_cmo_name, iblk_num, if_debug_local
      integer icharlnf,eltyp
      integer ierror_return

      integer nnodes, nelements, nen, mbndry
      integer iout,loutx,louty,loutz,loutk,itype
      integer lout
      integer nsdgeom, ndimtopo
      integer status
      integer ilen, llen_string, llen_line
      integer id_elem, nelblocks, iblockel
      integer inumqarec
      integer wsize,fsize
      integer maxelblk
      integer nouter1, nouter2, nnouter
      integer nsidesets1, nsidesets2, nsidesets, nnodesets
      integer import_sidesets, nfiles
      integer n11, n12, n13, n21, n22, n23
      integer fnodes_j(4), fnodes_k(4)
      integer nfnk, nfnj, nf, nn
      integer nnj, nnk, col_j, col_k, nmat
      integer offset, ibeg, iend
      integer elem_id, iblk_id
      integer idexo, icompws, iows, inumatt, num_qa_rec
      integer idexi, in_compws, in_ws, in_numatt, in_num
      integer iunit, iflag, ncount, ival, ival2
      integer*4 iunit4

      real*8  t1xyz(3,3), t2xyz(3,3), lxyz1(3,2), lxyz2(3,2)
      real*8  cosang, cosang_flat
      real api,vers
      
      logical done

      integer num_att_vec(2)
      integer qarecvec(3)

      character*72 ifile, ifile2, filename, cmo_name
      character*72 flist(nwds)
      pointer (ipflist, flist)

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
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      pointer (ipitetp,itetp)
      integer itetp(*)

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

      pointer (ipelconblk,elconblk)
      integer*4 elconblk(*)


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
      pointer (ipsselemlist,sselemlist)
      integer*4 sselemlist(*)
      pointer (ipssfacelist,ssfacelist)
      integer*4 ssfacelist(*)
      pointer (ipfmarked,fmarked)
      integer fmarked(*)

      pointer (ipouternodes,outernodes)
      integer*4 outernodes(*)
      pointer (ipnmarked,nmarked)
      integer nmarked(*)

C
C     Wrapper function for the Exodus excre function
C
      integer excre_wrapper
      external excre_wrapper


c**********************************************************
C     Begin dumpnetcdf
C
      ierror_return = 0
      if_debug_local = 0
      import_sidesets = 0
c
      isubname ='dumpexodusII'
      lentitle= 'LaGriT to ExodusII output'
      lenebprop1='ID'

      cosang_flat = -0.707      ! Approximately 135 degrees

      api=4.5700002
      vers=4.5700002
      wsize=8
      fsize=0
      llen_string=33
      llen_line=81
      inumqarec=1
      inumatt=1

C     there is a delay during setup, write to screen
C     so user knows work is being done.
      write(logmess,"(a)")'ExodusII dump:' 
      call writloga('default',1,logmess,0,ierrw)

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
     *   " Error : filename not given."
         call writloga('default',0,logmess,1,ierr)
         ierror_return = -1
         go to 9999
      endif

      if (msgtype(4).eq. 3) then
         cmo_name = cmsgin(4)
      else
         write(logmess,"(a,a)") isubname,
     *   " Error : cmo name not given."
         call writloga('default',0,logmess,1,ierr)
         ierror_return = -1
      endif

      
C     dump/exo/ ifile/ cmoname / facesets 
C     dump/exo/ ifile / cmoname/ facesets / on
C     dump/exo/ ifile / cmoname/ facesets / off
C     dump/exo/ ifile / cmoname/ facesets / file1,file2,...filen
C     dump/exo/ ifile / cmoname/ facesets / on file1,file2,...filen
C     dump/exo/ ifile / cmoname/ facesets / off file1,file2,...filen

      if (msgtype(5).eq.3 .and. cmsgin(5)(1:8).eq.'facesets') then

         if (nwds.gt.5 .and. msgtype(6).eq.3 ) then
           if (cmsgin(6).eq. 'on') then
	      option = cmsgin(6)
              if (nwds .gt. 6) then
	      import_sidesets = 1

	      call mmgetblk("flist", isubname, ipflist, 72*nwds, 1, ierr)
              if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk flist')

              nfiles = 0
              do ii = 7,nwds
        	nfiles = nfiles+1
        	flist(nfiles) = cmsgin(ii)
        	print*,'got ',nfiles,flist(nfiles)
        	
        	ilen = icharlnf(flist(nfiles))
        	call fexist(flist(nfiles)(1:ilen),ierr)
        	if(ierr.eq.0) then
        	  print*,'Missing facesets file: ',flist(nfiles)(1:ilen)
        	  ierror_return = -2
        	  goto 9999
        	endif

              enddo
	      endif

	   elseif (cmsgin(6).eq. 'off') then
	      option = cmsgin(6)
              if (nwds .gt. 6) then
	      import_sidesets = 1

	      call mmgetblk("flist", isubname, ipflist, 72*nwds, 1, ierr)
              if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk flist')

              nfiles = 0
              do ii = 7,nwds
        	nfiles = nfiles+1
        	flist(nfiles) = cmsgin(ii)
        	print*,'got ',nfiles,flist(nfiles)
        	
        	ilen = icharlnf(flist(nfiles))
        	call fexist(flist(nfiles)(1:ilen),ierr)
        	if(ierr.eq.0) then
        	  print*,'Missing facesets file: ',flist(nfiles)(1:ilen)
        	  ierror_return = -2
        	  goto 9999
        	endif

              enddo
	      endif
	   else
	      import_sidesets = 1

	      call mmgetblk("flist", isubname, ipflist, 72*nwds, 1, ierr)
              if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk flist')

              nfiles = 0
              do ii = 6,nwds
        	nfiles = nfiles+1
        	flist(nfiles) = cmsgin(ii)
        	print*,'got ',nfiles,flist(nfiles)
        	
        	ilen = icharlnf(flist(nfiles))
        	call fexist(flist(nfiles)(1:ilen),ierr)
        	if(ierr.eq.0) then
        	  print*,'Missing facesets file: ',flist(nfiles)(1:ilen)
        	  ierror_return = -2
        	  goto 9999
        	endif

              enddo 
	   endif
         else 

           write(logmess,"(a,a)") 'dump/exo/ ifile / cmoname', 
     *     " Unknown options, will write default facesets."
           call writloga('default',0,logmess,1,ierr)

         endif

      endif


C-----Done parsing input ------------------------------------

c     Get CMO info

C     set cmo and ierror_return in case things blow up
      call cmo_exist(cmo_name,ierr)
      if(ierr.ne.0) then
        write(logmess,*)
     *  'dump/exo Warning: cannot find default mesh object.'
        call writloga('default',0,logmess,0,ierrw)
        ierror_return = -3
        go to 9999
      endif
      len_cmo_name=icharlnf(cmo_name)

      filename=ifile(1:icharlnf(ifile))

      write(logmess,"(a,a)")
     & 'Writing to file: ',filename(1:icharlnf(filename))
       call writloga('default',0,logmess,0,ierr)

      write(logmess,"(a,a)")  
     & 'Using cmo: ',cmo_name(1:len_cmo_name)
      call writloga('default',0,logmess,0,ierr)

c     Turn off screen output of dotask commands
c
      if(if_debug_local .eq. 0)call writset('stat','tty','off',ierr)
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
c
      write(cbuff,*) 'cmo/modatt/' // cmo_name(1:len_cmo_name)//
     &               '/ itetclr / ioflag / lga ; finish'
      call dotask(cbuff,ierr)


c     Retrieve detailed info about mesh including coordinates, connectivity

      call cmo_get_info('xic',cmo_name,ipxic,lout,itype,ierr)
      call cmo_get_info('yic',cmo_name,ipyic,lout,itype,ierr)
      call cmo_get_info('zic',cmo_name,ipzic,lout,itype,ierr)
      call cmo_get_info('imt1',cmo_name,ipimt1,lout,itype,ierr)
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







c-------------------------------------------------------------------
c     PREPROCESS
c-------------------------------------------------------------------



c     FIRST DO SOME PROCESSING TO SET UP ELEMENT BLOCKS

c
c     First count the blocks, then allocate an array, then fill the array.
c
c     nblocks is number of blocks
c     blockinfo has number of elements per block

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
            
c
c           start a new block with new material
c

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


c
c     Maximum number of elements in any block
c

      maxelblk=0
      do i = 1, nelblocks
         if (numelblk(i) .gt. maxelblk) maxelblk = numelblk(i)
      enddo







c     THEN DO SOME PROCESSING TO SET UP SIDESETS

c     First we will form a list of outer faces - outerfaces1,outerelems1
c
c     Then we will form lists outerfaces2,outerelems2 containing faces
c     grouped according to contiguous sets separated by sharp angles
c
c     Then we will repopulate outerfaces1,outerelems1 from outerfaces2,
c     outerelems2 by forming sub-groups of faces with the same material IDs

c     We assume there will be a maximum of 1000 sidesets


c     Count how many outer faces there are

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


      if (option .eq. 'off') then
	if ( import_sidesets .eq. 1) then
	   call mmgetblk
     &        ('sselemlist',isubname,ipsselemlist,3*nouter1,1,ierr)
	   call mmgetblk
     &        ('ssfacelist',isubname,ipssfacelist,3*nouter1,1,ierr)
	   nnodesets = 0
	   goto 2001
	else
	   print *, 'No facesets imported, no. of faceset = 0.'
	   nsidesets = 0
	   nnodesets = 0
	   goto 4000
	endif
      else
	goto 2000
      endif


c     Break up these faces first according to sharp edges (or edges
c     according to sharp corners)

 2000 print *, 'GOING THROUGH THE LONG LOOP!!!!!!!!!'
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





c     Now further subdivide the sidesets based on material IDs of elements
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



c     Now form the final sideset info

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
      print *, 'JUST GONE THROUGH THE LONG LOOP!!!!!!!!!'


c     ASSUME THERE ARE AS MANY NODESETS AS SIDESETS - EACH NODESET IS
c     JUST A COLLECTION OF THE UNIQUE NODES OF A SIDESET
      
c     assuming each mesh face has a max of four nodes, allocate the most
c     conservative amount of space for list of outer nodes

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
      nnodesets = nsidesets


C TAM - add option to import facesets from files
C     update facesets information
C     nsidesets - number of sets = nfiles
C     sideset_tag(1:nfiles) - id number tag to assign
C     nfaces_ss( )  - indexes into big arrays 
C     sselemlist( ) - indexed big array with all elem sets 
C     ssfacelist( )  - indexed big array with all face sets)

C     get sidesets from list of files
      if (import_sidesets .eq. 1) then
       
 2001 print*,'Exodus SIDESETS imported from files. ',nfiles
      print*,'Overwrite internal defs with nsidesets: ',nsidesets
      
      offset = 0
      ibeg=0
      do i = 1, nfiles

        iunit=-1
        ifile2 = flist(i)
        ilen = icharlnf(ifile2)
        call hassign(iunit,ifile2,ierr)
        iunit4 = iunit
        if (ierr.lt.0 .or. iunit.lt.0) then
            write(logmess,*) 'WARNING: file not opened: '
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

       print*
       print*,'done reading ',ifile2
       print*,' total values ',ncount
       if (ncount .le. 0) then
         print*,'No idelem idface tags in file ',ifile2(1:ilen)
       endif

C     ********** now overwrite previous sideset arrays *******
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

C     ********** done overwrite previous sideset arrays *******

C     end loop through nfiles
      enddo

 9000 if (ierr .lt. 0) then
        print*,'Could not read facesets file ',ifile2(1:ilen)
      endif
      goto 4000
  

C     default - create facesets internally using materials
      else 
      
        print*,'Exodus SIDESETS defined internally. '
        print*,'   nsidesets: ',nsidesets

      endif





c----------------------------------------------------------------
c     WRITE OUT INFO TO EXODUSII FILE
c----------------------------------------------------------------


c
c     create Exodus II file
c

C     This is the "computer word size." It should probably not be
C     hard-coded here. Likewise for iows, the IO word size.
 4000 icompws=8
      iows = 8
      idexo = excre_wrapper(filename,exclob,icompws,iows,status,
     *      icharlnf(filename))

      if (status.ne.0) then 
        call exerr(isubname,
     & " Error opening ExodusII file. ",status)
        ierror_return = status
        go to 9999
      endif


c
c     Put initialization information
c

C TAM
      if (option .eq. 'on') then
	print*,'INITIALIZE exodus '
	print*,'   nnodes: ',nnodes
	print*,'   nelements: ',nelements
	print*,'   nelblocks: ',nelblocks
	print*,'   nnodesets: ',nnodesets
	print*,'   nsidesets: ',nsidesets
	if (import_sidesets .eq. 1) then
           nsidesets = nfiles
           print*,'   nsidesets changed to: ',nsidesets
	else
	   print*, 'DEFAULT - parameters created internally.'
	endif
      elseif (option .eq. 'off') then
	if (import_sidesets .eq. 1) then
           nsidesets = nfiles
           print*,'   nsidesets is created: ',nsidesets
	   print*,'INITIALIZE exodus '
	   print*,'   nnodes: ',nnodes
	   print*,'   nelements: ',nelements
	   print*,'   nelblocks: ',nelblocks
	   print*,'   nnodesets: ',nnodesets
	   print*,'   nsidesets: ',nsidesets
	else
	   print*, 'No facesets imported, nsidesets = 0'
   	   print*,'INITIALIZE exodus '
	   print*,'   nnodes: ',nnodes
	   print*,'   nelements: ',nelements
	   print*,'   nelblocks: ',nelblocks
	   print*,'   nnodesets: ',nnodesets
	   print*,'   nsidesets: ',nsidesets
	endif
      endif
	

      call EXPINI(idexo, 'Lagrit-to-ExodusII', nsdgeom, nnodes, 
     &     nelements, nelblocks, nnodesets, nsidesets, status)

      if (status.ne.0) then 
         call exerr(isubname,
     &   " Error writing ExodusII init parameters.",status)
         ierror_return = status
         go to 9999
      endif

c
c     Put some QA info - problem name, date, time etc.
c

      num_qa_rec = 1

      qa_record(1,1) = cmo_name(1:MXSTLN)
      qa_record(2,1) = "probname"
      qa_record(3,1) = "Today"
      qa_record(4,1) = "Time"

      call EXPQA(idexo, num_qa_rec, qa_record, status)
      if (status.ne.0) then 
          call exerr(isubname,
     &    " Error writing ExodusII date/time. ",status)
          ierror_return = status
          go to 9999
      endif

      call EXPCOR(idexo, xic, yic, zic, status)
      if (status.ne.0) then 
          call exerr(isubname,
     &    " Error writing ExodusII coordinates. ",status)
          ierror_return = status
          go to 9999
      endif
      
c     Write out element blocks

      call mmgetblk
     &     ('elconblk',isubname,ipelconblk,8*maxelblk,1,ierr)

      do i=1,nelblocks

c        element block info

         iblk_id = elcolblk(i)*10000

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


         call EXPELB(idexo, iblk_id, eltyp_str, numelblk(i),
     &        nelnodes(eltyp), inumatt, status)
         if (status.ne.0) then 
              call exerr(isubname,
     &    "Error writing ExodusII element block. ",status)
          ierror_return = status
          go to 9999
      endif




c        element connectivity array - 8 is the maximum number of nodes
c        in any element and maxelblk is the maximum number of elements
c        in any block


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

         call EXPELC(idexo, iblk_id, elconblk, status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " Error writing ExodusII element connectivity. ",status)
            ierror_return = status
            go to 9999
         endif

      enddo


C     print *, "CHECK ERR P1"

c     Write out node sets

      ibeg = 1
      do i = 1, nnodesets

         call EXPNP(idexo, nodeset_tag(i), nnodes_ns(i), 0, status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " Error writing ExodusII node sets. ",status)
            ierror_return = status
            go to 9999
         endif

         call EXPNS(idexo, nodeset_tag(i), outernodes(ibeg), status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " Error writing ExodusII outer nodes. ",status)
            ierror_return = status
            go to 9999
         endif
         
         ibeg = ibeg + nnodes_ns(i)
      enddo

C     print *, "CHECK ERR P2"

c     Write out side sets


c     First transform LaGriT local face numbers to Exodus II local face
c     numbers

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


c     Then write out the info

      ibeg = 1
      do i = 1, nsidesets

         call EXPSP(idexo, sideset_tag(i), nfaces_ss(i), 0, status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " Error writing ExodusII sidesets tags. ",status)
            ierror_return = status
            go to 9999
         endif



         call EXPSS(idexo, sideset_tag(i), sselemlist(ibeg),
     &        ssfacelist(ibeg), status)
         if (status.ne.0) then 
            call exerr(isubname,
     &      " Error writing ExodusII sidesets. ",status)
            ierror_return = status
            go to 9999
         endif

         
      print*,'EXPSP ',i,' starting at ',ibeg
      print*,'   sideset_tag :',sideset_tag(i)
      print*,'   nfaces_ss :',nfaces_ss(i)
      print*,'EXPSS ',i
      print*,'   sselemlist :',sselemlist(ibeg)
      print*,'   ssfacelist :',ssfacelist(ibeg)

         ibeg = ibeg + nfaces_ss(i)
      enddo

c     Close the Exodus II file

      call EXCLOS(idexo, status)
      if (status.ne.0) then 
           call exerr(isubname,
     &      " Error closing ExodusII file. ",status)
            ierror_return = status
            go to 9999
       endif

c
c     For netCDF output reset ioflag of itetclr
c
      write(cbuff,*) 'cmo/modatt/' // cmo_name(1:len_cmo_name)//
     &               '/ itetclr / ioflag / l ; finish'
      call dotask(cbuff,ierr)
c

 9999 continue

c     Turn on screen output
      if(if_debug_local .eq. 0)call writset('stat','tty','on',ierr)

      if(if_debug_local .ne. 0)call mmprint()
c
      call mmrelprt(isubname, ierr)

      ierror = ierror_return
      if (ierror .ne. 0) then
         write(logmess,"(a,i5)")
     *   "ExodusII dump exiting with Error flag: ",ierror
         call writloga('default',1,logmess,1,ierr)
      endif


      return
      end





c     *******************************************************************
c
c     Get nodes of a local face of an element
c
c     *******************************************************************

      subroutine get_face_nodes(faceid, nfn, fnodes, eltype, nelnodes, 
     &     elnodes)
      integer*4 faceid
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
      real*8 lxyz1(3,3), lxyz2(3,3)
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


