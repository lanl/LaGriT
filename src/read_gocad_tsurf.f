      subroutine read_gocad_tsurf
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C#######################################################################
C    READ GOCAD TSURF (tri) or TSOLID (tetra)
C    This routine reads simple tri or tet meshes and skips
C    gocad objects such as Well, Geometry, Voxet, etc.
C 
C    FORMAT - read / gocad / file_name.ts or file_name.so / mo_name
C
C    INPUT ARGUMENTS - imsgin,xmsgin,cmsgin,msgtyp,nwds
C
C    OUTPUT - Triangle or Tet mesh object
C
C    file_name.ts = TSurf (triangulated surface)
C    file_name.so = TSolid (tets)
C
C
C General Format
C --------------------------
C HEADER
C The first line should indicate TSolid or TSurf file
C This is followed by coordinate and property information
C Node x y z and optional properties:
C --------------------------
C TVOLUME
C PVRTX 1 0 0 0 1
C PVRTX 2 20 0 0 2
C PVRTX 3 0 20 0 3
C PVRTX 4 20 20 0 4
C PVRTX 5 1.9999999999999991 0 0 5
C --------------------------
C Element connectivity and properties:
C --------------------------
C TETRA 788 805 806 350 2 1 2
C TETRA 805 834 357 350 2 2 2
C TETRA 762 736 431 396 1 3 1
C TETRA 805 806 358 836 2 4 2
C --------------------------
C END (optional keyword) 
C
C Optional Formats
C
C TFACE is a block of nodes and cells and attribute iblock is incremented
C END is usually end of file but can also indicate a new object
C 
C
C#######################################################################
C
C        $Log: read_gocad_tsurf.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS       Rev 1.3   18 Oct 2005 16:07:08   gable
CPVCS    Extend input file name length to 132 characters.
CPVCS 
CPVCS       Rev 1.2   23 Mar 2004 07:23:08   gable
CPVCS    Fix assignment of jtetoff array.
CPVCS    Added keywords PVRTX and PATOM as synonyms for
CPVCS    VRTX and ATOM (Brad Aagaard).
CPVCS   
CPVCS       Rev 1.1   26 Feb 2004 10:49:28   dcg
CPVCS    remove duplicate declarations
CPVCS    
CPVCS       Rev 1.0   22 Oct 2003 10:12:36   gable
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none

      include 'local_element.h'
C
C     Input variables
C
      integer nwds, imsgin(nwds), msgtyp(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      pointer (ipimt1, imt1)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)

      integer imt1(1000000)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      integer itet(10000000), jtet(10000000)

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8   xic(1000000), yic(1000000), zic(1000000)

C     these may be needed for .ts files
      pointer (ipiblock, iblock)
      integer iblock(1000000)
      pointer (ipid_node, id_node)
      integer id_node(1000000)

c     datptr points to pointer of added attributes
      pointer (ipn_datptr,n_datptr)
      pointer (ipe_datptr,e_datptr)
      integer n_datptr(100), e_datptr(100)

      pointer (ipiatt, iatt)
      integer iatt(*)

      pointer (ipivalues, ivalues)
      integer ivalues(1000000)
      pointer (ipxvalues, xvalues)
      real*8 xvalues(1000000)

c      
c local parser variables 
      integer lenparse, nwds2,nwds_vrtx,nwds_tet
      integer nmsg
      integer msg(128)
      real*8 xmsg(128)
      integer imsg(128)
      character*32 cmsg(128)
      character*32 n_attnames(128)
      character*32 e_attnames(128)
C     character*128 cmsgbig(512)

C
C     Local variables
      integer if_integer, if_real, if_character, nnodes_set,
     1    i, ii, j, ntets_set, ierror, ilen, ityp, ier, n_vrtx,
     2    n_elem, n_tet, n_tri, n_tface, n_object, iunit, istart,
     3    msg_num, i_tri_off, n_offset_vrtx, n_line_parse,
     4    id_vrtx, id_vrtx_max, n_tet_off, i_tet_off, 
     5    ninc, file_length,skipline,nEND, len_parse, ics,
     6    read_count, skip_count
     

       integer icharlnf, icharlnb


      integer plines, tetlines, trilines, elem_typ
      integer nen, nef, length, num_node_att, num_elem_att

      logical left_hand, if_nprops, if_tprops

      character*8 itype, ftype, mesh_type, z_type 
      character*32 isubname, cmoname, cmoatt
      character*132 cline, cbuff
      character*132 ifile
      character*512 logmess

c     same size as used in parse_string2
      character*4096 input_msg
      character*4096 input_nprops
      character*4096 input_tprops
C
C     ******************************************************************
C
C    TAM AUG 2019
C     major changes to code to replace user_sub calls with routine calls
C     and expand code to read 3D TSolid tets as well as 2D TSurf triangles
C     added error checking and removed hard-wired number of nodes and elements
C     replaced with a pre-read of file for counts and type (longer but safer)
C     Important variables such as isubname  now have default values.
C     All code with pre-set numbers have been removed, this includes code such as:
C      nnodes_set = 2000
C      ntets_set = 2000
C
C     ******************************************************************
C begin BEGIN
CCCC  set defaults

c     for message passing
      if_integer = 1
      if_real = 2
      if_character = 3
      isubname = "read_gocad"

c     valid gocad types are TSolid and Tsurf
      mesh_type = "TSolid"
      z_type = "Elev Up"
      left_hand = .false.
      if_nprops = .false.
      if_tprops = .false.

C     read_count and skip_count refer to file lines read
C     plines are parsed VRTX lines
C     tetlines and trilines are parsed TRGL and TETRA lines
C     nEND is a keyword expected once
      read_count = 0
      skip_count = 0
      plines = 0
      tetlines = 0
      trilines = 0
      ierror = 0
      nEND = 0

C     ******************************************************************
CCCC  parse commands

      if (nwds .ge. 3 .and. msgtyp(3).eq.if_character ) then
         ifile = cmsgin(3)
         ilen = icharlnf(ifile)
         ftype= ifile(ilen-2:ilen)
      else
        call x3d_error(isubname,'missing file name.')
        ierror = -1
        goto 9999
      endif
       
      if (nwds .eq. 4) then
        cmoname = cmsgin(4)
      else
        call x3d_error(isubname,'need 4 arguments.')
        ierror = -1
        goto 9999
      endif

C     check file extension for mesh type
      if (ftype.eq. ".ts") then
         mesh_type = "TSurf "
      endif
      if (ftype.eq. ".so") then
         mesh_type = "TSolid"
      endif

CCCC  done with command line options

C     ******************************************************************
C     OPEN  FILE 
C     Read twice, first time to check keywords and get counts
C     This takes longer but avoids memory errors
C
C        HEADER line 1
C        syntax line 1: GOCAD TYPE num_objects (hopefully 1)
C        where type is TSurf or TSolid
C
C        COORDINATE SYSTEM
C        Check for keyword ZPOSITIVE to indicate Depth or Elevation
C        If Depth this is left hand system and need to translate
C        From JewelSuite this means multiply Z values by -1
C
C        NODE PROPERTIES
C        syntax: PROPERTIES prop_name prop_name2 ...
C
C        TET PROPERTIES
C        syntax: TETRA_PROPERTIES prop_name prop_name2 ...
C
C        TRI PROPERTIES
C        syntax: TRGL_PROPERTIES prop_name prop_name2 ...
C  
C        Be cautious of PROPERTY_CLASS_HEADER lines that may include a colormap
C  

      iunit=-1
      ifile = ifile(1:icharlnf(ifile))
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'problem opening file.')
        ierror = -1
        goto 9999
      else
        write(logmess,'(a,a)') "Reading GOCAD file: ",ifile
        call writloga('default',0,logmess,0,ics)
      endif

C     READ file twice, first time to get sizes and types
C     we do not parse lines this first time through
      skipline = 0
      file_length = 0 
      do 
        input_msg = ' '
        read (iunit,'(a)', END=90) input_msg
        file_length = file_length + 1 

C       protect against extremly long lines and colormaps
        if (input_msg(1:1) .eq. "*") then 
            skipline = skipline+1
        else
          if (file_length.eq.1 .and. len(input_msg).gt.12) then
            if(input_msg(7:12) .eq. "TSolid") mesh_type = "TSolid"
            if(input_msg(7:11) .eq. "TSurf") mesh_type = "TSurf"
          endif
          if (input_msg(1:6) .eq. "PVRTX ") plines = plines +1
          if (input_msg(1:5) .eq. "VRTX ")  plines = plines +1
          if (input_msg(1:6) .eq. "TETRA ") tetlines = tetlines +1
          if (input_msg(1:5) .eq. "TRGL ")  trilines = trilines +1
        endif
      end do 


C     ******************************************************************
C     REWIND FILE to read data
90    rewind (iunit)

      if (file_length .le. 0) then
         write(logmess,'(a,a)') "ERROR: empty file: ",ifile
         call writloga('default',0,logmess,0,ics)
         ierror = ierror + 1
         goto 9999
      endif

C     Prepare mesh object and memory based on counts 

C debug for counts from first read
C     print*,"From first read:"
C     print*,"Mesh type = ",mesh_type
C     print*,"VRTX count = ",plines
C     print*,"TET  count = ",tetlines
C     print*,"TRI  count = ",trilines
C     print*,"Line count = ",file_length
C     print*,"Skip count = ",skipline
      
      if (trilines.ne.0 .and. tetlines.ne.0) then
          write(logmess,'(a)') 
     *    "Found both TRGL and TETRA, reading tets only."
          call writloga('default',0,logmess,0,ics)
          trilines = 0
      endif
      if (trilines.ne.0) then
         ntets_set = trilines
         elem_typ = ifelmtri
      endif
      if (tetlines.ne.0) then
         ntets_set = tetlines
         elem_typ = ifelmtet
      endif

      nnodes_set = plines
      if (plines .le. 0) then
         write(logmess,'(a,a)') "ERROR: NO VRTX defined in ",ifile
         call writloga('default',0,logmess,0,ics)
         ierror = ierror + 1
         goto 9999
      endif

      if (ntets_set .le. 0) then
         write(logmess,'(a,a)') 
     &   "Warning: NO elements defined in ",ifile
         call writloga('default',0,logmess,0,ics)
      endif

      if (mesh_type.eq. "TSolid") then
          cbuff = 
     1     'cmo/create/'//cmoname(1:icharlnf(cmoname))//
     2     '/ / /tet ; finish'
          call dotask(cbuff,ierror)
      else if (mesh_type.eq."TSurf") then
          cbuff = 
     1    'cmo/create/'//cmoname(1:icharlnf(cmoname))//
     2    '/ / /tri ; finish'
          call dotask(cbuff,ierror)
      else
C         Mesh type undefined, set as tet
          cbuff = 
     1    'cmo/create/'//cmoname(1:icharlnf(cmoname))//
     2    '/ / /tet ; finish'
          call dotask(cbuff,ierror)
      endif
      
      call cmo_set_info('nnodes',cmoname,nnodes_set,1,1,ierror)
      call cmo_set_info('nelements',cmoname,ntets_set,1,1,ierror)
      call cmo_newlen(cmoname,ierror)

C     Get Mesh Object attributes

      call cmo_get_info('imt1',cmoname,ipimt1,ilen,ityp,ier)
      call cmo_get_info('xic',cmoname, ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmoname, ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmoname, ipzic,ilen,ityp,ierror)
      call cmo_get_info('itetclr',cmoname,ipitetclr,ilen,ityp,ier)
      call cmo_get_info('itettyp',cmoname,ipitettyp,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmoname,ipitetoff,ilen,ityp,ier)
      call cmo_get_info('jtetoff',cmoname,ipjtetoff,ilen,ityp,ier)
      call cmo_get_info('itet',cmoname,ipitet,ilen,ityp,ier)
      call cmo_get_info('jtet',cmoname,ipjtet,ilen,ityp,ier)

C     Added attributes - this is from old version of code
C     TSurf may need block and id attributes, check examples

C     This is causing problems for general tsurf files
C     but needed for some types of tsurfs from gocad
C     iblock is assigned value of n_tface for each instance of TFACE

      if (mesh_type.eq."TSurf") then
        call dotask
     *  ('cmo/addatt/-def-/iblock/VINT/scalar/nelements;finish',ierror)
        call cmo_get_info('iblock',cmoname,
     *                             ipiblock,ilen,ityp,ier)
        call mmgetblk('id_node',isubname,ipid_node,nnodes_set,1,ier)
      endif

      n_vrtx = 0
      n_elem = 0
      n_tri = 0
      n_tet = 0
      n_tface = 0
      n_object = 1
      n_line_parse = 0
      n_offset_vrtx = 0


C     ******************************************************************
C     READ and PARSE All FILE LINES
C     remove hard-wired loop do i=1, 100000000

C     LOOP all lines 
C     This is a long set of cases based on first words each line
C     Switch on each first word to look for keyword 
      do
         
C        CHECK COUNTS to protect memory 
         if(n_vrtx .gt. nnodes_set)then
             write(logmess,'(a,i14,a,i14)')
     *       "Error: nodes expected: ",nnodes_set," read: ",n_vrtx
             call writloga('default',0,logmess,0,ics)
             ierror = ierror + 1
             goto 9999
         endif
         if(n_elem .gt. ntets_set)then
             write(logmess,'(a,i14,a,i14)')
     *       "Error: tets expected: ",ntets_set," read: ",n_elem
             call writloga('default',0,logmess,0,ics)
             ierror = ierror + 1
             goto 9999
         endif

C        READ A LINE

         input_msg = ' '
         read(iunit,'(a)', end=100) input_msg
         lenparse = len(input_msg)
         read_count = read_count+1

C        SKIP commented or bracket lines which may be very long
         if (input_msg(1:1).eq. "*" .or. 
     *       input_msg(1:1).eq. "#" .or.
     *       input_msg(1:1).eq. "}") then

             skip_count = skip_count+1
             cmsg(1) = "SKIP"
             nmsg = 0
Cdebug
C            print*,"Skip line: ",read_count
C            print*,"Skip line with length: ",lenparse


C        PARSE THE LINE
         else 
             call parse_string2(lenparse, input_msg,
     .                     imsg,msg,xmsg,cmsg,nmsg)
             n_line_parse = n_line_parse + 1

         endif

C     ******************************************************************
C     HEADER KEYWORDS 

C        If ZPOSITIVE is Elevation, this is normal z coordinates
C        If ZPOSITIVE is Depth, elevations are positive below 0, negative above
C        JewelSuite GOCAD Z coordinate is Depth and is left_hand system
C
C        Z coordinates and node order here are for JewelSuite GOCAD with Depth
C        Mult by Z and changing tet node order seems to put it into right-hand
C        node order for tets are also changed, need testing for non Jewelsuite


CCCCCCCCCCase Z Axis
         if (cmsg(1)(1:9).eq.'ZPOSITIVE')then
             if (cmsg(2)(1:5) .eq. "Depth") then
                 left_hand = .true.
                 z_type = "Z Depth"
             endif

C        KEYWORDS for node and element attributes, 
C        Add attributes after we know the type by reading data lines

CCCCCCCCCCase node properties 
         elseif (cmsg(1)(1:10).eq.'PROPERTIES')then
             if (if_nprops .eqv. .true.) then
                 print*,"PROPERTIES already set for nodes."
                 print*,cmsg(1)(1:10)," repeat ignored."
             else
                 if_nprops = .true.
                 input_nprops = input_msg
                 input_msg = ' '
             endif

CCCCCCCCCCase cell properties tet 
         elseif (cmsg(1)(1:16).eq.'TETRA_PROPERTIES')then
             if (if_tprops .eqv. .true.) then
                 print*,"TRGL_PROPERTIES already set for cells."
                 print*,cmsg(1)(1:16)," ignored."
             else
                 if_tprops = .true.
                 input_tprops = input_msg
                 input_msg = ' '
             endif

CCCCCCCCCCase cell properties tet 
         elseif (cmsg(1)(1:15).eq.'TRGL_PROPERTIES')then
             if (if_tprops .eqv. .true.) then
                 print*,"TETRA_PROPERTIES already set for cells."
                 print*,cmsg(1)(1:15)," ignored."
             else
                 if_tprops = .true.
                 input_tprops = input_msg
                 input_msg = ' '
             endif


C     ******************************************************************
C  Parse VRTX nodes and optional properties
C  syntax: VRTX ID X Y Z [PN ...] where PN are properties defined in the header

CCCCCCCCCCase cell properties 
        elseif (cmsg(1)(1:5) .eq. 'VRTX '
     *     .or. cmsg(1)(1:6).eq.'PVRTX ')then

C        -------------------------------------------------------
C        DO ONCE setup from first VRTX line
         if (n_vrtx .eq. 0) then

           istart = 6
           num_node_att = 0 

C          Use the first line of data to detirmine attribute type
C          and setup attributes with appropriate names
C          property values start at word 6 if they exist
C          VRTX ID X Y Z properties...

           write(logmess,'(a)')
     *     "..................................................." 
           call writloga('default',0,logmess,0,ier)

           write(logmess,'(a)')
     *     "SET VRTX properties: " 
           call writloga('default',0,logmess,0,ier)

           if (if_nprops) then
             call gocad_add_attributes(cmoname,
     *          input_nprops,msg,nmsg,n_attnames,num_node_att,ics)

C            if error, skip reading attributes but keep going
             if (ics .ne. 0) then
               num_node_att = 0
                write(logmess,'(a,i14)')
     * "Can not get node attribute from line: ",read_count 
               call writloga('default',0,logmess,0,ier)
               write(logmess,'(a)')
     *         "Warning: VRTX properties Skipped, can not get info."
               call writloga('default',0,logmess,0,ier)
             endif

           else
               ics = 0
               write(logmess,'(a)')
     *         "There are no VRTX properties." 
               call writloga('default',0,logmess,0,ier)
           endif

C          set pointers to the attributes to fill with data
           if (num_node_att .gt. 0) then
               call mmgetblk('n_datptr',isubname,
     *                    ipn_datptr,num_node_att,1,ics)
               if (ics.ne.0) print*,"mmgetblk n_datptr error",ics

               do i = 1, num_node_att
                   cmoatt = n_attnames(i)
                   call cmo_get_info(cmoatt(1:icharlnf(cmoatt)),
     *                  cmoname(1:icharlnf(cmoname)),
     *                  ipiatt,ilen,ityp,ier)

                   if (ipiatt.eq.0 .or. ier.ne.0) then
                       write(logmess,'(a,a,a,i5,i14)')
     *                 "Error: get data: ",
     *                  cmoatt(1:icharlnf(cmoatt)),
     *                 " flag, pointer: ", ier, ipiatt
                       call writloga('default',0,logmess,0,ics)
                       ierror = 2 
                       goto 9999
                   endif
                   n_datptr(i) = ipiatt
           enddo
           endif

           nwds_vrtx = istart+num_node_att-1

           write(logmess,'(a,i5)')
     *     "READ VRTX data with word count: ",nwds_vrtx
           call writloga('default',0,logmess,0,ier)
           write(logmess,'(a,i5)')
     *     "READ VRTX property with index:  ",num_node_att 
           call writloga('default',0,logmess,0,ier)
         endif
C        END first vrtx line, setup, and write once message
C        -------------------------------------------------------
C          parse data line and fill attributes

           if (nmsg .lt. nwds_vrtx) then
              write(logmess,'(a,i10)')
     * "Warning: missing VRTX values line: ",read_count
              call writloga('default',0,logmess,0,ier)
              write(logmess,'("Line ignored. ")')
              call writloga('default',0,logmess,0,ier)
              print*,"expected: ",nwds_vrtx," got ",nmsg
           else

             n_vrtx = n_vrtx + 1
             id_vrtx = imsg(2)
             id_vrtx_max = max(id_vrtx_max, id_vrtx)
           
C            the first 3 values are assumed to be xyz coordinates
             msg_num = 3
             if(msg(msg_num) .eq. if_real) then
                xic(n_vrtx) = xmsg(msg_num)
             elseif(msg(msg_num) .eq. if_integer) then
                xic(n_vrtx) = float(imsg(msg_num))
             endif
           
             msg_num = 4
             if(msg(msg_num) .eq. if_real) then
                yic(n_vrtx) = xmsg(msg_num)
             elseif(msg(msg_num) .eq. if_integer) then
                yic(n_vrtx) = float(imsg(msg_num))
             endif

             msg_num = 5
             if(msg(msg_num) .eq. if_real) then
                zic(n_vrtx) = xmsg(msg_num)
             elseif(msg(msg_num) .eq. if_integer) then
                zic(n_vrtx) = float(imsg(msg_num))
             endif
             if (left_hand) then
                zic(n_vrtx) = -1.0*(zic(n_vrtx))
             endif

C            loop through added property values for this node
             if (nmsg.gt.5 .and. num_node_att.gt.0) then

               ii = 1
               do i = 6, nmsg

                 if (ii .gt. num_node_att) then
                 write(logmess,'(a,i10)')
     *  "Warning: too many values line: ",read_count
                 call writloga('default',0,logmess,0,ier)
                 write(logmess,'("Extra values ignored. ")')
                 call writloga('default',0,logmess,0,ier)

                 else
        
                    call mmgettyp(n_datptr(ii),ityp,ics)

                    if (ics.ne.0) then
                      write(logmess,'(a,i12,i5)')
     *                "Problem property type line, word: ",
     *                n_line_parse, i 
                      call writloga('default',1,logmess,1,ier)
                    endif 

                    if (ityp.eq.1) then
                       ipivalues = n_datptr(ii)
                       if(msg(i) .eq. if_real) then
                         ivalues(n_vrtx) = int(xmsg(i))
                       elseif(msg(i) .eq. if_integer) then
                         ivalues(n_vrtx) = imsg(i)
                       endif
                    elseif (ityp.eq.2) then
                       ipxvalues = n_datptr(ii)
                       if(msg(i) .eq. if_real) then
                         xvalues(n_vrtx) = xmsg(i)
                       elseif(msg(i) .eq. if_integer) then
                         xvalues(n_vrtx) = float(imsg(i))
                       endif
                    endif

C DEBUG
c          if (n_vrtx.lt.5 .or.n_vrtx.gt.500) then
c             print*,n_vrtx," type ",ityp," word ",ii
c             print*,"pointers ",ipxvalues,ipivalues
c             print*,"...................................."
c          endif

                    ii = ii+1
                 endif
              enddo
           endif
           imt1(n_vrtx) = 1
           endif

C       end parse of nodes and properties first and remaining
C       -------------------------------------------------------
           
C
C        Parse elements TRGL or TETRA 
C        Parse TRGL for TSurf .ts mesh type 
C        Parse TETRA for TSolid .so mesh type
C        synatx: TETRA id1 id2 id3 id4 [PN ...] where PN are properties
C        Node order with normal pointing inward (AVS is outward)

CCCCCCCCCCase cell properties 
         elseif (cmsg(1)(1:5) .eq. 'TRGL ' .or. 
     *           cmsg(1)(1:6) .eq. 'TETRA ') then

C        ---------------------------------------------------------
C        DO ONCE setup with first cell line

         if (n_elem .eq. 0) then

          if (cmsg(1)(1:5) .eq. 'TRGL ') then
             istart = 5
          endif
          if (cmsg(1)(1:4) .eq. 'TET') then 
             istart = 6
          endif
          num_elem_att = 0

C         Use the first line of data to detirmine attribute type
C         and setup attributes with appropriate names
C         TETRA or TRGL Nid1 Nid2 Nid3 (Nid4) properties... 

           write(logmess,'(a)')
     *     "..................................................."
           call writloga('default',0,logmess,0,ier)

           write(logmess,'(a)')
     *     "SET CELL properties: "
           call writloga('default',0,logmess,0,ier)

          if (if_tprops) then
            call gocad_add_attributes(cmoname,
     *         input_tprops,msg,nmsg,e_attnames,num_elem_att,ics)

C            if error, skip reading attributes but keep going
             if (ics .ne. 0) then
               num_node_att = 0
                write(logmess,'(a,i14)')
     *  "Can not get cell attribute from line: ",read_count 
               call writloga('default',0,logmess,0,ier)
               write(logmess,'(a)')
     *         "Warning: CELL properties Skipped, can not get info."
               call writloga('default',0,logmess,0,ier)
             endif

          else
               ics = 0
               write(logmess,'(a)')
     *         "There are no Cell properties." 
               call writloga('default',0,logmess,0,ier)
          endif

C         set pointers to the attributes to fill with data
          if (num_elem_att .gt. 0) then

              call mmgetblk('e_datptr',isubname,
     *                    ipe_datptr,num_elem_att,1,ics)
              if (ics.ne.0) print*,"mmgetblk e_datptr error",ics

              do i = 1, num_elem_att 
                  cmoatt = e_attnames(i)
                  call cmo_get_info(cmoatt(1:icharlnf(cmoatt)),
     *                 cmoname(1:icharlnf(cmoname)),
     *                 ipiatt,ilen,ityp,ier)

                  if (ipiatt.eq.0 .or. ier.ne.0) then
                      write(logmess,'(a,a,a,i5,i14)')
     *                "Error: get data: ",
     *                 cmoatt(1:icharlnf(cmoatt)),
     *                " flag, pointer: ", ier, ipiatt
                      call writloga('default',0,logmess,0,ics)
                      ierror = 2
                      goto 9999
                   endif
                   e_datptr(i) = ipiatt
              enddo
           endif

           nwds_tet = istart+ num_elem_att - 1 

           write(logmess,'(a,i5)')
     *     "READ CELL data with word count: ",nwds_tet
           call writloga('default',0,logmess,0,ier)
           write(logmess,'(a,i5)')
     *     "READ CELL property with index:  ",num_elem_att
           call writloga('default',0,logmess,0,ier)


         endif
         n_elem = n_elem + 1
C        ---------------------------------------------------------
C        END first cell line, setup, and write once message

C          now parse each line and fill element attributes

           if (nmsg .lt. nwds_tet) then
              write(logmess,'(a,i10)')
     * "Warning: missing VRTX values line: ",read_count
              call writloga('default',0,logmess,0,ier)
              write(logmess,'("Line ignored. ")')
              call writloga('default',0,logmess,0,ier)
           else


C            READ and set TRGL element
             if (elem_typ .eq. ifelmtri) then
               if (nmsg .lt. 4 ) then
                  write(logmess,'(a,i10)')
     * "Warning: missing TRGL values line: ",read_count
                  call writloga('default',0,logmess,0,ier)
                  write(logmess,'("Line ignored. ")')
                  call writloga('default',0,logmess,0,ier)
               else
                 n_tri = n_tri + 1
                 i_tri_off = 3*(n_tri-1)
                 itetclr(n_tri) = n_object
                 iblock(n_tri)  = n_tface
                 itettyp(n_tri) = ifelmtri
                 itetoff(n_tri) = i_tri_off
                 jtetoff(n_tri) = i_tri_off
                 itet(i_tri_off + 1) = imsg(2) + n_offset_vrtx
                 itet(i_tri_off + 2) = imsg(3) + n_offset_vrtx
                 itet(i_tri_off + 3) = imsg(4) + n_offset_vrtx
                 jtet(i_tri_off + 1) = -1
                 jtet(i_tri_off + 2) = -1
                 jtet(i_tri_off + 3) = -1 
               endif
         
C            READ and set TETRA element
             else if (elem_typ .eq. ifelmtet) then
               if (nmsg .lt. 5 .and. num_elem_att .gt. 0) then
                  write(logmess,'(a,i10)')
     * "Warning: missing TETRA values line: ",read_count
                  call writloga('default',0,logmess,0,ier)
                  write(logmess,'("Line ignored. ")')
                  call writloga('default',0,logmess,0,ier)
               else
                 n_tet = n_tet + 1
                 i_tet_off = 4*(n_tet-1)
                 itetclr(n_tet) = n_object 
                 itettyp(n_tet) = ifelmtet
                 itetoff(n_tet) = i_tet_off
                 jtetoff(n_tet) = i_tet_off
                 jtet(i_tet_off + 1) = -1
                 jtet(i_tet_off + 2) = -1
                 jtet(i_tet_off + 3) = -1
                 jtet(i_tet_off + 4) = -1
                 if (left_hand) then
                   itet(i_tet_off + 1) = imsg(2) + n_offset_vrtx
                   itet(i_tet_off + 3) = imsg(3) + n_offset_vrtx
                   itet(i_tet_off + 4) = imsg(4) + n_offset_vrtx
                   itet(i_tet_off + 2) = imsg(5) + n_offset_vrtx
                 else
                   itet(i_tet_off + 1) = imsg(2) + n_offset_vrtx
                   itet(i_tet_off + 2) = imsg(3) + n_offset_vrtx
                   itet(i_tet_off + 3) = imsg(4) + n_offset_vrtx
                   itet(i_tet_off + 4) = imsg(5) + n_offset_vrtx
                 endif
               endif
           endif
C          done reading nodes for the element

C          loop through attribute values for this element
C          istart = index after node list 
c          TRGL has 4 so istart is 5, TETRA has 5 so istart is 6
c          current code supports mix tri and tet
c          but results not guaranteed use n_tet and n_tri
c          here we write atts for each elem of either type
c          index n_elem instead of n_tet but protect if not set


           if (nmsg .ge. istart .and. num_elem_att.gt.0) then
              ii = 1
              do i = istart, nmsg

                 if (ii .gt. num_elem_att) then
                   write(logmess,'(a,i10)')
     * "Warning: too many values line: ",read_count
                   call writloga('default',0,logmess,0,ier)
                   write(logmess,'("Extra values ignored. ")')
                   call writloga('default',0,logmess,0,ier)

                 elseif (n_elem .le. 0) then 

                   write(logmess,'(a,i10)')
     *            "Warning: n_elem not set: ",n_elem
                   call writloga('default',0,logmess,0,ier)
                   write(logmess,'("Extra values ignored. ")')
                   call writloga('default',0,logmess,0,ier)

                 else

                    call mmgettyp(e_datptr(ii),ityp,ics)

                    if (ics.ne.0) then
                      write(logmess,'(a,i12,i5)')
     *                "Problem property type line, word: ",
     *                n_line_parse, i
                      call writloga('default',1,logmess,1,ier)
                    endif

C debug
C           print*,"loop i and ii: ",i,ii
C           print*,"n_tri: ",n_tri
C           print*,"n_tet: ",n_tet
C           print*,"n_elem: ",n_elem

                    if (ityp.eq.1) then
                       ipivalues = e_datptr(ii)
                       if(msg(i) .eq. if_real) then
                         ivalues(n_elem) = int(xmsg(i))
                       elseif(msg(i) .eq. if_integer) then
                         ivalues(n_elem) = imsg(i)
                       endif
                    elseif (ityp.eq.2) then
                       ipxvalues = e_datptr(ii)
                       if(msg(i) .eq. if_real) then
                         xvalues(n_elem) = xmsg(i)
                       elseif(msg(i) .eq. if_integer) then
                         xvalues(n_elem) = float(imsg(i))
                       endif
                    endif

                    ii = ii+1
                 endif

              enddo

           endif

           endif
C          end reading element properties
C          print*,"END cells ------------"

C
C  Parse TFACE
C
CCCCCCCCCCase tface, count instance 
         elseif (cmsg(1)(1:6) .eq. 'TFACE ')then
           n_tface = n_tface + 1
C
C  Parse END
C
CCCCCCCCCCase end of file or new object 
         elseif((cmsg(1)(1:3) .eq. 'END').and.
     1          (cmsg(1)(1:4) .ne. 'END_'))then

           nEND = nEND + 1
           if (read_count .lt. file_length) then
               n_object = n_object + 1
               n_offset_vrtx = n_vrtx
           endif
  
C
C  Parse ATOM - note we do not have an example to test this option
C
CCCCCCCCCCase atom  
          elseif(cmsg(1)(1:4).eq.'ATOM'.or.cmsg(1)(1:5).eq.'PATOM')then
           n_vrtx = n_vrtx + 1
           id_vrtx = imsg(2)
           xic(n_vrtx) = xic(imsg(3))
           yic(n_vrtx) = yic(imsg(3))
           zic(n_vrtx) = zic(imsg(3))
C
C  Parse other
C  The pound sign character indicates comment to skip
C
CCCCCCCCCCase atom  
         else
c
c          ignore first word not implemented or not needed
C          print*,"unknown keyword: ",cmsg(1)
c
         endif

C     END loop lines
      enddo
  100 continue

Cdebug

C     Set mesh info	


      if (n_vrtx .gt. 0) then
         call cmo_set_info('nnodes',cmoname,n_vrtx,1,1,ierror)
      endif

      if (n_tet .gt. 0) then
         call cmo_set_info('nelements',cmoname,n_tet,1,1,ierror)
      else if (n_tri .gt. 0) then
         call cmo_set_info('nelements',cmoname,n_tri,1,1,ierror)
      endif



      call cmo_newlen(cmoname,ierror)
      call dotask('geniee ; finish',ierror)
      if (ierror .ne. 0) then
      write(logmess,'("Warning: geniee connectivity issues.")')
          call writloga('default',0,logmess,1,ier)
          ierror = 0
      endif

 9999 continue

      call mmrelprt(isubname,ics)      

C     Report results
C     GOCAD formats vary greatly 
C     report may help with possible issues

      write(logmess,'("--- READ GOCAD FINISHED -------- ")')
      call writloga('default',0,logmess,0,ier)

      write(logmess,'(" Mesh Type:    ",a6)') mesh_type
      call writloga('default',0,logmess,0,ier)

      write(logmess,'(" ZPOSITIVE:    ",a7)') z_type
      call writloga('default',0,logmess,0,ier)

      write(logmess,'(" Nodes:       ",i10)')n_vrtx
      call writloga('default',0,logmess,0,ier)

      if (n_tri .gt. 0) then
      write(logmess,'(" Triangles:   ",i10)')n_tri
      call writloga('default',0,logmess,0,ier)
      endif

      if (n_tet .gt. 0) then
      write(logmess,'(" Tets:        ",i10)')n_tet
      call writloga('default',0,logmess,0,ier)
      endif

      if (n_elem .gt. 0) then
      write(logmess,'(" Cells:       ",i10)')n_elem
      call writloga('default',0,logmess,0,ier)
      endif

      if (n_tface .gt. 1) then
      write(logmess,'(" TFACE:       ",i10)')n_tface
      call writloga('default',0,logmess,0,ier)
      endif

      if (n_object .gt. 1) then
      write(logmess,'(" OBJECTS:     ",i10)')n_object
      call writloga('default',0,logmess,0,ier)
      endif

      if (num_node_att .gt. 0) then
      write(logmess,'(" Node properties:  ",i5)')num_node_att
      call writloga('default',0,logmess,0,ier)
      endif

      if (num_elem_att .gt. 0) then
      write(logmess,'(" Cell properties:  ",i5)')num_elem_att
      call writloga('default',0,logmess,0,ier)
      endif


      if (n_vrtx .ne. nnodes_set) then
         write(logmess,'(a,i10)')
     *   " Warning: Total nodes expected: ",nnodes_set
         call writloga('default',0,logmess,0,ier)
      endif
      if (n_elem.gt.0 .and. n_elem .ne. ntets_set ) then
         write(logmess,'(a,i10)')
     *   " Warning: Total tets expected: ",ntets_set
         call writloga('default',0,logmess,0,ier)
      endif
      if (n_tri.gt.0 .and. n_tri .ne. ntets_set ) then
         write(logmess,'(a,i10)')
     *   " Warning: Total triangles expected: ",ntets_set
         call writloga('default',0,logmess,0,ier)
      endif

      if (file_length .ne. read_count) then
      write(logmess,'(" LINES expected: ",i14)')file_length
      call writloga('default',0,logmess,0,ier)
      endif

      write(logmess,'(" LINES read:     ",i14)')read_count
      call writloga('default',0,logmess,1,ier)

      if (ierror .ne. 0) then
         write(logmess,'("ERROR READ GOCAD: ",i5)')ierror
         call writloga('default',0,logmess,1,ier)
      endif

C     Display cmo status
      call dotask('cmo/status/brief ; finish',ierror)

C     END read_gocad_tsurf
      return
      end
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine gocad_add_attributes(cmoname,
     *  prop_line,msg,nmsg,attnames,natt,ierror)

C     use the property line from the GOCAD file to add attributes
C     these can be either node or element attributes
C     type is detirmined from the values on the first line of data
C     INPUT
C       cmoname
C       prop_line is the saved line with property names for attributes
C       msgtype is the parsed data line with nmsg types 
C     RETURN
C       attnames is array of attribute names 
C       natt are the number of attributes
C       ierror returns 0 if no errors
C     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none

C     arguments
      character*32 cmoname
      character*4096 prop_line
      character*32 attnames(128)
      integer nmsg
      integer msg(nmsg)
      integer natt,ierror

C     local variables

      pointer (ipdatptr,datptr)
      integer datptr(100)

      integer i, ii, j, jj, ilen, ityp, ier, ics, istart,  
     *  nprops, len_parse, length, lenline, lenparse, nwds2

      integer icharlnb, icharlnf

      character*6  attword
      character*32 cmotype, keyword, attlen
      character*32 isubname, cmoatt
      character*512 cmsgbig(512)
      character*512 logmess
      character*1024 cbuff

C     -------------------------------------------------------
C     Use the first line of data to detirmine attribute type
C     and setup attributes with appropriate names

      cmotype = " "
      cmoatt = " "
      attlen = " "
      attword = " "
      cbuff = " "
      isubname = "read_gocad"
      istart = 0
      ierror = 0
     
C     PARSE the saved PROPERTY line
C     Check for property names longer than 32 characters

C     get the array length for this line
      lenline = len(prop_line)


C debug
C     print*,"PARSE PROPERTY LINE ========================"
C     print*,"SHOW PROPERTY LINE(1:120): ", prop_line(1:120)
C     print*,"prop line len: ",lenline

C     Use first word to get attribute length  

      ilen = icharlnf(prop_line)
      keyword = prop_line(1:ilen)

      if (keyword .eq. "TETRA_PROPERTIES") then
          attlen = "nelements"
          attword = "_tetra"
          istart = 6
      else if (keyword .eq. "TRGL_PROPERTIES") then 
          attlen = "nelements"
          attword = "_trigl"
          istart = 5
      else if (keyword .eq. "PROPERTIES") then 
          attlen = "nnodes"
          attword = "_pvrtx"
          istart = 6
      else
          print*,"ERROR reading PROPERTY line."
          print*,"KEYWORD: ",prop_line(1:ilen)
          ierror = 1
          goto 9998
      endif
      nprops = nmsg-istart+1 

C     icharlnb seems to give the correct line length of line read 
C     the function len just returns the length of the declared array 
C     count characters from back of line array 
      lenparse = icharlnb(prop_line)
      
C     skip spaces and tabs
      j=1
      do while (prop_line(j:j).eq.' ' .or.
     *  prop_line(j:j).eq.achar(9) )
        j=j+1
      enddo

      nwds2=1
      jj=j

C     LOOP over the property line
 10   i= 0
      j= jj 
      
C debug
C     print*,"\\\\\\\\loop over line"

      cmsgbig(nwds2)(1:1) = ''

      do while (prop_line(j:j).ne.' '
     *    .and. prop_line(j:j).ne.','
     *    .and. prop_line(j:j).ne.char(0) 
     *    .and. j.le. lenparse)
          i=i+1
          cmsgbig(nwds2)(i:i)=prop_line(j:j)
c         print*,"i= ",i," j= ",j,"ch= ",prop_line(j:j)
          j=j+1
          if(j.gt.lenparse) go to 20
      enddo
      nwds2 = nwds2+1
      j=j+1
      jj = j

      if(j.lt.lenparse) go to 10
 20   cmsgbig(nwds2)(i+1:i+1) = ''

c     print*,"end loop over line //////////"


C debug write results of the parsed line
C     do i = 1, nwds2
C     print*,"cmsgbig ",i, cmsgbig(i)
C     enddo
C     print*,"End prop_line parse with words: ",nwds2

C     number of attributes = number of words minus keyword
      natt = nwds2-1 
      if (natt .ne. nprops) then
          print*,"ERROR: nprops ne natt ",nprops, natt
          print*,"Check for extra spaces or words at line:"
          print*, cmsgbig(1:icharlnf(cmsgbig(1)))
          print*," "
          ierror = 1
          natt = 0
          goto 9998
      endif

C     LOOP over each property name and create attributes
      do i = 2, nwds2

C          If property name is gt than 32 characters 
C          create a name using property_ plus count number
 
           length = icharlnf(cmsgbig(i))
           cmoatt = ' '
c          print*,"cmsbig i :",i, cmsgbig

           if (length .gt. 32 ) then
               print*,"Property name too long: ",cmsgbig(i)(1:length),i
               print*,"generic name will be created."
               if (i .lt. 10) then
                   write(cmoatt,'(a5,i1,a6)') "prop_",i-1,attword
               else
                   write(cmoatt,'(a5,i2,a6)') "prop_",i-1,attword
               endif
            else
               cmoatt=cmsgbig(i)(1:length)
            endif 

C           Now get the attribute type
            if (istart .gt. nmsg .or. istart.le.0 ) then
                print*,"ERROR reading data line."
                print*,"properties will not be read."
                ierror = 1
                natt = 0
                goto 9998
            endif

C           make default double
            cmotype = "VDOUBLE" 
            if( msg(istart) .eq. 1) then
                cmotype = " "
                cmotype = "VINT" 
            endif

c           print*,"ADD PROPERTY: ",cmoatt,i
                
            cbuff='cmo/addatt/' //
     *         cmoname(1:icharlnf(cmoname)) //
     *         '/' //
     *         cmoatt(1:icharlnf(cmoatt)) //
     *         '/' //
     *         cmotype(1:icharlnf(cmotype)) // ' scalar ' //
     *         '/' //
     *         attlen(1:icharlnf(attlen)) //
     *         '/' //
     *         ' linear/permanent/gxaf/0.0' //
     *         '/' //
     *         ' ; finish '

            call dotaskx3d(cbuff,ics)
            if (ics .ne.0) then
                print*,"Warning ics return non-zero. ",ics
            endif

            attnames(i-1) = cmoatt(1:icharlnf(cmoatt)) 

c           istart = istart+1
c           print*,"subroutine add attribute: ",cmoatt
       enddo


 9998 continue

C     clean the buffer after use
      
       length=len(cmsgbig)
       do i = 1, length
          cmsgbig(i) = " "
       enddo

      return 
      end
