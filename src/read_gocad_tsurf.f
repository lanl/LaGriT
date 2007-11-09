      subroutine read_gocad_tsurf
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C#######################################################################
C
C    FORMAT - read / gocad / file_name.ts / mo_name
C
C    INPUT ARGUMENTS - imsgin,xmsgin,cmsgin,msgtyp,nwds
C
C    OUTPUT - Triangle mesh object
C
C        $Log:   /pvcs.config/t3d/src/read_gocad_tsurf.f_a  $
CPVCS    
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
C
C     Input variables
C
      integer nwds, imsgin(nwds), msgtyp(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      pointer (ipimt, imt)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      integer imt(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000)
      integer itet(10000000), jtet(10000000)
      real*8   xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      pointer (ipiblock, iblock)
      integer iblock(1000000)
      pointer (ipid_node, id_node)
      integer id_node(1000000)

c      
c local parser variables 
      character*250 input_msg 
      integer lenparse
      integer msg(128)
      real*8 xmsg(128)
      integer imsg(128)
      character*32 cmsg(128)
C
      character*132 ifile
      character*8 itype
      character*32 isubname, cmoname, cmoatt, cmotype
      character*132 cline, cbuff,ctype
C
C     Local variables
      integer if_integer, if_real, if_character, nnodes_set,
     1        nelements_set, ierror, ilen, itp, ier, n_vrtx,
     2        n_trgl, n_tface, n_object, iunit, icharlnf, i,
     3        msg_num, i_tri_off, n_offset_vrtx, n_line_parse,
     4        id_vrtx, id_vrtx_max, ninc
      character*512 logmess
C
C     ******************************************************************
C
C
C  The access to user written subroutines is through the LaGriT subroutine, user_sub. It is passed the parsed command input line. The parser breaks up the input line into tokens and returns to LaGriT a count of number of tokens, an array containing the token types, and the tokens themselves. The parameters returned by the parser are:
C  nwds ( number of tokens)
C  msgtyp (integer array of token types : 1 for integer, 2 for real, 3 for character, msgtyp(nwds+1) = -1)
C  imsgin (array of integer tokens, e.g. if msgtyp(i)=1 then the ith token is type integer and imsgin(i) contains its value)
C  xmsgin (array of real tokens, e.g. if msgtyp(i)=2 then the ith token is type real and xmsgin(i) contains its value)
C  cmsgin (array of character tokens, e.g. if msgtyp(i)=3 then the ith token is type character and cmsgin(i) contains its value)
C
      if_integer = 1
      if_real = 2
      if_character = 3
      
      ifile = cmsgin(3)
      cmoname = cmsgin(4)
      
      nnodes_set = 2000
      nelements_set = 2000
      
      cbuff = 
     1 'cmo/create/'//cmoname(1:icharlnf(cmoname))//
     2 '/2000/2000/tri;finish'
      call dotask(cbuff,ierror)
      
      call cmo_set_info
     1       ('nnodes',cmoname,nnodes_set,1,1,ierror)
      call cmo_set_info
     1       ('nelements',cmoname,nelements_set,1,1,ierror)
      cbuff = 'cmo/setatt//ipolydat/no ; finish '    
      call dotask(cbuff,ierror)
      call dotask
     * ('cmo/addatt/-def-/iblock/VINT/scalar/nelements;finish',ierror)
                  call cmo_get_info('imt',cmoname,
     *                              ipimt,ilen,itp,ier)
                  call cmo_get_info('itp1',cmoname,
     *                              ipitp1,ilen,itp,ier)
                  call cmo_get_info('icr1',cmoname,
     *                              ipicr1,ilen,itp,ier)
                  call cmo_get_info('xic',cmoname,
     *                              ipxic,ilen,itp,ierror)
                  call cmo_get_info('yic',cmoname,
     *                              ipyic,ilen,itp,ierror)
                  call cmo_get_info('zic',cmoname,
     *                              ipzic,ilen,itp,ierror)
                  call cmo_get_info('itetclr',cmoname,
     *                                    ipitetclr,ilen,itp,ier)
                  call cmo_get_info('iblock',cmoname,
     *                                    ipiblock,ilen,itp,ier)
                  call cmo_get_info('itettyp',cmoname,
     *                                    ipitettyp,ilen,itp,ier)
                  call cmo_get_info('itetoff',cmoname,
     *                                    ipitetoff,ilen,itp,ier)
                  call cmo_get_info('jtetoff',cmoname,
     *                                    ipjtetoff,ilen,itp,ier)
                  call cmo_get_info('itet',cmoname,
     *                                    ipitet,ilen,itp,ier)
                  call cmo_get_info('jtet',cmoname,
     *                                    ipjtet,ilen,itp,ier)
C
      call mmgetblk('id_node',isubname,ipid_node,nnodes_set,1,ier)
      
      n_vrtx = 0
      n_trgl = 0
      n_tface = 0
      n_object = 1
      n_line_parse = 0
      n_offset_vrtx = 0
C
C
C     ASSIGN THE FILE TO THE NEXT AVAILABLE LOGICAL UNIT NUMBER.
C
      iunit=-1
      ifile = ifile(1:icharlnf(ifile))
      call hassign(iunit,ifile,ierror)
C
c      call dotask('cmo/status; finish',ierror)
      do i=1, 100000000
         
         if(n_vrtx+10 .gt. nnodes_set)then
C
C        Allocate more memory for nodes
C
           nnodes_set = 2*nnodes_set
           call cmo_set_info
     1       ('nnodes',cmoname,nnodes_set,1,1,ierror)
           call cmo_newlen(cmoname,ierror)
                  call cmo_get_info('imt',cmoname,
     *                              ipimt,ilen,itp,ier)
                  call cmo_get_info('itp1',cmoname,
     *                              ipitp1,ilen,itp,ier)
                  call cmo_get_info('icr1',cmoname,
     *                              ipicr1,ilen,itp,ier)
                  call cmo_get_info('xic',cmoname,
     *                              ipxic,ilen,itp,ierror)
                  call cmo_get_info('yic',cmoname,
     *                              ipyic,ilen,itp,ierror)
                  call cmo_get_info('zic',cmoname,
     *                              ipzic,ilen,itp,ierror)
                  call cmo_get_info('itetclr',cmoname,
     *                                    ipitetclr,ilen,itp,ier)
                  call cmo_get_info('iblock',cmoname,
     *                                    ipiblock,ilen,itp,ier)
                  call cmo_get_info('itettyp',cmoname,
     *                                    ipitettyp,ilen,itp,ier)
                  call cmo_get_info('itetoff',cmoname,
     *                                    ipitetoff,ilen,itp,ier)
                  call cmo_get_info('jtetoff',cmoname,
     *                                    ipjtetoff,ilen,itp,ier)
                  call cmo_get_info('itet',cmoname,
     *                                    ipitet,ilen,itp,ier)
                  call cmo_get_info('jtet',cmoname,
     *                                    ipjtet,ilen,itp,ier)
            ninc=nnodes_set/2
            call mmincblk('id_node',isubname,ipid_node,ninc,ier)
         endif
         if(n_trgl+10 .gt. nelements_set)then
C
C        Allocate more memory for elements
C
           nelements_set = 2*nelements_set
           call cmo_set_info
     1       ('nelements',cmoname,nelements_set,1,1,ierror)
           call cmo_newlen(cmoname,ierror)
                  call cmo_get_info('imt',cmoname,
     *                              ipimt,ilen,itp,ier)
                  call cmo_get_info('itp1',cmoname,
     *                              ipitp1,ilen,itp,ier)
                  call cmo_get_info('icr1',cmoname,
     *                              ipicr1,ilen,itp,ier)
                  call cmo_get_info('xic',cmoname,
     *                              ipxic,ilen,itp,ierror)
                  call cmo_get_info('yic',cmoname,
     *                              ipyic,ilen,itp,ierror)
                  call cmo_get_info('zic',cmoname,
     *                              ipzic,ilen,itp,ierror)
                  call cmo_get_info('itetclr',cmoname,
     *                                    ipitetclr,ilen,itp,ier)
                  call cmo_get_info('iblock',cmoname,
     *                                    ipiblock,ilen,itp,ier)
                  call cmo_get_info('itettyp',cmoname,
     *                                    ipitettyp,ilen,itp,ier)
                  call cmo_get_info('itetoff',cmoname,
     *                                    ipitetoff,ilen,itp,ier)
                  call cmo_get_info('jtetoff',cmoname,
     *                                    ipjtetoff,ilen,itp,ier)
                  call cmo_get_info('itet',cmoname,
     *                                    ipitet,ilen,itp,ier)
                  call cmo_get_info('jtet',cmoname,
     *                                    ipjtet,ilen,itp,ier)
         endif
         read(iunit,'(a)', end=100) input_msg
         lenparse = len(input_msg)
         
         call parse_string2(lenparse, input_msg,
     .                     imsg,msg,xmsg,cmsg,nwds)

         n_line_parse = n_line_parse + 1
C
C  Parse VRTX
C
         if (cmsg(1)(1:4) .eq. 'VRTX' .or. cmsg(1)(1:5).eq.'PVRTX')then
           n_vrtx = n_vrtx + 1
           
           id_vrtx = imsg(2)
           id_node(n_vrtx) = id_vrtx
           
           id_vrtx_max = max(id_vrtx_max, id_vrtx)
           
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
           
           imt(n_vrtx) = n_tface
           
C
C  Parse TRGL
C
         elseif (cmsg(1)(1:4) .eq. 'TRGL')then
           n_trgl = n_trgl + 1
           i_tri_off = 3*(n_trgl-1)
           itetclr(n_trgl) = n_object
           iblock(n_trgl)  = n_tface
           itettyp(n_trgl) = 3
           itetoff(n_trgl) = i_tri_off
           jtetoff(n_trgl) = i_tri_off
           itet(i_tri_off + 1) = imsg(2) + n_offset_vrtx
           itet(i_tri_off + 2) = imsg(3) + n_offset_vrtx
           itet(i_tri_off + 3) = imsg(4) + n_offset_vrtx
           jtet(i_tri_off + 1) = -1
           jtet(i_tri_off + 2) = -1
           jtet(i_tri_off + 3) = -1 
C
C  Parse TFACE
C
         elseif (cmsg(1)(1:5) .eq. 'TFACE')then
           n_tface = n_tface + 1
C
C  Parse END
C
         elseif((cmsg(1)(1:3) .eq. 'END').and.
     1          (cmsg(1)(1:4) .ne. 'END_'))then
           n_object = n_object + 1
           n_offset_vrtx = n_vrtx
C
C  Parse END
C
         elseif(cmsg(1)(1:4).eq.'ATOM'.or.cmsg(1)(1:5).eq.'PATOM')then
           n_vrtx = n_vrtx + 1
           id_vrtx = imsg(2)
           id_node(n_vrtx) = id_vrtx
           xic(n_vrtx) = xic(imsg(3))
           yic(n_vrtx) = yic(imsg(3))
           zic(n_vrtx) = zic(imsg(3))
C
C  Parse other
C
         else
c
c          keyword not implemented
c          do nothing with the data
c
         endif
      enddo
  100 continue
  
      write(logmess,'("---READ GOCAD TSURF--------        ")')
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(" # Nodes           ",i10)')n_vrtx
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(" # Triangles       ",i10)')n_trgl
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(" # TFACE           ",i10)')n_tface
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(" # OBJECTS         ",i10)')n_object
      call writloga('default',0,logmess,0,ier)

      call cmo_set_info('nnodes',cmoname,n_vrtx,1,1,ierror)
      call cmo_set_info('nelements',cmoname,n_trgl,1,1,ierror)
      call cmo_newlen(cmoname,ierror)
      call dotask('geniee; finish',ierror)
      call mmrelprt(isubname,ierror)      
      return
      end
      
