      subroutine cmo_get_num_io_att
     *   (cmo, cflag, nvalues_node, nvalues_elem)
C
C #####################################################################
C
C     PURPOSE -
C
C     Returns the number of node/element attributes with the specified
C     cflag value. 
C
C     Input:
C     cmo = mesh object name, character variable
C     cflag = flag value, single character variable
C     Options of cflag are:
C     a = AVS
C     g = GMV
C     l = LaGriT
C     x = X3D
C     t = TecPlot
C     n = netCDF
C
C    Output:
C    nvalues_node = number of node attributres with ioflag = cflag
C    nvalues_elem = number of element attributres with ioflag = cflag
C
C ######################################################################
C
      implicit none
C
      character*32 cmo
C
      integer itype_flag(*)
      pointer (ipitype_flag,itype_flag)
      integer  nmcmoatt
      pointer (ipoffs,ioffs)
      pointer (ipoffse,ioffse)
      pointer (ipidx,idx)
      pointer (ipidxe,idxe)
      pointer (ipcnames,cnames)
      pointer (ipranks,iranks)
      pointer (iplengths,clengths)
      pointer (ipioflags,cioflags)
      character*32 cnames(*),clengths(*),cioflags(*)
      integer iranks(*),ioffs(*),ioffse(*),idx(*),idxe(*)
C
      character*1  cflag
C
      character*32 isubname
      character*32  ctype , cinterp, crank,cpers
      character*132 logmess
      integer icharlnf
      integer  ierr, ilen, ityp, nvalues_node,
     1   nvalues_elem, lvalues, lvaluese, irowlen, irowlene, index
      integer i, j, len1, iflag
C
C ######################################################################
C
      isubname="parse_attributes"
C
C     Confirm that 'cflag' is a single character.
      if(icharlnf(cflag) .ne. 1)then
        write(logmess,"(a)")
     1  'ERROR cmo_get_num_io_att: cflag should be single character '
        call writloga('default',1,logmess,0,ierr)

      return
      endif
C     ******************************************************************
C     Count the number of fields that have the specified 'cflag' value
C
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   ierr)
C
      nvalues_node=0
      nvalues_elem=0
      lvalues=0
      lvaluese=0
      irowlen=0
      irowlene=0
c     Hardwire turning off output of -def- field.
c
      call dotaskx3d
     1 ('cmo/modatt/-def-/-def-/ioflag/x;finish',ierr)
      call mmgetblk('cnames',  isubname,ipcnames,nmcmoatt*8,1,ierr)
      call mmgetblk('clengths',isubname,iplengths,nmcmoatt*8,1,ierr)
      call mmgetblk('iranks',  isubname,ipranks,nmcmoatt,1,ierr)
      call mmgetblk('cioflags',isubname,ipioflags,nmcmoatt*8,1,ierr)
      call mmgetblk
     1   ('itype_flag',isubname,ipitype_flag,nmcmoatt,1,ierr)
      call mmgetblk('ioffs',isubname,ipoffs,nmcmoatt,1,ierr)
      call mmgetblk('ioffse',isubname,ipoffse,nmcmoatt,1,ierr)
      call mmgetblk('idx',isubname,ipidx,nmcmoatt,1,ierr)
      call mmgetblk('idxe',isubname,ipidxe,nmcmoatt,1,ierr)
C
c
      ioffs(1)=0
      ioffse(1)=0
c
      do i=1,nmcmoatt
C
C....    NAME Field.
C
         call cmo_get_attribute_name(cmo,i,cnames(i),ierr)
         call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *    clengths(i),cinterp,cpers,cioflags(i),ierr)
         call cmo_get_info(crank,cmo,iranks(i),ilen,ityp,ierr)
C
         len1=icharlnf(cioflags(i))
         iflag=0
         ioffs(1)=0
         do j=1,len1
            if(cioflags(i)(j:j).eq.cflag .and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
               iflag=1
               nvalues_node=nvalues_node+1
               idx(nvalues_node)=i
               lvalues=lvalues+iranks(i)
               ioffs(i)=irowlen
               irowlen=irowlen+iranks(i)
c               print *, 'node attribute ', 
c     *         i, cnames(i)(1:icharlnf(cnames(i))),' ',
c     *         cioflags(i)(1:len1), ityp, ilen
            endif
         enddo
         do j=1,len1
            if(cioflags(i)(j:j).eq.cflag .and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nelements') then
               iflag=1
               nvalues_elem=nvalues_elem+1
               idxe(nvalues_elem)=i
               lvaluese=lvaluese+iranks(i)
               ioffse(i)=irowlene
               irowlene=irowlene+iranks(i)
c               print *, 'elem attribute ', 
c     *         i, cnames(i)(1:icharlnf(cnames(i))),' ',
c     *         cioflags(i)(1:len1), ityp, ilen
            endif
         enddo
      enddo
      call mmrelprt(isubname, ierr)
C
      return
      end
      
