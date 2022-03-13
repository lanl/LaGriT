      subroutine dumpgeofest(ifile,cmo,
     *                   nsd,nen,nef,
     *                   nnodes,nelements,mbndry,
     *                   ihcycle,time,dthydro,
     *                   iopt_points,iopt_elements,
     *                   iopt_values_node,iopt_values_elem)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR GEOFEST.
C        http://www.openchannelsoftware.org/projects/GeoFEST
C        Converted from AVS output module.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: dumpgeofest.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   25 Jul 2005 11:09:20   gable
CPVCS    Initial
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
      integer nsd, nen, nef
      integer imt1(*), itp1(*), icr1(*), isn1(*)
      real*8 xic(*), yic(*), zic(*)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipisetwd, isetwd)
      integer isetwd(*)
      integer itet(*)
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
     *  iopt_values_node,iopt_values_elem,
     *  mbndry,nelements,nnodes,ihcycle,nvaluese,lenval,lvalues,
     *  lvaluese,izero,k,irowlen,irowlene, ncolumn_max
      integer nnodes_io,nelements_io,nvalues_node,nvalues_elem
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data cutoff/1.e-30/
      data maxval/1.e+30/
      data izero/0/
C
C ######################################################################
C
      if((nnodes .eq. 0) .and. (nelements .eq. 0))then
          write(logmess,'(a)')'WARNING: dumpgeofest'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: nnodes=0 nelements = 0'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: No output'
          call writloga('default',0,logmess,0,icscode)
          return
      endif
      isubname="dumpgeofest"
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

C
C  get information from  mesh object
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
C
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
c
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
               call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *         clengths(i),cinterp,cpers,cioflags(i),ierror_return)
                  len2=icharlnf(cnames(i))
                  if(len2 .eq. 3)then
                  if((cnames(i)(1:3).eq.'bcx') .or. 
     1               (cnames(i)(1:3).eq.'bcy') .or. 
     2               (cnames(i)(1:3).eq.'bcz')) then
               iflag=1
               nvalues=nvalues+1
               idx(nvalues)=i
               lvalues=lvalues+iranks(i)
               ioffs(i)=irowlen
               irowlen=irowlen+iranks(i)
               endif
               endif
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
C
C     write header at top of file only if writing node coordinates
C     num_nodes, num_elems, num_node_att, num_elem_att, num_mdata
C       num_mdata is model data and is not used and is set to 0
C       iopt_points toggle node coordinates
C       iopt_elements topggle element connectivity
C       iopt_values_node toggle node attributes 
C       iopt_values_elem toggle element attributes 

      if(iopt_points .eq. 1)then
         nnodes_io = nnodes
      else
         nnodes_io = izero
      endif
      if(iopt_elements .eq. 1)then
         nelements_io = nelements
      else
         nelements_io = izero
      endif
      if(iopt_values_node .eq. 1)then
         nvalues_node = nvalues
      else
         nvalues_node = izero
      endif
      if(iopt_values_elem .eq. 1)then
         nvalues_elem = nvaluese
      else
         nvalues_elem = izero
      endif
C AVS       write(iunit,9030)nnodes_io,nelements_io,nvalues_node,nvalues_elem,
C AVS     *     izero
      
 9030 format(5(i10,1x))
C
C        BEGIN: Write the Node attributes bcx, bcy, bcz if they exist
C
C        If there are no node attributes, skip
      write(iunit,*)'### GEOFEST ###'
      write(iunit,*)'### GEOFEST bcx,bcy,bcz values if they exist ###'
      write(iunit,*)'### GEOFEST ###'
         lenval=lvalues*nnodes
      if(lenval .eq. 0)then
      write(iunit,*)'### GEOFEST bcx,bcy,bcz do not exist ###'
      write(iunit,*)'### GEOFEST bcx,bcy,bcz not written ###'
      write(iunit,*)'### GEOFEST ###'
         go to 9025
      endif
         call mmgetblk('xvalues',isubname,ipxvalues,lenval,2,icscode)
         if (lenval.eq.0) then
           lenval = 0
           go to 9999
         endif

         nvalues1=0
         do i=1,nmcmoatt
            len1=icharlnf(cioflags(i))
            iflag=0
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
               call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *         clengths(i),cinterp,cpers,cioflags(i),ierror_return)
                  len2=icharlnf(cnames(i))
                  if(len2 .eq. 3)then
                  if((cnames(i)(1:3).eq.'bcx') .or. 
     1               (cnames(i)(1:3).eq.'bcy') .or. 
     2               (cnames(i)(1:3).eq.'bcz')) then
                        iflag=1
                        nvalues1=nvalues1+1
                  endif
                  endif
               endif
            enddo
            if(iflag.eq.1) then
 
C
               call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *         clengths(i),cinterp,cpers,cioflags(i),ierror_return)
               lent=icharlnf(ctype)
               if(ctype(1:lent).eq.'VINT') then
C
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
                          enddo
                        enddo
                     elseif(length.eq.nelements) then
c                        write(logmess,9100) cnames(i),
c     *                                      length,irank
c 9100                   format("Invalid length for AVS variable: ",
c     *                         a,' length=',i10,' rank=',i10)
c                        call x3d_error(isubname,'Illegal cmo_name')
                     else
                        write(logmess,9100) cnames(i),
     *                                      length,irank
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               elseif(ctype(1:lent).eq.'VDOUBLE') then
C
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

c
C   Only write out integers bcx, bcy, bcz integer values
C         
         ncolumn_max = 100
         do i1=1,nnodes
            write(iunit,9082) i1,0,
     *        (nint(xvalues(i+nvalues1*(i1-1))),
     *         i=1,min(ncolumn_max,nvalues1))
     
 9082       format(i10,i4,100(1x,i10))
            if(nvalues1.gt. ncolumn_max) then
               do j=ncolumn_max+1,nvalues1,ncolumn_max
                  write(iunit,9087)
     *             (xvalues(i+nvalues1*(i1-1)),
     *              i=j,min(j+ncolumn_max-1,nvalues1))
 9087             format(100(1x,1i10))
               enddo
            endif
         enddo
         write(iunit, 9046)0,0
C
C        END: Write the Node attributes bcx, bcy, bcz if they exist
C
 9025 continue
C     Write the point coordinates
      if(iopt_points.eq.1) then
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
            write(iunit,9040) i,0, a,b,c
 9040       format(i10.10,1x,i2,1x,3(1pe20.12))
         enddo
         write(iunit, 9046)0,0
 9046    format(i2,1x,i2)
      endif
      write(iunit,*)'### GEOFEST USER INPUT REQUIRED HERE ###'
      write(iunit,*)'### GEOFEST USER INPUT REQUIRED HERE ###'
      write(iunit,*)'### GEOFEST USER INPUT REQUIRED HERE ###'

C     Write the connectivity
      if(iopt_elements.eq.1) then
C
C     Test that elements are valid type for GEOFEST. If
C     they are not still write out but give a warning.
C
         call geofest_element_type_test(nelements,itettyp,
     1   ifelmpnt,ifelmlin,ifelmtri,ifelmqud,ifelmtet,ifelmpyr,
     2   ifelmpri,ifelmhex)
         
         do it=1,nelements
            if(itettyp(it).eq.ifelmpnt) then
               index=itetoff(it)
               i1=itet(1+index)
               write(iunit,9050) it,0,itetclr(it),i1
 9050          format(i10,1x,i2,1x,i5,8(1x,i10))
            elseif(itettyp(it).eq.ifelmlin) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               write(iunit,9050) it,0,itetclr(it),i1,i2
            elseif(itettyp(it).eq.ifelmtri) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               write(iunit,9050) it,0,itetclr(it),i1,i2,i3
            elseif(itettyp(it).eq.ifelmqud) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,9050) it,0,itetclr(it),i1,i2,i3,i4
            elseif(itettyp(it).eq.ifelmtet) then
               index=itetoff(it)
               i1=itet(1+index)
               i2=itet(2+index)
               i3=itet(3+index)
               i4=itet(4+index)
               write(iunit,9050) it,0,itetclr(it),i1,i2,i4,i3
            elseif(itettyp(it).eq.ifelmpyr) then
               index=itetoff(it)
               i2=itet(1+index)
               i3=itet(2+index)
               i4=itet(3+index)
               i5=itet(4+index)
               i1=itet(5+index)
               write(iunit,9050) it,0,itetclr(it),i1,i2,i3,i4,i5
            elseif(itettyp(it).eq.ifelmpri) then
               index=itetoff(it)
               i4=itet(1+index)
               i5=itet(2+index)
               i6=itet(3+index)
               i1=itet(4+index)
               i2=itet(5+index)
               i3=itet(6+index)
               write(iunit,9050) it,0,itetclr(it),
     *                           i1,i2,i3,i4,i5,i6
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
               write(iunit,9050) it,0,itetclr(it),
     *                           i1,i2,i3,i4,i5,i6,i7,i8
            endif
         enddo
         write(iunit, 9046)0,0
      endif
C
C     Write the node and/or element attributes
C
      
      if(iopt_values_node.eq.1) then
         write(iunit,9060) nvalues,(iranks(idx(i)),i=1,nvalues)
 9060    format(i5.5,100(1x,i2))
         do i=1,nmcmoatt
            len1=icharlnf(cioflags(i))
            do j=1,len1
               if(cioflags(i)(j:j).eq.'a'.and.
     *            clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
                  len2=icharlnf(cnames(i))
                  call mmfindbk(cnames(i),cmo,iadr,length,icscode)
                  call mmgettyp(iadr,itypout,icscode)
                  if(itypout.eq.1) then
                        write(iunit,9070) cnames(i)(1:len2),', integer '
                  elseif(itypout.eq.2) then
                        write(iunit,9070) cnames(i)(1:len2),', real '
                  else
                        write(iunit,9070) cnames(i)(1:len2),', no units'
                  endif
 9070             format(a,a)
               endif
            enddo
         enddo
C

C        Write the Node attributes
C        If there are no node attributes, skip to elem attributes
         lenval=lvalues*nnodes
         call mmgetblk('xvalues',isubname,ipxvalues,lenval,2,icscode)
         if (iopt_values_node.eq.0 .or. lenval.eq.0) then
           lenval = 0
           go to 9999
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
                          enddo
                        enddo
                     elseif(length.eq.nelements) then
c                        write(logmess,9100) cnames(i),
c     *                                      length,irank
 9100                   format("Invalid length for AVS variable: ",
     *                         a,' length=',i10,' rank=',i10)
c                        call x3d_error(isubname,'Illegal cmo_name')
                     else
                        write(logmess,9100) cnames(i),
     *                                      length,irank
                        call x3d_error(isubname,'Illegal cmo_name')
                     endif
                  endif
               elseif(ctype(1:lent).eq.'VDOUBLE') then
C
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
         
         ncolumn_max = 100
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
      endif

C
 9999 close(iunit)
      continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end


      subroutine geofest_element_type_test(nelements,itettyp,
     1   ifelmpnt,ifelmlin,ifelmtri,ifelmqud,ifelmtet,ifelmpyr,
     2   ifelmpri,ifelmhex)
      integer nelements
      integer ifelmpnt,ifelmlin,ifelmtri,ifelmqud,ifelmtet,ifelmpyr,
     1   ifelmpri,ifelmhex
      integer itettyp(*)
      integer iet_pnt_num, iet_lin_num, iet_tri_num, iet_quad_num,
     1        iet_tet_num, iet_pyr_num, iet_pri_num, iet_hex_num
      integer it
      character*132 logmess
      
      iet_pnt_num = 0
      iet_lin_num = 0
      iet_tri_num = 0
      iet_quad_num = 0
      iet_tet_num = 0
      iet_pyr_num = 0
      iet_pri_num = 0
      iet_hex_num = 0
      do it=1,nelements
            if(itettyp(it).eq.ifelmpnt) then
               iet_pnt_num = iet_pnt_num + 1
            elseif(itettyp(it).eq.ifelmlin) then
               iet_lin_num = iet_lin_num + 1
            elseif(itettyp(it).eq.ifelmtri) then
               iet_tri_num = iet_tri_num + 1
            elseif(itettyp(it).eq.ifelmqud) then
               iet_quad_num = iet_quad_num + 1
            elseif(itettyp(it).eq.ifelmtet) then
               iet_tet_num = iet_tet_num + 1
            elseif(itettyp(it).eq.ifelmpyr) then
               iet_pyr_num = iet_pyr_num + 1
            elseif(itettyp(it).eq.ifelmpri) then
               iet_pri_num = iet_pri_num + 1
            elseif(itettyp(it).eq.ifelmhex) then
               iet_hex_num = iet_hex_num + 1
            endif
      enddo

      if(iet_pnt_num .ne. 0)then
          write(logmess,100) iet_pnt_num
  100     format(' GEOFEST OUTPUT WARNING: ELEMENTS OF TYPE pnt = ',i10)
          call writloga('default',0,logmess,0,icscode)
      endif
      if(iet_lin_num .ne. 0)then
          write(logmess,101) iet_lin_num
  101     format(' GEOFEST OUTPUT WARNING: ELEMENTS OF TYPE lin = ',i10)
          call writloga('default',0,logmess,0,icscode)
      endif
      if(iet_pyr_num .ne. 0)then
          write(logmess,102) iet_pyr_num
  102     format(' GEOFEST OUTPUT WARNING: ELEMENTS OF TYPE pyr = ',i10)
          call writloga('default',0,logmess,0,icscode)
      endif
      if(iet_pri_num .ne. 0)then
          write(logmess,103) iet_pri_num
  103     format(' GEOFEST OUTPUT WARNING: ELEMENTS OF TYPE pri = ',i10)
          call writloga('default',0,logmess,0,icscode)
      endif
      return
      end
