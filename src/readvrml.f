*DK readvrml
      subroutine readvrml(ifile,ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE READS AN "VRML" FILE (*.wrl).
C
C     INPUT ARGUMENTS -
C
C        ifile - INPUT NGP FRO UNFORMATTED FILE.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - RETURN ERROR CODE (== 0 ==> OK, <> 0 ==> AN ERROR)
C
C      CHANGE HISTORY -
C
C        $Log: readvrml.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Mon Jan 26 11:01:54 1998   llt
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include 'chydro.h'
      include 'local_element.h'
C
      parameter (lenptr=10000000)
C
C ######################################################################
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipjtet2, jtet2)
C
      integer imt1(lenptr), itp1(lenptr)
      integer itetclr(lenptr), itettyp(lenptr)
      integer itet1(lenptr), jtet1(lenptr)
      integer itetoff(lenptr), jtetoff(lenptr),jtet2(lenptr)
ccc       REAL*8  xic(lenptr), yic(lenptr), zic(lenptr)
      dimension   xic(1000000), yic(1000000), zic(1000000)
C
      character ifile*(*)
      character*32 isubname, cmo
      character*132 logmess

c parser variables

      character*132 input_msg
      integer lenparse
      integer msg(10)
      integer nwds
      real*8 xmsg(10)
      integer*4 imsg(10)
      character*32 cmsg(10)

      integer ntets
      integer npoints
      integer ilen, itype, icount
C
C#######################################################################
C
C     ******************************************************************
C     SET THE NAME TO USED AS THE PARTITION FOR THE TEMPORARY MEMORY
C     THAT WILL BE ALLOCATED FOR THIS SUBROUTINE.
C
      isubname='readvrml'
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
C
ccc      read(iunit,*) ntets, npoints
      ntets = 1
      npoints = 1
      nsd=2
      nsg=3
      nen=nelmnen(ifelmtri)
      nef=nelmnef(ifelmtri)
C
      ind=0
      itri=0
      itoff=0
      jtoff=0
C
C     ******************************************************************
C     ALLOCATE SOME TEMPORARY MEMORY.
C
      length=nef*ntets
      call mmgetblk('jtet2',isubname,ipjtet2,length,2,icscode)
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmo,nsd,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmo,nsg,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
C
      call cmo_newlen(cmo,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,ierror)
C
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      write(logmess,'(a)')
     &              'Read VRML file (*.wrl) and convert it to X3D cmo.'
      call writloga('default',0,logmess,0,ierror)

c     PARSE FILE UNTIL FIND FIRST COORDINATE3 POINT 

 100  continue
      read(iunit, '(a132)', end=9999, err=9999) input_msg
      if (input_msg(1:7).ne. '# Faces') goto 100
      do i=1,4
        read(iunit, '(a132)', end=9999, err=9999) input_msg 
      enddo
c
c     FIND AND READ COORDINATE3 POINT and INDEXEDFACESET,
c     ASSIGN A NEW COLOR AND READ NEXT SET

      print*, 'Reading in points'

      icount = 0
      do j = 1, 1000

c       READ IN COORDINATE3 POINT
c 
 110    continue
        read(iunit, '(a132)',end=9999) input_msg 
        if (input_msg(5:9).ne.'Coord') goto 110
        npoints_save = ind
        iflag = 0
        dowhile(iflag.eq.0)
           read(iunit, '(a132)') input_msg
           if (input_msg(5:8) .eq. 'Norm') then
ccc             print*, 'LAST xic, yic, zic=', 
ccc     .                xic(ind-1), yic(ind-1), zic(ind-1)
ccc             print*, 'inpoints=', inpoints
             iflag = 1
             ind = ind - 1
           else
             lenparse = len(input_msg)
             do ii = 1, lenparse
               if (input_msg(ii:ii) .eq. '\t') then
c                 print*, 'tab found'
                 input_msg(ii:ii) = ' '
               end if
             end do
             call mmfindbk('xic',cmo,ipxic,length,icscode)
             if((ind+1).ge.length) then
                npointsinc=ind+1000
                call cmo_set_info('nnodes',cmo,
     *                            npointsinc,1,1,ierror)
                call mmgetlen(ipitetclr,nelementsmm,icscode)
                call cmo_set_info('nelements',cmo,
     *                            nelementsmm,1,1,ier)
                call cmo_newlen(cmo,ierror)
                call cmo_get_info('imt1',cmo,
     *                            ipimt1,lenimt1,icmotype,ierror)
                call cmo_get_info('itp1',cmo,
     *                            ipitp1,lenitp1,icmotype,ierror)
                call cmo_get_info('xic',cmo,
     *                            ipxic,lenxic,icmotype,ierror)
                call cmo_get_info('yic',cmo,
     *                            ipyic,lenyic,icmotype,ierror)
                call cmo_get_info('zic',cmo,
     *                            ipzic,lenzic,icmotype,ierror)
                call cmo_get_info('itetclr',cmo,
     *                            ipitetclr,lenitetclr,icmotype,ier)
                call cmo_get_info('itettyp',cmo,
     *                            ipitettyp,lenitettyp,icmotype,ier)
                call cmo_get_info('itetoff',cmo,
     *                            ipitetoff,lenitetoff,icmotype,ier)
                call cmo_get_info('jtetoff',cmo,
     *                            ipjtetoff,lenjtetoff,icmotype,ier)
                call cmo_get_info('itet',cmo,
     *                            ipitet,lenitet,icmotype,ierror)
                call cmo_get_info('jtet',cmo,
     *                            ipjtet,lenjtet,icmotype,ierror)
             endif
             ind = ind + 1
             inpoints = ind
             call parse_string(lenparse, input_msg,
     .                       imsg, msg, xmsg, cmsg, nwds)
             if (cmsg(1)(1:1).eq.'}') then
                 iflag = 1
             elseif (cmsg(1)(1:5).eq.'point') then
                xic(ind)=xmsg(3)
                yic(ind)=xmsg(4)
                zic(ind)=xmsg(5)
ccc                print*, 'FIRST xic, yic, zic=', xic(1), yic(1), zic(1)
             else
               xic(ind)=xmsg(1)
               yic(ind)=xmsg(2)
               zic(ind)=xmsg(3)
             endif
cc             read(iunit,*) xic(ind),yic(ind),zic(ind)
             itp1(ind)=i
             imt1(ind)=i
           end if
        enddo
C
C
c       READ IN INDEXEDFACESET
c
        print*, 'Reading in Indexed Face Set'
 120    continue
        read(iunit, '(a132)') input_msg 
        if (input_msg(5:18).ne.'IndexedFaceSet') goto 120

        iflag = 0
        dowhile(iflag.eq.0) 
           read(iunit, '(a132)') input_msg
           if (input_msg(2:5) .eq. 'norm') then
ccc             print*, 'LAST il, i2, i3=', i1, i2, i3 
ccc             print*, 'intets=', intets
             iflag = 1
           else  
             lenparse = len(input_msg)
             do ii = 1, lenparse
               if (input_msg(ii:ii) .eq. '\t') then
c                 print*, 'tab found'
                 input_msg(ii:ii) = ' '
               end if
             end do
             call mmgetlen(ipitetclr,length,icscode)
             if((itri+1).ge.length) then
                inc=1000
                ntetsinc=itri+inc
                call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
                call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
                call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
                call cmo_newlen(cmo,ierror)
                call cmo_get_info('imt1',cmo,
     *                            ipimt1,lenimt1,icmotype,ierror)
                call cmo_get_info('itp1',cmo,
     *                            ipitp1,lenitp1,icmotype,ierror)
                call cmo_get_info('xic',cmo,
     *                            ipxic,lenxic,icmotype,ierror)
                call cmo_get_info('yic',cmo,
     *                            ipyic,lenyic,icmotype,ierror)
                call cmo_get_info('zic',cmo,
     *                            ipzic,lenzic,icmotype,ierror)
                call cmo_get_info('itetclr',cmo,
     *                            ipitetclr,lenitetclr,icmotype,ier)
                call cmo_get_info('itettyp',cmo,
     *                            ipitettyp,lenitettyp,icmotype,ier)
                call cmo_get_info('itetoff',cmo,
     *                            ipitetoff,lenitetoff,icmotype,ier)
                call cmo_get_info('jtetoff',cmo,
     *                            ipjtetoff,lenjtetoff,icmotype,ier)
                call cmo_get_info('itet',cmo,
     *                            ipitet,lenitet,icmotype,ierror)
                call cmo_get_info('jtet',cmo,
     *                            ipjtet,lenjtet,icmotype,ierror)
             endif
             itri = itri + 1
             intets = itri
             call parse_string(lenparse, input_msg, 
     .                         imsg, msg, xmsg, cmsg, nwds)
             if (cmsg(1)(1:10) .eq. 'coordIndex') then
               if (imsg(6) .ne. -1) then
                 print*, 'ERROR - readvrml presently only handles tri'
                 goto 9999
               endif
               i1=imsg(3)+1
               i2=imsg(4)+1
               i3=imsg(5)+1
ccc               print*, 'FIRST, i1, i2, i3=', i1, i2, i3
             else
               i1=imsg(1)+1
               i2=imsg(2)+1
               i3=imsg(3)+1
             endif
ccc           read(iunit,*) itno,i1,i2,i3,itetclr(itri),idum
             itetclr(itri)=j
             itettyp(itri)=ifelmtri
             itetoff(itri)=itoff
             jtetoff(itri)=jtoff
             itoff=itoff+nelmnen(ifelmtri)
             jtoff=jtoff+nelmnef(ifelmtri)
             itet1(itetoff(itri)+1)=i1+npoints_save
             itet1(itetoff(itri)+2)=i2+npoints_save
             itet1(itetoff(itri)+3)=i3+npoints_save
C
             jtet1(jtetoff(itri)+1)=-1
             jtet1(jtetoff(itri)+2)=-1
             jtet1(jtetoff(itri)+3)=-1
           endif
        enddo
C
        npoints = ind
        ntets=itri
C
C     ******************************************************************
C     CLOSE THE DUMP FILE.
C
        do it=1,ntets
           imt1(itet1(itetoff(it)+1))= itetclr(it)
        enddo
        do i1=1,npoints
           imt1(i1)=max(1,imt1(i1))
        enddo
C
        call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
        call cmo_set_info('nelements',cmo,ntets,1,1,ier)
C
C     ******************************************************************
C     GENERATE CONNECTIVITY
C
        call geniee_cmo(cmo)
C
      enddo
C
      goto 9999
 9999 continue

      close(iunit)
C
C
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,ierror)
C
      return
      end
