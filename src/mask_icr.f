      subroutine mask_icr(ierror)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1996                              C
C This program was prepared by the Regents of the University of        C
C California at Los Alamos National Laboratory (the University) under  C
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). C
C The University has certain rights in the program pursuant to the     C
C contract and the program should not be copied or distributed outside C
C your organization. All rights in the program are reserved by the DOE C
C and the University. Neither the U.S. Government nor the University   C
C makes any warranty, express or implied, or assumes and liability or  C
C responsibility for the use of this software.                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C#######################################################################
C
C      PURPOSE -
C
C         Assign a bit pattern to the ibnd array of a cmo.
C         Each element is searched.
C         If all nodes of a face/edge are on the outside
C         determine the normal to the face/edge. From that
C         normal vector figure out which of 6 quadrents,
C         12 edges or 8 corners the normal points. Depending
C         on that value from 1-26, assign a bit pattern defined
C         below.
C
C         Note that a node could be of type top/bottom and that
C         would not be ambiguous since any node could belong to
C         faces/edges of many different orientations.
C
C      INPUT ARGUMENTS -
C
C         none
C
C      OUTPUT ARGUMENTS -
C
C         ierror
C
C     AUTHOR
C
C         Carl W. Gable  gable@lanl.gov (Los Alamos National Laboratory)
C
C      CHANGE HISTORY -
C
C        $Log: mask_icr.f,v $
C        Revision 2.00  2007/11/05 19:46:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   07 Feb 2000 18:31:52   gable
CPVCS    Modified by RVG to use ibnd temp array instead of icr array.
CPVCS    
CPVCS       Rev 1.3   Mon Mar 02 08:26:10 1998   tam
CPVCS    added epsilon to call assign_quadrant_id()
CPVCS    added check for unset itp1 array
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:52:44 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Mon Jun 03 15:12:34 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:39:10 1996   gable
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "local_element.h"
      include "chydro.h"
      integer ipat(64)
      character*160 logmess
c
c     The ibnd array will be assign an integer value depending on the following
c     bit patterns.
      data ipat( 1)  /  0 / ! 000000   ---    ---    ---    ---    ---    ---
      data ipat( 2)  /  1 / ! 000001  bottom  ---    ---    ---    ---    ---
      data ipat( 3)  /  2 / ! 000010   ---   top     ---    ---    ---    ---
      data ipat( 4)  /  3 / ! 000011  bottom top     ---    ---    ---    ---
      data ipat( 5)  /  4 / ! 000100   ---    ---   front   ---    ---    ---
      data ipat( 6)  /  5 / ! 000101  bottom  ---   front   ---    ---    ---
      data ipat( 7)  /  6 / ! 000110   ---   top    front   ---    ---    ---
      data ipat( 8)  /  7 / ! 000111  bottom top    front   ---    ---    ---
      data ipat( 9)  /  8 / ! 001000   ---    ---    ---   right   ---    ---
      data ipat(10)  /  9 / ! 001001  bottom  ---    ---   right   ---    ---
      data ipat(11)  / 10 / ! 001010   ---   top     ---   right   ---    ---
      data ipat(12)  / 11 / ! 001011  bottom top     ---   right   ---    ---
      data ipat(13)  / 12 / ! 001100   ---    ---   front  right   ---    ---
      data ipat(14)  / 13 / ! 001101  bottom  ---   front  right   ---    ---
      data ipat(15)  / 14 / ! 001110   ---   top    front  right   ---    ---
      data ipat(16)  / 15 / ! 001111  bottom top    front  right   ---    ---
      data ipat(17)  / 16 / ! 010000   ---    ---    ---    ---   back    ---
      data ipat(18)  / 17 / ! 010001  bottom  ---    ---    ---   back    ---
      data ipat(19)  / 18 / ! 010010   ---   top     ---    ---   back    ---
      data ipat(20)  / 19 / ! 010011  bottom top     ---    ---   back    ---
      data ipat(21)  / 20 / ! 010100   ---    ---   front   ---   back    ---
      data ipat(22)  / 21 / ! 010101  bottom  ---   front   ---   back    ---
      data ipat(23)  / 22 / ! 010110   ---   top    front   ---   back    ---
      data ipat(24)  / 23 / ! 010111  bottom top    front   ---   back    ---
      data ipat(25)  / 24 / ! 011000   ---    ---    ---   right  back    ---
      data ipat(26)  / 25 / ! 011001  bottom  ---    ---   right  back    ---
      data ipat(27)  / 26 / ! 011010   ---   top     ---   right  back    ---
      data ipat(28)  / 27 / ! 011011  bottom top     ---   right  back    ---
      data ipat(29)  / 28 / ! 011100   ---    ---   front  right  back    ---
      data ipat(30)  / 29 / ! 011101  bottom  ---   front  right  back    ---
      data ipat(31)  / 30 / ! 011110   ---   top    front  right  back    ---
      data ipat(32)  / 31 / ! 011111  bottom top    front  right  back    ---
      data ipat(33)  / 32 / ! 100000   ---    ---    ---    ---    ---   left
      data ipat(34)  / 33 / ! 100001  bottom  ---    ---    ---    ---   left
      data ipat(35)  / 34 / ! 100010   ---   top     ---    ---    ---   left
      data ipat(36)  / 35 / ! 100011  bottom top     ---    ---    ---   left
      data ipat(37)  / 36 / ! 100100   ---    ---   front   ---    ---   left
      data ipat(38)  / 37 / ! 100101  bottom  ---   front   ---    ---   left
      data ipat(39)  / 38 / ! 100110   ---   top    front   ---    ---   left
      data ipat(40)  / 39 / ! 100111  bottom top    front   ---    ---   left
      data ipat(41)  / 40 / ! 101000   ---    ---    ---   right   ---   left
      data ipat(42)  / 41 / ! 101001  bottom  ---    ---   right   ---   left
      data ipat(43)  / 42 / ! 101010   ---   top     ---   right   ---   left
      data ipat(44)  / 43 / ! 101011  bottom top     ---   right   ---   left
      data ipat(45)  / 44 / ! 101100   ---    ---   front  right   ---   left
      data ipat(46)  / 45 / ! 101101  bottom  ---   front  right   ---   left
      data ipat(47)  / 46 / ! 101110   ---   top    front  right   ---   left
      data ipat(48)  / 47 / ! 101111  bottom top    front  right   ---   left
      data ipat(49)  / 48 / ! 110000   ---    ---    ---    ---   back   left
      data ipat(50)  / 49 / ! 110001  bottom  ---    ---    ---   back   left
      data ipat(51)  / 50 / ! 110010   ---   top     ---    ---   back   left
      data ipat(52)  / 51 / ! 110011  bottom top     ---    ---   back   left
      data ipat(53)  / 52 / ! 110100   ---    ---   front   ---   back   left
      data ipat(54)  / 53 / ! 110101  bottom  ---   front   ---   back   left
      data ipat(55)  / 54 / ! 110110   ---   top    front   ---   back   left
      data ipat(56)  / 55 / ! 110111  bottom top    front   ---   back   left
      data ipat(57)  / 56 / ! 111000   ---    ---    ---   right  back   left
      data ipat(58)  / 57 / ! 111001  bottom  ---    ---   right  back   left
      data ipat(59)  / 58 / ! 111010   ---   top     ---   right  back   left
      data ipat(60)  / 59 / ! 111011  bottom top     ---   right  back   left
      data ipat(61)  / 60 / ! 111100   ---    ---   front  right  back   left
      data ipat(62)  / 61 / ! 111101  bottom  ---   front  right  back   left
      data ipat(63)  / 62 / ! 111110   ---   top    front  right  back   left
      data ipat(64)  / 63 / ! 111111  bottom top    front  right  back   left
 
C
C#######################################################################
C
      character*32  cmo
C
C
      integer icscode
C
C#######################################################################
C
      character*32 isubname
C
      pointer (ipibnd, ibnd)
      pointer (ipitp1, itp1)
      integer ibnd(10000000), itp1(1000000)
      pointer (ipxic, xic )
      real*8 xic(1000000)
      pointer (ipyic, yic )
      real*8 yic(1000000)
      pointer (ipzic, zic )
      real*8 zic(1000000)
C
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
      pointer (ipitettyp, itettyp )
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff )
      integer itetoff(1000000)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(1000000)
      pointer (ipitet, itet1 )
      integer itet1(1000000)
      pointer (ipjtet, jtet1 )
      integer jtet1(1000000)
C
      pointer (ipiwork, iwork)
      integer iwork(1000000)
C
      real*8 vector(3)
      real*8 x(4),y(4),z(4)
      real*8 dx, dy, dz, xmag_dir
      real*8 dxtri, dytri, dztri
      real*8 xdir, ydir, zdir
      real*8 epsilon
C
      integer ibits
      integer i,i1,it,i2,i3,i4,ioff,joff
      integer nnodes, nelements, mbndry
      integer ilen, itype, length
      integer id, mask1, mask2, mask3, mask4
      integer ierror, ierrw
      logical ifset
C
C#######################################################################
C
C
C#######################################################################
C
C
      isubname='mask_icr'
C
      icscode = -1
C
c     use epsilon for identify_dot()
c     to find very small values that should be zero
      call get_epsilon('epsilon',epsilon)
      write(logmess,'(a,1pe15.7)')
     *      "assign_quadrant_id epsilon for zero: ",epsilon
      call writloga('default',1,logmess,0,ierrw)
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
CRVG
CRVG  'ibnd' is not a standard mesh object array. It must be allocated
CRVG  in the routine calling mask_icr for temporary use
CRVG
      call cmo_get_info('ibnd',cmo,ipibnd,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      length=nnodes
C
      length = nnodes
      call mmgetblk('iwork',isubname,ipiwork,length,1,ierror)
c
c     Find exterior points using flags in itp1 array.
c     iwork array will be 1 for exterior, 0 for interior
c
      call unpacktp('boundary','set',nnodes,ipitp1,ipiwork,ierror)
c
      ifset=.false.
      do i1=1,nnodes
         ibnd(i1) = 0
         if (itp1(i1).ne.0) ifset=.true.
      enddo
      if (.not.ifset) then
           write(logmess,'(a,i10)')
     *     'WARNING: itp1 not set for this mesh - use resetpts/itp ' 
            call writloga('default',1,logmess,1,ierrw)
      endif
      do it=1,nelements
         ioff=itetoff(it)
         joff=jtetoff(it)
         do i=1,nelmnef(itettyp(it))
            i1=itet1(ioff+i)
            x(1)=xic(i1)
            y(1)=yic(i1)
            z(1)=zic(i1)
            if(jtet1(joff+i).eq.mbndry) then
               if(itettyp(it).eq.ifelmtet) then
                  i2=itet1(ioff+ielmface1(1,i,itettyp(it)))
                  i3=itet1(ioff+ielmface1(2,i,itettyp(it)))
                  i4=itet1(ioff+ielmface1(3,i,itettyp(it)))
c
c      Test that all 3 nodes are boundary nodes
c
                  if(iwork(i2)+iwork(i3)+iwork(i4).eq.3)then
                    x(2)=xic(i2)
                    y(2)=yic(i2)
                    z(2)=zic(i2)
                    x(3)=xic(i3)
                    y(3)=yic(i3)
                    z(3)=zic(i3)
                    x(4)=xic(i4)
                    y(4)=yic(i4)
                    z(4)=zic(i4)
                    vector(1)= 0.5d+00*
     *              ((y(2)-y(3))*(z(4)-z(3))-(y(4)-y(3))*(z(2)-z(3)))
                    vector(2)=-0.5d+00*
     *              ((x(2)-x(3))*(z(4)-z(3))-(x(4)-x(3))*(z(2)-z(3)))
                    vector(3)= 0.5d+00*
     *              ((x(2)-x(3))*(y(4)-y(3))-(x(4)-x(3))*(y(2)-y(3)))
c
c      Find out which of 26 possible directions the vector points
c
                    call assign_quadrant_id(id,vector,epsilon,ierror)
                    if (ierror.ne.0) then
                       write(logmess,'(a,i10)')
     *                 'WARNING: unknown orientation for element ',it
                       call writloga('default',1,logmess,1,ierrw)
                    endif
                    ibits = 0
c
c      Based on the value of id, set the bit pattern in ibits
c
                    call set_bit_type(ibits,id)
                    mask1 = ibits
                    mask2 = ibnd(i2)
                    mask3 = ibnd(i3)
                    mask4 = ibnd(i4)
                    ibnd(i2) = ior(mask1,mask2)
                    ibnd(i3) = ior(mask1,mask3)
                    ibnd(i4) = ior(mask1,mask4)
                  endif
               elseif(itettyp(it).eq.ifelmtri) then
                  i2=itet1(ioff+ielmface1(1,i,itettyp(it)))
                  i3=itet1(ioff+ielmface1(2,i,itettyp(it)))
c
c      Test that both nodes are boundary nodes
c
                  if(iwork(i2)+iwork(i3).eq.2)then
                    x(2)=xic(i2)
                    y(2)=yic(i2)
                    z(2)=zic(i2)
                    x(3)=xic(i3)
                    y(3)=yic(i3)
                    z(3)=zic(i3)
                    dx=x(3)-x(2)
                    dy=y(3)-y(2)
                    dz=z(3)-z(2)
                    dxtri= ((y(2)-y(1))*(z(3)-z(1))-
     *              (y(3)-y(1))*(z(2)-z(1)))
                    dytri=-((x(2)-x(1))*(z(3)-z(1))-
     *              (x(3)-x(1))*(z(2)-z(1)))
                    dztri= ((x(2)-x(1))*(y(3)-y(1))-
     *              (x(3)-x(1))*(y(2)-y(1)))
                    xdir=-(dy*dztri-dytri*dz)
                    ydir= (dx*dztri-dxtri*dz)
                    zdir=-(dx*dytri-dxtri*dy)
 
                    xmag_dir=sqrt(xdir**2+ydir**2+zdir**2)
                    if(xmag_dir.gt.0.0) then
                       vector(1)=xdir
                       vector(2)=ydir
                       vector(3)=zdir
                    else
                       vector(1)=0.0
                       vector(2)=0.0
                       vector(3)=0.0
                    endif
c
c      Find out which of 26 possible directions the vector points
c
                    call assign_quadrant_id(id,vector,epsilon,ierror)
                    if (ierror.ne.0) then
                       write(logmess,'(a,i10)')
     *                 'WARNING: unknown orientation for element ',it
                       call writloga('default',1,logmess,1,ierrw)
                    endif
                    ibits = 0
c
c      Based on the value of id, set the bit pattern in ibits
c
                    call set_bit_type(ibits,id)
                    mask1 = ibits
                    mask2 = ibnd(i2)
                    mask3 = ibnd(i3)
                    ibnd(i2) = ior(mask1,mask2)
                    ibnd(i3) = ior(mask1,mask3)
                  endif
               endif
            endif
         enddo
      enddo
C
      call mmrelprt(isubname,icscode)
C
      return
      end
