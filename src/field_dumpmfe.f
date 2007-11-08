*dk,field_dumpmfe
      subroutine field_dumpmfe(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                    ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        FIELD_DUMPMFE causes a binary dump of the specified fields to
C        two files in the MFEDRAW graphics format.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: field_dumpmfe.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   30 Sep 2004 09:04:36   dcg
CPVCS     change call to real to call to dble
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:47:44 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/05/95 08:25:50   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.2   11/07/95 17:17:20   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   09/19/95 13:10:54   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.0   08/24/95 16:38:10   kuprat
CPVCS    Initial revision.
C
c*  ----------------------------------------------------------------  $&
c*                                                                    $&
c*                                                                    $&
c*    FIELD/DUMPMFE/root file name/x1,y1,z1/x2,y2,z2/field list       $&
c*                                                                    $&
c*         This command causes a binary dump of the specified fields  $&
c*      to two files in the MFEDRAW input format.                     $&
c*                                                                    $&
c*         MFEDRAW is a graphics package for visualizing moving       $&
c*      piecewise linear functions of two variables, such as those    $&
c*      originally encountered in Moving Finite Elements.  The        $&
c*      FIELD/DUMPMFE command names the files 'root1.bin' and         $&
c*      'root2.bin', where 'root' is the ROOT FILE NAME argument.     $&
c*      With the current MFEDRAW package, the graphics files must be  $&
c*      named 'mfe1.bin' and 'mfe2.bin'.  Thus you have to choose     $&
c*      root='mfe', or you have to manually rename the files to       $&
c*      be 'mfe1.bin' and 'mfe2.bin' before viewing.                  $&
c*                                                                    $&
c*         Because the graphics data needs to be a function of two    $&
c*      variables, you must supply two orthonormal vectors            $&
c*      (X1,Y1,Z1) and (X2,Y2,Z2) which specify what the graphics     $&
c*      coordinate axes will be.  More precisely, given 3D coordinates$&
c*      (x,y,z), the 2D graphic coordinates will then be              $&
c*      (x*X1+y*Y1+z*Z1 , x*X2+y*Y2+z*Z2).  So, for example,          $&
c*      the choice /x1,y1,z1/x2,y2,z2/ = /1.,0.,0./0.,1.,0./          $&
c*      causes the writing of graphics data with the 'z' coordinate   $&
c*      discarded and the 'x' and 'y' coordinates unchanged.          $&
c*                                                                    $&
c#######################################################################
C
      implicit none
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,ierrw
 
      character*132 logmess
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      pointer (ipitet,itet)
      pointer (ipireal1,ireal1)
      pointer (ipfield,field)
      pointer (iprfield,rfield)
      pointer (ipu,u)
      pointer (ipv,v)
      integer isetwd(10000000)
      integer itp1(10000000)
      integer itet(3,10000000)
      integer ireal1(10000000)
      real*8 field(10000000)
      real*8 xic(10000000),yic(10000000),zic(10000000)
      REAL rfield(10000000),u(10000000),v(10000000)
      real*8 ufake,vfake,ffake,x1,y1,z1,x2,y2,z2
      integer lenroot, nepn, nelements, ivert, icell, indxreal, i
C
      pointer (ipmpary,mpary)
      integer mpary(10000000)
C
      character*32 cmo,isubname,blkname,root,prevroot,file1,file2
      integer nsdtopo,length,icmotype,icscode,mpno,nnodes
      integer icharln,iunit1,iunit2
      parameter (iunit1=18,iunit2=19)
 
      logical init
      data init /.true./, prevroot/' '/
 
      save init,prevroot
 
      isubname = 'field_dumpmfe'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *         'FIELD_DUMPMFE: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
C  Check that it's a 2D mesh.
      call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      if (nsdtopo .ne. 2) then
         ierror=1
         write(logmess,'(a)')
     *         'FIELD_DUMPMFE: ',cmo,' not a 2D mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
C     Find out which are the 'real' points.
C             ireal1() =  0 ==> not a real point.
C             ireal1() =  1 ==> a real point.
      call cmo_get_info('nnodes',cmo,
     *                        nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,
     *                        ipxic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,
     *                        ipyic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,
     *                        ipzic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,
     *                        ipitet,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,
     *                        nelements,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *                        ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierror)
      if(ierror.ne.0) call x3d_error(isubname, 'unpacktp')
 
      call mmgetblk('rfield',isubname,iprfield,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('u',isubname,ipu,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('v',isubname,ipv,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
 
      blkname=cmsgin(10)(1:icharln(cmsgin(10)))
      call mmgetpr(blkname,cmo,ipfield,icscode)
      if(icscode.ne.0) then
         ierror=icscode
         write(logmess,'(a)')
     *      'FIELD_DUMPMFE: bad field specified'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      lenroot=icharln(cmsgin(3))
      root=cmsgin(3)(1:lenroot)
      file1=root
      file2=root
      file1(lenroot+1:lenroot+5)="1.bin"
      file2(lenroot+1:lenroot+5)="2.bin"
 
      if (init.or.(root.ne.prevroot)) then
         if (.not.init) then
            close(iunit1)
            close(iunit2)
         else
            init=.false.
         endif
         open(unit=iunit1,file=file1,status='unknown',
     .        form='unformatted')
         open(unit=iunit2,file=file2,status='unknown',
     .        form='unformatted')
      endif
      prevroot=root
      nepn=3
      write(iunit2).true.
      write(iunit1)nepn,1,nelements,nnodes
      write(iunit1) dble(1.)
      write(iunit1)((itet(ivert,icell),ivert=1,3),icell=1,nelements)
      write(iunit1)1,2
 
      do i=1,nnodes
         if (ireal1(i).eq.1) then
            indxreal=i
            goto 100
         endif
      enddo
      write(logmess,'(a)')
     *   'FIELD_DUMPMFE: no real points!'
      call writloga('default',0,logmess,0,ierrw)
      goto 9999
 
 100  continue
      ufake=x1*xic(indxreal)+y1*yic(indxreal)+z1*zic(indxreal)
      vfake=x2*xic(indxreal)+y2*yic(indxreal)+z2*zic(indxreal)
      ffake=field(indxreal)
 
      call test_argument_type(6,2,4,imsgin,xmsgin,cmsgin,msgtype,nwds)
      x1=xmsgin(4)
      y1=xmsgin(5)
      z1=xmsgin(6)
      x2=xmsgin(7)
      y2=xmsgin(8)
      z2=xmsgin(9)
      do i=1,nnodes
         if (ireal1(i).eq.1) then
            u(i)=x1*xic(i)+y1*yic(i)+z1*zic(i)
            v(i)=x2*xic(i)+y2*yic(i)+z2*zic(i)
            rfield(i)=field(i)
         else
            u(i)=ufake
            v(i)=vfake
            rfield(i)=ffake
         endif
      enddo
 
      write(iunit2) (rfield(i),u(i),v(i),i=1,nnodes)
 
 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
 
      return
      end
