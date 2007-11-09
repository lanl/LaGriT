 
C#####################################################################
C
C      FILE -
C
C      Source code for geological applications using x3dgen
C      Original file was adrivgen.f by Carl Gable
C
C      CHANGE HISTORY -
C
C      Original version - Carl Gable - 97
C      error checking and extensions - T.Cherry - 9/97
C
C
C#####################################################################
 
C
C
      subroutine volume_material(imsgin,xmsgin,cmsgin,
     >                            msgtyp,nwds,ierr_return)
C
C#####################################################################
C
C      PURPOSE -
c
c     REPLACED by quality/volume/material command
C
C     Calculate the volume of each material where materials are
C     defined by the integers in the itetclr array.
C
C     The default is to loop through all element colors.
C
C     One can quary a single material color with the command:
C            volume_material / material_number
C
C     or a set of material colors using start, stop, stride parameters
C            volume_material / istart istop istride
C
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C        original version - Carl Gable - 97
C
C        $Log: tempgable.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#####################################################################
C
      implicit none
C
c      character*132 logmess
C
C ####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include "chydro.h"
      include "local_element.h"
c
      pointer (ipimt1, imt1)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet1, itet1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
 
      real*8 xic(10000000), yic(10000000), zic(10000000)
      integer imt1(1000000)
      integer itet1(10000000)
      integer itetclr(1000000), itettyp(1000000), itetoff(1000000)
      real*8 xicvol(8), yicvol(8), zicvol(8)
      integer ilen, itype, ier,ierrw, lenimt1, lenitetclr, lenitetoff
      integer lenitet1, lenitettyp
      integer idx, istart, iend, iinc
      integer it, ic, i, i1, icount_all
      integer nnodes, numtet, mbndry
      real*8  xtetvol, volume_all
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      integer icharlnf
      integer ierr_return
 
      real*8 vol_mat(1028)
      integer node_mat(1028)
 
      character*32 cmsgin(nwds)
      character*32 isubname, cmonam
      character*132 logmess
C
C
C#######################################################################
C
C
      isubname='volume_materials'
      ierr_return=0
c
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmonam,ier)
      if(ier.ne.0) then
         write(logmess,'(a)')
     *   'VOLUME/MATERIALS: ',cmonam,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         ierr_return = -1
         goto 9999
      endif
 
C     Check to see if this Mesh Object exists.
      call cmo_exist(cmonam,ier)
      if(ier.ne.0) then
         write(logmess,'(a,a)')
     *   'VOLUME/MATERIALS: Mesh Object does not exist: ', cmonam
         call writloga('default',0,logmess,0,ierrw)
         ierr_return = -1
         goto 9999
      endif
 
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,numtet,ilen,itype,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ier)
C
      call cmo_get_info('xic',cmonam,ipxic,ilen,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('yic',cmonam,ipyic,ilen,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('zic',cmonam,ipzic,ilen,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('imt1',cmonam,ipimt1,lenimt1,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('itet',cmonam,ipitet1,lenitet1,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('itettyp',cmonam,ipitettyp,lenitettyp,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('itetclr',cmonam,ipitetclr,lenitetclr,itype,ier)
      if(ier.ne.0) goto 9999
      call cmo_get_info('itetoff',cmonam,ipitetoff,lenitetoff,itype,ier)
      if(ier.ne.0) goto 9999
C
c
c     quick fix to reassign nodes based on element color
c
c     Loop through all colors (max - min)
c     Loop through all elements
C
c     parse commands - look for single or set of materials
      idx=0
      do i = 1, nwds
         if (msgtyp(i).eq.1  .and. idx.eq.0) idx=i
      enddo
 
C     use set of materials defined by start,end,stride
      if (idx.ne.0 .and.
     >    msgtyp(idx+1).eq.1 .and. msgtyp(idx+2).eq.1 ) then
         istart = imsgin(idx)
         iend   = imsgin(idx+1)
         iinc   = imsgin(idx+2)
 
c     report on one selected material
      elseif (idx .ne. 0) then
         istart = imsgin(idx)
         iend   = imsgin(idx)
         iinc   = imsgin(idx)
 
C     default - use all materials
      else
        iinc   = 1
        istart = 1
        iend   = 0
        do it = 1, numtet
          istart = min(istart, itetclr(it))
          iend   = max(iend  , itetclr(it))
        enddo
      endif
      if (iinc .eq. 0) iinc=1
 
c
      if(iend .gt. 1028)then
        write(logmess,'(a,i10)')
     1     'ERROR: Max. material number > 1028 ',iend
        call writloga('default',0,logmess,0,ierrw)
        call writloga('default',0,logmess,0,ierrw)
        call writloga('default',0,logmess,0,ierrw)
        call writloga('default',0,logmess,0,ierrw)
        return
      endif
 
c
      write(logmess,'(a,a,i10,2x,i10)')
     >'volume/area of materials for ',
     > cmonam(1:icharlnf(cmonam)),istart,iend
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a,i10,2x,i10)')
     >'Material   Num. Elements   Volume          Fractional Volume'
      call writloga('default',0,logmess,0,ierrw)
 
      volume_all = 0.0
      icount_all = 0
      do ic = istart, iend, iinc
      vol_mat(ic) = 0.0
      node_mat(ic) = 0
      do it = 1, numtet
        if(itetclr(it) .eq. ic)then
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  xicvol(i)=xic(i1)
                  yicvol(i)=yic(i1)
                  zicvol(i)=zic(i1)
               enddo
 
               call volume_element(itettyp(it),
     *                             xicvol,yicvol,zicvol,
     *                             xtetvol)
      if(xtetvol .lt. 0.0)then
      write(logmess,'(a,i10,2x,e14.7)')
     1     'Warning: Negative Element Volume ',it,xtetvol
      endif
 
               vol_mat(ic) = vol_mat(ic) + xtetvol
               node_mat(ic)= node_mat(ic)   + 1
        endif
      enddo
 
      icount_all = icount_all + node_mat(ic)
      volume_all = volume_all + vol_mat(ic)
c
      enddo
 
c.....Output volumes of materials
      do ic = istart, iend, iinc
 
         write(logmess,'(i5,2x,i10,9x,e14.7,2x,f14.9)')
     1   ic,node_mat(ic),vol_mat(ic),vol_mat(ic)/volume_all
         call writloga('default',0,logmess,0,ierrw)
      enddo
      if (istart.ne.iend) then
        write(logmess,'(a,2x,i10,9x,e14.7)')
     1  'Total     ',icount_all, volume_all
        call writloga('default',0,logmess,0,ierrw)
      endif
c
9999  if (ier.ne.0) then
        call x3d_error(isubname,' get info ')
        ierr_return=-1
      endif
 
      return
      end
C
C
      subroutine test_volume(imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C #####################################################################
C
C     PURPOSE -
c
c     REPLACED by quality/volume/lt|gt|eq/xvalue command
C
C     Calculate the volume of elements and report any element the
c     is below the threshold. The nodes and coordinates for these
c     elements are also reported. Default threshold volume is 0.0
c
c     User defined cutoff volume
c     test_volume / volume
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
CPVCS
CPVCS    Original version - Carl Gable - 97
C
c#######################################################################
c
c     FORMAT:
c
c        TEST_VOLUME    -  Default usage, reports elem below 0.0
c        TEST_VOLUME / threshold - user defined threshold value
C
C        This option has been added to the REPORT command:
C        REPORT/VOLUME/THRESHOLD
C        REPORT/VOLUME/THRESHOLD/xvalue
C        REPORT/VOLUME/THRESHOLD/COLOR
C
C          xvalue  -  user defined lower bound for element volumes
C                     any volumes below this are reported. The
C                     default value is 0.0
C          COLOR   -  character string option indicating that elements
C                     should be colored such that
C                     element volumes > threshold value -> 1
C                     element volumes = threshold value -> 2
C                     element volumes < threshold value -> 3
C
C     EXAMPLE:
C
C     _____
C    |
C    | Enter a command
C    |report/volume/threshold/ 1.0
C    |
C    |Looking for volume of elements .le.     1.0000000000000
C    | Volume    316   0.50770984862851
C    |  91    169736.27608300    232100.00000000    666.28329778760
C    |  92    169736.17628500    232100.00000000    673.72754260650
C    |  590    169736.35616210    232100.00000000    670.48469733350
C    |
C    |       1 elements le:   0.100000E+01
C    |_____
c
c#######################################################################
C
      implicit none
      include "chydro.h"
      include "local_element.h"
c
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
c
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet1, itet1)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitetclr, itetclr)
 
      real*8 xic(10000000), yic(10000000), zic(10000000)
      integer itet1(10000000), itettyp(10000000)
      integer itetoff(10000000), itetclr(1000000)
 
      real*8 xicvol(8), yicvol(8), zicvol(8)
c      real*8 x(8), y(8), z(8)
c      real*8 dx, dy, dz, xtetvol, xvol_vor, xvol_edge
c      real*8 voltet_vor(4), voledge_vor(6), vol_test
 
      integer ilen, itype
      integer ier,ier1,ier2,ier3, ierrw
      integer nnodes, nelements, mbndry
      integer ntotal, icolor
      integer nwds, it, i, i1
      real*8 xmsgin(nwds)
      real*8 vol_test
      real*8 xtetvol
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
      character*32 isubname, cmonam
      character*132 logmess
C
C
C#######################################################################
C
C
      isubname='test_volume'
      vol_test = 0.0d0
      icolor=0
 
      do i = 1, nwds
        if(msgtyp(i).eq.3) then
          if (cmsgin(i)(1:5).eq.'color') icolor=1
        endif
        if(msgtyp(i).eq.2) vol_test = xmsgin(i)
      enddo
 
      write(6,*)'Looking for volume of elements .le. ' ,vol_test
      if (icolor .ne. 0) write(6,*)'tet faces will be colored.'
c
      call cmo_get_name(cmonam,ier)
      if(ier.ne.0) call x3d_error('cmo get name :',cmonam)
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier1)
      call cmo_get_info('nelements',cmonam,nelements,ilen,itype,ier2)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ier3)
      if(ier1.ne.0  .or. ier2.ne.0  .or. ier3.ne.0)
     >   call x3d_error('isubname',' get info '//cmonam)
C
      call cmo_get_info('xic',cmonam,ipxic,ilen,itype,ier1)
      call cmo_get_info('yic',cmonam,ipyic,ilen,itype,ier2)
      call cmo_get_info('zic',cmonam,ipzic,ilen,itype,ier3)
      if(ier1.ne.0  .or. ier2.ne.0  .or. ier3.ne.0)
     >   call x3d_error('isubname',' get info '//cmonam)
 
      call cmo_get_info('itet',cmonam,ipitet1,ilen,itype,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,ilen,itype,ier1)
      call cmo_get_info('itetoff',cmonam,ipitetoff,ilen,itype,ier2)
      call cmo_get_info('itetclr',cmonam,ipitetclr,ilen,itype,ier3)
      if(ier.ne.0 .or. ier1.ne.0  .or. ier2.ne.0  .or. ier3.ne.0)
     >   call x3d_error('isubname',' get info '//cmonam)
 
C.....loop through elements
      ntotal=0
      do it=1,nelements
 
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            xicvol(i)=xic(i1)
            yicvol(i)=yic(i1)
            zicvol(i)=zic(i1)
         enddo
 
         call volume_element(itettyp(it),
     *                       xicvol,yicvol,zicvol,
     *                       xtetvol)
 
         if(xtetvol.le. vol_test) then
            write(6,*)' Volume  ', it,  xtetvol
            ntotal=ntotal+1
            do i=1,nelmnen(itettyp(it))
               i1=itet1(itetoff(it)+i)
               write(6,*)i1,xic(i1),yic(i1),zic(i1)
            enddo
         endif
 
c        assign colors according to volume
c        gt vol_test = 1, eq vol_test=2, lt vol_test=3
         if(icolor.ne.0) then
           if(xtetvol.gt.vol_test) then
              itetclr(it) = 1
           elseif (xtetvol.lt.vol_test) then
              itetclr(it) = 3
           else
              itetclr(it) = 2
           endif
         endif
 
c.....END loop through each element
      enddo
 
      write(logmess,'(i10,a,e14.6)')ntotal,' elements le: ',vol_test
      call writloga('default',1,logmess,1,ierrw)
 
 
C
c      do it=1,nelements
c
c   Permutations of node order result in negative volume for
c   j = 2 and 4
c
c             i1=itet1(itetoff(it)+1)
c             i2=itet1(itetoff(it)+2)
c             i3=itet1(itetoff(it)+3)
c             i4=itet1(itetoff(it)+4)
c         dx= ((yic(i2)-yic(i3))*(zic(i4)-zic(i3))
c     1       -(yic(i4)-yic(i3))*(zic(i2)-zic(i3)))
c         dy=-((xic(i2)-xic(i3))*(zic(i4)-zic(i3))
c     1       -(xic(i4)-xic(i3))*(zic(i2)-zic(i3)))
c         dz= ((xic(i2)-xic(i3))*(yic(i4)-yic(i3))
c     1       -(xic(i4)-xic(i3))*(yic(i2)-yic(i3)))
C
c         xtetvol=
c     1     -((xic(i3)-xic(i1))*dx
c     2     + (yic(i3)-yic(i1))*dy
c     3     + (zic(i3)-zic(i1))*dz)/6.0d+00
c
c         call volume_tet_voronoi(xic(i1),yic(i1),zic(i1),
c     1                           xic(i2),yic(i2),zic(i2),
c     2                           xic(i3),yic(i3),zic(i3),
c     3                           xic(i4),yic(i4),zic(i4),
c     4                           voltet_vor,voledge_vor)
c         xvol_edge = 0.0
c         xvol_vor  = 0.0
c         do i = 1,4
c           xvol_vor = voltet_vor(i) + xvol_vor
c         enddo
c         write(6,810)(voltet_vor(i),i=1,4)
c         do i = 1,6
c           xvol_edge = voledge_vor(i) + xvol_edge
c         enddo
c         write(6,810)(voledge_vor(i),i=1,6)
c  810    format(10e20.10)
c         xvol_edge = 2.0*xvol_edge
c         write(6,*)it,xtetvol, xvol_vor, xvol_edge
c         if(xtetvol.le. 0.0) then
c            write(6,*)'Negative Volume Tet ', it,i1,i2,i3,i4, xtetvol
c         endif
c      enddo
c      enddo
      return
      end
 
C
C
      subroutine filter2d(imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C #####################################################################
C
C     PURPOSE - undefined
C             probably had to do with trilayers
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
CPVCS
CPVCS    Original version - Carl Gable - 97
C
c#######################################################################
c
c     FORMAT:
c
c
c     EXAMPLE:
c
C     _____
C    |
C    |
C    |
C    |_____
c
 
c
c#######################################################################
C
      implicit none
 
C
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
c
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet1, itet1)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
 
      real*8 xic(10000000), yic(10000000), zic(10000000)
      integer itet1(10000000), itettyp(10000000), itetoff(10000000)
 
      real*8 dist, epsilon
 
      integer ilen, itype, ier, num_pts_merge
      integer nnodes, nelements, mbndry
      integer nwds, i,j, lenc, icharlnf
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
      character*32 isubname, cmonam
      character*132 logmessage
C
C
      isubname='filter2d'
c
      call cmo_get_name(cmonam,ier)
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,nelements,ilen,itype,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ier)
 
C
      call cmo_get_info('xic',cmonam,ipxic,ilen,itype,ier)
      call cmo_get_info('yic',cmonam,ipyic,ilen,itype,ier)
      call cmo_get_info('zic',cmonam,ipzic,ilen,itype,ier)
      call cmo_get_info('itet',cmonam,ipitet1,ilen,itype,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,ilen,itype,ier)
      call cmo_get_info('itetoff',cmonam,ipitetoff,ilen,itype,ier)
 
      epsilon = xmsgin(3)
      num_pts_merge = 0
      lenc=icharlnf(cmsgin(2))
 
      if ((cmsgin(2)(1:lenc) .eq. 'xy') .or.
     1    (cmsgin(2)(1:lenc) .eq. 'yx'))then
        do i = 1, nnodes
        do j = i+1, nnodes
 
        dist = sqrt( ((xic(i)-xic(j))**2) + ((yic(i)-yic(j))**2) )
 
         if(dist .le. epsilon)then
            if(zic(i) .gt. zic(j))then
              xic(j) = xic(i)
              yic(j) = yic(i)
            else
              xic(i) = xic(j)
              yic(i) = yic(j)
            endif
            num_pts_merge = num_pts_merge + 1
          endif
        enddo
        enddo
      elseif((cmsgin(2)(1:lenc) .eq. 'xz') .or.
     1       (cmsgin(2)(1:lenc) .eq. 'zx'))then
        do i = 1, nnodes
        do j = i+1, nnodes
 
        dist = sqrt( ((xic(i)-xic(j))**2) + ((zic(i)-zic(j))**2) )
 
         if(dist .le. epsilon)then
            if(yic(i) .gt. yic(j))then
              xic(j) = xic(i)
              zic(j) = zic(i)
            else
              xic(i) = xic(j)
              zic(i) = zic(j)
            endif
            num_pts_merge = num_pts_merge + 1
          endif
        enddo
        enddo
      elseif((cmsgin(2)(1:lenc) .eq. 'yz') .or.
     1       (cmsgin(2)(1:lenc) .eq. 'zy'))then
        do i = 1, nnodes
        do j = i+1, nnodes
 
        dist = sqrt( ((zic(i)-zic(j))**2) + ((yic(i)-yic(j))**2) )
 
         if(dist .le. epsilon)then
            if(xic(i) .gt. xic(j))then
              zic(j) = zic(i)
              yic(j) = yic(i)
            else
              zic(i) = zic(j)
              yic(i) = yic(j)
            endif
            num_pts_merge = num_pts_merge + 1
          endif
        enddo
        enddo
      else
c
c     Invalid input for cmsgin(2)
c
      endif
      write(logmessage,'(a,i10,e14.6)')
     1      'filter2d merged: ',num_pts_merge,epsilon
      call writloga('default',0,logmessage,0,ier)
      return
      end
C
C
      subroutine assign_color_normal
     1      (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C #####################################################################
C
C     PURPOSE -
c
c    USE with settets/normal command
C
C     Assign the itetclr array of a triangle mesh a value depending on
C     the normal vector dirction. There are 26 possible direction that
C     corrospond to the 6 faces, 12 edges and 8 corners of a cube.
C
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
CPVCS
CPVCS    Original version - Carl Gable - 97
C
c#######################################################################
c
c     FORMAT:
c
c        ASSIGN_COLOR_NORMAL
c
c#######################################################################
C
      implicit NONE
 
      include "local_element.h"
      include "chydro.h"
c
      character*32  cmo
      character*132  logmess
C
C
      integer icscode
C
C
C
      character*32 isubname
C
      pointer (ipicr1, icr1)
      pointer (ipitp1, itp1)
      integer icr1(10000000), itp1(1000000)
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
      real*8 vector(3)
      real*8 area
      real*8 x(4),y(4),z(4)
      real*8 dot(6)
      integer imsgin(*), msgtyp(*)
      integer nwds, ier
      character*32 cmsgin(nwds)
      real*8 xmsgin(*)
C
      integer i,i1,it,i2,i3,ioff,joff
      integer nnodes, nelements, mbndry
      integer ilen, itype
      integer id, ichange
      integer ierror, ierrw
      real*8 epsilon
C
C#######################################################################
C
      isubname='assign_color_normal'
 
      call cmo_get_name(cmo,ierror)
 
c     use epsilon for identify_dot()
c     to find very small values that should be zero
      call get_epsilon('epsilon',epsilon)
      write(logmess,'(a,1pe15.7)')
     *      "assign_color_normal epsilon for zero: ",epsilon
      call writloga('default',0,logmess,0,ierrw)
 
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,icscode)
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
 
      ichange = 0
      do it = 1,nelements
 
         ioff=itetoff(it)
         joff=jtetoff(it)
            i = 1
            i1=itet1(ioff+1)
            i2=itet1(ioff+2)
            i3=itet1(ioff+3)
            x(1)=xic(i1)
            y(1)=yic(i1)
            z(1)=zic(i1)
            x(2)=xic(i2)
            y(2)=yic(i2)
            z(2)=zic(i2)
            x(3)=xic(i3)
            y(3)=yic(i3)
            z(3)=zic(i3)
 
            call vector_area
     1        (x,y,z,vector(1),vector(2),vector(3),area,itettyp(it))
 
            if (abs(0.0 - abs(area)) .le. epsilon) then
               write(logmess,'(a,i10)')
     *         'WARNING: zero area for element ',it
               call writloga('default',1,logmess,0,ierror)
            endif
 
            call dot_product_6_planes(vector,dot)
 
            call identify_dot(dot,epsilon,id,ierror)
 
c            if (itetclr(it).ne.id ) then
c               write(logmess,'(i10,a,i10,a,i10)')
c     *         it,' changed from ',itetclr(it),' to ',id
c               call writloga('default',1,logmess,1,ierror)
c               write(*,'(3(2x,e14.6))')dot(1),dot(2),dot(3)
c               write(*,'(3(2x,e14.6))')dot(4),dot(5),dot(6)
c            endif
 
            if (ierror.ne.0 .or. id.eq.-1 ) then
               write(logmess,'(a,i10)')
     *         'WARNING: unknown orientation for element ',it
               call writloga('default',0,logmess,0,ierror)
            endif
 
            if (itetclr(it).ne.id) ichange=ichange+1
            itetclr(it) = id
      enddo
c
c
      write(logmess,'(i10,a,i10,a)')
     *ichange,' out of ',nelements,' element colors changed.'
      call writloga('default',0,logmess,1,ierrw)
 
      return
      end
C
C
      subroutine vector_area(x,y,z,xnorm,ynorm,znorm,area,ielmtype)
      implicit none
C
C
C ######################################################################
C
C     Calculate the vector area the normal vector of a triangle.
C
c
      real*8 xnorm(8), ynorm(8), znorm(8), area(8)
      real*8 x(8), y(8), z(8)
      real*8 x12,y12,z12,x13,y13,z13
      integer ielmtype
C
C
C#######################################################################
c
c
c    Calculate the area normal to a triangle.
c
      x12 = x(1)-x(2)
      y12 = y(1)-y(2)
      z12 = z(1)-z(2)
      x13 = x(1)-x(3)
      y13 = y(1)-y(3)
      z13 = z(1)-z(3)
c
c    Take the cross product of xyz12 and xyz13
c
      xnorm(1) = y12*z13 - y13*z12
      ynorm(1) = z12*x13 - z13*x12
      znorm(1) = x12*y13 - x13*y12
 
      area(1) = 0.5d0*sqrt(xnorm(1)**2 + ynorm(1)**2 + znorm(1)**2)
 
      return
      end
C
C
c
      function x3dtime()
 
c     Need this function since time is declared in chydro.h so I could
c     not declare time function as integer.
c
      integer time, x3dtime
      x3dtime = time()
      return
      end
 
C end file tempgable.f
 
 
