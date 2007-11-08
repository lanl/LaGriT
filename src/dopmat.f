*dk,dopmat
      subroutine dopmat(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Calculated doping materials and puts them in a nodal field variable
C           for 2D or 3D grids.  The profiles can be interpolated from
C           a second CMO by finding which elements a node of the first
C           CMO is located in. The elements of the first cmo can later be
C           colored using settets.
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/dopmat.f_a  $
CPVCS    
CPVCS       Rev 1.12   25 May 2000 14:21:50   nnc
CPVCS    Removed duplicate variable declarations.
CPVCS    
CPVCS       Rev 1.11   27 Apr 2000 11:00:08   dcg
CPVCS    fix subroutine so that all are implicit something
CPVCS    
CPVCS       Rev 1.10   Thu Apr 06 08:25:56 2000   dcg
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.9   Fri Jan 22 11:43:32 1999   dcg
CPVCS    move definition of alargenumber to correct subroutine
CPVCS
CPVCS       Rev 1.8   Wed Sep 30 14:02:18 1998   dcg
CPVCS    get memory for iefound
CPVCS
CPVCS       Rev 1.7   Tue Sep 22 14:39:32 1998   dcg
CPVCS    supply missing pointer statement for dopmat
CPVCS
CPVCS       Rev 1.6   Mon Sep 21 16:50:00 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.5   Mon Jun 22 09:43:02 1998   kmb
CPVCS    Added kdtree to integer1 type doping to speed up routine
CPVCS
CPVCS       Rev 1.4   Fri Mar 13 10:17:50 1998   kmb
CPVCS    changed description of intrp_element_mat subroutine to
CPVCS    intrp_element_dop subroutine
CPVCS    D
CPVCS
CPVCS       Rev 1.2   Fri Oct 03 13:24:18 1997   kmb
CPVCS    If a node is on element boundary, takes highest material number
CPVCS    except the material outside mesh.
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:43:26 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Apr 08 12:56:22 1997   kmb
CPVCS    Initial revision.
C
CPVCS
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
      parameter (nplen=1000000,ntlen=1000000)
C
C#######################################################################
C
      integer nwds,imsgin(nwds),msgtype(nwds)
      integer  xfield,matmax,it,mtype, cirtype_len
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*20  cirtype
C
      integer ierror
 
C
C#######################################################################
C
      character*132 logmess
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer isetwd(nplen)
      integer imt1(nplen), itp1(nplen),
     *        icr1(nplen), isn1(nplen)
      dimension xic(nplen), yic(nplen), zic(nplen)
 
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itetclr(ntlen), itettyp(ntlen),
     *        itetoff(ntlen), jtetoff(ntlen)
      integer itet1(ntlen), jtet1(ntlen)
C
      character*32 ich1,ich2,ich3
      character*32 isubname
      character*32 cgeom,  itype
      character*32 prtname, blkname, ioption, cmo, cmotable
      character*32  ctable
C
      pointer(ipmpary, mpary(nplen))
      pointer(ipxfield, xfield(nplen))
C
C
      pointer (jpimt1, imt1_tab)
      pointer (jpitp1, itp1_tab)
      pointer (jpicr1, icr1_tab)
      pointer (jpisn1, isn1_tab)
      pointer (jpxic, xic_tab)
      pointer (jpyic, yic_tab)
      pointer (jpzic, zic_tab)
      pointer (jpitetclr, itetclr_tab)
      pointer (jpitettyp, itettyp_tab)
      pointer (jpitetoff, itetoff_tab)
      pointer (jpjtetoff, jtetoff_tab)
      pointer (jpitet, itet1_tab)
      pointer (jpjtet, jtet1_tab)
      integer imt1_tab(nplen), itp1_tab(nplen),
     *        icr1_tab(nplen), isn1_tab(nplen)
      dimension xic_tab(nplen), yic_tab(nplen), zic_tab(nplen)
      integer itetclr_tab(ntlen), itettyp_tab(ntlen),
     *        itetoff_tab(ntlen), jtetoff_tab(ntlen)
      integer itet1_tab(ntlen), jtet1_tab(ntlen)
C
      pointer(iplist, list(ntlen))
      pointer (ipxintrp, xintrp(nplen))
      pointer (ipyintrp, yintrp(nplen))
      pointer (ipzintrp, zintrp(nplen))
      pointer (ipvalue, value)
        integer value(nplen)
      pointer (ipxtable, xtable(nplen))
C
C#######################################################################
C
C
C
      isubname='dopmat'
C
      ierror = 0
C
      icopy=0
C
C get mesh object
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'DOPING found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
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
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      if(ipointj.eq.0) ipointj=npoints
      if(ipointj.gt.npoints) ipointj=npoints
C
C
C     ******************************************************************
C     DETERMINE DOPING TYPE AND VALIDATE.
C
      itype=cmsgin(2)
       if(itype(1:8).ne.'integer1') then
         ierror=1
         write(logmess,9000) itype
 9000    format('  ERROR - INVALID DOPING TYPE ',a8)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
      len=icharlnf(cmsgin(3))
      blkname=' '
      blkname(1:len)=cmsgin(3)
      call mmgetpr(blkname,cmo,ipxfield,icscode)
C
      len=icharlnf(cmsgin(4))
      ioption(1:len)=cmsgin(4)
      if(ioption(1:3).eq.'set') then
         xfac1= 0.0d+00
         xfac2= 1.0d+00
      elseif(ioption(1:3).eq.'add') then
         xfac1= 1.0d+00
         xfac2=+1.0d+00
      elseif(ioption(1:3).eq.'sub') then
         xfac1= 1.0d+00
         xfac2=-1.0d+00
      endif
C
C
      length=npoints
      call mmgetblk('mpary',isubname,ipmpary,length,1,icscode)
C
C
C
C    set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      mpno=0
C
      if(msgtype(5).eq.1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
C
C     ******************************************************************
C
      if (itype(1:8).eq.'integer1') then
         cmotable=cmsgin(8)(1:icharlnf(cmsgin(8)))
         if(nwds.le.8) then
            ctable=cmsgin(3)(1:icharlnf(cmsgin(3)))
         else
            ctable=cmsgin(9)(1:icharlnf(cmsgin(9)))
         endif
         call cmo_exist(cmotable,ierror)
         if(ierror.ne.0) then
           write(logmess,'(a,a)') 'CMO-table name does not exist: ',
     *                          cmotable
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
         call mmfindbk(ctable,cmotable,ipout,lenout,icscode)
         if(icscode.ne.0) then
           write(logmess,'(a,a,a,a,a)') 'CMO-att name does not exist: ',
     *                   '  cmo= ',cmotable(1:icharlnf(cmotable)),
     *                   '  att= ',ctable(1:icharlnf(ctable))
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
         call cmo_get_info('nnodes',cmotable,
     *                     npoints_tab,length,icmotype,ierror)
         call cmo_get_info('nelements',cmotable,
     *                     ntets_tab,length,icmotype,ierror)
         call cmo_get_info('ndimensions_topo',cmotable,
     *                      nsdtopo_tab,length,icmotype,ierror)
         call cmo_get_info('ndimensions_geom',cmotable,
     *                      nsdgeom_tab,length,icmotype,ierror)
         call cmo_get_info('nodes_per_element',cmotable,
     *                      nen_tab,length,icmotype,ierror)
         call cmo_get_info('faces_per_element',cmotable,
     *                      nef_tab,length,icmotype,ierror)
         call cmo_get_info('imt1',cmotable,jpimt1,ilen,ityp,ierr)
         call cmo_get_info('itp1',cmotable,jpitp1,ilen,ityp,ierr)
         call cmo_get_info('icr1',cmotable,jpicr1,ilen,ityp,ierr)
         call cmo_get_info('isn1',cmotable,jpisn1,ilen,ityp,ierr)
         call cmo_get_info('xic',cmotable,jpxic,ilen,ityp,ierr)
         call cmo_get_info('yic',cmotable,jpyic,ilen,ityp,ierr)
         call cmo_get_info('zic',cmotable,jpzic,ilen,ityp,ierr)
         call cmo_get_info('itetclr',cmotable,
     *                    jpitetclr,ilen,ityp,ierr)
         call cmo_get_info('itettyp',cmotable,
     *                    jpitettyp,ilen,ityp,ierr)
         call cmo_get_info('itetoff',cmotable,
     *                    jpitetoff,ilen,ityp,ierr)
         call cmo_get_info('itetoff',cmotable,
     *                    jpjtetoff,ilen,ityp,ierr)
         call cmo_get_info('itet',cmotable,jpitet,ilen,ityp,ierr)
         call cmo_get_info('jtet',cmotable,jpjtet,ilen,ityp,ierr)
          interp=1
 
 
c     check to see if the attributes use min or max values
      mtype=2
      if(msgtype(10).eq.3) then
      cirtype=cmsgin(10)
      cirtype_len=icharlnf(cirtype)
      if(cirtype(1:cirtype_len).eq.'min')   mtype=1
      if(cirtype(1:cirtype_len).eq.'max')   mtype=2
      if(cirtype(1:cirtype_len).eq.'minp')   mtype=1
      if(cirtype(1:cirtype_len).eq.'maxp')   mtype=2
      endif
 
      if(msgtype(11).eq.3) then
      cirtype=cmsgin(11)
      cirtype_len=icharlnf(cirtype)
      if(cirtype(1:cirtype_len).eq.'use')   then
      write(logmess,'(a)') 'ERROR -DOPING, bad input command-use'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      elseif(cirtype(1:cirtype_len).eq.'create')   then
      write(logmess,'(a)') 'ERROR -DOPING, bad input command-create'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
      endif
 
 
C
          cgeom='-def-'
C
          prtname=cmotable
          blkname=' '
          len=icharlnf(ctable)
          blkname(1:len)=ctable
          call mmgetpr(blkname,prtname,ipxtable,icscode)
          length=mpno
          call mmgetblk('xintrp',isubname,ipxintrp,length,2,icscode)
          call mmgetblk('yintrp',isubname,ipyintrp,length,2,icscode)
          call mmgetblk('zintrp',isubname,ipzintrp,length,2,icscode)
          call mmgetblk('value' ,isubname,ipvalue ,length,1,icscode)
          length=ntets_tab
          call mmgetblk('list',isubname,iplist,length,1,icscode)
          do i=1,ntets_tab
             list(i)=i
          enddo
          listflag=1
          do i=1,mpno
             i1=mpary(i)
             xintrp(i)=xic(i1)
             yintrp(i)=yic(i1)
             zintrp(i)=zic(i1)
             value(i)=0
             xfield(i1)=0
          enddo
 
 
      matmax=0
      do it=1,ntets_tab
c         this is to get the maximum number of materials
         matmax=max(matmax,itetclr_tab(it))
         enddo
         matmax=matmax+1
 
 
          if(nsdtopo_tab.eq.2) then
             if(cgeom(1:5).eq.'-def-') then
C
C        Find a principal direction and do the interpolation in the
C        plane perpendicular to that direction.
C
                x1 = xic(itet1(1+itetoff(1)))
                y1 = yic(itet1(1+itetoff(1)))
                z1 = zic(itet1(1+itetoff(1)))
                x2 = xic(itet1(2+itetoff(1)))
                y2 = yic(itet1(2+itetoff(1)))
                z2 = zic(itet1(2+itetoff(1)))
                x3 = xic(itet1(3+itetoff(1)))
                y3 = yic(itet1(3+itetoff(1)))
                z3 = zic(itet1(3+itetoff(1)))
                xcross = abs( (y3-y1)*(z2-z1) - (z3-z1)*(y2-y1) )
                ycross = abs( (z3-z1)*(x2-x1) - (x3-x1)*(z2-z1) )
                zcross = abs( (x3-x1)*(y2-y1) - (y3-y1)*(x2-x1) )
C
C               If kdir.eq.0, do YZ-plane, else if kdir.eq.1,
C                  do ZX-plane, else do XY-plane.
C
                cgeom = 'yz'
                if ( ycross .gt. xcross ) cgeom = 'zx'
                if ( (zcross .gt. xcross) .and. (zcross .gt. ycross) )
     *                cgeom = 'xy'
             endif
C
 
             call intrp_element_dop(cgeom,
     *                          ipxintrp,ipyintrp,ipzintrp,mpno,
     *                          cmotable,
     *                          ctable,
     *                          ipvalue,
     *                          mtype,
     *                          ipmpary,
     *                          ierror)
C
          elseif(nsdtopo_tab.eq.3) then
            if(cgeom(1:5).eq.'-def-') cgeom='xyz'
            call intrp_element_dop(cgeom,
     *                         ipxintrp,ipyintrp,ipzintrp,mpno,
     *                         cmotable,
     *                         ctable,
     *                         ipvalue,
     *                         mtype,
     *                         ipmpary,
     *                         ierror)
C
          endif
 
          do i=1,mpno
             i1=mpary(i)
             if(value(i1).eq.0)  then
                xfield(i1)=matmax
             else
                 xfield(i1)=value(i)
             endif
          enddo
 
 
 
          call mmrelblk('list'  ,isubname,iplist,icscode)
          call mmrelblk('xintrp',isubname,ipxintrp,icscode)
          call mmrelblk('yintrp',isubname,ipyintrp,icscode)
          call mmrelblk('zintrp',isubname,ipzintrp,icscode)
          call mmrelblk('value' ,isubname,ipvalue,icscode)
          endif
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C
 
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      go to 9999
 9999 continue
 
      call mmrelblk('mpary' ,isubname,ipmpary ,icscode)
C
      return
      end
 
 
 
 
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
*dk,intrp_element_dop
      subroutine intrp_element_dop(corder_value,
     &                         ipxintrp,ipyintrp,ipzintrp,nump,
     &                         cmotab,
     &                         cfield,
     &                         ipvalue,
     &                         mtype,
     *                         ipmpary,
     &                         ierr2)
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE TAKES A SET OF POINTS IN X,Y,Z AND, FOR EACH
C         POINT, FINDS A MATERIAL
C         ON A HEXAHEDRAL GRID IN X,Y,Z.  THE METHOD IS TO
C         FIND WHICH ELEMENT IN LIST THE POINT LIES INSIDE AND THEN
C         GET THE MATERIAL NUMBER OF THAT ELEMENT
C
C         FORMAT:
C
C      INPUT ARGUMENTS -
C
C
C        cmotable - THE NAME OF THE CMO CONTAINING THE TABLE
C
C        ipxintrp - POINTER TO X-COORDINATES FOR INTERPOLATION POINTS
C
C        ipyintrp - POINTER TO Y-COORDINATES FOR INTERPOLATION POINTS
C
C        ipzintrp - POINTER TO Z-COORDINATES FOR INTERPOLATION POINTS
C
C        nump     - NUMBER OF POINTS TO BE INTERPOLATED
C
C        cmotab   - NAME OF THE CMO CONTAINING THE TABLE
C
C        cfield   - NAME OF THE FIELD VALUE TO BE INTERPOLATED
C
C        mtype    - If NODE FALLS ON ELEMENT BOUNDARY, MTYPE=1 GETS
C                   MINIMUM AND MTYPE=2 GETS MAXIMUM
C
C     OUTPUT ARGUMENTS -
C
C        ipvalue  - POINTER OF THE INTERPOLATED FIELD VALUES
C
C        ierr2    - ERROR INDICATOR, RETURNS PLACE IN LIST WHERE THERE
C                   WAS NO BOUNDING TET IN THE LIST FOR THIS POINT.
C                   RETURNS 0 IF NO TPROBLEMS, <0 IF SOME OTHER PROBLEM.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/intrp_element_dop.f_a  $
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:52:18 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Apr 08 12:56:52 1997   kmb
CPVCS    Initial revision.
C
CPVCS    Copied this subroutine from interp_element.f.  Modified
CPVCS     it to get material types.  K.Bower, 1/27/97
CPVCS
CPVCS       Rev 1.11   Mon Jul 29 15:43:18 1996   dcg
CPVCS    split off table_element, volume_element,tet,qud,tri
CPVCS
CPVCS       Rev 1.10   Wed Jul 24 17:32:56 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.9   Wed Jun 19 10:19:04 1996   het
CPVCS    If a ray gets lost, then restart it once by teleporting
CPVCS    close to the element that contains the target point.
CPVCS
CPVCS       Rev 1.8   Tue Apr 30 07:28:16 1996   het
CPVCS    Fix a error in the quad interpolation routine.
CPVCS
CPVCS       Rev 1.7   Tue Apr 02 02:24:06 1996   het
CPVCS    Correct an error in the volume_qud routine.
CPVCS
CPVCS       Rev 1.6   Fri Dec 22 14:13:08 1995   het
CPVCS    Correct a memory management error with ipath
CPVCS
CPVCS       Rev 1.5   12/05/95 08:25:42   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.4   11/16/95 17:02:18   het
CPVCS    Fix an error with the interpolation scheme
CPVCS
CPVCS       Rev 1.3   11/07/95 17:19:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.2   09/11/95 14:42:22   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.1   08/15/95 18:22:10   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.0   07/18/95 08:33:20   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:38   pvcs
CPVCS    Original version.
C
C#######################################################################
      parameter (nplen=1000000,ntlen=1000000)
C
C
      include "local_element.h"
      include "consts.h"
C
      character*32 cmotab,cmo, isubname
      character*1024 cbuff
      pointer (ipiefound,iefound)
      integer linkt(2*ntlen), iefound(ntlen),mtype
C
C
      pointer (ipxic_value, xic_value)
      pointer (ipyic_value, yic_value)
      pointer (ipzic_value, zic_value)
      real*8 xic_value(nplen), yic_value(nplen), zic_value(nplen)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(nplen), itettyp(nplen),
     *        itetoff(nplen), jtetoff(nplen)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(nplen), jtet1(nplen)
 
      pointer(ipmpary, mpary)
      integer mpary(nplen)
 
C
 
C
C
 
      integer  in_element, matmax
 
      data iclrpath / 0 /
C
      data xsmall / 1.0d-06 /
C
C
C######################################################################
C
      integer ierr2
      pointer (ipxintrp, xintrp)
      pointer (ipyintrp, yintrp)
      pointer (ipzintrp, zintrp)
      pointer (ipvalue, value)
      pointer (iplinkt,linkt)
      pointer (ipsbox,sbox)
      real*8 sbox(2,3,2*nplen)
      integer value(nplen)
 
      pointer (ipxic_table, xic_table)
      pointer (ipyic_table, yic_table)
      pointer (ipzic_table, zic_table)
      real*8 xic_table(nplen), yic_table(nplen), zic_table(nplen)
 
 
 
      dimension xintrp(nump),yintrp(nump),zintrp(nump)
      character*(*) corder_value,   cfield
C
      isubname='intrpmat'
C     CHECK FOR VALID ITYPE
C
      ierr2 = 0
C
 
C get info on current mesh object
C get mesh object
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'ERROR -DOPMAT  found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
 
      call cmo_get_info('xic',cmo,
     *                        ipxintrp,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,
     *                        ipyintrp,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,
     *                        ipzintrp,ilen,ityp,ierr)
 
C
C get table mesh object information
      call cmo_get_info('nnodes',cmotab,nnodes,ilen,ityp,ierror)
      call cmo_get_info('nelements',cmotab,ntets_tab,ilen,ityp,ierror)
      call cmo_get_info('faces_per_element',cmotab,nefcmo,
     *                                   ilen,ityp,ierror)
      call cmo_get_info('mbndry',cmotab,mbndry,ilen,ityp,ierror)
C
      call cmo_get_info('xic',cmotab,ipxic_table,ilen,ityp,ierror)
      call cmo_get_info('yic',cmotab,ipyic_table,ilen,ityp,ierror)
      call cmo_get_info('zic',cmotab,ipzic_table,ilen,ityp,ierror)
 
C
C
C     now do the current cmo values
        goto 606
      len1=icharlnf(corder_value)
      if(corder_value(1:2).eq.'xy') then
         ipxic_value=ipxintrp
         ipyic_value=ipyintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3) then
            ipzic_value=ipzintrp
         endif
      elseif(corder_value(1:2).eq.'xz') then
         ipxic_value=ipxintrp
         ipyic_value=ipzintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'y') then
            ipzic_value=ipyintrp
         endif
      elseif(corder_value(1:2).eq.'yx') then
         ipxic_value=ipyintrp
         ipyic_value=ipxintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'z') then
            ipzic_value=ipzintrp
         endif
      elseif(corder_value(1:2).eq.'yz') then
         ipxic_value=ipyintrp
         ipyic_value=ipzintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'x') then
            ipzic_value=ipxintrp
         endif
      elseif(corder_value(1:2).eq.'zx') then
         ipxic_value=ipzintrp
         ipyic_value=ipxintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'y') then
            ipzic_value=ipyintrp
         endif
      elseif(corder_value(1:2).eq.'zy') then
         ipxic_value=ipzintrp
         ipyic_value=ipyintrp
         if(len1.eq.2) then
            length=nump
            call mmgetblk('vtemp',isubname,ipzic_value,length,2,icscode)
            do i=1,length
               i1=mpary(i)
               zic_value(i1)=0.0d+00
            enddo
         elseif(len1.eq.3.and.corder_value(3:3).eq.'x') then
            ipzic_value=ipxintrp
         endif
      endif
606   ipxic_value=ipxintrp
      ipyic_value=ipyintrp
      ipzic_value=ipzintrp
 
C
      call cmo_get_info('itetclr',cmotab,
     *                        ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmotab,
     *                        ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmotab,
     *                        ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmotab,
     *                        ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmotab,ipitet,ilen,ityp,ierr)
c
      call cmo_get_info('jtet',cmotab,ipjtet,ilen,ityp,ierr)
 
 
C
       matmax=1
      do it=1,ntets_tab
c         this is to get the maximum number of materials
         matmax=max(matmax,itetclr(it))
      enddo
         matmax=matmax+1
 
C   put in the kdtree built for the cmo_tab
       len=icharlnf(cmotab)
       cbuff = 'cmo select '//cmotab(1:len)//' ;  finish'
       call dotaskx3d(cbuff,ierror)
 
       cbuff = 'cmo kdtree build; finish'
       call dotaskx3d(cbuff,ierror)
       call cmo_get_info('linkt',cmotab,
     *                        iplinkt,ilen,ityp,ierr)
       call cmo_get_info('sbox',cmotab,
     *                        ipsbox,ilen,ityp,ierr)
 
 
 
 
C
      do ip=1,nump
         i1=mpary(ip)
         xa=xic_value(i1)
         ya=yic_value(i1)
         za=zic_value(i1)
 
C
      value(ip)=0
      if(mtype.eq.1)   value(ip)=matmax
 200  continue
c  get temp work space
c
       length=ntets_tab
       call mmfindbk('iefound',isubname,ipiefound,lneout,icscode)
       if(icscode.ne.0)
     *    call mmgetblk('iefound',isubname,ipiefound,length,1,icscode)
       call get_epsilon('epsilonl',eps)
       call retrieve_within_eps(xa,ya,za,linkt,sbox,
     *           eps,nefound,iefound,ierr)
 
 
       do  jj=1,nefound
         itnext=iefound(jj)
         in_element=-1
         if(itettyp(itnext).eq.ifelmtri) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                        xa,ya,za,
     *                        in_element)
         elseif(itettyp(itnext).eq.ifelmqud) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            call inside_quad2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                         xa,ya,za,
     *                         in_element)
         elseif(itettyp(itnext).eq.ifelmtet) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            call inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      xa,ya,za,
     *                      in_element)
 
         elseif(itettyp(itnext).eq.ifelmhex) then
            i1=itet1(itetoff(itnext)+1)
            i2=itet1(itetoff(itnext)+2)
            i3=itet1(itetoff(itnext)+3)
            i4=itet1(itetoff(itnext)+4)
            i5=itet1(itetoff(itnext)+5)
            i6=itet1(itetoff(itnext)+6)
            i7=itet1(itetoff(itnext)+7)
            i8=itet1(itetoff(itnext)+8)
            x1=xic_table(i1)
            y1=yic_table(i1)
            z1=zic_table(i1)
            x2=xic_table(i2)
            y2=yic_table(i2)
            z2=zic_table(i2)
            x3=xic_table(i3)
            y3=yic_table(i3)
            z3=zic_table(i3)
            x4=xic_table(i4)
            y4=yic_table(i4)
            z4=zic_table(i4)
            x5=xic_table(i5)
            y5=yic_table(i5)
            z5=zic_table(i5)
            x6=xic_table(i6)
            y6=yic_table(i6)
            z6=zic_table(i6)
            x7=xic_table(i7)
            y7=yic_table(i7)
            z7=zic_table(i7)
            x8=xic_table(i8)
            y8=yic_table(i8)
            z8=zic_table(i8)
            call inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      xa,ya,za,
     *                      in_element)
         endif
         if(in_element.ge.0) then
             if(mtype.eq.1) then
                 value(ip)=min(value(ip), itetclr(itnext))
             else
                 value(ip)=max(value(ip), itetclr(itnext))
             endif
         endif
      enddo
C
      enddo
 
      cbuff = 'cmo kdtree release; finish'
      call dotaskx3d(cbuff,ierror)
 
      len=icharlnf(cmo)
      cbuff = 'cmo select '//cmo(1:len)//' ;  finish'
      call dotaskx3d(cbuff,ierror)
 
      goto 9999
 9999 continue
 
      call mmrelprt(isubname,icscode)
      return
      end
 
 
*dk,dopinteger
      subroutine dopinteger(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C      Take a two grids and dope (interpolate) the attributes
C      of a one grid (cmo_tab) onto another grid (cmo current).
C      This routine is called when profile is integer2 and used
C      the nodal values of the attribute (Voronoi cell based)
C      when doping.
C
C
C      the command is 'doping/profile/field/set|add|sub/ ifirst,ilast, istride/
C                  cmo_table/attribute_table/function/mapset
C
C
C                where field is the attribute name of the existing cmo and
C                cmo_table_name and attribute_table_name are the cmo
C                and attribute names of the cmo that already contains the
C                desired attribute.
C
C                Mapset is an indicator.  If mapset is 'create' then the mapping
C                of nodal pairs (one from cmo_table_name and one from the current
C                cmo) are put into a newly created attribute file called idop.
C                If mapset is 'use' then nodal pairs are read from an existing
C                attribute file n the current cmo called idop.
C
C                Operation can be max,min,maxp,minp where max sets nodes on
C                conflicting boundaries to the larger of the attribute values.
C                Maxp sets nodes on conflicting boundaries to the larger of the
C                attribute values AND sets nodes outside the source volume to
C                the maximum material plus one.
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C $Log:   /pvcs.config/t3d/src/dopint.f_a  $
CPVCS
CPVCS       Rev 1.1   Fri Jun 20 16:01:36 1997   dcg
CPVCS    separate out kdtree0 and nearestpoint0
C
C
C ######################################################################
C
      implicit none
      integer nplen,ntlen
      real*8 xs,ys, zs, xq,yq,zq
      parameter (nplen=1000000,ntlen=1000000)
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds), matmax, ierror, ii,jj
      integer isetwd(nplen), iff,mtfound, length,itp1(nplen),
     *        iptype, cirtype_len,ilen,ityp,ierr
C
       integer icopy,npoints,icmotype,
     * ntets,nsdgeom,ipointi,ipointj,len,
     * ipt1,ipt2,ipt3,jnode,inode_tab,nsdgeom_tab,nsdtopo_tab,
     *  ntets_tab,npoints_tab,ierrw,icscode,mpno,mtype
 
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*3 cmat
      character*20 cirtype
      character*1024  cbuff
C
C
C#######################################################################
C
      character*132 logmess
C
      pointer (ipisetwd, isetwd)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(nplen), yic(nplen), zic(nplen)
 
      character*32 ich1,ich2,ich3
      character*32 isubname
      character*32  blkname, cmo, cmotable
      character*32  ctable, ctable_tab
C
      pointer(ipmpary, mpary(nplen))
      pointer(ipifield, ifield(nplen))
      pointer(ipifield_tab, ifield_tab(nplen))
      integer mpary,ifield,ifield_tab
C
C
      pointer (jpxic, xic_tab)
      pointer (jpyic, yic_tab)
      pointer (jpzic, zic_tab)
      real*8 xic_tab(nplen), yic_tab(nplen), zic_tab(nplen)
C
      pointer (ipivalue, ivalue(nplen))
      pointer (ipsbox, sbox(nplen))
      pointer (iplinkt, linkt(nplen))
      pointer (ipitfound, itfound(nplen))
      pointer (ipidop,idop(nplen))
      integer ivalue,linkt,itfound,idop
      real*8 sbox,eps

      integer icharlnf
      real*8 alargenumber
      data alargenumber/1.d+99/
C
C#######################################################################
C
C
C
      isubname='dopinteger'
C
      ierror = 0
C
      icopy=0
C
C get mesh object
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'ERROR -DOPINTEGER found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
      call cmo_get_intinfo('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_intinfo('ndimensions_geom',cmo,
     *                      nsdgeom,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
c
      call cmo_get_intinfo('ipointi',cmo,ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_intinfo('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      if(ipointj.eq.0) ipointj=npoints
      if(ipointj.gt.npoints) ipointj=npoints
C
C
C     ******************************************************************
C     DETERMINE DOPING TYPE AND VALIDATE.
C
 
      ctable=cmsgin(3)(1:icharlnf(cmsgin(3)))
      len=icharlnf(cmsgin(3))
      blkname=' '
      blkname=ctable
      call cmo_get_info(blkname,cmo,ipifield,ilen,ityp,ierr)
       if(icscode.ne.0) then
           write(logmess,'(a,a,a,a,a)') 'CMO-att name does not exist: ',
     *                   '  cmo= ',cmo(1:icharlnf(cmo)),
     *                   '  att= ',ctable(1:icharlnf(ctable))
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
 
 
C
C
C
      length=npoints
      call mmgetblk('mpary',isubname,ipmpary,length,1,icscode)
C
C
C
C    set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      mpno=0
C
      if(msgtype(5).eq.1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
 
c     check to see if the nodal pairs are saved or used
      iptype=0
      if(nwds.ge.11) then
      cirtype=cmsgin(11)
      cirtype_len=icharlnf(cirtype)
      if(cirtype(1:cirtype_len).eq.'create')iptype=1
      if(cirtype(1:cirtype_len).eq.'use')   iptype=2
      endif
 
      cmotable=cmsgin(8)(1:icharlnf(cmsgin(8)))
 
         if(nwds.le.8) then
            ctable_tab=cmsgin(3)(1:icharlnf(cmsgin(3)))
            len=icharlnf(cmsgin(3))
         else
            ctable_tab=cmsgin(9)(1:icharlnf(cmsgin(9)))
            len=icharlnf(cmsgin(9))
         endif
         call cmo_exist(cmotable,ierror)
         if(ierror.ne.0) then
           write(logmess,'(a,a)') 'CMO-table name does not exist: ',
     *                          cmotable
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
 
      blkname=' '
      blkname=ctable_tab
      call cmo_get_info(blkname,cmotable,ipifield_tab,ilen,ityp,icscode)
      if(icscode.ne.0) then
      write(logmess,'(a,a,a,a,a)') 'CMO-att name does not exist: ',
     *                   '  cmo= ',cmotable(1:icharlnf(cmotable)),
     *                   '  att= ',ctable_tab(1:icharlnf(ctable_tab))
 
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
 
c     check to see if the attributes use min or max values
      mtype=2
      if(nwds.ge.10) then
      cirtype=cmsgin(10)
      cirtype_len=icharlnf(cirtype)
      if(cirtype(1:cirtype_len).eq.'min')    mtype=1
      if(cirtype(1:cirtype_len).eq.'max')    mtype=2
      if(cirtype(1:cirtype_len).eq.'minp')   mtype=3
      if(cirtype(1:cirtype_len).eq.'maxp')   mtype=4
      endif
 
 
c
c     if iptype=1 then create attribute array
      if(iptype.eq.1) then
       cbuff = 'cmo addatt//idop/vint/scalar/nnodes;finish'
       call dotaskx3d(cbuff,ierror)
       call cmo_get_info('idop',cmo,ipidop,ilen,ityp,ierr)
          do ii=1,npoints
               idop(ii)=0
          enddo
      elseif(iptype.eq.2) then
       call cmo_get_info('idop',cmo,ipidop,ilen,ityp,icscode)
       if(icscode.ne.0) then
      write(logmess,'(a,a,a,a,a)') 'CMO-att does not exist: ',
     *                   '  cmo= ',cmo(1:icharlnf(cmo)),
     *                   '  att= idop '
 
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
        endif
      endif
 
 
 
C
C     ******************************************************************
C
         call cmo_get_info('nnodes',cmotable,
     *                     npoints_tab,length,icmotype,ierror)
         call cmo_get_info('nelements',cmotable,
     *                     ntets_tab,length,icmotype,ierror)
         call cmo_get_info('ndimensions_topo',cmotable,
     *                      nsdtopo_tab,length,icmotype,ierror)
         call cmo_get_info('ndimensions_geom',cmotable,
     *                      nsdgeom_tab,length,icmotype,ierror)
         call cmo_get_info('xic',cmotable,jpxic,ilen,ityp,ierr)
         call cmo_get_info('yic',cmotable,jpyic,ilen,ityp,ierr)
         call cmo_get_info('zic',cmotable,jpzic,ilen,ityp,ierr)
C
C
C        check to see if the two cmo's have the same geometric dimension
         if( nsdgeom .ne. nsdgeom_tab) then
         write(logmess,'(a)')"ERROR -two cmo's need same dimension"
         call writloga('default',0,logmess,0,ierrw)
 
         stop
         endif
 
 
          len=icharlnf(ctable)
          length=npoints
          call mmgetblk('ivalue',isubname,ipivalue,length,1,icscode)
 
 
      do ii=1,npoints
        ivalue(ii)=0
        if((mtype.eq.1).or.(mtype.eq.3)) ivalue(ii)=999
      enddo
 
      matmax=1
      do ii=1,npoints_tab
        matmax=max(matmax, ifield_tab(ii))
      enddo
        matmax=matmax+1
 
 
 
C
C      if iptype=2, use previous idop array
       if(iptype.ne.2) then
C .... Use A. kuprat's kdtree routines here
 
 
      length=5*npoints_tab
      call mmgetblk('itfound',isubname,ipitfound,length,1,icscode)
      length=12*npoints_tab
      call mmgetblk('sbox',isubname,ipsbox,length,2,icscode)
      length=2*npoints_tab
      call mmgetblk('linkt',isubname,iplinkt,length,1,icscode)
 
 
      call kdtree0(xic_tab,yic_tab,zic_tab,npoints_tab,linkt,sbox,ierr)
 
c
         eps=-1.
         mtfound=0
 
         xs=alargenumber
         ys=alargenumber
         zs=0.
         if(nsdgeom_tab.eq.3) zs=alargenumber
 
 
        do ii=1,mpno
            ifield(mpary(ii))=0
            xq=xic(mpary(ii))
            yq=yic(mpary(ii))
            zq=0.
            if(nsdgeom.eq.3) zq=zic(mpary(ii))
 
           call nearestpoint0(xq,yq,zq,xs,ys,zs,linkt,sbox,eps,
     $    npoints_tab,mtfound,itfound,ierr)
 
 
 
 
 
            do iff=1,mtfound
c        .........inode_tab is in the source mesh
                inode_tab=itfound(iff)
c       ......... jnode is in the current mesh
                jnode=mpary(ii)
 
 
             if(ivalue(jnode).eq.matmax)  then
                ivalue(jnode)=ifield_tab(inode_tab)
             else
               if(ifield_tab(inode_tab).eq.matmax) then
                 if(ivalue(jnode).eq.0) ivalue(jnode)=matmax
               else
                 if((mtype.eq.1).or.(mtype.eq.3)) then
                 ivalue(jnode)=min(ivalue(jnode),ifield_tab(inode_tab))
                 else
                 ivalue(jnode)=max(ivalue(jnode),ifield_tab(inode_tab))
                 endif
               endif
             endif
 
 
 
 
                if(iptype.eq.1) then
                  if(ivalue(jnode).eq.ifield_tab(inode_tab)) then
                       idop(jnode)=inode_tab
                  endif
                endif
 
            enddo
         enddo
 
c    if iptype=2 then
       else
          do iff=1,npoints
c        .........inode_tab is in the source mesh
                inode_tab=idop(iff)
c       ......... jnode is in the current mesh
                if(inode_tab.ne.0) then
                jnode=iff
                       ivalue(jnode)=ifield_tab(inode_tab)
                endif
 
 
          enddo
      endif
 
        do ii=1,mpno
             jj=mpary(ii)
             ifield(jj)=ivalue(jj)
         enddo
 
 
c     if mtype.>=3 then set nodes outside source boundary to matmax
      if(mtype.ge.3) then
       len=icharlnf(cmotable)
       cbuff = '   cmo copy cmoxxx   '//cmotable(1:len)//' ;  finish'
       call dotaskx3d(cbuff,ierror)
 
       cbuff = '   cmo select cmoxxx;   finish'
       call dotaskx3d(cbuff,ierror)
 
       cbuff = '   cmo/setatt// imt1/ 1 0 0/ 1;   finish'
       call dotaskx3d(cbuff,ierror)
 
       cbuff = '   extract intrface -all- 1 0 0 cmoxx2 cmoxxx;   finish'
       call dotaskx3d(cbuff,ierror)
 
       len = icharlnf(cmo)
       cbuff = '   cmo select '//cmo(1:len)//'  ;finish'
       call dotaskx3d(cbuff,ierror)
 
 
       cbuff = '   surface sxbound reflect sheet cmoxx2;  finish'
       call dotaskx3d(cbuff,ierror)
 
 
       cbuff = '   region rxbound gt sxbound;   finish'
       call dotaskx3d(cbuff,ierror)
 
       cbuff = '   pset pxbound region rxbound 1 0 0;   finish'
       call dotaskx3d(cbuff,ierror)
 
       write(cmat,571) matmax
         len=icharlnf(cmat)
       cbuff = '   cmo/setatt// imt1 pset,get,pxbound/'//cmat(1:len)//
     *      ';   finish'
       call dotaskx3d(cbuff,ierror)
      endif
571   format(i3.3)
 
 
C
 
 
 
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C
 
C
      go to 9999
 9999 continue
      call mmrelblk('mpary' ,isubname,ipmpary ,icscode)
      call mmrelblk('ivalue',isubname,ipivalue,icscode)
      call mmrelblk('sbox' ,isubname,ipsbox,icscode)
      call mmrelblk('linkt' ,isubname,iplinkt,icscode)
      call mmrelblk('itfound' ,isubname,ipitfound,icscode)
C
      return
      end
