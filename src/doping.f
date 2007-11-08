      subroutine doping(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Calculated doping profiles and puts them in a field variable
C           for 2D or 3D grids.  The profiles can be interpolated from
C           a second CMO.
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
C
C     COMMAND FORMAT -
C
C        doping/profile/field/type/ifirst,ilast,istride/cmo_table/att_table/
C                  function/cmo_geometry/table_geometry
C
C            OR
C        doping/profile/field/type/ifirst,ilast,istride/cmo_table/att_table/
C                  function/mapset
C    
C           where
C               profile - constant, gaussian, table, integer1, integer2.
C                         Integer1 is used for doping nodal integer values 
C                         based on element material types.  Integer2 is
C                         used for doping nodal integer values using the
C                         nodal table values (uses Voronoi cells).
C
C               field - the attribute name of the current cmo.
C
C               type - set, add, or sub.
C
C               att_table - the attribute name of the source cmo.
C
C               function - linear, log, asinh.  However, if integer2 is used, the
C                          function may be maxp or minp to get outer node values set to
C                          a value based on the voronoi mesh; or the function
C                          may be min or max to get outer node values based on the 
C                          largest source material plus one.
C
C               mapset - used only with integer2.  Creates or uses an interger array,
C                        idop which contains the source nodes corresponding to the
C                        nodes of the sink.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/doping_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.0   27 Jan 2000 12:54:14   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.24   Wed Feb 04 09:24:38 1998   kmb
CPVCS    changed doping command to include doping of integer types as
CPVCS    well as reals.  The new doping command will dope nodal integers
CPVCS    based on voronoi cells and on element material types.
CPVCS    
CPVCS       Rev 1.23   Mon Apr 14 16:43:24 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.22   Mon Apr 07 14:53:44 1997   het
CPVCS    Add the element to element interpoation algorithm.
CPVCS    
CPVCS       Rev 1.21   Tue Nov 12 09:07:26 1996   kuprat
CPVCS    We change the 'user' option so that the FADPT function is
CPVCS    called once with a vector of x,y,z positions, and we
CPVCS    now also provide a vector matvec of material types.
CPVCS    
CPVCS       Rev 1.20   Thu Oct 31 23:35:18 1996   kuprat
CPVCS    We adjust the call to fadpt in the 'user' doping option.
CPVCS    An extra argument is now passed, matching the change in
CPVCS    the argument list of fadpt.  However, this is just a
CPVCS    bandaid fix and the argument needs to be treated properly
CPVCS    for the case where the function has different values at
CPVCS    different child points with the SAME parent point.
CPVCS    
CPVCS       Rev 1.19   Tue May 28 21:24:56 1996   kuprat
CPVCS    Added 'user' option to dope with subroutine 'fadpt'.
CPVCS    
CPVCS       Rev 1.18   Fri Feb 02 14:42:46 1996   dcg
CPVCS    remove references to icn1 and int1
CPVCS
CPVCS       Rev 1.17   Fri Feb 02 14:20:58 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.16   11/16/95 15:21:36   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.15   11/07/95 17:16:10   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.14   09/29/95 09:13:38   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.13   09/19/95 13:11:10   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.12   09/12/95 14:27:52   dcg
CPVCS    replace references to cgeom_in with cgeom_tab
CPVCS
CPVCS       Rev 1.11   09/11/95 14:46:02   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.10   08/15/95 18:22:18   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.9   07/14/95 23:31:22   het
CPVCS    Correct an error with the doping table option
CPVCS
CPVCS       Rev 1.8   07/14/95 10:13:48   het
CPVCS    Add new doping interpolate commands
CPVCS
CPVCS       Rev 1.7   06/13/95 09:01:38   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.6   05/02/95 10:16:04   jxf
CPVCS    Changed 2D stuff
CPVCS
CPVCS       Rev 1.5   03/14/95 08:57:28   het
CPVCS    Replace the table file option with a cmo name option
CPVCS
CPVCS       Rev 1.4   03/10/95 17:13:54   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.3   02/18/95 06:56:28   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.2   01/04/95 22:02:02   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/06/94 19:07:10   het
CPVCS    Add the "call cmo_get_name" to return the current mesh object name.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:38   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
C
      integer nplen,ntlen
      parameter (nplen=1000000,ntlen=1000000)
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character*132 logmess
      real*8 xdoping,xl,dz,dx,dy,xlatdiff,time,xcon,
     * x,y,z,xcross,ycross,zcross,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     * xavg,yavg,zavg,xfac1,xfac2,xstd
      integer iout,listflag,it,i1,i,nvalue,length_value,
     *  len1,interp,nef_tab,nsdgeom_tab,nsdtopo_tab,ntets_tab,
     *  npoints_tab,lenout,ipt1,ipt2,ipt3,mpno,len,icharlnf,
     *  ipointi,ipointj,icscode,ierr,ityp,ilen,ntets,icmotype,
     *  length,icopy,ierrw,npoints,nen_tab
      pointer(ipout,out)
      real*8 out(*)
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
      real*8 xic(nplen), yic(nplen), zic(nplen)
 
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
      character*32 cgeom, cgeom_tab, itype
      character*32 prtname, blkname, ioption, cmo, cmotable
      character*32 cinterp, ctable,cout
C
      pointer(ipmpary, mpary(nplen))
      pointer(ipxfield, xfield(nplen))
      integer mpary
      real*8 xfield
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
      real*8 xic_tab(nplen), yic_tab(nplen), zic_tab(nplen)
      integer itetclr_tab(ntlen), itettyp_tab(ntlen),
     *        itetoff_tab(ntlen), jtetoff_tab(ntlen)
      integer itet1_tab(ntlen), jtet1_tab(ntlen)
C
      pointer(iplist, list(ntlen))
      pointer (ipxintrp, xintrp(nplen))
      pointer (ipyintrp, yintrp(nplen))
      pointer (ipzintrp, zintrp(nplen))
      pointer (ipmatvec, matvec(nplen))
      pointer (ipyvalue, yvalue(nplen))
      pointer (ipxvalue, xvalue(nplen))
      real*8 xintrp,yintrp,zintrp,yvalue,xvalue
      integer matvec,list
C
C#######################################################################
C
C
C     ******************************************************************
C     GET THE MESH OBJECT NAME
C     
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'ERROR -DOPING found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      endif
C
C
C     ******************************************************************
C     WE NEED TO MAKE SURE THAT THE NODE AND ELEMENT CMO ATTRIBUTES ARE
C        THE CORRECT SIZE BECAUSE SOME OF THE INPUT OPTIONS (LIKE
C        "TABULAR") DEPEND ON THIS.
C
      call cmo_newlen(cmo,ierror)
C
C     ******************************************************************
C
      isubname='doping'

C
      ierror = 0
C
      icopy=0
C
C get mesh object
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'ERROR -DOPING found bad mesh object'
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
      if (icscode .ne. 0) call x3d_error(isubname,'ipointi')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'ipointj')
C
      if(ipointj.eq.0) ipointj=npoints
      if(ipointj.gt.npoints) ipointj=npoints
C
C
C     ******************************************************************
C     DETERMINE DOPING TYPE AND VALIDATE.
C
      itype=cmsgin(2)
      if (itype(1:8).ne.'gaussian' .and.
     *    itype(1:8).ne.'constant' .and.
     *    itype(1:7).ne.'tabular'.and.
     *    itype(1:5).ne.'table' .and.
     *    itype(1:4).ne.'user'  .and.
     *    itype(1:8).ne.'integer1' .and.
     *    itype(1:8).ne.'integer2') then 
         ierror=1
         write(logmess,9000) itype
 9000    format('  ERROR - INVALID DOPING TYPE ',a8)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
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
      if (itype(1:8).ne.'integer1') then
            if (itype(1:8).ne.'integer2') then
      length=npoints
      call mmgetblk('mpary',isubname,ipmpary,length,1,icscode)
      len=icharlnf(cmsgin(3))
      blkname=' '
      blkname(1:len)=cmsgin(3)
      call mmgetpr(blkname,cmo,ipxfield,icscode)

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
      endif
      endif
C
C     ******************************************************************
C
      if (itype(1:7).eq.'tabular'.or.itype(1:5).eq.'table') then
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
          cinterp=cmsgin(10)
          interp=1
          if(cinterp(1:6).eq.'linear') interp=1
          if(cinterp(1:3).eq.'log') interp=2
          if(cinterp(1:5).eq.'asinh') interp=3
C
          if(msgtype(11).eq.3) then
             len1=icharlnf(cmsgin(11))
             if(cmsgin(11)(1:len1).eq.'xy'  .or.
     *          cmsgin(11)(1:len1).eq.'xz'  .or.
     *          cmsgin(11)(1:len1).eq.'yx'  .or.
     *          cmsgin(11)(1:len1).eq.'yz'  .or.
     *          cmsgin(11)(1:len1).eq.'zx'  .or.
     *          cmsgin(11)(1:len1).eq.'zy'  .or.
     *          cmsgin(11)(1:len1).eq.'xyz' .or.
     *          cmsgin(11)(1:len1).eq.'xzy' .or.
     *          cmsgin(11)(1:len1).eq.'yxz' .or.
     *          cmsgin(11)(1:len1).eq.'yzx' .or.
     *          cmsgin(11)(1:len1).eq.'zxy' .or.
     *          cmsgin(11)(1:len1).eq.'zyx') then
                cgeom=cmsgin(11)(1:len1)
             else
                cgeom='-def-'
             endif
          else
             cgeom='-def-'      
          endif
C
          if(msgtype(12).eq.3) then
             len1=icharlnf(cmsgin(12))
             if(cmsgin(12)(1:len1).eq.'xy'  .or.
     *          cmsgin(12)(1:len1).eq.'xz'  .or.
     *          cmsgin(12)(1:len1).eq.'yx'  .or.
     *          cmsgin(12)(1:len1).eq.'yz'  .or.
     *          cmsgin(12)(1:len1).eq.'zx'  .or.
     *          cmsgin(12)(1:len1).eq.'zy'  .or.
     *          cmsgin(12)(1:len1).eq.'xyz' .or.
     *          cmsgin(12)(1:len1).eq.'xzy' .or.
     *          cmsgin(12)(1:len1).eq.'yxz' .or.
     *          cmsgin(12)(1:len1).eq.'yzx' .or.
     *          cmsgin(12)(1:len1).eq.'zxy' .or.
     *          cmsgin(12)(1:len1).eq.'zyx') then
                cgeom_tab=cmsgin(12)(1:len1)
             else
                cgeom_tab='-def-'
             endif
          else
             cgeom_tab='-def-'
          endif
C
          prtname=cmo
          blkname=' '
          len=icharlnf(ctable)
          blkname(1:len)=ctable
          call mmgetpr(blkname,prtname,ipxvalue,icscode)
          call mmgetlen(ipxvalue,length_value,icscode)
          if(length_value.eq.npoints) then
             nvalue=mpno
             length=mpno
             call mmgetblk('xintrp',isubname,ipxintrp,length,2,icscode)
             call mmgetblk('yintrp',isubname,ipyintrp,length,2,icscode)
             call mmgetblk('zintrp',isubname,ipzintrp,length,2,icscode)
             call mmgetblk('yvalue',isubname,ipyvalue,length,2,icscode)
             do i=1,mpno
                i1=mpary(i)
                xintrp(i)=xic(i1)
                yintrp(i)=yic(i1)
                zintrp(i)=zic(i1)
                yvalue(i)=0.0d+00
             enddo
          elseif(length_value.eq.ntets) then
             nvalue=ntets
             length=ntets
             call mmgetblk('xintrp',isubname,ipxintrp,length,2,icscode)
             call mmgetblk('yintrp',isubname,ipyintrp,length,2,icscode)
             call mmgetblk('zintrp',isubname,ipzintrp,length,2,icscode)
             call mmgetblk('yvalue',isubname,ipyvalue,length,2,icscode)
             do it=1,ntets
                xavg=0.0
                yavg=0.0
                zavg=0.0
                do i=1,nelmnen(itettyp(it))
                   i1=itet1(itetoff(it)+i)
                   xavg=xavg+xic(i1)
                   yavg=yavg+yic(i1)
                   zavg=zavg+zic(i1)
                enddo
                xintrp(it)=xavg/nelmnen(itettyp(it))
                yintrp(it)=yavg/nelmnen(itettyp(it))
                zintrp(it)=zavg/nelmnen(itettyp(it))
                yvalue(it)=0.0d+00
             enddo
          endif
C
          length=ntets_tab
          call mmgetblk('list',isubname,iplist,length,2,icscode)
          do i=1,ntets_tab
             list(i)=i
          enddo
          listflag=1
C
          if(nsdtopo_tab.eq.2) then
             if(cgeom(1:5).eq.'-def-') then
C
C               Find a principal direction and do the interpolation in the
C                  plane perpendicular to that direction.
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
             if(cgeom_tab(1:5).eq.'-def-') then
C
C               Find a principal direction and do the interpolation in the
C                  plane perpendicular to that direction.
C
                x1 = xic_tab(itet1_tab(1+itetoff_tab(1)))
                y1 = yic_tab(itet1_tab(1+itetoff_tab(1)))
                z1 = zic_tab(itet1_tab(1+itetoff_tab(1)))
                x2 = xic_tab(itet1_tab(2+itetoff_tab(1)))
                y2 = yic_tab(itet1_tab(2+itetoff_tab(1)))
                z2 = zic_tab(itet1_tab(2+itetoff_tab(1)))
                x3 = xic_tab(itet1_tab(3+itetoff_tab(1)))
                y3 = yic_tab(itet1_tab(3+itetoff_tab(1)))
                z3 = zic_tab(itet1_tab(3+itetoff_tab(1)))
                xcross = abs( (y3-y1)*(z2-z1) - (z3-z1)*(y2-y1) )
                ycross = abs( (z3-z1)*(x2-x1) - (x3-x1)*(z2-z1) )
                zcross = abs( (x3-x1)*(y2-y1) - (y3-y1)*(x2-x1) )
C
C               If kdir.eq.0, do YZ-plane, else if kdir.eq.1,
C                  do ZX-plane, else do XY-plane.
C
                cgeom_tab = 'yz'
                if ( ycross .gt. xcross ) cgeom_tab = 'zx'
                if ( (zcross .gt. xcross) .and. (zcross .gt. ycross) )
     *                cgeom_tab = 'xy'
             endif
C
 
             call intrp_element(cgeom,cgeom_tab,
     *                          ipxintrp,ipyintrp,ipzintrp,nvalue,
     *                          interp,
     *                          cmotable,
     *                          ctable,
     *                          ipyvalue,
     *                          ierror)
C
          elseif(nsdtopo_tab.eq.3.and.
     *           nen_tab.eq.4.and.nef_tab.eq.4) then
            if(cgeom(1:5).eq.'-def-') cgeom='xyz'
            if(cgeom_tab(1:5).eq.'-def-') cgeom_tab=cgeom
            call intrp_element(cgeom,cgeom_tab,
     *                         ipxintrp,ipyintrp,ipzintrp,nvalue,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ipyvalue,
     *                         ierror)
C
          elseif(nsdtopo_tab.eq.3.and.
     *           nen_tab.eq.8.and.nef_tab.eq.6) then
            if(cgeom(1:5).eq.'-def-') cgeom='xyz'
            if(cgeom_tab(1:5).eq.'-def-') cgeom_tab=cgeom
            call intrp_element(cgeom,cgeom_tab,
     *                         ipxintrp,ipyintrp,ipzintrp,nvalue,
     *                         interp,
     *                         cmotable,
     *                         ctable,
     *                         ipyvalue,
     *                         ierror)
          endif
          if(length_value.eq.npoints) then
             do i=1,mpno
                i1=mpary(i)
                xfield(i1)=xfac1*xfield(i1)+xfac2*yvalue(i)
             enddo
          elseif(length_value.eq.ntets) then
             do it=1,ntets
                xfield(it)=xfac1*xfield(it)+xfac2*yvalue(it)
             enddo
          endif
          call mmrelblk('list',isubname,iplist,icscode)
          call mmrelblk('xintrp',isubname,ipxintrp,icscode)
          call mmrelblk('yintrp',isubname,ipyintrp,icscode)
          call mmrelblk('zintrp',isubname,ipzintrp,icscode)
          call mmrelblk('yvalue',isubname,ipyvalue,icscode)
      elseif (itype(1:8).eq.'constant') then
        do i1=1,mpno
           i=mpary(i1)
           x=xic(i)
           y=yic(i)
           z=zic(i)
           call test_argument_type(1,2,8,imsgin,xmsgin,cmsgin,msgtype,
     *                       nwds)
           xcon=xmsgin(8)
           xfield(i)=xfac1*xfield(i)+xfac2*xcon
        enddo
      elseif (itype(1:4).eq.'user') then
        call get_global('time',iout,time,cout,itype,icscode)
        if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
        length=mpno
        call mmgetblk('xintrp',isubname,ipxintrp,length,2,icscode)
        call mmgetblk('yintrp',isubname,ipyintrp,length,2,icscode)
        call mmgetblk('zintrp',isubname,ipzintrp,length,2,icscode)
        call mmgetblk('matvec',isubname,ipmatvec,length,1,icscode)
        call mmgetblk('yvalue',isubname,ipyvalue,length,2,icscode)
        do i1=1,mpno
           i=mpary(i1)
           xintrp(i1)=xic(i)
           yintrp(i1)=yic(i)
           zintrp(i1)=zic(i)
           matvec(i1)=imt1(i)
        enddo
        call fadpt(xintrp,yintrp,zintrp,matvec,mpno,time,yvalue)
        do i1=1,mpno
           i=mpary(i1)
           xfield(i)=xfac1*xfield(i)+xfac2*yvalue(i1)
        enddo
        call mmrelblk('xintrp',isubname,ipxintrp,icscode)
        call mmrelblk('yintrp',isubname,ipyintrp,icscode)
        call mmrelblk('zintrp',isubname,ipzintrp,icscode)
        call mmrelblk('matvec',isubname,ipmatvec,icscode)
        call mmrelblk('yvalue',isubname,ipyvalue,icscode)


       elseif (itype(1:8).eq.'integer1') then
           call dopmat(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)


       elseif (itype(1:8).eq.'integer2') then
           call dopinteger(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)


       elseif (itype(1:8).eq.'gaussian') then
        do i1=1,mpno
           i=mpary(i1)
           x=xic(i)
           y=yic(i)
           z=zic(i)
C
           cgeom=cmsgin(8)
           call test_argument_type(8,2,9,imsgin,xmsgin,cmsgin,msgtype,
     *                       nwds)
           x1=xmsgin(9)
           y1=xmsgin(10)
           z1=xmsgin(11)
           x2=xmsgin(12)
           y2=xmsgin(13)
           z2=xmsgin(14)
           xlatdiff=xmsgin(15)
           xcon=xmsgin(16)
           xstd=xmsgin(17)
           dy=abs(y1-y)
           if(x.lt.x1.and.x.lt.x2) then
              dx=x-x1
           elseif(x.ge.x1.and.x.le.x2) then
              dx=0.0
           elseif(x.gt.x1.and.x.gt.x2) then
              dx=x-x2
           endif
           if(z.lt.z1.and.z.lt.z2) then
              dz=z-z1
           elseif(z.ge.z1.and.z.le.z2) then
              dz=0.0
           elseif(z.gt.z1.and.z.gt.z2) then
              dz=z-z2
           endif
           xl=sqrt(dy**2+(1.0/xlatdiff)*(dx**2+dz**2))
           xdoping=xcon*exp(-(xl/xstd)**2)
           xfield(i)=xfac1*xfield(i)+xfac2*xdoping
C
        enddo
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


      call mmrelprt(isubname,icscode)
C
      return
      end
