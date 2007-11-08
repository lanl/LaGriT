      subroutine do_extract(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Handles the parts of the EXTRACT command that deal with
C        planes, isosurfaces, and interfaces.
C
C     INPUT ARGUMENTS -
C
C        imsgin  - INTEGER VALUES FROM COMMAND LINE
C        xmsgin  - FLOAT VALUES FROM COMMAND LINE
C        cmsgin  - CHARACTER VALUES FROM COMMAND LINE
C        msgtype - DATA TYPE OF EACH TOKEN
C        nwds    - NUMBER OF TOKENS PASSED TO THIS ROUTINE
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - error returned (zero if no errors)
C
C     CHANGE HISTORY -
C
c $Log:   /pvcs.config/t3d/src/do_extract_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.5   01 Oct 2007 08:17:14   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.4   03 Nov 2005 09:18:34   dcg
CPVCS    fix definitions of d for ptnorm option
CPVCS    
CPVCS       Rev 1.3   14 May 2001 13:45:06   kuprat
CPVCS    Fixed documentation.
CPVCS    
CPVCS       Rev 1.2   14 May 2001 10:56:04   kuprat
CPVCS    New 'external' option for surfmesh.
CPVCS    
CPVCS       Rev 1.1   29 Feb 2000 14:55:30   gable
CPVCS    RVG No storage block version of do_extract with extract/surfmesh command
CPVCS    
CPVCS       Rev 1.0   08 Feb 2000 09:26:30   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.27   Wed Dec 08 03:50:46 1999   kuprat
CPVCS    Put in dotask call to popcomponents to support EXTRACT/SKELETON.
CPVCS
CPVCS       Rev 1.26   Wed Dec 02 09:42:22 1998   nnc
CPVCS    Fixed NETWORK selection bug.
CPVCS
CPVCS       Rev 1.25   Mon Nov 30 11:59:26 1998   nnc
CPVCS    Changed command name from extract/intrfac3 to extract/network.
CPVCS
CPVCS       Rev 1.24   Thu Nov 05 12:55:08 1998   dcg
CPVCS    implement new extract option...  extract/intrfac3
CPVCS
CPVCS       Rev 1.23   Tue Aug 19 14:29:58 1997   dcg
CPVCS    add icharlnf call to get length of input strings
CPVCS
CPVCS       Rev 1.22   Mon Apr 14 16:43:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.21   Thu Mar 06 21:51:24 1997   het
CPVCS    Save ipointi and ipointj counters.
CPVCS
CPVCS       Rev 1.20   Fri Oct 18 10:53:58 1996   jxf
CPVCS    added integer field isosurface capability.
CPVCS
CPVCS       Rev 1.19   11/07/95 17:16:06   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.18   09/19/95 13:11:00   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.17   08/23/95 13:52:58   jxf
CPVCS    added -all- option to INTRFAC2
CPVCS
CPVCS       Rev 1.16   06/21/95 13:19:32   het
CPVCS    Replace the character literal by a character variable in the call to hgetprt.f and add the hybrid grid arrays
CPVCS
CPVCS       Rev 1.15   05/22/95 11:01:52   ejl
CPVCS    Fixed memory manager error
CPVCS
CPVCS       Rev 1.14   05/18/95 10:28:04   dcg
CPVCS    add include file consts.h for epsilon
CPVCS
CPVCS       Rev 1.13   05/17/95 14:52:00   dcg
CPVCS    change paramater list to do_extract - get npoints,ntets with cmo_get_info
CPVCS
CPVCS       Rev 1.12   05/01/95 08:35:52   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.11   04/02/95 11:27:18   jxf
CPVCS
CPVCS
CPVCS       Rev 1.10   03/17/95 21:11:28   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.9   02/27/95 11:27:20   jxf
CPVCS    Added cmoin.  Changed 2-region intrface to intrfac2.
CPVCS
CPVCS       Rev 1.8   02/16/95 10:13:44   dcg
CPVCS    declare isubname to be type character
CPVCS
CPVCS       Rev 1.7   02/13/95 09:08:28   jxf
CPVCS    Complete rewrite -- routine now parses command and calls ohter
CPVCS    functions.
CPVCS
CPVCS
CPVCS       Rev 1.6   02/03/95 16:51:12   jxf
CPVCS    added another pointer to refresh after call to mmgetblk
CPVCS
CPVCS       Rev 1.5   02/03/95 16:36:22   jxf
CPVCS    added code to refresh pointers after call to mmgetblk
CPVCS
CPVCS       Rev 1.4   02/03/95 15:51:18   jxf
CPVCS    added include comdict.h for hgetprt; made mbndry 4-based even for
CPVCS    traingles; fixed test on jteta to .gt.0; added code to set point types on
CPVCS    boundaries.
CPVCS
CPVCS       Rev 1.3   02/03/95 09:06:48   jxf
CPVCS    Fixed bug in sceond call to hgetprt -- jpartindx.
CPVCS    Added checking for existence of cmoin.
CPVCS
CPVCS       Rev 1.2   02/01/95 10:36:10   jxf
CPVCS    Original implementation of extract/interfac
CPVCS
CPVCS
CPVCS       Rev 1.1   01/30/95 13:38:00   dcg
CPVCS
CPVCS
CPVCS       Rev 1.0   01/17/95 16:41:00   pvcs
CPVCS
C
C ######################################################################
C
C   comdict.h is needed for ipmodprm in hgetprt.
C
      implicit none
      integer nplen
      parameter (nplen=1000000)
      include "consts.h"
      integer nwds,ierr1
      character*132 logmess,cbuf
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*32 cmsgin(nwds), msg1, isubname, cmoin, cmoout,
     *             fieldname,ich1,ich2,ich3,cmode
      character*32 coption
      pointer(ipmpary, mpary(nplen))
      integer mpary
      pointer (ipisetwd, isetwd)
      pointer (ipjalias, jalias)
      pointer (ipitp1, itp1)
      integer isetwd(nplen), itp1(nplen), jalias(nplen)
      integer ipointi,ipointj,nnodes,jpartindx,ipartindx,icharlnf,
     *  interf,j,mpno,length,ntets,len,ity,ipt1,ipt2,ipt3,kpset,npoints,
     *  ierror,icscode,icmotype,ierr2
      real*8 a,b,c,d,xn,yn,zn,temp,valu,x1,y1,z1,x2,y2,z2,x3,y3,z3
C
      ierr1 = 0
      isubname = 'do_extract'
      cmoin = '-cmo-'
      cmoout = '-none-'
C
C   Check for character data on next input parameter after EXTRACT.
C   This parameter must be a character string.
C
      if(msgtype(1).ne.3) then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Bad value for extract type.  Must be character.'
         call writloga('default',0,logmess,0,ierr1)
         go to 9998
      endif
C
C   Make sure than a cmoout is supplied and also read the name for cmoin
C   if supplied.  Also get the first/last/stride or pset/get/name stuff.
C
      ich1=' '
      ich2=' '
      ich3=' '
      kpset = 0
C
      if (cmsgin(1)(1:5) .eq. 'plane') then
         if (cmsgin(2)(1:8) .eq. 'threepts') then
            if (msgtype(15) .eq. 3 ) cmoout = cmsgin(15)
            if (msgtype(16) .eq. 3 ) cmoin = cmsgin(16)
            if (msgtype(12) .eq. 1) then
               ipt1=imsgin(12)
               ipt2=imsgin(13)
               ipt3=imsgin(14)
            else
              ich1=cmsgin(12)
              ich2=cmsgin(13)
              ich3=cmsgin(14)
              kpset = 1
            endif
         else if (cmsgin(2)(1:6) .eq. 'ptnorm') then
            if (msgtype(12) .eq. 3 ) cmoout = cmsgin(12)
            if (msgtype(13) .eq. 3 ) cmoin = cmsgin(13)
            if (msgtype(9) .eq. 1) then
               ipt1=imsgin(9)
               ipt2=imsgin(10)
               ipt3=imsgin(11)
            else
              ich1=cmsgin(9)
              ich2=cmsgin(10)
              ich3=cmsgin(11)
              kpset = 1
            endif
         else if (cmsgin(2)(1:4) .eq. 'axes') then
            if (msgtype(9) .eq. 3 ) cmoout = cmsgin(9)
            if (msgtype(10) .eq. 3 ) cmoin = cmsgin(10)
            if (msgtype(6) .eq. 1) then
               ipt1=imsgin(6)
               ipt2=imsgin(7)
               ipt3=imsgin(8)
            else
              ich1=cmsgin(6)
              ich2=cmsgin(7)
              ich3=cmsgin(8)
              kpset = 1
            endif
         else if (cmsgin(2)(1:4) .eq. 'abcd') then
            if (msgtype(10) .eq. 3 ) cmoout = cmsgin(10)
            if (msgtype(11) .eq. 3 ) cmoin = cmsgin(11)
            if (msgtype(7) .eq. 1) then
               ipt1=imsgin(7)
               ipt2=imsgin(8)
               ipt3=imsgin(9)
            else
              ich1=cmsgin(7)
              ich2=cmsgin(8)
              ich3=cmsgin(9)
              kpset = 1
            endif
         endif
      else if (cmsgin(1)(1:3) .eq. 'iso') then
         if (msgtype(7) .eq. 3 ) cmoout = cmsgin(7)
         if (msgtype(8) .eq. 3 ) cmoin = cmsgin(8)
         if (msgtype(4) .eq. 1) then
            ipt1=imsgin(4)
            ipt2=imsgin(5)
            ipt3=imsgin(6)
         else
           ich1=cmsgin(4)
           ich2=cmsgin(5)
           ich3=cmsgin(6)
           kpset = 1
         endif
      else if (cmsgin(1)(1:8) .eq. 'intrface') then
         if (msgtype(6) .eq. 3 ) cmoout = cmsgin(6)
         if (msgtype(7) .eq. 3 ) cmoin = cmsgin(7)
         if (msgtype(3) .eq. 1) then
            ipt1=imsgin(3)
            ipt2=imsgin(4)
            ipt3=imsgin(5)
         else
           ich1=cmsgin(3)
           ich2=cmsgin(4)
           ich3=cmsgin(5)
           kpset = 1
         endif
      else if (cmsgin(1)(1:8) .eq. 'intrfac2') then
         if (msgtype(7) .eq. 3 ) cmoout = cmsgin(7)
         if (msgtype(8) .eq. 3 ) cmoin = cmsgin(8)
         if (msgtype(4) .eq. 1) then
            ipt1=imsgin(4)
            ipt2=imsgin(5)
            ipt3=imsgin(6)
         else
           ich1=cmsgin(4)
           ich2=cmsgin(5)
           ich3=cmsgin(6)
           kpset = 1
         endif
      else if (cmsgin(1)(1:7) .eq. 'network') then
         if (msgtype(5) .eq. 3 ) cmoout = cmsgin(5)
         if (msgtype(6) .eq. 3 ) cmoin = cmsgin(6)
         if (msgtype(2) .eq. 1) then
            ipt1=imsgin(2)
            ipt2=imsgin(3)
            ipt3=imsgin(4)
         else
           ich1=cmsgin(2)
           ich2=cmsgin(3)
           ich3=cmsgin(4)
           kpset = 1
         endif
      else if (cmsgin(1)(1:8) .eq. 'skeleton') then
         if (msgtype(5) .eq. 3 ) cmoout = cmsgin(5)
         if (msgtype(6) .eq. 3 ) cmoin = cmsgin(6)
         if (msgtype(2) .eq. 1) then
            ipt1=imsgin(2)
            ipt2=imsgin(3)
            ipt3=imsgin(4)
         else
           ich1=cmsgin(2)
           ich2=cmsgin(3)
           ich3=cmsgin(4)
           kpset = 1
         endif
      else if (cmsgin(1)(1:8) .eq. 'surfmesh') then
C
C     EXTRACTION OF EXTERIOR SURFACE MESH ONLY IF
C     CMODE = 'EXTERNAL'
C
         if (msgtype(5) .eq. 3 ) cmoout = cmsgin(5)
         if (msgtype(6) .eq. 3 ) cmoin = cmsgin(6)
         if (msgtype(7) .eq. 3 ) cmode = cmsgin(7)
         if (msgtype(2) .eq. 1) then
            ipt1=imsgin(2)
            ipt2=imsgin(3)
            ipt3=imsgin(4)
         else
           ich1=cmsgin(2)
           ich2=cmsgin(3)
           ich3=cmsgin(4)
           kpset = 1
         endif
      endif
C
      call cmo_exist(cmoin,ierror)
      if(ierror .ne. 0)then
          write(logmess,'(a)')'WARNING: EXTRACT'
          call writloga('default',0,logmess,0,ierror)
          write(logmess,'(a)')'WARNING: INPUT MO DOES NOT EXIST'
          call writloga('default',0,logmess,0,ierror)
          write(logmess,'(a)')'WARNING: No action'
          call writloga('default',0,logmess,0,ierror)
          goto 9998
      endif
      if(cmoout .eq. '-none-') then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Need a name for the output mesh object.'
         call writloga('default',0,logmess,0,ierr1)
         go to 9998
      endif
      call cmo_get_info('nnodes',cmoin,npoints,len,ity,ierror)
      call cmo_get_info('nelements',cmoin,ntets,len,ity,ierror)
      length=npoints
      call mmgetblk('mpary',isubname,ipmpary,length,2,icscode)
      call mmgetblk('jalias',isubname,ipjalias,length,2,icscode)
C
C      call cmo_get_name(cmoin,ierror)
C
      call cmo_get_info('isetwd',cmoin,ipisetwd,length,icmotype,
     *                   ierror)
      call cmo_get_info('itp1',cmoin,ipitp1,length,icmotype,ierror)
C
C    set the point index boundaries
C
      mpno=0
      if(kpset.eq.0)call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      if(kpset.ne.0)call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
C
      do j=1,length
         jalias(j) = 0
      enddo
      do j=1,mpno
         jalias(mpary(j)) = 1
      enddo
C
C   Figure out that the user wants to do.
C
      if(cmsgin(1)(1:5).eq.'plane')  then
C
C   If PLANE, compute the four coefficients A, B, C, and D, in:
C   Ax + By + Cz + D = 0
C
         if(cmsgin(2)(1:8).eq.'threepts') then
            call test_argument_type(9,2,3,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            x1 = xmsgin(3)
            y1 = xmsgin(4)
            z1 = xmsgin(5)
            x2 = xmsgin(6)
            y2 = xmsgin(7)
            z2 = xmsgin(8)
            x3 = xmsgin(9)
            y3 = xmsgin(10)
            z3 = xmsgin(11)
            a = (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
            b = (z2-z1)*(x3-x1) - (z3-z1)*(x2-x1)
            c = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
            d = -a*x1 - b*y1 - c*z1
            if((a*a + b*b + c*c).lt.epsilon) then
               ierr1 = 1
               write(logmess,'(a)')
     *           'Error:  Points are colinear.'
               call writloga('default',0,logmess,0,ierr1)
               go to 9998
            endif
         elseif(cmsgin(2)(1:6).eq.'ptnorm') then
            call test_argument_type(6,2,3,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            x1 = xmsgin(3)
            y1 = xmsgin(4)
            z1 = xmsgin(5)
            xn = xmsgin(6)
            yn = xmsgin(7)
            zn = xmsgin(8)
            temp = sqrt(xn*xn + yn*yn + zn*zn)
            if (abs(temp).lt.epsilon) then
               ierr1 = 1
               write(logmess,'(a)')
     *           'Error:  Cant have normals all zero.'
               call writloga('default',0,logmess,0,ierr1)
               go to 9998
            endif
            a = xn/temp
            b = yn/temp
            c = zn/temp
            d = -(x1*xn + y1*yn + z1*zn)/temp
         elseif(cmsgin(2)(1:4).eq.'axes') then
            call test_argument_type(3,2,3,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            x1 = xmsgin(3)
            y1 = xmsgin(4)
            z1 = xmsgin(5)
            if (abs(x1).lt.epsilon) then
               a = 1.0
               b = 0.0
               c = 0.0
               d = 0.0
            elseif(abs(y1).lt.epsilon) then
               a = 0.0
               b = 1.0
               c = 0.0
               d = 0.0
            elseif(abs(z1).lt.epsilon) then
               a = 0.0
               b = 0.0
               c = 1.0
               d = 0.0
            else
               a = 1.0/x1
               b = 1.0/y1
               c = 1.0/z1
               d = -1.0
               temp = sqrt(a*a + b*b + c*c)
               a = a/temp
               b = b/temp
               c = c/temp
               d = d/temp
            endif
         elseif(cmsgin(2)(1:4).eq.'abcd') then
            call test_argument_type(4,2,3,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            a = xmsgin(3)
            b = xmsgin(4)
            c = xmsgin(5)
            d = xmsgin(6)
            temp = sqrt(a*a + b*b + c*c)
            a = a/temp
            b = b/temp
            c = c/temp
            d = d/temp
         else
            ierr1 = 1
            write(logmess,'(a,a)')
     *        'Bad PLANE type:  ',cmsgin(2)
            call writloga('default',0,logmess,0,ierr1)
            go to 9998
         endif
C
         call extract_plane(cmoout,cmoin,npoints,ntets,a,b,c,d,
     *                      ierr1)
C
      elseif(cmsgin(1)(1:3).eq.'iso') then
C
         fieldname = cmsgin(2)
         call test_argument_type(1,2,3,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
         if (msgtype(3) .eq. 2) then
           valu = xmsgin(3)
         else
           valu = 1.0*imsgin(3)
         endif
         call extract_isosurface(cmoout,cmoin,npoints,ntets,
     *                           fieldname,valu,ierr1)
 
      elseif(cmsgin(1)(1:7).eq.'network') then
 
         call extract_network (cmoout, cmoin, jalias, ierr1)
 
      elseif(cmsgin(1)(1:8).eq.'surfmesh') then
 
         call extract_surfmesh (cmoout, cmoin, cmode, ierr1)
 
      elseif(cmsgin(1)(1:8).eq.'skeleton') then
 
         call cmo_set_name(cmoin,icscode)
         if (kpset.eq.0) then
            write(cbuf,1000) ipt1,ipt2,ipt3,cmoout
 1000       format('popcomponents/////',i10,'/',i10,'/',i10
     &         ,'/skeletononly/',a,' ; finish ')
         else
            write(cbuf,1001) ich1,ich2,ich3,cmoout
 1001       format('popcomponents/////',a,'/',a,'/',a
     &         ,'/skeletononly/',a,' ; finish ')
         endif
         call dotask(cbuf,ierr1)
 
      elseif(cmsgin(1)(1:3).eq.'int') then
C
C   If INTERFACE, get the material number.  Either it will be
C   supplied or we will find it from the region name.
C
         interf = 1
         if(msgtype(2).eq.3) then
            msg1 = cmsgin(2)(1:icharlnf(cmsgin(2)))
            if (msg1(1:5) .eq.'-all-') then
               ipartindx = -1
            else
               call get_material_number(msg1,ipartindx,ierr2)
               if(ierr2.ne.0) then
                  ierr1 = ierr2
                  write(logmess,'(a,a)')
     *              'Error:  No region with the name:  ',cmsgin(2)
                  call writloga('default',0,logmess,0,ierr2)
                  go to 9998
               endif
            endif
         else
            ipartindx = imsgin(2)
         endif
         if(cmsgin(1)(1:8).eq.'intrfac2') then
C
C   If we get to here, the user wants to find the interface between
C   two regions.  Get the material number for the second region.
C
            interf = 2
            if(msgtype(3).eq.3) then
               msg1 = cmsgin(3)(1:icharlnf(cmsgin(3)))
               if (msg1(1:5) .eq.'-all-') then
                  jpartindx = -1
               else
                  coption='part'
                  call get_material_number(msg1,jpartindx,ierr2)
                  if(ierr2.ne.0) then
                     ierr1 = ierr2
                     write(logmess,'(a,a)')
     *                 'Error:  No region with the name:  ',cmsgin(3)
                     call writloga('default',0,logmess,0,ierr2)
                     go to 9998
                  endif
               endif
            else
               jpartindx = imsgin(3)
            endif
         endif
         call extract_interface(cmoout,cmoin,npoints,ntets,ipjalias,
     *         ipartindx,jpartindx,interf,ierr1)
      endif
C
C
C     REPLACE THE STARTING AND ENDING POINT COUNTERS THAT REFER
C        TO THE LAST POINT SET GENERATED.
C
      call cmo_exist(cmoout,ierror)
      if(ierror.eq.0) then
         call cmo_get_info('nnodes',cmoout,nnodes,len,ity,ierror)
         ipointi=1
         ipointj=nnodes
         call cmo_set_info('ipointi',cmoout,
     *                   ipointi,1,1,icscode)
         call cmo_set_info('ipointj',cmoout,
     *                   ipointj,1,1,icscode)
 
c.... The output mesh object becomes the current mesh object.
         call cmo_set_name(cmoout,icscode)
 
      endif
C
 9998 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
