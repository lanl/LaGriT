      subroutine bndpts()
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE FINDS POINTS THAT ARE WITHIN A MINIMUM SEARCH RANGE
C        FROM CONSTRAINED BOUNDARIES AND SET icr1 FOR THE
C        POINTS.  IF A POINT FALLS WITHIN 2 OR MORE PLANES AND THE icr1
C        FOR icontab IS NOT SET, A NEW icontab ENTRY WILL BE CREATED.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C        $Log: bndpts.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   06 Nov 2003 14:07:04   gable
CPVCS    Changed i6 format to i9.
CPVCS    
CPVCS       Rev 1.3   Wed Apr 05 13:34:08 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.2   Wed Feb 02 13:18:44 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:30   dcg
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:18   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.17   Wed Nov 10 14:23:54 1999   dcg
CPVCS    make maxsurfcons a local variable
CPVCS
CPVCS       Rev 1.16   Tue May 18 14:31:10 1999   dcg
CPVCS    make sure icontab entries occur in increasing
CPVCS    surface number order
CPVCS
CPVCS       Rev 1.15   Mon Apr 14 16:39:16 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.14   Fri Jul 19 13:29:14 1996   dcg
CPVCS    check for 'real' points
CPVCS
CPVCS       Rev 1.13   Wed May 22 15:53:34 1996   dcg
CPVCS    fix set_info('nconbnd'... calls
CPVCS
CPVCS       Rev 1.12   Wed May 22 10:14:12 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.11   Wed May 22 09:02:12 1996   dcg
CPVCS    add mmrelprt call
CPVCS
CPVCS       Rev 1.10   Thu May 16 10:22:18 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.9   07/14/95 10:15:56   het
CPVCS    Correct errors with point types
CPVCS
CPVCS       Rev 1.8   06/22/95 11:20:30   dcg
CPVCS    replace character literal 'boundary' with variable cboundy
CPVCS
CPVCS       Rev 1.7   06/07/95 10:59:22   het
CPVCS    Eldon's versions with implicit none
CPVCS
CPVCS       Rev 1.6   05/11/95 13:45:32   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.5   03/31/95 09:03:12   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.4   03/23/95 22:57:20   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.3   02/12/95 08:41:50   het
CPVCS    Correct an error in setting itp1() and icr1() values
CPVCS
CPVCS       Rev 1.2   01/04/95 22:01:20   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/24/94 10:52:08   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:02   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'geom_lg.h'
C
C#######################################################################
C
      character*32 cmo, isubname,geom_name
      character*8 itest,ctype
C
      integer npoints,ierr,ilen,itype,ncon50,iout,lout,
     *   nconbnd,maxsurfcons,idum1,length,
     *   j,i,k,l,icrtab,ns,idx,ninc,icr,ip,
     *   ierror ,icrck,nicr
      logical neednew
c
      pointer (ipireal1, ireal1)
      integer ireal1(1000000)
C
      pointer (ipicr1, icr1)
      integer icr1(*)
      pointer (ipitp1, itp1)
      integer itp1(*)
      pointer (ipicontab, icontab)
      integer icontab(50,*)
      pointer (ipxic, xic)
      real*8 xic(*)
      pointer (ipyic, yic)
      real*8 yic(*)
      pointer (ipzic, zic)
      real*8 zic(*)
      character*132 logmess
      pointer (ipisurftst,isurftst)
      integer isurftst(*)
C
C#######################################################################
C
      real*8 srchval,rout
      pointer(ipout,out)
      real*8 out(*)
c
            data maxsurfcons / 48 /
C#######################################################################
C
C
      isubname='bndpts'
C
C
C     ******************************************************************
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_name')
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
C
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('ncon50',cmo,ncon50,ilen,itype,ierr)
      call cmo_get_info('icontab',cmo,ipicontab,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call cmo_get_info('nconbnd',cmo,nconbnd,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
C
      itest='eq      '
      call mmgetblk('isurftst',isubname,ipisurftst,npoints,1,ierr)
C
C     ******************************************************************
C     GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', srchval)
C
C     ******************************************************************
C     LOOP THROUGH ALL CONSTRAINED SURFACES
C
      call mmgetblk('ireal1',isubname,ipireal1,npoints,1,ierr)
      call unpacktp('allreal','set',npoints,ipitp1,ipireal1,ierr)
c
c  get pointers to surface data
c
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      do i=1,nsurf
 
         if(ibtype(i)(1:7).eq.'reflect'.or.
     *         ibtype(i)(1:7).eq.'virtual'
     *     .or.ibtype(i)(1:8).eq.'intrcons') then
C
C    FIND ICONTAB ENTRY FOR THIS SURFACE
C
         icrtab=0
         do j=1,nconbnd
            if(icontab(2,j).eq.2.and.icontab(3,j).eq.i) icrtab=j
         enddo
         if(icrtab.eq.0) then
C
C    NO ICONTAB FOR THIS SURFACE - ADD ONE
C
            nconbnd=nconbnd+1
            icontab(1,nconbnd)=1
            icontab(2,nconbnd)=2
            icontab(3,nconbnd)=i
         endif
C
C     LOOP THROUGH POINTS TO FIND THOSE WITHIN srchval FROM
C     BOUNDARY PLANES.
C
         call surftstv(xic,yic,zic,npoints,srchval,cmo,istype(i),
     *                 surfparam(offsparam(i)+1),sheetnm(i),
     *                 itest,isurftst)
         do ip=1,npoints
         if (ireal1(ip).ne.1) go to 100
            if(icr1(ip).eq.0.and.isurftst(ip).eq.1) then
               icr1(ip)=icrtab
            elseif (icr1(ip).gt.0.and.isurftst(ip).eq.1) then
C
C   SINCE ICR HAS A VALUE - FIND ALL PREVIOUS PLANES ADD THIS ONE
C    AND SEE IF THAT ICONTAB ENTRY EXISTS
C
               neednew=.true.
               ns=icontab(1,icr1(ip))
               do j=1,nconbnd
                  if (ns+1.eq.icontab(1,j)) then
                      do k=1,ns
                         do l=1,ns
                            if (icontab(2+l,j).eq.icontab(2+k,icr1(ip)))
     *                              go to 20
                         enddo
                         go to 30
 20                      if (k.eq.ns) then
                            do l=1,ns+1
                               if(i.eq.icontab(2+l,j)) then
                                 neednew=.false.
                                 idx=j
                                 go to 35
                               endif
                            enddo
                         endif
                      enddo
                  endif
 30               continue
               enddo
C
C  SET ICR VALUE BASED ON NEEDNEW
C
 35            if (neednew) then
                  nconbnd=nconbnd+1
                  if(nconbnd.gt.ncon50/50) then
                    call cmo_set_info('ncon50',cmo,(nconbnd+9)*50,1,1
     *                                     ,ierr)
                    ninc=(nconbnd+9)*50-ncon50
                    call mmincblk('icontab',cmo,ipicontab,ninc,ierr)
                    do k=nconbnd,nconbnd+9
                       do j=1,50
                          icontab(j,k)=0
                       enddo
                    enddo
                  endif
                  if (ns+1.gt.maxsurfcons) then
                     logmess='more than maxsurfcons constrained '
     *                     // 'surfaces additional surfaces ignored'
                     call writloga('default',0,logmess,0,ierr)
                  else
                     icontab(1,nconbnd)=ns+1
                     if (ns+1.eq.2 ) then
                        icontab(2,nconbnd)=1
                     elseif (ns+1.eq.1) then
                        icontab(2,nconbnd)=2
                     else
                        icontab(2,nconbnd)=0
                     endif
                     do j=1,ns
                       icontab(2+j,nconbnd)=icontab(2+j,icr1(ip))
                     enddo
                     icontab(3+ns,nconbnd)=i
                     call isort(icontab(3,nconbnd),idum1,ns+1,2)
                     icr1(ip)=nconbnd
                  endif
               else
                  icr1(ip)=idx
               endif
           endif
 100       continue
         enddo
       endif
      enddo
C
C     COUNT AND PRINT THE NO. OF POINTS PER icr.
C
      do icr=1,nconbnd
C
         nicr=0
         do i=1,npoints
            if (ireal1(i).eq.1 ) then
               icrck=icr1(i)
               if (icrck .eq. icr) nicr=nicr+1
            endif
         enddo
C
         if (nicr .gt. 0) then
            if(icontab(2,icr).eq.0) ctype='point'
            if(icontab(2,icr).eq.1) ctype='line'
            if(icontab(2,icr).eq.2) ctype='surface'
            write(logmess, 9000) icr,ctype,
     &                           (icontab(ip,icr),ip=3,5),nicr
 9000       format(' FOR icr',i4,' TYPE ',a8,3i4,' THERE ARE',i9,
     &             ' POINTS')
            call writloga('default',0,logmess,0,ierr)
         endif
C
      enddo
C
 9999 continue
      call cmo_set_info('nconbnd',cmo,nconbnd,1,1,ierr)
      if(ierr.ne.0) call x3d_error('bndpts','cmo_get_info')
      call mmrelprt(isubname,ierr)
      return
      end
