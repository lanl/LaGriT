      subroutine ifacerfl(mpnt,npt,srchval)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE FINDS POINTS THAT ARE WITHIN A MINIMUM SEARCH RANGE
C        FROM CONSTRAINED BOUNDARIES AND SET icr1 FOR THE POINTS
C
C     INPUT ARGUMENTS -
C
C          mpnt  index of first point to check
C          ntp     number of points
C          smaldist search range
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C        $Log: ifacerfl.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   30 Jan 2001 08:26:14   dcg
CPVCS    fix bad arguments in call to cmo_get_intinfo for ncon50
CPVCS    change cmo_get_info calls to cmo_get_intinfo where needed
CPVCS    
CPVCS       Rev 1.3   18 Oct 2000 16:36:48   dcg
CPVCS    don't change icr value if already okay
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 13:34:34 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   Wed Feb 02 12:54:34 2000   dcg
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 11:15:58   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.12   Tue May 18 14:30:32 1999   dcg
CPVCS    make sure icontab entries occur in increasing
CPVCS    surface number order
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 16:51:38 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Tue Jun 04 17:20:16 1996   dcg
CPVCS    remove extra comma in call to surftstv
CPVCS
CPVCS       Rev 1.9   Wed May 29 08:43:00 1996   het
CPVCS    Fix a problem when "nconbnd" was not defined.
CPVCS
CPVCS       Rev 1.8   Wed May 22 15:54:18 1996   dcg
CPVCS    fix set_info('nconbnd'... calls
CPVCS
CPVCS       Rev 1.7   Wed May 22 10:16:04 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.6   Fri May 17 08:53:46 1996   dcg
CPVCS    add mmrelprt call
CPVCS
CPVCS       Rev 1.5   Thu May 16 10:26:30 1996   dcg
CPVCS     changes for new interface type 3 and for new icontab, xcontab
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
      character*32 cmo, isubname, geom_name
      character*8  itest
C
      integer npoints,ierr,ilen,itype,ip,ncon50,idum1,
     *   j,i,k,icrtab,idx,ninc,length,ierror,ns,iout,lout
      pointer(ipout,out)
      real*8 out(*)
      logical neednew
C
      pointer (ipicr1, icr1)
      integer icr1(*)
      pointer (ipicontab, icontab)
      integer icontab(50,*)
      pointer (ipxic, xic)
      real*8 xic(*)
      pointer (ipyic, yic)
      real*8 yic(*)
      pointer (ipzic, zic)
      real*8 zic(*)
      pointer (ipisurftst,isurftst)
      integer isurftst(*)
      integer mpnt,npt,nconbnd
C
C#######################################################################
C
      real*8 srchval,rout
C
C#######################################################################
C
      isubname='ifacerfl'
C
C     ******************************************************************
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_name')
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
      call cmo_get_intinfo('nnodes',cmo,npoints,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_intinfo')
      call cmo_get_intinfo('nconbnd',cmo,nconbnd,ilen,itype,ierr)
      if(ierr.ne.0) goto 9999
C*****if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
C
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
      call cmo_get_intinfo('ncon50',cmo,ncon50,ilen,itype,ierr)
      call cmo_get_info('icontab',cmo,ipicontab,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error('ifacerfl','cmo_get_info')
C
            itest='eq      '
      call mmgetblk('isurftst',isubname,ipisurftst,npoints,1,ierr)
C
C     ******************************************************************
C     ******************************************************************
C     LOOP THROUGH ALL CONSTRAINED SURFACES
C
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
 
      do i=1,nsurf
         if(ibtype(i).eq.'reflect'.or.ibtype(i).eq.'virtual'
     *     .or.ibtype(i).eq.'intrcons') then
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
 
         call surftstv(xic(mpnt),yic(mpnt),zic(mpnt),npt,srchval,cmo,
     &       istype(i),surfparam(offsparam(i)+1),sheetnm(i),
     &       itest,isurftst)
         do ip=mpnt,mpnt+npt-1
            if(isurftst(ip-mpnt+1).eq.0) go to 37
            if(icr1(ip).eq.0) then
               icr1(ip)=icrtab
            elseif(icontab(1,icr1(ip)).eq.1.and.
     *         icontab(3,icr1(ip)).eq.i) then
               go to 37
            else
C
C   SINCE ICR HAS A VALUE - FIND ALL PREVIOUS PLANES ADD THIS ONE
C    AND SEE IF THAT ICONTAB ENTRY EXISTS
C
               neednew=.true.
               ns=icontab(1,icr1(ip))
               do j=1,nconbnd
                  if (ns+1.eq.icontab(1,j)) then
                      do k=1,ns
                         if (icontab(2+k,j).ne.icontab(2+k,icr1(ip)))
     *                              go to 33
                      enddo
                      neednew=.false.
                      idx=j
                      go to 35
                  endif
 33               continue
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
                    call cmo_get_intinfo('ncon50',cmo,ncon50,ilen,itype
     *                                     ,ierr)
                    do k=nconbnd,nconbnd+9
                       do j=1,50
                          icontab(j,k)=0
                       enddo
                    enddo
                  endif
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
                  call isort(icontab(3,nconbnd),idum1,ns+1,1)
                  icr1(ip)=nconbnd
               else
                  icr1(ip)=idx
               endif
           endif
 37        continue
         enddo
       endif
 
      enddo
C
      call cmo_set_info('nconbnd',cmo,nconbnd,1,1,ierr)
C
 9999 continue
      call mmrelprt(isubname,ierr)
      return
      end
