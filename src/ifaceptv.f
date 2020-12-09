      subroutine ifaceptv (x1,y1,z1,x2,y2,z2,npts,
     *                     epsilon,
     *                     ipxi,ipyi,ipzi,nptsnew,ipnconew,ierr)
C
C    PURPOSE
C     This routine returns a list of points that lie between
C     (x1,y1,z1) and (x2,y2,z2) pairs which also lie on surfaces
C     These are the points needed to break multimaterial connections
C     The algorithm calls getsfactv for all surfaces that are not
C     external boundary surfaces.  It accumulates the points
C     checks for duplicates and sends points to  getregv.  Points
C     which are 'on' surfaces are kept.
C
C    Input
C
C     x1,y1,z1,x2,y2,z2 coordinates of multimaterial point pairs
C     npts              number of point pairs
C     epsilon           epsilon for testing equivalence
C
C   Output
C     xi,yi,zi          coordinates of new points
C     nptsnew           number of new points
C     nconew		connection number for each new point
C     ierr              error return
C
C  CHANGE HISTORY
C $Log: ifaceptv.f,v $
C Revision 2.00  2007/11/05 19:45:58  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   14 Feb 2000 17:16:42   dcg
CPVCS    use geom_name in surface info find block calls
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:48:06   dcg
CPVCS    
CPVCS       Rev 1.1   04 Jan 2000 17:23:28   dcg
CPVCS
CPVCS       Rev 1.3   Fri Aug 28 14:24:56 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.2   Mon Nov 24 16:33:58 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.1   Fri Oct 03 11:01:50 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.0   Wed May 14 09:55:26 1997   dcg
CPVCS    Initial revision.
C
      implicit none
      include 'geom_lg.h'
      logical ichkpt
      integer ierr,npts,ierror,length,iout,lout,itype,
     *       nconew(npts),nptsnew
      real*8 x1(npts),y1(npts),z1(npts),x2(npts),y2(npts),z2(npts),
     *       xi(npts),yi(npts),zi(npts),epsilon,rout
      pointer(ipout,out)
      real*8 out(*)
      pointer (ipnsfact,nsfact)
      pointer (ipsfact,sfact)
      pointer (ipimts1,imts1)
      pointer (ipnimts,nimts)
      pointer (ipxi,xi)
      pointer (ipyi,yi)
      pointer (ipzi,zi)
      pointer (ipnconew,nconew)
      integer nsfact(1000000)
      integer imts1(1000000)
      integer nimts(1000000)
      integer j,k,is,jj,lng
      character*32 iword,blknm,prtnm,cmo,geom_name
      character*8 isubname
      real*8 sfact(100,1000000),ckmin,ckmax,xchk,ychk,zchk,
     *   alargenumber
      parameter (alargenumber=1.d+20)
c
       isubname='ifaceptv'
       ckmin=0.
       ckmax=alargenumber
       call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
       call mmfindbk('csall',geom_name,ipcsall,length,ierror)
       call mmfindbk('istype',geom_name,ipistype,length,ierror)
       call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
       call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
       call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
       call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
C
C   loop through surfaces, check type
C
      call mmgetblk('nsfact',isubname,ipnsfact,npts,1,ierr)
      call mmgetblk('sfact',isubname,ipsfact,npts*100,2,ierr)
      nptsnew=0
      do is=1,nsurf
         iword=ibtype(is)
         if(iword(1:4).eq.'free'.or.iword(1:7).eq.'reflect') go to 100
         call getsfactv(x1,y1,z1,x2,y2,z2,npts,cmo,istype(is),
     *                 surfparam(offsparam(is)+1),sheetnm(is),
     *                 ckmin,ckmax,epsilon,sfact,nsfact)
C
C  check for duplicates
C
         do j=1,npts
            if(nsfact(j).eq.0) go to 50
            do k=1,nsfact(j)
C
C  all sfacts should be between 0 and 1
C
               if(sfact(k,j).gt.1.0.or.sfact(k,j).le.0.0) go to 40
               xchk=x1(j)+sfact(k,j)*(x2(j)-x1(j))
               ychk=y1(j)+sfact(k,j)*(y2(j)-y1(j))
               zchk=z1(j)+sfact(k,j)*(z2(j)-z1(j))
               do jj=1,npts
                  if(.not.ichkpt(xchk,ychk,zchk,x1(jj),y1(jj),
     *                           z1(jj),epsilon)) go to  40
                  if(.not.ichkpt(xchk,ychk,zchk,x2(jj),y2(jj),
     *                           z2(jj),epsilon)) go to  40
               enddo
               do jj=1,nptsnew
                   if(.not.ichkpt(xchk,ychk,zchk,xi(jj),yi(jj),
     *                           zi(jj),epsilon)) go to  40
               enddo
               nptsnew=nptsnew+1
               call mmgetlen(ipxi,lng,ierr)
               if (lng.lt.nptsnew) then
            	  call mmgetnam(ipxi,blknm,prtnm,ierr)
            	  call mmnewlen(blknm,prtnm,ipxi,nptsnew+10,ierr)
                  call mmgetnam(ipyi,blknm,prtnm,ierr)
            	  call mmnewlen(blknm,prtnm,ipyi,nptsnew+10,ierr)	
            	  call mmgetnam(ipzi,blknm,prtnm,ierr)
            	  call mmnewlen(blknm,prtnm,ipzi,nptsnew+10,ierr)
                  call mmgetnam(ipnconew,blknm,prtnm,ierr)
            	  call mmnewlen(blknm,prtnm,ipnconew,nptsnew+10,ierr)	
               endif
               nconew(nptsnew)=j
               xi(nptsnew)=xchk
               yi(nptsnew)=ychk
               zi(nptsnew)=zchk
 40            continue
            enddo
 50         continue
         enddo
 100     continue
      enddo
C
C     make sure these prospective points are interface points
C
      if(nptsnew.eq.0) go to 9999
      call mmgetblk('imts1',isubname,ipimts1,nptsnew*nmregs,1,ierr)
      call mmgetblk('inmts',isubname,ipnimts,nptsnew,1,ierr)
      call ifaceregv(xi,yi,zi,nptsnew,epsilon,
     *               imts1,nimts,ierr)
C
C    delete non-interface points
C
 150  if(nimts(nptsnew).eq.0) then
         nptsnew=nptsnew-1
         go to 150
      endif
      do j=nptsnew-1,1,-1
         if(nimts(j).eq.0) then
            do k=j,nptsnew-1
               xi(k)=xi(k+1)
               yi(k)=yi(k+1)
               zi(k)=zi(k+1)
               nconew(k)=nconew(k+1)
            enddo
            nptsnew=nptsnew-1
         endif
      enddo
 9999 call mmrelprt(isubname,ierr)
      return
      end
C
      logical function ichkpt(x1,y1,z1,x2,y2,z2,epsilon)
C   return true if (x1,y1,z1) is different from (x2,y2,z2)
C
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,epsilon
      ichkpt=.true.
      if(abs(x1-x2).gt.epsilon) go to 9999
      if(abs(y1-y2).gt.epsilon) go to 9999
      if(abs(z1-z2).gt.epsilon) go to 9999
      ichkpt=.false.
 9999 continue
      return
      end
