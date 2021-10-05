 
      subroutine correctpc(iparent,isurv,iremov,iparsurv,iparremov,
     &   itettyp,iseedtet,itp1,itet,itetoff,jtet,jtetoff,nelts,
     &   ipielts,nef_cmo,mbndry,imt1,isn1,itetclr,lmoved)
C
C ######################################################################
C
C        $Log: correctpc.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      implicit none
 
      include 'local_element.h'
      include 'chydro.h'
 
      pointer (ipielts,ielts)
      integer ielts(*)
 
      integer itp1(*),iparent(*),itettyp(*),itet(*),itetoff(*),
     &   jtet(*),jtetoff(*),mbndry,imt1(*),isn1(*),
     &   itetclr(*)
      integer iparsurv,iparremov,matelt,nodk,ksurv,ichild,nod1,nod2,
     &   ityp,locnod,j,isurv,iremov,iseedtet,nod,ierrw,nelts,nef_cmo,
     &   ielt,ierr,k,itp
      logical lmoved(*),lset1,lset2,lreal
      character*132 cbuf,logmess
 
      ityp=itettyp(iseedtet)
      locnod=0
      do j=1,nelmnen(ityp)
         nod=itet(j+itetoff(iseedtet))
         if (iparsurv.eq.iparent(nod)) then
            locnod=j
         endif
      enddo
      if (locnod.eq.0) then
         write(logmess,'(a,i9,a,i9,a,i9)')
     &      'Try ',isurv,'<--',iremov,
     &      ':  FATAL ERROR : topological error at node ',isurv
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
      endif
 
      call get_elements_around_node(iseedtet,locnod,nelts
     &   ,ipielts,itetoff,jtetoff,itet,jtet,itettyp,iparent,
     &   nef_cmo,mbndry)
 
c.... Check that there is child at ISURV for each material type
c.... appearing in the retrieved tet list.
 
      do 200 j=1,nelts
         ielt=ielts(j)
         ityp=itettyp(ielt)
         matelt=itetclr(ielt)
 
         do k=1,nelmnen(ityp)
            nodk=itet(k+itetoff(ielt))
            if (nodk.eq.isurv) then
               ksurv=k
               if (imt1(nodk).eq.matelt) then
                  goto 200
               else
                  goto 300
               endif
            endif
         enddo
         goto 200
 
 300     continue
 
         nod=isn1(nodk)
         do k=1,10000
            if (imt1(nod).eq.matelt.and.itp1(nod).ne.ifitpcup)
     &         then
               itet(ksurv+itetoff(ielt))=nod
               goto 200
            endif
            nod=isn1(nod)
            if (nod.eq.nodk) goto 210
         enddo
         write(logmess,'(a,i9,a,i9,a,i9)')
     &      'Try ',isurv,'<--',iremov,
     &      ':  FAIL : topological error at node ',isurv
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
 
 210     continue
 
c.... There is no child for material MATELT of element IELT.
c.... This material must appear as a child at IREMOV.  Make this
c.... dudded child into the necessary child at ISURV.
 
         nod=isn1(iparent(iremov))
         if (nod.eq.0) then
            write(logmess,'(a,i9,a,i9,a,i9)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FATAL ERROR : topological error at node ',iremov
            call writloga('default',0,logmess,0,ierrw)
            call termcode()
         endif
         do k=1,10000
            if (imt1(nod).eq.matelt.and.itp1(nod).ne.ifitpcup) 
     *         goto 220
            nod=isn1(nod)
            if (nod.eq.iparent(iremov)) goto 230
         enddo
         write(logmess,'(a,i9,a,i9,a,i9)')
     &      'Try ',isurv,'<--',iremov,
     &      ':  FATAL ERROR : topological error at node ',iremov
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
 
 230     continue
         write(logmess,'(a,i9,a,i9,a,i9,a)')
     &      'Try ',isurv,'<--',iremov,
     &      ':  FATAL ERROR : material ',matelt,' unexpected.'
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
 
 220     continue
 
c.... Copy data of a child at ISURV to the child at IREMOV.
c.... Then put this former child at IREMOV into the parent/child
c.... system at ISURV.  Die if first available child at ISURV is
c.... not real.
 
         ichild=isn1(iparsurv)
         itp=itp1(ichild)
         lset1=.false.
         if (itp.ge.ifitpst1.and.itp.le.ifitpen1) lset1=.true.
         lset2=.false.
         if (itp.ge.ifitpst2.and.itp.le.ifitpen2) lset2=.true.
         lreal=lset1.or.lset2
         if (.not.lreal) then
            write(logmess,'(a,i9,a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FATAL ERROR.'
            call writloga('default',0,logmess,0,ierrw)
            call termcode()
         endif


c.... Take NOD out of parent/child system at IREMOV.
         iparent(nod)=iparsurv
         lmoved(nod)=.true.
 
         nod1=iparremov
         nod2=isn1(nod1)
         do k=1,10000
            if (nod2.eq.nod) then
               isn1(nod1)=isn1(nod)
               goto 370
            endif
            nod1=nod2
            nod2=isn1(nod2)
         enddo
         logmess='FATAL ERROR'
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
 
 370     continue

c.... Put NOD into parent/child system at ISURV.
 
         write(cbuf,'(10(a,i9))') 'copypts/',
     &      ichild,',',ichild,', 1 / ',nod,', 1 ; finish'
         call dotaskx3d(cbuf,ierr)

c.... Correct color of NOD
 
         imt1(nod)=matelt
 
c
         nod1=iparsurv
         nod2=isn1(nod1)
         do k=1,10000
            if ((matelt.lt.imt1(nod2).and.nod1.eq.iparsurv).or.
     &         (matelt.lt.imt1(nod2).and.matelt.gt.imt1(nod1))
     &         .or.nod2.eq.iparsurv) then
               isn1(nod)=isn1(nod1)
               isn1(nod1)=nod
               goto 310
            endif
            nod1=nod2
            nod2=isn1(nod2)
         enddo
         logmess='FATAL ERROR'
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
 
 310     continue
         itet(ksurv+itetoff(ielt))=nod
 200  continue
 
c.... Loop over child points and make sure each child material
c.... is the material of at least one incident tet.  If not,
c.... dud out this child.
 
      nod1=iparsurv
      nod2=isn1(iparsurv)
 
c.... We assume that iparsurv IS a parent node (type IFITPCUP)
c.... AND ISN1(IPARSURV).ne.IPARSURV.  If this isn't true, die.
 
      if (nod1.eq.nod2.or.itp1(nod1).ne.ifitpcup) then
         logmess='FATAL ERROR'
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
      endif
 
      do k=1,10000
         do j=1,nelts
            if (itetclr(ielts(j)).eq.imt1(nod2)) goto 340
         enddo
 
c.... Dud out NOD2.
 
         isn1(nod1)=isn1(nod2)
         itp1(nod2)=ifitpmrg
         goto 350
 
c.... Advance to next node.
 
 340     continue
         nod1=nod2
 
 350     continue
         nod2=isn1(nod2)
         if (nod2.eq.iparsurv) goto 360
 
      enddo
      logmess='FATAL ERROR'
      call writloga('default',0,logmess,0,ierrw)
      call termcode()
 
 360  continue
 
c.... Check we aren't down to just one child.  If so,
c.... change point types and get rid of parent point.
 
      nod=isn1(iparsurv)
      if (nod.eq.iparsurv) then
         logmess='FATAL ERROR'
         call writloga('default',0,logmess,0,ierrw)
         call termcode()
      endif
      if (isn1(nod).eq.iparsurv) then
         isn1(nod)=0
         iparent(nod)=nod
         itp1(iparsurv)=ifitpmrg
         if (itp1(nod).eq.ifitpini) then
            itp1(nod)=ifitpint
         elseif (itp1(nod).eq.ifitpirb) then
            itp1(nod)=ifitprfl
         elseif (itp1(nod).eq.ifitpvif) then
            itp1(nod)=ifitpvfb
         elseif (itp1(nod).eq.ifitpalb) then
            itp1(nod)=ifitpvrf
         elseif (itp1(nod).eq.ifitpifb) then
            itp1(nod)=ifitpfre
         elseif (itp1(nod).eq.ifitpirf) then
            itp1(nod)=ifitprfb
         elseif (itp1(nod).eq.ifitpvir) then
            itp1(nod)=ifitpvrb
         else
            write(logmess,'(10(a,i9))')
     &         'Try ',isurv,'<--',iremov,
     &         ':  WARNING : node ',nod,' now has '
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(10(a,i9))')
     &         'itp=',itp1(nod),' and only 1 material.'
     &
            call writloga('default',0,logmess,0,ierrw)
         endif
      endif
 
      return
      end
