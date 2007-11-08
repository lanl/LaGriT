      subroutine delaunay2d_lg(ntetmx,ntets)
C
C#######################################################################
C
C     PURPOSE -
C
C       Add points in lstptl to existing mesh using the point insertion
C       algorithm.
C
C     INPUT ARGUMENTS -
C
C        ntetmx  estimate of max number of tetrahedra
C
C     OUTPUT ARGUMENTS _
C
C
C     CHANGE HISTORY -
C
C        $Log: delaunay2d_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   02 Apr 2004 13:36:28   gable
CPVCS    Get rid of loop which zeros out the ioff array.
CPVCS    
CPVCS       Rev 1.9   11 Apr 2001 16:42:50   dcg
CPVCS    use itemp1 and integer ismax
CPVCS
CPVCS       Rev 1.8   10 Jan 2001 14:31:44   dcg
CPVCS    remove duplicate and unused declarations
CPVCS
CPVCS       Rev 1.7   07 Dec 2000 13:39:20   dcg
CPVCS    correct error in not setting nnodes after adding nodes to
CPVCS    fix small elements
CPVCS    change tests for 'inside' for 2d connect
CPVCS
CPVCS       Rev 1.6   28 Nov 2000 15:10:24   dcg
CPVCS     clear mark_path if error exit
CPVCS
CPVCS       Rev 1.5   26 Oct 2000 11:35:20   dcg
CPVCS    change test for 'inside of circumsphere'
CPVCS
CPVCS       Rev 1.4   05 May 2000 15:10:36   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.3   Thu Oct 07 16:51:22 1999   dcg
CPVCS    replace expensive inner loop with more efficient one.
CPVCS
CPVCS       Rev 1.2   Mon Aug 30 15:14:34 1999   dcg
CPVCS    remove calls to ssort routines replace with hpsort
CPVCS
CPVCS       Rev 1.1   Thu Apr 29 11:38:14 1999   dcg
CPVCS    fix incircle test
CPVCS
CPVCS       Rev 1.0   Fri Mar 19 13:14:44 1999   dcg
CPVCS    Initial revision.
 
      implicit none
 
      character*132 logmess
C
 
      real*8 alargenumber,asmallnumber,atolerance
      parameter (alargenumber=1.d+30, asmallnumber=1.d-10)
      parameter (atolerance=1.d+8)
      include 'chydro.h'
      include 'cmerge.h'
      include 'search.h'
      include 'consts.h'
C
C#######################################################################
C
 
      integer   ismaxi, ismin
      real*8 cvmgm, cvmgt, cvmgz
      integer lpath,itetuse,idx,ivertex,mpnt,markmin,
     *  nit,itetest,itetmin,npath,nstack,ntemp,lentemp,itemp11,
     *  icscode,itetnoo,nblocks,itethih,itetlow,jj,jk,iposo,ii,
     *  it,nlstmov,iinterior,ip1,ip2,iface,imove,
     *  minp,maxp,next,last,
     *  iboundary,leni,icmotype,ierror,ilstpt,i1,jtetlcl,n,ierr,
     *  itetdata(2,3)
      real*8 rtestmin,vololdt,d,ssum,volnewt,xdone,epsilonv,epsilona,
     * xn,xx,yn,yx,zn,zx
C
C#######################################################################
      pointer (iplstptl,lstptl(*))
      pointer (iplstold,lstold(*)    )
      pointer (ipitetol,itetold(3,*) )
      pointer (ipitetol,itetold1(*)  )
      pointer (ipjtetol,jtetold(3,*) )
      pointer (ipjtetol,jtetold1(*)  )
      pointer (ipifacol,ifacold(*)   )
      pointer (ipifacne,ifacnew(*)   )
      pointer (ipifacou,ifacout(*)   )
      pointer (ipvolold,volold(*)    )
      pointer (iplstmov,lstmov(*)    )
C        *** POINTERS RELATED TO OLD TETRAHEDRA IN INSERTION POLYHEDRON.
      pointer (ipitetne,itetnew(3,*) )
      pointer (ipitetne,itetnew1(*)  )
      pointer (ipjtetne,jtetnew(3,*) )
      pointer (ipjtetne,jtetnew1(*)  )
      pointer (ipvolnew,volnew(*)    )
      pointer (ipxvorne,xvornew(*)   )
      pointer (ipyvorne,yvornew(*)   )
      pointer (ipzvorne,zvornew(*)   )
c pointers for linked list
      pointer (ipioff,ioff)
      pointer (ipifacelst,ifacelst)
      integer ioff(*), ifacelst(6,*)
 
C        *** POINTERS RELATED TO NEW TETRAHEDRA IN INSERTION POLYHEDRON.
 
      pointer (iprtest,rtest(*)     )
      pointer (ipmark_path,mark_path(*) )
      pointer (ipipath,ipath(*) )
      integer ipath,mark_path,lstptl,lstold,itetold,itetold1,jtetold,
     *  jtetold1,ifacold,ifacnew,ifacout,lstmov,itetnew,itetnew1,
     *  jtetnew, jtetnew1,i,j,npoints,ntets,ntetmx,ntetsm,leno,iranidx,
     *  mbndry,nfailc
      real*8 volold,volnew,xvornew,yvornew,zvornew,rtest,
     *  test1
C        *** POINTERS RELATED TO WALKING ALGORITHM
C
      character*32 isubname,cmo
      pointer (ipitet,itet),(ipjtet,jtet)
      pointer (ipitet,itet1),(ipjtet,jtet1)
      integer itet(3,*),jtet(3,*),itet1(*),jtet1(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      data itetdata/2,3, 3,1, 1,2/
C
C#######################################################################
C
C     MACROS.
C
 
C#######################################################################
C     Access the mesh object.
C
      isubname='delaunay'
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,leni,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntetsm,leni,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
      call getsize(xn,xx,yn,yx,zn,zx,epsilona,epsilonv)
C
C    Some temporary memory for walking algorithm
C    ******************************************************************
      call mmgetblk('rtest',isubname,iprtest,ntetmx,2,icscode)
      call mmgetblk('ipath',isubname,ipipath,ntetmx,1,icscode)
      call mmgetblk('mark_path',isubname,ipmark_path,ntetmx,1,icscode)
C
C     GET MEMORY RELATED TO OLD TETRAHEDRA IN A POLYHEDRON.
C
      lenold=lenblk
      call mmgetblk('lstold',isubname,iplstold,lenold,1,icscode)
      call mmgetblk('itetold',isubname,ipitetol,3*lenold,1,icscode)
      call mmgetblk('jtetold',isubname,ipjtetol,3*lenold,1,icscode)
      call mmgetblk('ifacold',isubname,ipifacol,3*lenold,1,icscode)
      call mmgetblk('ifacnew',isubname,ipifacne,3*lenold,1,icscode)
      call mmgetblk('ifacout',isubname,ipifacou,3*lenold,1,icscode)
      call mmgetblk('volold',isubname,ipvolold,lenold,2,icscode)
      call mmgetblk('lstmov',isubname,iplstmov,lenold,1,icscode)
C
C     GET MEMORY RELATED TO NEW TETRAHEDRA IN A POLYHEDRON.
C
      lennew=3*lenold
      call mmgetblk('itetnew',isubname,ipitetne,3*lennew,1,icscode)
      call mmgetblk('jtetnew',isubname,ipjtetne,3*lennew,1,icscode)
      call mmgetblk('volnew',isubname,ipvolnew,lennew,2,icscode)
      call mmgetblk('xvornew',isubname,ipxvorne,lennew,2,icscode)
      call mmgetblk('yvornew',isubname,ipyvorne,lennew,2,icscode)
      call mmgetblk('zvornew',isubname,ipzvorne,lennew,2,icscode)
      call mmgetblk('temp',isubname,iptemp,6*lennew,2,icscode)
      leni = npoints
      call mmgetblk('ioff',isubname,ipioff,leni,1,icscode)
      leni = 6000
      call mmgetblk('ifacelst',isubname,ipifacelst,leni,1,icscode)
C
C     Refresh pointers
C
      call mmfindbk('ibint',nname,ipibint,lennew,icscode)
      call mmfindbk('lstptl',nname,iplstptl,lennew,icscode)
      call mmfindbk('xvor',nname,ipxvor,lennew,icscode)
      call mmfindbk('yvor',nname,ipyvor,lennew,icscode)
      call mmfindbk('zvor',nname,ipzvor,lennew,icscode)
      call mmfindbk('lstcns1',nname,iplstcns1,lennew,icscode)
      call mmfindbk('lstcns2',nname,iplstcns2,lennew,icscode)
      call mmfindbk('lsttts',nname,iplsttts,lennew,icscode)
      call mmfindbk('vol',nname,ipvol,lennew,icscode)
      call mmfindbk('lstfail',nname,iplstfal,lennew,icscode)
      ifailv=0
      ifailr=0
      do i=1,npoints
         ioff(i)=0
      enddo
C
C    Main loop to insert points one at a time - access nodes in
C    a random order
C
      if(istep.eq.1) then
         test1=10.*smalarea
      elseif (istep.eq.2) then
         test1=min(smalarea/100.,epsilona)
      else
         test1=0.0
      endif
      do 5000 ilstpt=1,nlstptl
         if (nlstptl.gt.100) then
            call primestep(nlstptl,iranidx)
            mpnt=lstptl(iranidx)
         else
            iranidx=ilstpt
            mpnt=lstptl(ilstpt)
         endif
         ifail=0
         nfailc=0
C
C        ...............................................................
C        DEFINE AN ARRAY rtest, WHICH, WHEN POSITIVE, INDICATES THAT THE
C        MASS POINT LIES WITHIN THE CIRCUMBALL OF THE TETRAHEDRON.
C
C        NOTE: IF ANY ABSOLUTE VALUE OF rtest IS BELOW A THRESHOLD, WE
C              HAVE A DEGENERACY.  IF IT IS STEP 1, ADD THIS MASS POINT
C              TO THE FAIL LIST AND GO TO THE NEXT MASS POINT.
C              OTHERWISE, DECIDE WHAT DEGENERATE TETRAHEDRA SHOULD BE
C              INCLUDED IN THE LIST.
C
C  Find a tet whose circumball contains the query point, mpnt
C  Start with last tet added
C  use jtet array to walk along path from test tet to query point.
            lpath=0
            itetuse = ntets
            idx=ibint(itetuse)
            ivertex=min(itet(1,idx),itet(2,idx),itet(3,idx))
            rtest(itetuse)=(xic(ivertex)-xvor(idx))**2+
     *                     (yic(ivertex)-yvor(idx))**2+
     *                     (zic(ivertex)-zvor(idx))**2-
     *                     (xic(mpnt)-xvor(idx))**2-
     *                     (yic(mpnt)-yvor(idx))**2-
     *                     (zic(mpnt)-zvor(idx))**2
            if (rtest(itetuse).ge.smalarea) go to 110
            if((istep.eq.2.and.rtest(itetuse).gt.smalarea/100.0).or.
     *         (istep.eq.3.and.rtest(itetuse).gt.0.0)  ) go to 110
C
C  Get neighbors of itetuse
C  Choose next tet on path by finding the neighbor that has been
C  visited the fewest times (min of mark) that has the closest rtest.
C
 102        lpath=lpath+1
 	    if(lpath.gt.ntets) then
 	        write(logmess,104) mpnt
 104            format('path longer that ntets for point ',i10)
 	        call writloga ('default',0,logmess,0,icscode)
                do npath=1,lpath-1
                   mark_path(ipath(npath))=0
                enddo
                ifailc=ifailc+1
                ifail=1
                go to 4980
 	     endif
             mark_path(itetuse)=mark_path(itetuse)+1
             ipath(lpath)=itetuse
             markmin=1000000
             do nit=1,3
                if(jtet(nit,itetuse).ne.mbndry.and.
     *                jtet(nit,itetuse).ne.0) then
                   if(jtet(nit,itetuse).gt.mbndry) then
                      itetest=1+(jtet(nit,itetuse)-mbndry-1)/3
                   else
                      itetest=1+(jtet(nit,itetuse)-1)/3
                   endif
                   if(mark_path(itetest).le.markmin) then
                      if(mark_path(itetest).lt.markmin)
     *                   rtestmin=alargenumber
                      markmin=mark_path(itetest)
                      idx=ibint(itetest)
                      ivertex=min(itet(1,idx),itet(2,idx),
     * 			                  itet(3,idx))
                      rtest(itetest)=(xic(ivertex)-xvor(idx))**2+
     *                     (yic(ivertex)-yvor(idx))**2+
     *                     (zic(ivertex)-zvor(idx))**2-
     *                     (xic(mpnt)-xvor(idx))**2-
     *                     (yic(mpnt)-yvor(idx))**2-
     *                     (zic(mpnt)-zvor(idx))**2
                      if(rtest(itetest).ge.test1) then
                         itetuse=itetest
                         go to 110
                      endif
                      if (rtest(itetest).ge.asmallnumber) then
                        nfailc=nfailc+1
                        if((istep.eq.1.and.nfailc.gt.20).or.
     *                     (istep.eq.2.and.nfailc.gt.100).or.
     *                     (istep.eq.3.and.nfailc.gt.1000)) then
                           do n=1,lpath
                             mark_path(ipath(n))=0
                           enddo
                           ifailc=ifailc+1
                           ifail=1
                           go to 4980
                        endif
                      endif
                      if(abs(rtest(itetest)).lt.rtestmin) then
                         rtestmin=abs(rtest(itetest))
                         itetmin=itetest
                      endif
                   endif
                endif
             enddo
             itetuse=itetmin
             go to 102
C
C  We have found a tet whose circumball contains or nearly contains the
C  query point.
C  Place tets in the cavity list (lsttts) if a tighter criterion is met
C
 110         do npath=1,lpath
                mark_path(ipath(npath))=0
             enddo
             nlstold=0
             if(istep.eq.1) then
                addeps=0
             else
                addeps= float(mod(istep+1,3)-1)*smalarea
             endif
C
C  Always check selected tri and its  neighbors
C  Add to cavity list if meets criterion
C  If no test is satisfied add point to fail list
C  Create a stack and push potential tets on stack with their neighbors
C  Pop tets off stack one at a time and quit when stack is empty
C  Initialize stack
             nstack=1
             ntemp=1
             itemp1(ntemp)=itetuse
             ipath(nstack)=itetuse
             mark_path(itetuse)=1
C
C  Pop a tet off the stack
C
             call mmgetlen(iptemp,lentemp,icscode)
 112         itetuse=ipath(nstack)
             nstack=nstack-1
             rtest(itetuse)=rtest(itetuse)+addeps
             if(istep.eq.1) then
               if(abs(rtest(itetuse)).lt.smalarea) then
                   do n=1,ntemp
                      mark_path(itemp1(n))=0
                   enddo
                   ifail=1
                   go to 4980
               endif
             elseif(istep.gt.1) then
                if(abs(rtest(itetuse)).lt.0.0) then
                   do n=1,ntemp
                      mark_path(itemp1(n))=0
                   enddo
                   ifail=1
                   go to 4980
                endif
             endif
             nlstold=nlstold+1
             lsttts(nlstold)=itetuse
 
C
C  Check all neighbors of popped tet
C  put on stack if pass test and if not already on stack
C
             do nit=1,3
                if(jtet(nit,itetuse).ne.mbndry.and.
     *             jtet(nit,itetuse).ne.0) then
                   if(jtet(nit,itetuse).gt.mbndry) then
                      itetest=1+(jtet(nit,itetuse)-mbndry-1)/3
                   else
                      itetest=1+(jtet(nit,itetuse)-1)/3
                   endif
                   if(mark_path(itetest).eq.0) then
                      mark_path(itetest)=1
                      if((ntemp+1).gt.(2*lentemp-1)) then
                         lentemp=ntemp+10000
                         call mmnewlen('temp',isubname,iptemp,
     *                                 lentemp,icscode)
                      endif
                      ntemp=ntemp+1
                      itemp1(ntemp)=itetest
                      idx=ibint(itetest)
                      ivertex=min(itet(1,idx),itet(2,idx),
     * 			                  itet(3,idx))
                      rtest(itetest)=(xvor(idx)-xic(ivertex))**2
     *                    +(yvor(idx)-yic(ivertex))**2
     *                    +(zvor(idx)-zic(ivertex))**2
     *                    -(xvor(idx)-xic(mpnt))**2
     *                    -(yvor(idx)-yic(mpnt))**2
     *                    -(zvor(idx)-zic(mpnt))**2
                      if(rtest(itetest).ge.test1) then
                         nstack=nstack+1
                         ipath(nstack)=itetest
                      endif
                    endif
                endif
             enddo
             if (nstack.ne.0) go to 112
             do n=1,ntemp
                mark_path(itemp1(n))=0
             enddo
             if (nlstold.eq.0) then
                 ifail=1
                 go to 4980
             endif                              	
             if(nlstold.gt.0)call hpsorti(nlstold,lsttts(1))
C        ...............................................................
C        INCREASE MEMORY, IF NECESSARY.
C
           call mmfindbk('lstold',isubname,iplstold,lenold,icscode)
           if (icscode.ne.0 ) then
C     GET MEMORY RELATED TO OLD TETRAHEDRA IN A POLYHEDRON.
           lenold=lenblk
           call mmgetblk('lstold',isubname,iplstold,lenold,1,icscode)
           call mmgetblk('itetold',isubname,ipitetol,3*lenold,1,icscode)
           call mmgetblk('jtetold',isubname,ipjtetol,3*lenold,1,icscode)
           call mmgetblk('ifacold',isubname,ipifacol,3*lenold,1,icscode)
           call mmgetblk('ifacnew',isubname,ipifacne,3*lenold,1,icscode)
           call mmgetblk('ifacout',isubname,ipifacou,3*lenold,1,icscode)
           call mmgetblk('volold',isubname,ipvolold,lenold,2,icscode)
           call mmgetblk('lstmov',isubname,iplstmov,lenold,1,icscode)
C
C     __________________________________________________________________
C
         else
            if(nlstold.gt.lenold) then
               leno=nlstold+lenblk
               call mmnewlen('lstold',isubname,iplstold,leno,icscode)
               call mmnewlen('itetold',isubname,ipitetol,3*leno,
     *                    icscode)
               call mmnewlen('jtetold',isubname,ipjtetol,3*leno,
     *                    icscode)
               call mmnewlen('ifacold',isubname,ipifacol,3*leno,
     *                    icscode)
               call mmnewlen('ifacnew',isubname,ipifacne,3*leno,
     *                    icscode)
               call mmnewlen('ifacout',isubname,ipifacou,3*leno,
     *                    icscode)
               call mmnewlen('volold',isubname,ipvolold,leno,icscode)
               call mmnewlen('lstmov',isubname,iplstmov,leno,icscode)
            endif
         endif
C     GET MEMORY RELATED TO NEW TETRAHEDRA IN A POLYHEDRON
         call mmfindbk('volnew',isubname,ipvolnew,lennew,icscode)
         if (icscode.ne.0 ) then
            lennew=3*lenblk
            call mmgetblk('itetnew',isubname,ipitetne,3*lennew,1,
     *            icscode)
            call mmgetblk('jtetnew',isubname,ipjtetne,3*lennew,1,
     *            icscode)
            call mmgetblk('volnew',isubname,ipvolnew,lennew,2,icscode)
            call mmgetblk('xvornew',isubname,ipxvorne,lennew,2,icscode)
            call mmgetblk('yvornew',isubname,ipyvorne,lennew,2,icscode)
            call mmgetblk('zvornew',isubname,ipzvorne,lennew,2,icscode)
            call mmgetblk('temp',isubname,iptemp,6*lennew,2,icscode)
         else
            if(nlstold.gt.lenold) then
               lennew=3*(nlstold+lenblk)
               lenold=nlstold+lenblk
               call mmnewlen('itetnew',isubname,ipitetne,3*lennew,
     *               icscode)
               call mmnewlen('jtetnew',isubname,ipjtetne,3*lennew,
     *               icscode)
               call mmnewlen('volnew',isubname,ipvolnew,lennew,icscode)
               call mmnewlen('xvornew',isubname,ipxvorne,lennew,icscode)
               call mmnewlen('yvornew',isubname,ipyvorne,lennew,icscode)
               call mmnewlen('zvornew',isubname,ipzvorne,lennew,icscode)
               call mmnewlen('temp',isubname,iptemp,6*lennew,icscode)
            endif
         endif
C
C        ...............................................................
C        COPY THE TETRAHEDRON LIST INTO ARRAY lstold.
C
         do 300 ii=1,nlstold
            lstold(ii)=lsttts(ii)
  300    continue
C
C
C        _______________________________________________________________
C        COPY itet AND jtet FOR THE OLD TETRAHEDRA INSIDE THE INSERTION
C        POLYHEDRON FROM THE itet AND jtet ARRAYS INTO LOCAL ARRAYS.
C        ALSO, SET itet TO A NEGATIVE VALUE FOR THESE TETRAHEDRA.
C        ALSO, COPY vol INTO THE LOCAL ARRAY volold.
C
 
         do ii=1,nlstold
            itetold(1,ii)=itet(1,lstold(ii))
            itetold(2,ii)=itet(2,lstold(ii))
            itetold(3,ii)=itet(3,lstold(ii))
            jtetold(1,ii)=cvmgt(jtet(1,lstold(ii)),
     $                          jtet(1,lstold(ii))-mbndry,
     $                          jtet(1,lstold(ii)).lt.mbndry
     $                         )
            jtetold(1,ii)=max(jtetold(1,ii),0)
            jtetold(2,ii)=cvmgt(jtet(2,lstold(ii)),
     $                          jtet(2,lstold(ii))-mbndry,
     $                          jtet(2,lstold(ii)).lt.mbndry
     $                         )
            jtetold(2,ii)=max(jtetold(2,ii),0)
            jtetold(3,ii)=cvmgt(jtet(3,lstold(ii)),
     $                          jtet(3,lstold(ii))-mbndry,
     $                          jtet(3,lstold(ii)).lt.mbndry
     $                         )
            jtetold(3,ii)=max(jtetold(3,ii),0)
            itet(1,lstold(ii))=-itet(1,lstold(ii))
            itet(2,lstold(ii))=-itet(2,lstold(ii))
            itet(3,lstold(ii))=-itet(3,lstold(ii))
            volold(ii)=vol(lstold(ii))
         enddo
C
C        _______________________________________________________________
C        CREATE A LIST OF THE SURFACE TRIANGLES OF THE INSERTION
C        POLYHEDRON IN RELATION TO THE OLD TETRAHEDRA (WITH SINGLE-
C        SUBSCRIPTED REFERENCES) INSIDE THE POLYHEDRON.
C        ...............................................................
C        SET UP THE SURFACE-TRIANGLE ARRAY, AND A DEBUG ARRAY.
C
         do  ii=1,nlstold
            ifacold(3*ii-2)=cvmgt(lstold(ii)*3-2,0,
     $                            jtetold(1,ii).eq.0.or.
     $                            itet1(jtetold(1,ii)).gt.0
     $                           )
            ifacold(3*ii-1)=cvmgt(lstold(ii)*3-1,0,
     $                            jtetold(2,ii).eq.0.or.
     $                            itet1(jtetold(2,ii)).gt.0
     $                           )
            ifacold(3*ii  )=cvmgt(lstold(ii)*3,0,
     $                            jtetold(3,ii).eq.0.or.
     $                            itet1(jtetold(3,ii)).gt.0
     $                           )
 
            itemp11=cvmgz(0,1,
     $                      min(ifacold(3*ii-2),ifacold(3*ii-1),
     $                           ifacold(3*ii  )))
            itemp1(ii)=itemp11
C              *** NONZERO temp1 INDICATES THAT ALL FOUR TETRAHEDRON
C              *** FACES ARE SURFACE TRIANGLES.
         enddo
C
C        ...............................................................
C        CHECK TO MAKE SURE THAT AT LEAST ONE FACE OF EACH TETRAHEDRON
C        IS AN INTERIOR FACE.  THIS CONDITION CAN BE VIOLATED ONLY IF A
C        TETRAHEDRON DOES NOT BELONG TO THE CURRENT POLYHEDRON.  IT CAN
C        HAPPEN IN CASE OF A THIN TETRAHEDRON WHOSE CIRCUMRADIUS IS
C        LARGE.
C                    SET A FLAG, iabort.  IF IT EVER HAPPENS, THE VOLUME
C                    TESTS (TO BE APPLIED LATER) SHOULD CATCH THIS, AND
C                    ADD THE POINT TO THE FAIL LIST.  THE CODE WILL BE
C                    ABORTED IF iabort IS NONZERO AND THE VOLUME TESTS
C                    DO NOT FAIL.
 
         iabort=0
         if(itemp1(ismaxi(nlstold,itemp1(1),1)).ne.0.0.and.nlstold.gt.1)
     $      iabort=1
 
C        ...............................................................
C        COMPRESS THE SURFACE-TRIANGLE ARRAY.
C
         call kmprsn(3*nlstold,ifacold(1),1,ifacold(1),1,ifacold(1),1,
     $               nfacold)
C
C        ...............................................................
C        DEFINE THE REMAINING COUNTERS ASSOCIATED WITH THE INSERTION
C        POLYHEDRON FACES.  ALSO, RETURN TO THIS SECTION IF WE ARE
C        UNABLE TO MERGE THE POINT BEING ADDED.
C
         nfacnew=nfacold
C           *** NUMBER OF INSERTION POLYHEDRON FACES EXCLUDING THOSE
C           *** TOUCHING THE MERGER POINT [POINT INTO WHICH THE POINT
C           *** BEING ADDED (mpnt) IS MERGED].  THE MERGER POINT WILL
C           *** BE DESIGNATED AS mpntmrg.
         nfacout=0
C           *** NUMBER OF FACES OF THE INSERTION POLYHEDRON TOUCHING
C           *** THE MERGER POINT.
         nlstnew=nfacnew
C           *** NUMBER OF NEW TETRAHEDRA.
C
C        _______________________________________________________________
C
C   DEFINE itet FOR NEW TETRAHEDRA.
C
         do  ii=1,nlstnew
            ifacnew(ii)=ifacold(ii)
            itetnoo=(ifacold(ii)+2)/3
            itetnew(1,ii)=iabs(itet(1,itetnoo))
            itetnew(2,ii)=iabs(itet(2,itetnoo))
            itetnew(3,ii)=iabs(itet(3,itetnoo))
            itetnew(ifacold(ii)-3*(itetnoo-1),ii)=mpnt
         enddo
C   _______________________________________________________________
C
C        COMPUTE NEW TETRAHEDRON VOLUMES AND VORONOI POINTS.  IN DOING
C        SO, LOOP OVER TETRAHEDRA IN BLOCKS OF SIZE "LENBLK."
C
         nblocks=min(1+(nlstnew-1)/lenblk,nlstnew)
         do 2000 n=1,nblocks
            itetlow=(n-1)*lenblk+1
            itethih=min(n*lenblk,nlstnew)
C           ............................................................
C           COMPUTE triangle VOLUMES AND VORONOI POINTS.
C
            do  it=1,itethih-itetlow+1
              call volume_tri(xic(itetnew(1,it)),yic(itetnew(1,it)),
     *         zic(itetnew(1,it)),xic(itetnew(2,it)),yic(itetnew(2,it)),
     *         zic(itetnew(2,it)),xic(itetnew(3,it)),yic(itetnew(3,it)),
     *         zic(itetnew(3,it)),voltet(it))
              call voronoi_center_2d(xic(itetnew(1,it)),
     *         yic(itetnew(1,it)),
     *         zic(itetnew(1,it)),xic(itetnew(2,it)),yic(itetnew(2,it)),
     *         zic(itetnew(2,it)),xic(itetnew(3,it)),yic(itetnew(3,it)),
     *         zic(itetnew(3,it)),rl(1,it),rl(2,it),rl(3,it))
               volnew(itetlow+it-1)=voltet(it)
               xvornew(itetlow+it-1)=rl(1,it)
               yvornew(itetlow+it-1)=rl(2,it)
               zvornew(itetlow+it-1)=rl(3,it)
            enddo
C
C           ............................................................
C
 2000    continue
C
C        _______________________________________________________________
C
C        CHECK IF THE FOLLOWING TWO CONDITIONS ARE SATISFIED.  IF NOT,
C        ADD THIS MASS POINT TO THE FAIL LIST AND GO TO THE NEXT POINT.
C
C        1. NONE OF THE NEW TETRAHEDRON VOLUMES IS BELOW A THRESHOLD
C           VALUE. This threshold is much smaller if a vertex of the
c           insertion cavity if one of the 'bigtet' nodes.
C
C        2. THE SUM OF THE NEW TETRAHEDRON VOLUMES IS EQUAL TO THE
C           INSERTION POLYHEDRON VOLUME.
C
         vololdt=ssum(nlstold,volold(1),1)
         jj=ismin(nlstnew,volnew(1),1)
         if(volnew(jj).le.epsilona) then
            do ii=1,nlstold
               do jk=1,3
                 if(iabs(itetold(jk,ii)).ge.ibigtet.and.
     *                  ibigtet.gt.0) then
                    d=volnew(jj)
                    if(d.gt.epsilona*asmallnumber) go to 2099
                 endif
               enddo
            enddo
            ifail=1
            ifailv=ifailv+1
            do  ii=1,nlstold
               itet(1,lstold(ii))=-itet(1,lstold(ii))
               itet(2,lstold(ii))=-itet(2,lstold(ii))
               itet(3,lstold(ii))=-itet(3,lstold(ii))
            enddo
C              *** RESET itetS BACK TO POSITIVE.
            goto 4980
         endif
 2099    continue
         volnewt=ssum(nlstnew,volnew(1),1)
         if(abs((volnewt-vololdt)/vololdt).gt.epsilonr*atolerance) then
            ifail=1
            ifailr=ifailr+1
            do  ii=1,nlstold
               itet(1,lstold(ii))=-itet(1,lstold(ii))
               itet(2,lstold(ii))=-itet(2,lstold(ii))
               itet(3,lstold(ii))=-itet(3,lstold(ii))
            enddo
C              *** RESET itetS BACK TO POSITIVE.
            goto 4980
         endif
C
C        _______________________________________________________________
C        AT THIS STAGE, IF ALL FOUR FACES OF ANY TETRAHEDRON WERE
C        POLYHEDRON FACES, THE POINT SHOULD HAVE GONE INTO THE FAIL
C        LIST. IF IT DID NOT HAPPEN, ABORT THE CODE.
C
         if(iabort.ne.0) call killcode(
     $    'NN3D: All four tetrahedron faces are surface triangles.')
C
C        _______________________________________________________________
C        SET THE jtet VALUES OPPOSITE POLYHEDRON FACES FOR NEW
C        TETRAHEDRA, AND SET THE REMAINING jtet VALUES TO AN UNREALISTIC
C        VALUE FOR THE TIME BEING.  ALSO, CORRECT THE jtet VALUES OF THE
C        TETRAHEDRA SURROUNDING THE INSERTION POLYHEDRON.
C
         do ii=1,nlstnew
            itetnoo=(ifacnew(ii)+2)/3
            iposo=ifacnew(ii)-3*(itetnoo-1)
            jtetnew(1,ii)=-1
            jtetnew(2,ii)=-1
            jtetnew(3,ii)=-1
            jtetnew(iposo,ii)=cvmgt(jtet(iposo,itetnoo),
     $                              jtet(iposo,itetnoo)-mbndry,
     $                              jtet(iposo,itetnoo).lt.mbndry
     $                             )
            jtet1(jtetnew(iposo,ii))
     $       =cvmgt(jtet1(jtetnew(iposo,ii)),3*(ntets+ii-1)+iposo,
     $              jtetnew(iposo,ii).le.0)
         enddo
C        _______________________________________________________________
C        COMPUTE jtet VALUES FOR THE INTERIOR FACES OF NEW TETRAHEDRA.
C
         call mmfindbk('ifacelst',isubname,ipifacelst,leni,icscode)
         if(leni.lt.nlstnew*24)  then
            leni=24*(nlstnew+50)
            call mmincblk('ifacelst',isubname,ipifacelst,leni,icscode)
         endif
         do i=1,leni/6
            do j=1,6
               ifacelst(j,i)=0
            enddo
         enddo
         next=1
         do it=1,nlstnew
            do iface=1,3
               if(jtetnew(iface,it).eq.-1) then
                  ip1=itetnew(itetdata(1,iface),it)
                  ip2=itetnew(itetdata(2,iface),it)
                  minp=min(ip1,ip2)
                  maxp=max(ip1,ip2)
 
C  look for this face in the linked list
C  the list ifacelst has ifacelst(1,j)  the minimum vertex number
c  that is not the insertion node,ifacelst(2,j) the max vertex
c  number that is not the insertion node and ifacelst(3,j) the
c  insertion node, ifacelst(4,j) is the tet the face belongs to
c  ifacelst(5,j) is the local face number
c  ifacelst(6,j) is the pointer to the next entry in the linked
c  list where entries occur in increasing vertex number.
c  if ifacelst(6,j) = 0 then this is the last entry for a
c  given initial vertex value.
c  ioff(node)  is a pointer to the first entry in ifacelst
c  for each initial vertex.
                  if(ioff(minp).eq.0) then
c  add this vertex set to the list - no existing entries for
c  the initial vertex.
                     ioff(minp) = next
                     ifacelst(1,next)=minp
                     ifacelst(2,next)=maxp
                     ifacelst(3,next)=it
                     ifacelst(4,next)=iface
                     ifacelst(5,next)=0
                     next=next+1
                  else
                     idx=ioff(minp)
                     last=0
                     if (ifacelst(5,idx).eq.0) then
c  only one item in list see if insert before or after
c  last=-1 says insert after last item; last=0 says insert before
c  first item
                        if (maxp.gt.ifacelst(2,idx)) last=-1
                     else
                        do while((minp.eq.ifacelst(1,idx)).and.
     *                     (ifacelst(5,idx).ne.0).and.
     *                     (maxp.gt.ifacelst(2,idx)))
                               last=idx
                               idx=ifacelst(5,idx)
                        enddo
                        if (ifacelst(5,idx).eq.0.and.
     *                      maxp.gt.ifacelst(2,idx)) last=-1
                     endif
C  look for matching face already in list or insert this face in list
                     if((minp.eq.ifacelst(1,idx)).and.
     *                  (maxp.eq.ifacelst(2,idx))) then
                        jtetnew(iface,it)=3*(ifacelst(3,idx)+ntets-1)+
     *                           ifacelst(4,idx)
                        jtetnew(ifacelst(4,idx),ifacelst(3,idx))=
     *                           3*(it+ntets-1)+iface
                     else
                         ifacelst(1,next)=minp
                         ifacelst(2,next)=maxp
                         ifacelst(3,next)=it
                         ifacelst(4,next)=iface
                         ifacelst(5,next)=idx
c  insert after last item in list
                         if(last.eq.-1) then
                            ifacelst(5,idx)=next
                            ifacelst(5,next)=0
c  this is new first entry for minp
                         elseif(last.eq.0) then
                            ioff(minp)=next
c  insert this entry between last and idx
                         else
                            ifacelst(5,last)=next
                         endif
                         next=next+1
                     endif
                  endif
               endif
            enddo
         enddo
c         do it=1,npoints
c            ioff(it)=0
c         enddo
         do it=1,nlstnew
            do j=1,3
               ioff(itetnew(j,it))=0
            enddo
         enddo
C
C        _______________________________________________________________
C        INCREASE MEMORY RELATED TO THE TOTAL NUMBER OF TETRAHEDRA, IF
C        NECESSARY.
C
         call mmfindbk('vol',nname,ipvol,leno,icscode)
         if(ntets+nlstnew.gt.ntetmx.or.
     *      ntets+nlstnew.gt.leno) then
            ntetmx=max(ntetmx,ntets+nlstnew+500)
            call cmo_set_info('nelements',cmo,ntetmx,1,1,ierr)
            call cmo_newlen(cmo,ierr)
            call cmo_get_intinfo('mbndry',cmo,mbndry,leni,icmotype,
     *        ierror)
            call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
            call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
            call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
            call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
            call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
            call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
            ntetmaxl=ntetmx-ntetexcl
            call mmnewlen('vol',nname,ipvol,ntetmx,icscode)
            call mmnewlen('xvor',nname,ipxvor,ntetmx,icscode)
            call mmnewlen('yvor',nname,ipyvor,ntetmx,icscode)
            call mmnewlen('zvor',nname,ipzvor,ntetmx,icscode)
            call mmnewlen('ibint',nname,ipibint,ntetmx,icscode)
            call mmnewlen('rtest',isubname,iprtest,ntetmx,icscode)
            call mmnewlen('lsttts',nname,iplsttts,ntetmx,icscode)
            call mmnewlen('ipath',isubname,ipipath,ntetmx,icscode)
            call mmnewlen('mark_path',isubname,ipmark_path,ntetmx,ierr)
         endif
C
C        _______________________________________________________________
C        APPEND NEW TETRAHEDRA TO THE CURRENT LIST.
C
         do it=1,nlstnew
            itet(1,ntets+it)=itetnew(1,it)
            itet(2,ntets+it)=itetnew(2,it)
            itet(3,ntets+it)=itetnew(3,it)
            jtet(1,ntets+it)=jtetnew(1,it)
            jtet(2,ntets+it)=jtetnew(2,it)
            jtet(3,ntets+it)=jtetnew(3,it)
            vol(ntets+it)=volnew(it)
            xvor(ntets+it)=xvornew(it)
            yvor(ntets+it)=yvornew(it)
            zvor(ntets+it)=zvornew(it)
            ibint(ntets+it)=ntets+it
         enddo
C
C        _______________________________________________________________
C        MAKE A LIST OF TETRAHEDRA THAT WILL BE MOVED TO FILL HOLES
C        CREATED BY OLD TETRAHEDRA.  THE PROCESS OF FILLING HOLES IS TO
C        BE ACCOMPLISHED BY STARTING WITH THE LAST TETRAHEDRON AND
C        MOVING IT TO LOCATION lstold(1), THEN MOVING THE LAST BUT 1
C        TETRAHEDRON TO LOCATION lstold(2), AND SO ON AND SO FORTH.
C
         imove=ntets+nlstnew+1
         do  it=ntets+nlstnew,ntets+nlstnew-nlstold+1,-1
            lstmov(imove-it)=cvmgm(0,it,itet(1,it))
         enddo
         call kmprsn(nlstold,lstmov(1),1,lstmov(1),1,lstmov(1),1,
     $               nlstmov)
C
C        _______________________________________________________________
C
C        MOVE NEW TETRAHEDRA TO FILL HOLES CREATED BY OLD TETRAHEDRA.
C        ...............................................................
C        UPDATE THE itet AND jtet VALUES IN SCALAR MODE.
C
         do ii=1,nlstmov
            itet(1,lstold(ii))=itet(1,lstmov(ii))
            itet(2,lstold(ii))=itet(2,lstmov(ii))
            itet(3,lstold(ii))=itet(3,lstmov(ii))
            jtet(1,lstold(ii))=jtet(1,lstmov(ii))
            jtet(2,lstold(ii))=jtet(2,lstmov(ii))
            jtet(3,lstold(ii))=jtet(3,lstmov(ii))
            jtetlcl=cvmgt(jtet(1,lstold(ii)),
     $                    jtet(1,lstold(ii))-mbndry,
     $                    jtet(1,lstold(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=3*lstold(ii)-2
            jtetlcl=cvmgt(jtet(2,lstold(ii)),
     $                    jtet(2,lstold(ii))-mbndry,
     $                    jtet(2,lstold(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=3*lstold(ii)-1
            jtetlcl=cvmgt(jtet(3,lstold(ii)),
     $                    jtet(3,lstold(ii))-mbndry,
     $                    jtet(3,lstold(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=3*lstold(ii)
          enddo
C
C        ...............................................................
C        SET THE REMAINING TETRAHEDRON-RELATED QUANTITIES IN THE NEW
C        LOCATIONS.
C
         do  ii=1,nlstmov
            vol(lstold(ii))=vol(lstmov(ii))
            xvor(lstold(ii))=xvor(lstmov(ii))
            yvor(lstold(ii))=yvor(lstmov(ii))
            zvor(lstold(ii))=zvor(lstmov(ii))
         enddo
C
C        _______________________________________________________________
C
C        INCREMENT/DECREMENT ntets.
C
         ntets=ntets+nlstnew-nlstold
C
C        _______________________________________________________________
C
C        ADD THE CURRENT MASS POINT TO THE FAIL LIST IF IT COULD NOT BE
C        CONNECTED; EITHER BECAUSE OF DEGENERACIES, OR BECAUSE OF SOME
C        RESULTING NEGATIVE-VOLUME OR WAFER-THIN TETRAHEDRA.
C
 4980    continue
         if(ifail.ne.0) then
            nlstfail=nlstfail+1
            lstfail(nlstfail)=mpnt
            if(idelaun.eq.0) then
               lstcns1(nlstfail)=lstcns1(iranidx)
               lstcns2(nlstfail)=lstcns2(iranidx)
            endif
C              *** COMPRESSED CONNECTION NUMBERS WHERE MASS POINTS COULD
C              *** NOT BE ADDED DURING THE STEP.
         endif
C
C        _______________________________________________________________
C
 
         if(mod(ntets,500).eq.0) then
            iinterior=0
            iboundary=0
            do i1=1,npoints
               if(itp1(i1).le.ifitpen1) then
                  iinterior=iinterior+1
               elseif(itp1(i1).le.ifitpen2) then
                  iboundary=iboundary+1
               endif
            enddo
C*****      xdone=100.0*ntets/max(1,(6*iinterior+3*iboundary))
            xdone=100.0*ntets/max(1,6*(iinterior+iboundary))
            write(logmess,4990) ntets,mpnt,nlstfail,xdone
 4990       format(' ntets=',i10,5x,'mpnt=',i9,5x,'nlstfail=',i8,
     *             ' %done=',f7.2)
            call writloga('default',0,logmess,0,icscode)
         endif
C        _______________________________________________________________
C
 5000 continue
C
      call mmrelprt(isubname,icscode)
      return
      end
 
 
 
