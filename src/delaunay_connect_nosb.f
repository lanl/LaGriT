      subroutine delaunay_connect(npoints,ntets,epsilon,
     *  ntetmax,nlsttts)
C
C#######################################################################
C
C     PURPOSE -
C
C   This routine does grid generation by calling a
C   point insertion algorithm
C
C     INPUT ARGUMENTS -
C
C   npoints -- number of nodes in the mesh
C   ntets -- number of elements in the mesh
c   epsilon --
c   ntetmax -- max number of elements
c   nlstts -- number of nodes to add
C
C     OUTPUT ARGUMENTS -
C
C   connected mesh (itet)
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/delaunay_connect_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.10   02 Aug 2005 08:00:28   gable
CPVCS    Changed variable ifadd to lifadd.
CPVCS    
CPVCS       Rev 1.9   18 Jul 2002 11:20:38   dcg
CPVCS    change definitions of smalarea to be used by delaunay in
CPVCS    constructing the insertion cavity
CPVCS    remove idrastic condition
CPVCS    reinstate istep=5
CPVCS    
CPVCS       Rev 1.8   11 Apr 2001 16:43:44   dcg
CPVCS    fix format
CPVCS    
CPVCS       Rev 1.7   08 Mar 2001 16:33:58   dcg
CPVCS    change format statements and warning message
CPVCS    
CPVCS       Rev 1.6   07 Dec 2000 13:40:10   dcg
CPVCS     correct error in not setting nnodes after adding nodes to 
CPVCS     fix small elements
CPVCS    
CPVCS       Rev 1.5   25 Oct 2000 11:22:26   dcg
CPVCS    restore new calculations of smaldist and smalarea
CPVCS    
CPVCS       Rev 1.3   Tue Apr 04 14:45:04 2000   dcg
CPVCS    make 'smal' variable definitions consistent with
CPVCS    dimensions of problem
CPVCS    
CPVCS       Rev 1.2   03 Feb 2000 09:21:20   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:46   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:32   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.5   Fri Oct 31 10:47:24 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.4   Fri Oct 03 17:23:00 1997   dcg
CPVCS    fix looping in multi-material - quit if all points
CPVCS    that were attempted to be added failed
CPVCS
CPVCS       Rev 1.3   Thu Aug 28 10:17:44 1997   dcg
CPVCS    disable step 4 in the point insertion algorithm
CPVCS    it was causing disconnected points and non-delaunay grids
CPVCS
CPVCS       Rev 1.0   Mon Aug 18 14:55:44 1997   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'cmo.h'
      include 'chydro.h'
      integer limithih,limitlow,it,i,j,ierr,k,leni,icmotype,
     *  npoints,ntets,ntetmax,nlsttts
      real*8 epsilon
      pointer (iplstptl,lstptl)
      integer lstptl(1000000)
      character*132 logmess
C
C#######################################################################
C
   90 continue
c      smalluse=small*amin0(istep,2)
      smalluse=small
c      if(idrastic.ne.0) smalluse=200.0*small
      smaldist=smalluse*(boxsizex*boxsizey*boxsizez)**(1.d0/3.d0)
c      smalarea=2.0*smalluse*(boxsizex*boxsizey*boxsizez)**(2.d0/3.d0)
c      smalvol=3.0*smalluse*boxsizex*boxsizey*boxsizez
      smalarea=smalluse*(boxsizex*boxsizey*boxsizez)**(2.d0/3.d0)
      smalvol=smalluse*boxsizex*boxsizey*boxsizez
C
C     __________________________________________________________________
C
C     NOTES:
C
C        istep=1             DEGENERATE POINTS ARE ADDED TO THE FAIL
C                            LIST.
C
C        istep=2--nstepdgn+1 ATTEMPT IS MADE TO CONNECT DEGENERATE
C                            POINTS.
C                            FOR istep=2, TETRAHEDRA THAT LIE WITHIN
C                            AN EPSILON OF ANY CIRCUMBALL ARE EXCLUDED
C                            FROM THE LIST.
C                            FOR istep=3, TETRAHEDRA THAT LIE ON THE
C                            CIRCUMBALLS AND ALL TETRAHEDRA INSIDE THE
C                            CIRCUMBALLS ARE INCLUDED IN THE LIST.
C                            FOR istep=4, TETRAHEDRA THAT LIE BEYOND
C                            AN EPSILON FROM THE CIRCUMBALLS ALSO ARE
C                            INCLUDED.
C                            istep=4 currently disabled as can generate
C                            non-delaunay meshes.
C
C     __________________________________________________________________
C
C
C     ******************************************************************
C
C     CONSTRUCT THE DELAUNAY TETRAHEDRALIZATION BY INSERTING THE MASS
C     POINTS INTO THE EXISTING TETRAHEDRAL STRUCTURE, ONE AT A TIME.
C
 
      nlstfail=0
      limitlow=1
      limithih=nlstptl
      call mmfindbk('ibint',nname,ipibint,leni,ierr)
      if (ierr.ne.0) then
         call mmgetblk('ibint',nname,ipibint,ntetmaxl,1,ierr)
      else
         if(leni.lt.ntetmaxl)
     *      call mmnewlen('ibint',nname,ipibint,ntetmaxl,ierr)
      endif
      do  it=1,ntetmaxl
         ibint(it)=ntetexcl+it
      enddo
      ifailv=0
      ifailr=0
      ifailc=0
      call delaunay(ntetmax,ntets)
C
C  refresh pointers
C
      call mmfindbk('lstptl',nname,iplstptl,leni,ierr)
      call mmfindbk('lstfail',nname,iplstfal,leni,ierr)
C
C   MAKE A NEW LIST OF POINTS THAT COULD NOT BE CONNECTED DURING THE
C   PREVIOUS PASS, AND PREPARE FOR THE NEXT PASS.
C
      if(nlstfail.ne.0) then
C
C   ITERATIONS INVOLVING THE SAME STEP.
C
         if(nlstfail.lt.nlstptl) then
            do  i=1,nlstfail
               lstptl(i)=lstfail(i)
            enddo
            nlstptl=nlstfail
            goto 90
C
C    JUST COMPLETED STEP istep.  PREPARE FOR NEXT STEP.
C
         elseif(istep.le.nstepdgn+1) then
            do  i=1,nlstfail
               lstptl(i)=lstfail(i)
            enddo
            nlstptl=nlstfail
            write(logmess,6200) nlstfail,istep
 6200       format(' There are',i10,
     $       ' points that failed step ',i10)
            call writloga('default',1,logmess,0,ierr)
            do  i=1,nlstfail,10
               write(logmess,6400) (lstfail(j),j=i,min(i+9,nlstfail))
 6400          format(2x,10i10)
               call writloga('bat',0,logmess,0,ierr)
            enddo
            if (ifailr+ifailv.ne.0) then
              write(logmess,6501) istep,ifailr
 6501         format(' At step ',i3,1x,i10,
     $        ' Points failed volume ratio test ')
              call writloga('default',1,logmess,0,ierr)
              write(logmess,6503) ifailv
 6503         format(13x,i10,' points failed min volume test.')
              call writloga('default',1,logmess,0,ierr)
            endif
            if (ifailc.ne.0)then
              write(logmess,6502) ifailc
 6502         format( 'Circumsphere problems ',
     *        'for ',i10,' points')
              call writloga('default',1,logmess,0,ierr)
            endif
c            if (istep.eq.nstepdgn) then
c               istep=1
c               idrastic=1
c            else
               istep=istep+1
               goto 90
            endif
c        endif
C
C        COMPLETED THE LAST STEP, nstepdgn1.  WHATEVER POINTS COULD
C        NOT BE CONNECTED, CANNOT BE CONNECTED FOR THIS CONFIGURATION.
C        IF THE DUD FLAG IS ON, DUD THESE POINTS.  OTHERWISE, SIMPLY
C        RESET idrastic BACK TO ZERO FOR THE NEXT PASS (PASS WITH THE
C        SURROUNDING TETRAHEDRON REMOVED).
C
         if(idud.ne.0) then
            call cmo_get_name(cmo,ierr)
            call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierr)
            call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierr)
            call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierr)
            call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierr)
            write(logmess,7600) nlstfail
 7600       format(i10,' points are being marked because they could',
     $       ' not be connected.')
            call writloga('default',1,logmess,0,ierr)
            do  i=1,nlstfail
               k=lstfail(i)
               write(logmess,7700) k,imt1(k),xic(k),yic(k),zic(k)
 7700          format(x,2i10,x,3e15.7)
               call writloga('default',0,logmess,0,ierr)
            enddo
            do i=1,nlstfail
               itp1(lstfail(i))=itp1(lstfail(i))+1000
            enddo
            idrastic=0
         else
            idrastic=0
         endif
C         nlstfail=0
      endif
C
C     CHECK FOR SMALL TETRAHEDRA, AND IF NECESSARY, ADD MORE POINTS TO
C     ELIMINATE THEM.
C
      if(iaddpts.ne.0.and.lifadd) then
         iaddpts=0
         iaddpass=iaddpass+1
         idelaun=1
C
         call fix_small_tets(npoints,ntets,epsilon,nlsttts)
         if (nlsttts.ne.0) then
            call cmo_set_info('nnodes',cmo,npoints,1,1,ierr)
            istep=1
            go to 90
         endif
      endif
      return
      end
C
