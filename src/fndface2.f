*dk,fndface2
      subroutine fndface2(itt1,itt2,itt3,ibdytet,id1,jd1,iposbest,
     *                    npoints,ntets)
       implicit none
C
C ######################################################################
C
C     PURPOSE -
C
C
C        This routine finds the best face through which to start a
C        marching sequence.  The best face is one which encounters no
C        boundaries during the flips.  The results of this routine
C        are used to determine if an inverted tetrahedron, which is
C        formed during a 2-to-3i flip, can be marched out without
C        affecting the interface.
C
C     INPUT ARGUMENTS -
C
C        itt1,itt2,itt3   -   the three potential new tetrahedra
C                             to be formed by a 2-to-3i flip.
C        ibdytet          -   indicates which of the three new tets
C                             is on the boundary.  The boundary tet
C                             would be inverted as a result of a
C                             2-to-3i flip.
C        id1,jd1          -   hold the itet and jtet information for
C                             the 2-to-3i flip.
C
C     OUTPUT ARGUMENTS -
C
C        iposbest -   =0  =>  there is no marching path which does
C                             not affect the interface.
C                     >0  =>  the position of the face through
C                             which an interior march may start.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/fndface2.f_a  $
CPVCS    
CPVCS       Rev 1.12   30 Sep 2004 10:02:42   dcg
CPVCS    use iand in place of .and. with integer variables
CPVCS    
CPVCS       Rev 1.11   05 Jan 2001 12:57:10   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.10   Wed Nov 10 15:09:18 1999   dcg
CPVCS    remove reference to ihcycle
CPVCS
CPVCS       Rev 1.9   Fri Aug 28 14:24:54 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.8   Wed Jul 09 09:10:04 1997   dcg
CPVCS    add argument ierflg - this indicates if flip is okay
CPVCS
CPVCS       Rev 1.7   Wed Apr 23 11:08:22 1997   dcg
CPVCS    only check for mesh tangling if idebug has
CPVCS    been set
CPVCS
CPVCS       Rev 1.6   Thu Apr 17 16:17:56 1997   dcg
CPVCS    replace call to termcode with return
CPVCS    note warning message printed in this case
CPVCS
CPVCS       Rev 1.5   11/07/95 17:17:52   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.4   12/02/94 16:19:06   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:08   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:16   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:53:14   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:14   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "cmerge.h"
C
C ######################################################################
C
      character*8 isubname
      integer itets(3),id1(12),jd1(12),id(12),jd(12)
      pointer (ipiitmp , iitmp(4,1))
      pointer (ipjjtmp , jjtmp(4,1))
      pointer (ipkttmp , kttmp(1)  )
C
      pointer (ipiisave, iisave(1) )
      pointer (ipjjsave, jjsave(1) )
      integer iitmp,jjtmp,kttmp,
     *  iisave,jjsave,i,j,k,i1,i2,i3,i4,ierror,length,
     *  icmotype,ilen,ictrecvr,maxiter,maxpairs,irecvrfl,iextbdy,
     *  iokflg,len,itinv,itk,it,kpt,niter,n2to3,n2to0,m,
     *  itx,ifposx,it2,ifpos2,it3,ninvrt
      real*8 volitk,volit,crosx1,crosy1,crosz1,xlarge,alargenumber,
     *  volume,distsq
      integer  itt1,itt2,itt3,ibdytet,iposbest,npoints,ntets,
     *  itnext,ierr,ierflg,ifpos,ics,len4,itetcnto,len5
C
C ######################################################################
C
C     MACROS.
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
      distsq(i,j)=(xic(i)-xic(j))**2+(yic(i)-yic(j))**2+
     *            (zic(i)-zic(j))**2
C
C ######################################################################
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      itetcnto=ntets
      xlarge=alargenumber
      iposbest=0
      ictrecvr=0
      maxiter=50
      maxpairs=10
      irecvrfl=0
      iextbdy=0
C
C     ******************************************************************
C
C     SET UP TEMPORARY MEMORY
C
      isubname='fndface2'
      len=200
      len4=800
      call mmgetblk('iitmp',isubname,ipiitmp,len4,1,ics)
      call mmgetblk('jjtmp',isubname,ipjjtmp,len4,1,ics)
      call mmgetblk('kttmp',isubname,ipkttmp,len,1,ics)
C
C
C  if mesh is tangling turn on debug
C
      if (idebug.ge.1) then
         call mmgetblk('iisave',isubname,ipiisave,4*ntets,1,ics)
         call mmgetblk('jjsave',isubname,ipjjsave,4*ntets,1,ics)
         do 10 i=1,4*ntets
            iisave(i)=itet1(i)
            jjsave(i)=jtet1(i)
 10      continue
      endif
C
C     ******************************************************************
C
      do 15 k=1,ntets
         kfix(2,k)=0
 15   continue
C
C     ******************************************************************
C
C     DETERMINE THE POTENTIAL NEW INVERTED TETRAHEDRON.
C
      itinv=itt1
      if (ibdytet .eq. 2) itinv=itt2
      if (ibdytet .eq. 3) itinv=itt3
C
C     ******************************************************************
C
C     LOOP OVER THE 2 FACES, PERFORMING TEMPORARY FLIPS.  RECOVER THE
C     ORIGINAL STATE OF THE MESH BEFORE STARTING WITH A NEW FACE.
C
      do 800 m=3,4
         if (irecvrfl .eq. 0) goto 75
C
C        ...............................................................
C        RECOVER THE ORIGINAL STATE OF THE MESH.
C
         do 20 k=1,ictrecvr
            itk=kttmp(k)
            itet(1,itk)=iitmp(1,k)
            itet(2,itk)=iitmp(2,k)
            itet(3,itk)=iitmp(3,k)
            itet(4,itk)=iitmp(4,k)
            jtet(1,itk)=jjtmp(1,k)
            jtet(2,itk)=jjtmp(2,k)
            jtet(3,itk)=jjtmp(3,k)
            jtet(4,itk)=jjtmp(4,k)
            kfix(2,itk)=0
 20      continue
         ntets=itetcnto
C
 75      continue
C
C        ...............................................................
C        TEMPORARILY PERFORM THE 2-TO-3I FLIP TO PRODUCE THE INVERTED
C        TETRAHEDRON AND TO RESET THE JTET POINTERS AROUND THIS
C        TETRAHEDRON.
C
         ictrecvr=0
         call recvr23(itt1,itt2,itt3,id1(1),jd1(1),ictrecvr,
     *                ipiitmp,ipjjtmp,ipkttmp,
     *                npoints,ntets)
C
         it=itinv
         ifpos=m
         kpt=itet(m,it)
         irecvrfl=1
         niter=0
         npairs2=0
         n2to3=0
         n2to0=0
C
C        ...............................................................
C        BEGIN THE ITERATIONS TO MATCH OUT THE INVERSION.
C
 100     continue
C
         niter=niter+1
         if (niter .gt. maxiter) goto 800
         if (jtet(ifpos,it) .lt. mbndry) then
C
C           ............................................................
C           ONLY PERFORM A TEMPORARY FLIP IF A NON-BOUNDARY FACE IS
C           FOUND.
C
            itx=0.25*dble(jtet(ifpos,it))+0.9
            ifposx=iand((jtet(ifpos,it)-1),maskface) + 1
C
            call find2to3(itx,ifposx,it2,ifpos2,it3,id(1),jd(1),
     *                    npoints,ntets,ierflg)
C
C           ............................................................
C           TEMPORARY DEBUG SECTION.
C
            if (it .ne. it2) then
               write (logdan,5432) niter,it,it2
 5432          format('IT.NE.IT2: niter,it,it2',3i10)
               call writloga('default', 0, logdan, 0, ierr)
C              call termcode(1)
               go to 800
            endif
C
C           ............................................................
C           ADJUST MEMORY FOR THE ARRAY WHICH STORES INFORMATION TO
C           RECOVER THE ORIGINAL CONDITION OF THE MESH.
C
C
            if (ictrecvr+8 .ge. len) then
               len=len+200
               len4=len*4
               len5=len*5
               call mmnewlen('iitmp',isubname,ipiitmp,len4,ics)
               call mmnewlen('jjtmp',isubname,ipjjtmp,len4,ics)
               call mmnewlen('kttmp',isubname,ipkttmp,len,ics)
            endif
C
C           ............................................................
C
C           PERFORM A TEMPORARY 2-TO-3 FLIP, SAVING INFORMATION TO
C           RECOVER THE ORIGINAL STATE OF THE MESH.
C
            call recvr23(itx,it2,it3,id(1),jd(1),ictrecvr,
     *                   ipiitmp,ipjjtmp,ipkttmp,
     *                   npoints,ntets)
            n2to3=n2to3+1
         else
            goto 800
         endif
C
C        ..............................................................
C
C        IF THE 2-TO-3 FLIP OCCURRED, NEXT TRY THE 2-TO-0 FLIP.
C
         itets(1)=itx
         itets(2)=it2
         itets(3)=it3
         do 250 k=1,3
            itk=itets(k)
            if (itk.eq.0 .or. itet(1,itk).le.0) goto 250
            do 210 j=3,4
               if (jtet(j,itk) .lt. mbndry) then
                  if (itet1(jtet(j,itk)) .eq. itet(j,itk)) then
                     it2=0.25*dble(jtet(j,itk))+0.9
                     if (itet(1,it2) .le. 0) goto 210
C
C                    ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C                    ADJUST MEMORY.
C
                     if (ictrecvr+8 .ge. len) then
                        len=len+200
                        len4=len*4
                        len5=len*5
                        call mmnewlen('iitmp',isubname,ipiitmp,len4,ics)
                        call mmnewlen('jjtmp',isubname,ipjjtmp,len4,ics)
                        call mmnewlen('kttmp',isubname,ipkttmp,len,ics)
                     endif
C
                     call recvr20(itk,it2,ictrecvr,ipiitmp,
     *                            ipjjtmp,ipkttmp)
                     itets(k)=0
                     n2to0=n2to0+1
                     goto 250
                  endif
               endif
 210        continue
 250     continue
C
C        ..............................................................
C        FIND THE NEXT INVERTED TET WITH WHICH TO WORK AND STORE
C        ADDITIONAL INVERTED TETS TO WORK WITH LATER.
C
         ninvrt=0
         do 300 k=1,3
            itk=itets(k)
            if (itk.eq.0 .or. itet(1,itk).le.0) goto 300
            volitk=volume(itet(1,itk),itet(2,itk),itet(3,itk),
     *                    itet(4,itk))
            if (volitk .le. 0) then
               ninvrt=ninvrt+1
               if (ninvrt .eq. 1) then
                  itnext=itk
               else
                  npairs2=npairs2+1
                  if (npairs2 .gt. maxpairs) goto 800
                  merglst2(1,npairs2)=itk
               endif
            endif
 300     continue
         ifpos=3
         if (ninvrt .eq. 0) then
 325        if (npairs2 .eq. 0) then
               iposbest=m
               goto 8888
            endif
            it=merglst2(1,npairs2)
            npairs2=npairs2-1
            if (itet(1,it) .le. 0) goto 325
            volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
            if (volit .gt. 0) goto 325
            goto 100
         else
            it=itnext
            goto 100
         endif
 800  continue
 8888 continue
C
C     *****************************************************************
C
C     RECOVER THE ORIGINAL STATE OF THE MESH.
C
      do 500 k=1,ictrecvr
         itk=kttmp(k)
         itet(1,itk)=iitmp(1,k)
         itet(2,itk)=iitmp(2,k)
         itet(3,itk)=iitmp(3,k)
         itet(4,itk)=iitmp(4,k)
         jtet(1,itk)=jjtmp(1,k)
         jtet(2,itk)=jjtmp(2,k)
         jtet(3,itk)=jjtmp(3,k)
         jtet(4,itk)=jjtmp(4,k)
 500  continue
      ntets=itetcnto
C
C     *****************************************************************
C
      if (idebug.ge.1) then
         iokflg=0
         do 666 i=1,4*ntets
            if (iisave(i).ne.itet1(i)) iokflg=1
            if (jjsave(i).ne.jtet1(i)) iokflg=1
 666     continue
         if (iokflg .eq. 1) call killcode('mesh recovery prob')
      endif
C
C     RELEASE TEMPORY MEMORY.
C
 9998 call mmrelprt(isubname,ics)
C
C     *****************************************************************
C
      goto 9999
 9999 continue
      end
