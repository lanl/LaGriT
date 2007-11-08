*dk,findface
      subroutine findface(itt,iposbest,
     *                    npoints,ntets)
C
       implicit none
C ######################################################################
C
C     PURPOSE -
C
C        This routine finds the best face through which to start a
C        marching sequence.  The best face is one which encounters no
C        boundaries during the flips.  If all faces encounter
C        boundaries, then the face which produces the shortest multi-
C        material connection is chosen.
C
C     INPUT ARGUMENTS -
C
C        itt      -  the tetrahedron
C
C     OUTPUT ARGUMENTS -
C
C        iposbest -  the position of the best face.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/findface.f_a  $
CPVCS    
CPVCS       Rev 1.14   30 Sep 2004 09:54:50   dcg
CPVCS    use iand in place of .and. with integers
CPVCS    
CPVCS       Rev 1.13   05 Jan 2001 12:56:00   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.12   Wed Nov 10 15:07:32 1999   dcg
CPVCS    remove reference to ihcycle
CPVCS
CPVCS       Rev 1.11   Mon Aug 30 15:14:38 1999   dcg
CPVCS    remove calls to ssort routines replace with hpsort
CPVCS
CPVCS       Rev 1.10   Fri Aug 28 14:24:48 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.9   Wed Jul 09 09:08:38 1997   dcg
CPVCS    add argument ierflg - this indicates if flip is okay
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:48:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   11/07/95 17:17:32   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   01/09/95 17:31:48   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.5   12/02/94 16:19:00   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.4   12/02/94 15:05:06   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.3   12/01/94 18:46:48   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.2   11/28/94 14:07:20   het
CPVCS    Changed the parameter list for ssort so the index array is real.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:04   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:18   pvcs
CPVCS    Original version.
C
C
C ######################################################################
C
      include "consts.h"
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "cmerge.h"
C
C ######################################################################
C
C
      character*8 isubname
      integer itets(3),id(12),jd(12),itetx(4),indx(6)
      real*8 distp(6)
      pointer (ipiitmp , iitmp(4,1))
      pointer (ipjjtmp , jjtmp(4,1))
      pointer (ipkttmp , kttmp(1)  )
C
      pointer (ipiisave, iisave(1) )
      pointer (ipjjsave, jjsave(1) )
      real*8 crosx1,crosy1,crosz1,volume,distsq
      integer i,j,k,i1,i2,i3,i4,ierror,ilen,icmotype,
     *  ictrecvr,maxiter,maxpairs,irecvrfl,iextbdy,len,len4,ics,m,
     *  kpt,kptn,jj,itk,it,ifpos,niter,n2to3,n2to0,itx,ifposx,it2,
     *  ifpos2,it3,ierflg,len5,jtemp,kpt2,ninvrt,itnext,ierr,iokflg
      integer itt,iposbest,npoints,ntets,
     *  iitmp,jjtmp,kttmp,iisave,jjsave,itetcnto
      real*8 xlarge
      real*8 dmin,volit,volitk
      real*8 alargenumber
      parameter ( alargenumber=1.d+30)
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
      call cmo_get_info('mbndry',cmo,mbndry,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
      endif
C
C     ******************************************************************
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
      isubname='findface'
      len=200
      len4=800
      call mmgetblk('iitmp',isubname,ipiitmp,len4,2,ics)
      call mmgetblk('jjtmp',isubname,ipjjtmp,len4,2,ics)
      call mmgetblk('kttmp',isubname,ipkttmp,len,2,ics)
C
      call mmgetblk('iisave',isubname,ipiisave,4*ntets,2,ics)
      call mmgetblk('jjsave',isubname,ipjjsave,4*ntets,2,ics)
      do 10 i=1,4*ntets
         iisave(i)=itet1(i)
         jjsave(i)=jtet1(i)
 10   continue
C
C     ******************************************************************
C
      do 15 k=1,ntets
         kfix(2,k)=0
 15   continue
C
C     ******************************************************************
C
C     DETERMINE THE ORDER OF THE FACES TO SEARCH BY SORTING THE LENGTHS
C     OF THE FIRST POTENTIAL CONNECTIONS.
C
      do 25 m=1,4
         distp(m)=xlarge
         indx(m)=m
         if (jtet(m,itt) .lt. mbndry) then
            kpt=itet(m,itt)
            kptn=itet1(jtet(m,itt))
            distp(m)=distsq(kpt,kptn)
         endif
 25   continue
      call hpsort1(4,distp,one,indx)
C
C     ******************************************************************
C
C     LOOP OVER THE 4 FACES, PERFORMING TEMPORARY FLIPS.  RECOVER THE
C     ORIGINAL STATE OF THE MESH BEFORE STARTING WITH A NEW FACE.
C
      do 800 jj=1,4
         m=indx(jj)
         distp(m)=xlarge
         itetx(m)=0
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
         ictrecvr=0
         ntets=itetcnto
 75      continue
         it=itt
         ifpos=m
         kpt=itet(m,it)
         irecvrfl=1
         ictrecvr=0
         niter=0
         npairs2=0
         n2to3=0
         n2to0=0
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
            call find2to3(itx,ifposx,it2,ifpos2,it3,id(1),jd(1),
     *                    npoints,ntets,ierflg)
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
            if (jtet(ifpos,it) .gt. mbndry) then
C
C              .........................................................
C              SAVE THE SQUARED LENGTH OF A POTENTIAL MULTI-MATERIAL
C              CONNECTION.  ALSO SAVE THE TETRAHEDRON ACROSS THE
C              BOUNDARY FACE.
C
               jtemp=jtet(ifpos,it)-mbndry
               kpt2=itet1(jtemp)
               distp(m)=distsq(kpt,kpt2)
               itetx(m)=0.25*dble(jtet(ifpos,it)-mbndry)+0.9
            else
               iextbdy=1
            endif
C
C              .........................................................
C              TRY ANOTHER FACE.
C
            goto 800
C
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
            if (volitk .lt. 0) then
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
 9888 continue
      if (iposbest .gt. 0) goto 9998
C
C     ******************************************************************
C
C     IF ONE OF THE VERTICES LIES IN ANOTHER TETRAHEDRON, CHOOSE
C     THAT FACE.
C
C     do 600 m=1,4
C        call pttet(itet(m,itt),itin)
C        if (itin .gt. 0) then
C           iposbest=m
C           goto 9998
C        endif
C600   continue
C
C     *****************************************************************
C
C     IF NO OTHER FACE HAS BEEN CHOSEN AND AN EXTERNAL BOUNDARY HAS
C     BEEN ENCOUNTERED, DO NOT CHOOSE A FACE (LET THE TET BE MERGED
C     OUT IN THE NORMAL MANNER).
C
      if (iextbdy.eq.1.or.jtet(1,itt).eq.mbndry.or.jtet(2,itt).eq.
     *    mbndry.or.jtet(3,itt).eq.mbndry.or.jtet(4,itt).eq.mbndry)
     *    then
         write (logdan,1010) itt
 1010    format('  inverted tet at external bdy : it=',i10)
         call writloga('default', 0, logdan, 0, ierr)
C        iposbest=0
C        goto 9998
      endif
C
C     *****************************************************************
C
C     IF NO OTHER FACE HAS BEEN CHOSEN, CHOOSE THE FACE WITH
C     THE SHORTEST MULTI-MATERIAL CONNECTION IF THAT CONNECTION IS
C     NOT TOO LONG.
C
      dmin=min(distp(1),distp(2),distp(3),distp(4))
      if (dmin .eq. xlarge) goto 9998
      if (iposbest .eq. 0) then
         do 400 m=1,4
            if (distp(m) .eq. dmin) then
               iposbest=m
               goto 9997
            endif
 400     continue
      endif
 9997 continue
      itx=itetx(iposbest)
      if (itx .eq. 0) call termcode(1)
      do 450 i=1,6
         indx(i)=i
         distp(i)=distsq(itet(ielist(4*i-3),itx),
     *                   itet(ielist(4*i-2),itx))
 450  continue
      call hpsort1(6,distp,one,indx)
      if (dmin .gt. 2.25*distp(6)) iposbest=0
C
C     *****************************************************************
C
C     RELEASE TEMPORY MEMORY.
C
      iokflg=0
      do 666 i=1,4*ntets
         if (iisave(i).ne.itet1(i)) iokflg=1
         if (jjsave(i).ne.jtet1(i)) iokflg=1
 666  continue
      if (iokflg .eq. 1) call killcode('mesh recovery prob')
C
 9998 call mmrelprt(isubname,ics)
C
      if(icmoset.eq.1) then
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      endif
C
C     *****************************************************************
C
      goto 9999
 9999 continue
      end
