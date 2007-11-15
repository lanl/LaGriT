*dk,flipbdyc
      subroutine flipbdyc(nbdyfc,n3to2i,n4to4i,n2to3i,n2to3,n2to0,
     *                    npoints,ntets,toldamage)
       implicit none
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine flips connection on interfaces.
C
C     INPUT ARGUMENTS -
C
C        nbdyfc   - the number of boundary faces collected in the
C                   itmp2 array.
C        n3to2i   - current number of 3-to-2i flips performed
C        n4to4i   - current number of 4-to-4i flips performed
C        n2to3i   - current number of 2-to-3i flips performed
C        n2to3    - current number of 2-to-3  flips performed
C        n2to0    - current number of 2-to-0  flips performed
C
C     OUTPUT ARGUMENTS -
C
C        n3to2i   - current number of 3-to-2i flips performed
C        n4to4i   - current number of 4-to-4i flips performed
C        n2to3i   - current number of 2-to-3i flips performed
C        n2to3    - current number of 2-to-3  flips performed
C        n2to0    - current number of 2-to-0  flips performed
C
C     CHANGE HISTORY -
C
C        $Log: flipbdyc.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.15   30 Sep 2004 09:57:44   dcg
CPVCS    use ior in place of .or. with integer variables
CPVCS
CPVCS       Rev 1.14   05 Jan 2001 12:56:28   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.13   Thu Nov 04 15:14:08 1999   dcg
CPVCS    remove unused code
CPVCS
CPVCS       Rev 1.12   Fri Jan 22 11:58:36 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.11   Fri Oct 16 18:47:12 1998   dcg
CPVCS    check nodes on interface flips to verify their 'material'
CPVCS    identities
CPVCS
CPVCS       Rev 1.10   Mon Jun 08 09:58:04 1998   dcg
CPVCS    restore tet numbers if damage control rejects a flip
CPVCS
CPVCS       Rev 1.9   Mon May 25 00:45:44 1998   kuprat
CPVCS    Put in calls to TOLDAMAGE.
CPVCS
CPVCS       Rev 1.8   Mon Mar 30 09:59:28 1998   dcg
CPVCS    check for negative node numbers (elements marked
CPVCS    as deleted due to previous flips ) and ignore them.
CPVCS
CPVCS       Rev 1.7   Mon Apr 21 14:41:30 1997   dcg
CPVCS    fix declarations
CPVCS
CPVCS       Rev 1.6   Thu Apr 17 16:16:44 1997   dcg
CPVCS    check for proper node types before attemptin
CPVCS    interface flips
CPVCS
CPVCS       Rev 1.5   Fri Jan 24 13:43:04 1997   het
CPVCS    Fix an error (also wrong in the CRAY version) with imt1() for
CPVCS    interfaces.
CPVCS
CPVCS       Rev 1.4   Tue Apr 16 14:35:36 1996   dcg
CPVCS    try2to2b.f try2to4r.f try4to4x.f trymtonr.f
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:26   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:47:18   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:46   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:13:04   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include 'local_element.h'
C
C ######################################################################
C
      integer nmulti
      parameter (nmulti = 200)
      integer id(16),jd(16),itets(4),ichain1(nmulti),imt1a(nmulti),
     *          ichain2(nmulti),imt2(nmulti),imt1b(nmulti),imt1c(nmulti)
      integer nbdyfc,n3to2i,n4to4i,n2to3i,n2to3,n2to0,
     *    ierror,npoints,ntets ,i,j,k ,iposfnd,length,icmotype,
     *    j0,ione,itwo,iface,jtemp,i1,i2,ntest,nflips,jj,kk,iflag2,
     *    iflag1,ibdytet,nfpos,ll,ict4,ict2,icount,
     *    ntv,i6,it5,i3b,ntets_local,ii,nelts,nef,iflag,iflip,
     *    it2save,itx,nepos,icscode,ict3,ipar,ict1,itv,imtx,it4,
     *    it3,jtemp2,nflipsb,i5,it2,kptr,k2,k1,n2,n1,n3,it,ifaceo,
     *    niter,n2to0o,n2to3o,i3,i4,i0,ipos3,ict,np2,np1,ichoice2,
     *    ichoice1,ifpos,ivt,ipos2
      real*8 toldamage,crosx1,crosy1,crosz1,volume,dot,dlen2,
     *  zn2,yn2,xn2,dlen1,xn1,yn1,zn1,volit2,xst,volit4,volit3,
     *  volit
      logical itsttp
      pointer (ipielts,ielts)
      integer ielts(*)
      pointer (ipiparent,iparent)
      integer iparent(*)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      integer itetoff(*),jtetoff(*)
      character*32 isubname
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
      iposfnd(i0,i1,i2,i3,i4)=min0(iabs(i1-i0)*4+1,iabs(i2-i0)*4+2,
     *                             iabs(i3-i0)*4+3,iabs(i4-i0)*4+4)
C
C ######################################################################
      isubname='flipbdyc'
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,nelements,length,icmotype,
     *    ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
C
C  fill itetoff and jtetoff
c
      do ii=1,ntets
         itetoff(ii)=4*(ii-1)
         jtetoff(ii)=4*(ii-1)
      enddo
c
c  get temp working space
      length= 200
      call mmgetblk('ielts',isubname,ipielts,length,1,
     *         icscode)
      length=npoints
      call mmgetblk('iparent',isubname,ipiparent,length,1,
     *         icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
c
C     ******************************************************************
C
C
      n2to3o=n2to3
      n2to0o=n2to0
      ione=1
      itwo=2
      niter=0
      xst=1.0e-09
C
C     ******************************************************************
C
C     LOOP OVER FACES LOOKING FOR OPPORTUNITIES TO FLIP
C     BOUNDARY CONNECTIONS.
C
      do 800 i=1,nbdyfc
 
       iface=itmp2(i)
       if (jtet1(iface).le.mbndry) goto 800
       jtemp=jtet1(iface)-mbndry
       do 799 k=1,2
         if (k .eq. 2) then
            if (niter .ge. 1) goto 800
            ifaceo=iface
            iface=jtemp
            jtemp=ifaceo
 
         endif
         it=0.25*dble(iface)+0.9
C
C        ...............................................................
C        DO NOT PERFORM AN INTERFACE FLIP UNLESS THE MATERIALS ON EACH
C        SIDE OF THE INTERFACE ARE ALREADY FLAGGED FOR RECONNECTION
C
         if(itet(1,it).le.0) go to 800
         if (matrecon(imt1(itet(1,it))) .eq. 0) goto 800
         ifpos=iand((iface-1),maskface)+1
         if (itet(1,it).le.0 .or. kfix(1,it) .gt. 0) goto 800
         volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if (volit .le. 0) goto 800
         n1=iflist(3*ifpos-2)
         n3=iflist(3*ifpos-1)
         n2=iflist(3*ifpos  )
         k1=n1
         k2=n2
         kptr=n3
C
C        ...............................................................
C        CHECK EACH EDGE OF THE FACE TO SEE IF THERE ARE EXACTLY
C        TWO TETRAHEDRA ON ONE SIDE OF THE INTERFACE.  IF SO,
C        CONSIDER THE VORON2D TEST TO SEE IF THE BOUNDARY CONNECTION
C        SHOULD BE FLIPPED.
C
         do 700 j=1,3
            if (j .eq. 2) then
               k1=n2
               k2=n3
               kptr=n1
            elseif (j .eq. 3) then
               k1=n3
               k2=n1
               kptr=n2
            endif
            if (jtet(kptr,it) .ge. mbndry) goto 700
            it2=0.25*dble(jtet(kptr,it))+0.9
            if (kfix(1,it2) .gt. 0) goto 700
            ipos2=iposfnd(itet(ifpos,it),itet(1,it2),itet(2,it2),
     *                    itet(3,it2),itet(4,it2))
            if (jtet(ipos2,it2) .le. mbndry) goto 700
            volit2=volume(itet(1,it2),itet(2,it2),itet(3,it2),
     *                    itet(4,it2))
            if (volit2 .le. 0) goto 700
            i1=itet(k1,it)
            i2=itet(k2,it)
            i3=itet(kptr,it)
            i4=itet(ifpos,it)
            i5=itet1(jtet(kptr,it))
            call voron2d(i1,i3,i2,i5,ichoice1)
            if (ichoice1 .eq. 1) goto 700
            call voron2d(i2,i5,i1,i3,ichoice2)
            if (ichoice2 .eq. 1) goto 700
C
C           ............................................................
C           DO NOT FLIP IF THE ANGLE BETWEEN THE NORMALS TO THE PLANES
C           IS TOO LARGE.
C
            xn1=crosx1(i1,i3,i2)
            yn1=crosy1(i1,i3,i2)
            zn1=crosz1(i1,i3,i2)
            dlen1=xn1*xn1+yn1*yn1+zn1*zn1
            xn2=crosx1(i1,i2,i5)
            yn2=crosy1(i1,i2,i5)
            zn2=crosz1(i1,i2,i5)
            dlen2=xn2*xn2+yn2*yn2+zn2*zn2
            dot=xn1*xn2+yn1*yn2+zn1*zn2
            if (dot*abs(dot) .lt. 0.75*dlen1*dlen2) goto 700
C
C           call vorpoint(i1,i5,i3,i4,xv,yv,zv,distsq1)
C           test1=distsq1-(xic(i2)-xv)**2-(yic(i2)-yv)**2-
C    *            (zic(i2)-zv)**2-distsq1*xst
C           if (test1 .gt. 0) goto 700
C           call vorpoint(i2,i3,i5,i4,xv,yv,zv,distsq2)
C           test2=distsq2-(xic(i1)-xv)**2-(yic(i1)-yv)**2-
C    *            (zic(i1)-zv)**2-distsq2*xst
C           if (test2 .gt. 0) goto 700
C
C           ............................................................
C           TRY THE 2-TO-0B FLIP.
C
            itets(1)=it
            itets(2)=it2
            call try2to0b(itets,itwo,nflips,nflipsb,itv,
     *                    npoints,ntets)
            ntest=nflips+nflipsb
            if (ntest .gt. 0) then
               n2to0=n2to0+ntest
               goto 800
            endif
C
C           ............................................................
C           DETERMINE THE TETRAHEDRA ON THE OTHER SIDE OF THE INTERFACE.
C
            jtemp2=jtet(ipos2,it2)-mbndry
            it3=0.25*dble(jtemp)+0.9
            it4=0.25*dble(jtemp2)+0.9
C
C           ............................................................
C           DO NOT PERFORM AN INTERFACE FLIP UNLESS THE MATERIALS ON
C           EACH SIDE OF THE INTERFACE ARE ALREADY FLAGGED FOR
C           RECONNECTION.
C
            imtx=imt1(itet(1,it3))
            if (matrecon(imtx).eq.0) go to 800
C
C           ............................................................
C           TRY THE 3-TO-2I FLIP.
C
 
c$$$c.... See what happens when we omit this possibility.
c$$$            goto 300
 
            if (it3 .ne. it4) goto 300
            volit3=volume(itet(1,it3),itet(2,it3),itet(3,it3),
     *                    itet(4,it3))
            if (volit3 .le. 0) goto 400
            itets(1)=it3
            call try2to0(itets,ione,nflips,ivt,
     *                   npoints,ntets)
            if (nflips .eq. 1) then
               n2to0=n2to0+1
               goto 800
 
            endif
            itets(1)=i1
            itets(2)=i2
            call getchain(i1,ichain1,imt1a,nmulti,ict1,ipar)
            call getchain(i2,ichain2,imt1b,nmulti,ict3,ipar)
            do jj=1,ict1
              if (imt1a(jj).eq.imtx) then
                 do kk =1,ict3
                    if(imt1b(kk).eq.imtx) then
                       itets(1)=ichain1(jj)
                       itets(2)=ichain2(kk)
                       go to 250
                     endif
                  enddo
               endif
            enddo
            write(logdan,249) it,it2,it3
 249        format (' material mismatch with 3to2i flip, tets '
     *               ,3i8)
            call writloga('bat',0,logdan,0,icscode)
            go to 800
 250        imtx=imt1a(jj)
            if (matrecon(imtx) .eq. 0) goto 800
            do 275 j0=1,6
               np1=itet(ielist(4*(j0-1)+1),it3)
               np2=itet(ielist(4*(j0-1)+2),it3)
               if (np1.eq.itets(1).and.np2.eq.itets(2) .or.
     *            np2.eq.itets(1).and.np1.eq.itets(2)) then
                  nepos=j0
                  goto 280
               endif
 275        continue
            call termcode(1)
 280        continue
            itx=it3
            it2save=it2
 
            call fnd3to2i(itx,nepos,it2,it3,id(1),jd(1),iflip,
     *                    npoints,ntets)
 
            if (iflag.eq.0) then
               it2=it2save
               it3=itx
               go to 400
            endif
            call testdamage(id(3),id(1),id(7),id(2),iflag,
     &         toldamage)
            if (iflag.eq.0) then
               it2=it2save
               it3=itx
               go to 400
            endif
            call flp3to2i(itx,it2,it3,id(1),jd(1),
     *                    npoints,ntets)
C  check all vertices of tet whose itetclr has been changed -
c  fix isn chain if necessary
            nef=4
            do j0=1,4
               i1=itet(j0,itx)
               if(isn1(i1).eq.0) go to 297
               call getchain(i1,ichain1,imt1a,nmulti,ict1,ipar)
               if(ict1.ge.2) then
                  call get_elements_around_node(itx,j0,
     *             nelts,ipielts,itetoff,jtetoff,
     *             itet,jtet,itettyp,iparent,
     *             nef,mbndry)
                  do ii=1,ict1
                     do jj=1,nelts
                       if(itetclr(ielts(jj)).eq.imt1a(ii)) go to 296
                     enddo
c
c  this imt no longer in chain so fix it - so dud node and fix
c  chain
c
                     i2=ipar
                     if (imt1(isn1(i2)).eq.imt1a(ii)) then
                        i1=isn1(i2)
                        isn1(i2)=isn1(i1)
                        isn1(i1)=0
                        itp1(i1)=ifitpdud
                     endif
 296                 continue
                  enddo
               endif
 297           continue
            enddo
 
            kfix(1,itx)=itx
            kfix(1,it2)=it2
            kfix(1,it3)=it3
            n3to2i=n3to2i+1
            ntets_local=2
            itets(1)=itx
            itets(2)=it2
            goto 798
C
 300        continue
C
C           ............................................................
C           TRY THE 4-TO-4I FLIP.
C
            if (kfix(1,it3) .gt. 0 .or. kfix(1,it4) .gt. 0) goto 800
            call getchain(itet(kptr,it),ichain1,imt1a,nmulti,ict,ipar)
            if (ict.eq.0) go to 400
            do jj=1,ict
               if (imt1a(jj) .eq. imtx) then
                  i3b=ichain1(jj)
                  goto 120
               endif
            enddo
            write(logdan,251) it,it2,it3
 251        format (' material mismatch with 4to4i flip, tets '
     *              ,3i8)
            call writloga('bat',0,logdan,0,icscode)
            go to 800
 120        continue
            ipos3=iposfnd(i3b,itet(1,it3),itet(2,it3),itet(3,it3),
     *                    itet(4,it3))
            if (jtet(ipos3,it3) .ge. mbndry) goto 400
            it5=0.25*dble(jtet(ipos3,it3))+0.9
            if (it4 .ne. it5) goto 400
            volit3=volume(itet(1,it3),itet(2,it3),itet(3,it3),
     *                    itet(4,it3))
            if (volit3 .le. 0) goto 400
            volit4=volume(itet(1,it4),itet(2,it4),itet(3,it4),
     *                    itet(4,it4))
            if (volit4 .le. 0) goto 400
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           TEST FOR GEOMETRIC RESTRICTIONS.
C
            i6=itet1(jtemp)
            call testdamage(i1,i3,i2,i5,iflag,toldamage)
            if (iflag .eq. 0) goto 400
            call test4to4(i3,i1,i6,i2,i4,i5,iflag)
            if (iflag .eq. 0) goto 400
C           call vorpoint(i1,i3,i5,i6,xv,yv,zv,distsq)
C           test3=distsq-(xic(i2)-xv)**2-(yic(i2)-yv)**2-
C    *            (zic(i2)-zv)**2-distsq*xst
C           if (test3 .gt. 0) goto 400
C           call vorpoint(i2,i5,i3,i6,xv,yv,zv,distsq)
C           test4=distsq-(xic(i1)-xv)**2-(yic(i1)-yv)**2-
C    *            (zic(i1)-zv)**2-distsq*xst
C           if (test4 .gt. 0) goto 400
C
C           ............................................................
C           TRY THE 2-TO-0 FLIP.
C
            itets(1)=it3
            itets(2)=it4
            call try2to0(itets,itwo,nflips,ntv,
     *                   npoints,ntets)
            if (nflips .ge. 1) then
               n2to0=n2to0+nflips
               goto 800
            endif
            call fnd4to4i(it,it2,it3,it4,i1,i2,i3,i3b,i4,i5,i6,id,jd,
     *                    npoints,ntets)
            call flp4to4i(it,it2,it3,it4,id,jd,
     *                    npoints,ntets)
            n4to4i=n4to4i+1
            kfix(1,it)=it
            kfix(1,it2)=it2
            kfix(1,it3)=it3
            kfix(1,it4)=it4
            ntets_local=4
            itets(1)=it
            itets(2)=it2
            itets(3)=it3
            itets(4)=it4
            goto 798
C
 400        continue
C
C           ............................................................
C           TRY THE 2-TO-3I FLIP.
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           VERIFY THAT THE CONNECTION IS A TWO-MATERIAL CONNECTION.
C           AND CHECK THAT ALL FOUR POINTS ON THE BOUNDARY TETS
C           HAVE THE CORRECT MATERIAL TYPE -- THAT BOTH TETS TO BE
C           FLIPPED HAVE ITERFACE FACES WITH THE CORRECT MATERIAL ON
C           THE OTHER SIDE
C
 
c$$$c.... See what happens when we omit this possibility.
c$$$            goto 700
 
            icount=0
            if(.not.itsttp('intrface',itp1(i1)).or.
     *         .not.itsttp('intrface',itp1(i2)).or.
     *         .not.itsttp('intrface',itp1(i3)).or.
     *         .not.itsttp('intrface',itp1(i5))) go to 700
            call getchain(i1,ichain1,imt1a,nmulti,ict1,ipar)
            call getchain(i2,ichain2,imt2 ,nmulti,ict2,ipar)
            call getchain(i3,ichain2,imt1b,nmulti,ict3,ipar)
            call getchain(i5,ichain2,imt1c,nmulti,ict4,ipar)
            if(ict1.eq.0.or.ict2.eq.0.or.ict3.eq.0.or.ict4.eq.0)
     *         go to 700
            do ii = 1,ict1
               do jj = 1,ict2
                  do kk = 1,ict3
                     do ll = 1,ict4
                         if(imt1a(ii).eq.imtx.and.
     *                      imt2(jj) .eq.imtx.and.
     *                      imt1b(kk).eq.imtx.and.
     *                      imt1c(ll).eq.imtx) go to 480
                      enddo
                   enddo
               enddo
            enddo
            go to 700
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           DETERMINE WHICH OF THE THREE NEW TETS WILL BE THE
C           BOUNDARY TET.
C
 480        nfpos=kptr
            itets(1)=itet(iflist(3*nfpos-2),it)
            itets(2)=itet(iflist(3*nfpos  ),it)
            ibdytet=1
            if (i4 .eq. itets(1)) ibdytet=2
            if (i4 .eq. itets(2)) ibdytet=3
            call fnd2to3i(it,nfpos,it2,it3,ibdytet,id,jd,
     *                    npoints,ntets)
C
            call testdamage(i1,i3,i2,i5,iflag,toldamage)
            if (iflag .eq. 0) goto 700
            call tst2to3i(i1,i2,i4,i3,i5,iflag1,iflag2,
     *                    npoints,ntets)
            if (iflag1 .eq. 0) goto 500
            call flp2to3i(it,it2,it3,ibdytet,id,jd,
     *                    npoints,ntets)
c
C  refresh pointers - flp2to3i can reallocate arrays
c
            call cmo_get_info('nelements',cmo,nelements,length,
     *         icmotype,ierror)
            call cmo_get_info('itettyp',cmo,ipitettyp,length,
     *         icmotype,ierror)
            call cmo_get_info('itetoff',cmo,ipitetoff,length,
     *         icmotype,ierror)
            call cmo_get_info('jtetoff',cmo,ipjtetoff,length,
     *         icmotype,ierror)
            call cmo_get_info('isn1',cmo,ipisn1,length,
     *         icmotype,ierror)
            do j0=1,nelements
               itetoff(j0)=(j0-1)*4
               jtetoff(j0)=(j0-1)*4
               itettyp(j0)=ifelmtet
            enddo
C
            n2to3i=n2to3i+1
            kfix(1,it)=it
            kfix(1,it2)=it2
            kfix(1,it3)=it3
            ntets_local=3
            itets(1)=it
            itets(2)=it2
            itets(3)=it3
            goto 798
C
 500        continue
 
c.... We skip over 'march out' code.
C
 700     continue
         goto 799
C
 798     continue
         goto 800
C
 799   continue
 800  continue
C
C     ******************************************************************
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      end
