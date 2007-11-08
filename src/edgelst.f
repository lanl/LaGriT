*DK edgelst
      subroutine edgelst(itnum,iepos,iplst,ntet,ipieplst,npts,
     *                   iflag,imatint,iextbdy)
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine constructs a list of all tetrahedra and points
C        surrounding the edge "iepos" in tet "it" in a clockwise
C        fashion.
C
C     INPUT ARGUMENTS -
C
C        itnum     - the tet number
C        iepos     - the edge position
C        iplst    - pointer to an array to write the tet list
C        ipieplst  - pointer to an array to write the edge points
C
C     OUTPUT ARGUMENTS -
C
C        ntet     - the number of tetrahedrons in the list
C        npts      - number of edge points in the point array
C        iflag     - 1 => there is an error
C                    2 => there is a zero-volume or inverted tet
C                          in the list
C        imatint   - 1 => a material-interface has been found
C        iextbdy   - 1 => an external boundary has been found
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/edgelst.f_a  $
CPVCS    
CPVCS       Rev 1.5   05 Jan 2001 12:55:56   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:44:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Fri Feb 02 14:21:34 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.2   12/02/94 15:04:48   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.1   12/01/94 18:46:18   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.0   11/17/94 21:43:00   het
CPVCS    Orginal Version
CPVCS
C
C     ##################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"
      include "cmerge.h"
C
C ##################################################################
C
      parameter (lstmax=200)
      pointer( iplst   , lst(1)   )
      pointer( ipieplst, ieplst(1))
C
C ######################################################################
C
C     MACROS
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
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
      endif
C
C     ******************************************************************
C
      istep=1
      iflag=0
      ibndflg=0
      imatint=0
      iextbdy=0
C
      it=itnum
      volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
      if (volit.le.0) then
         iflag=2
         goto 9999
      endif
      ntet=1
      lst(ntet)=it
      ibegpt=itet(ielist(4*(iepos-1)+4),it)
      iendpt=itet(ielist(4*(iepos-1)+3),it)
      ieplst(1)=iendpt
      ieplst(2)=ibegpt
      npts=2
C
      ipt=ibegpt
      ipto=iendpt
      do 10 i=1,300
         ipos=iposfnd(ipto,itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if(ipos.lt.1.or.ipos.gt.4) then
            iflag=1
            goto 9999
         endif
         newptr=jtet(ipos,it)
         if (newptr.gt.mbndry) then
            imatint=1
            newptr=newptr-mbndry
C
C           ...........................................................
C           IF A MATERIAL INTERFACE IS FOUND, GET THE CORRECT CHILD
C           POINT ACROSS THE INTERFACE.
C
            imtx=imt1(itet1(newptr))
            newipt=0
            mpnt1=ipt
 25         continue
            isnx=isn1(mpnt1)
            mpnt1=isnx
            if (itp1(isnx).eq.ifitpcup) then
               goto 25
            elseif (isnx.ne.ipt) then
               if (imt1(isnx).eq.imtx) then
                  newipt=isnx
                  goto 27
               endif
               goto 25
            elseif (itp1(isnx).ge.ifitpmrg) then
               call termcode(1)
            endif
C**JM       if (newipt.eq.0) call termcode(1)
            if (newipt.eq.0) then
               iflag=1
               write (logdan,2222) itnum,iepos
 2222          format(1x,'newipt=0: itnum,iepos: ',2i10)
               call writloga('default',0,logdan,0,messerr)
               goto 9999
            endif
 27         ipt=newipt
         endif
         if(newptr.lt.mbndry) then
            it=0.25*dble(newptr)+0.9
            volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
            if (volit.le.0) then
               iflag=2
               goto 9999
            endif
            if (it.eq.itnum) goto 9999
            ntet=ntet+1
            if (ntet .gt. lstmax)
     *         call killcode(' connection has more than 200 tets')
            lst(ntet)=it
            ipto=ipt
            ipt=itet1(newptr)
C           if(ipt.eq.iendpt) goto 9999
            npts=npts+1
            if (npts .gt. lstmax)
     *         call killcode(' connection has more than 200 points')
            ieplst(npts)=ipt
         else
            iextbdy=1
            if(ibndflg.eq.1) then
               ibndflg=2
               if (imatint .eq. 1) then
                  goto 9999
               else
                  goto 8888
               endif
            endif
            ibndflg=1
            ipt=iendpt
            ipto=ibegpt
            it=itnum
            ilast=npts
         endif
 10   continue
C
      iflag=1
C
 8888 continue
C
C     ******************************************************************
C
C     IF AN EXTERNAL BOUNDARY WAS FOUND, RESTRUCTURE THE TET AND POINT
C     LISTS SO THEY WILL BE IN CLOCKWISE ORDER.  THE FIRST SET OF TETS
C     AND POINTS WERE THOSE PUT IN THE LIST IN CLOCKWISE ORDER BEFORE
C     THE EXTERNAL BOUNDARY WAS ENCOUNTERED.  THE SECOND SET OF TETS
C     AND POINTS WERE THOSE PUT IN THE LIST AFTER STARTING AGAIN AT THE
C     ORIGINAL TET, itnum, AND GOING IN COUNTERCLOCKWISE ORDER UNTIL
C     THE EXTERNAL BOUNDARY WAS AGAIN ENCOUNTERED.
C
C
C     ..................................................................
C     TEMPORARILY STORE THE FIRST SET OF TETS AND POINTS AT THE END OF
C     THEIR LISTS.  NOTICE THAT npts=ntet+1.
C
      if (2*npts .gt. lstmax)
     *   call killcode(' reordering operation needs more work space')
      do 100 i=1,ntet
         lst(ntet+i)=lst(i)
         ieplst(npts+i)=ieplst(i)
 100  continue
      ieplst(npts+npts)=ieplst(npts)
C
C     ..................................................................
C     COPY THE SECOND SET OF TETS AND POINTS BACKWARDS AT THE BEGINNING
C     OF THE LISTS.  (npts=ntet+1)
C
      ict=0
      do 200 i=npts,ilast+1,-1
         ict=ict+1
         lst(ict)=lst(ntet+i-1)
         ieplst(ict)=ieplst(npts+i)
200   continue
C
C     ..................................................................
C     RECOPY THE FIRST SET OF TETS AFTER THE REORDERED SECOND SET.
C
      iofs=npts-ilast
      do 300 i=1,ilast-1
         lst(iofs+i)=lst(ntet+i)
         ieplst(iofs+i)=ieplst(npts+i)
 300  continue
      ieplst(npts)=ieplst(npts+ilast)
C
C     ******************************************************************
C
      goto 9999
 9999 continue
      return
      end
