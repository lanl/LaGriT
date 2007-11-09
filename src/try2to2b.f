*dk,try2to2b
      subroutine try2to2b(nface,n2to2,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine attempts to flip connections on boundaries.
C
C     INPUT ARGUMENTS -
C
C        nface   - the number of faces in the "irclst" array
C
C     OUTPUT ARGUMENTS -
C
C        n2to2   - the number of boundary flips performed
C
C     CHANGE HISTORY -
C
C        $Log: try2to2b.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.15   05 Jan 2001 12:58:14   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.14   Thu Jan 21 20:24:18 1999   nnc
CPVCS    Correct typo in pointer statement for ICONTAB.
CPVCS
CPVCS       Rev 1.13   Mon Dec 01 16:34:10 1997   dcg
CPVCS    comment out loc call
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 17:05:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   Mon Jun 03 15:11:54 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.10   Wed May 22 10:29:02 1996   dcg
CPVCS     get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.9   Thu May 16 10:32:06 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.8   Mon May 06 12:26:58 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.7   Thu May 02 11:22:10 1996   dcg
CPVCS    fix call to vorfclst
CPVCS
CPVCS       Rev 1.6   Tue Apr 16 14:38:22 1996   dcg
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.5   03/23/95 22:59:36   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.4   02/10/95 08:27:46   het
CPVCS    Change the nfaces variable to nface because of cmo.h
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:04   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:34   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:22   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:36   pvcs
CPVCS    Original version.
C
C ######################################################################
C
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
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
      dimension id(8),jd(8),ktmp(lenblk),itets(301)
      logical itsttp
C
      dimension ifindx(4,4)
C
C ######################################################################
C
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
C
C ######################################################################
C
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
      call cmo_get_info('nconbnd',cmo,nconbnd,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
      call cmo_get_info('icontab',cmo,ipicontab,ilen,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
C     ipitets=loc(itets)
C
      itwo=2
      xst=1.0e-11
      n2to2=0
C
C     ******************************************************************
C
C     ATTEMPT RECONNECTIONS ACROSS THE FACES
C
      do 100 i=1,nface
         iface=irclst(i)
C
C        _______________________________________________________________
C
C        CONSIDER BOUNDARY CONNECTIONS THAT ARE COMMON TO EXACTLY TWO
C        TETS. CONSIDER POTENTIAL FLIPS BETWEEN BOUNDARY POINTS ONLY.
C
         ip2=jtet1(iface)
         if(ip2.ge.mbndry) goto 100
         i5=itet1(ip2)
         if(jtet1(ip2).ge.mbndry) goto 100
         i4=itet1(jtet1(ip2))
Cdcg     if(itp1(i4).ne.ifitpini.and.itp1(i4).ne.ifitprfl.and.
Cdcg *      itp1(i4).ne.ifitpfre.and.itp1(i4).ne.ifitpinb.and.
Cdcg *      itp1(i5).ne.ifitpini.and.itp1(i5).ne.ifitprfl.and.
Cdcg *      itp1(i5).ne.ifitpfre.and.itp1(i5).ne.ifitpinb) goto 100
         if (.not.itsttp('boundary',itp1(i4)).and.
     *       .not.itsttp('intrface',itp1(i4)))    go to 100
         if (.not.itsttp('boundary',itp1(i5)).and.
     *       .not.itsttp('intrface',itp1(i5)))    go to 100
C           **SKIP FACE UNLESS BOTH POINTS ARE BOUNDARY POINTS
         call face(iface,ip1,it,n2,n1,n3)
         if(kfix(1,it).ge.1.or.itet(1,it).le.0) goto 100
         volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if(volit.le.0) goto 100
C           **SKIP FACE IF TETRAHEDRON IS INVERTED
         it2=0.25*dble(jtet1(iface))+0.9
         volit2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
         if(volit2.le.0) goto 100
C           **SKIP FACE IF TETRAHEDRON IS INVERTED
C
C        _______________________________________________________________
C
C        TEST FOR GEOMETRY RESTRICTIONS
C
         do 50 m=1,3
            idup=0
            if(m.eq.1) then
               i1=n1
               i2=n2
               i3=n3
            elseif(m.eq.2) then
               i1=n2
               i2=n3
               i3=n1
            elseif(m.eq.3) then
               i1=n3
               i2=n1
               i3=n2
            endif
C
Cdcg        if(itp1(i1).eq.ifitpint.or.itp1(i2).eq.ifitpint) goto 50
            if (itsttp('interior',itp1(i1)).or.
     *            itsttp('interior',itp1(i2))) go to 50
C              **SKIP IF EITHER POINT IS NOT BOUNDARY POINT
Cdcg        if(itp1(i1).eq.ifitpinb.and.itp1(i1).eq.ifitpinb) goto 50
            if (itsttp('intrface',itp1(i1)).and.
     *            itsttp('intrface',itp1(i2))) go to 50
C              **DO NOT BREAK AN EXTERNAL INTERFACE CONNECTION
Cdcg        if(itp1(i1).eq.ifitpfre.and.itp1(i2).eq.ifitpfre) goto 50
            if (itsttp('free',itp1(i1)).and.
     *            itsttp('free',itp1(i2))) go to 50
C              **DO NOT BREAK A FREE SURFACE BOUNDARY CONNECTION
C
C           ............................................................
C
C           FLIP ON REFLECTING SURFACE ONLY IF ALL FOUR POINTS LIE IN
C           SAME PLANE.
C
            icrnbr1=icr1(i1)
            icrnbr2=icr1(i2)
            if(icrnbr1.ne.0.and.icrnbr2.ne.0) then
               icrnbr4=icr1(i4)
               icrnbr5=icr1(i5)
               if(icrnbr4.ne.0.and.icrnbr5.ne.0) then
                  if(icrnbr2.ne.icrnbr1.or.icrnbr4.ne.icrnbr1.or.
     *               icrnbr5.ne.icrnbr1) then
                     if(nconbnd.le.0) then
                        do 300 j1=3,5
                           irb1=icrnbr1
                           do 310 j2=3,5
                              irb2=icrnbr2
                              if(irb2.eq.irb1) then
                                 do 320 j4=3,5
                                    irb4=icrnbr4
                                    if(irb4.eq.irb1) then
                                       do 330 j5=3,5
                                          irb5=icrnbr5
                                          if(irb5.eq.irb1) goto 340
  330                                  continue
                                    endif
  320                            continue
                              endif
  310                      continue
  300                   continue
                        goto 50
  340                   continue
                        if(irb1.eq.0) goto 50
                     else
                        do 200 j1=3,icontab(1,icrnbr1)
                           irb1=icontab(j1,icrnbr1)
                           do 210 j2=3,icontab(1,icrnbr2)
                              irb2=icontab(j2,icrnbr2)
                              if(irb2.eq.irb1) then
                                 do 220 j4=3,icontab(1,icrnbr4)
                                    irb4=icontab(j4,icrnbr4)
                                    if(irb4.eq.irb1) then
                                       do 230 j5=3,icontab(1,icrnbr5)
                                          irb5=icontab(j5,icrnbr5)
                                          if(irb5.eq.irb1) goto 240
  230                                  continue
                                    endif
  220                            continue
                              endif
  210                      continue
  200                   continue
                        goto 50
  240                   continue
                        if(irb1.eq.0) goto 50
                     endif
                  endif
               else
                  goto 50
               endif
            endif
C
            do 10 j=1,6
               ip1=ielist(4*(j-1)+1)
               ip2=ielist(4*(j-1)+2)
               if( (itet(ip1,it).eq.i1.and.itet(ip2,it).eq.i2).or.
     *             (itet(ip1,it).eq.i2.and.itet(ip2,it).eq.i1))then
                      iepos=j
                      goto 11
               endif
 10         continue
 11         continue
            iofs=4*(iepos-1)
            if(i3.eq.itet(ielist(iofs+3),it)) then
               if(jtet(ielist(iofs+3),it).lt.mbndry) goto 50
               if(jtet(ielist(iofs+4),it).ge.mbndry) goto 50
               it2=0.25*dble(jtet(ielist(iofs+4),it))+0.9
               if(kfix(1,it2).ge.1) goto 50
C
               ipos2=4
               if(itet(1,it2).eq.i3) ipos2=1
               if(itet(2,it2).eq.i3) ipos2=2
               if(itet(3,it2).eq.i3) ipos2=3
               if(jtet(ipos2,it2).lt.mbndry) goto 50
            elseif(i3.eq.itet(ielist(iofs+4),it)) then
               if(jtet(ielist(iofs+4),it).lt.mbndry) goto 50
               if(jtet(ielist(iofs+3),it).ge.mbndry) goto 50
               it2=0.25*dble(jtet(ielist(iofs+3),it))+0.9
               if(kfix(1,it2).ge.1) goto 50
C
               ipos2=4
               if(itet(1,it2).eq.i3) ipos2=1
               if(itet(2,it2).eq.i3) ipos2=2
               if(itet(3,it2).eq.i3) ipos2=3
               if(jtet(ipos2,it2).lt.mbndry) goto 50
            endif
C
            xn=xic(i5)-xic(i4)
            yn=yic(i5)-yic(i4)
            zn=zic(i5)-zic(i4)
            sn=sqrt(xn*xn+yn*yn+zn*zn)
            xnorm1=crosx1(i4,i1,i3)
            ynorm1=crosy1(i4,i1,i3)
            znorm1=crosz1(i4,i1,i3)
            snorm1=sqrt(xnorm1**2+ynorm1**2+znorm1**2)
            xnorm2=crosx1(i4,i3,i2)
            ynorm2=crosy1(i4,i3,i2)
            znorm2=crosz1(i4,i3,i2)
            snorm2=sqrt(xnorm2**2+ynorm2**2+znorm2**2)
            dot1=xn*xnorm1+yn*ynorm1+zn*znorm1
            dot2=xn*xnorm2+yn*ynorm2+zn*znorm2
            iflag=0
            if(dot1.ge.xst*sn*snorm1.and.
     *         dot2.ge.xst*sn*snorm2) iflag=1
            if(iflag.eq.0) goto 50
C
C           ............................................................
C           Make sure that the potentially new boundary faces do
C           not lie on top of already existing faces.
C
            do 30 j=1,6
               if(iepos.eq.j) goto 30
               ip1=itet(ielist(4*(j-1)+1),it)
               ip2=itet(ielist(4*(j-1)+2),it)
               if(ip1.eq.i3.or.ip2.eq.i3) goto 30
               call vorfclst(it,j,itets,0,ntet,ibndflg,ivorerr)
               if(ibndflg.ne.2) goto 30
               itlast=itets(ntet)
               itest1=min0(iabs(itet(1,itlast)-i5),
     *                     iabs(itet(2,itlast)-i5),
     *                     iabs(itet(3,itlast)-i5),
     *                     iabs(itet(4,itlast)-i5))
               if(itest1.eq.0) then
                  idup=j
                  goto 50
               endif
 30         continue
            do 40 j=1,6
               ip1=itet(ielist(4*(j-1)+1),it2)
               ip2=itet(ielist(4*(j-1)+2),it2)
               if(ip1.eq.i3.or.ip2.eq.i3) goto 40
              if((ip1.eq.i1.and.ip2.eq.i2).or.(ip1.eq.i2.and.ip2.eq.i1))
     *            goto 40
               call vorfclst(it2,j,itets,0,ntet,ibndflg,ivorerr)
               if(ibndflg.ne.2) goto 40
               itlast=itets(ntet)
               itest2=min0(iabs(itet(1,itlast)-i4),
     *                     iabs(itet(2,itlast)-i4),
     *                     iabs(itet(3,itlast)-i4),
     *                     iabs(itet(4,itlast)-i4))
               if(itest2.eq.0) then
                  idup=j
                  goto 50
               endif
 40         continue
C
C           ____________________________________________________________
C
C
 45         continue
            if(idebug.gt.1.and.(itest1.eq.0.or.itest2.eq.0)) then
               write(logmess,9000) i,it,(itet(idum,it),idum=1,4)
               call writloga("default",0,logmess,0,ierrwrt)
 9000          format("try2to2b: i=",i5," it=",5i9)
               write(logmess,9010) idup,it2,(itet(idum,it2),idum=1,4)
               call writloga("default",0,logmess,0,ierrwrt)
 9010          format("           ",i5," it=",5i9)
               write(logmess,9010) 0,itlast,(itet(idum,itlast),idum=1,4)
               call writloga("default",0,logmess,0,ierrwrt)
               write(logmess,9010) 0,i1,i2,i3,i4,i5
               call writloga("default",0,logmess,0,ierrwrt)
            endif
C
C           ____________________________________________________________
C
C           PERFORM THE FLIP
C
            call find2to2(it,it2,i1,i2,i3,i4,i5,id(1),jd(1),
     *                    npoints,ntets)
            call flip2to2(it,it2,id(1),jd(1),
     *                    npoints,ntets)
            n2to2=n2to2+1
            kfix(1,it)=it
            kfix(1,it2)=it2
C
C           ...........................................................
C           ATTEMPT A 2-TO-0 FLIP.
C
            itets(1)=it
            itets(2)=it2
            call try2to0(itets,itwo,nflips,itx,
     *                   npoints,ntets)
            n2to0=n2to0+nflips
C
C           ...........................................................
C
            goto 100
 50      continue
C
 100  continue
C
      goto 9999
 9999 continue
      return
      end
