*dk,hextotet_att
      subroutine hextotet_att(ioption,cmotet,cmohex,ierror)
C
C#######################################################################
C
C        $Log: hextotet_att.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
C#######################################################################
C
      include "chydro.h"
      include "local_element.h"
C
C ######################################################################
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipign1, ign1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000), ign1(1000000)
C
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipuic, uic)
      pointer (ipvic, vic)
      pointer (ipwic, wic)
      REAL*8 uic(1000000), vic(1000000), wic(1000000)
C
      pointer (ippic, pic)
      pointer (ipric, ric)
      pointer (ipeic, eic)
      REAL*8 pic(1000000), ric(1000000), eic(1000000)
C
C
C     *****************************************************************
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000)
      integer jtet1(1000000)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      integer itet(4,1000000)
      integer jtet(4,1000000)
C
C#######################################################################
C
      integer ioption, ielement, numhex, numtet
      character*32 cmohex, cmotet
C
      pointer (ipimt1hex, imt1hex(1000000))
      pointer (ipitp1hex, itp1hex(1000000))
      pointer (ipicr1hex, icr1hex(1000000))
      pointer (ipxhex, xhex(1000000))
      pointer (ipyhex, yhex(1000000))
      pointer (ipzhex, zhex(1000000))
      pointer (ipihexclr, ihexclr(1000000))
      pointer (ipihextyp, ihextyp(1000000))
      pointer (ipihexoff, ihexoff(1000000))
      pointer (ipjhexoff, jhexoff(1000000))
      pointer (ipihexnn,  ihexnn(8,100000))
      pointer (ipjhexnn,  jhexnn(6,100000))
      pointer (ipihexnn,  ihex1nn(1000000))
      pointer (ipjhexnn,  jhex1nn(1000000))
C
      pointer (ipitetnn2, itetnn2(4,100000))
C
      pointer (ipihexnn1, ihexnn1(6,100000))
      pointer (ipihexnn2, ihexnn2(6,100000))
      pointer (ipktet, ktet(6,100000))
      pointer (ipialiasp, ialiasp(1000000))
      pointer (ipitdel, itdel(1000000))
      pointer (ipnncnt, nncnt(1000000))
      pointer (ipnnlst, nnlst(20,100000))
C
      pointer (ipireal1, ireal1)
      integer ireal1(1000000)
C
      real*8 xic2, yic2, zic2
      pointer (ipxic2, xic2(1000000))
      pointer (ipyic2, yic2(1000000))
      pointer (ipzic2, zic2(1000000))
C
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      integer lalias(15)
C
      dimension distmat(1000)
C
      real*8 distmax, xfacdist, xfacvol, dist
      real*8 xavg,yavg,zavg,rad1,rad2
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4
      real*8 dx,dy,dz,voltet,voltot
C
      integer ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      integer iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      integer itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
C
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
      integer jtetface(2,3,24)
      data jtetface(1, 1,  1), jtetface(2, 1,  1) /  2,  2 /
      data jtetface(1, 2,  1), jtetface(2, 2,  1) /  1,  4 /
      data jtetface(1, 3,  1), jtetface(2, 3,  1) /  3,  12 /
      data jtetface(1, 1,  2), jtetface(2, 1,  2) /  2,  3 /
      data jtetface(1, 2,  2), jtetface(2, 2,  2) /  1,  1 /
      data jtetface(1, 3,  2), jtetface(2, 3,  2) /  3,  16 /
      data jtetface(1, 1,  3), jtetface(2, 1,  3) /  2,  4 /
      data jtetface(1, 2,  3), jtetface(2, 2,  3) /  1,  2 /
      data jtetface(1, 3,  3), jtetface(2, 3,  3) /  3,  20 /
      data jtetface(1, 1,  4), jtetface(2, 1,  4) /  2,  1 /
      data jtetface(1, 2,  4), jtetface(2, 2,  4) /  1,  3 /
      data jtetface(1, 3,  4), jtetface(2, 3,  4) /  3,  21 /
      data jtetface(1, 1,  5), jtetface(2, 1,  5) /  2,  6 /
      data jtetface(1, 2,  5), jtetface(2, 2,  5) /  1,  8 /
      data jtetface(1, 3,  5), jtetface(2, 3,  5) /  3,  23 /
      data jtetface(1, 1,  6), jtetface(2, 1,  6) /  2,  7 /
      data jtetface(1, 2,  6), jtetface(2, 2,  6) /  1,  5 /
      data jtetface(1, 3,  6), jtetface(2, 3,  6) /  3,  18 /
      data jtetface(1, 1,  7), jtetface(2, 1,  7) /  2,  8 /
      data jtetface(1, 2,  7), jtetface(2, 2,  7) /  1,  6 /
      data jtetface(1, 3,  7), jtetface(2, 3,  7) /  3,  14 /
      data jtetface(1, 1,  8), jtetface(2, 1,  8) /  2,  5 /
      data jtetface(1, 2,  8), jtetface(2, 2,  8) /  1,  7 /
      data jtetface(1, 3,  8), jtetface(2, 3,  8) /  3,  10 /
      data jtetface(1, 1,  9), jtetface(2, 1,  9) /  2,  10 /
      data jtetface(1, 2,  9), jtetface(2, 2,  9) /  1,  12 /
      data jtetface(1, 3,  9), jtetface(2, 3,  9) /  3,  24 /
      data jtetface(1, 1,  10), jtetface(2, 1,  10) /  2,  11 /
      data jtetface(1, 2,  10), jtetface(2, 2,  10) /  1,  9 /
      data jtetface(1, 3,  10), jtetface(2, 3,  10) /  3,  8 /
      data jtetface(1, 1,  11), jtetface(2, 1,  11) /  2,  12 /
      data jtetface(1, 2,  11), jtetface(2, 2,  11) /  1,  10 /
      data jtetface(1, 3,  11), jtetface(2, 3,  11) /  3,  13 /
      data jtetface(1, 1,  12), jtetface(2, 1,  12) /  2,  9 /
      data jtetface(1, 2,  12), jtetface(2, 2,  12) /  1,  11 /
      data jtetface(1, 3,  12), jtetface(2, 3,  12) /  3,  1 /
      data jtetface(1, 1,  13), jtetface(2, 1,  13) /  2,  14 /
      data jtetface(1, 2,  13), jtetface(2, 2,  13) /  1,  16 /
      data jtetface(1, 3,  13), jtetface(2, 3,  13) /  3,  11 /
      data jtetface(1, 1,  14), jtetface(2, 1,  14) /  2,  15 /
      data jtetface(1, 2,  14), jtetface(2, 2,  14) /  1,  13 /
      data jtetface(1, 3,  14), jtetface(2, 3,  14) /  3,  7 /
      data jtetface(1, 1,  15), jtetface(2, 1,  15) /  2,  16 /
      data jtetface(1, 2,  15), jtetface(2, 2,  15) /  1,  14 /
      data jtetface(1, 3,  15), jtetface(2, 3,  15) /  3,  17 /
      data jtetface(1, 1,  16), jtetface(2, 1,  16) /  2,  13 /
      data jtetface(1, 2,  16), jtetface(2, 2,  16) /  1,  15 /
      data jtetface(1, 3,  16), jtetface(2, 3,  16) /  3,  2 /
      data jtetface(1, 1,  17), jtetface(2, 1,  17) /  2,  18 /
      data jtetface(1, 2,  17), jtetface(2, 2,  17) /  1,  20 /
      data jtetface(1, 3,  17), jtetface(2, 3,  17) /  3,  15 /
      data jtetface(1, 1,  18), jtetface(2, 1,  18) /  2,  19 /
      data jtetface(1, 2,  18), jtetface(2, 2,  18) /  1,  17 /
      data jtetface(1, 3,  18), jtetface(2, 3,  18) /  3,  6 /
      data jtetface(1, 1,  19), jtetface(2, 1,  19) /  2,  20 /
      data jtetface(1, 2,  19), jtetface(2, 2,  19) /  1,  18 /
      data jtetface(1, 3,  19), jtetface(2, 3,  19) /  3,  22 /
      data jtetface(1, 1,  20), jtetface(2, 1,  20) /  2,  17 /
      data jtetface(1, 2,  20), jtetface(2, 2,  20) /  1,  19 /
      data jtetface(1, 3,  20), jtetface(2, 3,  20) /  3,  3 /
      data jtetface(1, 1,  21), jtetface(2, 1,  21) /  2,  22 /
      data jtetface(1, 2,  21), jtetface(2, 2,  21) /  1,  24 /
      data jtetface(1, 3,  21), jtetface(2, 3,  21) /  3,  4 /
      data jtetface(1, 1,  22), jtetface(2, 1,  22) /  2,  23 /
      data jtetface(1, 2,  22), jtetface(2, 2,  22) /  1,  21 /
      data jtetface(1, 3,  22), jtetface(2, 3,  22) /  3,  19 /
      data jtetface(1, 1,  23), jtetface(2, 1,  23) /  2,  24 /
      data jtetface(1, 2,  23), jtetface(2, 2,  23) /  1,  22 /
      data jtetface(1, 3,  23), jtetface(2, 3,  23) /  3,  5 /
      data jtetface(1, 1,  24), jtetface(2, 1,  24) /  2,  21 /
      data jtetface(1, 2,  24), jtetface(2, 2,  24) /  1,  23 /
      data jtetface(1, 3,  24), jtetface(2, 3,  24) /  3,  9 /
C
C
      integer ihex5tet(4,5,2)
      data ihex5tet / 1, 2, 4, 5,
     *                3, 2, 7, 4,
     *                6, 2, 5, 7,
     *                8, 4, 7, 5,
     *                2, 4, 5, 7,
     *                2, 1, 6, 3,
     *                4, 1, 3, 8,
     *                5, 1, 8, 6,
     *                7, 3, 6, 8,
     *                1, 3, 8, 6 /
C
      integer ihex6tet(4,6)
      data ihex6tet / 5, 6, 2, 3,
     *                5, 6, 3, 8,
     *                1, 8, 2, 3,
     *                1, 5, 2, 3,
     *                6, 7, 3, 8,
     *                1, 5, 3, 8 /
C
      pointer (ipihexclr5, ihexclr5)
      integer ihexclr5(1000000)
C
      data iradavg  / 0 /
      data iremove  / 1 /
      data icheckpt / 1 /
      data intflag  / 0 /
      data nnflag  / 0 /
      data idumphex / 0 /
      data idumpx3d / 0 /
      data ireadx3d / 0 /
      data irecon / 0 /
 
C
      character*32 isubname, cmotype
C
C#######################################################################
C
C
      isubname='hextotet_att'
      iwerr=0
C
      call cmo_set_name(cmohex,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_set_name')
C
      call cmo_get_info('idebug',cmohex,idebug,lencmo,itpcmo,ierror)
 
      call cmo_get_info('nnodes',cmohex,npoints,lencmo,itpcmo,ierror)
      call cmo_get_info('nelements',cmohex,numhex,lencmo,itpcmo,ierror)
      call cmo_get_info('mbndry',cmohex,mbndry,lencmo,itpcmo,ierror)
      call cmo_get_info('imt1',cmohex,ipimt1hex,lenimt1hex,icmotype,ier)
      call cmo_get_info('itp1',cmohex,ipitp1hex,lenitp1hex,icmotype,ier)
      call cmo_get_info('icr1',cmohex,ipicr1hex,lenicr1hex,icmotype,ier)
      call cmo_get_info('xic',cmohex,ipxhex,lenxhex,icmotype,ierror)
      call cmo_get_info('yic',cmohex,ipyhex,lenyhex,icmotype,ierror)
      call cmo_get_info('zic',cmohex,ipzhex,lenzhex,icmotype,ierror)
      call cmo_get_info('itetclr',cmohex,
     *                   ipihexclr,lenihexclr,icmotype,ier)
      call cmo_get_info('itettyp',cmohex,
     *                   ipihextyp,lenihextyp,icmotype,ier)
      call cmo_get_info('itetoff',cmohex,
     *                   ipihexoff,lenihexoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmohex,
     *                   ipjhexoff,lenjhexoff,icmotype,ier)
      call cmo_get_info('itet',cmohex,ipihexnn,lenihex,icmotype,ierror)
      call cmo_get_info('jtet',cmohex,ipjhexnn,lenjhex,icmotype,ierror)
C
      length=6*numhex
      call mmgetblk("ktet",isubname,ipktet,length,2,icscode)
      call mmgetblk("ihexnn1",isubname,ipihexnn1,length,2,icscode)
      call mmgetblk("ihexnn2",isubname,ipihexnn2,length,2,icscode)
C
      numhex1=0
      do ih=1,numhex
         if(ihexclr(ih).gt.0) then
            numhex1=numhex1+1
            ihexclr(numhex1)=ihexclr(ih)
            ihextyp(numhex1)=ihextyp(ih)
            ihexoff(numhex1)=nelmnen(ihextyp(ih))*(ih-1)
            jhexoff(numhex1)=nelmnef(ihextyp(ih))*(ih-1)
            do i=1,nelmnen(ihextyp(ih))
               ihexnn(i,numhex1)=ihexnn(i,ih)
            enddo
            do i=1,nelmnef(ihextyp(ih))
               jhexnn(i,numhex1)=jhexnn(i,ih)
            enddo
         endif
      enddo
      if(numhex1.gt.0.and.numhex1.lt.numhex) then
         write(logmess,9000) numhex,numhex1
         call writloga('default',0,logmess,0,ierwrt)
 9000    format("Compressing out zero-color hexes: ",2i10)
      endif
      numhex=numhex1
      call cmo_set_info('nelements',cmohex,numhex,1,1,ierror)
C
      call elmtestd(cmohex,20,ierror)
      if(ierror.eq.0) then
         do it=1,numhex
            nefhex=nelmnef(ihextyp(it))
            do i=1,nefhex
               if(jhex1nn(jhexoff(it)+i).gt.0 .and.
     *            jhex1nn(jhexoff(it)+i).lt.mbndry) then
                  jt=1+(jhex1nn(jhexoff(it)+i)-1)/nefhex
                  jf=jhex1nn(jhexoff(it)+i)-nefhex*(jt-1)
                  ihexnn1(i,it)=jt
                  ihexnn2(i,it)=jf
               else
                  ihexnn1(i,it)=0
                  ihexnn2(i,it)=0
               endif
            enddo
         enddo
      else
         do it=1,numhex
            do i=1,nelmnef(ihextyp(it))
               ktet(i,it)=0
               ihexnn1(i,it)=-1
               ihexnn2(i,it)=-2
            enddo
         enddo
         call geniee(ihexnn,ihexnn1,ihexnn2,8,6,numhex,npoints,
     *        3,npoints,numhex)
      endif
C
      do it=1,numhex
         do i=1,nelmnef(ihextyp(it))
            if(ihexnn1(i,it).eq.0) then
               jhexnn(i,it)=mbndry
            else
               jt=ihexnn1(i,it)
               jf=ihexnn2(i,it)
               if(ihexclr(it).ne.ihexclr(jt)) then
                  jhexnn(i,it)=mbndry+6*(ihexnn1(i,it)-1)+ihexnn2(i,it)
               else
                  jhexnn(i,it)=6*(ihexnn1(i,it)-1)+ihexnn2(i,it)
               endif
            endif
         enddo
      enddo
C
C
      length=npoints+numhex+6*numhex
      call mmgetblk("xic2",isubname,ipxic2,length,2,icscode)
      call mmgetblk("yic2",isubname,ipyic2,length,2,icscode)
      call mmgetblk("zic2",isubname,ipzic2,length,2,icscode)
      do i=1,length
         xic2(i)=0.0
         yic2(i)=0.0
         zic2(i)=0.0
      enddo
      do i=1,npoints
         xic2(i)=xhex(i)
         yic2(i)=yhex(i)
         zic2(i)=zhex(i)
      enddo
      distmax=0.0
      distmin=1.0e+30
      do i=1,1000
         distmat(i)=0.0d+00
      enddo
      imtmax=0
      do i=1,numhex
         do j=1,12
            i1= ihexnn(intpairhex(1,j),i)
            i2= ihexnn(intpairhex(2,j),i)
            dist=(xic2(i1)-xic2(i2))**2 +
     *           (yic2(i1)-yic2(i2))**2 +
     *           (zic2(i1)-zic2(i2))**2
            imtmax=max(imtmax,ihexclr(i))
            distmat(ihexclr(i))=max(distmat(ihexclr(i)),dist)
            distmin=min(distmin,dist)
         enddo
      enddo
      distmax=1.0d+30
      do i=1,imtmax
         if(distmat(i).le.1.0d-30) distmat(i)=1.0e+30
         distmax=min(distmax,distmat(i))
      enddo
      xfacdist=1.0e-06 * sqrt(distmax)
      write(logmess,9010) xfacdist,distmax
      call writloga('default',0,logmess,0,ierwrt)
 9010 format("Epsilon-distance: ",2(1pe15.7))
C
C
      inegvol=0
      numhex1=0
      volmax=0.0d+00
      do it=1,numhex
         volmin=1.0d+30
         do j=1,2
            voltot=0.0
            do i=1,5
               i1=ihexnn(ihex5tet(1,i,j),it)
               i2=ihexnn(ihex5tet(2,i,j),it)
               i3=ihexnn(ihex5tet(3,i,j),it)
               i4=ihexnn(ihex5tet(4,i,j),it)
               x1=xic2(i1)
               y1=yic2(i1)
               z1=zic2(i1)
               x2=xic2(i2)-x1
               y2=yic2(i2)-y1
               z2=zic2(i2)-z1
               x3=xic2(i3)-x1
               y3=yic2(i3)-y1
               z3=zic2(i3)-z1
               x4=xic2(i4)-x1
               y4=yic2(i4)-y1
               z4=zic2(i4)-z1
               dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
               dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
               dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
               voltet=-(x3*dx+y3*dy+z3*dz) / 6.0
               voltot=voltot+voltet
               volmin=min(volmin,voltet)
               volmax=max(volmax,voltet)
            enddo
         enddo
         numhex1=numhex1+1
         if(volmin.lt.-1.0e-06) then
            inegvol=inegvol+1
            if(inegvol.lt.20) then
               write(logmess,9030) inegvol,it,volmin,voltot
               call writloga('default',0,logmess,0,ierwrt)
 9030          format("  Hex with negative volume: ",2i10,2(1pe15.7))
               write(logmess,9031) (ihexnn(i,it),i=1,8)
               call writloga('default',0,logmess,0,ierwrt)
 9031          format("  Hex indices: ",8i8)
            endif
         else
C*****            numhex1=numhex1+1
C*****            ihexclr(numhex1)=ihexclr(it)
C*****            do i=1,8
C*****               ihexnn(i,numhex1)=ihexnn(i,it)
C*****            enddo
         endif
      enddo
C
      xfacvol=1.0e-06 * volmax
      write(logmess,9011) xfacvol,volmax
      call writloga('default',0,logmess,0,ierwrt)
 9011 format("Epsilon-volume: ",2(1pe15.7))
C
      if(numhex1.gt.0.and.numhex1.lt.numhex) then
         write(logmess,9040) numhex,numhex1
         call writloga('default',0,logmess,0,ierwrt)
 9040    format("Zero-volume hexes: ",2i10)
      endif
      numhex=numhex1
C
 9990 continue
C
      call cmo_set_info('nelements',cmohex,numhex,1,1,ierror)
      if(inegvol.gt.0) then
         write(logmess,9050) numhex,inegvol
         call writloga('default',0,logmess,0,ierwrt)
 9050    format("Total number of negative volume hexes: ",2i10)
      endif
C
      npstart=npoints
      if(ioption.eq.5) then
         nnodes_inc=0
         nelements_inc=5*numhex
      elseif(ioption.eq.24) then
         npoints=npoints+numhex
         do ielement=1,numhex
            do i=1,6
               ielop=ihexnn1(i,ielement)
               iflop=ihexnn2(i,ielement)
               if(ielop.eq.0.or.ielop.gt.ielement) then
                  npoints=npoints+1
                  lalias(i+8)=npoints
                  ktet(i,ielement)=npoints
               else
                  ktet(i,ielement)=ktet(iflop,ielop)
               endif
            enddo
         enddo
         nnodes_inc=npoints-npstart
         nelements_inc=24*numhex
      endif
C
      call cmo_set_name(cmotet,ierror)
C
      nsd=3
      nsdgeom=3
      nsdtopo=3
      nen=4
      nef=4
      mbndry=16000000
      nnodes=npoints
      numtet=0
      nelements=numhex+nelements_inc
      call cmo_set_info('nnodes',cmotet,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmotet,nelements,1,1,ierror)
      call cmo_set_info('mbndry',cmotet,mbndry,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmotet,nsdgeom,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmotet,nsdtopo,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmotet,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmotet,nef,1,1,ierror)
C
      call cmo_newlen(cmotet,ierror)
C
      call cmo_get_info('isetwd',cmotet,ipisetwd,lenisetwd,icmotype,ier)
      call cmo_get_info('imt1',cmotet,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmotet,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmotet,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmotet,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('ign1',cmotet,ipign1,lenign1,icmotype,ierror)
      call cmo_get_info('xic',cmotet,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmotet,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmotet,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('uic',cmotet,ipuic,lenuic,icmotype,ierror)
      call cmo_get_info('vic',cmotet,ipvic,lenvic,icmotype,ierror)
      call cmo_get_info('wic',cmotet,ipwic,lenwic,icmotype,ierror)
      call cmo_get_info('pic',cmotet,ippic,lenpic,icmotype,ierror)
      call cmo_get_info('ric',cmotet,ipric,lenric,icmotype,ierror)
      call cmo_get_info('eic',cmotet,ipeic,leneic,icmotype,ierror)
      call cmo_get_info('itetclr',cmotet,
     *                   ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmotet,
     *                   ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmotet,
     *                   ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmotet,
     *                   ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmotet,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmotet,ipjtet,lenjtet,icmotype,ierror)
C
      do i1=1,npoints
         imt1(i1)=imt1hex(i1)
         itp1(i1)=itp1hex(i1)
         icr1(i1)=icr1hex(i1)
         xic(i1)=xhex(i1)
         yic(i1)=yhex(i1)
         zic(i1)=zhex(i1)
      enddo
C
      length=4*ioption*numhex
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,2,icscode)
C
      do i=npstart+1,npoints
         imt1(i)=0
         itp1(i)=0
         icr1(i)=0
         isn1(i)=0
         ign1(i)=0
         xic(i)=0.0
         yic(i)=0.0
         zic(i)=0.0
      enddo
C
      do it=numtet+1,nelements
         do i=1,4
            itet(i,it)=0
            jtet(i,it)=-1
            itetnn2(i,it)=-1
         enddo
      enddo
C
      ntetstart=numtet+1
      ntet=numtet
      do ielement=1,numhex
         ntstart=ntet
         do i=1,8
            lalias(i)=ihexnn(i,ielement)
         enddo
         if(ioption.eq.5) then
            if(ielement.eq.1) then
      do ih=1,numhex
         do i=1,6
            jh=ihexnn1(i,ih)
            if(jh.le.0.or.jh.gt.numhex) then
                   ip1=ihexnn(ihexface1(1,i),ih)
                   ip2=ihexnn(ihexface1(2,i),ih)
                   ip3=ihexnn(ihexface1(3,i),ih)
                   ip4=ihexnn(ihexface1(4,i),ih)
                   xfacei=0.25d+00*(xic(ip1)+xic(ip2)+xic(ip3)+xic(ip4))
                   yfacei=0.25d+00*(yic(ip1)+yic(ip2)+yic(ip3)+yic(ip4))
                   zfacei=0.25d+00*(zic(ip1)+zic(ip2)+zic(ip3)+zic(ip4))
                     do jh=1,numhex
                        do j=1,6
                   jp1=ihexnn(ihexface1(1,j),jh)
                   jp2=ihexnn(ihexface1(2,j),jh)
                   jp3=ihexnn(ihexface1(3,j),jh)
                   jp4=ihexnn(ihexface1(4,j),jh)
                   xfacej=0.25d+00*(xic(jp1)+xic(jp2)+xic(jp3)+xic(jp4))
                   yfacej=0.25d+00*(yic(jp1)+yic(jp2)+yic(jp3)+yic(jp4))
                   zfacej=0.25d+00*(zic(jp1)+zic(jp2)+zic(jp3)+zic(jp4))
      dist=(xfacej-xfacei)**2+
     *     (yfacej-yfacei)**2+
     *     (zfacej-zfacei)**2
      if(ih.ne.jh.and.dist.lt.xfacdist) then
         ihexnn1(i,ih)=jh
         ihexnn2(i,ih)=j
         ihexnn1(j,jh)=ih
         ihexnn2(j,jh)=i
                  endif
               enddo
            enddo
 500        continue
         endif
      enddo
      enddo
               length=numhex
               call mmgetblk("ihexclr5",isubname,ipihexclr5,length,2,
     *                       icscode)
               do i=1,numhex
                  ihexclr5(i)=0
               enddo
               ihexclr5(1)=1
 400           continue
                  icount=0
                  do ih=1,numhex
                     if(ihexclr5(ih).eq.0) then
                        do i=1,6
                           jh=ihexnn1(i,ih)
                           if(jh.gt.0.and.jh.le.numhex) then
                              if(ihexclr5(ih).ne.0) then
                                 if(ihexclr5(jh).eq.ihexclr5(ih)) then
                                    write(logmess,9060) ih,i,jh,
     *                                                  ihexclr5(ih),
     *                                                  ihexclr5(jh)
                                    call writloga('default',0,logmess,
     *                                            0,ierwrt)
 9060                               format("Hex5 color error: ",5i10)
                                 endif
                              else
                                 if(ihexclr5(jh).eq.1) then
                                    icount=icount+1
                                    ihexclr5(ih)=2
                                 elseif(ihexclr5(jh).eq.2) then
                                    icount=icount+1
                                    ihexclr5(ih)=1
                                 endif
                              endif
                           endif
                        enddo
                     endif
                  enddo
                  if(icount.gt.0) goto 400
C**************call mmrelblk("ihexclr5",isubname,ipihexclr5,icscode)
                  do ih=1,numhex
                     if(ihexclr5(ih).le.0) then
                        write(logmess,9070) ih,ihexclr5(ih)
                        call writloga('default',0,logmess,0,ierwrt)
 9070                   format("Hex5 with no color: ",2i10)
                     endif
                  enddo
               endif
               do i=1,5
                  ntet=ntet+1
                  itetclr(ntet)=ihexclr(ielement)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=nen*(ntet-1)
                  jtetoff(ntet)=nef*(ntet-1)
                  itet(1,ntet)=lalias(ihex5tet(1,i,ihexclr5(ielement)))
                  itet(2,ntet)=lalias(ihex5tet(2,i,ihexclr5(ielement)))
                  itet(3,ntet)=lalias(ihex5tet(3,i,ihexclr5(ielement)))
                  itet(4,ntet)=lalias(ihex5tet(4,i,ihexclr5(ielement)))
                  do j=1,4
                     jtet(j,ntet)=-1
                     itetnn2(j,ntet)=-1
                  enddo
               enddo
         elseif(ioption.eq.6) then
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=5
            itet(2,ntet)=6
            itet(3,ntet)=2
            itet(4,ntet)=3
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=5
            itet(2,ntet)=6
            itet(3,ntet)=3
            itet(4,ntet)=8
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=1
            itet(2,ntet)=8
            itet(3,ntet)=3
            itet(4,ntet)=4
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=1
            itet(2,ntet)=5
            itet(3,ntet)=2
            itet(4,ntet)=3
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=6
            itet(2,ntet)=7
            itet(3,ntet)=3
            itet(4,ntet)=8
            ntet=ntet+1
            itetclr(ntet)=ihexclr(ielement)
            itettyp(ntet)=ifelmtet
            itetoff(ntet)=nen*(ntet-1)
            jtetoff(ntet)=nef*(ntet-1)
            itet(1,ntet)=1
            itet(2,ntet)=5
            itet(3,ntet)=3
            itet(4,ntet)=8
         elseif(ioption.eq.24) then
            lalias(15)=npstart+ielement
            do i=1,6
               lalias(i+8)=ktet(i,ielement)
               do j=1,4
                  jp1=j+1
                  if(jp1.gt.4) jp1=1
                  ntet=ntet+1
                  itetclr(ntet)=ihexclr(ielement)
                  itettyp(ntet)=ifelmtet
                  itetoff(ntet)=nen*(ntet-1)
                  jtetoff(ntet)=nef*(ntet-1)
                  itet(1,ntet)=ihexface1(j,i)
                  itet(2,ntet)=ihexface1(jp1,i)
                  itet(3,ntet)=i+8
                  itet(4,ntet)=15
               enddo
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            ign1(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,8
               itp=max(itp,itp1(ihexnn(i,ielement)))
               icr=min(icr,icr1(ihexnn(i,ielement)))
               ign=max(ign,ign1(ihexnn(i,ielement)))
               xr=xic2(ihexnn(i,ielement))
               yr=yic2(ihexnn(i,ielement))
               zr=zic2(ihexnn(i,ielement))
               xic2(lalias(15))=xic2(lalias(15))+xr
               yic2(lalias(15))=yic2(lalias(15))+yr
               zic2(lalias(15))=zic2(lalias(15))+zr
               rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
            enddo
            imt1(lalias(15))=ihexclr(ielement)
            itp1(lalias(15))=itp
            icr1(lalias(15))=icr
            ign1(lalias(15))=1+ign
            xic2(lalias(15))=xic2(lalias(15))/8.0
            yic2(lalias(15))=yic2(lalias(15))/8.0
            zic2(lalias(15))=zic2(lalias(15))/8.0
            rad1=rad1/8.0
            rad2=sqrt(xic2(lalias(15))**2+
     *                yic2(lalias(15))**2+
     *                zic2(lalias(15))**2)
            if(iradavg.eq.1) then
               xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
               yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
               zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
            endif
            do i=1,6
               ict=0
               imt1(lalias(i+8))=imt1(ihexnn(ihexface1(1,i),ielement))
               itp1(lalias(i+8))=0
               icr1(lalias(i+8))=0
               ign1(lalias(i+8))=0
               xic2(lalias(i+8))=0.0
               yic2(lalias(i+8))=0.0
               zic2(lalias(i+8))=0.0
               uic(lalias(i+8))=0.0
               vic(lalias(i+8))=0.0
               wic(lalias(i+8))=0.0
               pic(lalias(i+8))=0.0
               ric(lalias(i+8))=0.0
               rad1=0.0
               itp=0
               icr=999999
               ign=0
               do j=1,4
                  xr=xic2(ihexnn(ihexface1(j,i),ielement))
                  yr=yic2(ihexnn(ihexface1(j,i),ielement))
                  zr=zic2(ihexnn(ihexface1(j,i),ielement))
                  imt=imt1(ihexnn(ihexface1(j,i),ielement))
                  itp=max(itp,itp1(ihexnn(ihexface1(j,i),ielement)))
                  icr=min(icr,icr1(ihexnn(ihexface1(j,i),ielement)))
                  ign=max(ign,ign1(ihexnn(ihexface1(j,i),ielement)))
                  if(imt1(lalias(i+8)).eq.imt) ict=ict+1
                  xic2(lalias(i+8))=xic2(lalias(i+8))+xr
                  yic2(lalias(i+8))=yic2(lalias(i+8))+yr
                  zic2(lalias(i+8))=zic2(lalias(i+8))+zr
                  uic(lalias(i+8))=uic(lalias(i+8))+
     *                             uic(ihexnn(ihexface1(j,i),ielement))
                  vic(lalias(i+8))=vic(lalias(i+8))+
     *                             vic(ihexnn(ihexface1(j,i),ielement))
                  wic(lalias(i+8))=wic(lalias(i+8))+
     *                             wic(ihexnn(ihexface1(j,i),ielement))
                  pic(lalias(i+8))=pic(lalias(i+8))+
     *                             pic(ihexnn(ihexface1(j,i),ielement))
                  ric(lalias(i+8))=ric(lalias(i+8))+
     *                             ric(ihexnn(ihexface1(j,i),ielement))
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               enddo
               itp1(lalias(i+8))=itp
               icr1(lalias(i+8))=icr
               ign1(lalias(i+8))=1+ign
               if(ict.ne.4) then
                  imtel1=ihexclr(ielement)
                  if(ihexnn1(i,ielement).eq.0) then
                     imt1(lalias(i+8))=imtel1
                  else
                     jhex=ihexnn1(i,ielement)
                     if(jhex.gt.0.and.jhex.le.numhex) then
                        imtel2=ihexclr(ihexnn1(i,ielement))
                     else
                        imtel2=imtel1
                     endif
                     if(imtel1.eq.imtel2) then
                        imt1(lalias(i+8))=imtel1
                     else
 
                        if (iwerr.le.20 .or. idebug.gt.0) then
                          write(logmess,9080) ielement,i,lalias(i+8)
                          call writloga('default',0,logmess,0,ierwrt)
 9080                     format("Error in assigning node color:",
     *                         " element=",i10," face=",i3," node=",i10)
                        endif
                        iwerr=iwerr+1
                        imt1(lalias(i+8))=imtel1
                    endif
                  endif
               endif
               xic2(lalias(i+8))=xic2(lalias(i+8))/4.0
               yic2(lalias(i+8))=yic2(lalias(i+8))/4.0
               zic2(lalias(i+8))=zic2(lalias(i+8))/4.0
               rad1=rad1/4.0
               rad2=sqrt(xic2(lalias(i+8))**2+
     *                   yic2(lalias(i+8))**2+
     *                   zic2(lalias(i+8))**2)
               if(iradavg.eq.1) then
                  xic2(lalias(i+8))=xic2(lalias(i+8))*rad1/rad2
                  yic2(lalias(i+8))=yic2(lalias(i+8))*rad1/rad2
                  zic2(lalias(i+8))=zic2(lalias(i+8))*rad1/rad2
               endif
            enddo
            do j=ntstart+1,ntet
               do i=1,4
                  itet(i,j)=lalias(itet(i,j))
                  if(i.lt.4) then
                     ioff=j-ntstart
                     jtet(i,j)=ntstart+jtetface(2,i,ioff)
                     itetnn2(i,j)=jtetface(1,i,ioff)
C*****               jtet(i,j)=-1
C*****               itetnn2(i,j)=-1
                  else
                     jtet(i,j)=-1
                  endif
               enddo
            enddo
         endif
      enddo
      length=npoints+numhex+6*numhex
      call mmgetblk("ialiasp",isubname,ipialiasp,length,2,icscode)
      do i=1,npoints+numhex+6*numhex
         ialiasp(i)=i
      enddo
      do i=1,numhex
         do j=1,12
            i1=ialiasp(ihexnn(intpairhex(1,j),i))
            i2=ialiasp(ihexnn(intpairhex(2,j),i))
            dist=(xic2(i1)-xic2(i2))**2 +
     *           (yic2(i1)-yic2(i2))**2 +
     *           (zic2(i1)-zic2(i2))**2
            i1=max(i1,i2)
            if(dist.le.xfacdist) then
               do k=1,8
                  i2=ihexnn(k,i)
                  dist=(xic2(i1)-xic2(i2))**2 +
     *                 (yic2(i1)-yic2(i2))**2 +
     *                 (zic2(i1)-zic2(i2))**2
                  if(dist.le.xfacdist) then
                     i3=max(i1,i2)
                     i2=i1+i2-i3
                     ialiasp(i2)=ialiasp(i3)
                  endif
               enddo
            endif
         enddo
      enddo
      if(ioption.eq.24) then
         do i=1,numhex
            do j=1,6
               i1=ktet(j,i)
               jt=ihexnn1(j,i)
               jf=ihexnn2(j,i)
               if(jt.gt.0.and.jt.le.numhex) then
                  i2=ktet(jf,jt)
                  dist=(xic2(i1)-xic2(i2))**2 +
     *                 (yic2(i1)-yic2(i2))**2 +
     *                 (zic2(i1)-zic2(i2))**2
                  if(dist.le.xfacdist) then
                     i3=max(i1,i2)
                     i2=i1+i2-i3
                     ialiasp(i2)=ialiasp(i3)
                     do k=1,6
                        i2=ktet(k,jt)
                        dist=(xic2(i1)-xic2(i2))**2 +
     *                       (yic2(i1)-yic2(i2))**2 +
     *                       (zic2(i1)-zic2(i2))**2
                        if(dist.le.xfacdist) then
                           i3=max(i1,i2)
                           i2=i1+i2-i3
                           ialiasp(i2)=ialiasp(i3)
                        endif
                     enddo
                  endif
               endif
               i1=ktet(j,i)
               do k=1,6
                  i2=ktet(k,i)
                  dist=(xic2(i1)-xic2(i2))**2 +
     *                 (yic2(i1)-yic2(i2))**2 +
     *                 (zic2(i1)-zic2(i2))**2
                  if(dist.le.xfacdist) then
                     i3=max(i1,i2)
                     i2=i1+i2-i3
                     ialiasp(i2)=ialiasp(i3)
                  endif
                  do l=1,4
                     i2=ihexnn(ihexface1(l,k),i)
                     dist=(xic2(i1)-xic2(i2))**2 +
     *                    (yic2(i1)-yic2(i2))**2 +
     *                    (zic2(i1)-zic2(i2))**2
                     if(dist.le.xfacdist) then
                        i3=max(i1,i2)
                        i2=i1+i2-i3
                        ialiasp(i2)=ialiasp(i3)
                     endif
                  enddo
               enddo
            enddo
         enddo
         do i=1,numhex
            i1=npstart+i
            do k=1,8
               i2=ihexnn(k,i)
               dist=(xic2(i1)-xic2(i2))**2 +
     *              (yic2(i1)-yic2(i2))**2 +
     *              (zic2(i1)-zic2(i2))**2
               if(dist.le.xfacdist) then
                  i3=max(i1,i2)
                  i2=i1+i2-i3
                  ialiasp(i2)=ialiasp(i3)
               endif
            enddo
            do k=1,6
               i2=ktet(k,i)
               dist=(xic2(i1)-xic2(i2))**2 +
     *              (yic2(i1)-yic2(i2))**2 +
     *              (zic2(i1)-zic2(i2))**2
               if(dist.le.xfacdist) then
                  i3=max(i1,i2)
                  i2=i1+i2-i3
                  ialiasp(i2)=ialiasp(i3)
               endif
            enddo
            do k=1,6
             i1=npstart+i
             jt=ihexnn1(k,i)
             if(jt.gt.0) then
                i2=npstart+jt
                dist=(xic2(i1)-xic2(i2))**2 +
     *               (yic2(i1)-yic2(i2))**2 +
     *               (zic2(i1)-zic2(i2))**2
                if(dist.le.xfacdist) then
                   i3=max(i1,i2)
                   i2=i1+i2-i3
                   ialiasp(i2)=ialiasp(i3)
                   do l=1,6
                      i1=ktet(l,i)
                      do m=1,6
                         i2=ktet(m,jt)
                          dist=(xic2(i1)-xic2(i2))**2 +
     *                         (yic2(i1)-yic2(i2))**2 +
     *                         (zic2(i1)-zic2(i2))**2
                          if(dist.le.xfacdist) then
                             i3=max(i1,i2)
                             i2=i1+i2-i3
                             ialiasp(i2)=ialiasp(i3)
                         endif
                      enddo
                   enddo
                endif
             endif
            enddo
         enddo
      endif
      npoints1=npoints+numhex+6*numhex
      do i=1,npoints1
         ict=0
         i1=i
 200     continue
         ict=ict+1
         if(ict.gt.npoints1) then
            write(logmess,9090) i,i1,ialiasp(i1)
            call writloga('default',0,logmess,0,ierwrt)
 9090       format("Infinite loop on alias list: ",3i10)
            stop
         endif
         if(i1.ne.ialiasp(i1)) then
            i1=ialiasp(i1)
            goto 200
         else
            ialiasp(i)=i1
         endif
      enddo
      do ielement=1,numhex
         do i=1,8
            lalias(i)=ihexnn(i,ielement)
         enddo
         if(ioption.eq.5) then
         elseif(ioption.eq.24) then
            lalias(15)=npstart+ielement
            do i=1,6
               lalias(i+8)=ktet(i,ielement)
            enddo
            imt1(lalias(15))=0
            itp1(lalias(15))=0
            icr1(lalias(15))=0
            ign1(lalias(15))=0
            xic2(lalias(15))=0.0
            yic2(lalias(15))=0.0
            zic2(lalias(15))=0.0
            uic(lalias(15))=0.0
            vic(lalias(15))=0.0
            wic(lalias(15))=0.0
            pic(lalias(15))=0.0
            ric(lalias(15))=0.0
            rad1=0.0
            rad1=0.0
            itp=0
            icr=9999999
            ign=0
            ict=0
            do i=1,8
               i1=ihexnn(i,ielement)
C*****         if(ialiasp(i1).eq.i1) then
               if(i1.eq.i1) then
                  ict=ict+1
                  itp=max(itp,itp1(i1))
                  icr=min(icr,icr1(i1))
                  itp=max(itp,itp1(i1))
                  xr=xic2(i1)
                  yr=yic2(i1)
                  zr=zic2(i1)
                  xic2(lalias(15))=xic2(lalias(15))+xr
                  yic2(lalias(15))=yic2(lalias(15))+yr
                  zic2(lalias(15))=zic2(lalias(15))+zr
                  uic(lalias(15))=uic(lalias(15))+uic(i1)
                  vic(lalias(15))=vic(lalias(15))+vic(i1)
                  wic(lalias(15))=wic(lalias(15))+wic(i1)
                  pic(lalias(15))=pic(lalias(15))+pic(i1)
                  ric(lalias(15))=ric(lalias(15))+ric(i1)
                  rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
               endif
            enddo
            if(ict.gt.0) then
               imt1(lalias(15))=ihexclr(ielement)
               itp1(lalias(15))=itp
               icr1(lalias(15))=icr
               ign1(lalias(15))=1+ign
               xic2(lalias(15))=xic2(lalias(15))/dble(ict)
               yic2(lalias(15))=yic2(lalias(15))/dble(ict)
               zic2(lalias(15))=zic2(lalias(15))/dble(ict)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(ict)
                  rad2=sqrt(xic2(lalias(15))**2+
     *                      yic2(lalias(15))**2+
     *                      zic2(lalias(15))**2)
                  xic2(lalias(15))=xic2(lalias(15))*rad1/rad2
                  yic2(lalias(15))=yic2(lalias(15))*rad1/rad2
                  zic2(lalias(15))=zic2(lalias(15))*rad1/rad2
               endif
               uic(lalias(15))= uic(lalias(15))/dble(ict)
               vic(lalias(15))= vic(lalias(15))/dble(ict)
               wic(lalias(15))= wic(lalias(15))/dble(ict)
               pic(lalias(15))= pic(lalias(15))/dble(ict)
               ric(lalias(15))= ric(lalias(15))/dble(ict)
            endif
            do i=1,6
               ict=0
               imt1(lalias(i+8))=
     *            imt1(ihexnn(ihexface1(1,i),ielement))
               ign1(lalias(i+8))=0
               xic2(lalias(i+8))=0.0
               yic2(lalias(i+8))=0.0
               zic2(lalias(i+8))=0.0
               uic(lalias(i+8))=0.0
               vic(lalias(i+8))=0.0
               wic(lalias(i+8))=0.0
               pic(lalias(i+8))=0.0
               ric(lalias(i+8))=0.0
               rad1=0.0
               itp=0
               icr=9999999
               ign=0
               ict1=0
               do j=1,4
                  i1=ihexnn(ihexface1(j,i),ielement)
C*****            if(ialiasp(i1).eq.i1) then
                  if(i1.eq.i1) then
                     ict1=ict1+1
                     xr=xic2(i1)
                     yr=yic2(i1)
                     zr=zic2(i1)
                     imt=imt1(i1)
                     itp=max(itp,itp1(i1))
                     icr=min(icr,icr1(i1))
                     ign=max(ign,ign1(i1))
                     if(imt1(lalias(i+8)).eq.imt) ict=ict+1
                     xic2(lalias(i+8))=xic2(lalias(i+8))+xr
                     yic2(lalias(i+8))=yic2(lalias(i+8))+yr
                     zic2(lalias(i+8))=zic2(lalias(i+8))+zr
                     uic(lalias(i+8))=uic(lalias(i+8))+
     *                                uic(i1)
                     vic(lalias(i+8))=vic(lalias(i+8))+
     *                                vic(i1)
                     wic(lalias(i+8))=wic(lalias(i+8))+
     *                                wic(i1)
                     pic(lalias(i+8))=pic(lalias(i+8))+
     *                                pic(i1)
                     ric(lalias(i+8))=ric(lalias(i+8))+
     *                                ric(i1)
                     rad1=rad1+sqrt(xr*xr+yr*yr+zr*zr)
                  endif
               enddo
               if(ict1.gt.0) then
                  itp1(lalias(i+8))=itp
                  icr1(lalias(i+8))=icr
                  ign1(lalias(i+8))=1+ign
                  if(ict.ne.4) then
                     imtel1=ihexclr(ielement)
                     if(ihexnn1(i,ielement).eq.0) then
                        imt1(lalias(i+8))=imtel1
                     else
                        jhex=ihexnn1(i,ielement)
                        if(jhex.gt.0.and.jhex.le.numhex) then
                           imtel2=ihexclr(ihexnn1(i,ielement))
                        else
                           imtel2=imtel1
                        endif
                        if(imtel1.eq.imtel2) then
                           imt1(lalias(i+8))=imtel1
                        else
                           if (iwerr.le.20 .or. idebug.gt.0) then
                             write(logmess,9100) ielement,i,lalias(i+8),
     *                       (ihexnn(ihexface1(k,i),ielement),k=1,4)
                             call writloga('default',0,logmess,0,ierwrt)
 9100                        format("Error in assigning node color:",
     *                            " element=",i10," face=",i3,
     *                            " node=",i10," face nodes: ",4i10)
                           endif
                           iwerr=iwerr+1
                           imt1(lalias(i+8))=imtel1
                        endif
                     endif
                  endif
                  xic2(lalias(i+8))=xic2(lalias(i+8))/dble(ict1)
                  yic2(lalias(i+8))=yic2(lalias(i+8))/dble(ict1)
                  zic2(lalias(i+8))=zic2(lalias(i+8))/dble(ict1)
                  if(iradavg.eq.1) then
                     rad1=rad1/dble(ict1)
                     rad2=sqrt(xic2(lalias(i+8))**2+
     *                         yic2(lalias(i+8))**2+
     *                         zic2(lalias(i+8))**2)
                     xic2(lalias(i+8))=xic2(lalias(i+8))*rad1/rad2
                     yic2(lalias(i+8))=yic2(lalias(i+8))*rad1/rad2
                     zic2(lalias(i+8))=zic2(lalias(i+8))*rad1/rad2
                  endif
                  uic(lalias(i+8))= uic(lalias(i+8))/dble(ict1)
                  vic(lalias(i+8))= vic(lalias(i+8))/dble(ict1)
                  wic(lalias(i+8))= wic(lalias(i+8))/dble(ict1)
                  pic(lalias(i+8))= pic(lalias(i+8))/dble(ict1)
                  ric(lalias(i+8))= ric(lalias(i+8))/dble(ict1)
               endif
            enddo
         endif
      enddo
*GEO
      if (iwerr.gt.0) then
         write(logmess,'(i10,a)') iwerr,
     *        ' Total errors assigning node color.'
         call writloga('default',0,logmess,0,ierwrt)
      endif
 
      length=ntet
      call mmgetblk("itdel",isubname,ipitdel,length,2,icscode)
      ntdel=0
      ntneg=0
      do i=1,ntet
         itdel(i)=0
      enddo
      do it=1,numhex
         do i=1,8
            ihexnn(i,it)=ialiasp(ihexnn(i,it))
         enddo
      enddo
      do i=1,ntet
         do j=1,4
            itet(j,i)=ialiasp(itet(j,i))
         enddo
         i1=itet(1,i)
         i2=itet(2,i)
         i3=itet(3,i)
         i4=itet(4,i)
         x1=xic2(i1)
         y1=yic2(i1)
         z1=zic2(i1)
         x2=xic2(i2)-x1
         y2=yic2(i2)-y1
         z2=zic2(i2)-z1
         x3=xic2(i3)-x1
         y3=yic2(i3)-y1
         z3=zic2(i3)-z1
         x4=xic2(i4)-x1
         y4=yic2(i4)-y1
         z4=zic2(i4)-z1
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         voltet=-(x3*dx+y3*dy+z3*dz) / 6.0
         if(voltet.lt.-xfacvol) then
            ntneg=ntneg+1
         endif
         if(voltet.le.xfacvol) then
            ntdel=ntdel+1
            itdel(i)=1
         else
            itdel(i)=0
            voltot=voltot+voltet
         endif
      enddo
      if(ntdel.gt.0) then
         write(logmess,9110) ntet,ntdel
         call writloga('default',0,logmess,0,ierwrt)
 9110    format("Deleting tets: ",2i10)
      endif
      if(ntneg.gt.0) then
         write(logmess,9120) ntet,ntneg
         call writloga('default',0,logmess,0,ierwrt)
 9120    format("Negative volume tets: ",2i10)
      endif
      ntet1=ntet
      do it=1,ntet1
         if(itdel(it).eq.1) then
            nmove=ntet
 100        continue
            if(itdel(nmove).ne.0) then
               nmove=nmove-1
               goto 100
            endif
            do i=1,4
               jt=jtet(i,it)
               jf=itetnn2(i,it)
               if(jt.gt.0.and.jt.le.ntet1) then
                  jtet(jf,jt)=-1
               endif
               if(nmove.gt.it) then
                  jt=jtet(i,nmove)
                  jf=itetnn2(i,nmove)
                  if(jt.gt.0.and.jt.le.ntet1) then
                     jtet(jf,jt)=it
                     itetnn2(jf,jt)=i
                  endif
                  itetclr(it)=itetclr(nmove)
                  itettyp(it)=itettyp(nmove)
                  itetoff(it)=nen*(it-1)
                  jtetoff(it)=nef*(it-1)
                  itet(i,it)=itet(i,nmove)
                  jtet(i,it)=jtet(i,nmove)
                  itetnn2(i,it)=itetnn2(i,nmove)
                  jtet(i,nmove)=-1
               endif
            enddo
            if(nmove.gt.it) then
               itdel(it)=0
               itdel(nmove)=2
            endif
            ntet=ntet-1
         endif
      enddo
      do it=1,ntet
         do i=1,4
            jt=jtet(i,it)
            jf=itetnn2(i,it)
            if(jt.le.0) then
            elseif(jt.le.ntet) then
               kt=jtet(jf,jt)
               kf=itetnn2(jf,jt)
               if(kt.ne.it.and.kf.ne.i) then
                  write(logmess,9130) it,i,jt,jf,kt,kf
                  call writloga('default',0,logmess,0,ierwrt)
 9130             format("Hex conn error: ",6i10)
               endif
            else
               write(logmess,9130) it,i,jt,jf,0,0
               call writloga('default',0,logmess,0,ierwrt)
            endif
         enddo
      enddo
      call mmrelblk("itdel",isubname,ipitdel,icscode)
      numtet=ntet
      if(icheckpt.eq.1) then
 300     continue
         do i=1,npoints
            ialiasp(i)=0
         enddo
         do it=1,numtet
            do i=1,4
               ialiasp(itet(i,it))=itet(i,it)
            enddo
         enddo
	 idup=0
         do i1=1,npoints
            if(ialiasp(i1).eq.i1) then
               do i2=i1+1,npoints
                  if(ialiasp(i2).gt.0) then
                     dist=(xic2(i1)-xic2(i2))**2 +
     *                    (yic2(i1)-yic2(i2))**2 +
     *                    (zic2(i1)-zic2(i2))**2
                     if(dist.le.xfacdist) then
	                idup=idup+1
C*****                  print *,"Duplicate point: ",i1,i2
	                ialiasp(i2)=i1
                     endif
                   endif
               enddo
            endif
         enddo
         if(idup.le.0) then
            write(logmess,9140)
            call writloga('default',0,logmess,0,ierwrt)
 9140       format("No duplicate points")
	 else
            write(logmess,9150) idup
            call writloga('default',0,logmess,0,ierwrt)
 9150       format("Number of duplicate points: ",i10)
            do it=1,numtet
               do i=1,4
                  itet(i,it)=ialiasp(itet(i,it))
               enddo
            enddo
            do it=1,numhex
               do i=1,8
                  ihexnn(i,it)=ialiasp(ihexnn(i,it))
               enddo
            enddo
            goto 300
         endif
      endif
      call geniee(itet,jtet,itetnn2,4,4,numtet,npoints,
     *     3,npoints,numtet)
      nbndy=0
      do j=1,numtet
         do i=1,4
            jt=jtet(i,j)
            jf=itetnn2(i,j)
            if(jt.le.0.or.jt.gt.numtet) then
               nbndy=nbndy+1
               jtet(i,j)=mbndry
C*****         print *,"Boundary: ",j,i,(itet(itetface1(k,i),j),k=1,3)
               do k=1,3
                  itp1(itet(itetface1(k,i),j))=11
               enddo
            else
               jtet(i,j)=4*(jt-1)+jf
C*****               if(intflag.eq.0) then
C*****                  jtet(i,j)=4*(jt-1)+jf
C*****                  jtet(jf,jt)=4*(j-1)+i
C*****               elseif(intflag.eq.1) then
C*****                  imti1=itetclr(itet(i,j))
C*****                  imti2=itetclr(itet(jf,jt))
C*****                  if(imti1.eq.imti2) then
C*****                     jtet(i,j)=4*(jt-1)+jf
C*****                     jtet(jf,jt)=4*(j-1)+i
C*****                  else
C*****                     jtet(i,j)=mbndry
C*****                     jtet(jf,jt)=mbndry
C*****                  endif
C*****               endif
            endif
         enddo
      enddo
C
C
      if(iremove.eq.1) then
         do i=1,npoints
            ialiasp(i)=0
         enddo
         do it=1,numtet
            do i=1,4
               ialiasp(itet(i,it))=itet(i,it)
            enddo
         enddo
         ict=0
         do i=1,npoints
            if(ialiasp(i).ne.0) then
               ict=ict+1
               ialiasp(i)=ict
            endif
         enddo
         if(ict.lt.npoints) then
            do i=1,npoints
               i1=ialiasp(i)
               if(i1.gt.0) then
                  imt1(i1)=imt1(i)
                  itp1(i1)=itp1(i)
                  icr1(i1)=icr1(i)
                  ign1(i1)=ign1(i)
                  xic2(i1)=xic2(i)
                  yic2(i1)=yic2(i)
                  zic2(i1)=zic2(i)
                  uic(i1)=uic(i)
                  vic(i1)=vic(i)
                  wic(i1)=wic(i)
                  pic(i1)=pic(i)
                  ric(i1)=ric(i)
               endif
            enddo
            do it=1,numtet
               do i=1,4
                  itet(i,it)=ialiasp(itet(i,it))
               enddo
            enddo
            do i=1,npstart
               i1=ialiasp(i)
               if(i1.gt.0) then
                  imt1hex(i1)=imt1hex(i)
                  itp1hex(i1)=itp1hex(i)
                  icr1hex(i1)=icr1hex(i)
                  xhex(i1)=xhex(i)
                  yhex(i1)=yhex(i)
                  zhex(i1)=zhex(i)
               endif
            enddo
            do it=1,numhex
               do i=1,8
                  ihexnn(i,it)=ialiasp(ihexnn(i,it))
               enddo
            enddo
         endif
         npoints=ict
         do i=1,npoints
            ialiasp(i)=i
         enddo
      endif
C
      call mmrelblk("ktet",isubname,ipktet,icscode)
      call mmrelblk("ihexnn1",isubname,ipihexnn1,icscode)
      call mmrelblk("ihexnn2",isubname,ipihexnn2,icscode)
      call mmrelblk("ialiasp",isubname,ipialiasp,icscode)
C
      if(nnflag.eq.1) then
         length=npoints
         call mmgetblk("nncnt",isubname,ipnncnt,length,2,icscode)
         length=20*npoints
         call mmgetblk("nnlst",isubname,ipnnlst,length,2,icscode)
         do i=1,npoints
            nncnt(i)=1
            nnlst(1,i)=i
         enddo
         do it=1,numtet
            do i=1,4
               i1=itet(i,it)
               do j=1,3
                  i2=itet(itetface1(j,i),it)
                  iflag=0
                  n=nncnt(i1)
                  do k=1,n
                     if(nnlst(k,i1).eq.i2) then
                        iflag=i2
                     endif
                  enddo
                  if(iflag.eq.0) then
                     nncnt(i1)=nncnt(i1)+1
                     nnlst(nncnt(i1),i1)=i2
                  endif
               enddo
            enddo
         enddo
         do i=npstart+1,npoints
            if(nncnt(i).gt.1) then
               icount=0
               xavg=0.0
               yavg=0.0
               zavg=0.0
               uavg=0.0
               vavg=0.0
               wavg=0.0
               pavg=0.0
               ravg=0.0
               rad1=0.0
               do j=1,nncnt(i)
                  i1=nnlst(j,i)
                  if(i1.le.npstart) then
                     icount=icount+1
                     xavg=xavg+xic(i1)
                     yavg=yavg+yic(i1)
                     zavg=zavg+zic(i1)
                     uavg=uavg+uic(i1)
                     vavg=vavg+vic(i1)
                     wavg=wavg+wic(i1)
                     pavg=pavg+pic(i1)
                     ravg=ravg+ric(i1)
                     rad1=rad1+sqrt(xic(i1)*xic(i1)+
     *                              yic(i1)*yic(i1)+
     *                              zic(i1)*zic(i1) )
                  endif
               enddo
               xic2(i)=xic2(i)/dble(icount)
               yic2(i)=yic2(i)/dble(icount)
               zic2(i)=zic2(i)/dble(icount)
               uic(i)=uic(i)/dble(icount)
               vic(i)=vic(i)/dble(icount)
               wic(i)=wic(i)/dble(icount)
               pic(i)=pic(i)/dble(icount)
               ric(i)=ric(i)/dble(icount)
               if(iradavg.eq.1) then
                  rad1=rad1/dble(icount)
                  rad2=sqrt(xic2(i)**2+
     *                      yic2(i)**2+
     *                      zic2(i)**2)
                  xic2(i)=xic2(i)*rad1/rad2
                  yic2(i)=yic2(i)*rad1/rad2
                  zic2(i)=zic2(i)*rad1/rad2
               endif
            endif
         enddo
         call mmrelblk("nncnt",isubname,ipnncnt,icscode)
         call mmrelblk("nnlst",isubname,ipnnlst,icscode)
      endif
      do i=1,npoints
         xic(i)=xic2(i)
         yic(i)=yic2(i)
         zic(i)=zic2(i)
      enddo
      call mmrelblk("xic2",isubname,ipxic2,icscode)
      call mmrelblk("yic2",isubname,ipyic2,icscode)
      call mmrelblk("zic2",isubname,ipzic2,icscode)
C
C     .................................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
      do i=1,npoints
         itp1(i)=0
      enddo
      cmotype='tet'
      nen=4
      nef=4
      do it=1,numtet
         index=nef*(it-1)
         do i=1,nef
            index=nef*(it-1)+i
            if(jtet1(index).le.0.or.jtet1(index).ge.mbndry) then
               jndex=nef*(it-1)
               if(cmotype(1:3).eq.'tet') then
                  do j=1,3
                     j1=itet1(jndex+itetface1(j,i))
                     itp1(j1)=ifitprfl
                  enddo
               elseif(cmotype(1:3).eq.'hex') then
                  do j=1,4
                     j1=itet1(jndex+ihexface1(j,i))
                     itp1(j1)=ifitprfl
                  enddo
               endif
            endif
         enddo
      enddo
      do it=1,numtet
         index=nef*(it-1)
         do i=1,nef
            index=nef*(it-1)+i
            if(jtet1(index).gt.0.and.jtet1(index).lt.mbndry) then
               jt=1+(jtet1(index)-1)/nef
               jf=jtet1(index)-nef*(jt-1)
               if(itetclr(it).ne.itetclr(jt)) then
                  jndex=nef*(it-1)
                  if(cmotype(1:3).eq.'tet') then
                     do j=1,3
                        j1=itet1(jndex+itetface1(j,i))
                        if(itp1(j1).eq.ifitpinb) then
                        elseif(itp1(j1).eq.ifitpfre) then
                        elseif(itp1(j1).eq.ifitprfl) then
                           itp1(j1)=ifitpinb
                        else
                           itp1(j1)=ifitpini
                        endif
                     enddo
                  elseif(cmotype(1:3).eq.'hex') then
                     do j=1,4
                        j1=itet1(jndex+ihexface1(j,i))
                        if(itp1(j1).eq.ifitpinb) then
                        elseif(itp1(j1).eq.ifitpfre) then
                        elseif(itp1(j1).eq.ifitprfl) then
                           itp1(j1)=ifitpinb
                        else
                           itp1(j1)=ifitpini
                        endif
                     enddo
                  endif
                  jtet1(index)=mbndry+nef*(jt-1)+jf
                  jtet1(nef*(jt-1)+jf)=mbndry+index
               endif
            endif
         enddo
      enddo
C
C     ***************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL REAL NODES.
C          IREAL1 = 1  -> Real Node.
C          IREAL1 = 0  -> Not a real node.
      length=npoints
      call mmgetblk('ireal1',isubname,ipireal1,length,2,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call cmo_get_info('itp1',cmotet,ipitp1,lenitp1,icmotype,ierror)
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrdum)
         if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
C     ***************************************************************
C
      do i=1,npoints
         ialiasp(i)=0
      enddo
      do it=1,numtet
         do i=1,nelmnen(itettyp(it))
            ialiasp(itet(i,it))=itet(i,it)
         enddo
      enddo
      do i1=1,npoints
         if(ireal1(i1).eq.1.and.ialiasp(i1).eq.0) then
            itp1(i1)=ifitpdud
         endif
      enddo
C
 9998 continue
      goto 9999
 9999 continue
      call cmo_get_name(cmotet,ierror)
      call cmo_set_info('nnodes',cmotet,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmotet,numtet,1,1,ierror)
      call mmrelprt(isubname,ierror)
      return
      end
