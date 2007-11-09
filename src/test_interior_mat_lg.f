      subroutine test_interior_mat_lg
c
C#######################################################################
C
C      PURPOSE -
C
C      THIS ROUTINE tests all materials and print statistics on
c      volumes by materials and if material has a boundary node
c
C
C        $Log: test_interior_mat_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C
C#######################################################################
C
      implicit none
C
      include 'local_element.h'
      include 'consts.h'
C
      character*132 logmess
C
      pointer ( ipitp1 , itp1 )
      pointer ( ipitetclr , itetclr )
      pointer ( ipitetoff , itetoff )
      pointer ( ipjtetoff , jtetoff )
      pointer ( ipitettyp , itettyp )
      pointer ( ipitet1 , itet1 )
      pointer ( ipjtet1 , jtet1 )
      pointer ( ipxic,xic)
      pointer ( ipyic,yic)
      pointer ( ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer ( ipclr, clr)
      integer clr(*)
      pointer ( iptetnum, tetnum)
      integer tetnum(*)
      pointer ( ipnf, nf)
      integer nf(*)
      pointer ( ipareaf, areaf)
      real*8 areaf(*)
      integer itetclr(*),itettyp(*),itetoff(*),itet1(*),jtet1(*),
     *   jtetoff(*)
      integer  itp1(*)
C
      pointer ( ipibnd1, ibnd1)
      integer ibnd1(*)
C
C#######################################################################
C
      integer nelements,nnodes,i1,i2,i3,i4,length,icscode,i,kt,ityp,
     *   it,ne,oldclr,ilenc,itypc,neiclr,k,mbndry,j,jt,nfacets,
     *   maxfacets,n(6)
      character*32 isubname, cmo,status
      real*8 vol,vol1,areatri,a(6)
      include 'statementfunctions.h'
c
C     ******************************************************************
      isubname='test_interior_mat'
C
C     GET mesh object info
C
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_intinfo('nnodes',cmo,nnodes,ilenc,itypc,icscode)
      call cmo_get_intinfo('mbndry',cmo,mbndry,ilenc,itypc,icscode)
      call cmo_get_intinfo('nelements',cmo,nelements,ilenc,itypc,
     *     icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilenc,itypc,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilenc,itypc,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilenc,itypc,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilenc,itypc,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilenc,itypc,icscode)
      call cmo_get_info('itet',cmo,ipitet1,ilenc,itypc,icscode)
      call cmo_get_info('jtet',cmo,ipjtet1,ilenc,itypc,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilenc,itypc,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilenc,itypc,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilenc,itypc,icscode)
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES  ALL BOUNDARY NODES.
C
      length=nnodes
      call mmgetblk('ibnd1',isubname,ipibnd1,length,1,icscode)
      call unpacktp('boundary','set',length,ipitp1,ipibnd1,icscode)
C
C     ******************************************************************
C     copy itetclr to another array and sort it.
c
      length=nelements
      maxfacets=200
      call mmgetblk('nf',isubname,ipnf,maxfacets,1,icscode)
      call mmgetblk('areaf',isubname,ipareaf,maxfacets,2,icscode)
      call mmgetblk('clr',isubname,ipclr,length,1,icscode)
      call mmgetblk('tetnum',isubname,iptetnum,length,1,icscode)
      do i=1,nelements
         clr(i)=itetclr(i)
         tetnum(i)=i
      enddo
      vol=1.0
      call hpsortip(nelements,clr,vol,tetnum)
      vol=0.0
      oldclr=clr(tetnum(1))
      ne=0
      status='not bndry'
      nfacets=0
      do it=1,nelements
         if (itettyp(tetnum(it)).ne.ifelmtet) go to 9999
         i1 = itet1(itetoff(tetnum(it))+1)
         i2 = itet1(itetoff(tetnum(it))+2)
         i3 = itet1(itetoff(tetnum(it))+3)
         i4 = itet1(itetoff(tetnum(it))+4)
         call volume_tet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),
     *     zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4),
     *     vol1)
         if (clr(tetnum(it)).eq.oldclr) then
            vol=vol+vol1
            ne=ne+1
c
c  look for boundary faces and count number of materials
c
            do j=1,4
               jt=jtet1(jtetoff(tetnum(it))+j)
               if(jt.eq.mbndry) go to 50
               if(jt.gt.mbndry) then
                  kt=tetnum(it)
                  ityp=itettyp(kt)
                  i1 = itet1(itetoff(kt)+ielmface1(1,j,ityp))
                  i2 = itet1(itetoff(kt)+ielmface1(2,j,ityp))
                  i3 = itet1(itetoff(kt)+ielmface1(3,j,ityp))
                  areatri=darea(xic(i1),yic(i1),zic(i1),
     *                          xic(i2),yic(i2),zic(i2),
     *                          xic(i3),yic(i3),zic(i3))
                  jt=dble(jt-mbndry)*.25+.9
                  neiclr=itetclr(jt)
                  do k=1,nfacets
                     if(neiclr.eq.nf(k)) then
                        areaf(k)=areaf(k)+areatri
                        go to 50
                     endif
                  enddo
                  nfacets=nfacets+1
                  nf(nfacets)=neiclr
                  areaf(nfacets)=areatri
                endif
 50             continue
            enddo
         else
c
c  new grain
c
            write (logmess,100)status, oldclr,ne,vol,nfacets
 100        format(a12,' grain number ',i10,' # elements ',i10,
     *          ' volume ',e12.4,' # facets',i10)
            call writloga('default',0,logmess,0,icscode)
            i1=0
            do kt=1,nfacets
               i1=i1+1
               n(i1)=kt
               a(i1)=areaf(kt)
               if(i1.eq.6.or.kt.eq.nfacets) then
                   write (logmess,101) (n(j),a(j),j=1,i1)
 101               format(6(i3,1x,1pe12.4))
                   call writloga('default',0,logmess,0,icscode)
                   i1=0
               endif
            enddo
            ne=1
            vol=vol1
            oldclr=clr(tetnum(it))
            status='not bndry'
            do j=1,nfacets
               nf(j)=0
               areaf(j)=0.
            enddo
            nfacets=0
c
c  look for boundary faces and count number of materials
c
            do j=1,4
               jt=jtet1(jtetoff(tetnum(it))+j)
               if(jt.eq.mbndry) go to 150
               if(jt.gt.mbndry) then
                  kt=tetnum(it)
                  ityp=itettyp(kt)
                  i1 = itet1(itetoff(kt)+ielmface1(1,j,ityp))
                  i2 = itet1(itetoff(kt)+ielmface1(2,j,ityp))
                  i3 = itet1(itetoff(kt)+ielmface1(3,j,ityp))
                  areatri=darea(xic(i1),yic(i1),zic(i1),
     *                          xic(i2),yic(i2),zic(i2),
     *                          xic(i3),yic(i3),zic(i3))
                  jt=dble(jt-mbndry)*.25+.9
                  neiclr=itetclr(jt)
                  do k=1,nfacets
                     if(neiclr.eq.nf(k)) then
                        areaf(k)=areaf(k)+areatri
                        go to 50
                     endif
                  enddo
                  nfacets=nfacets+1
                  nf(nfacets)=neiclr
                  areaf(nfacets)=areatri
                endif
 150             continue
            enddo
         endif
         if(ibnd1(i1)+ibnd1(i2)+ibnd1(i3)+ibnd1(i4).gt.0) then
            status='boundary'
         endif
      enddo
      write (logmess,100)status, oldclr,ne,vol,nfacets
      call writloga('default',0,logmess,0,icscode)
            i1=0
            do kt=1,nfacets
               i1=i1+1
               n(i1)=kt
               a(i1)=areaf(kt)
               if(i1.eq.6.or.kt.eq.nfacets) then
                   write (logmess,101) (n(j),a(j),j=1,i1)
                   call writloga('default',0,logmess,0,icscode)
                   i1=0
               endif
            enddo
      call mmrelprt(isubname,icscode)
9999  return
      end
 
 
 
