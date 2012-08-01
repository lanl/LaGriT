*dk,voronoi_stor
      subroutine voronoi_stor(cmo,coption1,ifile)
C
C CHANGE HISTORY
C $Log: voronoi_stor.f,v $
C Revision 2.00  2007/11/09 20:04:06  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   29 Sep 2004 17:18:10   dcg
CPVCS    remove statements that set the value of pie - use the value in chydro set by
CPVCS    initlagrit.
CPVCS
CPVCS       Rev 1.7   21 Mar 2002 10:12:38   dcg
CPVCS    clean up
CPVCS
CPVCS       Rev 1.6   21 Mar 2002 10:09:04   dcg
CPVCS    add subroutines used only by this routine that were in temphet
CPVCS
CPVCS       Rev 1.5   Fri Jun 25 16:55:24 1999   tam
CPVCS    changed gmv format for general from i7 to 1xi10
CPVCS    added new fehm ascii stor header, max_num_conn is 0
CPVCS    still need to add binary version of the stor file
CPVCS
CPVCS       Rev 1.4   Wed Jul 09 07:30:16 1997   het
CPVCS    Correct a GMV output error by adding alias1 and alias2 arrays.
CPVCS
CPVCS       Rev 1.3   Thu Jul 03 15:23:42 1997   gable
CPVCS    Changed time stamp to use fdate()
CPVCS
CPVCS       Rev 1.2   Sat Jun 28 18:38:06 1997   gable
CPVCS    Include time stamp in stor file header.
CPVCS
CPVCS       Rev 1.1   Thu Jun 19 17:25:34 1997   gable
CPVCS    Modified to divide area coeffients by distance. Before
CPVCS    .stor file would output Aij, now it outputs Aij/xij.
CPVCS    Other minor changes and check added for debugging.
CPVCS
CPVCS       Rev 1.0   Wed May 14 10:02:46 1997   dcg
CPVCS    Initial revision.
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
C
C ######################################################################
C
C
      include "chydro.h"
      include "local_element.h"
C
      character*24 string, fdate
C
      character*(*) cmo, coption1, ifile
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer imt1(1000000), itp1(1000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(4*1000000), jtet1(4*1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipireal1, ireal1)
      integer ireal1(1000000)
C
      pointer (ipkbb, kbb)
      pointer (ipkbbtp, kbbtp)
      pointer (ipkbbpp, kbbpp)
      integer kbb(1000000), kbbtp(1000000), kbbpp(1000000)
      pointer (ipxna, xna)
      real*8 xna(3,1000000)
      pointer (ipxna1, xna1)
      real*8 xna1(3,1000000)
C
      pointer (ipnna, nna(1000000))
      pointer (ipnnatp, nnatp(1000000))
      pointer (ipnn , nn(1000000))
      pointer (ipnnbp,nnbp(1000000))
      pointer (ipnnb, nnb(1000000))
      pointer (ipnnbv, nnbv(1000000))
C
      pointer (ipnna1, nna1(1000000))
      pointer (ipnnatp1, nnatp1(1000000))
      pointer (ipnn1 , nn1(1000000))
      pointer (ipnnbp1,nnbp1(1000000))
      pointer (ipnnb1, nnb1(1000000))
C
      pointer (ipxvor1, xvor1)
      pointer (ipyvor1, yvor1)
      pointer (ipzvor1, zvor1)
      real*8 xvor1(10000), yvor1(10000), zvor1(10000)
C
      pointer (ipxvorg, xvorg)
      pointer (ipyvorg, yvorg)
      pointer (ipzvorg, zvorg)
      real*8 xvorg(10000), yvorg(10000), zvorg(10000)
C
C
      pointer (ipialias1, ialias1)
      pointer (ipialias2, ialias2)
      integer ialias1(1000000), ialias2(1000000)
C
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      pointer (ipiedge1, iedge1)
      pointer (ipiedge2, iedge2)
      integer nedge_bin(1000000), nedge_off(1000000)
      integer iedge1(1000000), iedge2(1000000)
C
      pointer (ipnnode_bin, nnode_bin)
      pointer (ipnnode_off, nnode_off)
      pointer (ipinode1, inode1)
      integer nnode_bin(1000000), nnode_off(1000000)
      integer inode1(1000000)
C
      pointer (ipnbndcnt, nbndcnt)
      pointer (ipnbndoff, nbndoff)
      pointer (ipnbndlst, nbndlst)
      integer nbndcnt(1000000), nbndoff(1000000), nbndlst(1000000)
C
      pointer (ipvolic, volic)
      real*8 volic(1000000)
      pointer (ipvolic2, volic2)
      real*8 volic2(1000000)
C
      pointer (ipirowcnt, irowcnt)
      pointer (ipirowoff, irowoff)
      pointer (ipirowdag, irowdag)
      pointer (ipirowmat, irowmat)
      pointer (ipicolmat, icolmat)
      integer irowcnt(1000000), irowoff(1000000), irowdag(1000000)
      integer irowmat(1000000), icolmat(1000000)
      pointer (ipamat, amat)
      pointer (ipxamat, xamat)
      pointer (ipyamat, yamat)
      pointer (ipzamat, zamat)
      real*8 amat(1000000),
     *       xamat(1000000), yamat(1000000), zamat(1000000)
C
      pointer (ipxtemp, xtemp)
      real*8 xtemp(1000000)
C
      pointer (ipitemp, itemp)
      integer itemp(1000000)
C
      pointer (ipnnvornnj, nnvornnj)
      pointer (ipnnvoroff, nnvoroff)
      pointer (ipnnvor, nnvor)
      integer nnvornnj(1000000), nnvoroff(1000000), nnvor(1000000)
C
      pointer (ipicolsort1, icolsort1)
      pointer (ipicolsort2, icolsort2)
      pointer (ipxcolsort1, xcolsort1)
      integer icolsort1(1000000), icolsort2(1000000)
      real*8 xcolsort1(1000000)
C
      pointer (ipitp10, itp10)
      integer itp10(1000000)
C
      real*8 xtetvol(4), xedgevol(6)
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
      character*32 isubname,  ifilename
C
C
C#######################################################################
C
      integer isearch_all
      data isearch_all / 0 /
      integer nnitermax
      data nnitermax / 1 /
C
C#######################################################################
C
      isubname='voronoi_volume'
C
      ibyte=4
C
      nniter=0
C
C*****nb=0
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
C
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
      call cmo_get_info('nodes_per_element',cmo,nen1,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef1,ilen,itype,icscode)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
C
      length=1000
      call mmgetblk('xvor1',isubname,ipxvor1,length,2,icscode)
      call mmgetblk('yvor1',isubname,ipyvor1,length,2,icscode)
      call mmgetblk('zvor1',isubname,ipzvor1,length,2,icscode)
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
C
      nkinmax=5000
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     UPDATE THE POINTER TO THE FIRST LOCAL NEIGHBOR BLOCKS
C
      nnjmaxo=nkinmax
      nnkmaxo=100*nnjmaxo
      nnj=nnjmaxo
      nnk=nnkmaxo
      call mmgetblk('nna',isubname,ipnna,nnj,1,icscode)
      call mmgetblk('nnatp',isubname,ipnnatp,nnj,1,icscode)
      call mmgetblk('nn',isubname,ipnn,nnj,1,icscode)
      call mmgetblk('nnbp',isubname,ipnnbp,nnj,1,icscode)
      call mmgetblk('nnb',isubname,ipnnb,nnk,1,icscode)
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     UPDATE THE POINTER TO THE FIRST LOCAL NEIGHBOR BLOCKS
C
      nnj1maxo=nkinmax
      nnk1maxo=100*nnj1maxo
      nnj1=nnj1maxo
      nnk1=nnk1maxo
      call mmgetblk('nna1',isubname,ipnna1,nnj1,1,icscode)
      call mmgetblk('nnatp1',isubname,ipnnatp1,nnj1,1,icscode)
      call mmgetblk('nn1',isubname,ipnn1,nnj1,1,icscode)
      call mmgetblk('nnbp1',isubname,ipnnbp1,nnj1,1,icscode)
      call mmgetblk('nnb1',isubname,ipnnb1,nnk1,1,icscode)
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     ALLOCATE MEMORY FOR THE VORONOI SEARCHING ROUTINE. THE MAXIMUM
C        NUMBER OF POSSIBLE NEIGHBORS IS THE TOTAL NUMBER OF POINTS
C        PLUS THE TOTAL NUMBER OF EXTERNAL BOUNDARIES.
C
      if(nb.gt.0) then
         length=nnodes+nb
      else
         length=2*nnodes
      endif
      call mmgetblk("kbb",isubname,ipkbb,length,1,icscode)
      call mmgetblk("kbbtp",isubname,ipkbbtp,length,1,icscode)
      call mmgetblk("kbbpp",isubname,ipkbbpp,length,1,icscode)
      call mmgetblk("xna",isubname,ipxna,3*length,2,icscode)
      call mmgetblk("xna1",isubname,ipxna1,3*length,2,icscode)
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BIN THE DELAUNAY CONNECTIONS TO FORM THE LIST OF "POSSIBLE"
C        NEAREST NEIGHBORS FOR THE VORONOI SEARCHING ALGORITHM.
C
      length=2*nnodes
      call mmgetblk('nedge_bin',isubname,
     *              ipnedge_bin,length,1,icscode)
      call mmgetblk('nedge_off',isubname,
     *              ipnedge_off,length,1,icscode)
      do i=1,2*nnodes
         nedge_bin(i)=0
         nedge_off(i)=0
      enddo
      do it=1,nelements
         do ie=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
            isum12=i1+i2
            nedge_bin(isum12)=nedge_bin(isum12)+1
         enddo
      enddo
      isum=0
      do i=1,2*nnodes
         if(nedge_bin(i).gt.0) then
            nedge_off(i)=isum
            isum=isum+nedge_bin(i)
         endif
         nedge_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('iedge1',isubname,
     *              ipiedge1,length,1,icscode)
      call mmgetblk('iedge2',isubname,
     *              ipiedge2,length,1,icscode)
      do i=1,length
         iedge1(i)=0
         iedge2(i)=0
      enddo
      do it=1,nelements
         do ie=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
            isum12=i1+i2
            if(nedge_bin(isum12).gt.0) then
               do j=1,nedge_bin(isum12)
                  j1=iedge1(nedge_off(isum12)+j)
                  j2=iedge2(nedge_off(isum12)+j)
                  if((i1.eq.j1.and.i2.eq.j2) .or.
     *               (i1.eq.j2.and.i2.eq.j1)) goto 300
               enddo
            endif
            nedge_bin(isum12)=nedge_bin(isum12)+1
            iedge1(nedge_off(isum12)+nedge_bin(isum12))=i1
            iedge2(nedge_off(isum12)+nedge_bin(isum12))=i2
 300     continue
         enddo
      enddo
C
      length=nnodes
      call mmgetblk('nnode_bin',isubname,
     *              ipnnode_bin,length,1,icscode)
      call mmgetblk('nnode_off',isubname,
     *              ipnnode_off,length,1,icscode)
      do i1=1,nnodes
         nnode_bin(i1)=0
         nnode_off(i1)=0
      enddo
      do i=1,2*nnodes
         if(nedge_bin(i).gt.0) then
            do j=1,nedge_bin(i)
               i1=iedge1(nedge_off(i)+j)
               i2=iedge2(nedge_off(i)+j)
               nnode_bin(i1)=nnode_bin(i1)+1
               nnode_bin(i2)=nnode_bin(i2)+1
            enddo
         endif
      enddo
      isum=0
      do i=1,nnodes
         if(nnode_bin(i).gt.0) then
            nnode_off(i)=isum
            isum=isum+nnode_bin(i)
         endif
         nnode_bin(i)=0
      enddo
      length=isum+1
      call mmgetblk('inode1',isubname,
     *              ipinode1,length,1,icscode)
      do i=1,length
         inode1(i)=0
      enddo
      do i=1,2*nnodes
         if(nedge_bin(i).gt.0) then
            do j=1,nedge_bin(i)
               i1=iedge1(nedge_off(i)+j)
               i2=iedge2(nedge_off(i)+j)
               nnode_bin(i1)=nnode_bin(i1)+1
               nnode_bin(i2)=nnode_bin(i2)+1
               inode1(nnode_off(i1)+nnode_bin(i1))=i2
               inode1(nnode_off(i2)+nnode_bin(i2))=i1
            enddo
         endif
      enddo
      call mmrelblk('nedge_bin',isubname,ipnedge_bin,icscode)
      call mmrelblk('nedge_off',isubname,ipnedge_off,icscode)
      call mmrelblk('iedge1',isubname,ipiedge1,icscode)
      call mmrelblk('iedge2',isubname,ipiedge2,icscode)
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     ALLOCATE AND ZERO THE VOLUME ACCUMULATION VECTORS.
C
      length=nnodes
      call mmgetblk('volic',isubname,ipvolic,length,2,icscode)
      call mmgetblk('volic2',isubname,ipvolic2,length,2,icscode)
      do i1=1,nnodes
         volic(i1)=0.0
         volic2(i1)=0.0
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     SAVE THE ORIGINAL NUMBER OF NODES.
C
      nnodes_save=nnodes
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     DETERMINE THE POINT SET TO SEARCH.
C
      length=nnodes
      call mmgetblk('ireal1',isubname,ipireal1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
      length=nnodes
      call mmgetblk('itp10',isubname,ipitp10,length,1,icscode)
      do i1=1,nnodes
         itp10(i1)=0
      enddo
      len1=icharlnf(coption1)
      if(coption1(1:len1).eq.'all') then
         do i1=1,nnodes
            if(ireal1(i1).eq.1) then
               itp10(i1)=i1
            else
               itp10(i1)=0
            endif
         enddo
      elseif(coption1(1:len1).eq.'rfl') then
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).eq.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     itp10(i1)=jtetoff(it)+i
                  enddo
               endif
            enddo
         enddo
      elseif(coption1(1:len1).eq.'fre') then
         do it=1,nelements
            do i=1,nelmnee(itettyp(it))
               i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
               i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
               if(itp1(i1).eq.ifitpfre.and.itp1(i2).eq.ifitpint) then
                  itp10(i2)=it
               endif
               if(itp1(i2).eq.ifitpfre.and.itp1(i1).eq.ifitpint) then
                  itp10(i1)=it
               endif
            enddo
         enddo
      endif
      nitp10=0
      do i1=1,nnodes
         if(itp10(i1).gt.0) then
            nitp10=nitp10+1
            itp10(nitp10)=i1
         endif
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BAIL OUT IF NO POINTS ARE IN THE SET OF POINTS TO BE SEARCHED.
C
      if(nitp10.eq.0) goto 9998
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     EXPAND THE COORDINATES OF THE EXTERNAL BOUNDARIES ABOUT THE
C        AVERAGE POINT BY AN EPSILON.
C
      if(nb.eq.0) then
         nbsave=0
         length=nnodes
         call mmgetblk('nbndcnt',isubname,ipnbndcnt,length,1,icscode)
         call mmgetblk('nbndoff',isubname,ipnbndoff,length,1,icscode)
         do i1=1,nnodes
            nbndcnt(i1)=0
            nbndoff(i1)=0
         enddo
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).ge.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     nbndcnt(i1)=nbndcnt(i1)+1
                  enddo
               endif
            enddo
         enddo
         icount=0
         do i1=1,nnodes
            nbndoff(i1)=icount
            icount=icount+nbndcnt(i1)
            nbndcnt(i1)=0
         enddo
         length=icount
         call mmgetblk('nbndlst',isubname,ipnbndlst,length,1,icscode)
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).ge.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     nbndcnt(i1)=nbndcnt(i1)+1
                     nbndlst(nbndoff(i1)+nbndcnt(i1))=nef1*(it-1)+i
                  enddo
               endif
            enddo
         enddo
         xoff=0.0
         yoff=0.0
         zoff=0.0
         xfac=1.0
         yfac=1.0
         zfac=1.0
      elseif(nb.gt.0) then
         nbsave=nb
         xminb=1.0e+30
         yminb=1.0e+30
         zminb=1.0e+30
         xmaxb=-xminb
         ymaxb=-yminb
         zmaxb=-zminb
         do j=1,nb
            do i=1,3
               xminb=min(xminb,xbb(i,j))
               yminb=min(yminb,ybb(i,j))
               zminb=min(zminb,zbb(i,j))
               xmaxb=max(xmaxb,xbb(i,j))
               ymaxb=max(ymaxb,ybb(i,j))
               zmaxb=max(zmaxb,zbb(i,j))
            enddo
         enddo
         xoff=xminb
         yoff=yminb
         zoff=zminb
         xfac=1.0/(xmaxb-xminb)
         yfac=1.0/(ymaxb-yminb)
         zfac=1.0/(zmaxb-zminb)
         xoff=0.0
         yoff=0.0
         zoff=0.0
         xfac=1.0
         yfac=1.0
         zfac=1.0
         do j=1,nb
            do i=1,3
C*****         xbb(i,j)=xfac*(xbb(i,j)-xoff)
C*****         ybb(i,j)=yfac*(ybb(i,j)-yoff)
C*****         zbb(i,j)=zfac*(zbb(i,j)-zoff)
            enddo
         enddo
         xminb=1.0e+30
         yminb=1.0e+30
         zminb=1.0e+30
         xmaxb=-xminb
         ymaxb=-yminb
         zmaxb=-zminb
         do j=1,nb
            do i=1,3
               xminb=min(xminb,xbb(i,j))
               yminb=min(yminb,ybb(i,j))
               zminb=min(zminb,zbb(i,j))
               xmaxb=max(xmaxb,xbb(i,j))
               ymaxb=max(ymaxb,ybb(i,j))
               zmaxb=max(zmaxb,zbb(i,j))
            enddo
         enddo
         xavgb=0.5*(xminb+xmaxb)
         yavgb=0.5*(yminb+ymaxb)
         zavgb=0.5*(zminb+zmaxb)
         do j=1,nb
            do i=1,3
               xbb(i,j)=xavgb+1.001*(xbb(i,j)-xavgb)
               ybb(i,j)=yavgb+1.001*(ybb(i,j)-yavgb)
               zbb(i,j)=zavgb+1.001*(zbb(i,j)-zavgb)
            enddo
         enddo
      endif
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     ALLOCATE MEMORY FOR THE GLOBAL VORONOI CONNECTIVITY MATRIX.
C
      ivorcoord=0
      lnnvor=0
      innvor=0
      length=nnodes
      call mmgetblk('nnvornnj',isubname,ipnnvornnj,length,1,icscode)
      call mmgetblk('nnvoroff',isubname,ipnnvoroff,length,1,icscode)
      length=1
      call mmgetblk('nnvor',isubname,ipnnvor,length,1,icscode)
      do i1=1,nnodes
         nnvornnj(i1)=0
         nnvoroff(i1)=0
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     CONSTRUCT THE VORONOI CONNECTIVITY MATRIX.
C
      nnodes_new=nnodes
C
      dsmax=0.0d+00
      do it=1,nelements
         do i=1,nelmnee(itettyp(it))
            i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
            i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
            ds=sqrt((xic(i2)-xic(i1))**2+
     *              (yic(i2)-yic(i1))**2+
     *              (zic(i2)-zic(i1))**2)
            dsmax=max(dsmax,ds)
         enddo
      enddo
      xfacbnd=0.001*dsmax
C
 9990 continue
C
C*****do i1=nnodes,1,-1
C*****do i1=1,nnodes
      do idum1=1,nitp10
         i1=itp10(idum1)
         if(mod(idum1,1000).eq.0) then
            print *,'Voronoi search: ',nitp10,idum1,i1
         endif
         mpnt=i1
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
C
         nkin=0
         if(isearch_all.eq.1) then
            do i2=1,nnodes
               if(i1.ne.i2.and.ireal1(i1).eq.1) then
                  nkin=nkin+1
                  kbb(nkin)=i2
                  kbbtp(nkin)=itp1(i2)
                  xna(1,nkin)=xic(i2)
                  xna(2,nkin)=yic(i2)
                  xna(3,nkin)=zic(i2)
               endif
            enddo
         else
            do i=1,nnode_bin(i1)
               i2=inode1(nnode_off(i1)+i)
               if(i2.ne.i1) then
                  nkin=nkin+1
                  kbb(nkin)=i2
                  kbbtp(nkin)=itp1(i2)
                  xna(1,nkin)=xic(i2)
                  xna(2,nkin)=yic(i2)
                  xna(3,nkin)=zic(i2)
               endif
            enddo
            nkin1=nkin
            do i=1,nkin1
               i2=kbb(i)
               do j=1,nnode_bin(i2)
                  i3=inode1(nnode_off(i2)+j)
                  if(i3.eq.i1.or.i3.eq.i2) then
                  else
                     iflag=0
                     do k=1,nkin
                        if(kbb(k).eq.i3) then
                           iflag=k
                        endif
                     enddo
                     if(iflag.eq.0) then
                        nkin=nkin+1
                        kbb(nkin)=i3
                        kbbtp(nkin)=itp1(i3)
                        xna(1,nkin)=xic(i3)
                        xna(2,nkin)=yic(i3)
                        xna(3,nkin)=zic(i3)
                     endif
                  endif
               enddo
            enddo
            nkin1=nkin
            do i=1,nkin1
               i2=kbb(i)
               do j=1,nnode_bin(i2)
                  i3=inode1(nnode_off(i2)+j)
                  if(i3.eq.i1.or.i3.eq.i2) then
                  else
                     iflag=0
                     do k=1,nkin
                        if(kbb(k).eq.i3) then
                           iflag=k
                        endif
                     enddo
                     if(iflag.eq.0) then
                        nkin=nkin+1
                        kbb(nkin)=i3
                        kbbtp(nkin)=itp1(i3)
                        xna(1,nkin)=xic(i3)
                        xna(2,nkin)=yic(i3)
                        xna(3,nkin)=zic(i3)
                     endif
                  endif
               enddo
            enddo
         endif
C
         if(nbsave.eq.0) then
            nb=0
            if(nbndcnt(i1).gt.0) then
               do i=1,nbndcnt(i1)
                  nb=nb+1
                  ib(nb)=3
                  if1=nbndlst(nbndoff(i1)+i)
                  it=1+(if1-1)/nef1
                  if=if1-nef1*(it-1)
                  do j=1,ielmface0(if,itettyp(it))
                     i2=itet1(itetoff(it)+ielmface1(j,if,itettyp(it)))
                     xbb(j,nb)=xic(i2)
                     ybb(j,nb)=yic(i2)
                     zbb(j,nb)=zic(i2)
                  enddo
                  x1=xbb(1,nb)
                  y1=ybb(1,nb)
                  z1=zbb(1,nb)
                  x2=xbb(2,nb)
                  y2=ybb(2,nb)
                  z2=zbb(2,nb)
                  x3=xbb(3,nb)
                  y3=ybb(3,nb)
                  z3=zbb(3,nb)
                  c1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
                  c2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
                  c3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
                  xcmag=sqrt(c1*c1+c2*c2+c3*c3)
                  c1=c1/xcmag
                  c2=c2/xcmag
                  c3=c3/xcmag
                  iflag=0
                  j=0
                  dowhile(j.lt.(nb-1).and.iflag.eq.0)
                     j=j+1
                     x1=xbb(1,j)
                     y1=ybb(1,j)
                     z1=zbb(1,j)
                     x2=xbb(2,j)
                     y2=ybb(2,j)
                     z2=zbb(2,j)
                     x3=xbb(3,j)
                     y3=ybb(3,j)
                     z3=zbb(3,j)
                     d1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
                     d2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
                     d3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
                     xdmag=sqrt(d1*d1+d2*d2+d3*d3)
                     d1=d1/xdmag
                     d2=d2/xdmag
                     d3=d3/xdmag
                     xdot=c1*d1+c2*d2+c3*d3
                     if(abs(1.0d-00-xdot).lt.0.02) then
                        iflag=1
                     endif
                  enddo
                  if(iflag.ne.0) then
                     nb=nb-1
                  else
                     do j=1,3
                        xbb(j,nb)=xbb(j,nb)+xfacbnd*c1
                        ybb(j,nb)=ybb(j,nb)+xfacbnd*c2
                        zbb(j,nb)=zbb(j,nb)+xfacbnd*c3
                     enddo
                  endif
               enddo
            endif
C*****            do ikin=1,nkin
C*****               i2=kbb(ikin)
C*****               if(nbndcnt(i2).gt.0) then
C*****                  do i=1,nbndcnt(i2)
C*****                     nb=nb+1
C*****                     ib(nb)=3
C*****                     if1=nbndlst(nbndoff(i2)+i)
C*****                     it=1+(if1-1)/nef1
C*****                     if=if1-nef1*(it-1)
C*****                     do j=1,ielmface0(if,itettyp(it))
C*****                        i3=itet1(itetoff(it)+
C*****     *                           ielmface1(j,if,itettyp(it)))
C*****                        xbb(j,nb)=xic(i3)
C*****                        ybb(j,nb)=yic(i3)
C*****                        zbb(j,nb)=zic(i3)
C*****                     enddo
C*****                     x1=xbb(1,nb)
C*****                     y1=ybb(1,nb)
C*****                     z1=zbb(1,nb)
C*****                     x2=xbb(2,nb)
C*****                     y2=ybb(2,nb)
C*****                     z2=zbb(2,nb)
C*****                     x3=xbb(3,nb)
C*****                     y3=ybb(3,nb)
C*****                     z3=zbb(3,nb)
C*****                     c1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
C*****                     c2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
C*****                     c3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
C*****                     xcmag=sqrt(c1*c1+c2*c2+c3*c3)
C*****                     c1=c1/xcmag
C*****                     c2=c2/xcmag
C*****                     c3=c3/xcmag
C*****                     iflag=0
C*****                     j=0
C*****                     dowhile(j.lt.(nb-1).and.iflag.eq.0)
C*****                        j=j+1
C*****                        x1=xbb(1,j)
C*****                        y1=ybb(1,j)
C*****                        z1=zbb(1,j)
C*****                        x2=xbb(2,j)
C*****                        y2=ybb(2,j)
C*****                        z2=zbb(2,j)
C*****                        x3=xbb(3,j)
C*****                        y3=ybb(3,j)
C*****                        z3=zbb(3,j)
C*****                        d1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
C*****                        d2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
C*****                        d3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
C*****                        xdmag=sqrt(d1*d1+d2*d2+d3*d3)
C*****                        d1=d1/xdmag
C*****                        d2=d2/xdmag
C*****                        d3=d3/xdmag
C*****                        xdot=c1*d1+c2*d2+c3*d3
C*****                        if(abs(1.0d-00-xdot).lt.0.02) then
C*****                           iflag=1
C*****                        endif
C*****                     enddo
C*****                     if(iflag.ne.0) then
C*****                        nb=nb-1
C*****                     else
C*****                        do j=1,3
C*****                           xbb(j,nb)=xbb(j,nb)+xfacbnd*c1
C*****                           ybb(j,nb)=ybb(j,nb)+xfacbnd*c2
C*****                           zbb(j,nb)=zbb(j,nb)+xfacbnd*c3
C*****                        enddo
C*****                     endif
C*****                  enddo
C*****               endif
C*****            enddo
C
C*****            j=0
C*****            dowhile(j.lt.nb)
C*****               j=j+1
C*****               x1=xbb(1,j)
C*****               y1=ybb(1,j)
C*****               z1=zbb(1,j)
C*****               x2=xbb(2,j)
C*****               y2=ybb(2,j)
C*****               z2=zbb(2,j)
C*****               x3=xbb(3,j)
C*****               y3=ybb(3,j)
C*****               z3=zbb(3,j)
C*****               d1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
C*****               d2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
C*****               d3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
C*****               xdmag=sqrt(d1*d1+d2*d2+d3*d3)
C*****               d1=d1/xdmag
C*****               d2=d2/xdmag
C*****               d3=d3/xdmag
C*****               print *,i1,nb,d1,d2,d3
C*****            enddo
            nkin_save=nkin
            if(nb.gt.0) then
               call boundrfl(mpnt,xa,ya,za,
     *                       nb,ib,xbb,ybb,zbb,
     *                       nkin,
     *                       kbb,kbbtp,kbbpp,
     *                       xna)
            endif
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((nnodes_new+nkin-nkin_save).gt.length) then
               newlen=nnodes_new+nkin-nkin_save+1000
               call cmo_set_info('nnodes',cmo,newlen,1,1,ier)
               call cmo_newlen(cmo,icscode)
               call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ics)
               call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ics)
               call cmo_get_info('xic',cmo,ipxic,ilen,itype,ics)
               call cmo_get_info('yic',cmo,ipyic,ilen,itype,ics)
               call cmo_get_info('zic',cmo,ipzic,ilen,itype,ics)
            endif
            if(nkin.gt.nkin_save) then
               do i=nkin_save+1,nkin
                  nnodes_new=nnodes_new+1
                  itp1(nnodes_new)=ib(iabs(kbb(i)))
                  xic(nnodes_new)=xna(1,i)
                  yic(nnodes_new)=xna(2,i)
                  zic(nnodes_new)=xna(3,i)
                  kbb(i)=nnodes_new
               enddo
            endif
            nb=0
         elseif(nb.gt.0) then
            nkin_save=nkin
            call boundrfl(mpnt,xa,ya,za,
     *                    nb,ib,xbb,ybb,zbb,
     *                    nkin,
     *                    kbb,kbbtp,kbbpp,
     *                    xna)
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((nnodes_new+nkin-nkin_save).gt.length) then
               newlen=nnodes_new+nkin-nkin_save+1000
               call cmo_set_info('nnodes',cmo,newlen,1,1,ier)
               call cmo_newlen(cmo,icscode)
               call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,icscode)
               call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
               call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
               call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
               call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
            endif
            if(nkin.gt.nkin_save) then
               do i=nkin_save+1,nkin
                  nnodes_new=nnodes_new+1
                  itp1(nnodes_new)=ib(iabs(kbb(i)))
                  xic(nnodes_new)=xna(1,i)
                  yic(nnodes_new)=xna(2,i)
                  zic(nnodes_new)=xna(3,i)
                  kbb(i)=nnodes_new
               enddo
            endif
         endif
C
         nnj=0
         nnk=0
         call mmgetlen(ipnna1,nnj1,icscode)
         if(length.lt.nnj1maxo) then
            nnj1=nnj1maxo
            call mmnewlen('nna1',isubname,ipnna1,nnj1,icscode)
            call mmnewlen('nnatp1',isubname,ipnnatp1,nnj1,icscode)
            call mmnewlen('nn1',isubname,ipnn1,nnj1,icscode)
            call mmnewlen('nnbp1',isubname,ipnnbp1,nnj1,icscode)
         endif
         call mmgetlen(ipnnb1,nnk1,icscode)
         if(length.lt.nnk1maxo) then
            nnk1=nnk1maxo
            call mmnewlen('nnb1',isubname,ipnnb1,nnk1,icscode)
         endif
         xa1=xfac*(xa-xoff)
         ya1=yfac*(ya-yoff)
         za1=zfac*(za-zoff)
         do i=1,nkin
            xna1(1,i)=xfac*(xna(1,i)-xoff)
            xna1(2,i)=yfac*(xna(2,i)-yoff)
            xna1(3,i)=zfac*(xna(3,i)-zoff)
         enddo
         imismax=5
         call poly3d(i1,xa1,ya1,za1,imismax,ierr1,
     *               nkin,xna1,kbb,kbbtp,kbbpp,
     *               nnj1,nnk1,
     *               ipnna1,ipnnatp1,
     *               ipnn1,ipnnbp1,ipnnb1)
         if(nnj1.gt.0.and.nnk1.gt.0) then
            nnj=nnj1
            nnk=nnk1
            call mmnewlen('nna',isubname,ipnna,nnj,icscode)
            call mmnewlen('nnatp',isubname,ipnnatp,nnj,icscode)
            call mmnewlen('nn',isubname,ipnn,nnj,icscode)
            call mmnewlen('nnbp',isubname,ipnnbp,nnj,icscode)
            call mmnewlen('nnb',isubname,ipnnb,nnk,icscode)
         else
            print *,"No neighbors for point: ",i1
            goto 100
         endif
C
         do i=1,nnj
            nna(i)=kbb(nna1(i))
            nn(i)=nn1(i)
            nnbp(i)=nnbp1(i)
         enddo
         do i=1,nnk
            nnb(i)=kbb(nnb1(i))
         enddo
C
 
         if(idebug.eq.1) then
            write(logmess,9001) i1
            call writloga('default',1,logmess,1,ierr)
 9001       format(' MPNT: ',i10)
            do j1=1,nnj
               j=nna(j1)
               write(logmess,9020) j1,j,nn(j1)
               call writloga('default',1,logmess,0,ierr)
 9020          format(i4,' NNA=',i6,' NN=',i4)
               write(logmess,9011) xic(j),yic(j),zic(j)
               call writloga('default',0,logmess,0,ierr)
 9011          format('X=',1pe13.6,' Y=',1pe13.6,' Z=',1pe13.6)
            enddo
            write(logmess,9025)
            call writloga('default',2,logmess,0,ierr)
 9025       format(' ')
            do j1=1,nnj
               j=nna(j1)
               write(logmess,9030) j1,j,
     *                            (nnb(nnbp(j1)+k-1),k=1,min0(8,nn(j1)))
               call writloga('default',0,logmess,0,ierr)
 9030          format(i4,' NNA=',i5,'  NNB: ',8(i5,' '))
               if(nn(j1).gt.8) then
                  do ktemp1=9,nn(j1),8
                     ktemp2=min0(ktemp1+7,nn(j1))
                     write(logmess,9031)
     *                  (nnb(nnbp(j1)+k-1),k=ktemp1,ktemp2)
                     call writloga('default',0,logmess,0,ierr)
 9031                format(21x,8(i8,' '))
                  enddo
               endif
            enddo
         endif
C
         lnnvor=lnnvor+1+3*nnj+(nn(nnj)+nnbp(nnj)-1)
         call mmnewlen('nnvor',isubname,ipnnvor,lnnvor,ics)
         nnvornnj(i1)=nnj
         nnvoroff(i1)=innvor
         innvor=innvor+1
         nnvor(innvor)=nnj
         do i=1,nnj
            innvor=innvor+1
            nnvor(innvor)=nna(i)
         enddo
         do i=1,nnj
            innvor=innvor+1
            nnvor(innvor)=nn(i)
         enddo
         do i=1,nnj
            innvor=innvor+1
            nnvor(innvor)=nnbp(i)
         enddo
         do i=1,nnj
            do j=1,nn(i)
               ivorcoord=ivorcoord+1
               innvor=innvor+1
               nnvor(innvor)=nnb(nnbp(i)+j-1)
            enddo
         enddo
 100     continue
      enddo
C
      call mmrelblk('nnode_bin',isubname,ipnnode_bin,icscode)
      call mmrelblk('nnode_off',isubname,ipnnode_off,icscode)
      call mmrelblk('inode1',isubname,ipinode1,icscode)
C
      if(nniter.lt.nnitermax) then
         nniter=nniter+1
C
         length=2*nnodes
         call mmgetblk('nedge_bin',isubname,
     *                 ipnedge_bin,length,1,icscode)
         call mmgetblk('nedge_off',isubname,
     *                 ipnedge_off,length,1,icscode)
         do i=1,2*nnodes
            nedge_bin(i)=0
            nedge_off(i)=0
         enddo
         innvor=0
         do i1=1,nnodes
            if(nnvornnj(i1).gt.0) then
               nnj=nnvor(innvor+1)
               innvor=innvor+1
               ipnna =loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnn  =loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnnbp=loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnnb =loc(nnvor(innvor+1))
               innvor=innvor+nn(nnj)+nnbp(nnj)-1
               do j=1,nnj
                  i2=nna(j)
                  if(i2.le.nnodes_save) then
                     isum12=i1+i2
                     nedge_bin(isum12)=nedge_bin(isum12)+1
                  endif
               enddo
            endif
         enddo
         isum=0
         do i=1,2*nnodes
            if(nedge_bin(i).gt.0) then
               nedge_off(i)=isum
               isum=isum+nedge_bin(i)
            endif
            nedge_bin(i)=0
         enddo
         length=isum+1
         call mmgetblk('iedge1',isubname,
     *                 ipiedge1,length,1,icscode)
         call mmgetblk('iedge2',isubname,
     *                 ipiedge2,length,1,icscode)
         do i=1,length
            iedge1(i)=0
            iedge2(i)=0
         enddo
         innvor=0
         do i1=1,nnodes
            if(nnvornnj(i1).gt.0) then
               nnj=nnvor(innvor+1)
               innvor=innvor+1
               ipnna =loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnn  =loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnnbp=loc(nnvor(innvor+1))
               innvor=innvor+nnj
               ipnnb =loc(nnvor(innvor+1))
               innvor=innvor+nn(nnj)+nnbp(nnj)-1
               do j=1,nnj
                  i2=nna(j)
                  if(i2.le.nnodes_save) then
                     isum12=i1+i2
                     if(nedge_bin(isum12).gt.0) then
                        do k=1,nedge_bin(isum12)
                           j1=iedge1(nedge_off(isum12)+k)
                           j2=iedge2(nedge_off(isum12)+k)
                           if((i1.eq.j1.and.i2.eq.j2) .or.
     *                        (i1.eq.j2.and.i2.eq.j1)) goto 310
                        enddo
                     endif
                     nedge_bin(isum12)=nedge_bin(isum12)+1
                     iedge1(nedge_off(isum12)+nedge_bin(isum12))=i1
                     iedge2(nedge_off(isum12)+nedge_bin(isum12))=i2
 310                 continue
                  endif
               enddo
            endif
         enddo
C
         length=nnodes
         call mmgetblk('nnode_bin',isubname,
     *                 ipnnode_bin,length,1,icscode)
         call mmgetblk('nnode_off',isubname,
     *                 ipnnode_off,length,1,icscode)
         do i1=1,nnodes
            nnode_bin(i1)=0
            nnode_off(i1)=0
         enddo
         do i=1,2*nnodes
            if(nedge_bin(i).gt.0) then
               do j=1,nedge_bin(i)
                  i1=iedge1(nedge_off(i)+j)
                  i2=iedge2(nedge_off(i)+j)
                  nnode_bin(i1)=nnode_bin(i1)+1
                  nnode_bin(i2)=nnode_bin(i2)+1
               enddo
            endif
         enddo
         isum=0
         do i=1,nnodes
            if(nnode_bin(i).gt.0) then
               nnode_off(i)=isum
               isum=isum+nnode_bin(i)
            endif
            nnode_bin(i)=0
         enddo
         length=isum+1
         call mmgetblk('inode1',isubname,
     *                 ipinode1,length,1,icscode)
         do i=1,length
            inode1(i)=0
         enddo
         do i=1,2*nnodes
            if(nedge_bin(i).gt.0) then
               do j=1,nedge_bin(i)
                  i1=iedge1(nedge_off(i)+j)
                  i2=iedge2(nedge_off(i)+j)
                  nnode_bin(i1)=nnode_bin(i1)+1
                  nnode_bin(i2)=nnode_bin(i2)+1
                  inode1(nnode_off(i1)+nnode_bin(i1))=i2
                  inode1(nnode_off(i2)+nnode_bin(i2))=i1
               enddo
            endif
         enddo
         call mmrelblk('nedge_bin',isubname,ipnedge_bin,icscode)
         call mmrelblk('nedge_off',isubname,ipnedge_off,icscode)
         call mmrelblk('iedge1',isubname,ipiedge1,icscode)
         call mmrelblk('iedge2',isubname,ipiedge2,icscode)
         goto 9990
      endif
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE VECTOR OF VORONOI COORDINATES ON AN ELEMENT BY ELEMENT
C        BASIS. THEY WILL BE COMPRESSED LATER.
C
      length=ivorcoord
      call mmgetblk('xvorg',isubname,ipxvorg,length,2,icscode)
      call mmgetblk('yvorg',isubname,ipyvorg,length,2,icscode)
      call mmgetblk('zvorg',isubname,ipzvorg,length,2,icscode)
C
      ivorcoord=0
      innvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            xa=xic(i1)
            ya=yic(i1)
            za=zic(i1)
            xl1=xa
            yl1=ya
            zl1=za
            do j=1,nnj
               i2=nna(j)
               xl2=xic(i2)
               yl2=yic(i2)
               zl2=zic(i2)
               call mmgetlen(ipxvor1,len1,icscode)
               if((nnj+1).gt.len1) then
                  length=nn(j)+1+1000
                  call mmnewlen('xvor1',isubname,ipxvor1,length,icscode)
                  call mmnewlen('yvor1',isubname,ipyvor1,length,icscode)
                  call mmnewlen('zvor1',isubname,ipzvor1,length,icscode)
               endif
               call mmgetlen(ipxvorg,len1,icscode)
               if((ivorcoord+nnj+1).gt.len1) then
                  length=ivorcoord+nn(j)+1+1000
                  call mmnewlen('xvorg',isubname,ipxvorg,length,icscode)
                  call mmnewlen('yvorg',isubname,ipyvorg,length,icscode)
                  call mmnewlen('zvorg',isubname,ipzvorg,length,icscode)
               endif
               do k=1,nn(j)
                  i3=nnb(nnbp(j)+k-1)
                  if(k.eq.nn(j)) then
                     i4=nnb(nnbp(j))
                  else
                     i4=nnb(nnbp(j)+k)
                  endif
                  xl3=xic(i3)
                  yl3=yic(i3)
                  zl3=zic(i3)
                  xl4=xic(i4)
                  yl4=yic(i4)
                  zl4=zic(i4)
                  xa=xl2
                  ya=yl2
                  za=zl2
                  xb=xl3-xa
                  yb=yl3-ya
                  zb=zl3-za
                  xc=xl4-xa
                  yc=yl4-ya
                  zc=zl4-za
                  xd=xl1-xa
                  yd=yl1-ya
                  zd=zl1-za
                  xn=  yb*zc-yc*zb
                  yn=-(xb*zc-xc*zb)
                  zn=  xb*yc-xc*yb
                  x2=  yn*zb-yb*zn
                  y2=-(xn*zb-xb*zn)
                  z2=  xn*yb-xb*yn
                  q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                   (x2*xc+y2*yc+z2*zc+1.0e-30)
                  xl=q*x2+0.5*xb
                  yl=q*y2+0.5*yb
                  zl=q*z2+0.5*zb
                  dvor=-0.5*(xd*xd+yd*yd+zd*zd)
                  qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/
     *                   (xd*xn+yd*yn+zd*zn+1.0d-30)
                  xvor=qvor2*xn+xl+xa
                  yvor=qvor2*yn+yl+ya
                  zvor=qvor2*zn+zl+za
                  distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
                  distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
                  distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
                  distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
                  if(k.eq.nn(j)) then
                  endif
                  xvor1(k)=xvor
                  yvor1(k)=yvor
                  zvor1(k)=zvor
                  ivorcoord=ivorcoord+1
                  xvorg(ivorcoord)=xvor
                  yvorg(ivorcoord)=yvor
                  zvorg(ivorcoord)=zvor
               enddo
            enddo
         endif
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE FEHM STORE FILE.
C
C*****goto 9997
C
      length=nnodes
      call mmgetblk('irowcnt',isubname,ipirowcnt,length,1,icscode)
      call mmgetblk('irowoff',isubname,ipirowoff,length,1,icscode)
      call mmgetblk('irowdag',isubname,ipirowdag,length,1,icscode)
      do i1=1,nnodes
         irowcnt(i1)=0
         irowoff(i1)=0
         irowdag(i1)=0
      enddo
      length=15*nnodes
      call mmgetblk('irowmat',isubname,ipirowmat,length,1,icscode)
      call mmgetblk('icolmat',isubname,ipicolmat,length,1,icscode)
      call mmgetblk('amat',isubname,ipamat,length,2,icscode)
      call mmgetblk('xamat',isubname,ipxamat,length,2,icscode)
      call mmgetblk('yamat',isubname,ipyamat,length,2,icscode)
      call mmgetblk('zamat',isubname,ipzamat,length,2,icscode)
C
      ncoefs=0
C
      innvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            length=ncoefs+1+nnj
            call mmnewlen('irowmat',isubname,ipirowmat,length,icscode)
            call mmnewlen('icolmat',isubname,ipicolmat,length,icscode)
            call mmnewlen('amat',isubname,ipamat,length,icscode)
            call mmnewlen('xamat',isubname,ipxamat,length,icscode)
            call mmnewlen('yamat',isubname,ipyamat,length,icscode)
            call mmnewlen('zamat',isubname,ipzamat,length,icscode)
            irowoff(i1)=ncoefs
            ncoefs=ncoefs+1
            irowdag(i1)=ncoefs
            irowcnt(i1)=1
            irowmat(ncoefs)=i1
            icolmat(ncoefs)=i1
            amat(ncoefs)=0.0
            xamat(ncoefs)=0.0
            yamat(ncoefs)=0.0
            zamat(ncoefs)=0.0
            xareat=0.0
            yareat=0.0
            zareat=0.0
            xa=xic(i1)
            ya=yic(i1)
            za=zic(i1)
            xl1=xa
            yl1=ya
            zl1=za
            do j=1,nnj
               i2=nna(j)
               xl2=xic(i2)
               yl2=yic(i2)
               zl2=zic(i2)
               call mmgetlen(ipxvor1,len1,icscode)
               if((nnj+1).gt.len1) then
                  length=nn(j)+1+1000
                  call mmnewlen('xvor1',isubname,ipxvor1,length,icscode)
                  call mmnewlen('yvor1',isubname,ipyvor1,length,icscode)
                  call mmnewlen('zvor1',isubname,ipzvor1,length,icscode)
               endif
               do k=1,nn(j)
                  i3=nnb(nnbp(j)+k-1)
                  if(k.eq.nn(j)) then
                     i4=nnb(nnbp(j))
                  else
                     i4=nnb(nnbp(j)+k)
                  endif
                  xl3=xic(i3)
                  yl3=yic(i3)
                  zl3=zic(i3)
                  xl4=xic(i4)
                  yl4=yic(i4)
                  zl4=zic(i4)
                  xa=xl2
                  ya=yl2
                  za=zl2
                  xb=xl3-xa
                  yb=yl3-ya
                  zb=zl3-za
                  xc=xl4-xa
                  yc=yl4-ya
                  zc=zl4-za
                  xd=xl1-xa
                  yd=yl1-ya
                  zd=zl1-za
                  xn=  yb*zc-yc*zb
                  yn=-(xb*zc-xc*zb)
                  zn=  xb*yc-xc*yb
                  x2=  yn*zb-yb*zn
                  y2=-(xn*zb-xb*zn)
                  z2=  xn*yb-xb*yn
                  q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                   (x2*xc+y2*yc+z2*zc+1.0e-30)
                  xl=q*x2+0.5*xb
                  yl=q*y2+0.5*yb
                  zl=q*z2+0.5*zb
                  dvor=-0.5*(xd*xd+yd*yd+zd*zd)
                  qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/
     *                   (xd*xn+yd*yn+zd*zn+1.0d-30)
                  xvor=qvor2*xn+xl+xa
                  yvor=qvor2*yn+yl+ya
                  zvor=qvor2*zn+zl+za
                  distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
                  distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
                  distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
                  distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
                  if(k.eq.nn(j)) then
                  endif
                  xvor1(k)=xvor
                  yvor1(k)=yvor
                  zvor1(k)=zvor
C
                  xicvol(1)=xic(i1)
                  yicvol(1)=yic(i1)
                  zicvol(1)=zic(i1)
                  xicvol(2)=xic(i2)
                  yicvol(2)=yic(i2)
                  zicvol(2)=zic(i2)
                  xicvol(3)=xic(i3)
                  yicvol(3)=yic(i3)
                  zicvol(3)=zic(i3)
                  xicvol(4)=xic(i4)
                  yicvol(4)=yic(i4)
                  zicvol(4)=zic(i4)
C
                  call volume_element_voronoi(ifelmtet,
     *                                        xicvol,yicvol,zicvol,
     *                                        xtetvol,xedgevol)
C
                  if(i1.le.nnodes_save) then
                     volic2(i1)=volic2(i1)+xedgevol(1)
                  endif
                  if(i1.le.nnodes_save) then
                     volic2(i1)=volic2(i1)+xedgevol(2)
                  endif
                  if(i1.le.nnodes_save) then
                     volic2(i1)=volic2(i1)+xedgevol(3)
                  endif
C
               enddo
               xvor1(nn(j)+1)=xvor1(1)
               yvor1(nn(j)+1)=yvor1(1)
               zvor1(nn(j)+1)=zvor1(1)
               xarea=0.0
               yarea=0.0
               zarea=0.0
               xl12=0.5d+00*(xl1+xl2)
               yl12=0.5d+00*(yl1+yl2)
               zl12=0.5d+00*(zl1+zl2)
               do k=1,nn(j)
                  xl3=xvor1(k)
                  yl3=yvor1(k)
                  zl3=zvor1(k)
                  xl4=xvor1(k+1)
                  yl4=yvor1(k+1)
                  zl4=zvor1(k+1)
                  call volume_tet(xl1,yl1,zl1,
     *                            xl12,yl12,zl12,
     *                            xl3,yl3,zl3,
     *                            xl4,yl4,zl4,
     *                            voltet)
                  volic(i1)=volic(i1)+voltet
                  dx= ((yl3-yl12)*(zl4-zl12)-(yl4-yl12)*(zl3-zl12))
                  dy=-((xl3-xl12)*(zl4-zl12)-(xl4-xl12)*(zl3-zl12))
                  dz= ((xl3-xl12)*(yl4-yl12)-(xl4-xl12)*(yl3-yl12))
                  xarea=xarea+0.5*dx
                  yarea=yarea+0.5*dy
                  zarea=zarea+0.5*dz
               enddo
               xareat=xareat+xarea
               yareat=yareat+yarea
               zareat=zareat+zarea
               if(i2.le.nnodes_save) then
                  irowcnt(i1)=irowcnt(i1)+1
                  ncoefs=ncoefs+1
                  irowmat(ncoefs)=i1
                  icolmat(ncoefs)=i2
                  amat(ncoefs)=sqrt(xarea**2+yarea**2+zarea**2)
                  xamat(ncoefs)=xarea
                  yamat(ncoefs)=yarea
                  zamat(ncoefs)=zarea
               endif
            enddo
         endif
      enddo
C
C     *******************************************************************
C     Sort each row so the column entries are in assending order.
C
      nnmax=0
      do i1=1,nnodes
         nnmax=max(nnmax,irowcnt(i1))
      enddo
      length=nnmax
      call mmgetblk('icolsort1',isubname,ipicolsort1,length,1,icscode)
      call mmgetblk('icolsort2',isubname,ipicolsort2,length,1,icscode)
      call mmgetblk('xcolsort1',isubname,ipxcolsort1,length,2,icscode)
      do i1=1,nnodes
         if(irowcnt(i1).gt.1) then
            do i=1,irowcnt(i1)
               icolsort1(i)=icolmat(irowoff(i1)+i)
               icolsort2(i)=i
            enddo
            call isort(icolsort1,icolsort2,irowcnt(i1),2)
            do i=1,irowcnt(i1)
               icolmat(irowoff(i1)+i)=icolsort1(i)
            enddo
            do i=1,irowcnt(i1)
               xcolsort1(i)=amat(irowoff(i1)+icolsort2(i))
            enddo
            do i=1,irowcnt(i1)
               amat(irowoff(i1)+i)=xcolsort1(i)
            enddo
            do i=1,irowcnt(i1)
               xcolsort1(i)=xamat(irowoff(i1)+icolsort2(i))
            enddo
            do i=1,irowcnt(i1)
               xamat(irowoff(i1)+i)=xcolsort1(i)
            enddo
            do i=1,irowcnt(i1)
               xcolsort1(i)=yamat(irowoff(i1)+icolsort2(i))
            enddo
            do i=1,irowcnt(i1)
               yamat(irowoff(i1)+i)=xcolsort1(i)
            enddo
            do i=1,irowcnt(i1)
               xcolsort1(i)=zamat(irowoff(i1)+icolsort2(i))
            enddo
            do i=1,irowcnt(i1)
               zamat(irowoff(i1)+i)=xcolsort1(i)
            enddo
         endif
      enddo
      do i=1,ncoefs
         if(irowmat(i).eq.icolmat(i)) then
            irowdag(irowmat(i))=i
         endif
      enddo
C
      amatmin=1.0d+30
      amatmax=-amatmin
      do i=1,ncoefs
         amatmin=min(amatmin,amat(i))
         amatmax=max(amatmax,amat(i))
      enddo
C
      inotnbr=0
      do i1=1,nnodes
         if(irowcnt(i1).gt.0) then
            do i=1,irowcnt(i1)
               i2=icolmat(irowoff(i1)+i)
               if(i1.eq.i2) then
                  if(irowdag(i1).ne.(irowoff(i1)+i)) then
                     print *,'Row diagonal error: ',i1
                  endif
               else
                  iflag=0
                  do j=1,irowcnt(i2)
                     i3=icolmat(irowoff(i2)+j)
                     if(i3.eq.i1) then
                        iflag=j
                     endif
                  enddo
                  if(iflag.eq.0) then
                     inotnbr=inotnbr+1
                     print *,'Not neighbors: ',i1,i2,amat(irowoff(i1)+i)
                     irowmat(irowoff(i1)+i)=-1
                  else
                     xdiff=amat(irowoff(i1)+i)-amat(irowoff(i2)+iflag)
                     if(abs(xdiff).gt.1.0d-06*amatmax) then
                        print *,'Unequal amat: ',i1,i2,
     *                  amat(irowoff(i1)+i) /
     *                  (amat(irowoff(i2)+iflag)+1.0e-30)
                     endif
                  endif
               endif
            enddo
         endif
      enddo
C
      if(inotnbr.gt.0) then
         do i1=1,nnodes
            irowcnt(i1)=0
            irowoff(i1)=0
            irowdag(i1)=0
         enddo
         icount=0
         do i=1,ncoefs
            if(irowmat(i).gt.0) then
               icount=icount+1
               irowmat(icount)=irowmat(i)
               icolmat(icount)=icolmat(i)
               amat(icount)=amat(i)
               xamat(icount)=xamat(i)
               yamat(icount)=yamat(i)
               zamat(icount)=zamat(i)
            endif
         enddo
         ncoefs=icount
         do i=1,ncoefs
            i1=irowmat(i)
            irowcnt(i1)=irowcnt(i1)+1
            if(i1.eq.icolmat(i)) then
               irowdag(i1)=i
            endif
         enddo
         icount=0
         do i1=1,nnodes
            irowoff(i1)=icount
            icount=icount+irowcnt(i1)
         enddo
         inotnbr=0
         do i1=1,nnodes
            if(irowcnt(i1).gt.0) then
               do i=1,irowcnt(i1)
                  i2=icolmat(irowoff(i1)+i)
                  if(i1.eq.i2) then
                     if(irowdag(i1).ne.(irowoff(i1)+i)) then
                        print *,'Row diagonal error: ',i1
                     endif
                  else
                     iflag=0
                     do j=1,irowcnt(i2)
                        i3=icolmat(irowoff(i2)+j)
                        if(i3.eq.i1) then
                           iflag=j
                        endif
                     enddo
                     if(iflag.eq.0) then
                        inotnbr=inotnbr+1
                        print *,'Not neighbors: ',i1,i2,
     *                          amat(irowoff(i1)+i)
                        irowmat(irowoff(i1)+i)=-1
                     else
                        xdiff=amat(irowoff(i1)+i) -
     *                        amat(irowoff(i2)+iflag)
                        if(abs(xdiff).gt.1.0d-06*amatmax) then
                           print *,'Unequal amat: ',i1,i2,
     *                     amat(irowoff(i1)+i) /
     *                     (amat(irowoff(i2)+iflag)+1.0e-30)
                        endif
                     endif
                  endif
               enddo
            endif
         enddo
      endif
C
      neq=nnodes
      neqp1=neq+1
      ncont=neqp1+ncoefs
      iwtotl=ncoefs
C
      length=ncont
      call mmgetblk('itemp',isubname,ipitemp,length,1,icscode)
C
      ifilename=ifile(1:icharlnf(ifile)) // '_vor.stor'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         goto 9999
      endif

C
C     Get a time stamp for the file header
C
      string = fdate()
C
c DUMP VORONOI STOR FILE
c fehm expects a header on the file:
c formatted ascii
c fehmstor ascir8i4 LaGriT Sparse Matrix, Voronoi Coefficients
c 123456789012345678901234567890123456789012345678901234567890123456789012
c fehmstor ieeer8i4 LaGriT Sparse Matrix, Voronoi Coefficients
c unformatted
 
C     BINARY header not yet implemented
c
c      if(io_type .eq. 3)then
c         write(title_string,'(a)')
c     1'fehmstor ieeer8i4 LaGriT Sparse Matrix Voronoi Coefficients'
c         write(iunit)title_string
c         write(title_string,*)
c     1        string,' 3-D Linear Diffusion Model (voronoi_stor)'
c         write(iunit)title_string
 
C     ASCII header
       write(iunit,'(a)')
     1'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
 
c     SECOND LINE
      write(iunit,*)string,' 3-D Linear Diffusion Model (voronoi_stor)'
C
c     THIRD LINE
c     FEHM is only using the scalar area coefficients so only output
c     one array nnodes long with the scalar areas. Old version of the
c     code had the scalar areas in the first nnodes entries and then
c     5*nnodes of zeros
c     add num_conn_max to end of third line, 0 for now
 
      num_conn_max = 0
      narea = 3
      write(iunit,9010) iwtotl,neq,ncont, narea, num_conn_max
C
      write(iunit,9000) (volic(i),i=1,neq)
 9000 format(5(1pe20.12))
C
      do i=1,neq
         itemp(i)=irowoff(i)+neqp1
      enddo
      itemp(neqp1)=irowoff(neq)+irowcnt(neq)+neqp1
      do i=1,ncoefs
         itemp(neqp1+i)=icolmat(i)
      enddo
      write(iunit,9010) (itemp(i),i=1,ncont)
 9010 format(5i10)
C
      do i=1,ncoefs
         itemp(i)=i
      enddo
      do i=1,neqp1
         itemp(ncoefs+i)=0
      enddo
      write(iunit,9010) (itemp(i),i=1,ncont)
C
      write(iunit,9010) (irowdag(i)+neqp1,i=1,neq)
c
c    Recalculate the itemp array
c
      do i=1,neq
         itemp(i)=irowoff(i)+neqp1
      enddo
      itemp(neqp1)=irowoff(neq)+irowcnt(neq)+neqp1
      do i=1,ncoefs
         itemp(neqp1+i)=icolmat(i)
      enddo
C
C     Divide the area vector by the distance  Aij/xij
C
      icount = 0
      do i = 1, neq
         numj = itemp(i+1)-itemp(i)
         do jnum = 1, numj
            icount = icount + 1
            j = itemp(itemp(i)+jnum)
c
c   Only look at the cases where i .ne. j
c
         if(i .ne. j)then
            xij = xic(j) - xic(i)
            yij = yic(j) - yic(i)
            zij = zic(j) - zic(i)
            xijmag = sqrt(xij**2 + yij**2 + zij**2)
            amag = sqrt(xamat(icount)**2 +
     1                  yamat(icount)**2 +
     2                  zamat(icount)**2)
            xij_dot_aij =
     1        xij*xamat(icount)+yij*yamat(icount)+zij*zamat(icount)
           if(amag .gt. 1.e-10*xijmag)then
            sign_dot = xij_dot_aij/(xijmag * amag)
           else
            sign_dot = 0.0d0
           endif
            if(idebug .ne. 0)then
            if(sign_dot .lt. 0.0)then
               write(6,778)i,j,sign_dot, amag, xijmag,sign_dot*amag
               write(6,779)xij,yij,zij
               write(6,779)xamat(icount),yamat(icount),zamat(icount)
            else
               if(idebug .ge. 2)then
               write(6,781)i,j,sign_dot, amag, xijmag,sign_dot*amag
               write(6,779)xij,yij,zij
               write(6,779)xamat(icount),yamat(icount),zamat(icount)
               endif
            endif
            endif
  777       format(i5,i5,5(e14.6))
  778       format('Negative Coef',i5,i5,5(e14.6))
  781       format('Positive Coef',i5,i5,5(e14.6))
  779       format('             ',10x,5(e14.6))
 
            xamat(icount) = xamat(icount)/xijmag
            yamat(icount) = yamat(icount)/xijmag
            zamat(icount) = zamat(icount)/xijmag
             amat(icount) =  amat(icount)/xijmag
c
c    Set the coefficients that are close to zero
c    to zero. This is not a robust test since one
c    really needs to do a relative size test. Leave
c    it for now. cwg
c
             if(abs(amat(icount)) .lt. 1.e-10)then
               xamat(icount) = 0.0
               yamat(icount) = 0.0
               zamat(icount) = 0.0
                amat(icount) = 0.0
              endif
         else
            xamat(icount) = 0.0
            yamat(icount) = 0.0
            zamat(icount) = 0.0
             amat(icount) = 0.0
         endif
         enddo
       enddo
c
c     Output vector area coefficients (xamat,yamat,zamat)
c    and the scalar area coefficients (amat)
c
      write(iunit,9000) (-xamat(i),i=1,ncoefs)
      write(iunit,9000) (-yamat(i),i=1,ncoefs)
      write(iunit,9000) (-zamat(i),i=1,ncoefs)
      write(iunit,9000) (- amat(i),i=1,ncoefs)
 
      if (iunit.gt.0) close(iunit)
C
      call mmrelblk('itemp',isubname,ipitemp,icscode)
C
      goto 9997
 9997 continue
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     FILTER THE VORONOI COORDINATES TO REMOVE DUPLICATES.
C
      call get_epsilon('epsilonl', epsilonl)
      length=ivorcoord
      call mmgetblk('ialias1',isubname,ipialias1,length,1,icscode)
      call mmgetblk('ialias2',isubname,ipialias2,length,1,icscode)
      do i=1,length
         ialias1(i)=0
         ialias2(i)=0
      enddo
      call filter_points(ivorcoord,xvorg,yvorg,zvorg,epsilonl,ialias1)
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     RENAME THE NODES IN THE VORONOI CONNECTIVITY MATRIX TO REMOVE
C        DUPLICATES.
C
      call mmgetlen(ipnnvor,length,icscode)
      call mmgetblk('nnbv',isubname,ipnnbv,length,1,icscode)
C
      jnnvor=0
      innvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            do i=1,nnj
               do j=1,nn(i)
                  jnnvor=jnnvor+1
                  nnbv(jnnvor)=ialias1(jnnvor)
               enddo
            enddo
         endif
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     COMPRESS THE LIST OF VORONOI COORDINATES.
C
      icount=0
      do i1=1,ivorcoord
         if(i1.eq.ialias1(i1)) then
            icount=icount+1
            ialias2(i1)=icount
            xvorg(icount)=xvorg(i1)
            yvorg(icount)=yvorg(i1)
            zvorg(icount)=zvorg(i1)
         else
            ialias2(i1)=0
         endif
      enddo
C
      ivorcoord=icount
C
      innvor=0
      jnnvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            do i=1,nnj
               do j=1,nn(i)
                  jnnvor=jnnvor+1
                  i2=nnbv(jnnvor)
                  i4=i2
                  icount=0
                  dowhile(i4.ne.ialias1(i4))
                     icount=icount+1
                     i4=ialias1(i4)
                  enddo
                  if(icount.gt.1) then
                     print *,icount,i1,i,j,i2,i4
                  endif
                  nnbv(jnnvor)=ialias2(i4)
               enddo
            enddo
         endif
      enddo
C
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     DUMP THE VORONOI GRID TO A GMV FILE.
C
      ifilename=ifile(1:icharlnf(ifile)) // '_vor.gmv'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         goto 9999
      endif

      write(iunit,"('gmvinput ascii')")
      write(iunit,"('nodes   ',i10)") ivorcoord
      write(iunit,"(10(1pe14.5e3))") (xvorg(i),i=1,ivorcoord)
      write(iunit,"(10(1pe14.5e3))") (yvorg(i),i=1,ivorcoord)
      write(iunit,"(10(1pe14.5e3))") (zvorg(i),i=1,ivorcoord)
      write(iunit,"('cells   ',i10)") nitp10
      innvor=0
      jnnvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            write(iunit,'(a,i10)') 'general ',nnj
            write(iunit,'(20i5)') (nn(j),j=1,nnj)
c           increase format from i7 to i10 with space
            do i=1,nnj
             write(iunit,'(10(1xi10))')(nnbv(j),j=1+jnnvor,nn(i)+jnnvor)
             jnnvor=jnnvor+nn(i)
            enddo
         endif
      enddo
C
      length=nnodes
      call mmgetblk('xtemp',isubname,ipxtemp,length,2,icscode)
      write(iunit,"('variable')")
C
      icount=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            icount=icount+1
            xtemp(icount)=nnvornnj(i1)
         endif
      enddo
      write(iunit,"(a,' 0')") 'nnj'
      write(iunit,"(10(1pe14.5e3))") (xtemp(j),j=1,icount)
C
      icount=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            icount=icount+1
            xtemp(icount)=i1
         endif
      enddo
      write(iunit,"(a,' 0')") 'i1alias'
      write(iunit,"(10(1pe14.5e3))") (xtemp(j),j=1,icount)
C
      write(iunit,"('endvars')")
C
      imatmax=0
      do i1=1,nnodes
         imatmax=max(imatmax,imt1(i1))
      enddo
      write(iunit,9120) imatmax+1
 9120 format('material   ',i10,'         0')
      do i=1,imatmax
         write(iunit,'(a3,i3.3)') 'mat',i
      enddo
      write(iunit,'(a5)') 'Boundary'
      length=nnodes
      call mmgetblk('itemp',isubname,ipitemp,length,1,icscode)
      do i=1,nitp10
         i1=itp10(i)
         itemp(i)=imt1(i1)
      enddo
      write(iunit,"(20i5)") (itemp(i),i=1,nitp10)
      write(iunit,"('polygons')")
C
      if(nb.gt.0) then
         do i=1,nb
            write(iunit,9130) imatmax+1,4
            write(iunit,9140) xbb(1,i),xbb(2,i),xbb(3,i),xbb(1,i),
     *                        ybb(1,i),ybb(2,i),ybb(3,i),ybb(1,i),
     *                        zbb(1,i),zbb(2,i),zbb(3,i),zbb(1,i)
         enddo
      endif
      innvor=0
      do i1=1,nnodes
         if(nnvornnj(i1).gt.0) then
            nnj=nnvor(innvor+1)
            innvor=innvor+1
            ipnna =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnn  =loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnbp=loc(nnvor(innvor+1))
            innvor=innvor+nnj
            ipnnb =loc(nnvor(innvor+1))
            innvor=innvor+nn(nnj)+nnbp(nnj)-1
            xa=xic(i1)
            ya=yic(i1)
            za=zic(i1)
            xl1=xa
            yl1=ya
            zl1=za
            do j=1,nnj
               i2=nna(j)
               if(i2.gt.nnodes_save) then
                  xl2=xic(i2)
                  yl2=yic(i2)
                  zl2=zic(i2)
                  call mmgetlen(ipxvor1,len1,icscode)
                  if((nnj+1).gt.len1) then
                     length=nn(j)+1+1000
                     call mmnewlen('xvor1',isubname,ipxvor1,length,ics)
                     call mmnewlen('yvor1',isubname,ipyvor1,length,ics)
                     call mmnewlen('zvor1',isubname,ipzvor1,length,ics)
                  endif
                  do k=1,nn(j)
                     i3=nnb(nnbp(j)+k-1)
                     if(k.eq.nn(j)) then
                        i4=nnb(nnbp(j))
                     else
                        i4=nnb(nnbp(j)+k)
                     endif
                     xl3=xic(i3)
                     yl3=yic(i3)
                     zl3=zic(i3)
                     xl4=xic(i4)
                     yl4=yic(i4)
                     zl4=zic(i4)
                     xa=xl2
                     ya=yl2
                     za=zl2
                     xb=xl3-xa
                     yb=yl3-ya
                     zb=zl3-za
                     xc=xl4-xa
                     yc=yl4-ya
                     zc=zl4-za
                     xd=xl1-xa
                     yd=yl1-ya
                     zd=zl1-za
                     xn=  yb*zc-yc*zb
                     yn=-(xb*zc-xc*zb)
                     zn=  xb*yc-xc*yb
                     x2=  yn*zb-yb*zn
                     y2=-(xn*zb-xb*zn)
                     z2=  xn*yb-xb*yn
                     q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                      (x2*xc+y2*yc+z2*zc+1.0e-30)
                     xl=q*x2+0.5*xb
                     yl=q*y2+0.5*yb
                     zl=q*z2+0.5*zb
                     dvor=-0.5*(xd*xd+yd*yd+zd*zd)
                     qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/
     *                      (xd*xn+yd*yn+zd*zn+1.0d-30)
                     xvor=qvor2*xn+xl+xa
                     yvor=qvor2*yn+yl+ya
                     zvor=qvor2*zn+zl+za
                     distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
                     distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
                     distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
                     distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
                     if(k.eq.nn(j)) then
                     endif
                     xvor1(k)=xvor
                     yvor1(k)=yvor
                     zvor1(k)=zvor
                  enddo
                  xvor1(nn(j)+1)=xvor1(1)
                  yvor1(nn(j)+1)=yvor1(1)
                  zvor1(nn(j)+1)=zvor1(1)
                  iclr=imt1(i1)
                  write(iunit,9130) iclr,nn(j)+1
                  write(iunit,9140) (xvor1(k),k=1,nn(j)+1),
     *                              (yvor1(k),k=1,nn(j)+1),
     *                              (zvor1(k),k=1,nn(j)+1)
 9130             format(2i5)
 9140             format(3(1x,1pe22.14e3))
               endif
            enddo
         endif
      enddo
      write(iunit,"('endpoly')")
      write(iunit,"('endgmv')")
      close(iunit)
C
 9998 continue
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
C
C
C     ..................................................................
C
C
C
      voltot1=0.0
      voltot2=0.0
      volmin=1.0e+30
      volmax=-volmin
      do idum1=1,nitp10
         i1=itp10(idum1)
         voltot1=voltot1+volic(i1)
         voltot2=voltot2+volic2(i1)
         volmin=min(volmin,volic(i1))
         volmax=max(volmax,volic(i1))
      enddo
C
      print *,"Volmin/volmax: ",volmin,volmax
      print *,"Volume total: ",voltot1,voltot2
C
      goto 9999
 9999 continue
C
      call cmo_set_info('nnodes',cmo,nnodes_save,1,1,ier)
      call cmo_newlen(cmo,icscode)
      call mmrelprt(isubname,icscode)
C
      return
      end
c
      subroutine boundrfl(mpnt,xa,ya,za,
     *                    nb,ib,xbb,ybb,zbb,
     *                    nkin,
     *                    kbb,kbbtp,kbbpp,
     *                    xna)
ccht
ccht
ccht  this routine calculates the reflection of a mass point
ccht  across 'several' reflective boundaries
ccht
ccht
ccht
ccht
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer mpnt,j1,j2,j3
      real*8 xa,ya,za,z1,y1,x1,x2,y2,z2,x3,y3,z3,c1,c2,c3,
     *e1,e2,e3,f1,f2,f3,g1,g2,g3,xi1,yi1,zi1,xj1,xj2,xj3,
     * xhmag,ximag,xjmag,xtmag,xh1,xh2,xh3,c1x,c1y,c1z,
     * c2x,c2y,c2z,c3x,c3y,c3z,ddiim1,ddiip1,ddii1,dot1,dot2,
     * xi2,xi3,xsave,ysave,zsave,xfac,xcmag,
     * xsmall,xlarge,xr1,yr1,zr1,xfac1
      integer nb, ib(nb),nkin,i,length,icscode,icount,nbpick,
     *  iflag,ib1,i1,nkin_save
      real*8 xbb(3,nb), ybb(3,nb), zbb(3,nb)
      integer kbb(nkin+nb), kbbtp(nkin+nb), kbbpp(nkin+nb)
      real*8 xna(3,nkin+nb)
C
C#######################################################################
C
      pointer (ipitb, itb)
      pointer (ipxtb, xtb)
      pointer (ipytb, ytb)
      pointer (ipztb, ztb)
      pointer (ipdist, dist)
      integer itb(nb)
      real*8 xtb(nb),ytb(nb),ztb(nb),dist(nb)
C
      real*8 xd(6),yd(6),zd(6),dot(6)
      real*8 alargenumber,dsmax,ds,atolerance,anothertolerance,
     * asmallnumber
C
      character*32 isubname
C
      data xfac  / 0.001 /
      data xfac1 / 0.000001 /
      data alargenumber/1.d+99/
      data asmallnumber /1.0d-20/
      data atolerance/1.0d-15 /
      data anothertolerance/1.0d-10/
C
C#######################################################################
C
      isubname='boundrfl'
C
      dsmax=0.0
      do i=1,nkin
         ds=(xna(1,i)-xa)**2 +
     *      (xna(2,i)-ya)**2 +
     *      (xna(3,i)-za)**2
         dsmax=max(dsmax,ds)
      enddo
      dsmax=sqrt(dsmax)
C
      length=nb
      call mmgetblk('itb',isubname,ipitb,length,2,icscode)
      call mmgetblk('xtb',isubname,ipxtb,length,2,icscode)
      call mmgetblk('ytb',isubname,ipytb,length,2,icscode)
      call mmgetblk('ztb',isubname,ipztb,length,2,icscode)
      call mmgetblk('dist',isubname,ipdist,length,2,icscode)
C
      nkin_save=nkin
C
      xsmall=asmallnumber
      xlarge=alargenumber
      icount=0
      do 100 i1=1,nb
         x1=xbb(1,i1)
         y1=ybb(1,i1)
         z1=zbb(1,i1)
         x2=xbb(2,i1)
         y2=ybb(2,i1)
         z2=zbb(2,i1)
         x3=xbb(3,i1)
         y3=ybb(3,i1)
         z3=zbb(3,i1)
         call perpen3(x1,y1,z1,x2,y2,z2,x3,y3,z3,xa,ya,za,
     *                xi1,yi1,zi1,
     *                xr1,yr1,zr1)
         if(ib(i1).ne.4.and.ib(i1).ne.5) goto 110
         goto 160
ccht     check to see if (xa,ya,za) has a reflection that lies
ccht     within a section of the plane surrounded by the three
ccht     points 1 , 2 , and 3. (use point i as a reference).
         c1=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
         c2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
         c3=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
         xcmag=sqrt(c1*c1+c2*c2+c3*c3)
         e1=x1-xi1
         e2=y1-yi1
         e3=z1-zi1
         f1=x2-xi1
         f2=y2-yi1
         f3=z2-zi1
         g1=x3-xi1
         g2=y3-yi1
         g3=z3-zi1
         xh1=e2*f3-e3*f2
         xh2=-(e1*f3-e3*f1)
         xh3=e1*f2-e2*f1
         xi1=f2*g3-f3*g2
         xi2=-(f1*g3-f3*g1)
         xi3=f1*g2-f2*g1
         xj1=g2*e3-g3*e2
         xj2=-(g1*e3-g3*e1)
         xj3=g1*e2-g2*e1
         xhmag=xh1*xh1+xh2*xh2+xh3*xh3
         ximag=xi1*xi1+xi2*xi2+xi3*xi3
         xjmag=xj1*xj1+xj2*xj2+xj3*xj3
         xtmag=sqrt(xhmag)+sqrt(ximag)+sqrt(xjmag)
         if(xtmag.gt.(xcmag+atolerance)) goto 100
160      continue
         xd(2)=x1
         yd(2)=y1
         zd(2)=z1
         xd(3)=x2
         yd(3)=y2
         zd(3)=z2
         xd(4)=x3
         yd(4)=y3
         zd(4)=z3
         xd(5)=x1+x3-x2
         yd(5)=y1+y3-y2
         zd(5)=z1+z3-z2
         xd(1)=xd(5)
         yd(1)=yd(5)
         zd(1)=zd(5)
         xd(6)=x1
         yd(6)=y1
         zd(6)=z1
         do 130 j2=2,5
         j1=j2-1
         j3=j2+1
         c1x=xd(j1)-xd(j2)
         c1y=yd(j1)-yd(j2)
         c1z=zd(j1)-zd(j2)
         c2x=xd(j3)-xd(j2)
         c2y=yd(j3)-yd(j2)
         c2z=zd(j3)-zd(j2)
         c3x=xi1-xd(j2)
         c3y=yi1-yd(j2)
         c3z=zi1-zd(j2)
         ddiim1=c1x*c1x+c1y*c1y+c1z*c1z
         ddiip1=c2x*c2x+c2y*c2y+c2z*c2z
         ddii1=c3x*c3x+c3y*c3y+c3z*c3z
         dot1=(xd(j3)-xd(j2))*(xd(j1)-xd(j2))+
     1        (yd(j3)-yd(j2))*(yd(j1)-yd(j2))+
     1        (zd(j3)-zd(j2))*(zd(j1)-zd(j2))
         dot1=dot1/sqrt(ddiip1*ddiim1)
         dot2=(xi1-xd(j2))*(xd(j1)-xd(j2))+
     1        (yi1-yd(j2))*(yd(j1)-yd(j2))+
     1        (zi1-zd(j2))*(zd(j1)-zd(j2))
         dot2=dot2/sqrt(ddiim1*ddii1)
         dot(j2-1)=dot1-dot2
130      continue
         do j2=1,4
            if(dot(j2).gt.anothertolerance) goto 100
         enddo
110      continue
         icount=icount+1
         itb(icount)=i1
         xtb(icount)=xr1
         ytb(icount)=yr1
         ztb(icount)=zr1
         dist(icount)=(xr1-xa)**2+(yr1-ya)**2+(zr1-za)**2
100   continue
      xsave=xlarge
      ysave=xlarge
      zsave=xlarge
      nbpick=nb
      if(icount.lt.nbpick) nbpick=icount
      do 120 i1=1,nbpick
         if(dist(i1).lt.xsmall) then
            x1=xbb(1,i1)
            y1=ybb(1,i1)
            z1=zbb(1,i1)
            x2=xbb(2,i1)
            y2=ybb(2,i1)
            z2=zbb(2,i1)
            x3=xbb(3,i1)
            y3=ybb(3,i1)
            z3=zbb(3,i1)
            c1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
            c2=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
            c3=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
            ds=sqrt(c1**2+c2**2+c3**2)
            xtb(i1)=xa+xfac*c1/ds
            ytb(i1)=ya+xfac*c2/ds
            ztb(i1)=za+xfac*c3/ds
         endif
         iflag=0
         if(nkin.gt.nkin_save) then
            do i=nkin_save+1,nkin
               ds=(xna(1,i)-xtb(i1))**2 +
     *            (xna(2,i)-ytb(i1))**2 +
     *            (xna(3,i)-ztb(i1))**2
               if(sqrt(ds).lt.xfac1*dsmax) then
                  iflag=i
               endif
            enddo
         endif
         if(iflag.eq.0) then
            ib1=i1
            xsave=xtb(ib1)
            ysave=ytb(ib1)
            zsave=ztb(ib1)
            nkin=nkin+1
            kbb(nkin)=itb(ib1)
            kbbtp(nkin)=30
            kbbpp(nkin)=0
            xna(1,nkin)=xtb(ib1)
            xna(2,nkin)=ytb(ib1)
            xna(3,nkin)=ztb(ib1)
            dist(ib1)=xlarge
         endif
120   continue
      goto 9999
9999  continue
      call mmrelprt(isubname,icscode)
      return
      end
c
      subroutine poly3d(mpnt,xa,ya,za,imismax,ierr1,
     *                  nkin,xna,kbb,kbbtp,kbbpp,
     *                  nnj1,nnk1,
     *                  ipnna1,ipnnatp1,
     *                  ipnn1,ipnnbp1,ipnnb1)
C
C#######################################################################
C
C      PURPOSE -
C
C
C
C      INPUT ARGUMENTS -
C
C         mpnt     - GLOBAL INDEX OF THE MASS POINT THAT IS BEING
C                       SEARCHED (CALLED THE CENTRAL MASS POINT).
C         xa       - X-COORDINATE OF THE CENTRAL MASS POINT.
C         ya       - Y-COORDINATE OF THE CENTRAL MASS POINT.
C         za       - Z-COORDINATE OF THE CENTRAL MASS POINT.
C         imismax  - THE NUMBER OF (RE)TRIES THE THIS ROUTINE CAN
C                       ATTEMPT WHILE SELECTING NEIGHBORS.
C
C
C      OUTPUT ARGUMENTS -
C
C         ierr1  - THE RETURN ERROR FLAG.
C                =  ==> EVERYTHING O.K.
C               < > ==> NO NEIGHBORS SELECTED.
C
C
C      CHANGE HISTORN -
C
C         HT0106AC-87, HT0203AA-87, HT0513AA-87
C
C
C#######################################################################
C
      implicit none
      integer mpnt,imismax,ierr1,nkin,nnj1,nnk1,itry1,num3,kst2,num2,
     * nkint,numnnk1,numnnj1,num1,numnnj,numnnk,nkinaa,klos2,klos1,
     * idista,ierr,idistmax,ismax,idistmin,nkineff,ierrdum,ihcycle,
     *  iw1,nnmax,ifnnj,itry2,nkinmax,nbpick,m,nnj1max,nnk1max,
     *  nndebug1,nndebug2,icscode,ibyte,nkinnew,klsave,iflag,n2,
     *  n1,icount,k1,imov,j1,kn,i1,ict2,ifval,mnext,next,m1,ict1,n,
     *  ictmiss,ismin,nnj,nnk,ics
      real*8 xa,ya,za,xfacarea,sixth,b31,b21,c13,c12,c11,ddac,zc,yc,
     * xc,xsmall,ddab,xb,yb,zb,vol,xl,yl,zl,zero,fac2,xthsnd,fac1,
     * distmin,time,facdist,third,half,cospiov4,angp3d,xlarge,
     * ddao,zon,yon,xon,asmn,ddlmin,dmin,cdab,fvalmin,fac,
     *  d1,d2,d3,sn1,cvmgtr,xtest,delt,b11,c33,c32,c31,c23,c22,c21,
     *  facddad,deltiv
C
      character*132 logmess
C
C#######################################################################
C
      integer kbb(nkin), kbbtp(nkin), kbbpp(nkin)
      real*8 xna(3,nkin)
C
      pointer ( ipnntemp, nntemp(1000000)   )
      pointer ( ipxvtemp, xvtemp(3,1000000) )
      pointer ( ipnnsave, nnsave(2,1000000) )
      pointer ( ipxvsave, xvsave(3,1000000) )
      pointer ( ipknn   , knn(1000000)      )
      pointer ( ipddad  , ddad(1000000)     )
      pointer ( ipsn    , sn(1000000)       )
      pointer ( ipasc   , asc(1000000)      )
      pointer ( ipvsc   , vsc(1000000)      )
      pointer ( ipcosdab, cosdab(1000000)   )
      pointer ( ipddlcen, ddlcen(1000000)   )
      pointer ( ipxcen  , xcen(3,1000000)   )
      pointer ( ipfval  , fval(1000000)     )
      pointer ( ipdista , dista(1000000)    )
      pointer ( ipkcc   , kcc(1000000)      )
      pointer ( ipxkcc  , xkcc(3,1000000)   )
      pointer ( ipkdd   , kdd(1000000)      )
      pointer ( ipktt   , ktt(1000000)      )
      pointer ( ipkaa   , kaa(1000000)      )
      pointer ( ipxkaa  , xkaa(3,1000000)   )
      pointer ( ipxkbb  , xkbb(3,1000000)   )
      integer nntemp,nnsave,knn,kcc,kdd,ktt,kaa
      real*8 xvtemp,xvsave,ddad,sn,asc,vsc,cosdab,ddlcen,xcen,
     *  fval,dista,xkcc,xkaa,xkbb
C
      pointer (ipnna1, nna1(nnj1))
      pointer (ipnnatp1, nnatp1(nnj1))
      pointer (ipnn1 , nn1(nnj1))
      pointer (ipnnbp1,nnbp1(nnj1))
      pointer (ipnnb1, nnb1(nnk1))
      integer nna1,nnatp1,nn1,nnbp1,nnb1
      real *8 alargenumber,asmallnumber,asmallernumber,atolerance
C
C#######################################################################
C
      character*32 isubname, iblknam, iprtnam
C
C#######################################################################
C
      data xfacarea / 1.0d-13 /
      data alargenumber /1.0d+99/
      data asmallnumber /1.0d-20/
      data asmallernumber /1.0d-200/
      data atolerance /1.0d-14/
C
C#######################################################################
C
C
C     this routine constructs a polyhedron about a mass point
C
C
C
C
      ibyte=4
C
C
      isubname='poly3d'
C
C
      nnj=0
      nnk=0
      nkinmax=nkin
C
      nbpick=0
      do m=1,nkin
         kbbpp(m)=0
         if(kbbtp(m).ge.30) nbpick=nbpick+1
      enddo
C
      nnj1max=nkin
      nnk1max=10*nkin
C
C
C......................................................................
C     SET THE POINTERS FOR THE 'KIN' TEMPARARY WORK SPACE
C
      call mmgetblk('nntemp',isubname,ipnntemp,  nkinmax,2,icscode)
      call mmgetblk('xvtemp',isubname,ipxvtemp,3*nkinmax,2,icscode)
      call mmgetblk('nnsave',isubname,ipnnsave,2*nkinmax,2,icscode)
      call mmgetblk('xvsave',isubname,ipxvsave,3*nkinmax,2,icscode)
      call mmgetblk('knn   ',isubname,ipknn   ,  nkinmax,2,icscode)
      call mmgetblk('ddad  ',isubname,ipddad  ,  nkinmax,2,icscode)
      call mmgetblk('sn    ',isubname,ipsn    ,  nkinmax,2,icscode)
      call mmgetblk('asc   ',isubname,ipasc   ,  nkinmax,2,icscode)
      call mmgetblk('vsc   ',isubname,ipvsc   ,  nkinmax,2,icscode)
      call mmgetblk('cosdab',isubname,ipcosdab,  nkinmax,2,icscode)
      call mmgetblk('ddlcen',isubname,ipddlcen,  nkinmax,2,icscode)
      call mmgetblk('xcen  ',isubname,ipxcen  ,3*nkinmax,2,icscode)
      call mmgetblk('fval  ',isubname,ipfval  ,  nkinmax,2,icscode)
      call mmgetblk('dista ',isubname,ipdista ,  nkinmax,2,icscode)
      call mmgetblk('kcc   ',isubname,ipkcc   ,  nkinmax,2,icscode)
      call mmgetblk('xkcc  ',isubname,ipxkcc  ,3*nkinmax,2,icscode)
      call mmgetblk('kdd   ',isubname,ipkdd   ,  nkinmax,2,icscode)
      call mmgetblk('ktt   ',isubname,ipktt   ,  nkinmax,2,icscode)
      call mmgetblk('kaa   ',isubname,ipkaa   ,  nkinmax,2,icscode)
      call mmgetblk('xkaa  ',isubname,ipxkaa  ,3*nkinmax,2,icscode)
      call mmgetblk('xkbb  ',isubname,ipxkbb  ,3*nkinmax,2,icscode)
C
C......................................................................
C     SET THE POINTER TO THE SECOND LOCAL NEIGHBOR STORAGE AREA.
C
C
C......................................................................
C
      nndebug1=0
      nndebug2=0
      goto 60
 60   continue
C
      facddad=0.0
C
      itry2=0
      ifnnj=0
 70   continue
      nnmax=nkin
      iw1=11
      third=1.0/3.0
      half=0.5
      cospiov4=cos(angp3d)
      xlarge=alargenumber
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         write(logmess,9100) ihcycle,time,itry2,ifnnj,nbpick
         call writloga('bat',1,logmess,1,ierrdum)
 9100    format("Neighbor debugging information: ",i8,1x,1pe15.7,
     *          1x,3i5)
         write(logmess,9110) mpnt,xa,ya,za,itry2,facddad
         call writloga('bat',0,logmess,0,ierrdum)
 9110    format("mpnt=",i8,3(1x,1pe15.7),2x,i5,2x,1pe15.7)
      endif
      goto 90
90    continue
      fac=1.0
      nkineff=nkin-nbpick
      do 85 m=1,nkin
         dista(m)=(xna(1,m)-xa)*(xna(1,m)-xa)+
     *            (xna(2,m)-ya)*(xna(2,m)-ya)+
     *            (xna(3,m)-za)*(xna(3,m)-za)
 85   continue
      facdist=0.0
      if(nkineff.gt.0) then
         idistmin=ismin(nkineff,dista,1)
         idistmax=ismax(nkineff,dista,1)
         distmin=dista(idistmin)
         if(distmin.lt.asmallnumber) then
            write(logmess,9060) mpnt,kbb(idistmin)
            call writloga('default',1,logmess,0,ierr)
 9060       format('Points too close: ',2i10)
            call termcode(1)
         endif
         idista=ismax(nkineff,dista,1)
         fac1=fac/sqrt(dista(idista))
         facdist=dista(idistmin)/(dista(idistmax)+asmallernumber)
         xthsnd=0.0001
         if(itry2.gt.0) facddad=min(xthsnd,facdist)
      else
         fac1=1.0
         write(logmess,9070) mpnt
         call writloga('default',2,logmess,0,ierr)
 9070    format('There only boundary points left for point=',i6,
     *            ' to pick neighbors from')
      endif
      do 80 m=1,nkin
         knn(m)=0
         fac2=fac1
         xkbb(1,m)=(xna(1,m)-xa)*fac2
         xkbb(2,m)=(xna(2,m)-ya)*fac2
         xkbb(3,m)=(xna(3,m)-za)*fac2
         kaa(m)=m
         xkaa(1,m)=xkbb(1,m)
         xkaa(2,m)=xkbb(2,m)
         xkaa(3,m)=xkbb(3,m)
80    continue
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         do 700 m=1,nkin
            write(logmess,9120) m,kbb(m),kbbtp(m),kbbpp(m),
     *                          xkaa(1,m),xkaa(2,m),xkaa(2,m),dista(m)
            call writloga('bat',0,logmess,0,ierrdum)
 9120       format(i3,i7,i3,i2,4(1x,1pe15.7))
 700     continue
      endif
C
C
      ictmiss=0
 125  continue
C
C     ..................................................................
C     SET SOME GLOBAL COUNTERS TO ZERO. (THIS MUST BE DONE HERE BECAUSE
C        THIS ROUTINE HAS A NEIGHBOR RETRY CAPABILITY.)
C
      nnj1=0
      nnk1=0
      do 127 m=1,nkin
         kbbpp(m)=0
 127  continue
C
C     ...................................................................
C
      zero=0.0
      call first(mpnt,zero,zero,zero,klos1,klos2,xl,yl,zl,
     *           nkin,ipxkbb,ipdista,fac1,ierr1)
C
C    CHECK TO SEE IF THERE WAS AN ERROR IN FINDING THE FIRST
C       NEAREST NEIGHBOR.
C
      if(ierr1.ne.0) then
         goto 9999
      endif
C
C     ..................................................................
C
      vol=0.0
      num1=1
      nnsave(1,1)=klos1
      knn(1)=klos1
      numnnj=0
      numnnk=0
      nkinaa=nkin
130   continue
      xb=xkbb(1,klos1)
      yb=xkbb(2,klos1)
      zb=xkbb(3,klos1)
      ddab=xb**2+yb**2+zb**2
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         write(logmess,9130) numnnj1,numnnk1,num1,klos1
         call writloga('bat',0,logmess,0,ierrdum)
 9130    format("FACE loop: nnj1=",i4,"  nnk1=",i4,"  num1=",i4,
     *          "klos1=",i4)
         write(logmess,9132) xb,yb,zb,ddab
         call writloga('bat',0,logmess,0,ierrdum)
 9132    format("     xb=",1pe15.7," yb=",1pe15.7," zb=",1pe15.7,
     *          "  ddab=",1pe15.7)
      endif
      if(((xb-xlarge)+(yb-xlarge)+(zb-xlarge)).eq.0.0) then
         ierr1=1
         goto 9999
      endif
      nkint=nkinaa
      if(angp3d.ne.0.0) then
CHET**         call ronnn(nkint,nkinaa,kaa,xkaa,ktt,fval,
CHET**     *                    xa,ya,za,xb,yb,zb,ddab,cospiov4,ierr)
      endif
      num2=0
      kst2=0
      num3=0
135   continue
      xsmall=atolerance
      itry1=0
      xc=xkbb(1,klos2)
      yc=xkbb(2,klos2)
      zc=xkbb(3,klos2)
      if(((xc-xlarge)+(yc-xlarge)+(zc-xlarge)).eq.0.0) then
         ierr1=1
         goto 9999
      endif
      ddac=xc*xc+yc*yc+zc*zc
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         write(logmess,9140) num2,num3,kst2,klos2
         call writloga('bat',0,logmess,0,ierrdum)
 9140    format("EDGE loop: num2=",i4,"  num3=",i4,"  kst2=",i4,
     *          " klos2=",i4)
         write(logmess,9142) xc,yc,zc,ddac
         call writloga('bat',0,logmess,0,ierrdum)
 9142    format(" xc=",1pe15.7," yc=",1pe15.7," zc=",1pe15.7,
     *          "  ddac=",1pe15.7)
         write(logmess,9144) xl,yl,zl
         call writloga('bat',0,logmess,0,ierrdum)
 9144    format(" xl=",1pe15.7," yl=",1pe15.7," zl=",1pe15.7)
      endif
C*****nkint=nkinaa
C*****call ronnn(nkint,nkinaa,kaa,xkaa,ktt,fval,
C*****                 xa,ya,za,xc,yc,zc,ddac,ierr)
      c11=yc*zb-yb*zc
      c12=-(xc*zb-xb*zc)
      c13=xc*yb-xb*yc
      b21=half*ddac
      b31=half*ddab
      sixth=1.0/6.0
      do 300 m=1,nkinaa
         kcc(m)=m
c        sn(m)=sixth*(xkaa(1,m)*c11+xkaa(2,m)*c12+xkaa(3,m)*c13)
         sn(m)=sixth*(xkaa(1,m)*c11+xkaa(2,m)*c12+xkaa(3,m)*c13) +
     *         xfacarea
300   continue
      sn(klos1)=xlarge
      sn(klos2)=xlarge
      if(num2.gt.1) then
         do 321 n=2,num2
            sn(nntemp(n))=xlarge
 321     continue
      endif
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         do 710 m=1,nkinaa
            write(logmess,9150) m,sn(m),xkaa(1,m),xkaa(2,m),xkaa(3,m)
            call writloga('bat',0,logmess,0,ierrdum)
 9150       format("SN1: ",i4,4(1x,1pe15.7))
 710     continue
      endif
      call kmprsmr(nkinaa,sn,1,kcc,1,kcc,1,ict1)
c     ict1=0
c     do 305 m=1,nkinaa
c        if(sn(m).ge.-xfacarea) goto 305
c        if(kaa(m).eq.klos1.or.kaa(m).eq.klos2) goto 305
c        ict1=ict1+1
c        kcc(ict1)=m
c305  continue
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         if(ict1.gt.0) then
            do 720 m1=1,ict1
               m=kcc(m1)
               write(logmess,9160) m1,m,sn(m),xkaa(1,m),xkaa(2,m),
     *                                        xkaa(3,m)
               call writloga('bat',0,logmess,0,ierrdum)
 9160          format("SN2: ",i4,1x,i4,4(1x,1pe15.7))
 720        continue
         endif
      endif
      if(ict1.eq.1) then
         next=kaa(kcc(1))
         mnext=1
         c21=-(xkaa(2,kcc(1))*zb-yb*xkaa(3,kcc(1)))
         c22=  xkaa(1,kcc(1))*zb-xb*xkaa(3,kcc(1))
         c23=-(xkaa(1,kcc(1))*yb-xb*xkaa(2,kcc(1)))
         c31=  xkaa(2,kcc(1))*zc-yc*xkaa(3,kcc(1))
         c32=-(xkaa(1,kcc(1))*zc-xc*xkaa(3,kcc(1)))
         c33=  xkaa(1,kcc(1))*yc-xc*xkaa(2,kcc(1))
         b11=half*(xkaa(1,kcc(1))**2+xkaa(2,kcc(1))**2+
     *             xkaa(3,kcc(1))**2)
         delt=xkaa(1,kcc(1))*c11+xkaa(2,kcc(1))*c12+xkaa(3,kcc(1))*c13
         xtest=abs(delt)
         sn1=cvmgtr(1.0,delt,xtest.lt.xsmall)
         deltiv=cvmgtr(0.0,1.0/sn1,xtest.lt.xsmall)
         xon=deltiv*(c11*b11+c21*b21+c31*b31)
         yon=deltiv*(c12*b11+c22*b21+c32*b31)
         zon=deltiv*(c13*b11+c23*b21+c33*b31)
         goto 180
      elseif(ict1.ne.0) then
         call hgather(ict1,xkcc(1,1),3,xkaa(1,1),3,kcc,1)
         call hgather(ict1,xkcc(2,1),3,xkaa(2,1),3,kcc,1)
         call hgather(ict1,xkcc(3,1),3,xkaa(3,1),3,kcc,1)
      else
         write(logmess,9101) mpnt
         call writloga('default',0,logmess,0,ierr)
 9101    format('No possible neighbors left after step 1: mpnt=',i10)
         ictmiss=ictmiss+1
         if(ictmiss.le.imismax) then
            if(ictmiss.eq.1) then
               if(nndebug1.eq.1) then
                  if(nndebug2.eq.0) then
                     nndebug2=1
                     ictmiss=0
                     goto 60
                  endif
               endif
               facddad=0.01
            else
               xkaa(1,klos1)=xlarge
               xkaa(2,klos1)=xlarge
               xkaa(3,klos1)=xlarge
               xkbb(1,klos1)=xlarge
               xkbb(2,klos1)=xlarge
               xkbb(3,klos1)=xlarge
            endif
            goto 125
         else
            ierr1=1
            goto 9999
         endif
      endif
c*****xl=third*(xb+xc)
c*****yl=third*(yb+yc)
c*****zl=third*(zb+zc)
      asmn=xlarge
      ddlmin=xlarge
      next=-1000
      dmin=xlarge
      cdab=xlarge
      fvalmin=xlarge
      do 310 m=1,ict1
         c21=-(xkcc(2,m)*zb-yb*xkcc(3,m))
         c22=  xkcc(1,m)*zb-xb*xkcc(3,m)
         c23=-(xkcc(1,m)*yb-xb*xkcc(2,m))
         c31=  xkcc(2,m)*zc-yc*xkcc(3,m)
         c32=-(xkcc(1,m)*zc-xc*xkcc(3,m))
         c33=  xkcc(1,m)*yc-xc*xkcc(2,m)
         b11=half*(xkcc(1,m)**2+xkcc(2,m)**2+xkcc(3,m)**2)
         delt=xkcc(1,m)*c11+xkcc(2,m)*c12+xkcc(3,m)*c13
         deltiv=1.0/(delt+asmallernumber)
         xcen(1,m)=deltiv*(c11*b11+c21*b21+c31*b31)
         xcen(2,m)=deltiv*(c12*b11+c22*b21+c32*b31)
         xcen(3,m)=deltiv*(c13*b11+c23*b21+c33*b31)
         d1=(yl-half*yb)*(xcen(3,m)-half*zb)-
     *        (zl-half*zb)*(xcen(2,m)-half*yb)
         d2=-((xl-half*xb)*(xcen(3,m)-half*zb)-
     *        (zl-half*zb)*(xcen(1,m)-half*xb))
         d3=(xl-half*xb)*(xcen(2,m)-half*yb)-
     *        (yl-half*yb)*(xcen(1,m)-half*xb)
         vsc(m)=abs(sixth*half*(xb*d1+yb*d2+zb*d3))
         ddlcen(m)=(xl-xcen(1,m))*(xl-xcen(1,m))+
     1             (yl-xcen(2,m))*(yl-xcen(2,m))+
     1             (zl-xcen(3,m))*(zl-xcen(3,m))
         ddad(m)=xkcc(1,m)**2+xkcc(2,m)**2+xkcc(3,m)**2
         fval(m)=vsc(m)+ddlcen(m)+facddad*ddad(m)
310   continue
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         do 740 m=1,ict1
            write(logmess,9180) m,kcc(m),fval(m),vsc(m),ddlcen(m)
            call writloga('bat',0,logmess,0,ierrdum)
 9180       format("ICT1: ",i4,1x,i4," fval=",1pe15.7," vsc=",1pe15.7,
     *             " ddlcen=",1pe15.7)
            write(logmess,9182) ddad(m),xcen(1,m),xcen(2,m),xcen(3,m)
            call writloga('bat',0,logmess,0,ierrdum)
 9182       format(5x," ddad=",1pe15.7," xcen=",1pe15.7," ycen=",
     *             1pe15.7," zcen=",1pe15.7)
 740     continue
      endif
      ifval=ismin(ict1,fval,1)
      fvalmin=fval(ifval)
c     do 320 m=1,ict1
c        kdd(m)=m
c        fval(m)=(fval(m)-fvalmin)-xfacarea
c320  continue
c     call kmprsmr(ict1,fval,1,kdd,1,kdd,1,ict2)
c     call kmprsmr(ict1,fval,1,kcc,1,kcc,1,ict2)
      ict2=0
      do 320 m=1,ict1
         if(fval(m).le.1.001*fvalmin) then
            ict2=ict2+1
            kcc(ict2)=kcc(m)
            vsc(ict2)=vsc(m)
            ddlcen(ict2)=ddlcen(m)
            ddad(ict2)=ddad(m)
            xcen(1,ict2)=xcen(1,m)
            xcen(2,ict2)=xcen(2,m)
            xcen(3,ict2)=xcen(3,m)
         endif
 320  continue
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         do 750 m=1,ict2
            write(logmess,9190) m,kcc(m),fvalmin,vsc(m),ddlcen(m)
            call writloga('bat',0,logmess,0,ierrdum)
 9190       format("ICT2: ",i4,1x,i4," fval=",1pe15.7," vsc=",1pe15.7,
     *             " ddlcen=",1pe15.7)
            write(logmess,9192) ddad(m),xcen(1,m),xcen(2,m),xcen(3,m)
            call writloga('bat',0,logmess,0,ierrdum)
 9192       format(5x," ddad=",1pe15.7," xcen=",1pe15.7," ycen=",
     *             1pe15.7," zcen=",1pe15.7)
 750     continue
      endif
      if(ict2.eq.1) then
         next=kaa(kcc(1))
         mnext=1
         xon=xcen(1,1)
         yon=xcen(2,1)
         zon=xcen(3,1)
      elseif(ict2.ne.0) then
         call hgather(ict2,xkcc(1,1),3,xkbb(1,1),3,kcc,1)
         call hgather(ict2,xkcc(2,1),3,xkbb(2,1),3,kcc,1)
         call hgather(ict2,xkcc(3,1),3,xkbb(3,1),3,kcc,1)
c        call gather(ict2,vsc,vsc,kdd)
c        call gather(ict2,ddlcen,ddlcen,kdd)
c        call hgather(ict2,xcen(1,1),3,xcen(1,1),3,kdd,1)
c        call hgather(ict2,xcen(2,1),3,xcen(2,1),3,kdd,1)
c        call hgather(ict2,xcen(3,1),3,xcen(3,1),3,kdd,1)
         do 330 m=1,ict2
            ddad(m)=xcen(1,m)**2+xcen(2,m)**2+xcen(3,m)**2
            cosdab(m)=abs(xb*xkcc(1,m)+yb*xkcc(2,m)+
     *                    zb*xkcc(3,m))/sqrt(ddab*ddad(m))
 330     continue
         if(nndebug1.eq.1.and.nndebug2.eq.1) then
            do 760 m=1,ict2
               write(logmess,9200) m,kcc(m),ddad(m),cosdab(m)
               call writloga('bat',0,logmess,0,ierrdum)
 9200          format("DDAD: ",i4,1x,i4," ddad=",1pe22.15," cosdab=",
     *                1pe22.15)
 760        continue
         endif
         m=ismin(ict2,ddad,1)
         next=kaa(kcc(m))
         mnext=m
         dmin=ddad(m)
         cdab=cosdab(m)
         xon=xcen(1,m)
         yon=xcen(2,m)
         zon=xcen(3,m)
 340     continue
      else
         write(logmess,9010) mpnt
         call writloga('bat',2,logmess,0,ierr)
 9010    format('No possible neighbors left after step 2: mpnt=',i10)
         ierr1=1
         goto 9999
      endif
      goto 180
 180  continue
 181  continue
      if(nndebug1.eq.1.and.nndebug2.eq.1) then
         ddao=xon**2+yon**2+zon**2
         write(logmess,9210) next,xon,yon,zon,ddao
         call writloga('bat',0,logmess,0,ierrdum)
 9210    format("NEXT: ",i4,4(1x,1pe15.7))
      endif
      if(next.eq.kst2) goto 255
      if(next.eq.klos2.or.next.eq.-1000) goto 242
      if(num2.ge.nnmax) goto 240
      xl=xon
      yl=yon
      zl=zon
      klos2=next
      num3=num3+1
      if(num3.le.3) goto 135
      if(num2.eq.0)  kst2=next
      do 185 i1=1,num2
         if(next.eq.nntemp(i1)) then
         num2=num2+1
         nntemp(num2)=next
         goto 240
      endif
 185  continue
      num2=num2+1
      kn=num2
      knn(num2)=next
      nntemp(num2)=next
      xvtemp(1,num2)=xon
      xvtemp(2,num2)=yon
      xvtemp(3,num2)=zon
      klos2=next
      next=0
      xl=xon
      yl=yon
      zl=zon
      ierr=0
      goto 135
240   continue
245   continue
         do 600 i1=1,num2
         do 610 j1=i1+1,num2
         if(nntemp(i1).eq.nntemp(j1)) then
            imov=0
            do 120 k1=i1,j1-1
            imov=imov+1
            nntemp(imov)=nntemp(k1)
            xvtemp(1,imov)=xvtemp(1,k1)
            xvtemp(2,imov)=xvtemp(2,k1)
            xvtemp(3,imov)=xvtemp(3,k1)
 120        continue
            num2=j1-i1
            xon=xvtemp(1,num2)
            yon=xvtemp(2,num2)
            zon=xvtemp(3,num2)
            goto 255
        endif
 610    continue
 600    continue
      goto 242

C compiler error - This statement can not be reached.
C     need to redo this weird set of statements
      if(ierr.ne.0) goto 242

      ierr=1
      goto 135
242   continue
      num2=0
      write(logmess,9000) mpnt
      call writloga('default',1,logmess,0,ierr)
 9000 format('Pollycon cannot construct polyhedron for mass point: ',i9)
250   continue
C
C    set an error code so that the calling code can
C    cleanup if it wants to.
C
      ierr1=1
      goto 9999
 255  continue
      xvtemp(1,1)=xon
      xvtemp(2,1)=yon
      xvtemp(3,1)=zon
260   continue
      icount=num1
      do 400 i1=1,num2
         n1=iabs(nntemp(i1))
         do 410 j1=1,num1
            n2=iabs(nnsave(1,j1))
            if(n1.eq.n2) goto 415
410      continue
         icount=icount+1
         nnsave(1,icount)=n1
         nnsave(2,icount)=klos1
         xvsave(1,icount)=xvtemp(1,i1)
         xvsave(2,icount)=xvtemp(2,i1)
         xvsave(3,icount)=xvtemp(3,i1)
         goto 400
415      continue
         nnsave(2,j1)=klos1
         xvsave(1,j1)=xvtemp(1,i1)
         xvsave(2,j1)=xvtemp(2,i1)
         xvsave(3,j1)=xvtemp(3,i1)
400   continue
C
C.........................................................................
C      HERE WE CHECK TO SEE IF THE LOCAL NEIGHBOR ACCUMULATION ARRIES
C         HAVE BEEN FILLED UP. IF THEY HAVE BEEN THEN WE MUST EXPAND
C         MEMORY.  NOTE: SINCE THESE ARRIES ARE MANAGED AS ONE BLOCK
C         OF MEMORY THE INTERNAL STRUCTURE IS NOT KNOWN TO THE MEMORY
C         MANAGER. THEREFORE, WE MUST CAREFULLY MOVE THE NEIGHBOR
C         DATA AROUND OURSELVES.
C
      iflag=0
      if(numnnj+1.gt.nnj1max) then
         iflag=1
         nnj1max=nnj1max+10
      endif
      if(numnnk+num2.gt.nnk1max) then
         iflag=1
         nnk1max=nnk1max+num2+10
      endif
      if(iflag.eq.1) then
CHET**   call hmemadjb('nodneig2',1)
         print *,"Increment neigh2"
         call mmgetnam(ipnna1,iblknam,iprtnam,ics)
            call mmnewlen(iblknam,iprtnam,ipnna1,nnj1max,ics)
         call mmgetnam(ipnnatp1,iblknam,iprtnam,ics)
            call mmnewlen(iblknam,iprtnam,ipnnatp1,nnj1max,ics)
         call mmgetnam(ipnn1,iblknam,iprtnam,ics)
            call mmnewlen(iblknam,iprtnam,ipnn1,nnj1max,ics)
         call mmgetnam(ipnnbp1,iblknam,iprtnam,ics)
            call mmnewlen(iblknam,iprtnam,ipnnbp1,nnj1max,ics)
         call mmgetnam(ipnnb1,iblknam,iprtnam,ics)
            call mmnewlen(iblknam,iprtnam,ipnnb1,nnk1max,ics)
C
C......................................................................
C        RESET SET THE POINTERS FOR THE 'KIN' TEMPARARY WORK SPACE
C
         call mmnewlen('nntemp',isubname,ipnntemp,  nkinmax,icscode)
         call mmnewlen('xvtemp',isubname,ipxvtemp,3*nkinmax,icscode)
         call mmnewlen('nnsave',isubname,ipnnsave,nkinmax,icscode)
         call mmnewlen('xvsave',isubname,ipxvsave,3*nkinmax,icscode)
         call mmnewlen('knn   ',isubname,ipknn   ,  nkinmax,icscode)
         call mmnewlen('ddad  ',isubname,ipddad  ,  nkinmax,icscode)
         call mmnewlen('sn    ',isubname,ipsn    ,  nkinmax,icscode)
         call mmnewlen('asc   ',isubname,ipasc   ,  nkinmax,icscode)
         call mmnewlen('vsc   ',isubname,ipvsc   ,  nkinmax,icscode)
         call mmnewlen('cosdab',isubname,ipcosdab,  nkinmax,icscode)
         call mmnewlen('ddlcen',isubname,ipddlcen,  nkinmax,icscode)
         call mmnewlen('xcen  ',isubname,ipxcen  ,3*nkinmax,icscode)
         call mmnewlen('fval  ',isubname,ipfval  ,  nkinmax,icscode)
         call mmnewlen('dista ',isubname,ipdista ,  nkinmax,icscode)
         call mmnewlen('kcc   ',isubname,ipkcc   ,  nkinmax,icscode)
         call mmnewlen('xkcc  ',isubname,ipxkcc  ,3*nkinmax,icscode)
         call mmnewlen('kdd   ',isubname,ipkdd   ,  nkinmax,icscode)
         call mmnewlen('ktt   ',isubname,ipktt   ,  nkinmax,icscode)
         call mmnewlen('kaa   ',isubname,ipkaa   ,  nkinmax,icscode)
         call mmnewlen('xkaa  ',isubname,ipxkaa  ,3*nkinmax,icscode)
         call mmnewlen('xkbb  ',isubname,ipxkbb  ,3*nkinmax,icscode)
C
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
      endif
C
C.........................................................................
      numnnj=numnnj+1
      nnj1=numnnj
      nnk1=nnk1+num2
      kbbpp(klos1)=1
      nna1(numnnj)=klos1
      nn1(numnnj)=num2
      nnbp1(numnnj)=numnnk+1
         do 420 i1=1,num2
         numnnk=numnnk+1
         nnb1(numnnk)=nntemp(i1)
         kbbpp(n1)=1
  420 continue
      num1=icount
      klsave=klos1
      do 430 i1=1,num1
         klos1=nnsave(1,i1)
         if(klsave.eq.klos1) nnsave(1,i1)=-nnsave(1,i1)
         klos1=nnsave(1,i1)
         if(klos1.ge.0) then
            klos2=nnsave(2,i1)
            xl=xvsave(1,i1)
            yl=xvsave(2,i1)
            zl=xvsave(3,i1)
c***********xl=third*(xkbb(1,klos1)+xkbb(1,klos2))
c***********yl=third*(xkbb(2,klos1)+xkbb(2,klos2))
c***********zl=third*(xkbb(3,klos1)+xkbb(3,klos2))
            num2=1
            nntemp(1)=klos2
            kst2=klos2
            xvtemp(1,1)=xl
            xvtemp(2,1)=yl
           xvtemp(3,1)=zl
            goto 130
         endif
430   continue
C
C.....................................................................
C     THE FOLLOWING ALGORITHM WILL RECALCULATE THE NEIGHBORS OF A
C        POINT IF THE NUMBER OF "NEW" NEIGHBORS IS SIGNIFICANTLY
C        GREATER THAN THE NUMBER OF "OLD" NEIGHBORS.
C
      if(itry2.eq.0.and.nnj.gt.0.and.nnj1.gt.int(1.5*nnj)) then
         call kmprsn(nkin,kbbpp,1,kbb,1,kbb,1,nkinnew)
         call kmprsn(nkin,kbbpp,1,kbbtp,1,kbbtp,1,nkinnew)
         call kmprsnirr(nkin,kbbpp,1,xna(1,1),3,xna(1,1),3,nkinnew)
         call kmprsnirr(nkin,kbbpp,1,xna(2,1),3,xna(2,1),3,nkinnew)
         call kmprsnirr(nkin,kbbpp,1,xna(3,1),3,xna(3,1),3,nkinnew)
         nkin=nkinnew
         nbpick=0
         do 500 m=1,nkin
            kbbpp(m)=0
            if(kbbtp(m).ge.30) nbpick=nbpick+1
 500     continue
         itry2=itry2+1
         ifnnj=nnj1
         goto 70
      endif
C
C........................................................................
C
      goto 9999
 9999 continue
      if(ierr1.eq.1.and.nndebug1.eq.1) then
         if(nndebug2.eq.0) then
            nndebug2=1
            ierr1=0
            goto 60
         elseif(nndebug2.eq.1) then
            call writfls('bat',ierrdum)
         endif
      endif
C
      call mmrelprt(isubname,icscode)
C
      return
      end
      subroutine cenpt(xa,ya,za,xb,yb,zb,xc,yc,zc,xcen,ycen,zcen)
ccht
ccht
ccht  this routine finds the center of the triangle given
ccht  the three point a,b, and c.
ccht
ccht
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      real*8 alargenumber,asmallnumber
      data alargenumber/ 1.0d+99/
      data asmallnumber/ 1.0d-200/
C
C#######################################################################
C
      dx=(yb-ya)*(zc-za)-(yc-ya)*(zb-za)
      dy=-((xb-xa)*(zc-za)-(xc-xa)*(zb-za))
      dz=(xb-xa)*(yc-ya)-(xc-xa)*(yb-ya)
      xsmall=asmallnumber
      iw1=11
      xac=0.5*(xa+xc)
      yac=0.5*(ya+yc)
      zac=0.5*(za+zc)
      xab=0.5*(xa+xb)
      yab=0.5*(ya+yb)
      zab=0.5*(za+zb)
      c11=(yb-yab)*(dz)-(dy)*(zb-zab)
      c12=(xb-xab)*(dz)-(dx)*(zb-zab)
      c13=(xb-xab)*(dy)-(dx)*(yb-yab)
      c21=(yc-yac)*(dz)-(dy)*(zc-zac)
      c22=(xc-xac)*(dz)-(dx)*(zc-zac)
      c23=(xc-xac)*(dy)-(dx)*(yc-yac)
      c31=(yc-yac)*(zb-zab)-(yb-yab)*(zc-zac)
      c32=(xc-xac)*(zb-zab)-(zc-zac)*(xb-xab)
      c33=(xc-xac)*(yb-yab)-(xb-xab)*(yc-yac)
      delta=(xc-xac)*c11-(yc-yac)*c12+(zc-zac)*c13
ccht  delta=abs(delta)
      if(abs(delta).lt.xsmall) goto 100
      c12=-c12
      c21=-c21
      c23=-c23
      c32=-c32
      b11=xac*(xc-xac)+yac*(yc-yac)+zac*(zc-zac)
      b21=xab*(xb-xab)+yab*(yb-yab)+zab*(zb-zab)
      b31=dx*xa+dy*ya+dz*za
      deltaiv=1.0/delta
      xcen=c11*b11+c21*b21+c31*b31
      ycen=c12*b11+c22*b21+c32*b31
      zcen=c13*b11+c23*b21+c33*b31
      xcen=xcen*deltaiv
      ycen=ycen*deltaiv
      zcen=zcen*deltaiv
ccht  ddisa=(xcen-xa)*(xcen-xa)+(ycen-ya)*(ycen-ya)+(zcen-za)*(zcen-za)
ccht  ddisb=(xcen-xb)*(xcen-xb)+(ycen-yb)*(ycen-yb)+(zcen-zb)*(zcen-zb)
ccht  ddisc=(xcen-xc)*(xcen-xc)+(ycen-yc)*(ycen-yc)+(zcen-zc)*(zcen-zc)
ccht  ddisd=(xcen-xd)*(xcen-xd)+(ycen-yd)*(ycen-yd)+(zcen-zd)*(zcen-zd)
      goto 9998
100   continue
      xcen=alargenumber
      ycen=alargenumber
      zcen=alargenumber
9998  continue
      goto 9999
9999  continue
      return
      end
      subroutine first(mpnt,xa,ya,za,klos1,klos2,xl,yl,zl,
     *                 nkin,ipxkbb,ipdista,xepsilon,ierr1)
ccht
ccht
ccht  this routine is used to find the first two neighbors of the
ccht  central mass point so that the polyhedron
ccht  construction algorithm can be initiated
ccht
ccht
C
C#######################################################################
C
      implicit none
      include 'chydro.h'
      include 'consts.h'
      integer ierr1,nsmax,nkin,m,iw1,next,klos1,mpnt,
     *  ismin,nnmax,klos2,ierr
      real*8 ref,xb,yb,zb,dista,xon,yon,zon,xkbb,xepsilon,
     *   xcen,ycen,zcen,fval,ddac,cosbac,wgtdist,wgtcos,
     *   fvalmin,asmn,csmn,xc,yc,zc,fval1,as,c3,c2,c1,
     *  ddab,xsmall,xlarge,xa,ya,za,xl,yl,zl
C
      character*132 logmess
      real*8 alargenumber,asmallnumber
      data alargenumber,asmallnumber/1.0d+99,1.0d-14/
C
C#######################################################################
C
ccht
ccht
      pointer ( ipxkbb, xkbb(3,1) )
      pointer ( ipdista, dista(1) )
ccht
      ierr1=0
ccht
      nsmax=9999
      nnmax=nkin
      goto 90
90    continue
      iw1=11
      xlarge=alargenumber
      xsmall=asmallnumber
      ref=xlarge
      do 120 m=1,nkin
         xb=xkbb(1,m)
         yb=xkbb(2,m)
         zb=xkbb(3,m)
         dista(m)=xb**2+yb**2+zb**2
120   continue
      klos1=ismin(nkin,dista,1)
      xb=xkbb(1,klos1)
      yb=xkbb(2,klos1)
      zb=xkbb(3,klos1)
      xl=half*xb
      yl=half*yb
      zl=half*zb
      ddab=xb**2+yb**2+zb**2
      next=0
      asmn=xlarge
      csmn=xlarge
      fvalmin=xlarge
      do m=1,nkin
         if(klos1.ne.m) then
            xc=xkbb(1,m)
            yc=xkbb(2,m)
            zc=xkbb(3,m)
            call cenpt(xa,ya,za,xb,yb,zb,xc,yc,zc,xcen,ycen,zcen)
            c1=yl*zcen-ycen*zl
            c2=xl*zcen-xcen*zl
            c3=xl*ycen-xcen*yl
            as=sqrt(c1*c1+c2*c2+c3*c3)
            if(abs(as).gt.xepsilon*1.0d-10) then
               ddac=xc*xc+yc*yc+zc*zc
               cosbac=(xb*xc+yb*yc+zb*zc)/sqrt(ddab*ddac)
               wgtdist=xepsilon*.001
               wgtcos=xepsilon*.000001
               fval1=as+wgtcos*cosbac+wgtdist*ddac
               if(fval1.le.fvalmin) then
                  fvalmin=fval1
                  xon=xcen
                  yon=ycen
                  zon=zcen
                  next=m
               endif
            endif
         endif
      enddo
      if(next.eq.klos1.or.next.eq.0) then
         xon=0.5*xb
         yon=0.5*yb
         zon=0.5*zb
         do 190 m=1,nkin
         dista(m)=(xon-xkbb(1,m))**2+
     *            (yon-xkbb(2,m))**2+
     *            (zon-xkbb(3,m))**2
 190     continue
         dista(klos1)=xlarge
         next=ismin(nkin,dista,1)
      endif
      goto 260
240   continue
      write(logmess,9020) mpnt,klos1
      call writloga('default',2,logmess,0,ierr)
9020  format('First: could not start ' , 2i10)
ccht
ccht set the error code to 2
ccht
      ierr1=2
      goto 9999
ccht
260   continue
      klos2=next
      xl=xon
      yl=yon
      zl=zon
      goto 9999
 9999 continue
      return
      end

      subroutine hgather(n,a,ia,b,ib,index,iindex)
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C
C#######################################################################
C
      dimension a(ia,1), b(ib,1), index(iindex,1)
      do 100 i=1,n
         a(1,i)=b(1,index(1,i))
 100  continue
      goto 9999
 9999 continue
      return
      end
c
      subroutine perpen3(x1,y1,z1,x2,y2,z2,x3,y3,z3,xa,ya,za,
     * xii,yii,zii,xr,yr,zr)
ccht
ccht
ccht  this routine finds the reflection and intersection of a
ccht  point (xa,ya,za) across a reflective boundary formed by the
ccht  three point 1,2 and 3.
ccht
ccht
ccht
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C#######################################################################
C
      xsmall=1.0e-10
      dx12=x2-x1
      dy12=y2-y1
      dz12=z2-z1
      dx13=x3-x1
      dy13=y3-y1
      dz13=z3-z1
      if(abs(dx12).lt.xsmall) dx12=0.0
      if(abs(dy12).lt.xsmall) dy12=0.0
      if(abs(dz12).lt.xsmall) dz12=0.0
      if(abs(dx13).lt.xsmall) dx13=0.0
      if(abs(dy13).lt.xsmall) dy13=0.0
      if(abs(dz13).lt.xsmall) dz13=0.0
      c1=  dy12*dz13-dz12*dy13
      c2=-(dx12*dz13-dz12*dx13)
      c3=  dx12*dy13-dy12*dx13
      b1=xa*dx12+ya*dy12+za*dz12
      b2=xa*dx13+ya*dy13+za*dz13
      b3=c1*x1+c2*y1+c3*z1
      a11=dx12
      a12=dy12
      a13=dz12
      a21=dx13
      a22=dy13
      a23=dz13
      a31=c1
      a32=c2
      a33=c3
      c11=  a22*a33-a32*a23
      c12=-(a12*a33-a13*a32)
      c13=  a12*a23-a13*a22
      c21=-(a21*a33-a23*a31)
      c22=  a11*a33-a13*a31
      c23=-(a11*a23-a13*a21)
      c31=  a21*a32-a22*a31
      c32=-(a11*a32-a12*a31)
      c33=  a11*a22-a12*a21
      delta1=a11*c11+a12*c21+a13*c31
      if(abs(delta1).lt.1.0e-15) goto 9999
      deltiv=1.0/delta1
      xii=deltiv*(c11*b1+c12*b2+c13*b3)
      yii=deltiv*(c21*b1+c22*b2+c23*b3)
      zii=deltiv*(c31*b1+c32*b2+c33*b3)
      xr=2.0*xii-xa
      yr=2.0*yii-ya
      zr=2.0*zii-za
      goto 9999
9999  continue
      return
      end
