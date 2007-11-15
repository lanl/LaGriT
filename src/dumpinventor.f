      subroutine dumpinventor(ifile,cmo)
 
c #####################################################################
c
c     purpose -
c
c        create a gmv formatted file from the current mesh object
c
c     input arguments -
c
c        none
c
c     output arguments -
c
c        none
c
c     change history -
c
C        $Log: dumpinventor.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   10 Apr 2001 11:17:10   dcg
CPVCS    remove bad line
CPVCS
CPVCS       Rev 1.5   14 Mar 2001 14:06:02   dcg
CPVCS    move data statements
CPVCS    get rid of upper case
CPVCS
CPVCS       Rev 1.4   13 Apr 2000 09:03:02   nnc
CPVCS    Eliminated multiple variable declarations.
CPVCS
CPVCS       Rev 1.3   Fri Apr 07 10:48:52 2000   dcg
CPVCS    fix typo get global should be get_global
CPVCS
CPVCS       Rev 1.2   Thu Apr 06 09:00:42 2000   dcg
CPVCS    implicit none
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.1   Tue Feb 08 15:09:50 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 14:22:36 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   28 Jan 2000 16:38:06   dcg
CPVCS    remove sbname
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:44:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Mon Jul 29 15:18:04 1996   dcg
CPVCS    Initial revision.
c
c ######################################################################
c
      implicit none
c
      include "local_element.h"
c
      character ifile*(*)
      character cmo*(*)
C
      integer nsd, nen, nef
c
      integer nenlin,nfacelin,nentri,nfacetri,nentet,nfacetet,nenprism,
     *  nfaceprism,nenhex,nfacehex
      parameter (nenlin=2, nfacelin=2)
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
c
      integer ihexface0(nfacehex), ihexface1(4,nfacehex)
c     top,bottom,front,right,back,left
      integer iprismface0(nfaceprism), iprismface1(4,nfaceprism)
c     top,bottom,right,back,left
c
      integer intpairhex(2,12)
 
      integer itetface0(nfacetet), itetface1(4,nfacetet)
c     top,back,left,right
      integer itriface0(nfacetri), itriface1(3,nfacetri)
C
c     top,back,left,right
C
      integer ilinface0(nfacelin), ilinface1(4,nfacelin)
C     top,back,left,right
      integer intpairtet(2,6)
c
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      real*8 rout,time,crosz,crosy,crosx,distsqd,distsqc,distsqb,distsqa
     *,zvor,yvor,xvor,qvor2,dvor,q,zc,yc,xc,xtest,xtestmax,z132,y132
     * ,z124,y124,x124,z143,y143,x143,z234,y234,x234,voltot,voltet,
     * az4,ay4,az3,ay3,ax3,az2,ay2,ax2,az1,ay1,ax1,zl4,yl4,xl4,zl3,
     * yl3,xl3,zl2,yl2,xl2,zl1,yl1,xl1,zv4,yv4,xv4,ds34,ds24,ds14,
     * x132,ax4,zv3,yv3,xv3,ds33,ds23,ds13,zv2,yv2,xv2,
     * ds32,ds22,ds12,zv1,yv1,xv1,ds31,ds21,ds11,z24,y24,x24,z14,y14,
     * x14,z41,y41,x41,z34,y34,x34,z4,y4,x4,zcen,ycen,xcen,
     * z23,y23,x23,z13,y13,x13,z12,y12,x12,ds3,ds2,ds1,zl,yl,xl,ql,
     * rb3,dot3,dotb3,rn,zn,yn,xn,zn1,yn1,xn1,zd,xd,yd,zb,yb,xb,xfac,
     * xa,ya,za,xv,yv,zv,a,b,c,d,e,f,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     * xm,ym,zm,epsilonl,distmax,dist,disttest
      integer jt,jf,i3,iclr,iflag,maxclrelement,ie,jtoff,itoff,
     *  ntriinv,i,iunit,ierrdum,icscode,ierr,ityp,ilen,nefcmo,
     *  nsdgeom,mbndry,nelements,ierror,icmotype,length,nnodes
      character*32 cout
      real*8 xic(1000000), yic(1000000), zic(1000000)
      integer imt1(1000000), itp1(1000000)
      integer itetclr(10000000), itettyp(10000000),
     *        itetoff(10000000), jtetoff(10000000)
      integer itet1(1000000), jtet1(10000000)
C
      pointer (ipxptemp, xptemp(1000000))
      real*8 xptemp,xtemp
      pointer (ipxtemp, xtemp(1000000))
      pointer (ipitemp, itemp(1000000))
      integer itemp
      pointer (ipireal1, ireal1(1000000))
      pointer (ipiclrinv, iclrinv(1000000))
      pointer (ipitypinv, itypinv(1000000))
C
C
C
c
      character*32 isubname
      character*8 cpart,  cdefname
C
C
C
C
      integer maxclrs,iout,itype,ihcycle,i5,icount,invoff,i4,j4,iclrm,
     *  iclrv,it,j,j1,j2,j3,i1,i2,ireal1,iclrinv,itypinv,
     * ivoronoi2d,ivoronoi3d,ipolydata,iflag_all
      parameter (maxclrs=256)
 
      real cdred0(maxclrs),cdgreen0(maxclrs),cdblue0(maxclrs)
      real cdred1(maxclrs),cdgreen1(maxclrs),cdblue1(maxclrs)
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      data ilinface0 / 1, 1 /
      data ilinface1 / 2, 1, 0, 0,
     *                 1, 2, 0, 0 /
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
      data cdred0 /0.,1.,1.,1.,1.,0.,1.,1.,0.,0.,.115,0.,.148,
     '  .2079,1.,.6245,1.164e-2,.248,.448,1.,1.,3.513e-2,2.122e-2,.625,
     '  .6245,1.164e-2,1.164e-2,.448,6.857e-2,.306,.4485,.448,224*1.0/
      data cdgreen0 /1.,0.,0.,1.,1.,0.,8.905e-2,0.,1.,1.,0.,
     '  8.905e-2,.248,.2079,.248,8.032e-3,.2079,1.,2.122e-2,.2745,
     '  8.905e-2,5.035e-3,.6245,.005,1.164e-2,.6245,.2079,6.857e-2,
     '  3.513e-2,1.164e-2,5.035e-3,1.,224*1.0/
      data cdblue0 /0.,.448,0.,0.,1.,1.,0.,.115,1.,.148,.448,
     '  1.,0.,.2079,5.035e-3,3.513e-2,.306,5.035e-3,8.905e-2,5.035e-2,
     '  5.235e-2,1.,2.122e-2,1.,.115,.2079,.6245,.148,1.,1.,8.905e-2,
     '  1.164e-2,224*1.0/
      data cdred1 /0.,1.,1.,1.,1.,0.,1.,1.,0.,0.,.115,0.,.148,
     '  .2079,1.,.6245,1.164e-2,.248,.448,1.,1.,3.513e-2,2.122e-2,.625,
     '  .6245,1.164e-2,1.164e-2,.448,6.857e-2,.306,.4485,.448,224*1.0/
      data cdgreen1 /1.,0.,0.,1.,1.,0.,8.905e-2,0.,1.,1.,0.,
     '  8.905e-2,.248,.2079,.248,8.032e-3,.2079,1.,2.122e-2,.2745,
     '  8.905e-2,5.035e-3,.6245,.005,1.164e-2,.6245,.2079,6.857e-2,
     '  3.513e-2,1.164e-2,5.035e-3,1.,224*1.0/
      data cdblue1 /0.,.448,0.,0.,1.,1.,0.,.115,1.,.148,.448,
     '  1.,0.,.2079,5.035e-3,3.513e-2,.306,5.035e-3,8.905e-2,5.035e-2,
     '  5.235e-2,1.,2.122e-2,1.,.115,.2079,.6245,.148,1.,1.,8.905e-2,
     '  1.164e-2,224*1.0/
C
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data ipolydata / 1 /
      data iflag_all / 0 /
c
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
c
      isubname="gmvdmp"
      cpart='part'
      cdefname='default'
c  get information from  mesh object
c
      call cmo_get_info('nnodes',cmo,
     *                  nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *                   mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsd,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nefcmo,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
      length=nnodes
      call mmgetblk("xptemp",isubname,ipxptemp,length,2,icscode)
C
      length=max(nnodes,nelements)
      call mmgetblk("xtemp",isubname,ipxtemp,length,2,icscode)
      length=max(nnodes,nelements)
      call mmgetblk("itemp",isubname,ipitemp,length,2,icscode)
C
      length=nnodes
      call mmgetblk("ireal1",isubname,ipireal1,length,2,icscode)
C
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('unpacktp', isubname)
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      write(iunit,"('#Inventor V2.0 ascii')")
      write(iunit,"('      BaseColor {')")
      write(iunit,"('        rgb     [')")
      do i=1,maxclrs
         write(iunit,"(3(1pe15.7,' '),',')")
     *                cdred0(i),cdgreen0(i),cdblue0(i)
      enddo
      write(iunit,"('                ]')")
      write(iunit,"('      }')")
      write(iunit,"('      MaterialBinding {')")
      write(iunit,"('        value   PER_FACE_INDEXED')")
      write(iunit,"('      }')")
      write(iunit,"('Separator {')")
      if(ipolydata.eq.1) then
         write(iunit,"('    Coordinate3 {')")
         write(iunit,"('	point	[')")
         call get_epsilon('epsilonl', epsilonl)
         ntriinv=0
         length=nefcmo*nelements
         call mmgetblk("iclrinv",isubname,ipiclrinv,length,1,icscode)
         call mmgetblk("itypinv",isubname,ipitypinv,length,1,icscode)
         itoff=0
         jtoff=0
         do ie=1,nelements
C
            distmax=epsilonl
            do i=1,nelmnee(itettyp(ie))
               i1=ielmedge1(1,i,itettyp(ie))
               i2=ielmedge1(2,i,itettyp(ie))
               dist=(xic(i2)-xic(i1))**2+
     *              (yic(i2)-yic(i1))**2+
     *              (zic(i2)-zic(i1))**2
               distmax=max(distmax,dist)
            enddo
            disttest=epsilonl*distmax
C
         if(itettyp(ie).eq.ifelmlin) then
            if(nelements.gt.0.and.ie.eq.1) then
               maxclrelement=0
               do it=1,nelements
                  maxclrelement=max(maxclrelement,max(1,itetclr(it)))
               enddo
               do it=1,nelements
                  if(itetclr(it).le.0) then
                     itemp(it)=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     itemp(it)=maxclrelement
                  else
                     itemp(it)=itetclr(it)
                  endif
               enddo
            endif
            it=ie
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            if(itetclr(it).le.0) then
               iclr=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               iclr=maxclrelement+1
            else
               iclr=itetclr(it)
            endif
            ntriinv=ntriinv+1
            iclrinv(ntriinv)=iclr
            itypinv(ntriinv)=itettyp(ie)
            j1=i1
            j2=i2
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j1),yic(j1),zic(j1)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j2),yic(j2),zic(j2)
C*****            write(iunit,9010)
C*****     *            iclr,2,
C*****     *            xic(i1),xic(i2),
C*****     *            yic(i1),yic(i2),
C*****     *            zic(i1),zic(i2)
C***** 9010       format(2i5,2(1x,1pe22.14e3),/,2(1x,1pe22.14e3),/,
C*****     *                   2(1x,1pe22.14e3))
            itoff=itoff+nelmnen(ifelmlin)
            jtoff=jtoff+nelmnef(ifelmlin)
         elseif(itettyp(ie).eq.ifelmtri) then
            if(nelements.gt.0.and.ie.eq.1) then
               maxclrelement=0
               do it=1,nelements
                  maxclrelement=max(maxclrelement,max(1,itetclr(it)))
               enddo
               do it=1,nelements
                  if(itetclr(it).le.0) then
                     itemp(it)=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     itemp(it)=maxclrelement
                  else
                     itemp(it)=itetclr(it)
                  endif
               enddo
            endif
            it=ie
            nef=nelmnef(ifelmtri)
            j1=itet1(itoff+1)
            j2=itet1(itoff+2)
            j3=itet1(itoff+3)
            if(itetclr(it).le.0) then
               iclr=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               iclr=maxclrelement+1
            else
               iclr=itetclr(it)
            endif
            ntriinv=ntriinv+1
            iclrinv(ntriinv)=iclr
            itypinv(ntriinv)=itettyp(ie)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j1),yic(j1),zic(j1)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j2),yic(j2),zic(j2)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j3),yic(j3),zic(j3)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nefcmo
                  jf=jtet1(jtoff+i)-mbndry-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nefcmo
                  jf=jtet1(jtoff+i)-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  j1=itet1(itoff+itriface1(1,i))
                  j2=itet1(itoff+itriface1(2,i))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
C*****                  write(iunit,9010)
C*****     *                  iclr,2,
C*****     *                  xic(j1),xic(j2),
C*****     *                  yic(j1),yic(j2),
C*****     *                  zic(j1),zic(j2)
               endif
            enddo
            if(ivoronoi2d.gt.0) then
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
               xm=(xic(i1)+xic(i2)+xic(i3))/3.0
               ym=(yic(i1)+yic(i2)+yic(i3))/3.0
               zm=(zic(i1)+zic(i2)+zic(i3))/3.0
               xv=xm
               yv=ym
               zv=zm
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0
               xb=xfac*(xic(i2)-xa)
               yb=xfac*(yic(i2)-ya)
               zb=xfac*(zic(i2)-za)
               xd=xfac*(xic(i3)-xa)
               yd=xfac*(yic(i3)-ya)
               zd=xfac*(zic(i3)-za)
               xn1=crosx(xb,yb,zb,xd,yd,zd)
               yn1=crosy(xb,yb,zb,xd,yd,zd)
               zn1=crosz(xb,yb,zb,xd,yd,zd)
               xn=crosx(xb,yb,zb,xn1,yn1,zn1)
               yn=crosy(xb,yb,zb,xn1,yn1,zn1)
               zn=crosz(xb,yb,zb,xn1,yn1,zn1)
               rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
               xn=xn*rn
               yn=yn*rn
               zn=zn*rn
               dotb3=xb*xd+yb*yd+zb*zd
               dot3=dotb3/(xd*xd+yd*yd+zd*zd)
               rb3=1.0/(xb*xb+yb*yb+zb*zb)
               ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
               xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
               yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
               zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
               ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
               ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
               ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
               xv=xl+xa
               yv=yl+ya
               zv=zl+za
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x13=0.5*(xic(i1)+xic(i3))
               y13=0.5*(yic(i1)+yic(i3))
               z13=0.5*(zic(i1)+zic(i3))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               xcen=xv
               ycen=yv
               zcen=zv
               if(itetclr(it).le.0) then
                  iclrv=max(1,maxclrelement+1)
                  iclrm=max(1,maxclrelement+1)
               elseif(itetclr(it).gt.maxclrelement) then
                  iclrv=max(1,maxclrelement+1)
                  iclrm=max(1,maxclrelement+1)
               else
                  iclrv=  maxclrelement+1+itetclr(it)
                  iclrm=2*maxclrelement+1+itetclr(it)
               endif
C
               dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrv,2,
C*****     *                           xcen,x12,
C*****     *                           ycen,y12,
C*****     *                           zcen,z12
               endif
               dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrv,2,
C*****     *                           xcen,x13,
C*****     *                           ycen,y13,
C*****     *                           zcen,z13
               endif
               dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrv,2,
C*****     *                           xcen,x23,
C*****     *                           ycen,y23,
C*****     *                           zcen,z23
               endif
               xcen=xm
               ycen=ym
               zcen=zm
               dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x12,
C*****     *                           ycen,y12,
C*****     *                           zcen,z12
               endif
               dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x13,
C*****     *                           ycen,y13,
C*****     *                           zcen,z13
               endif
               dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x23,
C*****     *                           ycen,y23,
C*****     *                           zcen,z23
               endif
            endif
            itoff=itoff+nelmnen(ifelmtri)
            jtoff=jtoff+nelmnef(ifelmtri)
         elseif(itettyp(ie).eq.ifelmqud) then
            if(nelements.gt.0.and.ie.eq.1) then
               maxclrelement=0
               do it=1,nelements
                  maxclrelement=max(maxclrelement,max(1,itetclr(it)))
               enddo
               do it=1,nelements
                  if(itetclr(it).le.0) then
                     itemp(it)=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     itemp(it)=maxclrelement
                  else
                     itemp(it)=itetclr(it)
                  endif
               enddo
            endif
            it=ie
            nef=nelmnef(ifelmqud)
            j1=itet1(itoff+1)
            j2=itet1(itoff+2)
            j3=itet1(itoff+3)
            j4=itet1(itoff+4)
            if(itetclr(it).le.0) then
               iclr=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               iclr=maxclrelement+1
            else
               iclr=itetclr(it)
            endif
            ntriinv=ntriinv+1
            iclrinv(ntriinv)=iclr
            itypinv(ntriinv)=itettyp(ie)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j1),yic(j1),zic(j1)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j2),yic(j2),zic(j2)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j3),yic(j3),zic(j3)
            write(iunit,"(3(1pe22.14e3),', ')")
     *            xic(j4),yic(j4),zic(j4)
            it=ie
            nef=nelmnef(ifelmqud)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nefcmo
                  jf=jtet1(jtoff+i)-mbndry-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nefcmo
                  jf=jtet1(jtoff+i)-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  j1=itet1(itoff+ielmface1(1,i,ifelmqud))
                  j2=itet1(itoff+ielmface1(2,i,ifelmqud))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
C*****                  write(iunit,9010)
C*****     *                  iclr,2,
C*****     *                  xic(j1),xic(j2),
C*****     *                  yic(j1),yic(j2),
C*****     *                  zic(j1),zic(j2)
               endif
            enddo
            if(ivoronoi2d.gt.0) then
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               i4=itet1(itoff+4)
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
               x4=xic(i4)
               y4=yic(i4)
               z4=zic(i4)
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               x34=0.5*(xic(i3)+xic(i4))
               y34=0.5*(yic(i3)+yic(i4))
               z34=0.5*(zic(i3)+zic(i4))
               x41=0.5*(xic(i4)+xic(i1))
               y41=0.5*(yic(i4)+yic(i1))
               z41=0.5*(zic(i4)+zic(i1))
               xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
               ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
               zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
               xcen=xm
               ycen=ym
               zcen=zm
C
               if(itetclr(it).le.0) then
                  iclrm=max(1,maxclrelement+1)
               elseif(itetclr(it).gt.maxclrelement) then
                  iclrm=max(1,maxclrelement+1)
               else
                  iclrm=2*maxclrelement+1+itetclr(it)
               endif
C
               dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x12,
C*****     *                           ycen,y12,
C*****     *                           zcen,z12
               endif
               dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x23,
C*****     *                           ycen,y23,
C*****     *                           zcen,z23
               endif
               dist=(xcen-x34)**2+(ycen-y34)**2+(zcen-z34)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x34,
C*****     *                           ycen,y34,
C*****     *                           zcen,z34
               endif
               dist=(xcen-x41)**2+(ycen-y41)**2+(zcen-z41)**2
               if(dist.gt.disttest) then
C*****                  write(iunit,9010) iclrm,2,
C*****     *                           xcen,x41,
C*****     *                           ycen,y41,
C*****     *                           zcen,z41
               endif
            endif
            itoff=itoff+nelmnen(ifelmqud)
            jtoff=jtoff+nelmnef(ifelmqud)
         elseif(itettyp(ie).eq.ifelmtet) then
            if(nelements.gt.0.and.ie.eq.1) then
               maxclrelement=0
               do it=1,nelements
                  maxclrelement=max(maxclrelement,max(1,itetclr(it)))
               enddo
               iclr=maxclrelement+1
               if(ivoronoi3d.gt.0) iclr=2*maxclrelement+1
               do it=1,nelements
                  if(itetclr(it).le.0) then
                     itemp(it)=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     itemp(it)=maxclrelement
                  else
                     itemp(it)=itetclr(it)
                  endif
               enddo
            endif
            it=ie
            nef=nelmnef(ifelmtet)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nefcmo
                  jf=jtet1(jtoff+i)-mbndry-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nefcmo
                  jf=jtet1(jtoff+i)-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  j1=itet1(itoff+itetface1(1,i))
                  j2=itet1(itoff+itetface1(2,i))
                  j3=itet1(itoff+itetface1(3,i))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  ntriinv=ntriinv+1
                  iclrinv(ntriinv)=iclr
                  itypinv(ntriinv)=itettyp(ie)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j1),yic(j1),zic(j1)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j2),yic(j2),zic(j2)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j3),yic(j3),zic(j3)
C*****                  write(iunit,9030) iclr,3,
C*****     *                  xic(j1),xic(j2),xic(j3),
C*****     *                  yic(j1),yic(j2),yic(j3),
C*****     *                  zic(j1),zic(j2),zic(j3)
C***** 9030          format(2i5,3(1x,1pe22.14e3),/,3(1x,1pe22.14e3),/,
C*****     *                    3(1x,1pe22.14e3))
               endif
            enddo
            if(ivoronoi3d.gt.0) then
               if(itetclr(it).le.0) then
                  iclr=maxclrelement+1
               elseif(itetclr(it).gt.maxclrelement) then
                  iclr=maxclrelement+1
               else
                  iclr=maxclrelement+1+itetclr(it)
               endif
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               i4=itet1(itoff+4)
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x13=0.5*(xic(i1)+xic(i3))
               y13=0.5*(yic(i1)+yic(i3))
               z13=0.5*(zic(i1)+zic(i3))
               x14=0.5*(xic(i1)+xic(i4))
               y14=0.5*(yic(i1)+yic(i4))
               z14=0.5*(zic(i1)+zic(i4))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               x24=0.5*(xic(i2)+xic(i4))
               y24=0.5*(yic(i2)+yic(i4))
               z24=0.5*(zic(i2)+zic(i4))
               x34=0.5*(xic(i3)+xic(i4))
               y34=0.5*(yic(i3)+yic(i4))
               z34=0.5*(zic(i3)+zic(i4))
               if(ivoronoi3d.eq.1) then
                  xa=yic(i2)
                  ya=yic(i2)
                  za=zic(i2)
                  xfac=1.0d+00
                  xb=xfac*(yic(i3)-xa)
                  yb=xfac*(yic(i3)-ya)
                  zb=xfac*(zic(i3)-za)
                  xd=xfac*(yic(i4)-xa)
                  yd=xfac*(yic(i4)-ya)
                  zd=xfac*(zic(i4)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds11=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds21=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds31=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv1=xl+xa
                  yv1=yl+ya
                  zv1=zl+za
                  xa=yic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(yic(i4)-xa)
                  yb=xfac*(yic(i4)-ya)
                  zb=xfac*(zic(i4)-za)
                  xd=xfac*(yic(i3)-xa)
                  yd=xfac*(yic(i3)-ya)
                  zd=xfac*(zic(i3)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds12=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds22=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds32=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv2=xl+xa
                  yv2=yl+ya
                  zv2=zl+za
                  xa=yic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(yic(i2)-xa)
                  yb=xfac*(yic(i2)-ya)
                  zb=xfac*(zic(i2)-za)
                  xd=xfac*(yic(i4)-xa)
                  yd=xfac*(yic(i4)-ya)
                  zd=xfac*(zic(i4)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds13=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds23=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds33=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv3=xl+xa
                  yv3=yl+ya
                  zv3=zl+za
                  xa=yic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(yic(i3)-xa)
                  yb=xfac*(yic(i3)-ya)
                  zb=xfac*(zic(i3)-za)
                  xd=xfac*(yic(i2)-xa)
                  yd=xfac*(yic(i2)-ya)
                  zd=xfac*(zic(i2)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds14=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds24=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds34=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv4=xl+xa
                  yv4=yl+ya
                  zv4=zl+za
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  xl1=yic(i1)
                  yl1=yic(i1)
                  zl1=zic(i1)
                  xl2=yic(i2)
                  yl2=yic(i2)
                  zl2=zic(i2)
                  xl3=yic(i3)
                  yl3=yic(i3)
                  zl3=zic(i3)
                  xl4=yic(i4)
                  yl4=yic(i4)
                  zl4=zic(i4)
                  ax1=  (yl3-yl2)*(zl4-zl2)-(zl3-zl2)*(yl4-yl2)
                  ay1=-((xl3-xl2)*(zl4-zl2)-(zl3-zl2)*(xl4-xl2))
                  az1=  (xl3-xl2)*(yl4-yl2)-(yl3-yl2)*(xl4-xl2)
                  ax2=  (yl4-yl1)*(zl3-zl1)-(zl4-zl1)*(yl3-yl1)
                  ay2=-((xl4-xl1)*(zl3-zl1)-(zl4-zl1)*(xl3-xl1))
                  az2=  (xl4-xl1)*(yl3-yl1)-(yl4-yl1)*(xl3-xl1)
                  ax3=  (yl2-yl1)*(zl4-zl1)-(zl2-zl1)*(yl4-yl1)
                  ay3=-((xl2-xl1)*(zl4-zl1)-(zl2-zl1)*(xl4-xl1))
                  az3=  (xl2-xl1)*(yl4-yl1)-(yl2-yl1)*(xl4-xl1)
                  ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
                  ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
                  az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
                  voltet=-((xl4-xl1)*ax4+(yl4-yl1)*ay4+(zl4-zl1)*az4)
                  voltot=voltot+voltet
                  x234=(xl2+xl3+xl4)/3.0
                  y234=(yl2+yl3+yl4)/3.0
                  z234=(zl2+zl3+zl4)/3.0
                  x143=(xl1+xl4+xl3)/3.0
                  y143=(yl1+yl4+yl3)/3.0
                  z143=(zl1+zl4+zl3)/3.0
                  x124=(xl1+xl2+xl4)/3.0
                  y124=(yl1+yl2+yl4)/3.0
                  z124=(zl1+zl2+zl4)/3.0
                  x132=(xl1+xl3+xl2)/3.0
                  y132=(yl1+yl3+yl2)/3.0
                  z132=(zl1+zl3+zl2)/3.0
                  xtestmax=1.0e+20
                  xtest=xtestmax
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
                  qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/(xd*xn+yd*yn+zd*zn+
     *                  1.0d-30)
                  xvor=qvor2*xn+xl+xa
                  yvor=qvor2*yn+yl+ya
                  zvor=qvor2*zn+zl+za
                  distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
                  distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
                  distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
                  distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv3),(x12),(xv4),
C*****     *                    (yvor),(yv3),(y12),(yv4),
C*****     *                    (zvor),(zv3),(z12),(zv4)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv4),(x13),(xv2),
C*****     *                    (yvor),(yv4),(y13),(yv2),
C*****     *                    (zvor),(zv4),(z13),(zv2)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv2),(x14),(xv3),
C*****     *                    (yvor),(yv2),(y14),(yv3),
C*****     *                    (zvor),(zv2),(z14),(zv3)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv1),(x23),(xv4),
C*****     *                    (yvor),(yv1),(y23),(yv4),
C*****     *                    (zvor),(zv1),(z23),(zv4)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv3),(x24),(xv1),
C*****     *                    (yvor),(yv3),(y24),(yv1),
C*****     *                    (zvor),(zv3),(z24),(zv1)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv1),(x34),(xv2),
C*****     *                    (yvor),(yv1),(y34),(yv2),
C*****     *                    (zvor),(zv1),(z34),(zv2)
               elseif(ivoronoi3d.eq.2) then
                  xv1=(xic(i2)+xic(i3)+xic(i4))/3.0
                  yv1=(yic(i2)+yic(i3)+yic(i4))/3.0
                  zv1=(zic(i2)+zic(i3)+zic(i4))/3.0
                  xv2=(xic(i1)+xic(i3)+xic(i4))/3.0
                  yv2=(yic(i1)+yic(i3)+yic(i4))/3.0
                  zv2=(zic(i1)+zic(i3)+zic(i4))/3.0
                  xv3=(xic(i1)+xic(i2)+xic(i4))/3.0
                  yv3=(yic(i1)+yic(i2)+yic(i4))/3.0
                  zv3=(zic(i1)+zic(i2)+zic(i4))/3.0
                  xv4=(xic(i1)+xic(i2)+xic(i3))/3.0
                  yv4=(yic(i1)+yic(i2)+yic(i3))/3.0
                  zv4=(zic(i1)+zic(i2)+zic(i3))/3.0
                  xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
                  ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
                  zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
                  xvor=xm
                  yvor=ym
                  zvor=zm
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv3),(x12),(xv4),
C*****     *                    (yvor),(yv3),(y12),(yv4),
C*****     *                    (zvor),(zv3),(z12),(zv4)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv4),(x13),(xv2),
C*****     *                    (yvor),(yv4),(y13),(yv2),
C*****     *                    (zvor),(zv4),(z13),(zv2)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv2),(x14),(xv3),
C*****     *                    (yvor),(yv2),(y14),(yv3),
C*****     *                    (zvor),(zv2),(z14),(zv3)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv1),(x23),(xv4),
C*****     *                    (yvor),(yv1),(y23),(yv4),
C*****     *                    (zvor),(zv1),(z23),(zv4)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv3),(x24),(xv1),
C*****     *                    (yvor),(yv3),(y24),(yv1),
C*****     *                    (zvor),(zv3),(z24),(zv1)
C*****                  write(iunit,9000) iclr,4,
C*****     *                    (xvor),(xv1),(x34),(xv2),
C*****     *                    (yvor),(yv1),(y34),(yv2),
C*****     *                    (zvor),(zv1),(z34),(zv2)
C***** 9000             format(2i5,4(1x,1pe22.14e3),/,
C*****     *                       4(1x,1pe22.14e3),/,
C*****     *                       4(1x,1pe22.14e3))
               elseif(ivoronoi3d.eq.3) then
                  do i=1,4
                  if(jtet1(jtoff+i).ge.mbndry) then
                  i1=itet1(itoff+itetface1(1,i))
                  i2=itet1(itoff+itetface1(2,i))
                  i3=itet1(itoff+itetface1(3,i))
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  xm=(xic(i1)+xic(i2)+xic(i3))/3.0
                  ym=(yic(i1)+yic(i2)+yic(i3))/3.0
                  zm=(zic(i1)+zic(i2)+zic(i3))/3.0
                  xv=xm
                  yv=ym
                  zv=zm
                  xa=yic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0
                  xb=xfac*(yic(i2)-xa)
                  yb=xfac*(yic(i2)-ya)
                  zb=xfac*(zic(i2)-za)
                  xd=xfac*(yic(i3)-xa)
                  yd=xfac*(yic(i3)-ya)
                  zd=xfac*(zic(i3)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv=xl+xa
                  yv=yl+ya
                  zv=zl+za
                  x12=0.5*(xic(i1)+xic(i2))
                  y12=0.5*(yic(i1)+yic(i2))
                  z12=0.5*(zic(i1)+zic(i2))
                  x13=0.5*(xic(i1)+xic(i3))
                  y13=0.5*(yic(i1)+yic(i3))
                  z13=0.5*(zic(i1)+zic(i3))
                  x23=0.5*(xic(i2)+xic(i3))
                  y23=0.5*(yic(i2)+yic(i3))
                  z23=0.5*(zic(i2)+zic(i3))
C
                  xcen=xv
                  ycen=yv
                  zcen=zv
                  dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+2,2,
C*****     *                              xcen,x12,
C*****     *                              ycen,y12,
C*****     *                              zcen,z12
                  endif
                  dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+2,2,
C*****     *                              xcen,x13,
C*****     *                              ycen,y13,
C*****     *                              zcen,z13
                  endif
                  dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+2,2,
C*****     *                              xcen,x23,
C*****     *                              ycen,y23,
C*****     *                              zcen,z23
                  endif
                  xcen=xm
                  ycen=ym
                  zcen=zm
                  dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+3,2,
C*****     *                              xcen,x12,
C*****     *                              ycen,y12,
C*****     *                              zcen,z12
                  endif
                  dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+3,2,
C*****     *                              xcen,x13,
C*****     *                              ycen,y13,
C*****     *                              zcen,z13
                  endif
                  dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
                  if(dist.gt.disttest) then
C*****                     write(iunit,9010) maxclrelement+3,2,
C*****     *                              xcen,x23,
C*****     *                              ycen,y23,
C*****     *                              zcen,z23
                  endif
                  endif
                  enddo
               endif
            endif
            itoff=itoff+nelmnen(ifelmtet)
            jtoff=jtoff+nelmnef(ifelmtet)
         elseif(itettyp(ie).eq.ifelmhex) then
            if(nelements.gt.0.and.ie.eq.1) then
               maxclrelement=0
               do it=1,nelements
                  maxclrelement=max(maxclrelement,max(1,itetclr(it)))
               enddo
               do it=1,nelements
                  if(itetclr(it).le.0) then
                     itemp(it)=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     itemp(it)=maxclrelement
                  else
                     itemp(it)=itetclr(it)
                  endif
               enddo
            endif
            it=ie
            nef=nelmnef(ifelmhex)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nefcmo
                  jf=jtet1(jtoff+i)-mbndry-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nefcmo
                  jf=jtet1(jtoff+i)-nefcmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  j1=itet1(itoff+ihexface1(1,i))
                  j2=itet1(itoff+ihexface1(2,i))
                  j3=itet1(itoff+ihexface1(3,i))
                  j4=itet1(itoff+ihexface1(4,i))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  ntriinv=ntriinv+1
                  iclrinv(ntriinv)=iclr
                  itypinv(ntriinv)=itettyp(ie)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j1),yic(j1),zic(j1)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j2),yic(j2),zic(j2)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j3),yic(j3),zic(j3)
                  write(iunit,"(3(1pe22.14e3),', ')")
     *                  xic(j4),yic(j4),zic(j4)
C*****                  write(iunit,9000)
C*****     *               iclr,
C*****     *               4,
C*****     *               xic(j1),xic(j2),xic(j3),xic(j4),
C*****     *               yic(j1),yic(j2),yic(j3),yic(j4),
C*****     *               zic(j1),zic(j2),zic(j3),zic(j4)
                 endif
              enddo
              itoff=itoff+nelmnen(ifelmhex)
              jtoff=jtoff+nelmnef(ifelmhex)
         endif
         enddo
         write(iunit,"('                ]')")
         write(iunit,"('    }')")
         write(iunit,"('    ShapeHints {')")
         write(iunit,"('	vertexOrdering	COUNTERCLOCKWISE')")
         write(iunit,"('    }')")
         write(iunit,"('    IndexedFaceSet {')")
         write(iunit,"('	coordIndex	[ ')")
         invoff=-1
         do i=1,ntriinv
            if(itypinv(i).eq.ifelmlin) then
               icount=2
               i1=invoff+1
               i2=i1+1
               i5=-1
               invoff=invoff+icount
               write(iunit,"(3(i6,a2))")
     *            i1,', ',i2,', ',i5,', '
            elseif(itypinv(i).eq.ifelmtri .or.
     *             itypinv(i).eq.ifelmtet) then
               icount=3
               i1=invoff+1
               i2=i1+1
               i3=i2+1
               i5=-1
               invoff=invoff+icount
               write(iunit,"(4(i6,a2))")
     *            i1,', ',i2,', ',i3,', ',i5,', '
            elseif(itypinv(i).eq.ifelmqud .or.
     *             itypinv(i).eq.ifelmhex) then
               icount=4
               i1=invoff+1
               i2=i1+1
               i3=i2+1
               i4=i3+1
               i5=-1
               invoff=invoff+icount
               write(iunit,"(5(i6,a2))")
     *            i1,', ',i2,', ',i3,', ',i4,', ',i5,', '
            endif
         enddo
         write(iunit,"('                        ]')")
         write(iunit,"('	materialIndex	[ ')")
         do i=1,ntriinv,10
            write(iunit,"(10(i3,','))")
     *                   (iclrinv(j),j=i,min(i+9,ntriinv))
         enddo
         write(iunit,"('                        ]')")
         write(iunit,"('    }')")
         write(iunit,"('}')")
      endif
      call get_global('ihcycle',ihcycle,rout,cout,itype,icscode)
      if (icscode .eq. 0) then
C*****         write(iunit,"(a8,i10)") 'cycleno ',ihcycle
      endif
      call get_global('time',iout,
     *                time,cout,itype,icscode)
      if (icscode .eq. 0) then
C*****         write(iunit,"(a9,1pe14.5e3)") 'probtime ',time
      endif
      close(iunit)
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
