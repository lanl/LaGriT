      subroutine recon2d(cmoin,toldamage,lcheckaxy,epsilona)
C
CCCCCC
CCCCC
CCCCC
CC PURPOSE   perform delaunay or geometric edge swapping of
C            a 2D mesh object
C            criterion based on value of ivoronoi (1 says delaunay)
C                                                 (-2 says geometric)
c                                                 ( 2 says user function)
c
C  INPUT  cmoin name of mesh object
C  OUPUT  none
C
C$Log: recon2d.f,v $
CRevision 2.00  2007/11/09 20:04:00  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.27   08 Feb 2006 14:35:36   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.26   15 May 2001 14:48:54   kuprat
CPVCS    We now obtain the parameters TOLDAMAGE, LCHECKAXY, EPSILONA
CPVCS    on the call line.  If LCHECKAXY is .true., we check that
CPVCS    the xy-projected areas of the new triangles would be greater
CPVCS    than EPSILONA.
CPVCS    
CPVCS       Rev 1.25   04 May 2001 16:36:08   dcg
CPVCS    remove test on areas of triangles before and
CPVCS    after flip - testdamage should be sufficient
CPVCS
CPVCS       Rev 1.24   Thu Apr 06 13:45:46 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.23   Wed Nov 10 14:58:20 1999   dcg
CPVCS    make xmin,xmax,ymin,ymax,zmin,zmax local variables
CPVCS
CPVCS       Rev 1.22   Mon May 10 11:12:18 1999   dcg
CPVCS    print number of flips
CPVCS
CPVCS       Rev 1.21   Thu Apr 29 11:10:28 1999   jtg
CPVCS    duplicate declaration of mbndry removed
CPVCS
CPVCS       Rev 1.20   Thu Apr 29 09:48:30 1999   dcg
CPVCS    fix error with ivoronoi=-2 (itetoff not set for prospective
CPVCS    new tets)
CPVCS
CPVCS       Rev 1.19   Tue Feb 02 11:10:26 1999   dcg
CPVCS    use cmo.h so that icmoget can be passed to testdamage
CPVCS    this meant using itetoff and jtetoff to access itet and jtet info
CPVCS    add beginning comments
C
      implicit none
C
      include 'chydro.h'
      include 'consts.h'
      include 'cmo.h'

C arguments
      character*(*) cmoin
      real*8  toldamage
      logical lcheckaxy
      real*8  epsilona
C
      integer ntri(10), mtri(10)

      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetoff(*), jtetoff(*)
 
      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipitflag, itflag)
      integer itflag(*)

      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itetnn(3,*), itetnn1(3,*), itetnn2(3,*)

      integer i1,i2,i3,j1,it,icscode,kdim,kpe,length,icmotype,ierror,
     *  ntets,npoints,ntetsmax,iter,i,jt,jf,n,iflag,m,irecon,
     *  lenout,nen,nef

      real*8  dsmax,xv,yv,zv,
     *  xa,ya,za,xb,yb,zb,xd,yd,zd,
     *  dotb3,dot3,rb3,ql,xl,yl,zl,ds1,ds2,ds3,ds,em,en,dsj,
     *  ds12,ds23,ds31,az1,az2
 
      pointer (ipxmegah, xmegah)
      pointer (ipxmegadet, xmegadet)
      pointer (ipxmegaerr, xmegaerr)
      real*8 xmegah(*), xmegadet(*), xmegaerr(*)

      logical flip
C
      character*132 logmess
      character*32 isubname
C
      integer itriface0(3), itriface1(3,3)
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
C
      integer itermax
      data itermax / 50 /
 
C     useful functions - should be just ahead of any other statement functions
      include 'statementfunctions.h'
C
C#######################################################################
C BEGIN begin
C Note tamiller - this routine has minimal error checking
C
      isubname='recon2d'
      icmoget=1
      cmo=cmoin
C
      call cmo_get_info('idebug',cmo,
     *                idebug,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
 
      call cmo_get_info('ivoronoi',cmo,
     *                ivoronoi,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
C     check for 0 elements
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      if (ntets .le. 0) then
         write(logmess,'(a)') 
     *   'WARNING Recon2d Early Exit: 0 elements'
         call writloga('default',1,logmess,1,ierror)
         ierror = -1
         goto 9999
      endif

      if(abs(ivoronoi).eq.2) then
         ntetsmax=ntets
         ntetsmax=max(ntetsmax,ntets+10)
         call cmo_set_info('nelements',cmo,ntetsmax,1,1,ierror)
         call cmo_newlen(cmo,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call mega_hessian()
         call mega_error()
         call mmfindbk('megah',cmo,ipxmegah,lenout,icscode)
         call mmfindbk('megadet',cmo,ipxmegadet,lenout,icscode)
         call mmfindbk('megaerr',cmo,ipxmegaerr,lenout,icscode)
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
C
C     ******************************************************************
C
C     Get the parents for each node.
C
      length=npoints
      call mmgetblk("iparent",isubname,ipiparent,length,2,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,2,icscode)
      length=nen*ntets
      call mmgetblk("itetnn",isubname,ipitetnn,length,2,icscode)
      length=nef*ntets
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,2,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,2,icscode)
C
      kdim=3
      kpe=kdim
      dsmax=0.0d-00
      do it=1,ntets
         i1=itet1(itetoff(it)+1)
         i2=itet1(itetoff(it)+2)
         i3=itet1(itetoff(it)+3)
         ds12=((xic(i2)-xic(i1))**2+
     *         (yic(i2)-yic(i1))**2+
     *         (zic(i2)-zic(i1))**2)
         ds23=((xic(i3)-xic(i2))**2+
     *         (yic(i3)-yic(i2))**2+
     *         (zic(i3)-zic(i2))**2)
         ds31=((xic(i1)-xic(i3))**2+
     *         (yic(i1)-yic(i3))**2+
     *         (zic(i1)-zic(i3))**2)
         dsmax=max(dsmax,ds12,ds23,ds31)
      enddo
      dsmax=sqrt(dsmax)
C
      iter=0
 10   continue
      iter=iter+1
      if(itermax.gt.0) then
         if(iter.gt.itermax) then
            write(logmess,'(a,i10)') "Recon2d max iterations: ",iter
            call writloga('default',0,logmess,0,ierror)
            if (idebug.gt.1) call mmverify()
            goto 9999
         endif
      endif
      do i=1,ntets
         itflag(i)=0
      enddo
      irecon=0
      do it=1,ntets
         if(itflag(it).eq.0) then
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            i3=itet1(itetoff(it)+3)
            xa=xic(i1)
            ya=yic(i1)
            za=zic(i1)
            xb=(xic(i2)-xa)
            yb=(yic(i2)-ya)
            zb=(zic(i2)-za)
            xd=(xic(i3)-xa)
            yd=(yic(i3)-ya)
            zd=(zic(i3)-za)
            dotb3=xb*xd+yb*yd+zb*zd
            dot3=dotb3/(xd*xd+yd*yd+zd*zd)
            rb3=1.0/(xb*xb+yb*yb+zb*zb)
            ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
            xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
            yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
            zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
            ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
            ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
            ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
            ds=min(ds1,ds2,ds3)
            xv=xl+xa
            yv=yl+ya
            zv=zl+za
            do i=1,3
               if(jtet1(jtetoff(it)+i).gt.0.and.
     *            jtet1(jtetoff(it)+i).lt.mbndry) then
                  i1=itet1(itetoff(it)+itriface1(3,i))
                  i2=itet1(itetoff(it)+itriface1(1,i))
                  i3=itet1(itetoff(it)+itriface1(2,i))
                  jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                  jf=jtet1(jtetoff(it)+i)-nef*(jt-1)
                  if(itflag(jt).eq.0 .and.
     *               itetclr(it).eq.itetclr(jt)) then
                     j1=itet1(itetoff(jt)+jf)
                     iflag=0
                     if(ivoronoi.eq.1) then
                        dsj=sqrt((xic(j1)-xv)*(xic(j1)-xv)+
     *                           (yic(j1)-yv)*(yic(j1)-yv)+
     *                           (zic(j1)-zv)*(zic(j1)-zv))
                        if((dsj-ds).lt.-1.0d-06*dsmax) iflag=1
                     elseif(abs(ivoronoi).eq.2) then
                        n=2
                        ntri(1)=it
                        ntri(2)=jt
                        m=2
                        mtri(1)=ntets+1
                        mtri(2)=ntets+2
                        itetoff(ntets+1)=ntets*3
                        itetoff(ntets+2)=(ntets+1)*3
                        itet1(itetoff(mtri(1))+1)=i1
                        itet1(itetoff(mtri(1))+2)=i2
                        itet1(itetoff(mtri(1))+3)=j1
                        itet1(itetoff(mtri(2))+1)=i1
                        itet1(itetoff(mtri(2))+2)=j1
                        itet1(itetoff(mtri(2))+3)=i3
                        xmegadet(it)=-1.0
                        xmegaerr(it)=-1.0
                        xmegadet(jt)=-1.0
                        xmegaerr(jt)=-1.0
                        call b2dnxm (n,ntri,en,m,mtri,em,
     +                               npoints,kdim,xic,yic,zic,xmegah,
     *                               ntets,kpe,itet,
     *                               xmegadet,xmegaerr,
     +                               flip)
                        if(flip) iflag=2
                     endif
                     if(iflag.gt.0) then
                        call testdamage(i2,i1,i3,j1,iflag,toldamage)
                        icmoget=0
                        if (lcheckaxy.and.iflag.gt.0) then
                           az1=0.5d0*dcrosz(xic(i1),yic(i1),zic(i1)
     &                        ,xic(j1),yic(j1),zic(j1),xic(i3),yic(i3)
     &                        ,zic(i3))
                           az2=0.5d0*dcrosz(xic(i3),yic(i3),zic(i3)
     &                        ,xic(j1),yic(j1),zic(j1),xic(i2),yic(i2)
     &                        ,zic(i2))
                      if (az1.le.epsilona.or.az2.le.epsilona) iflag=0
                        endif
                        if(iflag.gt.0) then
                              irecon=irecon+1
                              itflag(it)=1
                              itflag(jt)=1
                              itet1(itetoff(it)+1)=i1
                              itet1(itetoff(it)+2)=i2
                              itet1(itetoff(it)+3)=j1
                              jtet1(jtetoff(it)+1)=-1
                              jtet1(jtetoff(it)+2)=-1
                              jtet1(jtetoff(it)+3)=-1
                              itet1(itetoff(jt)+1)=i1
                              itet1(itetoff(jt)+2)=j1
                              itet1(itetoff(jt)+3)=i3
                              jtet1(jtetoff(jt)+1)=-1
                              jtet1(jtetoff(jt)+2)=-1
                              jtet1(jtetoff(jt)+3)=-1
                              goto 20
                        endif
                     endif
                  endif
               endif
            enddo
            if (idebug.gt.2) call mmverify()
 20         continue
         endif
      enddo
      if(irecon.ne.0) then
         do it=1,ntets
            do i=1,3
               itetnn(i,it)=iparent(itet1(itetoff(it)+i))
            enddo
         enddo
         do it=1,ntets
            do i=1,3
               itetnn1(i,it)=-1
               itetnn2(i,it)=-1
            enddo
         enddo

         call geniee(itetnn,itetnn1,itetnn2,3,3,
     *               ntets,npoints,2,npoints,ntets)
C
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,length,icmotype,ierror)
         do it=1,ntets
            do i=1,3
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  if(itetclr(it).eq.itetclr(itetnn1(i,it))) then
                     jtet1(jtetoff(it)+i)=3*(itetnn1(i,it)-1)+
     *                    itetnn2(i,it)
                  else
                     jtet1(jtetoff(it)+i)=mbndry+3*(itetnn1(i,it)-1)+
     *                    itetnn2(i,it)
                  endif
               else
                  jtet1(jtetoff(it)+i)=mbndry
               endif
            enddo
         enddo
         write(logmess,'(a,i10,a,i10)') 'Recon2d: iteration number= ',
     *       iter,' number of flips= ',irecon
         call writloga('default',0,logmess,0,ierror)
         if (idebug.gt.1) call mmverify()
         goto 10
      endif
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      if(abs(ivoronoi).eq.2) then
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megadet/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megadet;finish'
 9990    format (a,a,a)
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megaerr/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megaerr;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megah/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megah;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'mega2d/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'mega2d;finish'
         call dotaskx3d(logmess,ierror)
      endif
      return
      end
