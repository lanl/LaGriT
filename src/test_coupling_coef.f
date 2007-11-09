C---------------------------------------------------------------------------
 
      subroutine ident_coupling_coef(ierr)
 
C---------------------------------------------------------------------------
 
c
c#######################################################################
c
c     purpose -
c
C  This command test all tets on the boundary for negative coupling
C  coupling coefficients
C  It adds attributes to the mesh object:
C
C   neg_coup_coeff, type=VDOUBLE, length=ntets
C              of coupling coefficient
C              1.0 if it has positive coupling coefficients
C              < 1.0 --> implies "negative" coupling coeff.  The closer to
C                0.0, the worse it is.
c     input arguments -
c
c     output arguments -
c
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
 
      implicit none
C
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
 
 
C
      integer ierr, ilen,itype
C
C ######################################################################
C
      character*40 cmo
C
      integer npoints, ntets, nen, nef, nsdtopo, nsdgeom,mbndry
      integer p1,p2,p3,p4
C
 
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer isn1(1000000), itp1(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(10000000), itetoff(10000000)
C
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitetclr, itetclr)
      pointer (ipjtetoff, jtetoff)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
 
      integer itetclr(1000000), jtetoff(1000000)
      integer itet(10000000),  jtet(10000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
 
C
C
C#######################################################################
      character*32 isubname
      character*132 logmess
      character*132 ibuff
      integer i,it
 
      real*8 centx,centy,centz,radius2,dist
 
      pointer(ipncc,ncc)
      real*8 ncc(10000000)
 
      include 'chydro.h'
      include 'local_element.h'
 
 
C
C#######################################################################
 
      real*8 dist2, det2, det3, a11,a12,a13,a21,a22,a23,a31,a32,a33
      det3(a11,a12,a13,a21,a22,a23,a31,a32,a33) =
     *  ((a31*((a12*a23)-(a22*a13)))-(a32*((a11*a23)-(a13*a21)))+
     *  (a33*((a11*a22)- (a21*a12))))
 
      det2(a11,a12,a21,a22) = (a11*a22 - a21*a12)
 
      dist2(a11,a12,a13, a21,a22,a23)  =
     *  (a11-a21)**2 + (a12-a22)**2 + (a13-a23)**2
 
C
C#######################################################################
      isubname='test_coupling_coeff_local'
      ierr=0
C     Get mesh object.
C
      call cmo_get_name (cmo,ierr)
C
      if(ierr.ne.0) then
        write(logmess,'(a)')
     *          'test_coupling_coeff_local found bad mesh object'
        call writloga('default',0,logmess,0,ierr)
        goto 9999
      endif
C
C     ******************************************************************
C     Get mesh object data.
C
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,
     *                   mbndry,ilen, itype, ierr)
      call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,ilen,itype,ierr)
      call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,ilen,itype,ierr)
      call cmo_get_info('nodes_per_element',cmo,
     *                        nen,ilen,itype,ierr)
      call cmo_get_info('faces_per_element',cmo,
     *                        nef,ilen,itype,ierr)
      call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
 
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierr)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierr)
      call cmo_get_info('jtet',  cmo,ipjtet,ilen,itype,ierr)
 
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
 
 
      ibuff='cmo/addatt//neg_coup_coeff/VDOUBLE/' //
     *         'scalar/nelements/linear/temporary/gx/0' //
     *         ' ; finish'
 
      call dotaskx3d(ibuff,ierr)
      call cmo_get_info('neg_coup_coeff',  cmo,ipncc,ilen,itype,ierr)
 
C#######################################################################
 
c        ntets=number of tet
c        nelmnef= face number on tet
c        mbndry=(ntets*nelmnef) + 1
c        jtet1= boundary flag
c        itp1=boundary flag(0,10,2,12,etc)
c        ielmface0=node numbers on face
 
      do it=1,ntets
c        check to see that we are dealing with tets!
         if (nelmnef(itettyp(it)).ne.4) then
            call x3d_error(isubname,'assumes a tetrahedral mesh!')
         endif
 
 
         ncc(it) = 1.0
 
         do i=1,nelmnef(itettyp(it))
            if (jtet(jtetoff(it)+i).ge.mbndry) then
 
               p1 = itet(itetoff(it)+ielmface1(1,i,itettyp(it)))
               p2 = itet(itetoff(it)+ielmface1(2,i,itettyp(it)))
               p3 = itet(itetoff(it)+ielmface1(3,i,itettyp(it)))
 
               call compute_minsphere(xic(p1),yic(p1),zic(p1),
     *              xic(p2),yic(p2),zic(p2),
     *              xic(p3),yic(p3),zic(p3),
     *              centx, centy, centz, radius2)
 
               p4 = itet(itetoff(it)+ielmface1(4,i,itettyp(it)))
 
               dist = dist2(xic(p4),yic(p4),zic(p4),
     *              centx,centy,centz)
 
               if (dist.le.radius2) then
                  ncc(it) = dist/radius2
               endif
            endif
         enddo
      enddo
 
      call cmo_set_info('neg_coup_coeff',  cmo,ipncc,ilen,itype,ierr)
 
 9999 return
      end
 
 
 
 
C---------------------------------------------------------------------------
 
      subroutine test_coupling_coef(num_neg)
 
C---------------------------------------------------------------------------
C
c
c#######################################################################
c
c     purpose -
c
C  This command test all edges of all boundary faces for negative
C  coupling coefficients
C  It adds attributes to the mesh object:
C   num_neg_coup_coeff, type=INT, length=scalar -number of negative
C              coupling coefficients
C   neg_coup_coeff, type=VDOUBLE, length=num_neg_coup_coeff - value
C              of coupling coefficient
C   ietet_aij, type=VINT, length=num_neg_coup_coeff ,rank=vector -
C              (tet number, face number, edge number)
C              for each negative coupling coefficient
c
c     input arguments -
c
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
c
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c     change history -
C
C $Log:   /pvcs.config/t3d/src/test_coupling_coef.f_a  $
CPVCS    
CPVCS       Rev 1.9   Thu Apr 06 14:23:16 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.8   03 Apr 2000 18:45:56   gable
CPVCS    Changed some comments.
CPVCS
CPVCS       Rev 1.7   Fri Jul 24 14:59:12 1998   dcg
CPVCS    get rid of unused variables
CPVCS
CPVCS       Rev 1.6   Fri Jul 24 14:50:56 1998   dcg
CPVCS    only check for negative coupling coefficients that are
CPVCS    sufficiently large
CPVCS
CPVCS       Rev 1.5   Thu Mar 19 16:30:40 1998   murphy
CPVCS    Added negative_aij/identify code.
CPVCS
CPVCS       Rev 1.4   Wed Mar 18 11:59:08 1998   murphy
CPVCS    Added code for an improved positive coupling coefficients test.
CPVCS    Also, added code that simply identifies the offending tets and
CPVCS    places them in an attribute.
CPVCS
CPVCS       Rev 1.3   Thu Feb 26 12:15:54 1998   dcg
CPVCS    modify to fit into coupling_coef_wrapper code
CPVCS
CPVCS       Rev 1.2   Tue Jan 27 11:54:04 1998   dcg
CPVCS    compare to matbld3d_stor output
C
C
      implicit none
      include 'local_element.h'
C
 
c     Default usage:
c
c
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
c
      integer k,i,it
      pointer (ipncc,ncc)
      real*8 ncc(10000000)
      pointer (ipietet,ietet)
      integer ietet(3,10000000)
      real*8 dist,dist2,radius2,centx,centy,centz,
     *    a11,a12,a13,a21,a22,a23
      character*32 isubname
      character*132 ibuff
C
      integer ierror, ilen,itype
C
C ######################################################################
C
      character*40 cmo
C
      integer npoints, ntets, nen, nef, nsdtopo, nsdgeom,mbndry
      integer p1,p2,p3,p4
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer isn1(1000000), itp1(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(10000000), itetoff(10000000)
C
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitetclr, itetclr)
      pointer (ipjtetoff, jtetoff)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
 
      integer itetclr(1000000), jtetoff(1000000)
      integer itet(10000000),  jtet(10000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
      pointer (ipiparent, iparent)
      integer iparent(1000000)
      real*8 xcoef,ycoef,zcoef,xcoef1,ycoef1,zcoef1,sumcoef,d,
     *   xij,yij,zij,cmag,coef,mostnegcoef,eps
      integer iff,itt,idir,jt,nf,nfstart,ie,i1,i2,q1,q2,ierfnd,
     *  itnew,ifnew,i3,i4,j,l,q3,q4,itstart,nfskip,ittyp,r1,r2,r3,
     *  num_neg,ll,idebug,itworst,ifworst,iedge
      character*8 cdefault
C
      dist2(a11,a12,a13,a21,a22,a23)  =
     *  (a11-a21)**2 + (a12-a22)**2 + (a13-a23)**2
C
      isubname='test_coupling_coef'
      cdefault='default'
      ierfnd=0
C
C     ******************************************************************
C     Get mesh object data.
C
      call cmo_get_name(cmo,ierror)
      call get_epsilon('epsilonl',eps)
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
C  see if attribute already exists
      call mmfindbk('ietet_aij',cmo,ipietet,ilen,ierror)
      if(ierror.ne.0) then
         ibuff='cmo/addatt//num_neg_coup_coeff/INT/' //
     *         'scalar/scalar/linear/temporary/x/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,ierror)
         call cmo_set_info('num_neg_coup_coeff',cmo,ntets,1,1,ierror)
         ibuff='cmo/addatt//neg_coup_coeff/VDOUBLE/' //
     *         'scalar/num_neg_coup_coeff/linear/temporary/x/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,ierror)
         ibuff='cmo/addatt//ietet_aij/VINT/' //
     *         'vector/num_neg_coup_coeff/linear/temporary/x/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,ierror)
      else
         call cmo_set_info('num_neg_coup_coeff',cmo,ntets,1,1,ierror)
         call cmo_newlen(cmo,ierror)
      endif
      call cmo_get_info('mbndry',cmo,
     *                   mbndry,ilen, itype, ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,ilen,itype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,ilen,itype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                        nen,ilen,itype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                        nef,ilen,itype,ierror)
      call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,itype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,itype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff', cmo, ipjtetoff,ilen,itype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet', cmo, ipjtet, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
 
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierror)
      call cmo_get_info('jtet',  cmo,ipjtet,ilen,itype,ierror)
 
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierror)
      call cmo_get_info('ietet_aij',  cmo,ipietet,ilen,itype,ierror)
      call cmo_get_info('neg_coup_coeff',  cmo,ipncc,ilen,itype,ierror)
      call cmo_get_info('idebug',cmo,idebug,ilen,itype,ierror)
C
C  put parent nodes in iparent
C
      call mmgetblk('iparent',isubname,ipiparent,npoints,1,ierror)
      call unpackpc(npoints,itp1,isn1,iparent)
 
 
C#######################################################################
 
c        ntets=number of tet
c        nelmnef= face number on tet
c        mbndry=(ntets*nelmnef) + 1
c        jtet1= boundary flag
c        itp1=boundary flag(0,10,2,12,etc)
c        ielmface0=node numbers on face
c        num_neg is number of negative coupling coefficients
      do it=1,ntets
c        check to see that we are dealing with tets!
         if (nelmnef(itettyp(it)).ne.4) then
            call x3d_error(isubname,'assumes a tetrahedral mesh!')
         endif
         ncc(it) = 1.0
         ittyp=itettyp(it)
c        check if element face is on an external or internal boundary.
         do i=1,nelmnef(itettyp(it))
            if (jtet(jtetoff(it)+i).ge.mbndry) then
               p1 = iparent(itet(itetoff(it)+
     *                   ielmface1(1,i,itettyp(it))))
               p2 = iparent(itet(itetoff(it)+
     *                   ielmface1(2,i,itettyp(it))))
               p3 = iparent(itet(itetoff(it)+
     *                   ielmface1(3,i,itettyp(it))))
               p4 = iparent(itet(itetoff(it)+
     *                   i))
 
               call compute_minsphere(xic(p1),yic(p1),zic(p1),
     *                                xic(p2),yic(p2),zic(p2),
     *                                xic(p3),yic(p3),zic(p3),
     *                                centx, centy, centz, radius2)
 
c
c replace with kd-tree if this a bottleneck.
c
 
               do k=1,npoints
                  dist = dist2(xic(k),yic(k),zic(k),
     *                         centx,centy,centz)
                  if (dist.le.radius2) then
                     if (ncc(it).gt.(dist/radius2)) then
C
C  found point inside minimun sphere defined by face
C  loop around all three edges of this face and
C  calculate coupling between the pairs of nodes defining
C  each edge of the face
C  If the coupling coefficient is negative
C  Attempt to refine the 'bad' edge
C
                        do l=1,ielmface0(i,ittyp)
                           xcoef=0.0
                           ycoef=0.0
                           zcoef=0.0
                           q4=p4
                           if (l.eq.1) then
                              q1=p1
                              q2=p2
                              q3=p3
                           elseif (l.eq.2) then
                              q1=p2
                              q2=p3
                              q3=p1
                           else
                              q1=p3
                              q2=p1
                              q3=p2
                           endif
C
C  Check if this edge is already on the list
C
                           if(num_neg.ne.0) then
                              do ll=1,num_neg
                                itt=ietet(1,ll)
                                iedge=ietet(3,ll)
                                r1=iparent(itet(itetoff(itt)
     *                            +ielmedge1(1,iedge,ittyp)))
                                r2=iparent(itet(itetoff(itt)
     *                            +ielmedge1(2,iedge,ittyp)))
                                if((q1.eq.r1.and.q2.eq.r2).or.
     *                             (q2.eq.r1.and.q1.eq.r2))
     *                             go to   50
                              enddo
                           endif
C
C  First get coupling coefficient contribution for this tet
C
                           call coefficient(xic(q1),yic(q1),zic(q1),
     *                                      xic(q2),yic(q2),zic(q2),
     *                                      xic(q3),yic(q3),zic(q3),
     *                                      xic(q4),yic(q4),zic(q4),
     *                                      xcoef,ycoef,zcoef)
                           xij=xic(q2)-xic(q1)
                           yij=yic(q2)-yic(q1)
                           zij=zic(q2)-zic(q1)
                           d=sqrt(xij**2+yij**2+zij**2)
                           mostnegcoef=
     *                        (xij*xcoef+yij*ycoef+zij*zcoef)/d
c
C  Loop around edge and accumulate coeffient for all tets
C  Sharing this edge
                          itstart=it
                          nfstart=i
                          itworst=it
                          ifworst=i
                          itnew=0
                          ifnew=0
                          itt=it
                          iff=i
                          idir=0
 10                       jt=jtet(jtetoff(itt)+iff)
                          ittyp=itettyp(itt)
C  check for hitting external boundary if so
C  go back to starting tet, find other face that contains tet
C  make sure we have checked both ways.
                          if(jt.eq.mbndry) then
                             idir=idir+1
                             if(idir.eq.2) go to 30
                             itt=itstart
                             do nf =1,nelmnef(ittyp)
                               if(nf.ne.nfstart) then
                                 do j=1,ielmface0(nf,ittyp)
                                   ie=ielmface2(j,nf,ittyp)
                                   i1=itet(itetoff(itt)+
     *                                ielmedge1(1,ie,ittyp))
                                   i2=itet(itetoff(itt)+
     *                                ielmedge1(2,ie,ittyp))
                                   i1=iparent(i1)
                                   i2=iparent(i2)
                                   if(i1.eq.q1.and.i2.eq.q2.or.
     *                                i2.eq.q1.and.i1.eq.q2) then
                                      iff=nf
                                      nfstart=nf
                                      go to 10
                                   endif
                                 enddo
                               endif
                             enddo
                          elseif(jt.gt.mbndry) then
                             jt=jt-mbndry
                          endif
C look for matching edge
C on other face of tet
  15                      itnew=1+(jt-1)/nef
                          nfskip=jt-nef*(itnew-1)
                          itt=itnew
                          do nf =1,nelmnef(ittyp)
                            if(nf.ne.nfskip) then
                               i1=iparent(itet(itetoff(itt)+
     *                             ielmface1(1,nf,ittyp)))
                               i2=iparent(itet(itetoff(itt)+
     *                             ielmface1(2,nf,ittyp)))
                               i3=iparent(itet(itetoff(itt)+
     *                             ielmface1(3,nf,ittyp)))
                               i4=iparent(itet(itetoff(itt)+nf))
                               do j=1,ielmface0(nf,ittyp)
                                  if (j.eq.1) then
                                    r1=i1
                                    r2=i2
                                    r3=i3
                                  elseif (j.eq.2) then
                                    r1=i2
                                    r2=i3
                                    r3=i1
                                  else
                                    r1=i3
                                    r2=i1
                                    r3=i2
                                  endif
                                  if(r1.eq.q1.and.r2.eq.q2.or.
     *                              r2.eq.q1.and.r1.eq.q2) then
                                    ifnew=nf
                                    go to 18
                                  endif
                               enddo
                            endif
                          enddo
                          ierfnd=1
                          go to 100
   18                     if (itnew.ne.itstart) then
                             i4=itet(itetoff(itt)+ifnew)
                             call coefficient(xic(r1),yic(r1),zic(r1),
     *                                        xic(r2),yic(r2),zic(r2),
     *                                        xic(r3),yic(r3),zic(r3),
     *                                        xic(i4),yic(i4),zic(i4),
     *                                        xcoef1,ycoef1,zcoef1)
                             coef=(xij*xcoef1+yij*ycoef1+zij*zcoef1)/d
                             if(r1.eq.q1)then
                                xcoef=xcoef+xcoef1
                                ycoef=ycoef+ycoef1
                                zcoef=zcoef+zcoef1
                             else
                                xcoef=xcoef-xcoef1
                                ycoef=ycoef-ycoef1
                                zcoef=zcoef-zcoef1
                                coef=-coef
                             endif
                             if(coef.lt.mostnegcoef) then
                                mostnegcoef=coef
                                itworst=itt
                                ifworst=ifnew
                             endif
                             iff=ifnew
                             go to 10
                          else
C   back to starting element
                             go to 30
                          endif
C
C  dot the components with the edge
C
  30                      cmag=sqrt(xcoef**2+ycoef**2+zcoef**2)
                          sumcoef=(xij*xcoef+yij*ycoef+zij*zcoef)/d
                          if(sumcoef.lt.-eps) then
                             num_neg=num_neg+1
                             ncc(num_neg) = sumcoef
                             if(idebug.ge.2) then
                               write(ibuff,35) it,q1,q2,sumcoef
  35                           format('for tet,  nodes: ',3i8,
     *                           ' coupling coefficient ',e14.6)
                               call writloga(cdefault,0,ibuff,0,
     *                           ierror)
                             endif
C
C  find local edge to store with tet number
                             do ll=1,6
                               r1=iparent(itet(itetoff(itworst)
     *                            +ielmedge1(1,ll,ittyp)))
                               r2=iparent(itet(itetoff(itworst)
     *                            +ielmedge1(2,ll,ittyp)))
                               if((q1.eq.r1 .and.
     *                             q2.eq.r2).or.
     *                            (q1.eq.r2 .and.
     *                             q2.eq.r1))  then
                                   ietet(1,num_neg)=itworst
                                   ietet(2,num_neg)=ifworst
                                   ietet(3,num_neg)=ll
                                endif
                             enddo
                          endif
 50                       continue
C  end loop on all edges of a face
                        enddo
C  end ifs on incircle tests
                     endif
                  endif
C  end loop on all points
               enddo
c  end if on boundary face
            endif
C  end loop over faces of tet
         enddo
C  end loop over all tets
      enddo
      call cmo_set_info('num_neg_coup_coeff',cmo,num_neg,1,1,ierror)
      call cmo_newlen(cmo,ierror)
 100  continue
      call mmrelprt(isubname,ierror)
 
 9999 return
      end
 
C---------------------------------------------------------------------------
 
      subroutine compute_minsphere(ax,ay,az, bx,by,bz, cx,cy,cz,
     *                             centx, centy, centz, radius2)
 
C---------------------------------------------------------------------------
 
C
C  This routine computes the center and the radius of the minimum radius
C  sphere enclosing the points defined by a,b, and c.   (Component wise,
C  known as ax,ay,az,bx,by,bz,cx,cy,cz).  This returns the variables
C  centx,centy,centz the x,y,z components of the circle center and the square
C  of the radius.
C
 
      REAL*8 ax,ay,az,bx,by,bz,cx,cy,cz,centx,centy,centz,radius2
      REAL*8 A(3,3), B(3), deta
 
C
C#######################################################################
C Mike Macros
 
      real*8 dist2, det2, det3, a11,a12,a13,a21,a22,a23,a31,a32,a33
 
      det3(a11,a12,a13,a21,a22,a23,a31,a32,a33) =
     *  ((a31*((a12*a23)-(a22*a13)))-(a32*((a11*a23)-(a13*a21)))+
     *  (a33*((a11*a22)- (a21*a12))))
 
      det2(a11,a12,a21,a22) = (a11*a22 - a21*a12)
 
      dist2(a11,a12,a13,a21,a22,a23)  =
     *  (a11-a21)**2 + (a12-a22)**2 + (a13-a23)**2
 
C
C#######################################################################
 
      A(1,1) =  cx-ax
      A(1,2) =  cy-ay
      A(1,3) =  cz-az
 
      A(2,1) =  bx - ax
      A(2,2) =  by - ay
      A(2,3) =  bz - az
 
      A(3,1) =  det2(A(1,2),A(1,3),A(2,2),A(2,3))
      A(3,2) = -det2(A(1,1),A(1,3),A(2,1),A(2,3))
      A(3,3) =  det2(A(1,1),A(1,2),A(2,1),A(2,2))
 
      B(1) = 0.5*(A(1,1)*(ax+cx)+A(1,2)*(ay+cy)+A(1,3)*(az+cz))
      B(2) = 0.5*(A(2,1)*(ax+bx)+A(2,2)*(ay+by)+A(2,3)*(az+bz))
      B(3) = (A(3,1)*ax) + (A(3,2)*ay) + (A(3,3)*az)
 
c       Now solve the system
      deta = det3(A(1,1),A(1,2),A(1,3),
     *            A(2,1),A(2,2),A(2,3),
     *            A(3,1),A(3,2),A(3,3))
 
c     if (fabs(deta) < 0.00000001) {
c     /* Are we ever setting ourselves up for epsilon problems
c        with this test, baby  ! */
c       printf("ERROR: Singular matrix--Compute Triangle center.\n");
c       }
 
      centx = det3(B(1),A(1,2),A(1,3),
     *             B(2),A(2,2),A(2,3),
     *             B(3),A(3,2),A(3,3))/deta
 
      centy = det3(A(1,1),B(1),A(1,3),
     *             A(2,1),B(2),A(2,3),
     *             A(3,1),B(3),A(3,3))/deta
 
      centz = det3(A(1,1),A(1,2),B(1),
     *             A(2,1),A(2,2),B(2),
     *             A(3,1),A(3,2),B(3))/deta
 
      radius2 = dist2(ax,ay,az,centx,centy,centz)
 
      return
      end
