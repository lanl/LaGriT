C---------------------------------------------------------------------------
 
      subroutine pcc_test()
 
C---------------------------------------------------------------------------
 
c This subroutine loops through all of the faces on the boundary of the
c mesh and check to see if the "empty minimum diameter circle" criterion
c is satisfied.  If so, then the CMO will yield "positive coupling
c coefficients" when discretized using one of the matbld3d programs.  If
c the empty mincircle criterion is not satisfied, then the subroutine
c reports the offensive tetrahedra.  Further an attribute neg_coup_coeff
c is created for each tet.  The range is between 0 and 1.  A 1 signifies
c the tet is OK.  Anything less than 1 means the tet is bad.  The purpose
c of this attribute is so that the bad tetrahedra can be visually
c identified using GMV or some other visualization program.
C        $Log: pcc_test.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C  
CPVCS    
CPVCS       Rev 1.3   Wed Oct 13 11:03:44 1999   dcg
CPVCS    remove extra comma in write statement that ibm compiler didn't like
CPVCS
CPVCS       Rev 1.2   Mon Oct 04 11:37:04 1999   murphy
CPVCS    Added comments and a $Log
 
      implicit none
      include 'local_element.h'
c
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
 
      integer k,i,it
      pointer (ipncc,ncc)
      real*8 ncc(10000000)
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
      character* 132 logmess
 
C
      integer npoints, ntets, nen, nef, nsdtopo, nsdgeom,mbndry
      integer p1,p2,p3
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
      character*8 cglobal,cdefault
C
      dist2(a11,a12,a13,a21,a22,a23)  =
     *  (a11-a21)**2 + (a12-a22)**2 + (a13-a23)**2
C
      isubname='pcc_test'
      cglobal='global'
      cdefault='default'
C
C
C     ******************************************************************
C     Get mesh object data.
C
 
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
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
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
 
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierror)
      call cmo_get_info('jtet',  cmo,ipjtet,ilen,itype,ierror)
 
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierror)
 
      isubname='pcc_test'
 
      ibuff='cmo/addatt//neg_coup_coeff/VDOUBLE/' //
     *         'scalar/nelements/linear/temporary/gx/0' //
     *         ' ; finish'
 
      call dotaskx3d(ibuff,ierror)
      call cmo_get_info('neg_coup_coeff',  cmo,ipncc,ilen,itype,ierror)
 
C#######################################################################
 
c        ntets=number of tet
c        nelmnef= face number on tet
c        jtet1= boundary flag
c        itp1=boundary flag(0,10,2,12,etc)
c        ielmface0=node numbers on face
 
      write(logmess,'(a)')'Negative Coupling Coefficients indicated: '
      call writloga('default',1,logmess,0,ierror)
 
      do it=1,ntets
c        check to see that we are dealing with tets!
         if (nelmnef(itettyp(it)).ne.4) then
            call x3d_error(isubname,'assumes a tetrahedral mesh!')
         endif
 
         ncc(it) = 1.0
c        check if element face is on an external or internal boundary.
         do i=1,nelmnef(itettyp(it))
            if (jtet(jtetoff(it)+i).ge.mbndry) then
               p1 = itet(itetoff(it)+ielmface1(1,i,itettyp(it)))
               p2 = itet(itetoff(it)+ielmface1(2,i,itettyp(it)))
               p3 = itet(itetoff(it)+ielmface1(3,i,itettyp(it)))
 
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
                        ncc(it) = dist/radius2
                     endif
                  endif
               enddo
            endif
         enddo
         if (ncc(it).lt.0.999999999) then
           write(logmess,'("tet ",i10," has negative coeff ",e11.4)')
     *      it, -ncc(it)
            call writloga('default',1,logmess,0,ierror)
         endif
      enddo
 
      call cmo_set_info('neg_coup_coeff',  cmo,ipncc,ilen,itype,ierror)
 
 9999 return
      end
