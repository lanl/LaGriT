      subroutine offsetsurf(imsgin,xmsgin,cmsgin,msgtype,nwds,ier)
C
C########################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE OFFSETS A TRIANGULATED SURFACE OR A POLYLINE
C     IN THE DIRECTION OF ITS OUTWARD NORMAL OR IN CONSTANT DIRECTION.
C
C     USAGE -  offsetsurf / cmo_out / cmo_in / d
C                   offset a triangulated surface a distance d
C           -  offsetsurf / cmo_out / cmo_in / d / [xy,xz,yx,yz,zx,zy]
C                    offset a line cmo a distance d in a
C                    direction parallel to the specified plane
C                    and bisecting the vertex of each line segment.
C           -  offsetsurf / cmo_out / cmo_in / d / x y z
C                    offset a line cmo a distance d in the
C                    direction specified by the vector (x,y,z).
C
C          NOTE: There is no error checking for valid input,
C                collision detection which can invert triangles,
C                automatic detection of non-planar line cmo and
C                determination of offset direction in the plane
C                of two line segments, etc.
C
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C
C
C
C     CHANGE HISTORY -
C
C        $Log: offsetsurf.f,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   01 Sep 2006 13:44:32   gable
CPVCS    Added option of area weighted or angle weighted synthetic normal vectors.
CPVCS    
CPVCS       Rev 1.4   07 Jul 2006 08:49:26   gable
CPVCS    Added keepatt option. Modified error checks. Modified epsilon usage.
CPVCS    
CPVCS       Rev 1.3   30 Sep 2004 15:36:14   dcg
CPVCS    make epsln double precision
CPVCS
CPVCS       Rev 1.2   30 Sep 2004 14:17:02   gable
CPVCS    Added error checking for cases elements are degenerate and
CPVCS    angle weighted normal is undefined. Result will be incorrect
CPVCS    for degenerate cases but divide by zero error is avoided.
CPVCS
CPVCS       Rev 1.1   28 Mar 2000 14:09:22   dcg
CPVCS    remove include 'machine.h'
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 15:47:42 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   Sun Jun 07 13:02:08 1998   gable
CPVCS    Made a few changes to allow offsetsurf to work on cmo
CPVCS    that has line type elements.
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:56:14 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Thu Feb 15 11:20:04 1996   dcg
CPVCS    Initial revision.
CPVCS
C
C
c offsetsurf/new_cmo/old_cmo/dist
C########################################################################
C
      implicit none
C
      include "local_element.h"
C
      integer lenptr
      real*8 epsln
      integer nwds,ier
      parameter (lenptr=1000000)
      parameter (epsln=1.0d-16)
C
C########################################################################
C
      pointer (iptn, tn)
      pointer (ipxnorm, xnorm)
      pointer (ipynorm, ynorm)
      pointer (ipznorm, znorm)
      pointer (ipx_n_norm, x_n_norm)
      pointer (ipy_n_norm, y_n_norm)
      pointer (ipz_n_norm, z_n_norm)
C
      real*8 tn(3,*)
      real*8 xnorm(*),ynorm(*),znorm(*)
      real*8 x_n_norm(*),y_n_norm(*),z_n_norm(*)
C
      real*8 xmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
      character*132 logmess
      character*(*) cmsgin(nwds)
      character*32 isubname
      character*32 cmoin,cmoout
C
C     SET POINTERS FOR INCOMING cmo
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
C
      real*8 xic(*),yic(*),zic(*)
      integer imt1(*),itp1(*),itetclr(*),
     &        itettyp(*),itet(*),jtet(*),
     &        itetoff(*),jtetoff(*)
C
C     SET POINTERS FOR OUTGOING cmo
C
      pointer (ipimt1o, imt1o)
      pointer (ipitp1o, itp1o)
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
      pointer (ipitetclro, itetclro)
      pointer (ipitettypo, itettypo)
      pointer (ipiteto, iteto)
      pointer (ipjteto, jteto)
      pointer (ipitetoffo, itetoffo)
      pointer (ipjtetoffo, jtetoffo)
C
      real*8 xico(*),yico(*),zico(*)
      integer imt1o(*),itp1o(*),itetclro(*),
     &        itettypo(*),iteto(*),jteto(*),
     &        itetoffo(*),jtetoffo(*)
C
      real*8 dbarea,u1,v1,u2,v2,u3,v3,pi,xplane,yplane,zplane,
     *  x1,y1,z1,x2,y2,z2,x3,y3,z3,dcross,vx1,vy1,vz1,vx2,vy2,
     *  vz2,top,bot,ang1,ang2,ang3,anorm,d,scale1,scale2,scale3
      integer nnodes,ilen,icmotype,nelements,nsdgeom,
     *  nsdtopo,nen,nef,nee,mbndry,lenmm1,lenmm2,lenmm3,lenmm4,i,
     * itri,i1,i2,i3,inode,ics,icscode,i_count,if_keepatt,
     * lencmoname
      integer icharlnf 
C
C########################################################################
C
C     STATEMENT FUNCTION DBAREA GIVES DOUBLE THE AREA OF A TRIANGLE
C     ORDERED (COUNTERCLOCKWISE) 1,2,3 IN THE u-v PLANE. THIS MEANS
C     THAT FOR A TRIANGLE ORDERED 1,2,3 IN x-y-z SPACE, THE (R.H. RULE)
C     VECTOR NORMAL TO THIS TRIANGLE WITH MAGNITUDE EQUAL TO DOUBLE THE
C     AREA IS GIVEN BY ( dbarea(y1,z1,y2,z2,y3,z3),
C                        dbarea(z1,x1,z2,x2,z3,x3),
C                        dbarea(x1,y1,x2,y2,x3,y3) ).
 
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
C
C########################################################################
C     ier - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ier = 0
 
      pi=3.14159265385
      cmoin = '-cmo-'
      cmoout = '-none-'
C
C     *******************************************************************
C     SET THE MEMORY MANAGED PARTITION NAME.
C
      isubname='offsetsurf'
C
      cmoin=cmsgin(3)
      call cmo_exist(cmoin,ier)
      if(ier.eq.0) then
         cmoout = cmsgin(2)
      else
         write(logmess,'(a)')
     &        'Error in subroutine offsetsurf: cmo does not exist'
         call writloga('default',0,logmess,0,ier)
         ier = 1
         go to 9999
      endif
C
C     *******************************************************************
C     GET INCOMING cmo INFO.
C
         call cmo_get_info('nnodes',cmoin,nnodes,ilen,icmotype,ier)
         call cmo_get_info('nelements',cmoin,
     &                      nelements,ilen,icmotype,ier)
         call cmo_get_info('ndimensions_geom',cmoin,
     &                      nsdgeom,ilen,icmotype,ier)
         call cmo_get_info('ndimensions_topo',cmoin,
     &                      nsdtopo,ilen,icmotype,ier)
         call cmo_get_info('nodes_per_element',cmoin,
     &                      nen,ilen,icmotype,ier)
         call cmo_get_info('faces_per_element',cmoin,
     &                      nef,ilen,icmotype,ier)
         call cmo_get_info('edges_per_element',cmoin,
     &                      nee,ilen,icmotype,ier)
         call cmo_get_info('itp1',cmoin,ipitp1,ilen,icmotype,ier)
         call cmo_get_info('imt1',cmoin,ipimt1,ilen,icmotype,ier)
         call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ier)
         call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ier)
         call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ier)
         call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,icmotype
     &                     ,ier)
         call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,icmotype
     &                      ,ier)
         call cmo_get_info('itetoff',cmoin,ipitetoff,ilen,icmotype
     &                      ,ier)
         call cmo_get_info('jtetoff',cmoin,ipjtetoff,ilen,icmotype
     &                      ,ier)
         call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ier)
         call cmo_get_info('jtet',cmoin,ipjtet,ilen,icmotype,ier)
C
C     *******************************************************************
C     CREATE THE NEW cmo.
C
      call cmo_exist(cmoout,ier)
C
C     ier.eq.0 MEANS THAT THE cmo ALREADY EXISTS.
C
      if(ier.eq.0) call cmo_release(cmoout,ier)
C
      call cmo_create(cmoout,ier)
C
      call cmo_set_info('nnodes',cmoout,nnodes,1,1,ier)
      call cmo_set_info('nelements',cmoout,nelements,1,1,ier)
      call cmo_set_info('ndimensions_topo',cmoout,nsdtopo,1,1,ier)
      call cmo_set_info('ndimensions_geom',cmoout,nsdgeom,1,1,ier)
      call cmo_set_info('nodes_per_element',cmoout,nen,1,1,ier)
      call cmo_set_info('faces_per_element',cmoout,nef,1,1,ier)
      call cmo_set_info('edges_per_element',cmoout,nee,1,1,ier)
C
      call cmo_newlen(cmoout,ier)
C
      call cmo_get_info('mbndry',cmoout,mbndry,ilen,icmotype,ier)
C
      call cmo_get_info('imt1',cmoout,ipimt1o,ilen,icmotype,ier)
      call cmo_get_info('itp1',cmoout,ipitp1o,ilen,icmotype,ier)
      call cmo_get_info('xic',cmoout,ipxico,ilen,icmotype,ier)
      call cmo_get_info('yic',cmoout,ipyico,ilen,icmotype,ier)
      call cmo_get_info('zic',cmoout,ipzico,ilen,icmotype,ier)
      call cmo_get_info('itetclr',cmoout,ipitetclro,ilen,
     &                   icmotype,ier)
      call cmo_get_info('itettyp',cmoout,ipitettypo,ilen,
     &                   icmotype,ier)
      call cmo_get_info('itetoff',cmoout,ipitetoffo,ilen,
     &                   icmotype,ier)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffo,ilen,
     &                   icmotype,ier)
      call cmo_get_info('itet',cmoout,ipiteto,ilen,icmotype,ier)
      call cmo_get_info('jtet',cmoout,ipjteto,ilen,icmotype,ier)
C
C     *******************************************************************
      if_keepatt = 0
      if((nwds .eq. 5).and.(msgtype(5) .eq. 3))then
      if(cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'keepatt')then
C
C        Compute node angle weighted normals and keep the
C        vector components in three scalar attributes,
C        x_n_norm, y_n_norm, z_n_norm
C
C        These will be kept in both cmoin and cmoout
C
         if_keepatt = 1
      elseif(cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'keep_angle')then
C
C        Compute node angle weighted normals and keep the
C        vector components in three scalar attributes,
C        x_n_norm, y_n_norm, z_n_norm
C
C        These will be kept in both cmoin and cmoout
C
         if_keepatt = 2
      elseif(cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'keep_area')then
C
C        Compute node area weighted normals and keep the
C        vector components in three scalar attributes,
C        x_n_norm, y_n_norm, z_n_norm
C
C        These will be kept in both cmoin and cmoout
C
         if_keepatt = 3
      endif
      endif
C
C     *******************************************************************
C     GET MEMORY FOR LOCAL ARRAYS.
C
      lenmm1=nnodes
      lenmm2=nelements
      lenmm3=3*nelements
      lenmm4=20000
C
      call mmgetblk('xnorm',isubname,ipxnorm,lenmm1,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('ynorm',isubname,ipynorm,lenmm1,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('znorm',isubname,ipznorm,lenmm1,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('tn',isubname,iptn,lenmm3,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
C     ****************************************************************
C     INITIALIZE THE OUTWARD NORMALS AT EACH NODE.
C
      do i=1,nnodes
         xnorm(i)=0.0
         ynorm(i)=0.0
         znorm(i)=0.0
      enddo
C
      if((nwds .eq. 5).and. (msgtype(5) .eq. 3))then
      if    (cmsgin(5) .eq. 'xy')then
         xplane = 0.0
         yplane = 0.0
         zplane = 1.0
      elseif(cmsgin(5) .eq. 'xz')then
         xplane = 0.0
         yplane = 1.0
         zplane = 0.0
      elseif(cmsgin(5) .eq. 'yx')then
         xplane = 1.0
         yplane = 0.0
         zplane = 0.0
      elseif(cmsgin(5) .eq. 'yz')then
         xplane = 0.0
         yplane = 0.0
         zplane = -1.0
      elseif(cmsgin(5) .eq. 'zx')then
         xplane = 0.0
         yplane = -1.0
         zplane = 0.0
      elseif(cmsgin(5) .eq. 'zy')then
         xplane = -1.0
         yplane = 0.0
         zplane = 0.0
      endif
      elseif(nwds .eq. 7)then
         xplane = xmsgin(5)
         yplane = xmsgin(6)
         zplane = xmsgin(7)
      endif
C     *******************************************************************
C     COMPUTE THE UNIT OUTWARD NORMALS FOP EACH TRIANGLE AND ASSIGN IT
C     TO EACH OF ITS NODES. THE NORMALS OF EACH TRIANGLE NODE ARE WIEGHTED
C     BY THE ANGLE AT THAT NODE.
C
C     ****************************************************************
C
C     Check if all elements were line or triangle.
C
      i_count = 0
      do itri=1,nelements
         if((itettyp(itri) .ne. 3) .or. (itettyp(itri) .eq. 2)) then
            i_count = i_count + 1
         endif
      enddo
C
      if(i_count .ne. 0)then
          write(logmess,849)
  849     format('ERROR offsetsurf:')
          call writloga('default',0,logmess,0,ier)
          write(logmess,850)
  850     format('ERROR:Invalid element types')
          call writloga('default',0,logmess,0,ier)
          write(logmess,851)
  851     format('ERROR:Only tri and line elements supported')
          call writloga('default',0,logmess,0,ier)
          write(logmess,952) nelements, i_count
  952     format('ERROR:# elements = ',i10,' Invalid elements = ',i10)
          call writloga('default',0,logmess,0,ier)
          write(logmess,953)
  953     format('ERROR:NO ACTION')
          call writloga('default',0,logmess,0,ier)
          write(logmess,849)
          call writloga('default',0,logmess,0,ier)
          logmess = 
     1       'cmo/delete/'//cmoout(1:icharlnf(cmoout))//'; finish'     
          call dotask(logmess,ier)
          go to 9999
      endif
C
C     ****************************************************************

      do itri=1,nelements
C
C        ----------------------------------------------------------------
C        NOW WE STORE THE NORMALS OF THE ELEMENTS IN THE ARRAY tn.
C
         if(itettyp(itri) .eq. 3)then
            i1=itet(itetoff(itri) + 1)
            i2=itet(itetoff(itri) + 2)
            i3=itet(itetoff(itri) + 3)
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            x3=xic(i3)
            y3=yic(i3)
            z3=zic(i3)
         elseif(itettyp(itri) .eq. 2)then
            i1=itet(itetoff(itri) + 1)
            i2=itet(itetoff(itri) + 2)
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            x3=xic(i2) + xplane
            y3=yic(i2) + yplane
            z3=zic(i2) + zplane
         else
            i_count = i_count + 1
         endif
C
C        Compute 2*area vector for the triangle.
C
         tn(1,itri)=dbarea(y1,z1,y2,z2,y3,z3)
         tn(2,itri)=dbarea(z1,x1,z2,x2,z3,x3)
         tn(3,itri)=dbarea(x1,y1,x2,y2,x3,y3)
C
C        Compute the magnitude of the 2*area vector.
C   
         dcross=sqrt( tn(1,itri)*tn(1,itri)+
     &                tn(2,itri)*tn(2,itri)+
     &                tn(3,itri)*tn(3,itri) )
C
C        Normalize the area vector.
C
         tn(1,itri)=tn(1,itri)/dcross
         tn(2,itri)=tn(2,itri)/dcross
         tn(3,itri)=tn(3,itri)/dcross
C
C        ****************************************************************
C        COMPUTE THE THREE ANGLES AT EACH OF THE ELEMENT'S NODES.
C
         if(itettyp(itri) .eq. 3)then
         vx1=x2-x1
         vy1=y2-y1
         vz1=z2-z1
         vx2=x3-x1
         vy2=y3-y1
         vz2=z3-z1
         top=vx1*vx2+vy1*vy2+vz1*vz2
         bot=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)*
     &       sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
         bot=max(epsln,bot)
         ang1=acos(top/bot)
C
         vx1=x3-x2
         vy1=y3-y2
         vz1=z3-z2
         vx2=x1-x2
         vy2=y1-y2
         vz2=z1-z2
         top=vx1*vx2+vy1*vy2+vz1*vz2
         bot=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)*
     &       sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
         bot=max(epsln,bot)
         ang2=acos(top/bot)
C
         vx1=x1-x3
         vy1=y1-y3
         vz1=z1-z3
         vx2=x2-x3
         vy2=y2-y3
         vz2=z2-z3
         top=vx1*vx2+vy1*vy2+vz1*vz2
         bot=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)*
     &       sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
         bot=max(epsln,bot)
         ang3=acos(top/bot)
C
C        FOR LINES SCALE BY THE LENGTH OF THE LINE SEGMENT
C
         elseif(itettyp(itri) .eq. 2)then
 
         ang1 = sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)
         ang2 = ang1
         ang1 = 1.
         ang2 = 1.
         endif
 
         if(if_keepatt .ge. 0 .and. if_keepatt .le. 2)then
C
C           Angle weighted
C
            scale1 = ang1
            scale2 = ang2
            scale3 = ang3
         elseif(if_keepatt .eq. 3)then
C
C           Area weighted
C
            scale1 = dcross
            scale2 = dcross
            scale3 = dcross
         endif
         xnorm(i1)=xnorm(i1)+scale1*tn(1,itri)
         ynorm(i1)=ynorm(i1)+scale1*tn(2,itri)
         znorm(i1)=znorm(i1)+scale1*tn(3,itri)
C
         xnorm(i2)=xnorm(i2)+scale2*tn(1,itri)
         ynorm(i2)=ynorm(i2)+scale2*tn(2,itri)
         znorm(i2)=znorm(i2)+scale2*tn(3,itri)
C
         if(itettyp(itri) .eq. 3)then
         xnorm(i3)=xnorm(i3)+scale3*tn(1,itri)
         ynorm(i3)=ynorm(i3)+scale3*tn(2,itri)
         znorm(i3)=znorm(i3)+scale3*tn(3,itri)
         endif
C
      enddo
C     End loop over all elements.
C     ****************************************************************
C     NORMALIZE EACH NODE'S OUTWARD NORMAL.
C
      i_count = 0
      do i=1,nnodes
         if(abs(xnorm(i)) .lt. 100*epsln)xnorm(i) = 0.0d0
         if(abs(ynorm(i)) .lt. 100*epsln)ynorm(i) = 0.0d0
         if(abs(znorm(i)) .lt. 100*epsln)znorm(i) = 0.0d0
         
         anorm=sqrt(xnorm(i)*xnorm(i)+
     &              ynorm(i)*ynorm(i)+
     &              znorm(i)*znorm(i))
 
         if(anorm .gt. 1.e-9)then
         xnorm(i)=xnorm(i)/anorm
         ynorm(i)=ynorm(i)/anorm
         znorm(i)=znorm(i)/anorm
         else
         write(logmess,948) i, anorm
  948    format
     & ('WARNING offsetsurf: Degenerate Normal Node ',i10,2x,e20.12)
         call writloga('default',0,logmess,0,ier)
         i_count = i_count + 1
         xnorm(i)=anorm
         ynorm(i)=anorm
         znorm(i)=anorm
         endif
      enddo
      if(i_count .ne. 0)then
         write(logmess,949)
  949    format('WARNING offsetsurf: Degenerate Normals ')
         call writloga('default',0,logmess,0,ier)
         write(logmess,950) i_count
  950    format('WARNING offsetsurf: Total = ',i10)
         call writloga('default',0,logmess,0,ier)
      endif
C
C     ****************************************************************
C     OFFSET THE NODE COORDINATES OF THE CURRENT SURFACE BY DISTANCE d
C     IN THE DIRECTION OF EACH NODE'S OUTWARD NORMAL.
C
      d=xmsgin(4)
      do i=1,nnodes
         xico(i)=xic(i)+d*xnorm(i)
         yico(i)=yic(i)+d*ynorm(i)
         zico(i)=zic(i)+d*znorm(i)
         imt1o(i)=imt1(i)
         itp1o(i)=itp1(i)
      enddo
C
C     ****************************************************************
C     SET UP THE CONNECTIVITY AND OTHER ATTRIBUTES OF THE NEW cmo
C     AS THE SAME AS THE OLD cmo.
C
      do itri=1,nelements
         do inode = 1,nelmnen(itettyp(itri))
            iteto(itetoff(itri)+inode)=itet(itetoff(itri)+inode)
            jteto(jtetoff(itri)+inode)=jtet(jtetoff(itri)+inode)
         enddo
C
         itetclro(itri)=itetclr(itri)
         itettypo(itri)=itettyp(itri)
         itetoffo(itri)=itetoff(itri)
         jtetoffo(itri)=jtetoff(itri)
C
      enddo
 
C     call cmo_select(cmo,ierr)
C
C
C     ****************************************************************
C     If attributes are to be kept, create and fill them here.
C
      if((if_keepatt .ge. 1).and.(if_keepatt .le. 3))then
      lencmoname = icharlnf(cmoin)
      logmess = 'cmo/addatt/'//cmoin(1:lencmoname)//
     1   '/x_n_norm/vdouble/scalar/nnodes/-def-/permanent; finish'     
      call dotask(logmess,ier)
      logmess = 'cmo/addatt/'//cmoin(1:lencmoname)//
     1   '/y_n_norm/vdouble/scalar/nnodes/-def-/permanent; finish'     
      call dotask(logmess,ier)
      logmess = 'cmo/addatt/'//cmoin(1:lencmoname)//
     1   '/z_n_norm/vdouble/scalar/nnodes/-def-/permanent; finish'     
      call dotask(logmess,ier)

      call cmo_get_info('x_n_norm',cmoin,ipx_n_norm,ilen,icmotype,ier)
      call cmo_get_info('y_n_norm',cmoin,ipy_n_norm,ilen,icmotype,ier)
      call cmo_get_info('z_n_norm',cmoin,ipz_n_norm,ilen,icmotype,ier)
      
      do i=1,nnodes
         x_n_norm(i)=xnorm(i)
         y_n_norm(i)=ynorm(i)
         z_n_norm(i)=znorm(i)
      enddo
C
C     Do not add the normal attribute to cmoout since
C     it is not really the normal to the new, offset surface.
C
      endif
C     ****************************************************************
      goto 9999
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
 9995 call mmrelprt(isubname,ics)
C
C     *******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT.
C
      return
      end
