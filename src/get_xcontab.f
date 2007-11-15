*dk,get_xcontab
      subroutine get_xcontab(coption,cmo,ipxcontab_node)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE DETERMINES THE LOCAL CONSTRAINT TABLE FOR EACH
C            NODE IN A CMO USING THE DEGREE-OF-FREEDOM INFO IN ICONTAB
C
C      INPUT ARGUMENTS -
C
C         coption - (character) The Option.
C         cmo     - Current-Mesh_Object (CMO) name.
C
C      OUTPUT ARGUMENTS -
C
C         ipxcontab_node - Pointer to the local constraint table for each
C                        node.
C
C      CHANGE HISTORY -
C
C        $Log: get_xcontab.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:49:38 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Tue Jul 30 16:26:32 1996   dcg
CPVCS    fix typo with zarea
CPVCS
CPVCS       Rev 1.5   Mon Jun 24 10:14:54 1996   dcg
CPVCS    if there is no icontab assume degree of freedom is 3
CPVCS
CPVCS       Rev 1.4   Thu May 16 10:26:16 1996   dcg
CPVCS     changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.3   12/05/95 08:25:32   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.2   11/07/95 17:18:00   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   08/15/95 18:22:04   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.0   07/17/95 16:24:06   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "local_element.h"
      include "chydro.h"
C
C#######################################################################
C
      character*(*) coption, cmo
C
      pointer (ipxcontab_node, xcontab_node )
      real*8 xcontab_node(1000000)
C
      real*8 xcontab2(3,3)
C
      integer icscode
C
C#######################################################################
C
      character*32 isubname
C
      real*8     xc(3)   ,
     *           xid(3,3)
C
      pointer (ipicr1, icr1)
      pointer (ipitp1, itp1)
      integer icr1(10000000), itp1(1000000)
      pointer (ipxic, xic )
      real*8 xic(1000000)
      pointer (ipyic, yic )
      real*8 yic(1000000)
      pointer (ipzic, zic )
      real*8 zic(1000000)
C
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
      pointer (ipitettyp, itettyp )
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff )
      integer itetoff(1000000)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(1000000)
      pointer (ipitet, itet1 )
      integer itet1(1000000)
      pointer (ipjtet, jtet1 )
      integer jtet1(1000000)
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
C
      pointer (ipxarea, xarea )
      real*8 xarea(1000000)
      pointer (ipyarea, yarea )
      real*8 yarea(1000000)
      pointer (ipzarea, zarea )
      real*8 zarea(1000000)
C
      real*8 x2,y2,z2, x3,y3,z3, x4,y4,z4
      real*8 xarea_tri, yarea_tri, zarea_tri,
     8       xareamag, xareamax
      real*8 x1,y1,z1,dx,dy,dz,dxtri,dytri,dztri,xdir,ydir,zdir,
     * xn,yn,zn,ds,xavg,yavg,zavg,xfac,xmag,xmag_dir
C
      integer i,j,i1,it,i2,i3,i4,ioff,joff,loption, j1
      integer nnodes, nelements, mbndry, idofi1, idofj1, icount
      integer ilen, itype, length
C
C#######################################################################
C
      integer icharlnf
      logical isicontab
C
C#######################################################################
C
C
C#######################################################################
C
C
      isubname='get_xcontab_node'
C
      icscode = -1
C
      loption=icharlnf(coption)
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icontab',cmo,ipicontab,ilen,itype,icscode)
      isicontab=.true.
         if(icscode.ne.0) isicontab=.false.
C
      length=nnodes
      call mmgetblk('xarea',isubname,ipxarea,length,2,icscode)
      call mmgetblk('yarea',isubname,ipyarea,length,2,icscode)
      call mmgetblk('zarea',isubname,ipzarea,length,2,icscode)
C
      do i1=1,nnodes
         xarea(i1)=0.0d+00
         yarea(i1)=0.0d+00
         zarea(i1)=0.0d+00
      enddo
      do it=1,nelements
         ioff=itetoff(it)
         joff=jtetoff(it)
         do i=1,nelmnef(itettyp(it))
            i1=itet1(ioff+i)
            if (icr1(i1).eq.0.or..not.isicontab) then
               idofi1=3
            else
               idofi1=icontab(2,icr1(i1))
            endif
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            if(jtet1(joff+i).ge.mbndry) then
               if(itettyp(it).eq.ifelmtet) then
                  i2=itet1(ioff+ielmface1(1,i,itettyp(it)))
                  i3=itet1(ioff+ielmface1(2,i,itettyp(it)))
                  i4=itet1(ioff+ielmface1(3,i,itettyp(it)))
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  xarea_tri= 0.5d+00*((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
                  yarea_tri=-0.5d+00*((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
                  zarea_tri= 0.5d+00*((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
               elseif(itettyp(it).eq.ifelmtri) then
                  i2=itet1(ioff+ielmface1(1,i,itettyp(it)))
                  i3=itet1(ioff+ielmface1(2,i,itettyp(it)))
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  dx=x3-x2
                  dy=y3-y2
                  dz=z3-z2
                  dxtri= ((y2-y1)*(z3-z1)-(y3-y1)*(z2-z1))
                  dytri=-((x2-x1)*(z3-z1)-(x3-x1)*(z2-z1))
                  dztri= ((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))
                  xdir=-(dy*dztri-dytri*dz)
                  ydir= (dx*dztri-dxtri*dz)
                  zdir=-(dx*dytri-dxtri*dy)
                  xmag_dir=sqrt(xdir**2+ydir**2+zdir**2)
                  xmag=sqrt(dx**2+dy**2+dz**2)
                  if(xmag_dir.gt.0.0) then
                     xarea_tri=xmag*xdir/xmag_dir
                     yarea_tri=xmag*ydir/xmag_dir
                     zarea_tri=xmag*zdir/xmag_dir
                  else
                     xarea_tri=0.0
                     yarea_tri=0.0
                     zarea_tri=0.0
                  endif
               endif
               if(coption(1:loption).eq.'area_vector'.or.
     *            coption(1:loption).eq.'area_normal') then
                  xfac=1.0d+00/ielmface0(i,itettyp(it))
                  do j=1,ielmface0(i,itettyp(it))
                     j1=itet1(ioff+ielmface1(j,i,itettyp(it)))
                     xarea(j1)=xarea(j1)+xfac*xarea_tri
                     yarea(j1)=yarea(j1)+xfac*yarea_tri
                     zarea(j1)=zarea(j1)+xfac*zarea_tri
                  enddo
               elseif(coption(1:loption).eq.'default') then
                  do j=1,ielmface0(i,itettyp(it))
                     j1=itet1(ioff+ielmface1(j,i,itettyp(it)))
                     if (icr1(j1).eq.0.or..not.isicontab) then
                        idofj1=3
                     else
                        idofj1=icontab(2,icr1(j1))
                     endif
                     if(idofi1.eq.1) then
                        if(( idofj1.lt.idofi1 .and.
     *                       itp1(j1).eq.itp1(i1)
     *                     )
     *                    .or.
     *                     ( idofj1.eq.idofi1 .and.
     *                       icr1(j1).eq.icr1(i1)   .and.
     *                       itp1(j1).eq.itp1(i1)
     *                     )
     *                    ) then
                           xavg=0.5d+00*(xic(j1)+x1)
                           yavg=0.5d+00*(yic(j1)+y1)
                           zavg=0.5d+00*(zic(j1)+z1)
                           ds=sqrt((xic(j1)-x1)**2+
     *                             (yic(j1)-y1)**2+
     *                             (zic(j1)-z1)**2)
                           xn=(xic(j1)-x1)/ds
                           yn=(yic(j1)-y1)/ds
                           zn=(zic(j1)-z1)/ds
                           xarea(i1)=xarea(i1)+xavg*xn
                           yarea(i1)=yarea(i1)+yavg*yn
                           zarea(i1)=zarea(i1)+zavg*zn
                        endif
                     endif
                     if(idofj1.eq.2) then
                        xarea(j1)=xarea(j1)+xarea_tri/2.0d+00
                        yarea(j1)=yarea(j1)+yarea_tri/2.0d+00
                        zarea(j1)=zarea(j1)+zarea_tri/2.0d+00
                     endif
                  enddo
               endif
            endif
         enddo
      enddo
      if(coption(1:loption).eq.'area_vector') then
         do i1=1,nnodes
            xcontab_node(1+3*(i1-1))=-xarea(i1)
            xcontab_node(2+3*(i1-1))=-yarea(i1)
            xcontab_node(3+3*(i1-1))=-zarea(i1)
         enddo
      elseif(coption(1:loption).eq.'area_normal') then
         xareamax=0.0d+00
         do i1=1,nnodes
            xareamag=xarea(i1)**2+yarea(i1)**2+zarea(i1)**2
            xareamax=max(xareamax,xareamag)
         enddo
         xareamax=sqrt(xareamax)
         do i1=1,nnodes
            xareamag=sqrt(xarea(i1)**2+yarea(i1)**2+zarea(i1)**2)
            if(xareamag.gt.1.0d-06*xareamax) then
               xcontab_node(1+3*(i1-1))=-xarea(i1)/xareamag
               xcontab_node(2+3*(i1-1))=-yarea(i1)/xareamag
               xcontab_node(3+3*(i1-1))=-zarea(i1)/xareamag
            else
               xcontab_node(1+3*(i1-1))=0.0
               xcontab_node(2+3*(i1-1))=0.0
               xcontab_node(3+3*(i1-1))=0.0
            endif
         enddo
      elseif(coption(1:loption).eq.'default') then
         xareamax=0.0d+00
         do i1=1,nnodes
            xareamag=xarea(i1)**2+yarea(i1)**2+zarea(i1)**2
            xareamax=max(xareamax,xareamag)
         enddo
         xareamax=sqrt(xareamax)
         do i1=1,nnodes
            xareamag=sqrt(xarea(i1)**2+yarea(i1)**2+zarea(i1)**2)
            if(xareamag.gt.1.0d-06*xareamax) then
               xarea(i1)=-xarea(i1)/xareamag
               yarea(i1)=-yarea(i1)/xareamag
               zarea(i1)=-zarea(i1)/xareamag
            endif
         enddo
C
C        ******************************************************************
C
C        DEFINE IDENTITY OPERATOR.
C
         do j = 1,3
            do i = 1,3
               if(i.eq.j) then
                  xid(i,j) = 1.0
               else
                  xid(i,j) = 0.0
               endif
            enddo
         enddo
         do i1=1,nnodes
            if (icr1(i1).eq.0.or..not.isicontab) then
               idofi1=3
            else
               idofi1=icontab(2,icr1(i1))
            endif
            if(idofi1.eq.0) then
               do j=1,3
                  do i=1,3
                     xcontab2(i,j)=0.0
                  enddo
               enddo
            elseif(idofi1.eq.1) then
               xc(1) = xarea(i1)
               xc(2) = yarea(i1)
               xc(3) = zarea(i1)
               do j = 1,3
                  do i = 1,3
                     xcontab2(i,j) = xc(i)*xc(j)
                  enddo
               enddo
            elseif(idofi1.eq.2) then
               xc(1) = xarea(i1)
               xc(2) = yarea(i1)
               xc(3) = zarea(i1)
               do j = 1,3
                  do i = 1,3
                     xcontab2(i,j) = xid(i,j)-xc(i)*xc(j)
                  enddo
               enddo
            elseif(idofi1.eq.3) then
               do j=1,3
                  do i=1,3
                     xcontab2(i,j)=xid(i,j)
                  enddo
               enddo
            endif
            ioff=3*3*(i1-1)
            icount=0
            do j=1,3
               do i=1,3
                  icount=icount+1
                  xcontab_node(ioff+icount)=xcontab2(i,j)
               enddo
            enddo
         enddo
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
