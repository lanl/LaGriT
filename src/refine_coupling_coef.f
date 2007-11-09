      subroutine refine_coupling_coef()
c#######################################################################
c
c     purpose -
c
C  This routine takes the information in ietet_aij (made by test_coupling_
C  coef and attempts to refine the 'bad' edges by projecting the
C  4th point of the tet onto the bad face and then projecting this
C  point onto the 'bad' edge
C
C $Log:   /pvcs.config/t3d/src/refine_coupling_coef.f_a  $
CPVCS    
CPVCS       Rev 1.3   07 Jan 2002 13:48:48   dcg
CPVCS    add error return argument to refine_edge_add_tet call
CPVCS
CPVCS       Rev 1.2   Thu Apr 06 13:48:42 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.1   Fri Jan 22 15:36:38 1999   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.0   Thu Feb 26 12:14:46 1998   dcg
CPVCS    Initial revision.
      implicit none
      include 'local_element.h'
      include 'chydro.h'
      pointer (ipiadd,iadd)
      pointer (ipieadd,ieadd)
      pointer (ipitadd,itadd)
      pointer (ipxadd,xadd)
      pointer (ipyadd,yadd)
      pointer (ipzadd,zadd)
      pointer (ipitpadd,itpadd)
      pointer (ipicradd,icradd)
      integer ieadd(*),iadd(*),itadd(*),itpadd(*),icradd(*)
      real*8 xadd(*),yadd(*),zadd(*)
      pointer (ipietet,ietet)
      integer ietet(3,10000000)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipiparent,iparent)
      pointer (ipitp,itp1)
      pointer (ipicr,icr1)
      pointer (ipisn,isn1)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      pointer (ipitettyp,itettyp)
      integer iparent(*),itp1(*),isn1(*),itet(*),itetoff(*),itettyp(*),
     *   icr1(*)
      integer ierror,ilen,itype,npoints,ntets,numneg,icscode,len_elist,
     *   nadd,it,i,iface,ittyp,i1,i2,i3,i4,iedge,node1,
     *   node,flag
      character*32 cmo,isubname,cdefault
      real*8 a,b,c,d,sf,au,bu,cu,xp,yp,zp,xproj,yproj,zproj
      character*132 logmess
C
      isubname='refine_coup_coef'
      cdefault='default'
C
C     Get mesh object data.
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ietet_aij',cmo,ipietet,ilen,itype,ierror)
      call cmo_get_info('num_neg_coup_coeff',cmo,numneg,ilen,itype,
     *      ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierror)
      call cmo_get_info('itp1',cmo,ipitp,ilen,itype,ierror)
      call cmo_get_info('isn1',cmo,ipisn,ilen,itype,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierror)
c
      call cmo_get_info('idebug',cmo,idebug,ilen,itype,ierror)
C
C
C    Get temporary memory
C
      len_elist=numneg
      call mmgetblk('itadd',isubname,ipitadd,len_elist,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,len_elist,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,len_elist,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,len_elist,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,len_elist,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,len_elist,2,icscode)
      call mmgetblk('itpadd',isubname,ipitpadd,len_elist,1,icscode)
      call mmgetblk('icradd',isubname,ipicradd,len_elist,1,icscode)
C
C  put parent nodes in iparent
C
      call mmgetblk('iparent',isubname,ipiparent,npoints,1,ierror)
      call unpackpc(npoints,itp1,isn1,iparent)
C
C  Loop through bad edges and make point to send to the
C  refine routine
C
      nadd=0
      do i=1,numneg
         it=ietet(1,i)
         ittyp=itettyp(it)
         iface=ietet(2,i)
         i1 = iparent(itet(itetoff(it)+
     *                   ielmface1(1,iface,ittyp)))
         i2 = iparent(itet(itetoff(it)+
     *                   ielmface1(2,iface,ittyp)))
         i3 = iparent(itet(itetoff(it)+
     *                   ielmface1(3,iface,ittyp)))
         i4 = iparent(itet(itetoff(it)+
     *                   iface))
 
c
c project the fourth point of the tet onto the bad face
C
C        SET UP THE EQUATION OF THE PLANE FROM THE 3 POINTS.
C
         a=  (yic(i2)-yic(i1))*(zic(i3)-zic(i1)) -
     &       (yic(i3)-yic(i1))*(zic(i2)-zic(i1))
         b=-((xic(i2)-xic(i1))*(zic(i3)-zic(i1)) -
     &       (xic(i3)-xic(i1))*(zic(i2)-zic(i1)))
         c=  (xic(i2)-xic(i1))*(yic(i3)-yic(i1)) -
     &       (xic(i3)-xic(i1))*(yic(i2)-yic(i1))
         d=a*xic(i1)+b*yic(i1)+c*zic(i1)
C        DETERMINE THE UNIT VECTOR.
C    and the distance to the plane (sf)
         au=a/sqrt(a*a + b*b + c*c)
         bu=b/sqrt(a*a + b*b + c*c)
         cu=c/sqrt(a*a + b*b + c*c)
         sf=(a*xic(i4) + b*yic(i4) + c*zic(i4) - d)/sqrt(a*a + b*b
     &            + c*c)
         xproj=xic(i4)-au*sf
         yproj=yic(i4)-bu*sf
         zproj=zic(i4)-cu*sf
C  check that plane is ok
         if(a.eq.0.0.and.b.eq.0.0.and.c.eq.0.0) go to 100
C  project point onto edge
         iedge=ietet(3,i)
         i1=iparent(itet(itetoff(it)+ielmedge1(1,iedge,ittyp)))
         i2=iparent(itet(itetoff(it)+ielmedge1(2,iedge,ittyp)))
C get unit vector for edge
         sf=sqrt((xic(i2)-xic(i1))**2+(yic(i2)-yic(i1))**2+
     *           (zic(i2)-zic(i1))**2)
         au=(xic(i2)-xic(i1))/sf
         bu=(yic(i2)-yic(i1))/sf
         cu=(zic(i2)-zic(i1))/sf
C get magnitude of (vector from i1 to projected point) projected
C onto the edge
         d=((xic(i2)-xic(i1))*(xproj-xic(i1))+
     *      (yic(i2)-yic(i1))*(yproj-yic(i1))+
     *      (zic(i2)-zic(i1))*(zproj-zic(i1)))/sf
C get coordinates of point projected onto the edge
         xp=au*d+xic(i1)
         yp=bu*d+yic(i1)
         zp=cu*d+zic(i1)
C  check that projection is between the endpoints of the edge
         if(xp.lt.min(xic(i1),xic(i2)).or.
     *      xp.gt.max(xic(i1),xic(i2)).or.
     *      yp.lt.min(yic(i1),yic(i2)).or.
     *      yp.gt.max(yic(i1),yic(i2)).or.
     *      zp.lt.min(zic(i1),zic(i2)).or.
     *      zp.gt.max(zic(i1),zic(i2))) go to 100
C  point okay so add it to the list of points to insert using refine
         nadd=nadd+1
         itadd(nadd)=it
         iadd(nadd)=0
         ieadd(nadd)=iedge
         xadd(nadd)=xp
         yadd(nadd)=yp
         zadd(nadd)=zp
         if(idebug.ge.2) then
            write(logmess,90)it,iedge,i1,i2
  90        format ('tet ',i8,' edge ',i2,' nodes ',2i10,
     *           ' will be refined due to coupling coefficient error')
            call writloga(cdefault,0,logmess,0,ierror)
         endif
c  branch to here if rejecting a point
 100     continue
      enddo
C  do refinement
      if(nadd.eq.0) go to 200
      call refine_fix_add(cmo,nadd,ipitadd,ipieadd,ipiadd,
     &   ipitpadd,ipicradd)
      call refine_edge_add_tet(cmo,nadd,ipitadd,ipieadd,
     &   iadd,xadd,yadd,zadd,flag)
 
c.... Fix up ITP1, ICR1 values.
      call cmo_get_info('itp1',cmo,
     *   ipitp,ilen,itype,ierror)
      call cmo_get_info('icr1',cmo,
     *   ipicr,ilen,itype,ierror)
      call cmo_get_info('isn1',cmo,
     *   ipisn,ilen,itype,ierror)
      do i = 1,nadd
         if (iadd(i).gt.0) then
            node=iadd(i)
            if (isn1(node).eq.0) then
               itp1(node)=itpadd(i)
               icr1(node)=icradd(i)
            else
               if (itp1(node).ne.ifitpcup) then
                  itp1(node)=itpadd(i)
                  icr1(node)=icradd(i)
               endif
               node1=isn1(node)
               do while (node1.ne.node)
                  if (itp1(node1).ne.ifitpcup) then
                     itp1(node1)=itpadd(i)
                     icr1(node1)=icradd(i)
                  endif
                  node1=isn1(node1)
               enddo
            endif
         endif
      enddo
 
      call dotaskx3d('recon/0 ; finish' ,icscode)
200   call mmrelprt(isubname,icscode)
      return
      end
 
 
 
 
 
 
 
 
 
