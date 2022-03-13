      subroutine refine_spawn_lg(mpno,mpary,cxnew,cynew,cznew,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C  This routine spawns new nodes at the locations given
c  It finds the existing edge that is closest to the
c  desired node
c  It then refines that edge and moves the resulting
c  node to the desired location provided that
c  the move does not invert any elements
c
c    INPUT ARGUMENTS -
C
C       mpno    number of nodes to add to the mesh
c       mpary   array of nodes to be spawned
c       xnew,ynew,znew  coordinates of spawned nodes
c
C     OUTPUT ARGUMENTS -
C
C        IERROR -- error return.
C
C     CHANGE HISTORY -
C
C        $Log: refine_spawn_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   07 Jan 2002 13:47:20   dcg
CPVCS    add error condition argument to refine_edge_add_tet call
CPVCS
CPVCS       Rev 1.4   05 May 2000 16:06:40   dcg
CPVCS    change nelmnee to nelmnef where needed
CPVCS
CPVCS       Rev 1.3   Wed Apr 05 13:34:58 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.2   01 Mar 2000 13:53:56   dcg
CPVCS    add multimaterial code
c
C ######################################################################
C
      implicit none
      include 'local_element.h'
      include 'chydro.h'
      include 'consts.h'

C arguments
      integer mpno,mpary(*),ierror
      character*32 cxnew,cynew,cznew

C variables
      pointer (ipxnew,xnew),(ipynew,ynew),(ipznew,znew)
      real*8 xnew(*),ynew(*),znew(*)
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      pointer (ipitet,itet)
      pointer (ipjtet,jtet)
      integer itet(*),jtet(*)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      integer itetoff(*),jtetoff(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipisn1,isn1)
      integer isn1(*)
      pointer (ipicr1,icr1)
      integer icr1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipisetwd,isetwd)
      integer isetwd(*)
      pointer (ipiseed,iseed)
      integer iseed(*)
      pointer (ipiparent,iparent)
      integer iparent(*)
      pointer (ipielts,ielts)
      integer ielts(*)
      pointer (ipipset,ipset)
      integer ipset(*)
      pointer (ipitadd,itadd)
      pointer (ipieadd,ieadd)
      pointer (ipiadd,iadd)
      pointer (ipitpadd,itpadd)
      pointer (ipicradd,icradd)
      integer itadd(*),ieadd(*),iadd(*),itpadd(*),icradd(*)

      pointer (ipxic,xic)
      real*8 xic(*)
      pointer (ipyic,yic)
      real*8 yic(*)
      pointer (ipzic,zic)
      real*8 zic(*)
      pointer (ipout,out)
      real*8 out(*)

      pointer (ipxadd,xadd),(ipyadd,yadd),(ipzadd,zadd),
     *  (ipxsave,xsave),(ipysave,ysave),(ipzsave,zsave)
      real *8 xadd(*),yadd(*),zadd(*),xsave(*),ysave(*),zsave(*)

      real*8 vol,dist,distance_lg,epsilonl,epsilonv,
     *  c,cos_lg,cosangle

      integer icscode,nelements,ilen,itype,ierr,i,iseedtet,locnod,
     *  nelts,nef_cmo,nnodes,i1,i2,i3,i4,nodesold,node1,node,nod,
     *  icmotype,iemin,itmin,ielt,mbndry,length,nadd,ie,j,
     *  ityp,iout,len_elist,l,ipt,icount,flag

      character*132 logmess
      character*32 cmo,cout,isubname

C ######################################################################
C BEGIN begin

      isubname='refine_spawn'
c.... Allocate memory for 'add' arrays.
      len_elist=mpno
      call mmgetblk('itadd',isubname,ipitadd,len_elist,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,len_elist,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,len_elist,1,icscode)
      call mmgetblk('itpadd',isubname,ipitpadd,len_elist,1,icscode)
      call mmgetblk('icradd',isubname,ipicradd,len_elist,1,icscode)
      call mmgetblk('ipset',isubname,ipipset,len_elist,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,len_elist,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,len_elist,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,len_elist,2,icscode)
      call mmgetblk('xsave',isubname,ipxsave,len_elist,2,icscode)
      call mmgetblk('ysave',isubname,ipysave,len_elist,2,icscode)
      call mmgetblk('zsave',isubname,ipzsave,len_elist,2,icscode)
      call cmo_get_name(cmo,icscode)
c.... Get cmo pointers.
 
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,itype,ierr)
      call cmo_get_intinfo('nnodes',cmo,nnodes,ilen,itype,ierr)
      call cmo_get_intinfo('mbndry',cmo,mbndry,ilen,itype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,itype,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ierr)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,itype,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierr)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      call cmo_get_info(cxnew,cmo,ipxnew,ilen,itype,ierr)
      if(ierr.ne.0) then
         write(logmess,10) cxnew
         call writloga('default',0,logmess,0,icscode)
         go to 9999
      endif
      call cmo_get_info(cynew,cmo,ipynew,ilen,itype,ierr)
      if(ierr.ne.0) then
         write(logmess,10) cynew
         call writloga('default',0,logmess,0,icscode)
         go to 9999
      endif
      call cmo_get_info(cznew,cmo,ipznew,ilen,itype,ierr)
      if(ierr.ne.0) then
         write(logmess,10) cznew
 10      format (' attribute missing for refine/spawn ',a)
         call writloga('default',0,logmess,0,icscode)
         go to 9999
      endif
      call cmo_get_intinfo('faces_per_element',cmo,nef_cmo,ilen,itype,
     *   ierr)
      call cmo_get_attinfo('epsilon',cmo,iout,epsilon,cout,
     *  ipout,ilen,itype, ierr)
c.... Get space for local arrays.
 
      call mmgetblk('iseed',isubname,ipiseed,nnodes,1,icscode)
      call mmgetblk('ielts',isubname,ipielts,200,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
c.... Define IPARENT array.
 
      call unpackpc(nnodes,itp1,isn1,iparent,icscode)
 
c.... Loop through elements to fill seed tet list.
 
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnen(ityp)
            nod=itet(j+itetoff(i))
            iseed(iparent(nod))=i
         enddo
      enddo
c.... Loop through nodes to be spawned and find edges that contain node
      nadd=0
      do i=1,mpno
         locnod=iparent(mpary(i))
         iseedtet=iseed(iparent(mpary(i)))
         ityp=itettyp(iseedtet)
         l=0
         do j=1,nelmnef(ityp)
            if (iparent(itet(itetoff(iseedtet)+j)).eq.locnod) l=j
         enddo
         if(l.eq.0) go to 9999
         call get_elements_around_node(iseedtet,l,nelts,ipielts,
     *      itetoff,jtetoff,itet,jtet,itettyp,iparent,
     *      nef_cmo,mbndry)
C.... Find closest edge
         cosangle=0.0
         do j=1,nelts
            ielt=ielts(j)
            ityp=itettyp(ielt)
            do ie=1,nelmnee(ityp)
               i1=itet(itetoff(ielt)+ielmedge1(1,ie,itettyp(ielt)))
               i2=itet(itetoff(ielt)+ielmedge1(2,ie,itettyp(ielt)))
               i3=0
               if(iparent(i1).eq.locnod) then
                  i3=i2
               elseif(iparent(i2).eq.locnod) then
                  i3=i1
               endif
               if(i3.ne.0) then
                  c=cos_lg(xic(locnod),yic(locnod),zic(locnod),
     *              xic(i3),yic(i3),zic(i3),xnew(locnod),
     *             ynew(locnod),znew(locnod),
     *              epsilon,ierr)
                  if(c.ge.cosangle) then
                     cosangle=max(c,cosangle)
                     iemin=ie
                     itmin=ielt
                     ipt=i3
                  endif
               endif
            enddo
         enddo
         if (cosangle.gt.0) then
               nadd=nadd+1
               itadd(nadd)=itmin
               ieadd(nadd)=iemin
               iadd(nadd)=0
               ipset(nadd)=iand(isetwd(ipt),isetwd(locnod))
               xadd(nadd)=half*(xic(ipt)+xic(locnod))
               yadd(nadd)=half*(yic(ipt)+yic(locnod))
               zadd(nadd)=half*(zic(ipt)+zic(locnod))
               xsave(nadd)=xnew(locnod)
               ysave(nadd)=ynew(locnod)
               zsave(nadd)=znew(locnod)
         endif
      enddo
      nodesold=nnodes
C...  refine edges
c
      call refine_fix_add(cmo,nadd,ipitadd,ipieadd,ipiadd,
     &   ipitpadd,ipicradd)
      call refine_edge_add_tet(cmo,nadd,ipitadd,ipieadd,
     &   iadd,xadd,yadd,zadd,flag)
c.... Fix up ITP1, ICR1 values.
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,
     *   ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,
     *   ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,
     *   ipisn1,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *   ipisetwd,length,icmotype,ierror)
      do i = 1,nadd
         if (iadd(i).gt.0) then
            node=iadd(i)
            isetwd(node)=ipset(i)
            if (isn1(node).eq.0) then
               itp1(node)=itpadd(i)
               icr1(node)=icradd(i)
            else
               icr1(node)=icradd(i)
               if (itp1(node).ne.ifitpcup) then
                  itp1(node)=itpadd(i)
               endif
               node1=isn1(node)
               do while (node1.ne.node)
                  icr1(node1)=icradd(i)
                  if (itp1(node1).ne.ifitpcup) then
                     itp1(node1)=itpadd(i)
                  endif
                  node1=isn1(node1)
               enddo
            endif
         endif
      enddo
c... Now move nodes to new locations
 
c.... Get cmo pointers.
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,itype,ierr)
      call cmo_get_intinfo('nnodes',cmo,nnodes,ilen,itype,ierr)
      call cmo_get_intinfo('mbndry',cmo,mbndry,ilen,itype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,itype,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ierr)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,itype,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierr)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      call cmo_get_intinfo('faces_per_element',cmo,nef_cmo,
     *   ilen,itype, ierr)
      call cmo_get_attinfo('epsilonl',cmo,iout,epsilonl,cout,
     *   ipout,ilen,itype, ierr)
      call cmo_get_attinfo('epsilonv',cmo,iout,epsilonv,cout,
     *   ipout,ilen,itype, ierr)
c.... Fill seed array
 
      call mmnewlen('iseed',isubname,ipiseed,nnodes,icscode)
      call mmnewlen('iparent',isubname,ipiparent,nnodes,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent,icscode)
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnen(ityp)
            nod=itet(j+itetoff(i))
            iseed(iparent(nod))=i
         enddo
      enddo
c.... Find all elements around new nodes and make sure that new
C     positions will not invert tets
      do i=nodesold+1,nodesold+nadd
         dist=distance_lg(xic(i),yic(i),zic(i),xsave(i-nodesold),
     *     ysave(i-nodesold),zsave(i-nodesold),ierror)
         if(dist.gt.epsilonl) then
            iseedtet=iseed(iparent(i))
            locnod=iparent(i)
            ityp=itettyp(iseedtet)
            l=0
            do j=1,nelmnef(ityp)
               if (iparent(itet(itetoff(iseedtet)+j)).eq.locnod) l=j
            enddo
            if(l.eq.0) go to 9999
            call  get_elements_around_node(iseedtet,l,nelts,
     *       ipielts,
     *       itetoff,jtetoff,itet,jtet,itettyp,iparent,
     *       nef_cmo,mbndry)
            do j=1,nelts
               i1=itet(itetoff(ielts(j))+1)
               i2=itet(itetoff(ielts(j))+2)
               i3=itet(itetoff(ielts(j))+3)
               i4=itet(itetoff(ielts(j))+4)
               call volume_tet(xic(i1),yic(i1),zic(i1),
     *          xic(i2),yic(i2),zic(i2),
     *          xic(i3),yic(i3),zic(i3),
     *          xic(i4),yic(i4),zic(i4),vol)
               if(vol.lt.epsilonv) then
                  write(logmess,100) xsave(i-nodesold),
     *             ysave(i-nodesold),zsave(i-nodesold)
 100              format(' node cannot be moved to ',3e20.5)
                  call writloga('default',0,logmess,0,icscode)
               else
                  xic(i)=xsave(i-nodesold)
                  yic(i)=ysave(i-nodesold)
                  zic(i)=zsave(i-nodesold)
c
c....  change all child nodes as well
c
                if(isn1(i).ne.0) then
                     l=isn1(i)
                     icount=0
                     do while (l.ne.i.and.icount.le.1000)
                        xic(l)=xsave(i-nodesold)
                        yic(l)=ysave(i-nodesold)
                        zic(l)=zsave(i-nodesold)
                        l=isn1(l)
                        icount=icount+1
                     enddo
                  endif
               endif
            enddo
         endif
      enddo
9999  call mmrelprt(isubname,icscode)
      return
      end
