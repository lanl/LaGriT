      subroutine addmesh_reflect(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *  ierror)
C
C
C ######################################################################
C
C     PURPOSE -  reflect a tet mesh in a direction around a line
c      add this mesh to the original mesh
c
c   addmesh/reflect/cmonew/cmoold/x|y|z/origin/[bleed|nobleed]
c   e.g. addmesh/reflect/3dmesh/y/0./bleed
c
c    only implemented for tet meshes because of subroutine
c    reverse_elements  -- this could be extended for other element
c     types.
c     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_reflect.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   10 Jan 2001 14:26:26   dcg
CPVCS    remove duplicate definitions
CPVCS    
CPVCS       Rev 1.3   18 Sep 2000 15:13:22   dcg
CPVCS    use big epsilons
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include 'local_element.h'
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
      character*32 isubname,cmo,cmonew
      integer  ilen, itype, ierr, nelements, maxclr, it
      real*8 xmn,xmx,ymn,ymx,zmn,zmx,epa,epv,origin
      character*256 logmess
      character*3 dir
      character*7 bleed
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
      isubname='addmesh_reflect'
C     ******************************************************************
c  decode the input
      dir='xic'
      origin=0.0
      bleed='bleed'
      if(nwds.lt.3.or.msgtype(2).ne.3.or. msgtype(3).ne.3) then
         logmess=' mesh objects not specified'
         call writloga('default',0,logmess,0,ierror)
         go to 9999
      endif
      cmo=cmsgin(4)
      cmonew=cmsgin(3)
      if(nwds.ge.7.and.msgtype(7).eq.3) then
         bleed=cmsgin(7)
      endif
      if(nwds.ge.5.and.msgtype(6).eq.2) then
         origin=xmsgin(6)
      elseif(nwds.ge.6.and.msgtype(6).eq.1) then
         origin=float(imsgin(6))
      endif
      if(nwds.ge.5.and.msgtype(5).eq.3) then
         if(cmsgin(5).eq.'x'.or.cmsgin(5).eq.'X') then
            dir='xic'
         elseif(cmsgin(5).eq.'y'.or.cmsgin(5).eq.'Y') then
            dir='yic'
         elseif(cmsgin(5).eq.'z'.or.cmsgin(5).eq.'Z') then
            dir='zic'
         endif
      endif
c
C  Get mesh object.
c
      call cmo_get_name(cmo,ierror)
c
c  duplicate the mesh
c
      write(logmess,'(a,a,a)') 'cmo/copy/newmesh_xxx/',cmo,
     *   ';finish'
      call dotask(logmess,ierror)
      call cmo_select('newmesh_xxx',ierror)
c
c  translate the nodes
c
 
      origin=2*origin
      write(logmess,'(a,a,d20.13,a)')
     *  'cmo/addatt/newmesh_xxx/temp_xxx;',
     *  'cmo/setatt/newmesh_xxx/temp_xxx/1,0,0/',origin,';finish'
      call dotask(logmess,ierror)
      write(logmess,'(6a)') 'math/sub/newmesh_xxx/',dir,'/1,0,0/',
     *  'newmesh_xxx /temp_xxx/newmesh_xxx/',dir,'/;finish'
      call dotask(logmess,ierror)
 
c
c  reverse the elements
c
      logmess='reverse_elements/1,0,0/;finish'
      call dotask(logmess,ierror)
c
c  get number of colors and increment itetclr
c
      call cmo_get_intinfo('nelements','newmesh_xxx', nelements,ilen,
     *   itype,ierr)
      call cmo_get_info('itetclr','newmesh_xxx', ipitetclr,ilen,
     *   itype,ierr)
      maxclr=0
      do it=1,nelements
         maxclr=max(itetclr(it),maxclr)
      enddo
      write(logmess,'(2a,i4.4,a)')
     *  'math/add/newmesh_xxx/itetclr/1,0,0/',
     *  'newmesh_xxx/itetclr/',maxclr,'/;finish'
      call dotask(logmess,ierror)
c
c  add the meshes
c
      write(logmess,'(5a)') 'addmesh/glue/',cmonew,
     * '/',cmo,'/newmesh_xxx / ;finish'
      call dotask(logmess,ierror)
      call cmo_release(cmo,ierror)
      call cmo_release('newmesh_xxx',ierror)
      call cmo_select(cmonew,ierror)
c
c  set up the new geometry
c
      call setsize()
      call getsize(xmn,xmx,ymn,ymx,zmn,zmx,epa,epv)
      write(logmess,'(5a)')
     * 'geometry/release/biggeom/;geometry/create/biggeom/;',
     * 'cmo/geometry/',cmonew,'/biggeom/;',
     * 'cmo/setatt//nconbnd/0 /;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/top/   reflect/plane/0.,0.,',
     * zmx,'/1.,0.,',zmx,'/1.,1.,',zmx,'/;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/bottom/reflect/plane/0.,0.,',
     * zmn,'/1.,0.,',zmn,'/1.,1.,',zmn,'/;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/left/  reflect/plane/',xmn,',0.,0./',
     * xmn,',1.,0./',xmn,',1.,1./;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/right/  reflect/plane/',xmx,',0.,0./',
     * xmx,',1.,0./',xmx,',1.,1./;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/front/  reflect/plane/0.,',
     * ymx,',0./1.,',ymx,',0./1.,',ymx,',1./;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3(a,d20.13),a)')
     *  'surface/back/  reflect/plane/0.,',
     * ymn,',0./1.,',ymn,',0./1.,',ymn,',1./;finish'
      call dotask(logmess,ierror)
      write(logmess,'(2a)')
     * 'region/r1/ le top and ge bottom and ge left and le right',
     *  ' and le front and ge back /;finish'
      call dotask(logmess,ierror)
      write(logmess,'(2a)')
     * 'mregion/m1/ le top and ge bottom and ge left and le right',
     *  ' and le front and ge back /;finish'
      call dotask(logmess,ierror)
      write(logmess,'(3a)') 'cmo/setatt//epsilonl/1.e-7;',
     *  ' cmo/setatt//epsilonv /1.e-7;finish'
      call dotask(logmess,ierror)
      write(logmess,'(2a)')
     *   'resetpts/parents;filter/1,0,0/;geniee',
     *   ';finish'
      call dotask(logmess,ierror)
c
c  fix colors
c
      if(bleed.eq.'bleed') then
         write(logmess,'(a,i10,a)') 'bleed_color/',maxclr,'/;finish'
         call dotask(logmess,ierror)
      endif
c
c  fix node attributes (itp,icr,isn)
c
      write(logmess,'(3a)') 'cmo/setatt//itp1/1,0,0/0;',
     *  'resetpts/itp;settets/color_points',
     *  ';cmo/setatt//icr1/1,0,0/0;finish'
      call dotask(logmess,ierror)
c
      write(logmess,'(4a)') 'boundary/dirichlet/icr1/1/top;',
     * 'boundary/dirichlet/icr1/2/bottom/;',
     * 'boundary/dirichlet/icr1/3/right/;',
     * 'boundary/dirichlet/icr1/4/left/;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)') 'boundary/dirichlet/icr1/5/front/;',
     * 'boundary/dirichlet/icr1/6/back/;',
     * 'boundary/dirichlet/icr1/7/top/back/;',
     * 'boundary/dirichlet/icr1/8/top/front/;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)') 'boundary/dirichlet/icr1/9/top/left/;',
     * 'boundary/dirichlet/icr1/10/top/right;',
     * 'boundary/dirichlet/icr1/11/front/right;',
     * 'boundary/dirichlet/icr1/12/front/left;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)')'boundary/dirichlet/icr1/13/front/bottom;',
     * 'boundary/dirichlet/icr1/14/bottom/right;',
     * 'boundary/dirichlet/icr1/15/bottom/left;',
     * ' boundary/dirichlet/icr1/16/bottom/back ;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)')
     * 'boundary/dirichlet/icr1/17/back/left;',
     * 'boundary/dirichlet/icr1/18/back/right;',
     * 'boundary/dirichlet/icr1/19/top/back/left;',
     * 'boundary/dirichlet/icr1/20/top/back/right;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)')
     * 'boundary/dirichlet/icr1/21/top/front/left;',
     * 'boundary/dirichlet/icr1/22/top/front/right;',
     * 'boundary/dirichlet/icr1/23/bottom/back/left;',
     * 'boundary/dirichlet/icr1/24/bottom/back/right;finish'
      call dotask(logmess,ierror)
      write(logmess,'(4a)')
     * 'boundary/dirichlet/icr1/25/bottom/front/left;',
     * 'boundary/dirichlet/icr1/26/bottom/front/right;finish'
      call dotask(logmess,ierror)
c
 9999 continue
      return
      end
 
c
      subroutine reverse_elements_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *  ierror)
C
C
C ######################################################################
C
C     PURPOSE -  reverse the order of the nodes in itet to get new
c      order
c      call geniee to regenerate the jtet
c
c    reverse_elements/eltset,get,eltsetname
c     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_reflect.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include 'local_element.h'
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C ######################################################################
C
      pointer (ipxtetwd, xtetwd)
      integer xtetwd(*)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitet, itet)
      integer itet(*)
      pointer (ipjtet, jtet1)
      integer jtet1(*)
      pointer (ipmpary,mpary)
      integer mpary(*)
      integer nnodes, ilen, itype,ierr,mbndry,nelements,ipt1,ipt2,ipt3,
     *   mpno,it,j,nefcmo
      character*32 cmo,isubname
      character*132 logmess
c
      isubname='reverse_elements'
C     ******************************************************************
C     Get mesh object.
C
      call cmo_get_name(cmo,ierror)
 
      call cmo_get_intinfo('nnodes', cmo, nnodes, ilen, itype, ierr)
      call cmo_get_intinfo('mbndry', cmo, mbndry, ilen, itype, ierr)
      call cmo_get_intinfo('nelements', cmo, nelements,ilen,itype,ierr)
      if(nnodes.le.0 .or. nelements.le.0) goto 9999
      call cmo_get_intinfo('faces_per_element',cmo,nefcmo,ilen,
     *     itype,ierr)
      call cmo_get_info('xtetwd', cmo, ipxtetwd, ilen, itype, ierr)
C
      call cmo_get_info('itetclr', cmo, ipitetclr,ilen,itype,ierr)
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,itype,ierr)
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtetoff', cmo, ipjtetoff,ilen,itype,ierr)
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierr)
      call cmo_get_info('jtet', cmo, ipjtet, ilen, itype, ierr)
      call mmgetblk('mpary',isubname,ipmpary,nelements,1,ierr)
      ipt1=0
      if (nwds.le.1) then
          ipt1=1
          ipt2=nelements
          ipt3=1
      elseif(nwds.ge.4.and.msgtype(2).eq.1.and.msgtype(3).eq.1.and.
     *    msgtype(4).eq.1) then
          ipt1=imsgin(2)
          ipt2=imsgin(3)
          ipt3=imsgin(4)
	  if (ipt2 .le. 0) ipt2 = nelements
          ipt1=max(1,min(ipt1,nelements))
          ipt2=max(1,min(ipt2,nelements))
          ipt3=max(1,min(ipt3,nelements))
          mpno=0
          do it=ipt1,ipt2,ipt3
             mpno=mpno+1
             mpary(mpno)=it
          enddo
      elseif(nwds.ge.4.and.msgtype(2).eq.3.and.msgtype(3).eq.3.and.
     *    msgtype(4).eq.3) then
          call eltlimc(cmsgin(1),cmsgin(2),cmsgin(4),ipmpary,mpno,
     *      nelements,xtetwd)
      else
          write(logmess,'(a)') ' illegal arguments to reverse_elements'
          call writloga('default',0,logmess,0,ierr)
      endif
c
c  reverse nodes 2 and 3 in all elements in set
c
      do it=1,mpno
         if(itettyp(it).ne.ifelmtet) then
            write(logmess,'(a)') ' illegal element type ',
     *       ' in reverse_elements'
            call writloga('default',0,logmess,0,ierr)
            go to 9999
         endif
         j=itet(itetoff(it)+2)
         itet(itetoff(it)+2)=itet(itetoff(it)+3)
         itet(itetoff(it)+3)=j
      enddo
      call dotask('geniee;finish',ierr)
 
 
 9999 call mmrelprt(isubname,ierr)
      return
      end
c
       subroutine bleed_color_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *   ierror)
C
C
C ######################################################################
C
C     PURPOSE -
C
C    Find faces with ncolor and ncolor+nbleed change itetlcr of all
C    elements with ncolor+nbleed to ncolor -
c    Also change imt1 values
c
c   bleed_color/nbleed
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_reflect.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
 
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
      include "chydro.h"
 
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C ######################################################################
C
      character*32 cmo
      pointer (ipimt1, imt1)
      integer imt1(*)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitet, itet1)
      integer itet1(*)
      pointer (ipjtet, jtet1)
      integer jtet1(*)
c
      pointer(ipclrs,clrs)
      integer clrs(*)
c
      integer j,k,it,ierr,itype,ilen,nnodes,nelements,
     * nefcmo,nbleed,jt,newclr,mbndry,nclrs,jelm,joff
 
      character*32 isubname
C
 
C#######################################################################
 
C     Get the nbleed
C
      if(nwds.le.1) then
         go to 9999
      else
         nbleed=imsgin(2)
 
      endif
 
C     ******************************************************************
C     Get mesh object.
C
      call cmo_get_name(cmo,ierror)
 
      call cmo_get_intinfo('nnodes', cmo, nnodes, ilen, itype, ierr)
      call cmo_get_intinfo('mbndry', cmo, mbndry, ilen, itype, ierr)
      call cmo_get_intinfo('nelements', cmo, nelements,ilen,itype,ierr)
      if(nnodes.le.0 .or. nelements.le.0) goto 9999
      call cmo_get_intinfo('faces_per_element',cmo,nefcmo,ilen,
     *     itype,ierr)
      call cmo_get_info('imt1', cmo, ipimt1, ilen, itype, ierr)
C
      call cmo_get_info('itetclr', cmo, ipitetclr,ilen,itype,ierr)
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,itype,ierr)
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtetoff', cmo, ipjtetoff,ilen,itype,ierr)
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierr)
      call cmo_get_info('jtet', cmo, ipjtet, ilen, itype, ierr)
c
C     ******************************************************************
 
      call mmgetblk('clrs',isubname,ipclrs,nbleed,1,ierr)
C
c  loop through all tets and all faces looking for interface faces
c  between ncolor and ncolor+nbleed
c
      nclrs=0
      do it=1,nelements
         joff=jtetoff(it)
         do j=1,nelmnef(itettyp(it))
            jt=jtet1(joff+j)-mbndry
            if(jt.gt.0) then
               jelm=(1+jt)/nefcmo
               if(abs(itetclr(jelm)-itetclr(it)).eq.nbleed) then
                  newclr=max(itetclr(jelm),itetclr(it))
                  do k=1,nclrs
                     if(clrs(k).eq.newclr) go to 10
                  enddo
                  nclrs=nclrs+1
                  clrs(nclrs)=max(itetclr(jelm),itetclr(it))
 10               continue
               endif
            endif
         enddo
      enddo
c
c  now change colors of all tets whose color is in clrs array
c
      do it=1,nelements
         do j=1,nclrs
            if(itetclr(it).eq.clrs(j)) then
               itetclr(it)=itetclr(it)-nbleed
            endif
         enddo
      enddo
 
 9999 continue
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,ierror)
C
      return
      end
