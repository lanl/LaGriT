      subroutine perturb_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
c
cc #####################################################################
c
c    PURPOSE
c   perturb coordinate of a set of nodes
cc
c    INPUT ARGUMENTS -
c
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
C $Log: perturb_lg.f,v $
C Revision 2.00  2007/11/09 20:03:58  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   06 Feb 2002 12:10:10   dcg
CPVCS    fix calling sequence
CPVCS    
CPVCS       Rev 1.2   30 Jan 2001 08:29:38   dcg
CPVCS    remove duplicate declaration
CPVCS    
CPVCS       Rev 1.1   05 Sep 2000 12:25:36   dcg
CPVCS    use integer variable in call not literal
CPVCS    
CPVCS       Rev 1.0   05 Sep 2000 11:44:18   dcg
CPVCS    Initial revision.
c
c #####################################################################
 
      implicit none
C
      integer nplen
      parameter (nplen=1000000)

C     arguments
      integer ierror,nwds
      integer imsgin(nwds),msgtype(nwds)
      real*8 xmsgin(nwds)
      character*32 cmsgin(nwds)

C     variables
      integer istart,iend,istride,mpno,length,icmotype,
     *   nnodes,i,j,icount,ierr,ierrw
      real*8 factor1,factor2,factor3

C     cmo pointers
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipisetwd, isetwd)
      pointer (ipmpary, mpary)
      pointer (ipiparent, iparent)
      integer itp1(nplen), isn1(nplen)
      integer isetwd(nplen)
      integer mpary(nplen), iparent(nplen)

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(nplen), yic(nplen), zic(nplen)

C     local strings
      character*32 ich1,ich2,ich3,psetname,cmo,isubname
      character*132 logmess
c
c     isubname='perturb'
c     set ierror to error in case we leave early 
      ierror = -1

      factor1=.01
      factor2=.01
      factor3=.01
      istart=1
      iend=0
      istride=0
      ich1=' '
      ich2=' '
      ich3=' '
      psetname='-notset-'

      if(nwds.ge.7.and.msgtype(7).eq.2) factor3=xmsgin(7)
      if(nwds.ge.6.and.msgtype(6).eq.2) factor2=xmsgin(6)
      if(nwds.ge.5.and.msgtype(5).eq.2) factor1=xmsgin(5)
      if(nwds.ge.4.and.msgtype(4).eq.3) then
          psetname=cmsgin(4)
      elseif(nwds.ge.4.and.msgtype(4).eq.1.and.msgtype(3).eq.1
     *   .and.msgtype(2).eq.1) then
          istart=imsgin(2)
          iend=imsgin(3)
          istride=imsgin(4)
      endif
c....
      call cmo_get_name(cmo,ierr)
      write(logmess,'(a,a)') 'perturb ',cmo
      call writloga('default',0,logmess,0,ierrw)
c
c.... Get info from mesh object.
      call cmo_get_intinfo('nnodes',cmo,nnodes,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmo')
      if(nnodes .le. 0)then
        write(logmess,'(a)') 'WARNING: No nodes in mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        ierror = 1
        goto 9000
      endif

      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itp1 cmo')
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info isn1 cmo')
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info isetwd cmo')
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xic cmo')
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info yic cmo')
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info zic cmo')
      if (ierr.ne.0) goto 9000

      call mmgetblk('mpary',isubname,ipmpary,nnodes,1,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'mmgetblk mpary')
      if(ierr.ne.0) goto 9000

      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,ierr)
      call unpackpc(nnodes,itp1,isn1,iparent)
 
C     set the point index boundaries
      mpno=0
      if (psetname.eq.'-notset-') then
          call pntlimn(istart,iend,istride,ipmpary,mpno,
     *        nnodes,isetwd,itp1)
      else
         ich1='pset'
         ich2='get'
         ich3=psetname
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif

c     mpno should be correctly set at this point
      if (mpno.gt.0) then
           write(logmess,'(a,i10)')
     *      'nodes in indexed point set  = ',mpno
           call writloga('default',0,logmess,1,ierrw)
      else
           write(logmess,'(a)') 'No points in indexed point set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9000
      endif

      call perturb_nodes_lg(mpno,mpary,xic,yic,zic,factor1,
     *  factor2,factor3)

c
c  set all children to same value
c
      do i=1,nnodes
         if(iparent(i).ne.i) then
            j=isn1(i)
            icount=0
            do while (j.ne.i.and.j.ne.0.and.icount.lt.10000)
               xic(j)=xic(i)
               yic(j)=yic(i)
               zic(j)=zic(i)
               icount=icount+1
               j=isn1(j)
            enddo
         endif
      enddo

      ierror = 0
      goto 9000
 9000 continue

      if (ierror .ne. 0) call x3d_error(isubname,'Errors.')
      call mmrelprt(isubname,ierr)
      return
      end
 
C     ###################################################################

      subroutine perturb_nodes_lg(n,mpary,x,y,z,factor1,
     *  factor2,factor3)

      implicit none

      integer n,mpary(*)
      real*8 x(*),y(*),z(*),factor1,factor2,factor3
      real*8 ran2_lg,r1,r2,r3,r4

      integer i, ipt,myint, iseed
      myint=-137
      iseed=-myint

      do i=1,n
         ipt=mpary(i)

         r1=factor1*ran2_lg(iseed)
         r2=factor2*ran2_lg(iseed)
         r3=factor3*ran2_lg(iseed)
         r4=ran2_lg(iseed)
         if(r4.lt.0.5) r1=-r1
         r4=ran2_lg(iseed)
         if(r4.lt.0.5) r2=-r2
         r4=ran2_lg(iseed)
         if(r4.lt.0.5) r3=-r3

         x(ipt)=x(ipt)+r1
         y(ipt)=y(ipt)+r2
         z(ipt)=z(ipt)+r3
      enddo

      return
      end
