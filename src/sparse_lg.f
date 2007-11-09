*dk sparse
C #####################################################################
C
C     PURPOSE -
C
C       Remove interior nodes with probability 1-"sparsity",
C       but leaving interior nodes within the specified protection distance
C       of interior interfaces and exterior boundaries,
C       as well as (if the field exists) near where an integer
C       field changes value
C
C       Sparse is useful only if the mesh has a large fraction of the nodes
C       in the interior (and away from the boundaries and interfaces)
C       and you want a quick and dirty way of removing the interior
C       nodes without messing up the interfaces too bad.
C
C     USAGE NOTES -
C
C       if nelements>0 on input, will connect after sparse-ing
C       => only useful if enough points removed so that, eg,
C       connect is significantly faster than massage
C       and also don't care about uniform node density
C       (or trying to interpolate fields).
C
C       if nelements=0 on input, will not be connected and
C       also all interior pts will be used
C
C       Note it may be desireable to smooth afterwards,
C       and the interfaces/exterior may be modified by connect's choices.
C       (The interface/exterior protection supposedly protects against this,
C        and larger values should help more...)
C
C       Note the assumption is that the field values on the children are
C       the ones to compare re field difference (field on parents ignored).
C       Also, it is attempted to recover the child field values
C       lost by "resetpts parents;connect;settets" by storing the
C       isn1 chain prior to connect and attempting to restore it.
C
C     CAVEATS -
C
C       Historically sparse came from the needs of the Potts code run on
C       a large initial mesh to create a final mesh with only a few grains
C       back when massage could not handle large numbers of nodes.
C       In "almost all" cases it is now better to use massage.
C
C     COMMAND FORMAT -
C
C       rmpoint / sparse / pset,get,psetname
C               / sparsity
C               / ext_protect / intrf_protect
C               / field_name / field_diff / field_protect
C               / seed1,seed2
C
C       defaults:
C         pset,get,psetname = 1,0,0
C         sparsity=0.0
C         ext_protect=1
C         intrf_protect=1
C         field_name='-none-'
C         field_diff=0.5
C         field_protect=2
C         seed1=seed2=0 (no reset)
C
C     INPUT ARGUMENTS -
C
C       pset,get,psetname
C          pset info (only interior nodes in pset are dudded)
C       sparsity -
C          fraction of interior,unprotected nodes to keep
C       intrf_protect -
C          if distance=1, interior nodes sharing an element
C             with interface nodes will not be dudded
C          if distance>1, then increase the protection to this many shells
C       ext_protect -
C          if distance=1, interior nodes sharing an element
C             with exterior nodes will not be dudded
C          if distance>1, then increase the protection to this many shells
C       field_name
C          attribute field to protect (field_name='-none-' means don't use)
C       field_diff
C          minimum field value difference to protect
C       field_protect -
C          if distance=1, interior nodes sharing an edge with
C             a node with a value of field differing by
C             at least field_diff will not be dudded
C          if distance>1, then increase the protection to this many shells
C       seed1,seed2 -
C          random number generator seeds
C
C     OUTPUT ARGUMENTS -
C       on output, the mesh will be unconnected
C       n_dud - the number of parent points removed
C
C     CHANGE HISTORY -
C
C   $Log:   /pvcs.config/t3d/src/sparse_lg_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   26 Jan 2000 16:31:32   jtg
CPVCS    fixed log line
CPVCS    
CPVCS       Rev 1.2   Wed Nov 10 11:24:40 1999   dcg
CPVCS    comment out calls to timing _ timing was a noop anyway
CPVCS
CPVCS       Rev 1.1   Wed Sep 01 13:28:46 1999   dcg
CPVCS    fix typo of orig_nnodes to nnodes_orig
CPVCS
CPVCS       Rev 1.0   Thu Jul 29 13:05:46 1999   jtg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   Tue Feb 02 11:33:06 1999   jtg
CPVCS    fixed character dimension from 32 to "*"
CPVCS
CPVCS       Rev 1.1   Mon Dec 21 20:37:20 1998   jtg
CPVCS    lots of small changes to make it do what I need,
CPVCS    but I am not sure how to summarize.
CPVCS
CPVCS       Rev 1.0   Thu May 14 15:41:46 1998   jtg
CPVCS    Initial revision.
C
C #####################################################################
 
c ........................................................................
 
C #####################################################################
        subroutine sparse_cmd_lg(imsgin,xmsgin,cmsgin,msgtyp
     &      ,nwds,ierror)
 
        implicit none
        include 'chydro.h'
 
        integer nwds, imsgin(nwds), msgtyp(nwds), ierror
        real*8 xmsgin(nwds)
        character*(*) cmsgin(nwds)
 
        pointer (ip_mpary,mpary),(ip_ifield,ifield)
     &         ,(ip_itp1,itp1),(ip_isetwd,isetwd)
     &         ,(ip_isn1,isn1)
        integer mpary(*),ifield(*)
     &         ,itp1(*),isetwd(*),isn1(*)
 
        pointer (ip_rfield,rfield)
        real*8 rfield(*)
 
        real*8 sparsity,field_min,field_diff
        integer intrf_protect,ext_protect,field_protect
     &          ,mpno,iwd,ierr,npt,nch,len,ityp,i,j,nnodes
     &          ,ip_field,ipt(3),seed(2),ifield_min,index
 
        character*132 cbuf
        character*32 isubname,ich(3),ctype,clength
     &              ,cmo,field_name,crank,cinter,cpers,cio
c -----------------------------------------------------------------
 
        isubname='sparse_com_lg'
 
c.... translate command words following root that got one here
 
        iwd=2
 
c.... get cmo name
 
        call cmo_get_name(cmo,ierr)
        if (ierr.ne.0) goto 9999
 
C.... set the point index boundaries
 
        mpno=0
        if (nwds.lt.iwd+3) then
           iwd=iwd+3
        elseif (msgtyp(iwd+1).eq.3.and.cmsgin(4)(1:5).eq.'-def-') then
           iwd=iwd+3
        else
           npt=0
           nch=0
           do j=1,3
              iwd=iwd+1
              ich(j)=' '
              ipt(j)=0
              if (msgtyp(iwd) .eq. 1) then
                 ipt(j)=imsgin(iwd)
                 npt=npt+1
              elseif (msgtyp(iwd) .eq. 2) then
                 ipt(j)=int(xmsgin(iwd)+1.d-6)
                 npt=npt+1
              elseif (msgtyp(iwd) .eq. 3) then
                 ich(j)=cmsgin(iwd)
                 nch=nch+1
              endif
           enddo
           if (nch.eq.3.or.npt.eq.3) then
              call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
              if (ierr .ne. 0) goto 9999
              call mmggetbk('mpary',isubname,ip_mpary,nnodes,1,ierr)
              if (ierr .ne. 0) goto 9999
              call cmo_get_info('isetwd',cmo,ip_isetwd,len,ityp,ierr)
              if (ierr .ne. 0) goto 9999
              call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
              if (ierr .ne. 0) goto 9999
              if (npt.eq.3) then
                 call pntlimn(ipt(1),ipt(2),ipt(3),ip_mpary,mpno,
     &            nnodes,isetwd,itp1)
              else ! if (nch.eq.3) then
                 call pntlimc(ich(1),ich(2),ich(3),ip_mpary,mpno,
     &            nnodes,isetwd,itp1)
              endif
              if (mpno.le.0) goto 9999
           endif
        endif
        if (mpno.eq.0) then
           call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
           if (ierr .ne. 0) goto 9999
           call mmggetbk('mpary',isubname,ip_mpary,nnodes,1,ierr)
           if (ierr .ne. 0) goto 9999
           do i=1,nnodes
              mpary(i)=0
           enddo
        else
           do i=1,mpno
              isetwd(i)=mpary(i)
              mpary(i)=-1
           enddo
           do i=mpno+1,nnodes
              mpary(i)=-1
           enddo
           do i=1,mpno
              j=isetwd(i)
              mpary(j)=0
           enddo
           ! if parents in mpary, then so are all children
           call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
           if (ierr .ne. 0) goto 9999
           do i=1,nnodes
              if (itp1(i).eq.ifitpcup.and.mpary(i).eq.0) then
                 j=isn1(i)
                 do while (j.ne.i.and.j.ne.0)
                    mpary(j)=0
                    j=isn1(j)
                 enddo
              endif
           enddo
        endif
 
c.... find sparsity (probability to keep)
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           sparsity=0.d0
        elseif (msgtyp(iwd) .eq. 1) then
           sparsity=dble(imsgin(iwd))
        elseif (msgtyp(iwd) .eq. 2) then
           sparsity=xmsgin(iwd)
        else
           sparsity=0.d0
        endif
 
c.... find exterior protection distance
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           ext_protect=1
        elseif (msgtyp(iwd) .eq. 1) then
           ext_protect=imsgin(iwd)
        elseif (msgtyp(iwd) .eq. 2) then
           ext_protect=int(xmsgin(iwd)+1.d-6)
        else
           ext_protect=1
        endif
 
c.... find interface protection distance
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           intrf_protect=1
        elseif (msgtyp(iwd) .eq. 1) then
           intrf_protect=imsgin(iwd)
        elseif (msgtyp(iwd) .eq. 2) then
           intrf_protect=int(xmsgin(iwd)+1.d-6)
        else
           intrf_protect=1
        endif
 
c.... find field name to protect (must be attribute)
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           field_name='-none-'
        elseif (msgtyp(iwd) .eq. 3) then
           field_name=cmsgin(iwd)
        else
           field_name='-none'
        endif
 
c.... find field diff
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           field_diff=0.5d0
        elseif (msgtyp(iwd) .eq. 1) then
           field_diff=dble(imsgin(iwd))
        elseif (msgtyp(iwd) .eq. 2) then
           field_diff=xmsgin(iwd)
        else
           field_diff=0.5d0
        endif
 
c.... find field protection distance
 
        iwd=iwd+1
        if (nwds.lt.iwd) then
           field_protect=2
        elseif (msgtyp(iwd) .eq. 1) then
           field_protect=imsgin(iwd)
        elseif (msgtyp(iwd) .eq. 2) then
           field_protect=int(xmsgin(iwd)+1.d-6)
        else
           field_protect=2
        endif
 
c.... reset field protection distance to zero if no field
c otherwise create integer field with properties:
c   negative: not in pset
c   zero: in pset, but no diff to neighbors
c   positve: in pset, if differs by > field_diff, then protect
c (place in mpary)
 
        if (field_diff.le.0.d0) field_protect=0
 
        if (field_name.eq.'-none-'.or.field_protect.le.0) then
           field_protect=0
        else
           call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
           if (ierr .ne. 0) field_protect=0
           call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
           if (ierr .ne. 0) goto 9999
           call cmo_get_info(field_name,cmo,ip_field,len,ityp,ierr)
           if (ierr .ne. 0) field_protect=0
           call cmo_get_attparam(field_name,cmo,index,ctype,crank
     &       ,clength,cinter,cpers,cio,ierr)
           if (ierr.ne.0
     &       .or.(clength.ne.'nnodes'.and.clength.ne.'NNODES'))
     &           field_protect=0
           if (field_protect.eq.0) goto 100
           if (ctype.eq.'VDOUBLE'.or.ctype.eq.'vdouble') then
              ip_rfield=ip_field
              if (itp1(1).ne.ifitpdud.and.itp1(1).ne.ifitpmrg
     &           .and.itp1(1).ne.ifitpcup) then
                 field_min=rfield(1)
              else
                 field_min=1.d99
              endif
              do i=1,nnodes
                 if (rfield(i).lt.field_min
     &              .and.itp1(i).ne.ifitpdud.and.itp1(i).ne.ifitpmrg
     &              .and.itp1(i).ne.ifitpcup) field_min=rfield(i)
              enddo
              do i=1,nnodes
                 if (mpary(i).ge.0)
     &             mpary(i)=1+int((rfield(i)-field_min)/field_diff)
              enddo
              field_diff=0.5d0
           elseif (ctype.eq.'VINT'.or.ctype.eq.'vint') then
              ip_ifield=ip_field
              if (itp1(1).ne.ifitpdud.and.itp1(1).ne.ifitpmrg
     &           .and.itp1(1).ne.ifitpcup) then
                 ifield_min=ifield(1)
              else
                 ifield_min=10000000
              endif
              do i=1,nnodes
                 if (ifield(i).lt.ifield_min
     &              .and.itp1(i).ne.ifitpdud.and.itp1(i).ne.ifitpmrg
     &              .and.itp1(i).ne.ifitpcup) ifield_min=ifield(i)
              enddo
              ifield_min=ifield_min-1
              do i=1,nnodes
                 if (mpary(i).ge.0)
     &               mpary(i)=ifield(i)-ifield_min
              enddo
           else
              field_protect=0
           endif
        endif
        if (field_protect.eq.0) goto 100
 
c.... find random seeds
 
100     do j=1,2
           iwd=iwd+1
           if (nwds.lt.iwd) then
              seed(j)=0
           elseif (msgtyp(iwd) .eq. 1) then
              seed(j)=imsgin(iwd)
           elseif (msgtyp(iwd) .eq. 2) then
              seed(j)=int(xmsgin(iwd)+1.d-6)
           else
              seed(j)=0
           endif
        enddo
        if (seed(1).gt.0.and.seed(2).gt.0) then
           call seed_rand_lg(seed(1),seed(2))
        endif
 
c.... sparse (ie, do all the real work)
 
        call sparse_lg(sparsity,intrf_protect,ext_protect
     &          ,field_protect,mpary,field_diff,cmo,ierror)
        call mmrelprt(isubname,ierr)
 
        cbuf='rmpoint compress; finish'
        call dotaskx3d(cbuf,ierr)
 
c.... normal return
 
        return
 
c.... error return
 
9999    ierror=1
        write(cbuf,*)
     &      'SPARSE WARNING: get error or no nodes in pset'
        call writloga('default',0,cbuf,0,ierr)
        call mmrelprt(isubname,ierr)
        return
 
        end
 
C #####################################################################
        subroutine sparse_lg(sparsity,intrf_protect,ext_protect
     &          ,field_protect,ifield,field_diff,cmo,ierror)
 
        implicit none
 
        include 'local_element.h'
        include 'chydro.h'
 
        real*8 sparsity,field_diff
        integer intrf_protect,ext_protect
     &         ,field_protect,ierror
 
        integer ifield(*)
        character*(*) cmo
 
        pointer (ip_itet,itet),(ip_itetoff,itetoff)
     &         ,(ip_jtet,jtet),(ip_jtetoff,jtetoff)
     &         ,(ip_itettyp,itettyp),(ip_st_isn1,st_isn1)
     &         ,(ip_itp1,itp1),(ip_isn1,isn1)
     &         ,(ip_iparent,iparent),(ip_imt1,imt1)
        integer itet(*),itetoff(*),jtet(*),jtetoff(*)
     &         ,itettyp(*),itp1(*),isn1(*),imt1(*)
     &         ,iparent(*),st_isn1(*)
 
        integer nnodes,nelements,mbndry,local_debug
     &          ,n_dud,ityp,len,ierr
     &          ,i,k,iel,i1,i2,j1,j2,dpro
     &          ,ioff,joff,itpoff,itpoff2,ipro
     &          ,nnodes_orig,n_isn,n_extra
 
        character*132 cbuf
        character*32 isubname
 
        real*8 rand_lg
        external rand_lg
 
c -----------------------------------------------------------------
c note as far as this subroutine is concerned, ifield is both
c the field to protect difference of (if positive)
c and a mask for points to ignore when dudding (if negative)
c -----------------------------------------------------------------
 
c ........... (begin) .............
 
        isubname='sparse_lg'
        local_debug=0
        n_dud=0
        if (sparsity.ge.1.d0) goto 1000 ! no nodes will be dudded
 
        if (local_debug.gt.0) then
         write(cbuf,*) 'DEBUG: start sparsity'
         call writloga('default',0,cbuf,0,ierr)
         write(cbuf,*) 'SPARSE WARNING: "big dips"'
     &                  //' if bdry layer does not protect enough...'
         call writloga('default',0,cbuf,0,ierr)
c        call timing()
        endif
 
c ........... (get initial data) .............
 
        call cmo_get_name(cmo,ierr)
        if (ierr.ne.0) goto 9999 ! stop 'SPARSE: cmo_get_name cmo'
 
        call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
        if (ierr.ne.0) goto 9999 ! stop 'SPARSE: cmo_get_info nnodes'
        call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999 ! stop 'SPARSE: cmo_get_info itp1'
        call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999 ! stop 'SPARSE: cmo_get_info isn1'
 
        call mmggetbk('iparent',isubname,ip_iparent,nnodes,1,ierr)
        if (ierr.ne.0) goto 9999
        call unpackpc(nnodes,itp1,isn1,iparent)
 
        call cmo_get_info('nelements',cmo,nelements,len,ityp,ierr)
        if (ierr.ne.0.or.nelements.lt.1) nelements=0
        call cmo_get_info('mbndry',cmo,mbndry,len,ityp,ierr)
        if (ierr.ne.0) mbndry=0
        call cmo_get_info('itet',cmo,ip_itet,len,ityp,ierr)
        if (ierr.ne.0) nelements=0
        call cmo_get_info('itetoff',cmo,ip_itetoff,len,ityp,ierr)
        if (ierr.ne.0) nelements=0
        call cmo_get_info('jtet',cmo,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) nelements=0
        call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) nelements=0
        call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) nelements=0
 
c itpoff is used to separate itp's protected by differnt things
C -> should be greater than max allowed itp
c (100 would be OK, but set to 1000 "to be safe" re possible more allowed itp)
 
        itpoff=1000
        itpoff2=2*itpoff
 
c ........... (protect ifield) .............
 
        do ipro=1,3
 
c ........... (protect field) .............
           if (ipro.eq.1) then ! protect fields
 
              if (field_protect.lt.1
     &         .or.nelements.le.0) goto 100
 
              ! protect ifield bdrys:
              ! don't "sparse" nodes if neighbor has different "ifield"
              ! set ipt1 to "-itpoff-itp1" to flag as protected
 
              do iel=1,nelements
                 ityp=itettyp(iel)
                 ioff=itetoff(iel)
                 do j1=1,nelmnee(ityp)
                    i1=iparent(itet(ielmedge1(1,j1,ityp)+ioff))
                    i2=iparent(itet(ielmedge1(2,j1,ityp)+ioff))
                    if (dble(abs(ifield(i1)-ifield(i2))).gt.field_diff
     &                 .and.ifield(i1).gt.0.and.ifield(i2).gt.0) then
                       if (itp1(i1).ge.0) itp1(i1)=-itpoff2-itp1(i1)
                       if (itp1(i2).ge.0) itp1(i2)=-itpoff2-itp1(i2)
                    endif
                 enddo
              enddo
              dpro=field_protect-1
 
c ........... (protect exterior) .............
           elseif (ipro.eq.2) then ! protect exterior
 
 
              if (ext_protect.lt.1.or.nelements.le.0) goto 100
 
              ! protect nodes near external boundaries
              ! only interior nodes have itp1<0 at this point
              ! -> don't have to worry about for marking exterior nodes
              !    (or protect unless adding at least 1 layer)
 
              ! mark the exterior
              do iel=1,nelements
                 ityp=itettyp(iel)
                 joff=jtetoff(iel)
                 ioff=itetoff(iel)
                 do j1=1,nelmnef(ityp)
                    if (jtet(j1+joff).eq.mbndry) then
                       do j2=1,ielmface0(j1,ityp)
                          i1=itet(ielmface1(j2,j1,ityp)+ioff)
                          i1=iparent(i1)
                          if (itp1(i1).ge.0) itp1(i1)=-itpoff2-itp1(i1)
                       enddo
                    endif
                 enddo
              enddo
 
              dpro=ext_protect
 
c ........... (protect interior interfaces) .............
           elseif (ipro.eq.3) then ! protect interfaces
 
              if (intrf_protect.lt.1.or.nelements.le.0) goto 100
 
              ! protect nodes near interior interfaces
              ! only interior nodes have itp1<0 at this point
              ! -> don't have to worry about for interface nodes
              !    (or protect unless adding at least 1 layer)
 
              ! mark the interior interfaces
              do iel=1,nelements
                 ityp=itettyp(iel)
                 joff=jtetoff(iel)
                 ioff=itetoff(iel)
                 do j1=1,nelmnef(ityp)
                    i1=jtet(j1+joff)
                    if ((i1.gt.mbndry.and.mbndry.gt.0)
     &                     .or.(i1.lt.0.and.mbndry.eq.0)) then
                       do j2=1,ielmface0(j1,ityp)
                          i1=itet(ielmface1(j2,j1,ityp)+ioff)
                          i1=iparent(i1)
                          if (itp1(i1).ge.0) itp1(i1)=-itpoff2-itp1(i1)
                       enddo
                    endif
                 enddo
              enddo
 
              dpro=intrf_protect
 
           endif
 
c ........... (increase protection) .............
c ipt: -itpoff2=current layer, -1=next layer, -itpoff=previous marking
 
           do k=1,dpro
              ! mark next layer
              do iel=1,nelements
                 ityp=itettyp(iel)
                 ioff=itetoff(iel)
c                ! sharing element .......
                 do j1=1,nelmnen(ityp)
                    i1=iparent(itet(j1+ioff))
                    if (itp1(i1).le.-itpoff2) goto 50
                 enddo
                 goto 60
50               do j1=1,nelmnen(ityp)
                    i1=iparent(itet(j1+ioff))
                    if (itp1(i1).ge.0) then
                       itp1(i1)=-1-itp1(i1)
                    elseif (itp1(i1).le.-itpoff
     &                      .and.itp1(i1).gt.-itpoff2) then
                       itp1(i1)=itp1(i1)+itpoff-1
                    endif
                 enddo
60               continue
c$$              ! sharing face .......
c$$              do j1=1,nelmnen(ityp)
c$$                 i1=iparent(itet(j1+ioff))
c$$                 if (itp1(i1).le.-itpoff2) then
c$$                 if (itp1(i1).le.-itpoff2) then
c$$                    do j2=1,ielmnode0(j1,ityp)
c$$                       i2=ielmnode1(j2,j1,ityp)
c$$                       i2=iparent(itet(i2+ioff))
c$$                       if (itp1(i2).ge.0) then
c$$                          itp1(i2)=-1-itp1(i2)
c$$                       elseif (itp1(i2).le.-itpoff
c$$  &                            .and.itp1(i2).gt.-itpoff2) then
c$$                          itp1(i2)=itp1(i2)+itpoff-1
c$$                       endif
c$$                    enddo
c$$                 endif
c$$              enddo
c$$              ! .......
              enddo
              ! reset marking for following layer
              do i=1,nnodes
                 if (itp1(i).le.-itpoff2) then
                     itp1(i)=itp1(i)+itpoff2-1
                 elseif (itp1(i).lt.0.and.itp1(i).gt.-itpoff) then
                     itp1(i)=itp1(i)-itpoff2+1
                 endif
              enddo
           enddo
 
           ! reset itp1 for non-interior nodes
           ! (leave all virtual nodes -> don't need to protect)
           ! mark interior nodes only, with -iptoff
           do i=1,nnodes
              i1=itp1(i)
              if (i1.le.-itpoff2) then
                 if (i1.ne.-itpoff2-ifitpint) then
                    itp1(i)=-itpoff2-i1
                 else
                    itp1(i)=i1+itpoff
                 endif
              elseif (i1.le.-itpoff) then
                 if (i1.ne.-itpoff-ifitpint) itp1(i)=-itpoff-i1
              elseif (i1.lt.0) then
                 if (i1.ne.-1-ifitpint) then
                    itp1(i)=-1-i1
                 else
                    itp1(i)=i1+1-itpoff
                 endif
              endif
           enddo
 
 
100        continue
 
        enddo
 
c ........... (dud/sparse) .............
c dud nodes:
c sparsity = fraction of nodes to keep
c Note: using this algorithm really only makes sense
c   if "most" of the nodes duded (re cost of "connect")
 
        ! initalize counter (count to see if did anything)
        n_dud=0
 
        if (sparsity.le.0.d0) then
 
            ! dud all unprotected interior nodes with positive ifield
 
            do i=1,nnodes
               if (itp1(i).eq.ifitpint.and.ifield(i).ge.0) then
                  itp1(i)=ifitpdud
                  n_dud=n_dud+1
               endif
            enddo
 
        else
 
            ! sparse unprotected interior nodes with positive ifield
            ! keep with probability=sparsity
 
            do i=1,nnodes
               if (itp1(i).eq.ifitpint.and.ifield(i).ge.0) then
                  if (rand_lg().gt.sparsity) then
                     itp1(i)=ifitpdud
                     n_dud=n_dud+1
                  endif
               endif
            enddo
 
        endif
 
        ! restore ipt1 value for marked interior pts
        do i=1,nnodes
          if (itp1(i).lt.0) itp1(i)=-itpoff-itp1(i)
        enddo
 
        write(cbuf,*) 'sparsity removed ',n_dud,' interior pts'
        call writloga('default',0,cbuf,0,ierr)
 
        if (n_dud.eq.0) goto 1000 ! sparsity did not remove any pts
 
c ........... (connect) .............
c Note: need to store isn1, resetpts, and restore children afterwards
c (re field values) as connect only works if all parents ....
 
        if (nelements.eq.0) goto 1000 ! wasn't connected on input...
 
        if (local_debug.gt.0) then
          write(*,*) 'DEBUG: sparse - starting connect'
c         call timing()
        endif
 
        n_isn=0
        do i=1,nnodes
           if (isn1(i).ne.0.and.itp1(i).ne.ifitpdud
     &                     .and.itp1(i).ne.ifitpmrg) then
              n_isn=1
              goto 500
           endif
        enddo
500     if (n_isn.ne.0) then
           call mmggetbk('st_isn1',isubname,ip_st_isn1,nnodes,1,ierr)
           nnodes_orig=nnodes
           do i=1,nnodes
              st_isn1(i)=isn1(i)
           enddo
           write(cbuf,*) 'resetpts parents; finish'
           call dotaskx3d(cbuf,ierr)
        endif
 
        write(cbuf,*) 'connect; settets; finish'
        call dotaskx3d(cbuf,ierr)
 
        if (n_isn.eq.0) then
           ! just in case was multimatl....
           write(cbuf,*) 'resetpts parents; finish'
           call dotaskx3d(cbuf,ierr)
        else
           call mmfindbk('st_isn1',isubname,ip_st_isn1,len,ierr)
           call cmo_get_info('nelements',cmo,nelements,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('imt1',cmo,ip_imt1,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itet',cmo,ip_itet,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itetoff',cmo,ip_itetoff,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call mmggetbk('iparent',isubname,ip_iparent,nnodes,1,ierr)
           if (ierr.ne.0) goto 9999
           call unpackpc(nnodes,itp1,isn1,iparent)
           do iel=1,nelements
              ityp=itettyp(iel)
              ioff=itetoff(iel)
              do i=1,nelmnen(ityp)
                 i1=itet(ioff+i)
                 if (i1.gt.nnodes_orig) then
                    ! find child of same color in st_isn chain
                    i2=iparent(i1)
                    if (i2.gt.nnodes_orig) goto 9999
                    j1=st_isn1(i2)
                    do while (j1.gt.0.and.j1.ne.i2
     &                        .and.j1.le.nnodes_orig)
                       if (imt1(j1).eq.imt1(i1)) then
                          ! replace the itet entry with the old value
                          itet(ioff+i)=j1
                          ! fix the old point type
                          if (itp1(i1).ge.0) then
                             itp1(j1)=itp1(i1)
                             itp1(i1)=-1-itp1(i1)
                             ! insert old value into new isn1 chain
                             if (isn1(j1).ne.0) goto 9999
                             j2=i2
                             do while (isn1(j2).ne.i1)
                                j2=isn1(j2)
                                if (j2.eq.i2.or.j2.eq.0) goto 9999
                             enddo
                             isn1(j2)=j1
                             isn1(j1)=isn1(i1)
                             isn1(i1)=0
                          else
                             itet(ioff+i)=j1
                             itp1(j1)=-1-itp1(i1)
                          endif
                          goto 70
                       endif
                       j1=st_isn1(j1)
                    enddo
                    ! not in original chain: just live with .... ! goto 9999
70                  continue
                 endif
              enddo
           enddo
           n_extra=0
           do i=nnodes_orig+1,nnodes
              if (itp1(i).ge.0) then
                 n_extra=n_extra+1
              else
                 itp1(i)=ifitpdud
              endif
           enddo
           if (n_extra.gt.0) then
              write(cbuf,*) 'SPARSE: cannot interpolate ',n_extra
     &                 ,' nodes'
              call writloga('default',0,cbuf,0,ierr)
           endif
        endif
 
c ........... (release temp memory) .............
1000    call mmrelprt(isubname,ierr)
 
c ........... (compress) .............
c no: leave for user to do afterwards if desired
c (for command form, do in sparse_com_lg subroutine after call to this)
c$$     cbuf='rmpoint compress; finish'
c$$     call dotaskx3d(cbuf,ierr)
 
c ........... (normal return) .............
        if (local_debug.gt.0) then
           write(cbuf,*)
     &       'DEBUG: releasing memory and returning from sparse'
           call writloga('default',0,cbuf,0,ierr)
c          call timing()
        endif
 
        ierror=0
        return
 
c ........... (error return) .............
9999    write(cbuf,*) 'SPARSE ERROR: aborting - mesh may be invalid'
        call mmrelprt(isubname,ierr)
        ierror=1
        return
 
        end
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
