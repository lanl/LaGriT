      subroutine rzamr_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C       this routine will take an existing rzbrick type grid and use
C       octree refinement for all elements that have at least one node
C       inside the specified region.  The routine loops over the number
C       of levels of refinement specified in the command.
C
C     INPUT ARGUMENTS -
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     FORMAT -
C       RZAMR/region_name/number_of_levels
C       RZAMR/R1/3  will refine region r1 3 levels
C
C     DEFAULTS -
C       empty region_name field means all regions
C       the default number_of_levels is one
C
C     CHANGE HISTORY -
C
C        $Log: rzamr_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   02 Nov 2005 09:37:12   gable
CPVCS    Fixed syntax of format statement 110.
CPVCS    
CPVCS       Rev 1.3   Thu Apr 06 14:00:54 2000   dcg
CPVCS    fix ipointj retrieval
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 11:13:52 2000   dcg
CPVCS    use geom_name to access region data arrays
CPVCS
CPVCS       Rev 1.1   11 Feb 2000 09:04:36   gable
CPVCS    Code will exit if nlevel = 0. Added warning message when
CPVCS    changing nlevel > 5 to 5.
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 13:56:00 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.0   Wed Jun 30 11:44:58 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
 
      implicit none
 
      include 'chydro.h'
      include 'consts.h'
      include 'local_element.h'
      include 'geom_lg.h'
c
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipitp1, itp1)
      pointer (ipimt1, imt1)
      pointer (ipicr1, icr1)
      integer itp1(*),imt1(*),icr1(*),itet(*),itetoff(*),
     *  itettyp(*)
      pointer (ipireg,ireg)
      pointer (ipisrf,isrf)
      integer ireg(*),isrf(*)
c
      character*32 cmo, isubname,regname,cregion,geom_name
      character*132 buf
      integer icharlnf
      real *8 epsln,rout
      pointer(ipout,out)
      real*8 out(*)
c
      integer ierr,icscode,ipointi,ipointj,ilen,ityp
     &  ,nnodes,nlevels,i,ir,len1,len2,len,nl,iregck,nelements,
     *   ic,nnodes_save,nnodes_new,nelements_save,nelements_new,
     *   nt1,nt2,ne,nf,nelements_orig,nnodesmax,nelementsmax,nt,
     *   itype,nn,i1,i2,i3,i4,newtet,iout
c
C#######################################################################
      isubname='rzamr_lg'
      cregion='region'
      ierr=0
      call get_epsilon('epsilonl',epsln)
c
c  get info from command line - region name and number of levels
c
      regname='-all-'
      nlevels=1
      if(nwds.ge.2) then
         regname=cmsgin(2)
         if(regname.eq.'-def-') regname='-all-'
         if(nwds.ge.3) then
            nlevels=imsgin(3)
            if( nlevels.le.0) then
              write(buf,8)
   8  format('WARNING:creatpts/amr/ nlevel = 0 , no refinement')
              call writloga('default',0,buf,0,icscode)
              go to 9999
           endif
           if( nlevels .gt. 5) then
              write(buf,9)
   9  format('WARNING:creatpts/amr/ nlevel > 5 , setting nlevel = 5')
              call writloga('default',0,buf,0,icscode)
              nlevels=min(nlevels,5)
           endif
         endif
      endif
c
C get some initial info from mesh object - name, current node number
c
      call cmo_get_name(cmo,ierr)
      if (ierr.ne.0) then
         ierr=1
         goto 9999
      endif
      call cmo_get_info('ipointi',cmo,
     &                ipointi,ilen,itype,ierr)
      if (ierr.ne.0) then
         ierr=2
         goto 9999
      endif
      call cmo_get_info('ipointj',cmo,ipointj,ilen,itype,ierr)
      if (ierr.ne.0) then
         ierr=3
         goto 9999
      endif
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=4
         goto 9999
      endif
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=4
         goto 9999
      endif
c
C get mesh object pointers parent nodes
c
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=3
        goto 9999
      endif
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=4
        goto 9999
      endif
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9999
      endif
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9999
      endif
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9999
      endif
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9999
      endif
c
C     SET THE REGION NAMES, OFFSETS, AND NUMBER OF ELEMENTS FOR ALL
C     REGIONS.  ALSO GET SURFACE NAMES AND POINTERS
C
      if(regname.eq.'-all-') then
         do i=1,nnodes
            ireg(1)=1
         enddo
      else
         call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *     ipout,ilen,ityp,ierr)
         call mmfindbk('cregs',geom_name,ipcregs,ilen,ierr)
         do ir=1,nregs
            len2=icharlnf(cregs(ir))
            len1=icharlnf(regname)
            len=max(len1,len2)
            if (regname(1:len1) .eq. cregs(ir)(1:len)) then
               iregck=ir
               go to 10
            endif
         enddo
 10      call mmgetblk('ireg',isubname,ipireg,nnodes,1,icscode)
         call mmgetblk('isrf',isubname,ipisrf,nnodes,1,icscode)
         call getregv(xic,yic,
     $                     zic,nnodes,
     &                     epsln,cregion,iregck,
     $                     ireg,isrf,icscode)
      endif
c
c  make some space for new nodes and new elements
c
      nnodes_save=nnodes
      nelements_save=nelements
      nelements_orig=nelements
      nnodesmax=1.5*nnodes
      nelementsmax=1.5*nelements
      call cmo_set_info('nnodes',cmo,nnodesmax,1,1,icscode)
      call cmo_set_info('nelements',cmo,nelementsmax,1,1,icscode)
      call cmo_newlen(cmo,icscode)
      if(icscode.ne.0) go to 9999
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
C
C loop over number of levels of refinement desired
c
      nt1=1
      nt2=nelements_save
      do nl=1,nlevels
c
c   loop over new hexes
c
         nnodes_new=0
         nelements_new=0
         do nt=nt1,nt2
            itype=itettyp(nt)
            if(itype.ne.ifelmhex) go to 9999
            do nn=1,nelmnen(itettyp(nt))
               if(ireg(itet(itetoff(nt)+nn)).ne.0) go to 20
            enddo
            go to 100
c
c   refine this element
c
c   check if enough memory
c
 20         ic=0
            if(nnodes_save+nnodes_new+19.gt.nnodesmax) then
               nnodesmax=nnodesmax+1000
               call cmo_set_info('nnodes',cmo,nnodesmax,1,1,icscode)
               ic=1
            endif
            if(nelements_save+nelements_new+1.gt.nelementsmax) then
               nelementsmax=nelementsmax+1000
               call cmo_set_info('nelements',cmo,nelementsmax,1,1,
     *                          icscode)
               ic=1
            endif
            if (ic.eq.1)  then
               call cmo_newlen(cmo,icscode)
               if(icscode.ne.0) go to 9999
               call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
               call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
               call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
               call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
               call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
               call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
            endif
c
c   add  midpoints of all edges, midpoint of all faces and midpoint of element
c
            do ne=1,nelmnee(itype)
               i1=itet(itetoff(nt)+ielmedge1(1,ne,itype))
               i2=itet(itetoff(nt)+ielmedge1(2,ne,itype))
               nnodes_new=nnodes_new+1
               xic(nnodes_save+nnodes_new)=.5*(xic(i1)+xic(i2))
               yic(nnodes_save+nnodes_new)=.5*(yic(i1)+yic(i2))
               zic(nnodes_save+nnodes_new)=.5*(zic(i1)+zic(i2))
            enddo
            do nf=1,nelmnef(itype)
               i1=itet(itetoff(nt)+ielmface1(1,nf,itype))
               i2=itet(itetoff(nt)+ielmface1(2,nf,itype))
               i3=itet(itetoff(nt)+ielmface1(3,nf,itype))
               i4=itet(itetoff(nt)+ielmface1(4,nf,itype))
               nnodes_new=nnodes_new+1
               xic(nnodes_save+nnodes_new)=.25*(xic(i1)+xic(i2)+xic(i3)
     *               +xic(i4))
               yic(nnodes_save+nnodes_new)=.25*(yic(i1)+yic(i2)+yic(i3)
     *               +yic(i4))
               zic(nnodes_save+nnodes_new)=.25*(zic(i1)+zic(i2)+zic(i3)
     *               +zic(i4))
            enddo
            nnodes_new=nnodes_new+1
            xic(nnodes_save+nnodes_new)=.125*(xic(itet(itetoff(nt)+1))+
     *        xic(itet(itetoff(nt)+2))+ xic(itet(itetoff(nt)+3))+
     *        xic(itet(itetoff(nt)+4))+ xic(itet(itetoff(nt)+5))+
     *        xic(itet(itetoff(nt)+6))+ xic(itet(itetoff(nt)+7))+
     *        xic(itet(itetoff(nt)+8)))
            yic(nnodes_save+nnodes_new)=.125*(yic(itet(itetoff(nt)+1))+
     *        yic(itet(itetoff(nt)+2))+ yic(itet(itetoff(nt)+3))+
     *        yic(itet(itetoff(nt)+4))+ yic(itet(itetoff(nt)+5))+
     *        yic(itet(itetoff(nt)+6))+ yic(itet(itetoff(nt)+7))+
     *        yic(itet(itetoff(nt)+8)))
            zic(nnodes_save+nnodes_new)=.125*(zic(itet(itetoff(nt)+1))+
     *        zic(itet(itetoff(nt)+2))+ zic(itet(itetoff(nt)+3))+
     *        zic(itet(itetoff(nt)+4))+ zic(itet(itetoff(nt)+5))+
     *        zic(itet(itetoff(nt)+6))+ zic(itet(itetoff(nt)+7))+
     *        zic(itet(itetoff(nt)+8)))
c
c  now add the new elements
c
c top left front
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-16
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-4
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-1
            itet(itetoff(newtet)+5)=itet(itetoff(nt)+5)
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-10
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-5
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-9
            itettyp(newtet)=ifelmhex
c bottom left front
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=itet(itetoff(nt)+1)
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-18
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-6
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-17
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-16
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-4
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-1
            itettyp(newtet)=ifelmhex
c top right front
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-4
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-14
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-3
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-10
            itet(itetoff(newtet)+6)=itet(itetoff(nt)+6)
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-8
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-5
            itettyp(newtet)=ifelmhex
c bottom right front
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-18
            itet(itetoff(newtet)+2)=itet(itetoff(nt)+2)
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-15
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-6
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-4
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-14
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-3
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new
            itettyp(newtet)=ifelmhex
c top left back
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *               itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-1
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-2
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-11
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-9
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-5
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-7
            itet(itetoff(newtet)+8)=itet(itetoff(nt)+8)
            itettyp(newtet)=ifelmhex
c bottom left back
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-17
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-6
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-13
            itet(itetoff(newtet)+4)=itet(itetoff(nt)+4)
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-1
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-2
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-11
            itettyp(newtet)=ifelmhex
c top right back
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=
     *                 itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-3
            itet(itetoff(newtet)+3)=nnodes_save+nnodes_new-12
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-2
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new-5
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-8
            itet(itetoff(newtet)+7)=itet(itetoff(nt)+7)
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-7
            itettyp(newtet)=ifelmhex
c bottom right back
            nelements_new=nelements_new+1
            newtet=nelements_save+nelements_new
            itetoff(newtet)=itetoff(newtet-1)+8
            itet(itetoff(newtet)+1)=nnodes_save+nnodes_new-6
            itet(itetoff(newtet)+2)=nnodes_save+nnodes_new-15
            itet(itetoff(newtet)+3)=itet(itetoff(nt)+3)
            itet(itetoff(newtet)+4)=nnodes_save+nnodes_new-13
            itet(itetoff(newtet)+5)=nnodes_save+nnodes_new
            itet(itetoff(newtet)+6)=nnodes_save+nnodes_new-3
            itet(itetoff(newtet)+7)=nnodes_save+nnodes_new-12
            itet(itetoff(newtet)+8)=nnodes_save+nnodes_new-2
            itettyp(newtet)=ifelmhex
  100       continue
         enddo
         if(nnodes_new.le.0) then
            call cmo_set_info('nnodes',cmo,nnodes_save,1,1,icscode)
            call cmo_set_info('nelements',cmo,nelements_orig,1,1,
     *          icscode)
            call cmo_newlen(cmo,icscode)
            write(buf,105)
  105       format(' no nodes added by rzamr',
     *           ' - no original nodes in region')
            call writloga('default',0,buf,0,icscode)
            go to 9999
         endif
c
c  call filter to get rid of duplicate nodes
c
         call cmo_set_info('nnodes',cmo,nnodes_save+nnodes_new
     *       ,1,1,icscode)
         call cmo_set_info('nelements',cmo,nelements_save+
     *       nelements_new,1,1,icscode)
         call cmo_newlen(cmo,icscode)
         write(buf,110) nnodes_save+1,nnodes_save+nnodes_new
 110     format('filter/',i10,',',i10,',1;rmpoint/compress;finish')
         call dotask(buf,icscode)
c
c  restore size of mesh
c
         call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,icscode)
         call cmo_get_info('nelements',cmo,nelements,ilen,ityp,icscode)
         call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
         call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
         call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
         call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
         call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
         call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
c
c  find out region membership of new nodes
c
         call mmnewlen('ireg',isubname,ipireg,nnodes,icscode)
         call mmnewlen('isrf',isubname,ipisrf,nnodes,icscode)
         call getregv(xic,yic,
     $                     zic,nnodes,
     &                     epsln,cregion,iregck,
     $                     ireg,isrf,
     *                     icscode)
         nt1=nelements_save+1
         nt2=nelements_save+nelements_new
c
c  update imt,icr,itp for new nodes
         call cmo_get_info('itp',cmo,ipitp1,ilen,ityp,ierr)
         call cmo_get_info('icr',cmo,ipicr1,ilen,ityp,ierr)
         call cmo_get_info('imt',cmo,ipimt1,ilen,ityp,ierr)
         do i=1,nnodes
            if(ireg(i).ne.0) imt1(i)=iregck
            itp1(i)=0
            icr1(i)=0
         enddo
         nnodes_save=nnodes
         nelements_save=nelements
         nnodesmax=nnodes
         nelementsmax=nelements
      enddo
c
c  throw away all new elements - just want nodes
c
      call cmo_set_info('nelements',cmo,nelements_orig,1,1,icscode)
      call cmo_set_info('nnodes',cmo,nnodes_save,1,1,icscode)
      call cmo_newlen(cmo,icscode)
c
c     write(buf,120) nnodes_orig+1,nnodes_save+nnodes_new
 120  format('rzamr added nodes ',i10,' to ',i10)
      call writloga('default',0,buf,0,icscode)
c
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
