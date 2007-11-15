      subroutine addmesh_amr(cmoc,cmob,ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine adds two meshes together to preserve the AMR
C           data structures.
C
C     INPUT ARGUMENTS -
C
C        cmoc    - The course mesh_object (source1).
C        cmob    - The fine mesh_object (source2).
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The resulting mesh_object.
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C    $Log: addmesh_amr.f,v $
C    Revision 2.00  2007/11/05 19:45:45  spchu
C    Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Mar 2002 10:07:38   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.89   Wed Apr 05 13:35:20 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.88   Wed Sep 01 13:36:34 1999   dcg
CPVCS    define asmallnumber
CPVCS
CPVCS       Rev 1.87   Tue Sep 01 08:37:06 1998   dcg
CPVCS    remove unused subroutines
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character*132 logmess
C
C ######################################################################
C
      character*(*) cmoc, cmob
      integer ierror
C
C ######################################################################
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000),
     *        itetkid(1000000),
     *        itetlev(1000000)
C
C ######################################################################
C
      integer npointsa, numteta
      integer npointsb, numtetb
      integer npointsc, numtetc
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(1000000), itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(1000000), yic(1000000), zic(1000000)
C
C ######################################################################
C
      pointer (ipielmb, ielmb)
      integer ielmb(1000000)
      pointer (ipxelmb, xelmb)
      pointer (ipyelmb, yelmb)
      pointer (ipzelmb, zelmb)
      real*8 xelmb(1000000), yelmb(1000000), zelmb(1000000)
C
      real*8 xavg, yavg, zavg
C
C ######################################################################
C
      integer icscode, lenout, length, icmotp, i, i1, it, itb, itc
      integer icharlnf
C
      character*32 isubname, cmoattnam
      character*8192 cbuff
C
C ######################################################################
C
      isubname='addmesh_amr'
C
      call cmo_select(cmoc,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_select')
C
      call cmo_get_info('nnodes',cmoc,npointsa,length,icmotp,icscode)
      call cmo_get_info('nelements',cmoc,numteta,length,icmotp,icscode)
C
      cmoattnam='itetpar'
      call mmfindbk(cmoattnam,cmoc,ipitetpar,lenout,icscode)
      if(icscode.ne.0) then
         cbuff='cmo/addatt/' //
     *         cmoc(1:icharlnf(cmoc)) //
     *         '/' //
     *         cmoattnam(1:icharlnf(cmoattnam)) //
     *         '/VINT' //
     *         '/scalar/nelements/linear/permanent/gxa/0.0' //
     *         ' ; finish '
         call dotaskx3d(cbuff,icscode)
         call mmfindbk(cmoattnam,cmoc,ipitetpar,lenout,icscode)
         do i1=1,numteta
            itetpar(i1)=0
         enddo
      endif
      cmoattnam='itetkid'
      call mmfindbk(cmoattnam,cmoc,ipitetkid,lenout,icscode)
      if(icscode.ne.0) then
         cbuff='cmo/addatt/' //
     *         cmoc(1:icharlnf(cmoc)) //
     *         '/' //
     *         cmoattnam(1:icharlnf(cmoattnam)) //
     *         '/VINT' //
     *         '/scalar/nelements/linear/permanent/gxa/0.0' //
     *         ' ; finish '
         call dotaskx3d(cbuff,icscode)
         call mmfindbk(cmoattnam,cmoc,ipitetkid,lenout,icscode)
         do i1=1,numteta
            itetkid(i1)=0
         enddo
      endif
      cmoattnam='itetlev'
      call mmfindbk(cmoattnam,cmoc,ipitetlev,lenout,icscode)
      if(icscode.ne.0) then
         cbuff='cmo/addatt/' //
     *         cmoc(1:icharlnf(cmoc)) //
     *         '/' //
     *         cmoattnam(1:icharlnf(cmoattnam)) //
     *         '/VINT' //
     *         '/scalar/nelements/linear/permanent/gxa/0.0' //
     *         ' ; finish '
         call dotaskx3d(cbuff,icscode)
         call mmfindbk(cmoattnam,cmoc,ipitetlev,lenout,icscode)
         do i1=1,numteta
            itetlev(i1)=0
         enddo
      endif
C
      call cmo_get_info('nnodes',cmob,npointsb,length,icmotp,icscode)
      call cmo_get_info('nelements',cmob,numtetb,length,icmotp,icscode)
      call cmo_get_info('xic',cmob,ipxic,length,icmotp,icscode)
      call cmo_get_info('yic',cmob,ipyic,length,icmotp,icscode)
      call cmo_get_info('zic',cmob,ipzic,length,icmotp,icscode)
      call cmo_get_info('itettyp',cmob,ipitettyp,length,icmotp,icscode)
      call cmo_get_info('itetoff',cmob,ipitetoff,length,icmotp,icscode)
      call cmo_get_info('jtetoff',cmob,ipjtetoff,length,icmotp,icscode)
      call cmo_get_info('itet',cmob,ipitet,length,icmotp,icscode)
      call cmo_get_info('jtet',cmob,ipjtet,length,icmotp,icscode)
C
      length=numtetb
      call mmgetblk('ielmb',isubname,ipielmb,length,2,icscode)
      call mmgetblk('xelmb',isubname,ipxelmb,length,2,icscode)
      call mmgetblk('yelmb',isubname,ipyelmb,length,2,icscode)
      call mmgetblk('zelmb',isubname,ipzelmb,length,2,icscode)
C
      do it=1,numtetb
         xavg=0.0
         yavg=0.0
         zavg=0.0
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            xavg=xavg+xic(i1)
            yavg=yavg+yic(i1)
            zavg=zavg+zic(i1)
         enddo
         xelmb(it)=xavg/nelmnen(itettyp(it))
         yelmb(it)=yavg/nelmnen(itettyp(it))
         zelmb(it)=zavg/nelmnen(itettyp(it))
      enddo
C
      call table_element(cmoc,
     &                   ipxelmb,ipyelmb,ipzelmb,numtetb,
     &                   ipielmb,
     &                   icscode)
C
      call addmesh_merge(cmoc,cmob,icscode)
C
      call cmo_get_info('nnodes',cmoc,npointsc,length,icmotp,icscode)
      call cmo_get_info('nelements',cmoc,numtetc,length,icmotp,icscode)
C
      call cmo_get_info('itetpar',cmoc,ipitetpar,length,icmotp,icscode)
      call cmo_get_info('itetkid',cmoc,ipitetkid,length,icmotp,icscode)
      call cmo_get_info('itetlev',cmoc,ipitetlev,length,icmotp,icscode)
C
      do it=1,numtetb
         itb=numteta+it
         itc=ielmb(it)
         if(itc.gt.0) then
            itetpar(itb)=itc
            itetkid(itc)=itb
            itetlev(itb)=itetlev(itc)+1
         else
            write(logmess,'(a,i10)') 'Not in any element: ',itb
            call writloga('default',0,logmess,0,icscode)
         endif
      enddo
C
      cbuff='filter/1,0,0 ; geniee ; finish'
      call dotaskx3d(cbuff,icscode)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
