*dk,refine_element_add
      subroutine refine_element_add(cmo_name,
     *                              iprd, nadd,
     *                              ipitadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine calls the correct grid refinement routine
C           depending on the type and dimensionality of the CMO.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        new variable iprd - refine direction for refine_hex_prd()
C                 principal refine direction (prd) is based on topology
C                 iprd = 0 refine xyz with refine_hex_add()
C                 iprd = 4 refine xyz with refine_hex_prd()
C                 iprd = 1 refine along x direction (quad edges 1 and 4)
C                 iprd = 2 refine along y direction (quad edges 2 and 3)
C                 iprd = 3 refine along z direction
C                 iprd = 12 refine along x and y direction
C                 iprd = 13 refine along x and z direction
C                 iprd = 23 refine along y and z direction
C        nadd     - Number of nodes to add.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C
C        $Log: refine_tet_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.35   26 Feb 2004 10:33:18   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.34   24 Dec 2003 10:24:44   tam
CPVCS    add refine_hex_prd() het version of refine_hex_add()
CPVCS    this is Harold's version with few alterations that
CPVCS    restricts refinement direction based on elem topology
CPVCS    also added debug options
CPVCS
CPVCS       Rev 1.33   03 Oct 2002 13:59:18   gable
CPVCS    Added error checking so that in situations where
CPVCS    refine_*_add got called with nadd=0 the code will
CPVCS    return rather than quit with error in memory allocation
CPVCS    of zero lenght array.
CPVCS
CPVCS       Rev 1.32   17 Dec 2001 10:12:28   dcg
CPVCS    make implicit none
CPVCS
CPVCS       Rev 1.31   04 Dec 2001 16:31:22   gable
CPVCS    Incorrect cmo_name argument when getting mbndry information.
CPVCS
CPVCS       Rev 1.30   05 May 2000 15:42:26   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.29   Tue Mar 03 12:38:46 1998   dcg
CPVCS    fix number of arguments in calls to x3d_error
CPVCS
CPVCS       Rev 1.28   Mon Nov 03 14:21:08 1997   dcg
CPVCS    refresh itp1 pointer in subroutine refine_tet_add
CPVCS
CPVCS       Rev 1.27   Wed Oct 08 16:54:56 1997   dcg
CPVCS    fix number of arguments in calls to x3d_error
CPVCS
CPVCS       Rev 1.26   Thu Apr 17 16:04:02 1997   dcg
CPVCS    set point type to zero for points added to
CPVCS    interior of existing elements
CPVCS
CPVCS       Rev 1.0   Thu Apr 17 16:02:46 1997   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.25   Mon Apr 14 16:59:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.24   Sun Feb 23 10:38:52 1997   het
CPVCS    Add the refinement for quads.
CPVCS
CPVCS       Rev 1.23   Fri Jan 24 14:28:56 1997   het
CPVCS    Add new children points if necessary.
CPVCS
CPVCS       Rev 1.22   Mon Dec 09 09:02:40 1996   het
CPVCS    Save the "ipointi" and "ipointj" counters.
CPVCS
CPVCS       Rev 1.21   Mon Dec 02 08:54:22 1996   het
CPVCS    Account for interface boundaries
CPVCS
CPVCS       Rev 1.20   Mon Nov 11 21:01:24 1996   het
CPVCS    Initialize an unitialized variable.
CPVCS
CPVCS       Rev 1.19   Mon Oct 21 12:16:36 1996   het
CPVCS
CPVCS    Correct an error with an unitialized variable
CPVCS
CPVCS       Rev 1.18   Wed Jul 24 17:34:56 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.17   Thu Jun 27 14:56:20 1996   het
CPVCS    For addpts use the names of points without duplicating the points.
CPVCS
CPVCS       Rev 1.16   Tue Apr 02 02:28:06 1996   het
CPVCS    Change this routine to give new nodes names.
CPVCS
CPVCS       Rev 1.15   Thu Mar 14 13:39:28 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.14   Mon Jan 29 22:22:24 1996   het
CPVCS
CPVCS    Fix some errors with the refine/addpts option
CPVCS
CPVCS       Rev 1.13   Wed Jan 24 17:42:26 1996   het
CPVCS    Correct an error.
CPVCS
CPVCS       Rev 1.12   Tue Jan 23 09:17:10 1996   het
CPVCS    Add the refine_hex_add routine for refining hexes.
CPVCS
CPVCS       Rev 1.11   Fri Dec 22 14:14:48 1995   het
CPVCS    Add the triangle element refinement routine.
CPVCS
CPVCS       Rev 1.10   11/07/95 17:24:48   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   11/07/95 11:28:18   het
CPVCS    Modify the 2D triangle refinement algorithms.
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
C
C arguments
      character*32 cmo_name
      integer iprd, nadd
      pointer (ipitadd, itadd)
      pointer (ipiadd, iadd)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      integer itadd(nadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
C ######################################################################

C
      integer nadd3,idifflev,jf,jt,jtoff,iflag,itpar,nadd2,length,
     *  ipointj,it,i,nadd1,i1,lenout,nsd,nef,nen,mbndry,nelements,
     *  icscode,itype,ilen,nnodes,ierrwrt,icharlnf,ierror,ics,
     *  imesh_type, ifdebug
 
C prd variables
      integer nelements_save1, icount, dsmin
      integer iout,lout
      pointer (ipout,out)
      real*8 out(*)

C cmo pointers
      pointer (ipitettyp, itettyp)
      pointer (ipjtetoff, jtetoff)
      pointer (ipjtet, jtet1)
      integer itettyp(1000000), jtetoff(1000000), jtet1(1000000)
 
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet1)
      integer itetoff(1000000), itet1(1000000)

C prd variables
      pointer (ipiptest, iptest)
      pointer (ipittest, ittest)
      integer iptest(10000000), ittest(10000000)
      pointer (ipxradavg, xradavg)
      real*8 xradavg(1000000)
 
c octree
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000),
     *        itetkid(1000000),
     *        itetlev(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitadd2, itadd2)
      pointer (ipiadd2, iadd2)
      pointer (ipxadd2, xadd2)
      pointer (ipyadd2, yadd2)
      pointer (ipzadd2, zadd2)
      integer itflag(1000000), itadd2(1000000), iadd2(1000000)
      real*8 xadd2(1000000), yadd2(1000000), zadd2(1000000)
 
C
      character*32 isubname,cmoattnam,cglobal,cdefault,mesh_type,cout
      character*132 logmess
      character*8192 cbuff
C
 
      integer ileveldiff
      data ileveldiff / 1 /
C
C#######################################################################
C
C
      isubname='refine_element_add'
      cglobal='global'
      cdefault='default'
C
      call cmo_exist(cmo_name,ierror)
      if(ierror.ne.0) then
         write(logmess,9000) cmo_name(1:icharlnf(cmo_name))
 9000    format('CMO does not exist: ',a)
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
C
      call cmo_get_info('nnodes',cmo_name,
     *                  nnodes,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo_name,
     *                  nelements,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo_name,
     *                  mbndry,ilen,itype,icscode)
      call cmo_get_info('nodes_per_element',cmo_name,
     *                  nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo_name,
     *                  nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo_name,
     *                  nsd,ilen,itype,icscode)
      call cmo_get_mesh_type(cmo_name,mesh_type,imesh_type,icscode)
      call cmo_get_info('idebug',cmo_name,ifdebug,ilen,itype,icscode)
      if (ifdebug.gt.0) then
        write(logmess,'(a,i5)') 'REFINE Running in debug mode: ',ifdebug
        call writloga('default',1,logmess,1,ierrwrt)
        if (iprd.gt.0) then
           write(logmess,'(a,i5)') 'Refine PRD: ',iprd
           call writloga('default',0,logmess,0,ierrwrt)
        endif
      endif
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         call refine_tri_add(cmo_name,
     *                       nadd,
     *                       ipitadd,
     *                       iadd,xadd,yadd,zadd)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call refine_tet_add(cmo_name,
     *                       nadd,
     *                       ipitadd,
     *                       iadd,xadd,yadd,zadd)
      elseif(
     *       (nen.eq.nelmnen(ifelmhex).and.nef.eq.nelmnef(ifelmhex).and.
     *        nsd.eq.3) .or.
     *       (nen.eq.nelmnen(ifelmqud).and.nef.eq.nelmnef(ifelmqud).and.
     *        nsd.eq.2)
     *      ) then
 
C        CREATE AMR ATTRIBUTES
         cmoattnam='itetpar'
         call mmfindbk(cmoattnam,cmo_name,ipitetpar,lenout,icscode)
         if(icscode.ne.0) then
            cbuff='cmo/addatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '/VINT' //
     *            '/scalar/nelements/linear/permanent/  /0.0' //
     *            ' ; finish '
            call dotaskx3d(cbuff,ierror)
            call mmfindbk(cmoattnam,cmo_name,
     *                    ipitetpar,lenout,icscode)
            do i1=1,nelements
               itetpar(i1)=0
            enddo
         endif
         cmoattnam='itetkid'
         call mmfindbk(cmoattnam,cmo_name,ipitetkid,lenout,icscode)
         if(icscode.ne.0) then
            cbuff='cmo/addatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '/VINT' //
     *            '/scalar/nelements/linear/permanent/   /0.0' //
     *            ' ; finish '
            call dotaskx3d(cbuff,ierror)
            call mmfindbk(cmoattnam,cmo_name,ipitetkid,lenout,icscode)
            do i1=1,nelements
               itetkid(i1)=0
            enddo
         endif
         cmoattnam='itetlev'
         call mmfindbk(cmoattnam,cmo_name,ipitetlev,lenout,icscode)
         if(icscode.ne.0) then
            cbuff='cmo/addatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '/VINT' //
     *            '/scalar/nelements/linear/permanent/   /0.0' //
     *            ' ; finish '
            call dotaskx3d(cbuff,ierror)
            call mmfindbk(cmoattnam,cmo_name,ipitetlev,lenout,icscode)
            do i1=1,nelements
               itetlev(i1)=0
            enddo
         endif
 
C        Add work attribute for refine_hex_prd()
         if (iprd.gt.0) then
           cmoattnam='xradavg'
           call mmfindbk(cmoattnam,cmo_name,ipxradavg,lenout,icscode)
           if(icscode.ne.0) then
              cbuff='cmo/addatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '/VDOUBLE' //
     *            '/scalar/nnodes/linear/temporary/  /0.0' //
     *            ' ; finish '
              call dotaskx3d(cbuff,ierror)
              call mmfindbk(cmoattnam,cmo_name,ipxradavg,lenout,icscode)
              do i1=1,nnodes
                 xradavg(i1)=0
              enddo
           endif
         endif
C        END adding AMR attributes
 
         nadd1=0
         do i=1,nadd
            it=itadd(i)
            if(itetkid(it).eq.0) then
               nadd1=nadd1+1
               itadd(nadd1)=itadd(i)
               iadd(nadd1)=iadd(i)
               xadd(nadd1)=xadd(i)
               yadd(nadd1)=yadd(i)
               zadd(nadd1)=zadd(i)
            endif
         enddo
         nadd=nadd1
         nelements_save1=nelements
 
c        If prd=0, then refine xyz with refine_hex_add()
c        If prd=4, then refine xyz with refine_hex_prd()
c        If prd>0, then refine direction with refine_hex_prd()
         if (iprd .gt. 0) then
           if (iprd.eq.4) iprd = 0
           call refine_hex_prd(cmo_name,
     *                       iprd,
     *                       nadd,
     *                       ipitadd,
     *                       iadd,xadd,yadd,zadd)
           if (iprd.eq.0) iprd = 4
 
         else
           call refine_hex_add(cmo_name,
     *                       nadd,
     *                       ipitadd,
     *                       iadd,xadd,yadd,zadd)
         endif
 
         call cmo_get_info('nnodes',cmo_name,nnodes,ilen,itype,icscode)
         ipointj=nnodes
         call set_info_i('ipointj',cmo_name,cglobal,cdefault,
     *                   ipointj,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
 
C prd code for amr attributes
C
         call cmo_get_info('faces_per_element',cmo_name,
     *                     nef,ilen,itype,icscode)
         call cmo_get_info('nnodes',cmo_name,
     *                        nnodes,ilen,itype,icscode)
         call cmo_get_info('nelements',cmo_name,
     *                        nelements,ilen,itype,icscode)
         call cmo_get_info('mbndry',cmo_name,
     *                        mbndry,ilen,itype,icscode)

         if(nelements_save1.lt.nelements) then
            call cmo_get_info('itettyp',cmo_name,
     *                        ipitettyp,ilen,itype,icscode)

            call cmo_get_info('itetoff',cmo_name,
     *                        ipitetoff,ilen,itype,icscode)

            call cmo_get_info('jtetoff',cmo_name,
     *                        ipjtetoff,ilen,itype,icscode)

            call cmo_get_info('itet',cmo_name,
     *                        ipitet,ilen,itype,icscode)

            call cmo_get_info('jtet',cmo_name,
     *                        ipjtet,ilen,itype,icscode)

            cmoattnam='itetpar'
            call mmfindbk(cmoattnam,cmo_name,ipitetpar,lenout,icscode)
            cmoattnam='itetkid'
            call mmfindbk(cmoattnam,cmo_name,ipitetkid,lenout,icscode)
            cmoattnam='itetlev'
            call mmfindbk(cmoattnam,cmo_name,ipitetlev,lenout,icscode)
            length=nnodes
            call mmgetblk("ipiptest",isubname,ipiptest,length,1,icscode)

            do i1=1,nnodes
               iptest(i1)=0
            enddo

            length=nelements
            call mmgetblk("ipittest",isubname,ipittest,length,1,icscode)

            do it=1,nelements
               ittest(it)=0
            enddo

            do it=nelements_save1+1,nelements
               ittest(it)=1
               itpar=itetpar(it)
               dowhile(itpar.ne.0)
                  ittest(itpar)=4
                  itpar=itetpar(itpar)
               enddo
               if(itetpar(it).gt.0) then
                  itpar=itetpar(it)
                  ittest(itpar)=2
               endif
            enddo
            do it=1,nelements
               if(ittest(it).eq.2) then
                  do i=1,nelmnef(itettyp(it))
                     jtoff=jtetoff(it)+i
                     if(jtet1(jtoff).lt.mbndry) then
                        jt=1+(jtet1(jtoff)-1)/nef
                        jf=jtet1(jtoff)-nef*(jt-1)
                        if(ittest(jt).eq.0) then
                           ittest(jt)=3
                        endif
                        itpar=itetpar(jt)
                        dowhile(itpar.ne.0)
                           if(ittest(itpar).eq.0) then
                              ittest(itpar)=4
                           endif
                           itpar=itetpar(itpar)
                        enddo
                     elseif(jtet1(jtoff).eq.mbndry) then
                     elseif(jtet1(jtoff).gt.mbndry) then
                        jt=1+(jtet1(jtoff)-mbndry-1)/nef
                        jf=jtet1(jtoff)-mbndry-nef*(jt-1)
                        if(ittest(jt).eq.0) then
                           ittest(jt)=3
                        endif
                        itpar=itetpar(jt)
                        dowhile(itpar.ne.0)
                           if(ittest(itpar).eq.0) then
                              ittest(itpar)=4
                           endif
                           itpar=itetpar(itpar)
                        enddo
                     endif
                  enddo
               endif
            enddo
            do it=1,nelements_save1
               if(itetpar(it).gt.0) then
                  itpar=itetpar(it)
                  if(ittest(itpar).eq.2.or.ittest(itpar).eq.3) then
                     if(ittest(it).eq.0) then
                        ittest(it)=5
                     endif
                  endif
               endif
            enddo
            do it=1,nelements
               if(ittest(it).gt.0) then
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     iptest(i1)=it
                  enddo
               endif
            enddo
            icount=0
            do i1=1,nnodes
               if(iptest(i1).gt.0) then
                  icount=icount+1
                  iptest(icount)=i1
               endif
            enddo
 
C           call get_epsilon('epsilonl', dsmin)
            call cmo_get_attinfo('epsilonl',cmo_name,iout,dsmin,cout,
     *           ipout,lout,itype,icscode)
            if(icscode.ne.0) call x3d_error(isubname,'cmo_get_attinfo')

            if (icount .gt. nnodes) then
               write(logmess,9025) icount,nnodes 
 9025          format('ERROR refine prd: icount gt nnodes: ',i14,i14)
               call writloga('default',1,logmess,1,ics)
               ierror = 1
            endif

            call filter_subset(cmo_name,icount,iptest,dsmin)
            icount=0
            do it=1,nelements
               if(ittest(it).gt.0) then
                  icount=icount+1
                  ittest(icount)=it
               endif
            enddo
            call geniee_cmo_subset(cmo_name,icount,ittest)
            call mmrelblk("ipiptest",isubname,ipiptest,ics)
            call mmrelblk("ipittest",isubname,ipittest,ics)
         endif
 
C end prd code
 
         if(ileveldiff.gt.0) then
            cbuff='filter/1,0,0 ; geniee ; finish'
            call dotaskx3d(cbuff,ierror)
 100        continue
            call cmo_get_info('nelements',cmo_name,
     *                        nelements,ilen,itype,icscode)
            call cmo_get_info('itettyp',cmo_name,
     *                        ipitettyp,ilen,itype,icscode)
            call cmo_get_info('jtetoff',cmo_name,
     *                        ipjtetoff,ilen,itype,icscode)
            call cmo_get_info('jtet',cmo_name,
     *                        ipjtet,ilen,itype,icscode)
            cmoattnam='itetpar'
            call mmfindbk(cmoattnam,cmo_name,ipitetpar,lenout,icscode)
            cmoattnam='itetkid'
            call mmfindbk(cmoattnam,cmo_name,ipitetkid,lenout,icscode)
            cmoattnam='itetlev'
            call mmfindbk(cmoattnam,cmo_name,ipitetlev,lenout,icscode)
            length=nelements
            call mmgetblk('itflag',isubname,ipitflag,length,1,icscode)
            do it=1,nelements
               itflag(it)=0
            enddo
            nadd2=0
            do it=1,nelements
               if(itetkid(it).eq.0) then
                  do i=1,nelmnef(itettyp(it))
                     if(jtet1(jtetoff(it)+i).eq.mbndry) then
                        if(itetpar(it).eq.0) then
                        else
                           itpar=itetpar(it)
                           iflag=0
                           dowhile(iflag.eq.0)
                             if(itetpar(itpar).eq.0) then
                                iflag=1
                             elseif(jtet1(jtetoff(itpar)+i).lt.
     *                              mbndry) then
                                iflag=1
                             elseif(jtet1(jtetoff(itpar)+i).gt.
     *                              mbndry) then
                                iflag=1
                             else
                                itpar=itetpar(itpar)
                             endif
                           enddo
                           jtoff=jtetoff(itpar)+i
                           if(jtet1(jtoff).eq.mbndry) then
                           elseif(jtet1(jtoff).gt.mbndry) then
                              jt=1+(jtet1(jtoff)-mbndry-1)/nef
                              jf=jtet1(jtoff)-mbndry-nef*(jt-1)
                              if(itetkid(jt).eq.0) then
                                 idifflev=itetlev(it)-itetlev(jt)
                                 if(idifflev.gt.ileveldiff) then
                                    nadd2=nadd2+1
                                    itflag(jt)=1
                                 endif
                              endif
                           else
                              jt=1+(jtet1(jtoff)-1)/nef
                              jf=jtet1(jtoff)-nef*(jt-1)
                              if(itetkid(jt).eq.0) then
                                 idifflev=itetlev(it)-itetlev(jt)
                                 if(idifflev.gt.ileveldiff) then
                                    nadd2=nadd2+1
                                    itflag(jt)=1
                                 endif
                              endif
                           endif
                        endif
                     endif
                  enddo
               endif
            enddo
            if(nadd2.gt.0) then
               length=nadd2
               call mmgetblk('itadd2',isubname,
     *                       ipitadd2,length,1,icscode)
               length=nadd2
               call mmgetblk('iadd2',isubname,ipiadd2,length,1,icscode)
               call mmgetblk('xadd2',isubname,ipxadd2,length,2,icscode)
               call mmgetblk('yadd2',isubname,ipyadd2,length,2,icscode)
               call mmgetblk('zadd2',isubname,ipzadd2,length,2,icscode)
               nadd3=0
               do it=1,nelements
                  if(itetkid(it).eq.0.and.itflag(it).gt.0) then
                     nadd3=nadd3+1
                     itadd2(nadd3)=it
                  endif
               enddo
               do i=1,nadd3
                  iadd2(i)=0
                  xadd2(i)=0.0
                  yadd2(i)=0.0
                  zadd2(i)=0.0
               enddo
 
c add prd code
               if (iprd .gt. 0) then
                 if (iprd.eq.4) iprd = 0
                 call refine_hex_prd(cmo_name,
     *                             iprd,
     *                             nadd3,
     *                             ipitadd2,
     *                             iadd2,xadd2,yadd2,zadd2)
                 if (iprd.eq.0) iprd = 4
               else
                 call refine_hex_add(cmo_name,
     *                             nadd3,
     *                             ipitadd2,
     *                             iadd2,xadd2,yadd2,zadd2)
               endif
 
               call cmo_get_info('nnodes',cmo_name,
     *                           nnodes,ilen,itype,icscode)
               ipointj=nnodes
               call set_info_i('ipointj',cmo_name,cglobal,cdefault,
     *                         ipointj,icscode)
               if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
 
C prd code
               call cmo_get_info('faces_per_element',cmo_name,
     *                           nef,ilen,itype,icscode)
               call cmo_get_info('nnodes',cmo_name,
     *                              nnodes,ilen,itype,icscode)
               call cmo_get_info('nelements',cmo_name,
     *                              nelements,ilen,itype,icscode)
               call cmo_get_info('mbndry',cmo_name,
     *                              mbndry,ilen,itype,icscode)
               if(nelements_save1.lt.nelements) then
                  call cmo_get_info('itettyp',cmo_name,
     *                              ipitettyp,ilen,itype,icscode)
                  call cmo_get_info('itetoff',cmo_name,
     *                              ipitetoff,ilen,itype,icscode)
                  call cmo_get_info('jtetoff',cmo_name,
     *                              ipjtetoff,ilen,itype,icscode)
                  call cmo_get_info('itet',cmo_name,
     *                              ipitet,ilen,itype,icscode)
                  call cmo_get_info('jtet',cmo_name,
     *                              ipjtet,ilen,itype,icscode)
                  cmoattnam='itetpar'
                  call mmfindbk(cmoattnam,cmo_name,ipitetpar,lenout,ics)
                  cmoattnam='itetkid'
                  call mmfindbk(cmoattnam,cmo_name,ipitetkid,lenout,ics)
                  cmoattnam='itetlev'
                  call mmfindbk(cmoattnam,cmo_name,ipitetlev,lenout,ics)
                  length=nnodes
                  call mmgetblk("ipiptest",isubname,
     *                          ipiptest,length,1,ics)
                  do i1=1,nnodes
                     iptest(i1)=0
                  enddo
                  length=nelements
                  call mmgetblk("ipittest",isubname,
     *                          ipittest,length,1,ics)
                  do it=1,nelements
                     ittest(it)=0
                  enddo
                  do it=nelements_save1+1,nelements
                     ittest(it)=1
                     itpar=itetpar(it)
                     dowhile(itpar.ne.0)
                        ittest(itpar)=4
                        itpar=itetpar(itpar)
                     enddo
                     if(itetpar(it).gt.0) then
                        itpar=itetpar(it)
                        ittest(itpar)=2
                     endif
                  enddo
                  do it=1,nelements
                     if(ittest(it).eq.2) then
                        do i=1,nelmnef(itettyp(it))
                           jtoff=jtetoff(it)+i
                           if(jtet1(jtoff).lt.mbndry) then
                              jt=1+(jtet1(jtoff)-1)/nef
                              jf=jtet1(jtoff)-nef*(jt-1)
                              if(ittest(jt).eq.0) then
                                 ittest(jt)=3
                              endif
                              itpar=itetpar(jt)
                              dowhile(itpar.ne.0)
                                 if(ittest(itpar).eq.0) then
                                    ittest(itpar)=4
                                 endif
                                 itpar=itetpar(itpar)
                              enddo
                           elseif(jtet1(jtoff).eq.mbndry) then
                           elseif(jtet1(jtoff).gt.mbndry) then
                              jt=1+(jtet1(jtoff)-mbndry-1)/nef
                              jf=jtet1(jtoff)-mbndry-nef*(jt-1)
                              if(ittest(jt).eq.0) then
                                 ittest(jt)=3
                              endif
                              itpar=itetpar(jt)
                              dowhile(itpar.ne.0)
                                 if(ittest(itpar).eq.0) then
                                    ittest(itpar)=4
                                 endif
                                 itpar=itetpar(itpar)
                              enddo
                           endif
                        enddo
                     endif
                  enddo
                  do it=1,nelements_save1
                     if(itetpar(it).gt.0) then
                        itpar=itetpar(it)
                        if(ittest(itpar).eq.2 .or.
     *                     ittest(itpar).eq.3) then
                           if(ittest(it).eq.0) then
                              ittest(it)=5
                           endif
                        endif
                     endif
                  enddo
                  do it=1,nelements
                     if(ittest(it).gt.0) then
                        do i=1,nelmnen(itettyp(it))
                           i1=itet1(itetoff(it)+i)
                           iptest(i1)=it
                        enddo
                     endif
                  enddo
                  icount=0
                  do i1=1,nnodes
                     if(iptest(i1).gt.0) then
                        icount=icount+1
                        iptest(icount)=i1
                     endif
                  enddo
                  call get_epsilon('epsilonl', dsmin)
                  call filter_subset(cmo_name,icount,iptest,dsmin)
                  icount=0
                  do it=1,nelements
                     if(ittest(it).gt.0) then
                        icount=icount+1
                        ittest(icount)=it
                     endif
                  enddo
                  call geniee_cmo_subset(cmo_name,icount,ittest)
                  call mmrelblk("ipiptest",isubname,ipiptest,ics)
                  call mmrelblk("ipittest",isubname,ipittest,ics)
               endif
 
C end prd code
               cbuff='filter/1,0,0 ; geniee ; finish'
               call dotaskx3d(cbuff,ierror)
               call mmrelblk('itflag',isubname,ipitflag,icscode)
               call mmrelblk('itadd2',isubname,ipitadd2,icscode)
               call mmrelblk('iadd2',isubname,ipiadd2,icscode)
               call mmrelblk('xadd2',isubname,ipxadd2,icscode)
               call mmrelblk('yadd2',isubname,ipyadd2,icscode)
               call mmrelblk('zadd2',isubname,ipzadd2,icscode)
               goto 100
            endif
         endif
      else
         write(logmess,9010) cmo_name(1:icharlnf(cmo_name))
 9010    format('Refine on this CMO type is not implemented: ',a)
         call writloga('default',1,logmess,0,ierrwrt)
         write(logmess,9015) mesh_type(1:8)
 9015    format('mesh element type: ',a5)
         call writloga('default',0,logmess,1,ierror)
         goto 9999
      endif
C
      if (ifdebug.gt.0) call mmverify()
      if (ifdebug.ge.3 .and. iprd.gt.0) then
            cmoattnam = 'itetlev'
            cbuff='cmo/printatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '  minmax ; finish '
            call dotaskx3d(cbuff,ierror)
            cmoattnam = 'itetpar'
            cbuff='cmo/printatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '  minmax ; finish '
            call dotaskx3d(cbuff,ierror)
            cmoattnam = 'itetkid'
            cbuff='cmo/printatt/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            '  minmax ; finish '
            call dotaskx3d(cbuff,ierror)
      endif
 
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
 
c     Remove temp work array for refine_hex_prd()
      if (ifdebug.le.5) then
         cmoattnam='xradavg'
         call mmfindbk(cmoattnam,cmo_name,ipxradavg,lenout,icscode)
         if(icscode.eq.0) then
            cbuff='cmo/DELATT/' //
     *            cmo_name(1:icharlnf(cmo_name)) //
     *            '/' //
     *            cmoattnam(1:icharlnf(cmoattnam)) //
     *            ' ; finish '
            call dotaskx3d(cbuff,ierror)
         endif
      endif
C
      return
      end
C     END refine_element_add()
 
 
c
c**************************************************************
      subroutine refine_hex_add(cmo_name,
     *                          nadd,
     *                          ipitadd,
     *                          iadd,xadd,yadd,zadd)
C
C
       implicit none
C
      character*132 logmess
C
      include "local_element.h"
      integer nadd,leni,jt,kf,kt,ie11,ie12,ie10,ie9,ie8,ie7,ie6,
     *  ie5,if6,if5,if4,if3,if2,if1,i8,i7,i6,i5,i4,i3,i2,i1,ie4,
     *  ie3,ie2,ie1,inc1,inc2,idum,ics,nnodesmm,inc,
     *  j,joff,i9,itype,ilen,nelementsmm,npointsinc,it1,ntet1_save,
     *  nptsinc,ierrw,nef1,it,itetiter,ntet1,ntetsnew,npointsnew,
     *  nadd1,irefine,i,icscode,ier,nsdtopo,nen,nef,mbndry,
     *  ntetsinc,ninc,ntets,ierror,icmotype,length,npoints
      real*8 xxsmall
C
      character*(*) cmo_name
C
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipiaddorder, iaddorder)
      integer iaddorder(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer itp1(10000000), isn1(10000000)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(8*1000000), jtet1(6*1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000),
     *        itetkid(1000000),
     *        itetlev(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn1)
      pointer (ipitetnn1, jtetnn1)
      pointer (ipitetnn2, jtetnn2)
      integer itflag(1000000),
     *        itetnn1(8*1000000),
     *        jtetnn1(6*1000000), jtetnn2(6*1000000)
C
      pointer (iplist_sink1, list_sink1)
      pointer (iplist_source1, list_source1)
      pointer (ipxweight_source1, xweight_source1)
      integer list_sink1(nadd), list_source1(1000000)
      real*8 xweight_source1(1000000)
      pointer (iplist_sink2, list_sink2)
      pointer (iplist_source2, list_source2)
      pointer (ipxweight_source2, xweight_source2)
      integer list_sink2(nadd), list_source2(1000000)
      real*8 xweight_source2(1000000)
      pointer (iplist_sink3, list_sink3)
      pointer (iplist_source3, list_source3)
      pointer (ipxweight_source3, xweight_source3)
      integer list_sink3(nadd), list_source3(1000000)
      real*8 xweight_source3(1000000)
C
      integer ifacept(10), iedgept(20)
C
      character*32 cmo
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
C
C     ###################################################################
C
      isubname='refine_hex_add'
C
      xxsmall=1.0d-30
C
      cmo=cmo_name
C
C     This was getting called when nadd = 0 which results
C     in a zero length memory request and crash.
C     Could prevent it from being called in the first place
C     but this works.
C
      if(nadd .eq. 0)then
         logmess = "WARNING: refine_hex_add: zero elements to refine "
         call writloga('default',0,logmess,0,ierrw)
         logmess = "WARNING: refine_hex_add: No action, RETURN "
         call writloga('default',0,logmess,0,ierrw)
         return
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
      call cmo_get_info('itetpar',cmo,ipitetpar,leni,icmotype,ier)
      call cmo_get_info('itetkid',cmo,ipitetkid,leni,icmotype,ier)
      call cmo_get_info('itetlev',cmo,ipitetlev,leni,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
C
      length=nadd
      call mmgetblk('iaddorder',isubname,ipiaddorder,length,1,icscode)
      do i=1,nadd
         iaddorder(i)=0
      enddo
C
      length=nadd
      call mmgetblk('list_sink1',isubname,iplist_sink1,length,1,icscode)
      length=nen*nadd
      call mmgetblk('list_source1',isubname,
     *              iplist_source1,length,1,icscode)
      call mmgetblk('xweight_source1',isubname,
     *              ipxweight_source1,length,2,icscode)
C
      if(nen.eq.4.and.nef.eq.4.and.nsdtopo.eq.2) then
         length=nelmnee(ifelmqud)*nadd
         call mmgetblk('list_sink3',isubname,
     *                 iplist_sink3,length,1,icscode)
         length=2*nelmnee(ifelmqud)*nadd
         call mmgetblk('list_source3',isubname,
     *                 iplist_source3,length,1,icscode)
         call mmgetblk('xweight_source3',isubname,
     *                 ipxweight_source3,length,2,icscode)
      elseif(nen.eq.8.and.nef.eq.6.and.nsdtopo.eq.3) then
         length=nelmnef(ifelmhex)*nadd
         call mmgetblk('list_sink2',isubname,
     *                 iplist_sink2,length,1,icscode)
         length=4*nelmnef(ifelmhex)*nadd
         call mmgetblk('list_source2',isubname,
     *                 iplist_source2,length,1,icscode)
         call mmgetblk('xweight_source2',isubname,
     *                 ipxweight_source2,length,2,icscode)
C
         length=nelmnee(ifelmhex)*nadd
         call mmgetblk('list_sink3',isubname,
     *                 iplist_sink3,length,1,icscode)
         length=2*nelmnee(ifelmhex)*nadd
         call mmgetblk('list_source3',isubname,
     *                 iplist_source3,length,1,icscode)
         call mmgetblk('xweight_source3',isubname,
     *                 ipxweight_source3,length,2,icscode)
      endif
 
C    Get child-parent chains
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,1,icscode)
      length=nen*ntets
      call mmgetblk("itetnn" ,isubname,ipitetnn ,length,1,icscode)
      length=nef*ntets
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,1,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,1,icscode)
C
      irefine=0
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      ntet1=nadd
      itetiter=0
  11  continue
      do it=1,ntets
         itflag(it)=0
      enddo
 
C     LOOP all elements and fill connectivity arrays
      do it=1,ntets
         do i=1,nelmnen(itettyp(it))
            itetnn1(itetoff(it)+i)=itet1(itetoff(it)+i)
         enddo
         nef1=nelmnef(itettyp(it))
         do i=1,nef1
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               jtetnn1(jtetoff(it)+i)=0
               jtetnn2(jtetoff(it)+i)=0
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               jtetnn1(jtetoff(it)+i)=
     *                             1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                mbndry -
     *                                nef*(jtetnn1(jtetoff(it)+i)-1)
            else
               jtetnn1(jtetoff(it)+i)=1+(jtet1(jtetoff(it)+i)-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                nef*(itetnn1(jtetoff(it)+i)-1)
            endif
         enddo
      enddo
      npointsnew=npoints
      ntetsnew=ntets
      itetiter=itetiter+1
      write(logmess,'(a,2i10)') "Element iteration: ",itetiter,ntet1
      call writloga('default',0,logmess,0,ierrw)
 
C     UPDATE for new elements and points
      if(ntet1.ne.0) then
         if(nen.eq.4.and.nef.eq.4.and.nsdtopo.eq.2) then
            nptsinc=5
         elseif(nen.eq.8.and.nef.eq.6.and.nsdtopo.eq.3) then
            nptsinc=19
         endif
         ntet1_save=0
         do it1=1,ntet1
            it=itadd(it1)
            irefine=irefine+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((npointsnew+nptsinc).gt.length) then
               npointsinc=npointsnew+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,nelementsmm,1,1,ier)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
               call mmnewlen('iparent',isubname,ipiparent,npointsinc,
     *                       icscode)
            endif
C
            npointsnew=npointsnew+1
            iparent(npointsnew)=npointsnew
            i9=npointsnew
C
            nadd1=nadd1+1
            iaddorder(nadd1)=nadd1
            list_sink1(nadd1)=npointsnew
            do i=1,nelmnen(itettyp(it))
               list_source1(i+nen*(nadd1-1))=itet1(itetoff(it)+i)
               xweight_source1(i+nen*(nadd1-1))=1.0
            enddo
C
C          Fill lists for interpolation
            if(itettyp(it).eq.ifelmqud) then
               do i=1,nelmnee(itettyp(it))
                  npointsnew=npointsnew+1
                  iparent(npointsnew)=npointsnew
                  iedgept(i)=npointsnew
                 list_sink3(i+nelmnee(itettyp(it))*(nadd1-1))=npointsnew
                  joff=2*(i-1)+2*nelmnee(itettyp(it))*(nadd1-1)
                  do j=1,2
                     list_source3(joff+j)=
     *                  itet1(itetoff(it)+ielmedge1(j,i,itettyp(it)))
                     xweight_source3(joff+j)=1.0
                  enddo
               enddo
            elseif(itettyp(it).eq.ifelmhex) then
               do i=1,nelmnef(itettyp(it))
                  npointsnew=npointsnew+1
                  iparent(npointsnew)=npointsnew
                  ifacept(i)=npointsnew
                 list_sink2(i+nelmnef(itettyp(it))*(nadd1-1))=npointsnew
                  joff=ielmface0(i,itettyp(it))*(i-1)+
     *           ielmface0(i,itettyp(it))*nelmnef(itettyp(it))*(nadd1-1)
                  do j=1,ielmface0(i,itettyp(it))
                     list_source2(joff+j)=
     *                  itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     xweight_source2(joff+j)=1.0
                  enddo
               enddo
C
               do i=1,nelmnee(itettyp(it))
                  npointsnew=npointsnew+1
                  iparent(npointsnew)=npointsnew
                  iedgept(i)=npointsnew
                 list_sink3(i+nelmnee(itettyp(it))*(nadd1-1))=npointsnew
                  joff=2*(i-1)+2*nelmnee(itettyp(it))*(nadd1-1)
                  do j=1,2
                     list_source3(joff+j)=
     *                  itet1(itetoff(it)+ielmedge1(j,i,itettyp(it)))
                     xweight_source3(joff+j)=1.0
                  enddo
               enddo
            endif
C
            if(itettyp(it).eq.ifelmtri) then
               ninc=2
            elseif(itettyp(it).eq.ifelmqud) then
               ninc=4
            elseif(itettyp(it).eq.ifelmtet) then
               ninc=3
            elseif(itettyp(it).eq.ifelmhex) then
               ninc=8
            endif
 
C           UPDATE cmo sizes and increment arrays
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
               call cmo_get_info('itetpar',cmo,
     *                           ipitetpar,leni,icmotype,ier)
               call cmo_get_info('itetkid',cmo,
     *                           ipitetkid,leni,icmotype,ier)
               call cmo_get_info('itetlev',cmo,
     *                           ipitetlev,leni,icmotype,ier)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,leni,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,leni,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,leni,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,leni,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,leni,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,leni,icmotype,ierror)
            endif
 
            call mmgetlen(ipitflag,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               call mmgetnam(ipitflag,iblknam,iprtnam,icscode)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       icscode)
               do idum=ntetsnew+1,ntetsnew+inc
                  itflag(idum)=0
               enddo
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,icscode)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       icscode)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,icscode)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       icscode)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,icscode)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       icscode)
            endif
 
C           REFINE ELEMENTS
            if(itettyp(it).eq.ifelmtri) then
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               itflag(it)=1
               itetnn1(itetoff(it)+1)=i2
               itetnn1(itetoff(it)+2)=i3
               itetnn1(itetoff(it)+3)=npointsnew
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i3
                  itetnn1(itetoff(ntetsnew)+2)=i1
                  itetnn1(itetoff(ntetsnew)+3)=npointsnew
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=npointsnew
            elseif(itettyp(it).eq.ifelmtet) then
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               itflag(it)=1
               itetnn1(itetoff(it)+1)=i2
               itetnn1(itetoff(it)+2)=i4
               itetnn1(itetoff(it)+3)=i3
               itetnn1(itetoff(it)+4)=npointsnew
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=i3
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i4
                  itetnn1(itetoff(ntetsnew)+3)=i2
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i3
                  itetnn1(itetoff(ntetsnew)+3)=i4
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew
            elseif(itettyp(it).eq.ifelmqud) then
C
C              1x4 quad refinement order:
C                  1 - lower left
C                  2 - lower right
C                  3 - upper left
C                  4 - upper right
C
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               ie1=iedgept(1)
               ie2=iedgept(2)
               ie3=iedgept(3)
               ie4=iedgept(4)
C
               itetkid(it)=ntetsnew+1
C
C*****         ntetsnew=ntetsnew+1
C*****                  itflag(ntetsnew)=1
C*****                  itetclr(ntetsnew)=itetclr(it)
C*****                  itettyp(ntetsnew)=itettyp(it)
C*****                  itetoff(ntetsnew)=nen*(ntetsnew-1)
C*****                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
C*****                  itetnn1(itetoff(ntetsnew)+1)=ie2
C*****                  itetnn1(itetoff(ntetsnew)+2)=i1
C*****                  itetnn1(itetoff(ntetsnew)+3)=ie1
C*****                  itetnn1(itetoff(ntetsnew)+4)=i9
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=ie1
                  itetnn1(itetoff(ntetsnew)+3)=i9
                  itetnn1(itetoff(ntetsnew)+4)=ie2
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=ie1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=ie3
                  itetnn1(itetoff(ntetsnew)+4)=i9
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=ie2
                  itetnn1(itetoff(ntetsnew)+2)=i9
                  itetnn1(itetoff(ntetsnew)+3)=ie4
                  itetnn1(itetoff(ntetsnew)+4)=i4
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i9
                  itetnn1(itetoff(ntetsnew)+2)=ie3
                  itetnn1(itetoff(ntetsnew)+3)=i3
                  itetnn1(itetoff(ntetsnew)+4)=ie4
            elseif(itettyp(it).eq.ifelmhex) then
C
C              1x8 hex refinement order:
C                  1 - bottom lower left
C                  2 - bottom lower right
C                  3 - bottom upper left
C                  4 - bottom upper right
C                  5 - top lower left
C                  6 - top lower right
C                  7 - top upper left
C                  8 - top upper right
C
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               i5=itet1(itetoff(it)+5)
               i6=itet1(itetoff(it)+6)
               i7=itet1(itetoff(it)+7)
               i8=itet1(itetoff(it)+8)
               if1=ifacept(1)
               if2=ifacept(2)
               if3=ifacept(3)
               if4=ifacept(4)
               if5=ifacept(5)
               if6=ifacept(6)
               ie1=iedgept(1)
               ie2=iedgept(2)
               ie3=iedgept(3)
               ie4=iedgept(4)
               ie5=iedgept(5)
               ie6=iedgept(6)
               ie7=iedgept(7)
               ie8=iedgept(8)
               ie9=iedgept(9)
               ie10=iedgept(10)
               ie11=iedgept(11)
               ie12=iedgept(12)
C
               itetkid(it)=ntetsnew+1
C
C*****         ntetsnew=ntetsnew+1
C*****                  itflag(ntetsnew)=1
C*****                  itetclr(ntetsnew)=itetclr(it)
C*****                  itettyp(ntetsnew)=itettyp(it)
C*****                  itetoff(ntetsnew)=nen*(ntetsnew-1)
C*****                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
C*****                  itetnn1(itetoff(ntetsnew)+1)=i1
C*****                  itetnn1(itetoff(ntetsnew)+2)=ie1
C*****                  itetnn1(itetoff(ntetsnew)+3)=if1
C*****                  itetnn1(itetoff(ntetsnew)+4)=ie2
C*****                  itetnn1(itetoff(ntetsnew)+5)=ie3
C*****                  itetnn1(itetoff(ntetsnew)+6)=if3
C*****                  itetnn1(itetoff(ntetsnew)+7)=i9
C*****                  itetnn1(itetoff(ntetsnew)+8)=if6
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=ie1
                  itetnn1(itetoff(ntetsnew)+3)=if1
                  itetnn1(itetoff(ntetsnew)+4)=ie2
                  itetnn1(itetoff(ntetsnew)+5)=ie3
                  itetnn1(itetoff(ntetsnew)+6)=if3
                  itetnn1(itetoff(ntetsnew)+7)=i9
                  itetnn1(itetoff(ntetsnew)+8)=if6
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=ie1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=ie4
                  itetnn1(itetoff(ntetsnew)+4)=if1
                  itetnn1(itetoff(ntetsnew)+5)=if3
                  itetnn1(itetoff(ntetsnew)+6)=ie5
                  itetnn1(itetoff(ntetsnew)+7)=if4
                  itetnn1(itetoff(ntetsnew)+8)=i9
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=ie2
                  itetnn1(itetoff(ntetsnew)+2)=if1
                  itetnn1(itetoff(ntetsnew)+3)=ie6
                  itetnn1(itetoff(ntetsnew)+4)=i4
                  itetnn1(itetoff(ntetsnew)+5)=if6
                  itetnn1(itetoff(ntetsnew)+6)=i9
                  itetnn1(itetoff(ntetsnew)+7)=if5
                  itetnn1(itetoff(ntetsnew)+8)=ie8
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=if1
                  itetnn1(itetoff(ntetsnew)+2)=ie4
                  itetnn1(itetoff(ntetsnew)+3)=i3
                  itetnn1(itetoff(ntetsnew)+4)=ie6
                  itetnn1(itetoff(ntetsnew)+5)=i9
                  itetnn1(itetoff(ntetsnew)+6)=if4
                  itetnn1(itetoff(ntetsnew)+7)=ie7
                  itetnn1(itetoff(ntetsnew)+8)=if5
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=ie3
                  itetnn1(itetoff(ntetsnew)+2)=if3
                  itetnn1(itetoff(ntetsnew)+3)=i9
                  itetnn1(itetoff(ntetsnew)+4)=if6
                  itetnn1(itetoff(ntetsnew)+5)=i5
                  itetnn1(itetoff(ntetsnew)+6)=ie9
                  itetnn1(itetoff(ntetsnew)+7)=if2
                  itetnn1(itetoff(ntetsnew)+8)=ie10
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=if3
                  itetnn1(itetoff(ntetsnew)+2)=ie5
                  itetnn1(itetoff(ntetsnew)+3)=if4
                  itetnn1(itetoff(ntetsnew)+4)=i9
                  itetnn1(itetoff(ntetsnew)+5)=ie9
                  itetnn1(itetoff(ntetsnew)+6)=i6
                  itetnn1(itetoff(ntetsnew)+7)=ie11
                  itetnn1(itetoff(ntetsnew)+8)=if2
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=if6
                  itetnn1(itetoff(ntetsnew)+2)=i9
                  itetnn1(itetoff(ntetsnew)+3)=if5
                  itetnn1(itetoff(ntetsnew)+4)=ie8
                  itetnn1(itetoff(ntetsnew)+5)=ie10
                  itetnn1(itetoff(ntetsnew)+6)=if2
                  itetnn1(itetoff(ntetsnew)+7)=ie12
                  itetnn1(itetoff(ntetsnew)+8)=i8
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetpar(ntetsnew)=it
                  itetkid(ntetsnew)=0
                  itetlev(ntetsnew)=itetlev(it)+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i9
                  itetnn1(itetoff(ntetsnew)+2)=if4
                  itetnn1(itetoff(ntetsnew)+3)=ie7
                  itetnn1(itetoff(ntetsnew)+4)=if5
                  itetnn1(itetoff(ntetsnew)+5)=if2
                  itetnn1(itetoff(ntetsnew)+6)=ie11
                  itetnn1(itetoff(ntetsnew)+7)=i7
                  itetnn1(itetoff(ntetsnew)+8)=ie12
            endif
         enddo
      endif
C     DONE with ELEMENT REFINEMENT
 
      write(logmess,'(a,i10,a,i10)')
     *   "Octree refined elems: old=",ntets," new=",ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntetsnew
            do i=1,nelmnen(itettyp(it))
               itet1(itetoff(it)+i)=itetnn1(itetoff(it)+i)
            enddo
            do i=1,nelmnen(itettyp(it))
               itetnn1(itetoff(it)+i)=iparent(itetnn1(itetoff(it)+i))
            enddo
            do i=1,nelmnef(itettyp(it))
               jtetnn1(jtetoff(it)+i)=-1
               jtetnn2(jtetoff(it)+i)=-1
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,nelmnef(itettyp(it))
                  kt=1+(jtet1(jtetoff(it)+i)-1)/nelmnef(itettyp(it))
                  kf=jtet1(jtetoff(it)+i)-nelmnef(itettyp(it))*(kt-1)
                  if(kt.le.ntets) then
                     jtetnn1(jtetoff(kt)+kf)=-1
                     jtetnn2(jtetoff(kt)+kf)=-1
                  endif
                  jtetnn1(jtetoff(it)+i)=-1
                  jtetnn2(jtetoff(it)+i)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn1,jtetnn1,jtetnn2,
     *               nen,nef,
     *               ntets,npoints,
     *               nsdtopo,
     *               npoints,ntets)
         call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
         do it=1,ntets
            do i=1,nelmnef(itettyp(it))
               if(jtetnn1(jtetoff(it)+i).gt.0 .and.
     *            jtetnn1(jtetoff(it)+i).le.ntets) then
                  jt=jtetnn1(jtetoff(it)+i)
                  if(itetclr(it).eq.itetclr(jt)) then
                     jtet1(jtetoff(it)+i)=
     *                  nelmnef(itettyp(it))*(jtetnn1(jtetoff(it)+i)-1)+
     *                     jtetnn2(jtetoff(it)+i)
                  else
                     jtet1(jtetoff(it)+i)=mbndry+
     *                  nelmnef(itettyp(it))*(jtetnn1(jtetoff(it)+i)-1)+
     *                     jtetnn2(jtetoff(it)+i)
                  endif
               else
                  jtet1(jtetoff(it)+i)=mbndry
               endif
            enddo
         enddo
         if(ntet1_save.gt.0) then
            ntet1=ntet1_save
            goto 11
         endif
      endif
      goto 9999
 9999 continue
C
      cmolength='nnodes'
      call cmo_interpolate(cmo_name,cmo_name,
     *                     cmolength,
     *                     nadd1,nen,
     *                     list_sink1,list_source1,xweight_source1,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
      if(nen.eq.4.and.nef.eq.4.and.nsdtopo.eq.2) then
         cmolength='nnodes'
         call cmo_interpolate(cmo_name,cmo_name,
     *                        cmolength,
     *                        4*nadd1,2,
     *                        list_sink3,list_source3,xweight_source3,
     *                        ierror)
         if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
      elseif(nen.eq.8.and.nef.eq.6.and.nsdtopo.eq.3) then
         cmolength='nnodes'
         call cmo_interpolate(cmo_name,cmo_name,
     *                        cmolength,
     *                        6*nadd1,4,
     *                        list_sink2,list_source2,xweight_source2,
     *                        ierror)
         if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
         cmolength='nnodes'
         call cmo_interpolate(cmo_name,cmo_name,
     *                        cmolength,
     *                        12*nadd1,2,
     *                        list_sink3,list_source3,xweight_source3,
     *                        ierror)
         if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
      endif
C
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      call mmrelprt(isubname,icscode)
      return
      end
c
c**********************************************************************
      subroutine refine_tet_add(cmo_name,
     *                          nadd,
     *                          ipitadd,
     *                          iadd,xadd,yadd,zadd)
c
c  Input
c    cmo_name     name of mesh object to be refined
c    nadd         number of nodes to add
c    ipitadd      pointer to list of tetrahedra to be refined
c    iadd         if iadd(i).eq.0 new nodes are created and added
c                 if iadd(i).gt.0 iadd(i) is the node to be added
c    xadd, yadd, zadd  coordinates of node to be added to the
c                      mesh
C
C     CHANGE HISTORY -
C
C        $Log: refine_tet_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.8   10/20/95 10:47:40   het
CPVCS    Fix iparent memory management error and add new refine options.
CPVCS
CPVCS       Rev 1.7   10/05/95 15:46:52   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.6   09/29/95 09:14:04   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.5   06/05/95 10:36:40   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.4   05/26/95 13:18:16   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.3   03/28/95 14:16:24   het
CPVCS    Change the Face-refine message to Tet-refine
CPVCS
CPVCS       Rev 1.2   03/28/95 12:34:34   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.1   03/13/95 16:15:08   het
CPVCS    Get mbndry from cmo and fixed 1st edge selection error
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:08   pvcs
CPVCS    Original version.
C
      implicit none
      integer nadd,npoints,length,icmotype,ntets,ierror,mbndry,nen,
     *  nef,nsdtopo,leni,ier,icscode,i,irefine,nadd1,npointsnew,
     *  ntetsnew,ntet1,itetiter,it,nef1,ierrw,ntet1_save,it1,
     *  npointsnew1,npointsinc,nelementsmm,ilen,itype,ninc,inc,
     *  ntetsinc,nnodesmm,ics,idum,inc1,inc2,i1,i2,i3,i4,kt,kf,jt
      real*8 xxsmall
 
C
      character*132 logmess
C
      include "local_element.h"
      include 'chydro.h'
C
      character*(*) cmo_name
C
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipiaddorder, iaddorder)
      integer iaddorder(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer itp1(10000000), isn1(10000000)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(4*1000000), jtet1(4*1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn1)
      pointer (ipitetnn1, jtetnn1)
      pointer (ipitetnn2, jtetnn2)
      integer itflag(1000000),
     *        itetnn1(4*1000000),
     *        jtetnn1(4*1000000), jtetnn2(4*1000000)
C
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(nadd), list_source(1000000)
      real*8 xweight_source(1000000)
C
      character*32 cmo
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
C
C     ###################################################################
C
      isubname='refine_tet_add'
C
      xxsmall=1.0d-30
C
      cmo=cmo_name
C
C     This was getting called when nadd = 0 which results
C     in a zero length memory request and crash.
C     Could prevent it from being called in the first place
C     but this works.
C
      if(nadd .eq. 0)then
         logmess = "WARNING: refine_tet_add: zero elements to refine "
         call writloga('default',0,logmess,0,ierrw)
         logmess = "WARNING: refine_tet_add: No action, RETURN "
         call writloga('default',0,logmess,0,ierrw)
         return
      endif
C
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
C
      length=nadd
      call mmgetblk('iaddorder',isubname,ipiaddorder,length,1,icscode)
      do i=1,nadd
         iaddorder(i)=0
      enddo
C
      length=nadd
      call mmgetblk('list_sink',isubname,iplist_sink,length,1,icscode)
      length=nen*nadd
      call mmgetblk('list_source',isubname,iplist_source,length,1,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,1,icscode)
      length=nen*ntets
      call mmgetblk("itetnn" ,isubname,ipitetnn ,length,1,icscode)
      length=nef*ntets
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,1,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,1,icscode)
C
      irefine=0
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      ntet1=nadd
      itetiter=0
  11  continue
      do it=1,ntets
         itflag(it)=0
      enddo
      do it=1,ntets
         do i=1,nelmnen(itettyp(it))
            itetnn1(itetoff(it)+i)=itet1(itetoff(it)+i)
         enddo
         nef1=nelmnef(itettyp(it))
         do i=1,nef1
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               jtetnn1(jtetoff(it)+i)=0
               jtetnn2(jtetoff(it)+i)=0
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               jtetnn1(jtetoff(it)+i)=
     *                             1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                mbndry -
     *                                nef*(jtetnn1(jtetoff(it)+i)-1)
            else
               jtetnn1(jtetoff(it)+i)=1+(jtet1(jtetoff(it)+i)-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                nef*(itetnn1(jtetoff(it)+i)-1)
            endif
         enddo
      enddo
      npointsnew=npoints
      ntetsnew=ntets
      itetiter=itetiter+1
      write(logmess,'(a,2i10)') "Element iteration: ",itetiter,ntet1
      call writloga('default',0,logmess,0,ierrw)
      if(ntet1.ne.0) then
         ntet1_save=0
         do it1=1,ntet1
            it=itadd(it1)
            irefine=irefine+1
            nadd1=nadd1+1
            iaddorder(nadd1)=nadd1
            if(iadd(iaddorder(nadd1)).gt.0) then
               npointsnew1=iadd(iaddorder(nadd1))
               list_sink(nadd1)=npointsnew1
               do i=1,nen
                  list_source(nen*(nadd1-1)+i)=npointsnew1
                  xweight_source(nen*(nadd1-1)+i)=1.0
               enddo
            elseif(iadd(iaddorder(nadd1)).lt.0) then
               npointsnew1=iabs(iadd(iaddorder(nadd1)))
               list_sink(nadd1)=npointsnew1
               do i=1,nen
                  list_source(nen*(nadd1-1)+i)=itet1(itetoff(it)+i)
                  xweight_source(nen*(nadd1-1)+i)=1.0
               enddo
            else
               npointsnew=npointsnew+1
               npointsnew1=npointsnew
               iadd(iaddorder(nadd1))=npointsnew1
               call mmfindbk('xic',cmo,ipxic,length,icscode)
               if((npointsnew+1).gt.length) then
                  npointsinc=npointsnew+1000
                  call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
                  call mmgetlen(ipitetclr,nelementsmm,icscode)
                  call cmo_set_info('nelements',cmo,nelementsmm,1,1,ier)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
                  call mmnewlen('iparent',isubname,
     *                          ipiparent,npointsinc,icscode)
               endif
               list_sink(nadd1)=npointsnew1
               do i=1,nen
                  list_source(nen*(nadd1-1)+i)=itet1(itetoff(it)+i)
                  xweight_source(nen*(nadd1-1)+i)=1.0
               enddo
            endif
C
            iparent(npointsnew1)=npointsnew1
C
            if(itettyp(it).eq.ifelmtri) then
               ninc=2
            elseif(itettyp(it).eq.ifelmtet) then
               ninc=3
            endif
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,leni,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,leni,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,leni,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,leni,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,leni,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,leni,icmotype,ierror)
            endif
            call mmgetlen(ipitflag,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               call mmgetnam(ipitflag,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       ics)
               do idum=ntetsnew+1,ntetsnew+inc
                  itflag(idum)=0
               enddo
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       ics)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       ics)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       ics)
            endif
            if(itettyp(it).eq.ifelmtri) then
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               itflag(it)=1
               itetnn1(itetoff(it)+1)=i2
               itetnn1(itetoff(it)+2)=i3
               itetnn1(itetoff(it)+3)=npointsnew1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i3
                  itetnn1(itetoff(ntetsnew)+2)=i1
                  itetnn1(itetoff(ntetsnew)+3)=npointsnew1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=npointsnew1
            elseif(itettyp(it).eq.ifelmtet) then
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               itflag(it)=1
               itetnn1(itetoff(it)+1)=i2
               itetnn1(itetoff(it)+2)=i4
               itetnn1(itetoff(it)+3)=i3
               itetnn1(itetoff(it)+4)=npointsnew1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i2
                  itetnn1(itetoff(ntetsnew)+3)=i3
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i4
                  itetnn1(itetoff(ntetsnew)+3)=i2
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn1(itetoff(ntetsnew)+1)=i1
                  itetnn1(itetoff(ntetsnew)+2)=i3
                  itetnn1(itetoff(ntetsnew)+3)=i4
                  itetnn1(itetoff(ntetsnew)+4)=npointsnew1
            endif
         enddo
      endif
      write(logmess,'(a,i10,a,i10)')
     *   "refined elems: old=",ntets," new=",ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntetsnew
            do i=1,nelmnen(itettyp(it))
               itet1(itetoff(it)+i)=itetnn1(itetoff(it)+i)
            enddo
            do i=1,nelmnen(itettyp(it))
               itetnn1(itetoff(it)+i)=iparent(itetnn1(itetoff(it)+i))
            enddo
            do i=1,nelmnef(itettyp(it))
               jtetnn1(jtetoff(it)+i)=-1
               jtetnn2(jtetoff(it)+i)=-1
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,nelmnef(itettyp(it))
                  kt=1+(jtet1(jtetoff(it)+i)-1)/nelmnef(itettyp(it))
                  kf=jtet1(jtetoff(it)+i)-nelmnef(itettyp(it))*(kt-1)
                  if(kt.le.ntets) then
                     jtetnn1(jtetoff(kt)+kf)=-1
                     jtetnn2(jtetoff(kt)+kf)=-1
                  endif
                  jtetnn1(jtetoff(it)+i)=-1
                  jtetnn2(jtetoff(it)+i)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn1,jtetnn1,jtetnn2,
     *               nen,nef,
     *               ntets,npoints,
     *               nsdtopo,
     *               npoints,ntets)
         call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
         do it=1,ntets
            do i=1,nelmnef(itettyp(it))
               if(jtetnn1(jtetoff(it)+i).gt.0 .and.
     *            jtetnn1(jtetoff(it)+i).le.ntets) then
                  jt=jtetnn1(jtetoff(it)+i)
                  if(itetclr(it).eq.itetclr(jt)) then
                     jtet1(jtetoff(it)+i)=
     *                  nelmnef(itettyp(it))*(jtetnn1(jtetoff(it)+i)-1)+
     *                     jtetnn2(jtetoff(it)+i)
                  else
                     jtet1(jtetoff(it)+i)=mbndry+
     *                  nelmnef(itettyp(it))*(jtetnn1(jtetoff(it)+i)-1)+
     *                     jtetnn2(jtetoff(it)+i)
                  endif
               else
                  jtet1(jtetoff(it)+i)=mbndry
               endif
            enddo
         enddo
         if(ntet1_save.gt.0) then
            ntet1=ntet1_save
            goto 11
         endif
      endif
      goto 9999
 9999 continue
C
      cmolength='nnodes'
      call cmo_interpolate(cmo_name,cmo_name,
     *                     cmolength,
     *                     nadd1,nen,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
C
      do i=1,nadd1
         i1=list_sink(i)
         i2=iaddorder(i)
         xic(i1)=xadd(i2)
         yic(i1)=yadd(i2)
         zic(i1)=zadd(i2)
         itp1(i1)=ifitpint
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      call mmrelprt(isubname,icscode)
      return
      end
c
c*****************************************************************
      subroutine refine_tri_add(cmo,
     *                          nadd,
     *                          ipitadd,
     *                          iadd,xadd,yadd,zadd)
C
c
c  Input
c    cmo_name     name of mesh object to be refined
c    nadd         number of nodes to add
c    ipitadd      pointer to list of tetrahedra to be refined
c    iadd         if iadd(i).eq.0 new nodes are created and added
c                 if iadd(i).gt.0 iadd(i) is the node to be added
c    xadd, yadd, zadd  coordinates of node to be added to the
c                      mesh
c
C
      implicit none
C
      include "local_element.h"
      integer nvalues,i,j,k,jcount,ierrdum,leni,kt,kf,isum,ie1,
     *  ie2,iedge,iedge1,i12,i31,i23,inc2,inc1,len,nnodesmm,
     *  ntetsinc,inc,icount,jf,jt,itype,nelementsmm,npointsinc,
     *  nef1,ntetsnew,npointsnew,nadd1,it,ierrw,nedge,iedge2,
     *  icscode,ier,nef,nen,mbndry,ntets,ierror,icmotype,npoints,
     *  ics,ilen,iedgeiter,length,i4,i3,i2,i1
 
      real*8 crosx1,crosy1,crosz1,volume,crosx,crosy,crosz,
     *  a,b,c,d,e,f
C
      character*132 logmess
C
      character*(*) cmo
      integer nadd
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipint1, int1)
      integer itp1(10000000), isn1(10000000), int1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet(3,10000000), jtet(3,10000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      real*8 xic(10000000), yic(10000000), zic(10000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      integer itetnn(3,1000000),
     *        itetnn1(3,1000000),
     *        itetnn2(3,1000000)
      pointer (ipiedge_tet, iedge_tet)
      pointer (ipiedge_face, iedge_face)
      pointer (ipiedge_edge, iedge_edge)
      integer iedge_tet(6*1000000), iedge_face(6*1000000),
     *        iedge_edge(6*1000000)
C
      parameter (nvalues=2)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(1000000), list_source(nvalues,1000000)
      real*8 xweight_source(nvalues,1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipifadd, ifadd)
      integer itflag(1000000)
      integer ifadd(3,1000000)
C
      integer ieadd(3)
C
      pointer (ipint1add, int1add)
      integer int1add(1000000)
C
      character*32 isubname, iblknam, iprtnam
      character*32 cmolength
      integer itriface0(3), itriface1(3,3)
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C
C ######################################################################
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
      isubname='refine_tri_add'
C
C
C     This was getting called when nadd = 0 which results
C     in a zero length memory request and crash.
C     Could prevent it from being called in the first place
C     but this works.
C
      if(nadd .eq. 0)then
         logmess = "WARNING: refine_tri_add: zero elements to refine "
         call writloga('default',0,logmess,0,ierrw)
         logmess = "WARNING: refine_tri_add: No action, RETURN "
         call writloga('default',0,logmess,0,ierrw)
         return
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
C
C
C     ******************************************************************
C
C     Get the parents for each node.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk('int1add',isubname,ipint1add,length,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=nef*ntets
      call mmgetblk('iedgetet',isubname,ipiedge_tet,length,1,icscode)
      call mmgetblk('iedgefac',isubname,ipiedge_face,length,1,icscode)
      call mmgetblk('iedgeedg',isubname,ipiedge_edge,length,1,icscode)
      length=nen*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,1,icscode)
      length=nef*ntets
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,1,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,1,icscode)
C
      write(logmess,'(a,i10,i10)') 'Edge iteration: ',iedgeiter,nedge
      call writloga('default',0,logmess,0,ierrw)
C
      length=ntets
      call mmgetblk('itflag',isubname,ipitflag,length,1,icscode)
      length=nef*ntets
      call mmgetblk('ifadd',isubname,ipifadd,length,1,icscode)
C
      do it=1,ntets
         do i=1,nen
            itetnn(i,it)=itet(i,it)
         enddo
         do i=1,nelmnef(itettyp(it))
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/nef
               itetnn2(i,it)=jtet(i,it)-mbndry-nef*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/nef
               itetnn2(i,it)=jtet(i,it)-nef*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
C
      length=6*ntets
      call mmgetblk('list_sink',isubname,iplist_sink,length,1,icscode)
      length=nvalues*6*ntets
      call mmgetblk('list_source',isubname,iplist_source,length,1,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      do it=1,ntets
         itflag(it)=0
         do i=1,nelmnef(ifelmtri)
            ifadd(i,it)=0
         enddo
      enddo
C
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      do i=1,nadd
         it=itadd(i)
         if(itflag(it).eq.0) then
            itflag(it)=1
            nef1=nelmnef(ifelmtri)
            do j=1,nef1
               if(ifadd(j,it).eq.0) then
                  nadd1=nadd1+1
                  npointsnew=npointsnew+1
                  iadd(nadd1)=npointsnew
                  call mmfindbk('xic',cmo,ipxic,length,icscode)
                  if((npointsnew+1).gt.length) then
                     npointsinc=npointsnew+1000
                     call cmo_set_info('nnodes',cmo,
     *                                 npointsinc,1,1,ier)
                     call mmgetlen(ipitetclr,nelementsmm,icscode)
                     call cmo_set_info('nelements',cmo,
     *                                 nelementsmm,1,1,ier)
                     call cmo_newlen(cmo,ierror)
                     call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
                     call mmnewlen('iparent',isubname,
     *                             ipiparent,npointsinc,icscode)
                     call mmnewlen('int1add',isubname,
     *                             ipint1add,npointsinc,icscode)
                  endif
                  i1=itet1(itetoff(it)+ielmface1(1,j,ifelmtri))
                  i2=itet1(itetoff(it)+ielmface1(2,j,ifelmtri))
                  list_sink(npointsnew-npoints)=npointsnew
                  list_source(1,npointsnew-npoints)=i1
                  list_source(2,npointsnew-npoints)=i2
                  xweight_source(1,npointsnew-npoints)=1.0
                  xweight_source(2,npointsnew-npoints)=1.0
                  ifadd(j,it)=npointsnew
                  if(jtet1(jtetoff(it)+j).gt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+j)-mbndry-1)/nef
                     jf=jtet1(jtetoff(it)+j)-mbndry-nef*(jt-1)
                     ifadd(jf,jt)=npointsnew
                     int1add(npointsnew-npoints)=1
                  elseif(jtet1(jtetoff(it)+j).gt.0.and.
     *                   jtet1(jtetoff(it)+j).lt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+j)-1)/nef
                     jf=jtet1(jtetoff(it)+j)-nef*(jt-1)
                     ifadd(jf,jt)=npointsnew
                     int1add(npointsnew-npoints)=0
                  endif
               endif
            enddo
         endif
      enddo
C
      do it=1,ntets
         icount=0
         do i=1,nelmnef(ifelmtri)
            if(ifadd(i,it).gt.0) then
               icount=icount+1
               ieadd(icount)=i
            endif
         enddo
         if(icount.gt.0) then
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+3).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('mbndry',cmo,
     *                  mbndry,ilen,itype,icscode)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,leni,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,leni,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,len,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,leni,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,leni,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,leni,icmotype,ierror)
            endif
            call mmgetlen(ipitflag,length,icscode)
            if((ntetsnew+3).gt.length) then
               inc=1000
               call mmgetnam(ipitflag,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       ics)
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       ics)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       ics)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       ics)
            endif
         endif
C
         if(icount.eq.3) then
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            i3=itet1(itetoff(it)+3)
            i23=ifadd(1,it)
            i31=ifadd(2,it)
            i12=ifadd(3,it)
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i12
            itetnn(2,it)=i23
            itetnn(3,it)=i31
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i12
               itetnn(2,ntetsnew)=i2
               itetnn(3,ntetsnew)=i23
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i23
               itetnn(2,ntetsnew)=i3
               itetnn(3,ntetsnew)=i31
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i31
               itetnn(2,ntetsnew)=i1
               itetnn(3,ntetsnew)=i12
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         elseif(icount.eq.2) then
            iedge1=ieadd(1)
            iedge2=ieadd(2)
            if(iedge1.eq.2.and.iedge2.eq.3) then
               iedge=1
               ie1=ifadd(2,it)
               ie2=ifadd(3,it)
            elseif(iedge1.eq.1.and.iedge2.eq.3) then
               iedge=2
               ie1=ifadd(1,it)
               ie2=ifadd(3,it)
            elseif(iedge1.eq.1.and.iedge2.eq.2) then
               iedge=3
               ie1=ifadd(1,it)
               ie2=ifadd(2,it)
            endif
            i2=itet1(itetoff(it)+ielmface1(1,iedge,ifelmtri))
            i3=itet1(itetoff(it)+ielmface1(2,iedge,ifelmtri))
            isum=itet1(itetoff(it)+1)+
     *           itet1(itetoff(it)+2)+
     *           itet1(itetoff(it)+3)
            i1=isum-i2-i3
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i1
            itetnn(2,it)=ie2
            itetnn(3,it)=ie1
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=ie2
               itetnn(2,ntetsnew)=i2
               itetnn(3,ntetsnew)=ie1
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i2
               itetnn(2,ntetsnew)=i3
               itetnn(3,ntetsnew)=ie1
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         elseif(icount.eq.1) then
            iedge=ieadd(1)
            ie1=ifadd(iedge,it)
            i2=itet1(itetoff(it)+ielmface1(1,iedge,ifelmtri))
            i3=itet1(itetoff(it)+ielmface1(2,iedge,ifelmtri))
            isum=itet1(itetoff(it)+1)+
     *           itet1(itetoff(it)+2)+
     *           itet1(itetoff(it)+3)
            i1=isum-i2-i3
            itetclr(it)=itetclr(it)
            itettyp(it)=itettyp(it)
            itetoff(it)=nelmnen(ifelmtri)*(it-1)
            jtetoff(it)=nelmnef(ifelmtri)*(it-1)
            itetnn(1,it)=i1
            itetnn(2,it)=i2
            itetnn(3,it)=ie1
            itetnn1(1,it)=-1
            itetnn1(2,it)=-1
            itetnn1(3,it)=-1
            itetnn2(1,it)=-1
            itetnn2(2,it)=-1
            itetnn2(3,it)=-1
            ntetsnew=ntetsnew+1
               itetclr(ntetsnew)=itetclr(it)
               itettyp(ntetsnew)=itettyp(it)
               itetoff(ntetsnew)=nelmnen(ifelmtri)*(ntetsnew-1)
               jtetoff(ntetsnew)=nelmnef(ifelmtri)*(ntetsnew-1)
               itetnn(1,ntetsnew)=i1
               itetnn(2,ntetsnew)=ie1
               itetnn(3,ntetsnew)=i3
               itetnn1(1,ntetsnew)=-1
               itetnn1(2,ntetsnew)=-1
               itetnn1(3,ntetsnew)=-1
               itetnn2(1,ntetsnew)=-1
               itetnn2(2,ntetsnew)=-1
               itetnn2(3,ntetsnew)=-1
         endif
      enddo
      write(logmess,'(a,i10,a,i10)') 'Edge-refined elems: old=',ntets,
     *                               ' new=',ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,3
                  kt=1+(jtet(i,it)-1)/nef
                  kf=jtet(i,it)-nef*(kt-1)
                  if(kt.gt.0.and.kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,3,3,ntets,npoints,
     *               2,npoints,ntets)
         do it=1,ntets
            do i=1,3
               itet(i,it)=itetnn(i,it)
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  jtet(i,it)=nef*(itetnn1(i,it)-1)+itetnn2(i,it)
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
      endif
C
      goto 9999
 9999 continue
C
      cmolength='nnodes'
      call cmo_interpolate(cmo,cmo,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
C
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
C
      length=npoints
      call mmgetblk("int1",isubname,ipint1,length,1,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      icount=0
      do i=1,nadd1
         i1=list_sink(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         if(int1add(i).eq.1) then
            icount=icount+1
         endif
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0.or.icount.gt.0) then
         call dotaskx3d('settets/parents ; finish',ierror)
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      endif
C
C
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      call mmrelprt(isubname,icscode)
      return
      end
 
c**************************************************************
C PRD version of hex refinement
c harold trease's version of refine_hex_add() from  for
c restricting refinement by prd (principal refine direction)
 
      subroutine refine_hex_prd(cmo_name,
     *                          iprd, nadd,
     *                          ipitadd,
     *                          iadd,xadd,yadd,zadd)
C
C        cmo_name - The name of the CMO.
C        iprd -   The principal refinement direction.
C                 principal refine direction (prd) is based on topology
C                 iprd = 0 refine xyz with refine_hex_add()
C                 iprd = 4 refine xyz with refine_hex_prd()
C                 iprd = 1 refine along x direction (quad edges 1 and 4)
C                 iprd = 2 refine along y direction (quad edges 2 and 3)
C                 iprd = 3 refine along z direction
C                 iprd = 12 refine along x and y direction
C                 iprd = 13 refine along x and z direction
C                 iprd = 23 refine along y and z direction
C        nadd     - Number of nodes to add
C        (ip)itadd - pointer to the list of elems where nodes are added
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
c
c Original Source code:
c Apr 29  2000 /scratch/indigo2/het/ibm/sgi/x3d.all.tar.gz
C     CHANGE HISTORY -
C
C        refine_hex_prd changes:
C
C           Nov 11 2003 tam
C           Comment out interpolation of element attributes
C           Move addatt for xradavg outside routine with rest
C           of AMR added attributes, remove xradavg when done
C
C           Oct 22 2003 tam
C           add implicit none
C           add this routine to refine_tet_add.f
C
C        $Log: refine_tet_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.26   Fri Feb 20 20:50:12 1998   het
CPVCS    Change the dotaskx3d( calles to dotaskgen(
CPVCS
CPVCS       Rev 1.25   Mon Sep 15 12:56:28 1997   het
CPVCS    Add the AMR algorithm for all element types.
CPVCS
CPVCS       Rev 1.24   Sun Feb 23 10:38:52 1997   het
CPVCS    Add the refinement for quads.
c**************************************************************
C
C
       implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
C prd added variables
      integer nadd, iprd, lenout, lenitp1, lenisn1, lenitetpar,
     *  lenitetkid, lenitetlev, lenitetclr, lenitettyp, lenxic,
     *  lenyic, lenzic,lenitetoff, lenjtetoff, lenitet, lenjtet,
     *  naddnew, iflag, itoff, jtoff,istride,ntnew,icount,
     *  length11,length12,length21,length22,length31,length32,
     *  naddtri,naddqud,naddtet,naddpyr,naddpri,naddhex
 
      integer leni,jt,kf,kt,ie11,ie12,ie10,ie9,ie8,ie7,ie6,
     *  ie5,if6,if5,if4,if3,if2,if1,i8,i7,i6,i5,i4,i3,i2,i1,ie4,
     *  ie3,ie2,ie1,inc1,inc2,idum,ics,nnodesmm,inc,ierr,
     *  j,joff,i9,itype,ilen,nelementsmm,npointsinc,it1,ntet1_save,
     *  nptsinc,ierrw,nef1,it,itetiter,ntet1,ntetsnew,npointsnew,
     *  nadd1,irefine,i,icscode,ier,nsdtopo,nen,nef,mbndry,
     *  ntetsinc,ninc,ntets,ierror,icmotype,length,npoints
 
      real*8 radavg, rad1, xradfac, rad2,rad3,radavg12
      real*8 xxsmall
C
      character*(*) cmo_name
C
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipiaddorder, iaddorder)
      integer iaddorder(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer itp1(10000000), isn1(10000000)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(8*1000000), jtet1(6*1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000),
     *        itetkid(1000000),
     *        itetlev(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn1)
      pointer (ipitetnn1, jtetnn1)
      pointer (ipitetnn2, jtetnn2)
      integer itflag(1000000),
     *        itetnn1(8*1000000),
     *        jtetnn1(6*1000000), jtetnn2(6*1000000)
C
      pointer (iplist_sink1, list_sink1)
      pointer (iplist_source1, list_source1)
      pointer (ipxweight_source1, xweight_source1)
      integer list_sink1(nadd), list_source1(1000000)
      real*8 xweight_source1(1000000)
      pointer (iplist_sink2, list_sink2)
      pointer (iplist_source2, list_source2)
      pointer (ipxweight_source2, xweight_source2)
      integer list_sink2(nadd), list_source2(1000000)
      real*8 xweight_source2(1000000)
      pointer (iplist_sink3, list_sink3)
      pointer (iplist_source3, list_source3)
      pointer (ipxweight_source3, xweight_source3)
      integer list_sink3(nadd), list_source3(1000000)
      real*8 xweight_source3(1000000)
c
c prd added ----
      pointer (ipitlist_sink, itlist_sink)
      pointer (ipitlist_source, itlist_source)
      integer itlist_sink(10000000), itlist_source(10000000)
 
      pointer (ipxtweight_source, xtweight_source)
      real*8 xtweight_source(10000000)
 
      pointer (ipxradavg, xradavg)
      real*8 xradavg(1000000)
 
      integer itet0(10)
c prd added ----
C
      integer ifacept(10), iedgept(20)
C
      integer icharlnf
      character*32 cmo
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
      character*32 cmoattnam
      character*8192 cbuff
 
C
C     ###################################################################
C
      isubname='refine_hex_prd'
C
      xxsmall=1.0d-30
C
      cmo=cmo_name
C
C     This was getting called when nadd = 0 which results
C     in a zero length memory request and crash.
C     Could prevent it from being called in the first place
C     but this works.
C
      if(nadd .eq. 0)then
         logmess = "WARNING: refine_hex_prd: zero elements to refine "
         call writloga('default',0,logmess,0,ierrw)
         logmess = "WARNING: refine_hex_prd: No action, RETURN "
         call writloga('default',0,logmess,0,ierrw)
         return
      endif
C
 
C Get work attribute xradavg
 
      cmoattnam='xradavg'
      call mmfindbk(cmoattnam,cmo,ipxradavg,lenout,icscode)
      if(icscode.ne.0) then
         cbuff='cmo/addatt/' //
     *         cmo(1:icharlnf(cmo)) //
     *         '/' //
     *         cmoattnam(1:icharlnf(cmoattnam)) //
     *         '/VDOUBLE' //
     *         '/scalar/nnodes/linear/permanent/gx/0.0' //
     *         ' ; finish '
         call dotaskx3d(cbuff,ierror)
         call mmfindbk(cmoattnam,cmo,ipxradavg,lenout,icscode)
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         do i1=1,npoints
            xradavg(i1)=0
         enddo
      endif
C
      call cmo_newlen(cmo,ierror)
C     *** BE SURE THAT ALL THE ARRAYS IN THE CMO ARE THE SAME LENGTH.
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ier)
      call cmo_get_info('itetpar',cmo,ipitetpar,lenitetpar,icmotype,ier)
      call cmo_get_info('itetkid',cmo,ipitetkid,lenitetkid,icmotype,ier)
      call cmo_get_info('itetlev',cmo,ipitetlev,lenitetlev,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
C     ..................................................................
C     CHECK TO SEE THAT WE CAN REFINE THE ELEMENTS TYPES SPECIFIED.
C        CURRENTLY WE CANNOT REFINE PRISMS AND PRYAMIDS.
C - note that this can be simplified since the routine is called
C   only for hex or quad meshes
      naddnew=0
      do i=1,nadd
         it=itadd(i)
         iflag=0
         if(itettyp(it).eq.ifelmtri) then
            iflag=1
         elseif(itettyp(it).eq.ifelmqud) then
            iflag=1
         elseif(itettyp(it).eq.ifelmtet) then
            iflag=1
         elseif(itettyp(it).eq.ifelmpyr) then
            iflag=1
         elseif(itettyp(it).eq.ifelmpri) then
            iflag=1
         elseif(itettyp(it).eq.ifelmhex) then
            iflag=1
         endif
         if(iflag.eq.1) then
            naddnew=naddnew+1
            itadd(naddnew)=itadd(i)
            iadd(naddnew)=iadd(i)
            xadd(naddnew)=xadd(i)
            yadd(naddnew)=yadd(i)
            zadd(naddnew)=zadd(i)
         endif
      enddo
      if(naddnew.eq.0) then
         goto 9999
      else
         nadd=naddnew
      endif
 
c assign lengths to arrays, iprd and length dependent
C
      length=max(1,nadd)
      call mmgetblk('iaddorder',isubname,ipiaddorder,length,1,icscode)
      do i=1,nadd
         iaddorder(i)=0
      enddo
C
      length11=0
      length12=0
      length21=0
      length22=0
      length31=0
      length32=0
C
      length11=max(1,nadd)
      call mmgetblk('list_sink1',isubname,
     *              iplist_sink1,length11,1,icscode)
      length12=nen*nadd
      call mmgetblk('list_source1',isubname,
     *              iplist_source1,length12,1,icscode)
      call mmgetblk('xweight_source1',isubname,
     *              ipxweight_source1,length12,2,icscode)
 
c    Assign 2D lengths
      if((nen.eq.nelmnen(ifelmtri) .and.
     *       nef.eq.nelmnef(ifelmtri) .and.
     *       nsdtopo.eq.2)
     *    .or.
     *   (nen.eq.nelmnen(ifelmqud) .and.
     *       nef.eq.nelmnef(ifelmqud) .and.
     *       nsdtopo.eq.2)
     *    .or.
     *   (nen.eq.nelmnen(ifelmhyb) .and.
     *       nef.eq.nelmnef(ifelmhyb) .and.
     *       nsdtopo.eq.2)
     *  ) then
 
         length31=nelmnee(ifelmqud)*nadd
         call mmgetblk('list_sink3',isubname,
     *                 iplist_sink3,length31,1,icscode)
         length32=2*nelmnee(ifelmqud)*nadd
         call mmgetblk('list_source3',isubname,
     *                 iplist_source3,length32,1,icscode)
         call mmgetblk('xweight_source3',isubname,
     *                 ipxweight_source3,length32,2,icscode)
 
      elseif(nen.eq.8.and.nef.eq.6.and.nsdtopo.eq.3) then
         length21=nelmnef(ifelmhex)*nadd
         call mmgetblk('list_sink2',isubname,
     *                 iplist_sink2,length21,1,icscode)
         length22=4*nelmnef(ifelmhex)*nadd
         call mmgetblk('list_source2',isubname,
     *                 iplist_source2,length22,1,icscode)
         call mmgetblk('xweight_source2',isubname,
     *                 ipxweight_source2,length22,2,icscode)
C
         length31=nelmnee(ifelmhex)*nadd
         call mmgetblk('list_sink3',isubname,
     *                 iplist_sink3,length31,1,icscode)
         length32=2*nelmnee(ifelmhex)*nadd
         call mmgetblk('list_source3',isubname,
     *                 iplist_source3,length32,1,icscode)
         call mmgetblk('xweight_source3',isubname,
     *                 ipxweight_source3,length32,2,icscode)
 
c    else Assign 3D lengths
      else
         length21=nelmnef(ifelmhyb)*nadd
         call mmgetblk('list_sink2',isubname,
     *                 iplist_sink2,length21,1,icscode)
         length22=4*nelmnef(ifelmhyb)*nadd
         call mmgetblk('list_source2',isubname,
     *                 iplist_source2,length22,1,icscode)
         call mmgetblk('xweight_source2',isubname,
     *                 ipxweight_source2,length22,2,icscode)
C
         length31=nelmnee(ifelmhyb)*nadd
         call mmgetblk('list_sink3',isubname,
     *                 iplist_sink3,length31,1,icscode)
         length32=2*nelmnee(ifelmhyb)*nadd
         call mmgetblk('list_source3',isubname,
     *                 iplist_source3,length32,1,icscode)
         call mmgetblk('xweight_source3',isubname,
     *                 ipxweight_source3,length32,2,icscode)
      endif
C
      if(length11.gt.0) then
         do i=1,length11
            list_sink1(i)=0
         enddo
      endif
      if(length21.gt.0) then
         do i=1,length21
            list_sink2(i)=0
         enddo
      endif
      if(length31.gt.0) then
         do i=1,length31
            list_sink3(i)=0
         enddo
      endif
      if(length12.gt.0) then
         do i=1,length12
            list_source1(i)=0
            xweight_source1(i)=0
         enddo
      endif
      if(length22.gt.0) then
         do i=1,length22
            list_source2(i)=0
            xweight_source2(i)=0
         enddo
      endif
      if(length32.gt.0) then
         do i=1,length32
            list_source3(i)=0
            xweight_source3(i)=0
         enddo
      endif
C
      length=8*nadd
      call mmgetblk('itlist_sink',isubname,
     *              ipitlist_sink,length,1,icscode)
      call mmgetblk('itlist_source',isubname,
     *              ipitlist_source,length,1,icscode)
      call mmgetblk('xtweight_source',isubname,
     *              ipxtweight_source,length,2,icscode)
 
c    Get child-parent chains
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,1,icscode)
      length=nen*ntets
      call mmgetblk("itetnn" ,isubname,ipitetnn ,length,1,icscode)
      length=nef*ntets
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,1,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,1,icscode)
C
      irefine=0
      nadd1=0
      naddtri=0
      naddqud=0
      naddtet=0
      naddpyr=0
      naddpri=0
      naddhex=0
      npointsnew=npoints
      ntetsnew=ntets
      ntet1=nadd
      itetiter=0
  11  continue
      itoff=itetoff(ntets)+nelmnen(itettyp(ntets))
      jtoff=jtetoff(ntets)+nelmnef(itettyp(ntets))
      do it=1,ntets
         itflag(it)=0
      enddo
 
C     LOOP all elements
      do it=1,ntets
         do i=1,nelmnen(itettyp(it))
            itetnn1(itetoff(it)+i)=itet1(itetoff(it)+i)
         enddo
         nef1=nelmnef(itettyp(it))
         do i=1,nef1
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               jtetnn1(jtetoff(it)+i)=0
               jtetnn2(jtetoff(it)+i)=0
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               jtetnn1(jtetoff(it)+i)=
     *                             1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                mbndry -
     *                                nef*(jtetnn1(jtetoff(it)+i)-1)
            else
               jtetnn1(jtetoff(it)+i)=1+(jtet1(jtetoff(it)+i)-1)/nef
               jtetnn2(jtetoff(it)+i)=jtet1(jtetoff(it)+i) -
     *                                nef*(itetnn1(jtetoff(it)+i)-1)
            endif
         enddo
      enddo
      npointsnew=npoints
      ntetsnew=ntets
      itetiter=itetiter+1
      write(logmess,'(a,2i10)') "Element iteration: ",itetiter,ntet1
      call writloga('default',0,logmess,0,ierrw)
 
      if(ntet1.ne.0) then
         nptsinc=0
         if((nen.eq.nelmnen(ifelmtri) .and.
     *          nef.eq.nelmnef(ifelmtri) .and.
     *          nsdtopo.eq.2)
     *       .or.
     *      (nen.eq.nelmnen(ifelmqud) .and.
     *          nef.eq.nelmnef(ifelmqud) .and.
     *          nsdtopo.eq.2)
     *       .or.
     *      (nen.eq.nelmnen(ifelmhyb) .and.
     *          nef.eq.nelmnef(ifelmhyb) .and.
     *          nsdtopo.eq.2)
     *     ) then
            nptsinc=5
         elseif((nen.eq.8.and.nef.eq.6.and.nsdtopo.eq.3)
     *       .or.
     *      (nen.eq.nelmnen(ifelmhyb) .and.
     *          nef.eq.nelmnef(ifelmhyb) .and.
     *          nsdtopo.eq.3)
     *     ) then
            nptsinc=19
         else
            nptsinc=19
         endif
         ntet1_save=0
         do it1=1,ntet1
            it=itadd(it1)
            irefine=irefine+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((npointsnew+nptsinc).gt.length) then
               npointsinc=npointsnew+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,nelementsmm,1,1,ier)
               call cmo_newlen(cmo,ierror)
               call mmnewlen('iparent',isubname,ipiparent,npointsinc,
     *                       icscode)
            endif
C
            nadd1=nadd1+1
            iaddorder(nadd1)=nadd1
 
C           SWITCH on 2D-3D and value of prd (refine direction)
 
C           QUAD
            if(itettyp(it).eq.ifelmtri .or.
     *         itettyp(it).eq.ifelmqud) then
               naddqud=naddqud+1
               if(itettyp(it).eq.ifelmtri) then
                  do i=1,nelmnen(itettyp(it))
                     itet0(i)=itet1(itetoff(it)+i)
                  enddo
                  itet0(4)=itet0(1)
               else
                  do i=1,nelmnen(itettyp(it))
                     itet0(i)=itet1(itetoff(it)+i)
                  enddo
               endif
 
C              QUAD XYZ and two directions fill sink1 and sink3
               if(iprd.eq.0.or.iprd.ge.3) then
                  npointsnew=npointsnew+1
                  iparent(npointsnew)=npointsnew
                  i9=npointsnew
                  list_sink1(naddqud)=npointsnew
                  do i=1,nelmnen(ifelmqud)
                     list_source1(i+nelmnen(ifelmqud)*(naddqud-1))=
     *                  itet0(i)
                     xweight_source1(i+nelmnen(ifelmqud)*(naddqud-1))=1.0
                  enddo
                  istride=nelmnee(ifelmqud)
                  do i=1,nelmnee(ifelmqud)
                     npointsnew=npointsnew+1
                     iparent(npointsnew)=npointsnew
                     iedgept(i)=npointsnew
                     list_sink3(i+istride*(nadd1-1))=npointsnew
                     joff=2*(i-1)+2*istride*(nadd1-1)
                     do j=1,2
                        list_source3(joff+j)=
     *                     itet0(ielmedge1(j,i,ifelmqud))
                        xweight_source3(joff+j)=1.0
                     enddo
                  enddo
               else
 
C                 QUAD X Y fill sink3
                  istride=2
                  icount=0
                  do i=1,nelmnee(ifelmqud)
 
C                    QUAD X use edges 1 and 4
                     if(iprd.eq.1.and.(i.eq.1.or.i.eq.4)) then
                        icount=icount+1
                        npointsnew=npointsnew+1
                        iparent(npointsnew)=npointsnew
                        iedgept(icount)=npointsnew
                        list_sink3(icount+istride*(nadd1-1)) =
     *                                              npointsnew
                        joff=2*(icount-1)+2*istride*(nadd1-1)
                        do j=1,2
                           list_source3(joff+j)=
     *                        itet0(ielmedge1(j,i,ifelmqud))
                           xweight_source3(joff+j)=1.0
                        enddo
 
C                    QUAD Y use edges 2 and 3
                     elseif(iprd.eq.2.and.(i.eq.2.or.i.eq.3)) then
                        icount=icount+1
                        npointsnew=npointsnew+1
                        iparent(npointsnew)=npointsnew
                        iedgept(icount)=npointsnew
                        list_sink3(icount+istride*(nadd1-1)) =
     *                                              npointsnew
                        joff=2*(icount-1)+2*istride*(nadd1-1)
                        do j=1,2
                           list_source3(joff+j)=
     *                        itet0(ielmedge1(j,i,ifelmqud))
                           xweight_source3(joff+j)=1.0
                        enddo
                     endif
                  enddo
               endif
 
C           HEX fill sink1 and sink2
            elseif(itettyp(it).eq.ifelmhex .or.
     *             itettyp(it).eq.ifelmpri .or.
     *             itettyp(it).eq.ifelmpyr .or.
     *             itettyp(it).eq.ifelmtet) then
               naddhex=naddhex+1
               if(itettyp(it).eq.ifelmhex) then
                  do i=1,nelmnen(itettyp(it))
                     itet0(i)=itet1(itetoff(it)+i)
                  enddo
               elseif(itettyp(it).eq.ifelmpri) then
                  naddpri=naddpri+1
                  itet0(1)=itet1(itetoff(it)+1)
                  itet0(2)=itet1(itetoff(it)+1)
                  itet0(3)=itet1(itetoff(it)+2)
                  itet0(4)=itet1(itetoff(it)+3)
                  itet0(5)=itet1(itetoff(it)+4)
                  itet0(6)=itet1(itetoff(it)+4)
                  itet0(7)=itet1(itetoff(it)+5)
                  itet0(8)=itet1(itetoff(it)+6)
               elseif(itettyp(it).eq.ifelmpyr) then
                  naddpyr=naddpyr+1
                  itet0(1)=itet1(itetoff(it)+1)
                  itet0(2)=itet1(itetoff(it)+2)
                  itet0(3)=itet1(itetoff(it)+3)
                  itet0(4)=itet1(itetoff(it)+4)
                  itet0(5)=itet1(itetoff(it)+5)
                  itet0(6)=itet1(itetoff(it)+5)
                  itet0(7)=itet1(itetoff(it)+5)
                  itet0(8)=itet1(itetoff(it)+5)
               elseif(itettyp(it).eq.ifelmtet) then
                  naddtet=naddtet+1
                  itet0(1)=itet1(itetoff(it)+1)
                  itet0(2)=itet1(itetoff(it)+1)
                  itet0(3)=itet1(itetoff(it)+1)
                  itet0(4)=itet1(itetoff(it)+1)
                  itet0(5)=itet1(itetoff(it)+2)
                  itet0(6)=itet1(itetoff(it)+2)
                  itet0(7)=itet1(itetoff(it)+3)
                  itet0(8)=itet1(itetoff(it)+4)
               endif
 
C              HEX XYZ fill sink2 and sink3
               if(iprd.eq.0) then
                  npointsnew=npointsnew+1
                  iparent(npointsnew)=npointsnew
                  i9=npointsnew
                  list_sink1(nadd1)=npointsnew
                  do i=1,nelmnen(ifelmhex)
                     list_source1(i+nen*(nadd1-1))=itet0(i)
                     xweight_source1(i+nen*(nadd1-1))=1.0
                  enddo
                  istride=nelmnef(ifelmhex)
                  do i=1,nelmnef(ifelmhex)
                     npointsnew=npointsnew+1
                     iparent(npointsnew)=npointsnew
                     ifacept(i)=npointsnew
                     list_sink2(i+istride*(nadd1-1))=npointsnew
                     joff=(i-1+istride*(nadd1-1))*
     *                       ielmface0(i,ifelmhex)
                     do j=1,ielmface0(i,ifelmhex)
                        list_source2(joff+j)=
     *                     itet0(ielmface1(j,i,ifelmhex))
                        xweight_source2(joff+j)=1.0
                     enddo
                  enddo
                  istride=nelmnee(ifelmhex)
                  do i=1,nelmnee(ifelmhex)
                     npointsnew=npointsnew+1
                     iparent(npointsnew)=npointsnew
                     iedgept(i)=npointsnew
                     list_sink3(i+istride*(nadd1-1))=npointsnew
                     joff=2*(i-1)+2*istride*(nadd1-1)
                     do j=1,2
                        list_source3(joff+j)=
     *                     itet0(ielmedge1(j,i,ifelmhex))
                        xweight_source3(joff+j)=1.0
                     enddo
                  enddo
               else
C                 HEX in two directions
                  if(iprd.gt.3) then
                     istride=2
                     icount=0
                     do i=1,nelmnef(ifelmhex)
 
C                       HEX XY
                        if(iprd.eq.12 .and.
     *                     (i.eq.1.or.i.eq.2)) then
                           icount=icount+1
                           npointsnew=npointsnew+1
                           iparent(npointsnew)=npointsnew
                           ifacept(icount)=npointsnew
                           list_sink2(icount+istride*(nadd1-1)) =
     *                                                 npointsnew
                           joff=(icount-1+istride*(nadd1-1))*
     *                          ielmface0(i,ifelmhex)
                           do j=1,ielmface0(i,ifelmhex)
                              list_source2(joff+j)=
     *                           itet0(ielmface1(j,i,ifelmhex))
                              xweight_source2(joff+j)=1.0
                           enddo
 
C                       HEX XZ
                        elseif(iprd.eq.13 .and.
     *                     (i.eq.3.or.i.eq.5)) then
                           icount=icount+1
                           npointsnew=npointsnew+1
                           iparent(npointsnew)=npointsnew
                           ifacept(icount)=npointsnew
                           list_sink2(icount+istride*(nadd1-1)) =
     *                                                 npointsnew
                           joff=(icount-1+istride*(nadd1-1))*
     *                          ielmface0(i,ifelmhex)
                           do j=1,ielmface0(i,ifelmhex)
                              list_source2(joff+j)=
     *                           itet0(ielmface1(j,i,ifelmhex))
                              xweight_source2(joff+j)=1.0
 
                           enddo
 
C                       HEX YZ
                        elseif(iprd.eq.23 .and.
     *                     (i.eq.4.or.i.eq.6)) then
                           icount=icount+1
                           npointsnew=npointsnew+1
                           iparent(npointsnew)=npointsnew
                           ifacept(icount)=npointsnew
                           list_sink2(icount+istride*(nadd1-1)) =
     *                                                 npointsnew
                           joff=(icount-1+istride*(nadd1-1))*
     *                          ielmface0(i,ifelmhex)
                           do j=1,ielmface0(i,ifelmhex)
                              list_source2(joff+j)=
     *                           itet0(ielmface1(j,i,ifelmhex))
                              xweight_source2(joff+j)=1.0
                           enddo
                        endif
                     enddo
                  endif
                  if(iprd.ge.1.and.iprd.le.3) then
                     istride=4
                  else
                     istride=8
                  endif
                  icount=0
C
                  do i=1,nelmnee(ifelmhex)
                     if((iprd.eq.1.or.iprd.eq.12.or.iprd.eq.13) .and.
     *                  (i.eq.1.or.i.eq.6.or.i.eq.9.or.i.eq.12)) then
                        icount=icount+1
                        npointsnew=npointsnew+1
                        iparent(npointsnew)=npointsnew
                        iedgept(icount)=npointsnew
                        list_sink3(icount+istride*(nadd1-1))=npointsnew
                        joff=2*(icount-1)+2*istride*(nadd1-1)
                        do j=1,2
                           list_source3(joff+j)=
     *                        itet0(ielmedge1(j,i,ifelmhex))
                           xweight_source3(joff+j)=1.0
                        enddo
                     endif
                  enddo
                  do i=1,nelmnee(ifelmhex)
                     if((iprd.eq.2.or.iprd.eq.12.or.iprd.eq.23) .and.
     *                  (i.eq.2.or.i.eq.4.or.i.eq.10.or.i.eq.11)) then
                        icount=icount+1
                        npointsnew=npointsnew+1
                        iparent(npointsnew)=npointsnew
                        iedgept(icount)=npointsnew
                        list_sink3(icount+istride*(nadd1-1))=npointsnew
                        joff=2*(icount-1)+2*istride*(nadd1-1)
                        do j=1,2
                           list_source3(joff+j)=
     *                        itet0(ielmedge1(j,i,ifelmhex))
                           xweight_source3(joff+j)=1.0
                        enddo
                     endif
                  enddo
                  do i=1,nelmnee(ifelmhex)
                     if((iprd.eq.3.or.iprd.eq.13.or.iprd.eq.23) .and.
     *                  (i.eq.3.or.i.eq.5.or.i.eq.7.or.i.eq.8)) then
                        icount=icount+1
                        npointsnew=npointsnew+1
                        iparent(npointsnew)=npointsnew
                        iedgept(icount)=npointsnew
                        list_sink3(icount+istride*(nadd1-1))=npointsnew
                        joff=2*(icount-1)+2*istride*(nadd1-1)
                        do j=1,2
                           list_source3(joff+j)=
     *                        itet0(ielmedge1(j,i,ifelmhex))
                           xweight_source3(joff+j)=1.0
                        enddo
                     endif
                  enddo
               endif
            endif
C
            if(itettyp(it).eq.ifelmtri) then
               if(iprd.eq.0.or.iprd.ge.3) then
                  ninc=4
               else
                  ninc=2
               endif
            elseif(itettyp(it).eq.ifelmqud) then
               if(iprd.eq.0.or.iprd.eq.3) then
                  ninc=4
               else
                  ninc=2
               endif
            elseif(itettyp(it).eq.ifelmtet) then
                if(iprd.eq.0) then
                   ninc=8
                elseif(iprd.ge.1.or.iprd.le.3) then
                   ninc=2
                else
                   ninc=4
                endif
            elseif(itettyp(it).eq.ifelmpyr) then
                if(iprd.eq.0) then
                   ninc=8
                elseif(iprd.ge.1.or.iprd.le.3) then
                   ninc=2
                else
                   ninc=4
                endif
            elseif(itettyp(it).eq.ifelmpri) then
                if(iprd.eq.0) then
                   ninc=6
                elseif(iprd.ge.1.or.iprd.le.3) then
                   ninc=2
                else
                   ninc=4
                endif
            elseif(itettyp(it).eq.ifelmhex) then
               if(iprd.eq.0) then
                  ninc=8
               elseif(iprd.ge.1.and.iprd.le.3) then
                  ninc=2
               else
                  ninc=4
               endif
            else
               ninc=0
            endif
            call mmgetlen(ipitlist_sink,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               call mmincblk('itlist_sink',isubname,
     *                       ipitlist_sink,inc,icscode)
               call mmincblk('itlist_source',isubname,
     *                       ipitlist_source,inc,icscode)
               call mmincblk('xtweight_source',isubname,
     *                       ipxtweight_source,inc,icscode)
            endif
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('itetpar',cmo,
     *                           ipitetpar,lenitetpar,icmotype,ier)
               call cmo_get_info('itetkid',cmo,
     *                           ipitetkid,lenitetkid,icmotype,ier)
               call cmo_get_info('itetlev',cmo,
     *                           ipitetlev,lenitetlev,icmotype,ier)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,lenitetclr,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,lenitettyp,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,lenitetoff,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,lenjtetoff,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,lenitet,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,lenjtet,icmotype,ierror)
            endif
            call mmgetlen(ipitflag,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               call mmgetnam(ipitflag,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       ics)
               do idum=ntetsnew+1,ntetsnew+inc
                  itflag(idum)=0
               enddo
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       ics)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       ics)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       ics)
            endif
 
c           QUAD refine
            if(itettyp(it).eq.ifelmtri .or.
     *         itettyp(it).eq.ifelmqud) then
C
C              1x4 quad refinement order:
C                  1 - lower left
C                  2 - lower right
C                  3 - upper left
C                  4 - upper right
C
               i1=itet0(1)
               i2=itet0(2)
               i3=itet0(3)
               i4=itet0(4)
C
               itetkid(it)=ntetsnew+1
C
c              changed -  if(iprd.eq.0.or.iprd.eq.3) then
               if(iprd.eq.0.or.iprd.ge.3) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=i9
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie3
                     itetnn1(itetoff(ntetsnew)+4)=i9
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie2
                     itetnn1(itetoff(ntetsnew)+2)=i9
                     itetnn1(itetoff(ntetsnew)+3)=ie4
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i9
                     itetnn1(itetoff(ntetsnew)+2)=ie3
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.1) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.2) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=ie1
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmqud
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=ie2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               endif
 
c           HEX refine
            elseif(itettyp(it).eq.ifelmtet .or.
     *             itettyp(it).eq.ifelmpyr .or.
     *             itettyp(it).eq.ifelmpri .or.
     *             itettyp(it).eq.ifelmhex) then
C
C                  hex refinement order:
C                  1 - bottom lower left
C                  2 - bottom lower right
C                  3 - bottom upper left
C                  4 - bottom upper right
C                  5 - top lower left
C                  6 - top lower right
C                  7 - top upper left
C                  8 - top upper right
C
               i1=itet0(1)
               i2=itet0(2)
               i3=itet0(3)
               i4=itet0(4)
               i5=itet0(5)
               i6=itet0(6)
               i7=itet0(7)
               i8=itet0(8)
C
               itetkid(it)=ntetsnew+1
C
               if(iprd.eq.0) then
                  if1=ifacept(1)
                  if2=ifacept(2)
                  if3=ifacept(3)
                  if4=ifacept(4)
                  if5=ifacept(5)
                  if6=ifacept(6)
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ie5=iedgept(5)
                  ie6=iedgept(6)
                  ie7=iedgept(7)
                  ie8=iedgept(8)
                  ie9=iedgept(9)
                  ie10=iedgept(10)
                  ie11=iedgept(11)
                  ie12=iedgept(12)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=if1
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=if3
                     itetnn1(itetoff(ntetsnew)+7)=i9
                     itetnn1(itetoff(ntetsnew)+8)=if6
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie4
                     itetnn1(itetoff(ntetsnew)+4)=if1
                     itetnn1(itetoff(ntetsnew)+5)=if3
                     itetnn1(itetoff(ntetsnew)+6)=ie5
                     itetnn1(itetoff(ntetsnew)+7)=if4
                     itetnn1(itetoff(ntetsnew)+8)=i9
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie2
                     itetnn1(itetoff(ntetsnew)+2)=if1
                     itetnn1(itetoff(ntetsnew)+3)=ie6
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=if6
                     itetnn1(itetoff(ntetsnew)+6)=i9
                     itetnn1(itetoff(ntetsnew)+7)=if5
                     itetnn1(itetoff(ntetsnew)+8)=ie8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if1
                     itetnn1(itetoff(ntetsnew)+2)=ie4
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie6
                     itetnn1(itetoff(ntetsnew)+5)=i9
                     itetnn1(itetoff(ntetsnew)+6)=if4
                     itetnn1(itetoff(ntetsnew)+7)=ie7
                     itetnn1(itetoff(ntetsnew)+8)=if5
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie3
                     itetnn1(itetoff(ntetsnew)+2)=if3
                     itetnn1(itetoff(ntetsnew)+3)=i9
                     itetnn1(itetoff(ntetsnew)+4)=if6
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=ie9
                     itetnn1(itetoff(ntetsnew)+7)=if2
                     itetnn1(itetoff(ntetsnew)+8)=ie10
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if3
                     itetnn1(itetoff(ntetsnew)+2)=ie5
                     itetnn1(itetoff(ntetsnew)+3)=if4
                     itetnn1(itetoff(ntetsnew)+4)=i9
                     itetnn1(itetoff(ntetsnew)+5)=ie9
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=ie11
                     itetnn1(itetoff(ntetsnew)+8)=if2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if6
                     itetnn1(itetoff(ntetsnew)+2)=i9
                     itetnn1(itetoff(ntetsnew)+3)=if5
                     itetnn1(itetoff(ntetsnew)+4)=ie8
                     itetnn1(itetoff(ntetsnew)+5)=ie10
                     itetnn1(itetoff(ntetsnew)+6)=if2
                     itetnn1(itetoff(ntetsnew)+7)=ie12
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i9
                     itetnn1(itetoff(ntetsnew)+2)=if4
                     itetnn1(itetoff(ntetsnew)+3)=ie7
                     itetnn1(itetoff(ntetsnew)+4)=if5
                     itetnn1(itetoff(ntetsnew)+5)=if2
                     itetnn1(itetoff(ntetsnew)+6)=ie11
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=ie12
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.1) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=ie3
                     itetnn1(itetoff(ntetsnew)+7)=ie4
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=ie4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.2) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=ie1
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=ie4
                     itetnn1(itetoff(ntetsnew)+8)=ie3
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=ie2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=ie4
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.3) then
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=ie1
                     itetnn1(itetoff(ntetsnew)+6)=ie2
                     itetnn1(itetoff(ntetsnew)+7)=ie3
                     itetnn1(itetoff(ntetsnew)+8)=ie4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=ie2
                     itetnn1(itetoff(ntetsnew)+3)=ie3
                     itetnn1(itetoff(ntetsnew)+4)=ie4
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.12) then
                  if1=ifacept(1)
                  if2=ifacept(2)
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ie5=iedgept(5)
                  ie6=iedgept(6)
                  ie7=iedgept(7)
                  ie8=iedgept(8)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=if1
                     itetnn1(itetoff(ntetsnew)+4)=ie5
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=ie3
                     itetnn1(itetoff(ntetsnew)+7)=if2
                     itetnn1(itetoff(ntetsnew)+8)=ie7
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie6
                     itetnn1(itetoff(ntetsnew)+4)=if1
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=ie8
                     itetnn1(itetoff(ntetsnew)+8)=if2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie5
                     itetnn1(itetoff(ntetsnew)+2)=if1
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=ie7
                     itetnn1(itetoff(ntetsnew)+6)=if2
                     itetnn1(itetoff(ntetsnew)+7)=ie4
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if1
                     itetnn1(itetoff(ntetsnew)+2)=ie6
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itetnn1(itetoff(ntetsnew)+5)=if2
                     itetnn1(itetoff(ntetsnew)+6)=ie8
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=ie4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.13) then
                  if1=ifacept(1)
                  if2=ifacept(2)
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ie5=iedgept(5)
                  ie6=iedgept(6)
                  ie7=iedgept(7)
                  ie8=iedgept(8)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=ie1
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=ie5
                     itetnn1(itetoff(ntetsnew)+6)=if1
                     itetnn1(itetoff(ntetsnew)+7)=if2
                     itetnn1(itetoff(ntetsnew)+8)=ie8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=ie2
                     itetnn1(itetoff(ntetsnew)+5)=if1
                     itetnn1(itetoff(ntetsnew)+6)=ie6
                     itetnn1(itetoff(ntetsnew)+7)=ie7
                     itetnn1(itetoff(ntetsnew)+8)=if2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie5
                     itetnn1(itetoff(ntetsnew)+2)=if1
                     itetnn1(itetoff(ntetsnew)+3)=if2
                     itetnn1(itetoff(ntetsnew)+4)=ie8
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=ie3
                     itetnn1(itetoff(ntetsnew)+7)=ie4
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if1
                     itetnn1(itetoff(ntetsnew)+2)=ie6
                     itetnn1(itetoff(ntetsnew)+3)=ie7
                     itetnn1(itetoff(ntetsnew)+4)=if2
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=ie4
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               elseif(iprd.eq.23) then
                  if1=ifacept(1)
                  if2=ifacept(2)
                  ie1=iedgept(1)
                  ie2=iedgept(2)
                  ie3=iedgept(3)
                  ie4=iedgept(4)
                  ie5=iedgept(5)
                  ie6=iedgept(6)
                  ie7=iedgept(7)
                  ie8=iedgept(8)
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=i1
                     itetnn1(itetoff(ntetsnew)+2)=i2
                     itetnn1(itetoff(ntetsnew)+3)=ie2
                     itetnn1(itetoff(ntetsnew)+4)=ie1
                     itetnn1(itetoff(ntetsnew)+5)=ie5
                     itetnn1(itetoff(ntetsnew)+6)=ie6
                     itetnn1(itetoff(ntetsnew)+7)=if1
                     itetnn1(itetoff(ntetsnew)+8)=if2
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie1
                     itetnn1(itetoff(ntetsnew)+2)=ie2
                     itetnn1(itetoff(ntetsnew)+3)=i3
                     itetnn1(itetoff(ntetsnew)+4)=i4
                     itetnn1(itetoff(ntetsnew)+5)=if2
                     itetnn1(itetoff(ntetsnew)+6)=if1
                     itetnn1(itetoff(ntetsnew)+7)=ie7
                     itetnn1(itetoff(ntetsnew)+8)=ie8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=ie5
                     itetnn1(itetoff(ntetsnew)+2)=ie6
                     itetnn1(itetoff(ntetsnew)+3)=if1
                     itetnn1(itetoff(ntetsnew)+4)=if2
                     itetnn1(itetoff(ntetsnew)+5)=i5
                     itetnn1(itetoff(ntetsnew)+6)=i6
                     itetnn1(itetoff(ntetsnew)+7)=ie4
                     itetnn1(itetoff(ntetsnew)+8)=ie3
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                  ntetsnew=ntetsnew+1
                     itlist_sink(ntetsnew-ntets)=ntetsnew
                     itlist_source(ntetsnew-ntets)=it
                     xtweight_source(ntetsnew-ntets)=1.0d+00
                     itflag(ntetsnew)=1
                     itetpar(ntetsnew)=it
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=itetlev(it)+1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmhex
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itetnn1(itetoff(ntetsnew)+1)=if2
                     itetnn1(itetoff(ntetsnew)+2)=if1
                     itetnn1(itetoff(ntetsnew)+3)=ie7
                     itetnn1(itetoff(ntetsnew)+4)=ie8
                     itetnn1(itetoff(ntetsnew)+5)=ie3
                     itetnn1(itetoff(ntetsnew)+6)=ie4
                     itetnn1(itetoff(ntetsnew)+7)=i7
                     itetnn1(itetoff(ntetsnew)+8)=i8
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
               endif
            endif
         enddo
      endif
      write(logmess,'(a,i10,a,i10)')
     *   "Octree(PRD) refined elems: old=",ntets," new=",ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         ntnew=ntetsnew-ntets
c         cmolength='nelements'
c         call cmo_interpolate(cmo_name,cmo_name,
c     *                        cmolength,
c     *                        ntnew,1,
c     *                        itlist_sink,
c     *                        itlist_source,xtweight_source,
c     *                        ierror)
c         if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
         do it=1,ntetsnew
            do i=1,nelmnen(itettyp(it))
               itet1(itetoff(it)+i)=itetnn1(itetoff(it)+i)
            enddo
            do i=1,nelmnen(itettyp(it))
               itetnn1(itetoff(it)+i)=iparent(itetnn1(itetoff(it)+i))
            enddo
            do i=1,nelmnef(itettyp(it))
               jtetnn1(jtetoff(it)+i)=-1
               jtetnn2(jtetoff(it)+i)=-1
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,nelmnef(itettyp(it))
                  if(jtet1(jtetoff(it)+i).gt.0) then
                     kt=1+(jtet1(jtetoff(it)+i)-1)/nef
                     kf=jtet1(jtetoff(it)+i)-nef*(kt-1)
                     if(kt.le.ntets) then
                        jtet1(jtetoff(kt)+kf)=-1
                     endif
                     jtet1(jtetoff(it)+i)=-1
                  endif
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C*****   call geniee_cmo(cmo)
C*****   call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
C*****   do it=1,ntets
C*****      do i=1,nelmnef(itettyp(it))
C*****         if(jtet1(jtetoff(it)+i).gt.0 .and.
C*****            jtet1(jtetoff(it)+i).lt.mbndry) then
C*****            jt=1+(jtet1(jtetoff(it)+i)-1)/nef
C*****            if(itetclr(it).eq.itetclr(jt)) then
C*****            else
C*****               jtet1(jtetoff(it)+i)=jtet1(jtetoff(it)+i)+mbndry
C*****            endif
C*****         else
C*****            jtet1(jtetoff(it)+i)=mbndry
C*****         endif
C*****      enddo
C*****   enddo
         if(ntet1_save.gt.0) then
            ntet1=ntet1_save
            goto 11
         endif
      endif
      goto 9998
 9998 continue
C
C     FILL XYZ COORDS for QUAD
      if((nen.eq.nelmnen(ifelmtri) .and.
     *       nef.eq.nelmnef(ifelmtri) .and.
     *       nsdtopo.eq.2)
     *    .or.
     *   (nen.eq.nelmnen(ifelmqud) .and.
     *       nef.eq.nelmnef(ifelmqud) .and.
     *       nsdtopo.eq.2)
     *    .or.
     *   (nen.eq.nelmnen(ifelmhyb) .and.
     *       nef.eq.nelmnef(ifelmhyb) .and.
     *       nsdtopo.eq.2)
     *  ) then
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
         call cmo_get_info('xradavg',cmo,ipxradavg,length,icmotype,ierr)
         if(naddqud.gt.0) then
 
c           QUAD refine XYZ or two directions
c           changed - if(iprd.eq.0.or.iprd.eq.3) then
            if(iprd.eq.0.or.iprd.ge.3) then
               cmolength='nnodes'
               call cmo_interpolate(cmo_name,cmo_name,
     *                              cmolength,
     *                              naddqud,nelmnen(ifelmqud),
     *                              list_sink1,
     *                              list_source1,xweight_source1,
     *                              ierr)
               if(ierr.ne.0) call x3d_error(isubname,'cmo_interpolate')
               do i=1,naddqud
                  i1=list_sink1(i)
                  iflag=0
                  radavg=0.0
                  do j=1,nelmnen(ifelmqud)
                     i2=list_source1(j+nelmnen(ifelmqud)*(i-1))
                     if(xradavg(i2).eq.2) then
                        iflag=iflag+1
                     endif
                     radavg=radavg+sqrt(xic(i2)**2 +
     *                                  yic(i2)**2 +
     *                                  zic(i2)**2)
                  enddo
                  if(iflag.eq.nelmnen(ifelmqud)) then
                     radavg=radavg/nelmnen(ifelmqud)
                     rad1=sqrt(xic(i1)**2+yic(i1)**2+zic(i1)**2)
                     xradfac=radavg/(rad1+1.0d-30)
                     xic(i1)=xic(i1)*xradfac
                     yic(i1)=yic(i1)*xradfac
                     zic(i1)=zic(i1)*xradfac
                     xradavg(i1)=2
                  endif
               enddo
               cmolength='nnodes'
               call cmo_interpolate(cmo_name,cmo_name,
     *                              cmolength,
     *                              4*nadd1,2,
     *                              list_sink3,
     *                              list_source3,xweight_source3,
     *                              ierr)
               if(ierr.ne.0) call x3d_error(isubname,'cmo_interpolate')
               do i=1,4*nadd1
                  i1=list_source3(1+2*(i-1))
                  i2=list_source3(2+2*(i-1))
                  if(xradavg(i1).eq.2.and.xradavg(i2).eq.2) then
                     i3=list_sink3(i)
                     rad1=sqrt(xic(i1)**2+yic(i1)**2+zic(i1)**2)
                     rad2=sqrt(xic(i2)**2+yic(i2)**2+zic(i2)**2)
                     rad3=sqrt(xic(i3)**2+yic(i3)**2+zic(i3)**2)
                     radavg12=0.5d+00*(rad1+rad2)
                     xradfac=radavg12/(rad3+1.0d-30)
                     xic(i3)=xic(i3)*xradfac
                     yic(i3)=yic(i3)*xradfac
                     zic(i3)=zic(i3)*xradfac
                     xradavg(i3)=2
                  endif
               enddo
 
            else
C              QUAD Refine one direction
               cmolength='nnodes'
               call cmo_interpolate(cmo_name,cmo_name,
     *                              cmolength,
     *                              2*nadd1,2,
     *                              list_sink3,
     *                              list_source3,xweight_source3,
     *                              ierr)
               if(ierr.ne.0) call x3d_error(isubname,'cmo_interpolate')
               do i=1,2*nadd1
                  i1=list_source3(1+2*(i-1))
                  i2=list_source3(2+2*(i-1))
                  if(xradavg(i1).eq.2.and.xradavg(i2).eq.2) then
                     i3=list_sink3(i)
                     rad1=sqrt(xic(i1)**2+yic(i1)**2+zic(i1)**2)
                     rad2=sqrt(xic(i2)**2+yic(i2)**2+zic(i2)**2)
                     rad3=sqrt(xic(i3)**2+yic(i3)**2+zic(i3)**2)
                     radavg12=0.5d+00*(rad1+rad2)
                     xradfac=radavg12/(rad3+1.0d-30)
                     xic(i3)=xic(i3)*xradfac
                     yic(i3)=yic(i3)*xradfac
                     zic(i3)=zic(i3)*xradfac
                     xradavg(i3)=2
                  endif
               enddo
            endif
         endif
 
      elseif(nsdtopo.eq.3) then
 
c        HEX refine three directions
         if(iprd.eq.0) then
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           nadd1,nen,
     *                           list_sink1,
     *                           list_source1,xweight_source1,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           6*nadd1,4,
     *                           list_sink2,
     *                           list_source2,xweight_source2,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           12*nadd1,2,
     *                           list_sink3,
     *                           list_source3,xweight_source3,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
 
c        HEX refine one direction
         elseif(iprd.ge.1.and.iprd.le.3) then
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           4*nadd1,2,
     *                           list_sink3,
     *                           list_source3,xweight_source3,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
 
c        HEX refine two directions
         else
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           2*nadd1,4,
     *                           list_sink2,
     *                           list_source2,xweight_source2,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
            cmolength='nnodes'
            call cmo_interpolate(cmo_name,cmo_name,
     *                           cmolength,
     *                           8*nadd1,2,
     *                           list_sink3,
     *                           list_source3,xweight_source3,
     *                           ierror)
            if(ierror.ne.0) then
               call x3d_error(isubname,'cmo_interpolate')
            endif
         endif
      endif
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ier)
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(naddqud.gt.0.and.naddtri.gt.0) then
         call cmo_to_hybrid(cmo)
      elseif(naddhex.gt.0.and.
     *      (naddtet.gt.0.or.naddpyr.gt.0.or.naddpri.gt.0)) then
         call cmo_to_hybrid(cmo)
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
      return
      end
 
