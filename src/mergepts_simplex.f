      subroutine mergepts_simplex(imerge,nmrg,cmo,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine merges the NMRG point pairs in IMERGE
C        in CMO, where CMO is a tetrahedral mesh.
C
C     INPUT ARGUMENTS -
C
C       IMERGE -- merge array.  IMERGE(1,i) is I'th survivor point,
C                    IMERGE(2,i) is I'th removed point.
C       NMRG   -- number of pairs in IMERGE.
C       CMO    -- current mesh object.
C
C     OUTPUT ARGUMENTS -
C
C        IERROR -- error return.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/mergepts_simplex.f_a  $
CPVCS    
CPVCS       Rev 1.8   30 Aug 2002 10:13:56   dcg
CPVCS    check that merge candidate has not already been merged away
CPVCS    
CPVCS       Rev 1.7   16 Jan 2002 11:02:24   dcg
CPVCS    change inversion test for 2d triangles to use area epsilon
CPVCS    in place of test on true zero.
CPVCS    
CPVCS       Rev 1.6   14 Jan 2000 11:57:26   jtg
CPVCS    fixed misspelling
CPVCS    
CPVCS       Rev 1.5   13 Jan 2000 18:49:18   jtg
CPVCS    jtet set to mbndry before call to rmpoint/element
CPVCS    so that jtet is valid when addmesh_delete is encountered.
CPVCS    
CPVCS       Rev 1.4   Tue Oct 12 08:22:52 1999   kuprat
CPVCS    We now use EPSILONV for volume tolerance, rather than EPSILONL.
CPVCS    
CPVCS       Rev 1.3   Fri Oct 23 16:23:22 1998   kuprat
CPVCS    We now call SYNTHNORMAL to determine a synthetic normal for
CPVCS    deciding whether to accept or reject a merge on a triangulated 
CPVCS    surface.  We deleted CORRECTPC which now exists elsewhere.
CPVCS    
CPVCS       Rev 1.2   Fri Sep 25 15:38:02 1998   kuprat
CPVCS    Corrected error where nonexistent tet opposite JSURV=MBNDRY face
CPVCS    was written to.
CPVCS    
CPVCS       Rev 1.1   Fri Jun 05 14:57:16 1998   dcg
CPVCS    add finish to dotask
CPVCS    fix nsdtopo tests
CPVCS
CPVCS       Rev 1.0   Wed Jun 03 08:39:24 1998   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   Sun May 24 00:16:56 1998   kuprat
CPVCS    Removed call to nonexistent TETTEST command.
CPVCS
CPVCS       Rev 1.1   Sat May 23 23:44:50 1998   kuprat
CPVCS    Deleted debug code.
CPVCS
CPVCS       Rev 1.0   Sat May 23 23:42:50 1998   kuprat
CPVCS    Initial revision.
C ######################################################################
C
      implicit none
 
      include 'local_element.h'
      include 'chydro.h'
      include 'cmerge.h'
      include 'consts.h'
 
      integer imerge(2,*),ierror
      character*32 cmo
 
      pointer (ipxic,xic)
      real*8 xic(*)
      pointer (ipyic,yic)
      real*8 yic(*)
      pointer (ipzic,zic)
      real*8 zic(*)
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      pointer (ipitet,itet)
      integer itet(*)
      pointer (ipitetoff,itetoff)
      integer itetoff(*)
      pointer (ipjtet,jtet)
      integer jtet(*)
      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipisn1,isn1)
      integer isn1(*)
      pointer (ipicr1,icr1)
      integer icr1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipicontab,icontab)
      integer icontab(50,*)
 
      pointer (ipiseed,iseed)
      integer iseed(*)
      pointer (ipiparent,iparent)
      integer iparent(*)
      pointer (ipielts,ielts)
      integer ielts(*)
      pointer (iplmoved,lmoved)
      logical lmoved(*)
 
      integer i,nelements,ityp,j,nod,ilen,itype,ierr,nnodes,icscode,
     &   isurv,iremov,mt1,mt2,mc1,mc2,j1,j2,ierrw,
     &   itemp(maxnen),iseedtet,nmrg,nsdtopo,
     &   locnod,nelts,nef_cmo,mbndry,ielt,k,i1,i2,i3,i4,nbr,
     &   length,jsurv,jfsurv,jtsurv,jremov,jfremov,jtremov,
     &   nextnod,ipar,iparsurv,iparremov,nod1,itsurv,itremov
      real*8 vtol,epsilonl,area(3),vol,synthx,synthy,synthz,dot,
     &   ax,ay,az,epsilonv,epsilona
      character*32 isubname
      character*132 logmess
      logical lgotisurv,lgotiremov,lgoreverse
 
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,crosx,crosy,crosz
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      isubname='mergepts_tet.f'
 
c.... Get EPSILONL,EPSILONA,EPSILONV
 
      call get_epsilon('epsilonl', epsilonl)
      call get_epsilon('epsilona', epsilona)
      call get_epsilon('epsilonv', epsilonv)
 
c.... Get cmo pointers.
 
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,ierr)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,ierr)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,ierr)
      call cmo_get_info('ndimensions_topo',cmo,nsdtopo,ilen,itype,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierr)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierr)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,itype,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ierr)
      call cmo_get_info('faces_per_element',cmo,nef_cmo,ilen,itype,ierr)
      call cmo_get_info('icontab',cmo,ipicontab,ilen,itype,ierr)
 
c.... Get space for local arrays.
 
      call mmgetblk('iseed',isubname,ipiseed,nnodes,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call mmgetblk('lmoved',isubname,iplmoved,nnodes,1,icscode)
 
      length=100
      call mmgetblk('ielts',isubname,ipielts,length,1,icscode)
 
      do i=1,nnodes
         lmoved(i)=.false.
         iseed(i)=0
      enddo
 
c.... Define IPARENT array.
 
      call unpackpc(nnodes,itp1,isn1,iparent,icscode)
 
c.... Loop through elements to fill seed tet list.
 
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnen(ityp)
            nod=itet(j+itetoff(i))
            iseed(iparent(nod))=(i-1)*maxnen+j
         enddo
      enddo
 
c.... Loop through merge pairs and attempt to merge them.
 
      do 10 i=1,nmrg
         isurv=imerge(1,i)
         iremov=imerge(2,i)
         iparsurv=iparent(isurv)
         iparremov=iparent(iremov)
         if (lmoved(isurv).or.lmoved(iremov)) goto 10
         itsurv=itp1(isurv)
         itremov=itp1(iremov)
         if (itsurv.eq.ifitpmrg.or.itremov.eq.ifitpmrg) go to 10
 
c.... Compute MT1, MT2, MC1, MC2.
c.... MT1=1 <==> merge table allows merge with survivor ISURV
c.... MT2=1 <==> merge table allows merge with survivor IREMOV
c.... MC1=1 <==> contraint inclusion allows merge with survivor ISURV
c.... MC2=1 <==> constraint inclusion allows merge with survivor IREMOV
 

         mt1=mtable(itsurv,itremov)
         mt2=mtable(itremov,itsurv)
 
         if (icr1(isurv).eq.0.and.icr1(iremov).eq.0) then
            mc1=1
            mc2=1
         elseif (icr1(isurv).ne.0.and.icr1(iremov).eq.0) then
            mc1=1
            mc2=0
         elseif (icr1(isurv).eq.0.and.icr1(iremov).ne.0) then
            mc1=0
            mc2=1
         else
            mc1=1
            do 30 j2=1,icontab(1,icr1(iremov))
               do j1=1,icontab(1,icr1(isurv))
                  if (icontab(2+j1,icr1(isurv)).eq.
     &               icontab(2+j2,icr1(iremov))) goto 30
               enddo
               mc1=0
 30         continue
            mc2=1
            do 20 j1=1,icontab(1,icr1(isurv))
               do j2=1,icontab(1,icr1(iremov))
                  if (icontab(2+j1,icr1(isurv)).eq.
     &               icontab(2+j2,icr1(iremov))) goto 20
               enddo
               mc2=0
 20         continue
         endif
 
 
c.... Check if MTi, MCi values allow merge with point
c.... swapping allowed.
 
         if ((mt1.eq.0.or.mc1.eq.0).and.(mt2.eq.0.or.mc2.eq.0)) then
            write(logmess,'(a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FAIL : ITP and/or ICR values do not allow merge'
            call writloga('default',0,logmess,0,ierrw)
            goto 10
         endif
 
c.... We've turned off the swapping option.
 
         if (mt1.eq.0.or.mc1.eq.0) then
 
            write(logmess,'(a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FAIL : ITP and/or ICR values do not allow merge'
            call writloga('default',0,logmess,0,ierrw)
            goto 10
 
c$$$            itmp=isurv
c$$$            isurv=iremov
c$$$            iremov=itmp
c$$$
c$$$            itmp=iparsurv
c$$$            iparsurv=iparremov
c$$$            iparremov=itmp
         endif
 
c.... Check that removed point is attached to a seed element.
 
         if (iseed(iparremov).eq.0) then
            write(logmess,'(a,i9,a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FAIL : ',iremov,' not attached to an element'
            call writloga('default',0,logmess,0,ierrw)
            goto 10
         endif
 
         iseedtet=(iseed(iparremov)-1)/maxnen+1
         locnod=iseed(iparremov)-(iseedtet-1)*maxnen
 
c.... Retrieve elements that share IREMOV.
 
         call get_elements_around_node(iseedtet,locnod,nelts,ipielts,
     *      itetoff,jtetoff,itet,jtet,itettyp,iparent,
     *      nef_cmo,mbndry)
 
c.... Check that ISURV, IREMOV both belong to a retrieved element.
 
         do j=1,nelts
            ielt=ielts(j)
            ityp=itettyp(ielt)
            lgotisurv=.false.
            lgotiremov=.false.
            do k=1,nelmnen(ityp)
               nod=itet(k+itetoff(ielt))
               if (nod.eq.isurv) lgotisurv=.true.
               if (nod.eq.iremov) lgotiremov=.true.
            enddo
            if (lgotisurv.and.lgotiremov) goto 40
         enddo
 
         write(logmess,'(a,i9,a,i9,a)')
     &      'Try ',isurv,'<--',iremov,
     &      ':  FAIL : nodes not adjacent.'
         call writloga('default',0,logmess,0,ierrw)
         goto 10
 
c.... Check that merge won't invert tetrahedra.
 
 40      continue
         if(nsdtopo.eq.3) then
            vtol=epsilonv
            do 50 j=1,nelts
               ielt=ielts(j)
               ityp=itettyp(ielt)
 
               do j1=1,nelmnen(ityp)
                  nbr=iparent(itet(j1+itetoff(ielt)))
                  if (nbr.eq.iparsurv) then
                     goto 50
                  else
                     if (nbr.eq.iparremov) then
                        itemp(j1)=isurv
                     else
                        itemp(j1)=nbr
                     endif
                  endif
               enddo
               i1=itemp(1)
               i2=itemp(2)
               i3=itemp(3)
               i4=itemp(4)
               area(1)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
               area(2)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
               area(3)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
               vol=one6th*(area(1)*(xic(i2)-xic(i1))+
     &         area(2)*(yic(i2)-yic(i1))+area(3)*(zic(i2)
     &         -zic(i1)))
               if (vol.le.vtol) then
                  write(logmess,'(a,i9,a,i9,a)')
     &            'Try ',isurv,'<--',iremov,
     &            ':  FAIL : would invert element.'
                  call writloga('default',0,logmess,0,ierrw)
                  goto 10
               endif
 50         continue
         elseif (nsdtopo.eq.2) then

c....  Compute (angle-weighted) synthetic normal and compute if
c....  there are any old triangles with orientations negative
c....  with respect to this normal.

            call synthnormal(iparremov,nelts,ielts,iparent,itet,
     &         itetoff,xic,yic,zic,epsilonl,synthx,synthy,synthz
     &         ,lgoreverse)

C....  If all old triangle point in same direction as weighted
C....  normal check normal of new triangles
            if(.not.lgoreverse) then
               do j=1,nelts
                  ielt=ielts(j)
                  ityp=itettyp(ielt)
                  do j1=1,nelmnen(ityp)
                     nbr=iparent(itet(j1+itetoff(ielt)))
                     if (nbr.eq.iparsurv) then
                        goto 55
                     else
                        if (nbr.eq.iparremov) then
                           itemp(j1)=isurv
                        else
                           itemp(j1)=nbr
                        endif
                     endif
                  enddo
                  i1=itemp(1)
                  i2=itemp(2)
                  i3=itemp(3)
                  ax=crosx(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  ay=crosy(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  az=crosz(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  dot=ax*synthx+ay*synthy+az*synthz
                  if(abs(dot).lt.epsilona) then
                     write(logmess,'(a,i9,a,i9,a)')
     &               'Try ',isurv,'<--',iremov,
     &               ':  FAIL : would invert element.'
                     call writloga('default',0,logmess,0,ierrw)
                    go to 10
                  endif
 55               continue
               enddo
           endif
        endif
c.... Tests have been passed, so try to perform merge.
c.... Loop through tets and correct ITET array with mapping
c.... IREMOV-->ISURV, or dud tet if ISURV already in ITET
c.... array.
 
         iseedtet=0
         do 60 j=1,nelts
            ielt=ielts(j)
            ityp=itettyp(ielt)
 
            do j1=1,nelmnen(ityp)
               nbr=itet(j1+itetoff(ielt))
               if (iparent(nbr).eq.iparsurv) then
 
c.... This tet will be dudded.  Pass on jtet info from this tet
c.... to surviving neighbors.
 
                  jsurv=jtet(j1+jtetoff(ielt))
                  if (jsurv.gt.mbndry) then
                     jtsurv=1+(jsurv-mbndry-1)/nef_cmo
                     jfsurv=jsurv-mbndry-nef_cmo*(jtsurv-1)
                     iseedtet=jtsurv
                  elseif (jsurv.lt.mbndry) then
                     jtsurv=1+(jsurv-1)/nef_cmo
                     jfsurv=jsurv-nef_cmo*(jtsurv-1)
                     iseedtet=jtsurv
                  endif
 
                  jremov=0
                  do j2=1,nelmnen(ityp)
                     if (iparent(itet(j2+itetoff(ielt))).eq.
     &                  iparremov) then
                        jremov=jtet(j2+jtetoff(ielt))
                     endif
                  enddo
                  if (jremov.eq.0) then
                     logmess='FATAL ERROR from MERGEPTS'//
     *                          ' no tets to remove'
                     call writloga('default',0,logmess,0,ierrw)
                     call termcode()
                  endif
                  if (jremov.gt.mbndry) then
                     jtremov=1+(jremov-mbndry-1)/nef_cmo
                     jfremov=jremov-mbndry-nef_cmo*(jtremov-1)
                     iseedtet=jtremov
                  elseif (jremov.lt.mbndry) then
                     jtremov=1+(jremov-1)/nef_cmo
                     jfremov=jremov-nef_cmo*(jtremov-1)
                     iseedtet=jtremov
                  endif
 
c.... Face opposite ISURV on dudded tet will now be identical to face
c.... opposite IREMOV on dudded tet.
 
                  if (jsurv.ne.mbndry) then
                     if (jremov.ne.mbndry) then
                        jtet(jfsurv+jtetoff(jtsurv))=
     &                     jfremov+nef_cmo*(jtremov-1)
                        if (itetclr(jtsurv).ne.itetclr(jtremov)) then
                           jtet(jfsurv+jtetoff(jtsurv))=
     &                        jtet(jfsurv+jtetoff(jtsurv))+mbndry
                        endif
                     else
                        jtet(jfsurv+jtetoff(jtsurv))=mbndry
                     endif
                  endif
 
                  if (jremov.ne.mbndry) then
                     if (jsurv.ne.mbndry) then
                        jtet(jfremov+jtetoff(jtremov))=
     &                     jfsurv+nef_cmo*(jtsurv-1)
                        if (itetclr(jtsurv).ne.itetclr(jtremov)) then
                           jtet(jfremov+jtetoff(jtremov))=
     &                        jtet(jfremov+jtetoff(jtremov))+mbndry
                        endif
                     else
                        jtet(jfremov+jtetoff(jtremov))=mbndry
                     endif
                  endif
 
c.... Signify that this tet is 'dudded'.
 
                  itet(1+itetoff(ielt))=-itet(1+itetoff(ielt))
 
                  goto 60
 
               else
 
                  if (iparent(nbr).eq.iparremov) then
                     itemp(j1)=isurv
                  else
                     itemp(j1)=nbr
                  endif
 
               endif
            enddo
 
            iseedtet=ielt
            do j1=1,nelmnen(ityp)
               itet(j1+itetoff(ielt))=itemp(j1)
            enddo
 
 60      continue
 
c.... Set ITP of IREMOV to IFITPMRG.
 
         itp1(iremov)=ifitpmrg
         if (isn1(iremov).ne.0) then
            nextnod=isn1(iremov)
            do j=1,10000
               itp1(nextnod)=ifitpmrg
               nextnod=isn1(nextnod)
               if (nextnod.eq.iremov) goto 100
            enddo
            write(logmess,'(a,i9,a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FAIL : infinite parent/child chain.'
            call writloga('default',0,logmess,0,ierrw)
            call termcode()
         endif
 
 100     continue
 
c.... Check for the possible case that collapsed tets are not
c.... adjacent to any other tets.
 
         if (iseedtet.eq.0) then
 
c.... Check if any existing tet in the mesh is connected to ISURV.
c.... If that is so, we have a 'weird' mesh where the topology
c.... around a point cannot be fully recovered using a 'seed tet'.
c.... (This kind of topology is "jtet-disconnected".)  In this
c.... case we stop the code---although this case could be repaired
c.... with extra coding.
 
            iseedtet=(iseed(iparsurv)-1)/maxnen+1
            if (itet(1+itetoff(iseedtet)).gt.0) then
               write(logmess,'(a,i9,a,i9,a,i9,a)')
     &            'Try ',isurv,'<--',iremov,
     &            ':  FATAL ERROR:',isurv
     &            ,' part of more than 1 component.'
               call writloga('default',0,logmess,0,ierrw)
               call termcode()
            endif
            ipar=iparsurv
            do j=1,nelements
               i1=itet(1+itetoff(j))
               if (i1.gt.0) then
                  do k=1,nelmnen(itettyp(j))
                     i1=iparent(itet(k+itetoff(j)))
                     if (i1.eq.ipar) then
                        write(logmess,'(a,i9,a,i9,a,i9,a)')
     &                  'Try ',isurv,'<--',iremov,':  FATAL ERROR : ',
     &                  isurv,' part of more than 1 component.'
                        call writloga('default',0,logmess,0,ierrw)
                        call termcode()
                     endif
                  enddo
               endif
            enddo
 
c.... ISURV is an isolated node, so set ITP of ISURV to IFITPMRG.
 
            itp1(isurv)=ifitpmrg
            if (isn1(isurv).ne.0) then
               nextnod=isn1(isurv)
               do j=1,10000
                  itp1(nextnod)=ifitpmrg
                  nextnod=isn1(nextnod)
                  if (nextnod.eq.isurv) goto 110
               enddo
               write(logmess,'(a,i9,a,i9,a,i9,a)')
     &            'Try ',isurv,'<--',iremov,
     &            ':  FATAL ERROR : infinite parent/child chain.'
               call writloga('default',0,logmess,0,ierrw)
               call termcode()
            endif
            goto 110
         endif
 
c.... If the surviving node is a parent/child cluster, inspect
c.... and possibly correct this cluster.
 
         if (itp1(iparsurv).eq.ifitpcup) then
            call correctpc(iparent,isurv,iremov,iparsurv,iparremov,
     &         itettyp,iseedtet,itp1,itet,itetoff,jtet,jtetoff,nelts,
     &         ipielts,nef_cmo,mbndry,imt1,isn1,itetclr,lmoved)
         elseif (itp1(iparremov).eq.ifitpcup) then
            write(logmess,'(a,i9,a,i9,a,i9,a)')
     &         'Try ',isurv,'<--',iremov,
     &         ':  FATAL ERROR : merging parent-child point ' //
     &          'into regular point.'
            call writloga('default',0,logmess,0,ierrw)
            call termcode()
         endif
 
c.... Distribute new tet info to ISEED
 
         do j=1,nelts
            ielt=ielts(j)
            ityp=itettyp(ielt)
            nod1=itet(1+itetoff(ielt))
            if (nod1.gt.0) then
               do k=1,nelmnen(ityp)
                  nod=itet(k+itetoff(ielt))
                  iseed(iparent(nod))=(ielt-1)*maxnen+k
               enddo
            endif
         enddo
 
 110     continue
 
         if (itp1(iparsurv).eq.ifitpcup) then
 
            if (idebug.ge.1) then
               write(logmess,'(a,i9,a,i9,a,i9,a)')
     &            'Interface merge ',isurv,'  <--',iremov,':  DONE.'
               call writloga('default',0,logmess,0,ierrw)
            endif
 
         elseif (itp1(iparsurv).eq.ifitpmrg) then
 
            write(logmess,'(a,i9,a,i9,a,i9,a)')
     &         'Interface merge ',isurv,'  <--',iremov,
     &         ':  DONE (interface removed).'
            call writloga('default',0,logmess,0,ierrw)
 
         else
 
            if (idebug.ge.1) then
               write(logmess,'(a,i9,a,i9,a,i9,a)')
     &            'Regular merge ',isurv,'  <--',iremov,':  DONE.'
               call writloga('default',0,logmess,0,ierrw)
            endif
 
         endif
 
 10   continue
 
c.... set jtet to mbndry for all deleted elements

      do i=1,nelements
         ityp=itettyp(i)
         if (itet(1+itetoff(i)).lt.0) then
            do j=1,nelmnef(ityp)
               jtet(j+jtetoff(i))=mbndry
            enddo
         endif
      enddo

c.... Compress tet list
 
      call dotaskx3d('rmpoint/element;finish',ierr)
c$$$      call dotaskx3d('rmpoint/compress',ierr)
      call dotaskx3d('geniee;finish',ierr)
c$$$      call dotaskx3d('tettest ; finish', ierr)
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
 
      return
      end
 
