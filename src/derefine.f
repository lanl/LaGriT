*dk,derefine
      subroutine derefine(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Mark tets and triangles that need to be derefined according to the
C        criterion given on the input line.
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
C        $Log:   /pvcs.config/t3d/src/derefine.f_a  $
CPVCS    
CPVCS       Rev 1.5   Thu Apr 06 08:22:42 2000   dcg
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.4   22 Mar 2000 08:47:50   dcg
CPVCS    use local_epsilon in place of epsilon
CPVCS
CPVCS       Rev 1.3   Fri Jan 22 11:40:04 1999   dcg
CPVCS    fix type mmaxmat mmatmax
CPVCS    remove unused declarations
CPVCS
CPVCS       Rev 1.2   Fri Sep 19 09:09:28 1997   dcg
CPVCS    refresh needed cmo pointers
CPVCS
CPVCS       Rev 1.1   Thu Jun 19 11:39:32 1997   kmb
CPVCS    Streamlined command process.
CPVCS
CPVCS       Rev 1.0   Tue Jun 17 08:07:14 1997   kmb
CPVCS    Initial revision.
C
C     COMMAND
C        derefine/derefine_criterion/field/pointtype,pointtype/
C               refine_type/ifirst,ilast,istride/xrefine
C
C        derefine criterion =
C                  junction (not enabled)
C                  minsize
C                  delta (not enabled)
C                  lambda (not enabled)
C                  lambade (not enabled)
C                  ratioxyz (not enabled)
C                  ratioxy (not enabled)
C                  ratioxz (not enabled)
C                  ratioyz (not enabled)
C                  merge/point1,point2 - merge point1 to point2
C
C        field (not enabled)
C
C        pointtype - allowed itp1 value for two merged nodes.
C                    Example: if 10,0 only pairs of nodes with these itp1
C                    values will be merged with node(10) merged to node(0)
C
C        refine_type =
C                   edge - length of edge is criterion
C                   face - area of face is criterion
C                   volume - volume of element is criterion
C                   pinchedge - length of edge is criterion but
C                     allows merging across a thin layer
C                   aspect - aspect ratio is criterion
C
C        ifirst,ilast,istride - psets are also acceptable
C
C        xrefine - this is the value of the length (area,volume) that is
C                   used for a cutoff value
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
C
      include "local_element.h"
      include "chydro.h"
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
      character*20    ch_merge_1, ch_merge_2
      character*1024  cbuff
      character*20 cirtype
      integer cirtype_len
C
      integer ierror,it,i,i1,i2,iptyp1,iptyp2,j1,j2,jj1
      integer ipt1,ipt2,ipt3
      integer j,itest1,itest2,iface,ktest2
      integer ntets, npoints, mpno, mpnot,icount
      integer elface
      integer mmatmax,jtest1,jtest2,nchild1,nchild2,mat, child
      integer itp_pt1, itp_pt2
C
C#######################################################################
C
      pointer (ipisetwd, isetwd(1000000))
      pointer (ipimt1, imt1(1000000))
      pointer (ipitp1, itp1(1000000))
      pointer (ipint1, int1(1000000))
      pointer (ipicr1, icr1(1000000))
      pointer (ipisn1, isn1(1000000))
      pointer (ipicn1, icn1(1000000))
      pointer (ipxic, xic(1000000))
      pointer (ipyic, yic(1000000))
      pointer (ipzic, zic(1000000))
      pointer (ipxicvol, xicvol(10))
      pointer (ipyicvol, yicvol(10))
      pointer (ipzicvol, zicvol(10))
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(4*1000000), jtet1(4*1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipielmedge,ielmedge)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000),
     *        ielmedge(1000000)
C
      real*8  xrefine,xdist,ydist,zdist,dist,amindist,amaxdist
C
      pointer (ipmpary, mpary(1000000))
      pointer (ipmparyt, mparyt(1000000))
      pointer (ipmmat1, mmat1(200))
      pointer (ipmmat2, mmat2(200))
      pointer (ipichange, ichange(1000000))
C
C
      character*32 ich1,ich2,ich3
      character*32  cmo
      character*32 coption
      integer coption_len
      character*32 isubname
C
C ######################################################################
C
      isubname='derefine'
c
      ierror = 0
c  ************************************************************
 
c
      ioption=0
      coption=cmsgin(2)
      coption_len=icharlnf(coption)
c      OPtions 1 and 3 to 13 are not currently enabled
      if(coption(1:coption_len).eq.'minsize') ioption=1
      if(coption(1:coption_len).eq.'junction') ioption=2
      if(coption(1:coption_len).eq.'delta') ioption=3
      if(coption(1:coption_len).eq.'lambda') ioption=4
      if(coption(1:coption_len).eq.'lambdade') ioption=5
      if(coption(1:coption_len).eq.'ratioxyz') ioption=10
      if(coption(1:coption_len).eq.'ratioxy') ioption=11
      if(coption(1:coption_len).eq.'ratioxz') ioption=12
      if(coption(1:coption_len).eq.'ratioyz') ioption=13
      if(coption(1:coption_len).eq.'merge') ioption=14
      if(ioption.eq.0) then
         write(logmess,'(a,a)') 'Illegal refine criteria: ',
     *                          coption(1:coption_len)
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      if((ioption.gt.1).and.(ioption.lt.14)) then
         write(logmess,'(a,a)') 'This criterion not enabled: ',
     *                          coption(1:coption_len)
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)') 'DEREFINE found bad mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,icscode)
 
      if (ioption.ne.14) then
 
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
 
      call mmgetblk('xicvol' ,isubname,ipxicvol,10,2,icscode)
      call mmgetblk('yicvol' ,isubname,ipyicvol,10,2,icscode)
      call mmgetblk('zicvol' ,isubname,ipzicvol,10,2,icscode)
 
      length=npoints
      call mmgetblk('mpary' ,isubname,ipmpary,length,1,icscode)
      length=2*npoints
      call mmgetblk('mparyt' ,isubname,ipmparyt,length,1,icscode)
      length=200
      call mmgetblk('mmat1' ,isubname,ipmmat1,length,1,icscode)
      call mmgetblk('mmat2' ,isubname,ipmmat2,length,1,icscode)
      ich1=' '
      ich2=' '
      ich3=' '
      ipt1=0
      ipt2=0
      ipt3=0
      mpno=0
      if(msgtype(7).eq.1) then
         ipt1=imsgin(7)
         ipt2=imsgin(8)
         ipt3=imsgin(9)
C
         call cmo_get_info('ipointi',cmo,
     *                   ipointi,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('ipointj',cmo,
     *                   ipointj,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
         if(ipt1.eq.0) ipt1=ipointi
         if(ipt2.eq.0) ipt2=ipointj
         if(ipt3.eq.0) ipt3=1
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      else
         ich1=cmsgin(7)
         ich2=cmsgin(8)
         ich3=cmsgin(9)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      endif
 
C     set the allowed point type
       iptyp1=imsgin(4)
       iptyp2=imsgin(5)
C
C     set irtype for refine type if not merge
 
c     irtype=0
      cirtype=cmsgin(6)
      cirtype_len=icharlnf(cirtype)
      if(cirtype(1:cirtype_len).eq.'edge')irtype=1
      if(cirtype(1:cirtype_len).eq.'face')irtype=2
      if(cirtype(1:cirtype_len).eq.'volume')irtype=3
      if(cirtype(1:cirtype_len).eq.'pinchedge')irtype=4
      if(cirtype(1:cirtype_len).eq.'aspect')irtype=5
      if(irtype.eq.0) then
       write(logmess,'(a)') 'The refine type is not defined.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
C
      xrefine=xmsgin(10)
      endif
 
 
C
      length=npoints
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,icscode)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('edges_per_element',cmo,nee,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('int1',cmo,ipint1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('icn1',cmo,ipicn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('ielmedge',cmo,ipielmedge,
     *                  ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
C
C     ************************************************************
 
C
C     now go through the criteria
           if(ioption.eq.1) then
 
C     Look at all the edges
             if((irtype.eq.1).or.(irtype.eq.4)) then
             mpnot=2*mpno
             do i=1,mpnot
                mparyt(i)=0
             enddo
 
 
             icount=0
560          continue
               do it=1,ntets
                  iface=nelmnee(itettyp(it))
                do i=1,iface
                  itest1=0
                  itest2=0
                  i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                  i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
 
                    xdist= xic(i1)-xic(i2)
                    ydist= yic(i1)-yic(i2)
                    zdist= zic(i1)-zic(i2)
                  dist=sqrt(xdist**2 + ydist**2 + zdist**2)
 
                    if(dist.le.xrefine) then
 
 
c      test to see if i1,i2 in the pointset
                   do j=1,mpno
                     if(i1.eq.mpary(j)) then
                       itest1=1
                     endif
                     if(i2.eq.mpary(j))  then
                       itest1=1
                     endif
 
c           check to see if the node pair has been merged before
                     if(mparyt(2*j-1).eq.i1) then
                       itest2=itest2+1
                       if(mparyt(2*j).eq.i2)    itest2=itest2+1
                     endif
                     if(mparyt(2*j-1).eq.i2) then
                       itest2=itest2+1
                       if(mparyt(2*j).eq.i1)    itest2=itest2+1
                     endif
 
                   enddo
 
c      check to see if on same interface boundary (if itp1 both equal2)
c      the check is to see if 1) each parent node has the same number of
c      children and 2)if there is the same number of each material type
c      present for each parent
 
c
                   itp_pt1=itp1(i1)
                   itp_pt2=itp1(i2)
                   if(itp_pt1.eq.41) itp_pt1=2
                   if(itp_pt2.eq.41) itp_pt2=2
c                  if(irtype.ne.4) then
                     if((iptyp1.eq.2).and.(iptyp2.eq.2))then
                        if((itp_pt1.eq.2).and.(itp_pt2.eq.2)) then
                         if((isn1(i1).ne.0).and.(isn1(i2).ne.0)) then
 
c                        initialize arrays
                         mmatmax=0
                         jtest1=0
                         jtest2=0
                         nchild1=0
                         nchild2=0
                          do ij=1,200
                            mmat1(ij)=0
                            mmat2(ij)=0
                          enddo
 
 
                           child=i1
                          do jj=2,100
                            child=isn1(child)
                            if(itp1(child).eq.41) jtest1=jtest1+1
                            if(jtest1.eq.2) goto 540
                            if(jtest1.eq.1) then
                              nchild1=nchild1+1
                              mat=imt1(child)
                              mmatmax=max(mmatmax,mat)
                              mmat1(mat)=mmat1(mat)+1
                            endif
                          enddo
540                       continue
                          jj1=mmatmax
 
                           child=i2
                          do jj=2,100
                            child=isn1(child)
                            if(itp1(child).eq.41) jtest2=jtest2+1
				    if(jtest2.eq.2) goto 545
				    if(jtest2.eq.1) then
                              nchild2=nchild2+1
                              mat=imt1(child)
                              mmatmax=max(mmatmax,mat)
                              mmat2(mat)=mmat2(mat)+1
                            endif
                          enddo
545                       continue
 
 
c         if merge is inappropriate goto 550
                           if(nchild1.ne.nchild2) goto 550
                           do ij=1,mmatmax
                             if(mmat1(ij).ne.mmat2(ij)) goto 550
                           enddo
 
                          endif
                        endif
                     endif
c                   endif
c
 
                    if(irtype.ne.4) then
                      if(itp_pt1.eq.iptyp1)then
                           if(itp_pt2.eq.iptyp2) then
                           write(ch_merge_1,571) i1
                           write(ch_merge_2,571) i2
                           else
                           goto 550
                           endif
                      elseif(itp_pt2.eq.iptyp1) then
                           if(itp_pt1.eq.iptyp2) then
                           write(ch_merge_2,571) i1
                           write(ch_merge_1,571) i2
                           else
                           goto 550
                           endif
                      else
                      goto 550
                      endif
                   else
c     if pinchedge, let maximum material node merge to the second node
                   if(itp_pt1.eq.2)then
                     if(itp_pt2.eq.2) then
                         if(jj1.ge.mmatmax) then
                           write(ch_merge_1,571) i1
                           write(ch_merge_2,571) i2
                         else
                           write(ch_merge_1,571) i2
                           write(ch_merge_2,571) i1
                         endif
                     else
                     goto 550
                     endif
                    else
                    goto 550
                   endif
                   endif
 
 
 
                  len1=icharlnf(ch_merge_1)
                  len2=icharlnf(ch_merge_2)
                  cbuff = 'merge/'
     *             // ch_merge_1(1:len1)
     *             // ','
     *             // ch_merge_2(1:len2)
     *             // ';  finish'
  571              format(i10)
 
 
         if(itest2.lt.2) then
         if(itest1.eq.1) then
 
c             save the nodal pair for future reference
              icount=1+icount
              mparyt(2*icount)=i1
              mparyt(2*icount-1)=i2
 
c            Attempt merge of nodes on shortest edge
 
             call dotaskx3d(cbuff,ierror)
         write(logmess,551)icount
551       format(1x,i9," merges are attempted in derefine.")
         call writloga('default',0,logmess,3,ierrw)
 
 
C     update element info after merge
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
 
             goto 560
          endif
          endif
 
c            end of distance check endif
550          continue
             endif
 
          enddo
          enddo
 
          goto 9999
 
C###############################################
             elseif(irtype.eq.2) then
 
c      if irtype=2 , we look at minimum faces
 
C        ...............................................................
              do it=1,ntets
                  iface=nelmnee(itettyp(it))
c
                 do ii= 1, iface
                        elface=ielmface0(ii,itettyp(it))
                     do j = 1,elface
                         k = itet1(itetoff(it) +
     1                      ielmface1(j,ii,itettyp(it)))
                         xicvol(j) = xic(k)
                         yicvol(j) = yic(k)
                         zicvol(j) = zic(k)
                     enddo
c
c
c      Determine itettyp value for FACE to pass to
c          volume/area calculation
c      The itettyp of the face is the same as the
c          number of nodes on the face
 
c         1 = point
c         2 = line
c         3 = tri
c         4 = quad
c
c
              call volume_element(elface,
     *                             xicvol,yicvol,zicvol,
     *                             xtetvol)
              if(abs(xtetvol) .lt. xrefine)then
                do i=1,iface
                  itest1=0
                  itest2=0
                  i1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                  i2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
                enddo
              endif
c   etcetera
c       End of face do loop
                  enddo
c       End of element do loop
                  enddo
c
c
C        END face_area and face_ratio
          goto 9999
 
C#########################################################################
C       check aspect ratios and merge accordingly
 
        elseif((irtype.eq.5).or.(irtype.eq.3)) then
        length=ntets
        call mmgetblk('ichange' ,isubname,ipichange,length,1,icscode)
 
C       check aspect ratios of all elements
        if(irtype.eq.5) then
        call elem_check(1,ichange,xrefine,ierror)
        if(ierror.ne.0) then
         write(logmess,'(a)') 'DEREFINE found aspect problems'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
        endif
 
C       check volumes of all elements
        elseif(irtype.eq.3) then
        call elem_check(2,ichange,xrefine,ierror)
        if(ierror.ne.0) then
         write(logmess,'(a)') 'DEREFINE found volume problems'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
        endif
        endif
 
 
             mpnot=2*mpno
             do i=1,mpnot
                mparyt(i)=0
             enddo
 
 
             icount=0
660          continue
               do it=1,ntets
                  if(ichange(it).ne.1) goto 670
                  amindist=1000000.
                  amaxdist=0.
                  iface=nelmnee(itettyp(it))
                do i=1,iface
                  itest1=0
                  itest2=0
                  j1=itet1(itetoff(it)+ielmedge1(1,i,itettyp(it)))
                  j2=itet1(itetoff(it)+ielmedge1(2,i,itettyp(it)))
 
                    xdist= xic(j1)-xic(j2)
                    ydist= yic(j1)-yic(j2)
                    zdist= zic(j1)-zic(j2)
                  dist=sqrt(xdist**2 + ydist**2 + zdist**2)
                  amaxdist=max(amaxdist,dist)
 
c           check to see if the node pair has been merged before
                   ktest2=0
                     if(mparyt(2*j-1).eq.j1) then
                       ktest2=ktest2+1
                       if(mparyt(2*j).eq.j2)    ktest2=ktest2+1
                     endif
                     if(mparyt(2*j-1).eq.j2) then
                       ktest2=ktest2+1
                       if(mparyt(2*j).eq.j1)    ktest2=ktest2+1
                     endif
 
                     if(ktest2.lt.2) then
                     if(dist.ne.amaxdist) then
                      amindist=min(amindist,dist)
                      if(amindist.eq.dist) then
 
                       i1=j1
                       i2=j2
                      endif
                     endif
                     endif
 
                enddo
 
 
 
c      test to see if i1,i2 in the pointset
                   do j=1,mpno
                     if(i1.eq.mpary(j)) then
                       itest1=1
                     endif
                     if(i2.eq.mpary(j))  then
                       itest1=1
                    endif
 
c           check to see if the node pair has been merged before
                     if(mparyt(2*j-1).eq.i1) then
                       itest2=itest2+1
                       if(mparyt(2*j).eq.i2)    itest2=itest2+1
                     endif
                     if(mparyt(2*j-1).eq.i2) then
                       itest2=itest2+1
                       if(mparyt(2*j).eq.i1)    itest2=itest2+1
                     endif
 
                   enddo
                 if(itest1.ne.1) goto 670
                 if(itest2.eq.2) goto 670
 
c      check to see if on same interface boundary (if itp1 both equal2)
c      the check is to see if 1) each parent node has the same number of
c      children and 2)if there is the same number of each material type
c      present for each parent
 
c
                   itp_pt1=itp1(i1)
                   itp_pt2=itp1(i2)
                   if(itp_pt1.eq.41) itp_pt1=2
                   if(itp_pt2.eq.41) itp_pt2=2
                     if((iptyp1.eq.2).and.(iptyp2.eq.2))then
                        if((itp_pt1.eq.2).and.(itp_pt2.eq.2)) then
                         if((isn1(i1).ne.0).and.(isn1(i2).ne.0)) then
 
c                        initialize arrays
                         mmatmax=0
                         jtest1=0
                         jtest2=0
                         nchild1=0
                         nchild2=0
                          do ij=1,200
                            mmat1(ij)=0
                            mmat2(ij)=0
                          enddo
 
 
                           child=i1
                          do jj=2,100
                            child=isn1(child)
                            if(itp1(child).eq.41) jtest1=jtest1+1
                            if(jtest1.eq.2) goto 640
                            if(jtest1.eq.1) then
                              nchild1=nchild1+1
                              mat=imt1(child)
                              mmatmax=max(mmatmax,mat)
                              mmat1(mat)=mmat1(mat)+1
                            endif
                          enddo
640                       continue
 
                           child=i2
                          do jj=2,100
                            child=isn1(child)
                            if(itp1(child).eq.41) jtest2=jtest2+1
                            if(jtest2.eq.2) goto 650
                            if(jtest2.eq.1) then
                              nchild2=nchild2+1
                              mat=imt1(child)
                              mmatmax=max(mmatmax,mat)
                              mmat2(mat)=mmat2(mat)+1
                            endif
                          enddo
650                       continue
 
 
c         if merge is inappropriate goto 670
                           if(nchild1.ne.nchild2) goto 670
                           do ij=1,mmatmax
                             if(mmat1(ij).ne.mmat2(ij)) goto 670
                           enddo
 
                          endif
                        endif
                     endif
 
c
                      if(itp_pt1.eq.iptyp1)then
                           if(itp_pt2.eq.iptyp2) then
                           write(ch_merge_1,671) i1
                           write(ch_merge_2,671) i2
                           else
                           goto 670
                           endif
                      elseif(itp_pt2.eq.iptyp1) then
                           if(itp_pt1.eq.iptyp2) then
                           write(ch_merge_2,671) i1
                           write(ch_merge_1,671) i2
                           else
                           goto 670
                           endif
                      else
                      goto 670
                      endif
                  len1=icharlnf(ch_merge_1)
                  len2=icharlnf(ch_merge_2)
                  cbuff = 'merge/'
     *             // ch_merge_1(1:len1)
     *             // ','
     *             // ch_merge_2(1:len2)
     *             // ';  finish'
  671              format(i10)
 
 
         if(itest2.lt.2) then
         if(itest1.eq.1) then
 
 
c             save the nodal pair for future reference
              icount=1+icount
              mparyt(2*icount)=i1
              mparyt(2*icount-1)=i2
 
c            Attempt merge of nodes on shortest edge
 
             call dotaskx3d(cbuff,ierror)
         write(logmess,651)icount
651       format(1x,i9," merges are attempted in derefine.")
         call writloga('default',0,logmess,3,ierrw)
 
 
C     update element info after merge
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
 
          goto 660
          endif
 
 
           endif
670          continue
 
          enddo
 
 
        call mmrelblk('ichange' ,isubname,iadr,icscode)
 
 
c      end irtype options for ioption.eq.1
             endif
 
 
 
 
 
C ###################################
C     if merge is the criterion, do it
      elseif (ioption.eq.14)then
                ch_merge_1=cmsgin(3)
                ch_merge_2=cmsgin(4)
 
 
                len1=icharlnf(ch_merge_1)
                len2=icharlnf(ch_merge_2)
                cbuff = 'merge/'
     *             // ch_merge_1(1:len1)
     *             // ','
     *             // ch_merge_2(1:len2)
 
             write(6,778)cbuff
  778        format(a72)
c            Attempt merge of nodes
             call dotaskx3d(cbuff,ierror)
 
c     end all ioptions
      endif
 
C
            goto 9999
C
 9999 continue
      call mmrelprt(isubname,icscode)
C
      return
      end
 
 
 
*dk,elem_check
      subroutine elem_check(ityp2,ichange,criterion,ierr1)
C
C
C#######################################################
 
C  This subroutine checks aspect ratios and volumes
C
C#######################################################
 
      implicit none
      include 'local_element.h'
      integer ierr1,ilen,ityp,ityp2
      integer ier,i,npoints,nerr,nhalf,n1,n5,n10,n20,
     $        n50,n100,nneg
      integer idebug, iwrite, nwrite, ichange(1000000)
      pointer(ipxic,xic(1000000))
      pointer(ipyic,yic(1000000))
      pointer(ipzic,zic(1000000))
      pointer(ipitetoff,itetoff(1000000))
      pointer(ipitettyp,itettyp(1000000))
      pointer(ipitet,itet1(1000000))
      real*8  criterion
      character*32 cmo, isubname
      integer i1,i2,i3,i4,it,i5,i6,i7,i8
      real*8 xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4
      real*8 xl5,yl5,zl5,xl6,yl6,zl6,xl7,yl7,zl7,xl8,yl8,zl8
      real*8 xb,yb,zb,xc,yc,zc,xd,yd,zd,xn,yn,zn,x2,y2,z2,q,
     * xa,ya,za,dvor,qvor2,rcir,xe,ye,ze
      real*8 ac1,bc1,cc1,dc1,ac2,bc2,cc2,dc2,ac3,bc3,cc3,dc3,
     *   ac4,bc4,cc4,dc4,dn1,dn2,dn3,dn4
      real*8 a11,a12,a13,d1,a21,a22,a23,a31,a32,a33,d3,d2
      real*8 qdet,rx,ry,rz,rinsc,srat
      real*8 xic,yic,zic,sratmin,sratmax,smdiag,lardiag,dist1,dist2
      real*8 ax4,ay4,az4,farea
      real*8 ds1,ds2,ds3
      integer itetoff,itet1,itettyp,nelements,length
      integer ielmtyp
      real*8 a,d,dist,xicv(8),yicv(8),zicv(8)
      logical ifaspect,ifvolume
      real*8 x1,x3,y1,y3,z1,z3,local_epsilon,volumelt
      dimension dist(4)
C     MACROS.
C
      a(x1,x2,x3,y1,y2,y3) = x1*(y2-y3) - y1*(x2-x3) + x2*y3 - y2*x3
      d(x1,x2,x3,y1,y2,y3,z1,z2,z3) = y1*(x2*z3 - z2*x3)
     &         - x1*(y2*z3 - z2*y3) - z1*(x2*y3 - y2*x3)
 
      isubname='elem_check'
      ierr1=0
      iwrite = 0
      nwrite = 20
 
      call cmo_get_name(cmo,ier)
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ier)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ier)
      length=nelements*nelmnef(ifelmtri)
 
      call get_info_i('idebug',cmo,idebug,ilen,ityp,ier)
      if(idebug.gt.1) nwrite = length
      if(idebug.eq.1) nwrite = 20
      if(idebug.eq.0) nwrite = 1
 
C  put aspect ratios or volumes into an attribute array
 
C    Initialize ichange array
      do 1090 i=1,nelements
            ichange(i)=0
1090  continue
 
 
C  get mesh object name and number of nodes
      ifaspect = .false.
      ifvolume = .false.
      if(ityp2.eq.1) ifaspect=.true.
      if(ityp2.eq.2) ifvolume=.true.
      if (.not.ifaspect) goto 1000
 
C
C              THIS IS ASPECT_RATIO/TET
C
C              Computes the ratio of the radius of the circumsphere to
C                  the radius of the inscribed sphere of a tetrahedron.
C                  Ratio is multiplied by 3 so that a value of one
C                  indicates a regular tetrahedron. If the ratio is
C                  smaller than xrefine(1) the tet and edges are taged.
C                  The ratio should never be greater than one.
C
      call get_epsilon('epsilonl',local_epsilon)
      local_epsilon=local_epsilon**3
 
      nerr = 0
      nneg = 0
      nhalf = 0
      n1 = 0
      n5 = 0
      n10 = 0
      n20 = 0
      n50 = 0
      n100 = 0
      sratmin = 100
      sratmax = -100
      do it=1,nelements
            if (itettyp(it) .eq. ifelmtet) then
                   i1=itet1(itetoff(it)+1)
                   i2=itet1(itetoff(it)+2)
                   i3=itet1(itetoff(it)+3)
                   i4=itet1(itetoff(it)+4)
                   xl1 = xic(i1)
                   yl1 = yic(i1)
                   zl1 = zic(i1)
                   xl2 = xic(i2)
                   yl2 = yic(i2)
                   zl2 = zic(i2)
                   xl3 = xic(i3)
                   yl3 = yic(i3)
                   zl3 = zic(i3)
                   xl4 = xic(i4)
                   yl4 = yic(i4)
                   zl4 = zic(i4)
C
C                  Calculation of "rcir", the radius of the
C                     circumscribed sphere of the tet.
C
                    xb = xl3 - xl2
                    yb = yl3 - yl2
                    zb = zl3 - zl2
                    xc = xl4 - xl2
                    yc = yl4 - yl2
                    zc = zl4 - zl2
                    xd = xl1 - xl2
                    yd = yl1 - yl2
                    zd = zl1 - zl2
                    xn =   yb*zc - yc*zb
                    yn = -(xb*zc - xc*zb)
                    zn =   xb*yc - xc*yb
                    x2 =   yn*zb - yb*zn
                    y2 = -(xn*zb - xb*zn)
                    z2 =   xn*yb - xb*yn
                    q = -0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                       (x2*xc+y2*yc+z2*zc+local_epsilon)
                    xa = q*x2 + 0.5*xb
                    ya = q*y2 + 0.5*yb
                    za = q*z2 + 0.5*zb
                    dvor = -0.5*(xd*xd + yd*yd + zd*zd)
                    qvor2 = -(xd*xa+yd*ya+zd*za+dvor)/
     *                       (xd*xn+yd*yn+zd*zn+1.0d-30)
 
                    rcir = sqrt( (qvor2*xn + xa)**2
     *                         + (qvor2*yn + ya)**2
     *                         + (qvor2*zn + za)**2 )
C
C                 Calculation of "rinsc", the radius of the inscribed
C                    sphere of the tet.
C
                   ac1 = a(yl2,yl4,yl3,zl2,zl4,zl3)
                   bc1 = a(zl2,zl4,zl3,xl2,xl4,xl3)
                   cc1 = a(xl2,xl4,xl3,yl2,yl4,yl3)
                   dc1 = d(xl2,xl4,xl3,yl2,yl4,yl3,zl2,zl4,zl3)
                   ac2 = a(yl1,yl4,yl2,zl1,zl4,zl2)
                   bc2 = a(zl1,zl4,zl2,xl1,xl4,xl2)
                   cc2 = a(xl1,xl4,xl2,yl1,yl4,yl2)
                   dc2 = d(xl1,xl4,xl2,yl1,yl4,yl2,zl1,zl4,zl2)
                   ac3 = a(yl1,yl2,yl3,zl1,zl2,zl3)
                   bc3 = a(zl1,zl2,zl3,xl1,xl2,xl3)
                   cc3 = a(xl1,xl2,xl3,yl1,yl2,yl3)
                   dc3 = d(xl1,xl2,xl3,yl1,yl2,yl3,zl1,zl2,zl3)
                   ac4 = a(yl1,yl3,yl4,zl1,zl3,zl4)
                   bc4 = a(zl1,zl3,zl4,xl1,xl3,xl4)
                   cc4 = a(xl1,xl3,xl4,yl1,yl3,yl4)
                   dc4 = d(xl1,xl3,xl4,yl1,yl3,yl4,zl1,zl3,zl4)
                   dn1 = sqrt(ac1**2 + bc1**2 + cc1**2)
                   dn2 = sqrt(ac2**2 + bc2**2 + cc2**2)
                   dn3 = sqrt(ac3**2 + bc3**2 + cc3**2)
                   dn4 = sqrt(ac4**2 + bc4**2 + cc4**2)
 
                   ac1 = ac1/dn1
                   bc1 = bc1/dn1
                   cc1 = cc1/dn1
                   dc1 = dc1/dn1
                   ac2 = ac2/dn2
                   bc2 = bc2/dn2
                   cc2 = cc2/dn2
                   dc2 = dc2/dn2
                   ac3 = ac3/dn3
                   bc3 = bc3/dn3
                   cc3 = cc3/dn3
                   dc3 = dc3/dn3
                   ac4 = ac4/dn4
                   bc4 = bc4/dn4
                   cc4 = cc4/dn4
                   dc4 = dc4/dn4
 
                   a11 = ac1 - ac2
                   a12 = bc1 - bc2
                   a13 = cc1 - cc2
                   d1  = dc2 - dc1
                   a21 = ac1 - ac3
                   a22 = bc1 - bc3
                   a23 = cc1 - cc3
                   d2  = dc3 - dc1
                   a31 = ac1 - ac4
                   a32 = bc1 - bc4
                   a33 = cc1 - cc4
                   d3  = dc4 - dc1
 
                   qdet = (a12*a23 - a13*a22)*a31
     *                  + (a13*a21 - a11*a23)*a32
     *                  + (a11*a22 - a12*a21)*a33
 
                   rx = ( (a22*a33 - a23*a32)*d1
     *                +   (a13*a32 - a12*a33)*d2
     *                +   (a12*a23 - a13*a22)*d3 )/qdet
 
                   ry = ( (a23*a31 - a21*a33)*d1
     *                +   (a11*a33 - a13*a31)*d2
     *                +   (a13*a21 - a11*a23)*d3 )/qdet
 
                   rz = ( (a21*a32 - a22*a31)*d1
     *                  + (a12*a31 - a11*a32)*d2
     *                  + (a11*a22 - a12*a21)*d3 )/qdet
 
                   rinsc =  ac1*rx + bc1*ry + cc1*rz + dc1
 
                   srat = dabs(3.d0*rinsc/rcir)
                   if(srat.le.criterion)  ichange(it)=1
 
 
C  Calculate for quads
        else if (itettyp(it) .eq. ifelmqud) then
                   i1=itet1(itetoff(it)+1)
                   i2=itet1(itetoff(it)+2)
                   i3=itet1(itetoff(it)+3)
                   i4=itet1(itetoff(it)+4)
                   xl1 = xic(i1)
                   yl1 = yic(i1)
                   zl1 = zic(i1)
                   xl2 = xic(i2)
                   yl2 = yic(i2)
                   zl2 = zic(i2)
                   xl3 = xic(i3)
                   yl3 = yic(i3)
                   zl3 = zic(i3)
                   xl4 = xic(i4)
                   yl4 = yic(i4)
                   zl4 = zic(i4)
C
C                  Calculation of "dist", the length of the
C                     diagonals of the quad.
C
                    xb = xl1 - xl3
                    yb = yl1 - yl3
                    zb = zl1 - zl3
                    xc = xl2 - xl4
                    yc = yl2 - yl4
                    zc = zl2 - zl4
                    dist1 = sqrt ( xb*xb + yb*yb + zb*zb )
                    dist2 = sqrt ( xc*xc + yc*yc + zc*zc )
         if (dist1 .lt. dist2) then
                    smdiag = dist1
                    lardiag = dist2
         else
                    lardiag = dist1
                    smdiag = dist2
         endif
                    srat = smdiag/lardiag
                    if(srat.le.criterion)  ichange(it)=1
 
 
C  Calculate for hexes
         else if (itettyp(it) .eq. ifelmhex) then
                   i1=itet1(itetoff(it)+1)
                   i2=itet1(itetoff(it)+2)
                   i3=itet1(itetoff(it)+3)
                   i4=itet1(itetoff(it)+4)
                   i5=itet1(itetoff(it)+5)
                   i6=itet1(itetoff(it)+6)
                   i7=itet1(itetoff(it)+7)
                   i8=itet1(itetoff(it)+8)
                   xl1 = xic(i1)
                   yl1 = yic(i1)
                   zl1 = zic(i1)
                   xl2 = xic(i2)
                   yl2 = yic(i2)
                   zl2 = zic(i2)
                   xl3 = xic(i3)
                   yl3 = yic(i3)
                   zl3 = zic(i3)
                   xl4 = xic(i4)
                   yl4 = yic(i4)
                   zl4 = zic(i4)
                   xl5 = xic(i5)
                   yl5 = yic(i5)
                   zl5 = zic(i5)
                   xl6 = xic(i6)
                   yl6 = yic(i6)
                   zl6 = zic(i6)
                   xl7 = xic(i7)
                   yl7 = yic(i7)
                   zl7 = zic(i7)
                   xl8 = xic(i8)
                   yl8 = yic(i8)
                   zl8 = zic(i8)
 
C
C                  Calculation of "dist", the length of the
C                     diagonals of the quad.
C
 
                    xb = xl1 - xl7
                    yb = yl1 - yl7
                    zb = zl1 - zl7
                    xc = xl2 - xl8
                    yc = yl2 - yl8
                    zc = zl2 - zl8
                    xd = xl3 - xl5
                    yd = yl3 - yl5
                    zd = zl3 - zl5
                    xe = xl4 - xl6
                    ye = yl4 - yl6
                    ze = zl4 - zl6
 
                    dist(1) = sqrt ( xb*xb + yb*yb + zb*zb )
                    dist(2) = sqrt ( xc*xc + yc*yc + zc*zc )
                    dist(3) = sqrt ( xd*xd + yd*yd + zd*zd )
                    dist(4) = sqrt ( xe*xe + ye*ye + ze*ze )
 
                    smdiag = dist(1)
                    lardiag = dist(1)
                do 21 i=2,4
                    if(dist(i) .ge. lardiag) lardiag = dist(i)
                    if(dist(i) .le. smdiag) smdiag = dist(i)
 21             continue
                    srat = smdiag/lardiag
                    if(srat.le.criterion) ichange(it)=1
 
 
C  Calculate for triangles
 
         else if (itettyp(it) .eq. ifelmtri) then
                    i1=itet1(itetoff(it)+1)
                    i2=itet1(itetoff(it)+2)
                    i3=itet1(itetoff(it)+3)
                    xl1 = xic(i1)
                    yl1 = yic(i1)
                    zl1 = zic(i1)
                    xl2 = xic(i2)
                    yl2 = yic(i2)
                    zl2 = zic(i2)
                    xl3 = xic(i3)
                    yl3 = yic(i3)
                    zl3 = zic(i3)
                    ax4 = (yl3 - yl1)*(zl2 - zl1)-(zl3-zl1)*(yl2-yl1)
                    ay4 = -((xl3 - xl1)*(zl2 - zl1)-(zl3 - zl1)*
     *                     (xl2 - xl1))
                    az4 = (xl3 - xl1)*(yl2 - yl1)-(yl3 - yl1)*
     *                    (xl2 - xl1)
C  radius of inscribed circle is 2*area of triangle/perimeter
C  radius of circumscribed circle is l1*l2*l3/4*area where l1,l2,l3 are edge
C  lengths
C
                     farea=.5*sqrt(ax4**2+ay4**2+az4**2)
                     ds1 = sqrt((xl3 - xl2)**2+(yl3 - yl2)**2+
     *                         (zl3 - zl2)**2)
                     ds2 = sqrt((xl1 - xl3)**2+(yl1 - yl3)**2+
     *                         (zl1 - zl3)**2)
                     ds3 = sqrt((xl2 - xl1)**2+(yl2 - yl1)**2 +
     *                         (zl2 - zl1)**2)
                  rinsc = 2.*farea/(ds1+ds2+ds3)
                  rcir= ds1*ds2*ds3/(4.*farea)
                  srat= 2.*rinsc/rcir
                  if(srat.le.criterion)  ichange(it)=1
             endif
 
          enddo
 
C   end of aspect ratio calculations
 
 
C   get element volume distribution if requested
C
 1000 if (.not.ifvolume) goto 2000
      do it=1,nelements
         ielmtyp=itettyp(it)
         do i=1,nelmnen(ielmtyp)
            i1 = itet1 (itetoff(it)+i)
            xicv(i)=xic(i1)
            yicv(i)=yic(i1)
            zicv(i)=zic(i1)
         enddo
         call volume_element(ielmtyp,xicv,yicv,zicv,volumelt)
         if(volumelt.le.criterion)  ichange(it)=1
      enddo
C
 2000 continue
 
 
      return
      end
