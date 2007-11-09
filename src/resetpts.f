      subroutine resetpts(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C ######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE RESETS NODE QUANTITIES DEPENDING ON THE
C     OPTION SPECIFIED
C
C     OPTIONS:
C
C      parent         resets itp and isn and itet
C                     loop through points looking for parent type
C                     change types of childen to dud
C                     change type of parent to max of child types
C                     fix up itet, replace references to child with parent
C
C      itp            resets itp
C                     set external boundary node type based on boundary faces
C                     for use when removing points or materials
C
C      color          resets itetclr
C                     set itetclr array to zero
C
C      fixitet        resets itet
C                     look for 2d meshes with reversed neighbor triangles
C                     (normals pointing wrong way)
C
C      cell_color     resets imt
C
C            imt_color_to_change
C                     loops through all elements 1 to ntets
C                     and resets chosen node imt to element color
C                     default - loop will go from 1 to ntets
C                     imt_color_to_change is negative, ntets to 1
C
C             no args or istart, iend, istride
C                     loops through all itetclr values 1 to maxclr
C                     and resets node imt to chosen element colors
C                     istart,istride,iend can be used to loop subset of colors
C                     see examples below
C
C      intrface       resets itp and jtet
C                     look at all faces - if more than one
C                     interior material on face make the nodes
C                     on the face interface nodes
C
c     EXAMPLES
C
C     for a grid with 4 itetclr colors:
c     resetpts/cell_color        will loop through colors from 1  to 4
c                                assigning node imt the element color
c     resetpts/cell_color/1,0,0  will loop through colors from 1  to 4
c     resetpts/cell_color/0,1,-1 will loop through colors from 4  to 1
c     resetpts/cell_color/1,2,1  will loop through colors from 1  to 2
C
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
C
C  temporary options are color and fixitet
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/resetpts.f_a  $
CPVCS    
CPVCS       Rev 1.19   04 Apr 2007 10:32:20   gable
CPVCS    Added return in the case of MO with zero nodes.
CPVCS    
CPVCS       Rev 1.18   10 Apr 2001 13:54:30   dcg
CPVCS    remove bad line
CPVCS
CPVCS       Rev 1.17   03 Apr 2000 18:35:48   gable
CPVCS    Changed some comments
CPVCS
CPVCS       Rev 1.16   Wed Mar 24 11:21:56 1999   dcg
CPVCS    change error flag on mmrelprt call - because
CPVCS    for some options no temporary memory is used
CPVCS
CPVCS       Rev 1.15   Wed Aug 19 12:16:58 1998   tam
CPVCS    added to cell_color option to reset imt from element colors
CPVCS    loop through all itetclr values 1 to maxclr, set imt from itetclr
CPVCS    istart,istride,iend can be used to loop subset of colors
CPVCS    also added comments and expanded information in header
CPVCS
CPVCS       Rev 1.14   Fri May 29 14:36:38 1998   dcg
CPVCS    for resetpts/itp skip nodes whose itp >= ifitpst3 = 20
CPVCS
CPVCS       Rev 1.13   Wed May 20 15:35:10 1998   dcg
CPVCS    add option 'interface'  look at all faces - if more than one
CPVCS    interior material on face make the nodes on the face interface nodes
CPVCS
CPVCS       Rev 1.12   Mon Mar 16 12:21:38 1998   gable
CPVCS    Added option resetpts / cell_color / node_color which
CPVCS    loops through all elements and resets node imt to element
CPVCS    color if the node is the user specified imt value.
CPVCS
CPVCS       Rev 1.11   Thu Mar 12 15:21:50 1998   kuprat
CPVCS    Fixed bug where merged and dudded points where reset
CPVCS    to interior points.
CPVCS
CPVCS       Rev 1.10   Tue Jul 15 10:46:58 1997   dcg
CPVCS    overwrite previous changes till fixed
CPVCS
CPVCS       Rev 1.8   Thu May 08 17:01:46 1997   dcg
CPVCS    add resetpts/color command to set itetclr array to zero
CPVCS    add resetpts/ifixitet command to look for 2d meshes
CPVCS    with reversed neighbor triangles (normals pointing
CPVCS    wrong way)
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:59:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Thu Mar 06 13:01:36 1997   kmb
CPVCS    Removed debugging printout
CPVCS
CPVCS       Rev 1.5   Thu Oct 03 13:02:20 1996   dcg
CPVCS    Reset the itp array for use when removing points or materials
CPVCS    changes added by Kathy Bowers(kmb)
CPVCS       Rev 1.4   Thu Jun 27 14:54:42 1996   het
CPVCS    Use unpackpc instead of the slow inline coding.
CPVCS
CPVCS       Rev 1.3   11/16/95 14:23:46   dcg
CPVCS    look for null parent/child lists
CPVCS
CPVCS       Rev 1.2   11/07/95 17:25:20   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   09/27/95 09:55:58   dcg
CPVCS    check for incomplete parent child loops
CPVCS
CPVCS       Rev 1.0   09/20/95 09:52:18   dcg
CPVCS    Initial revision.
C
C ######################################################################
      implicit none
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*32 cbuff
C
      integer ierror, ilen,itype,mbndry,length,icscode,ipnt,it,
     *  node1,jt,nf,ip1,ip2,jt1,jf,jp1,jp2,iip1,iip2,jjp1,jjp2,
     *  inode_color_change,istart,iend,istride,icount,in,imt0
      logical ibadface
      logical itsttp
C
C ######################################################################
C
      character*40 cmo
C
      integer npoints, ntets, nen, nef, nsdtopo, nsdgeom
      integer ic, ics, maxclr
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipimt1, imt1)
      integer isn1(1000000), itp1(1000000), imt1(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itettyp(10000000),
     *        itetoff(10000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      pointer (ipitetclr, itetclr)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), jtetoff(1000000)
      integer itet1(10000000),  jtet1(10000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
 
      pointer (ipidone, idone)
      integer idone(10000000)
C
C
C#######################################################################
      character*32 isubname, coption
      character*132 logmess
      integer itypar,nchild,itypmax,nxpt,itydud,i,j
      include 'chydro.h'
      include 'local_element.h'
C
C#######################################################################
      isubname='resetpts'
      ierror=0
C     Get mesh object.
C
      call cmo_get_name(cmo,ierror)
C
      if(ierror.ne.0) then
        write(logmess,'(a)') 'RESETPTS found bad mesh object'
        call writloga('default',0,logmess,0,ierror)
        goto 9999
      endif
C
C     ******************************************************************
C     Get mesh object data.
C
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,ilen,itype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,ilen,itype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                        nen,ilen,itype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                        nef,ilen,itype,ierror)
      call cmo_get_info('itp1', cmo, ipitp1, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1', cmo, ipisn1, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('imt1', cmo, ipimt1, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp', cmo, ipitettyp,ilen,itype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff', cmo, ipitetoff,ilen,itype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
 
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierror)
      call cmo_get_info('jtet',  cmo,ipjtet,ilen,itype,ierror)
C
      if(nwds.lt.2) then
         coption='parent'
      else
         coption=cmsgin(2)
      endif
 
      if(coption(1:6).eq.'parent') then
c.....reset itp and isn
         call mmgetblk('iparent',isubname,ipiparent,npoints,1,ierror)
         call unpackpc(npoints,itp1,isn1,iparent)
C        *** Get an array that indicates the parents of each point.
C
C        loop through points looking for parent type
C        change types of childen to dud
C        change type of parent to max of child types
C
         call getptyp(inamppar,itypar,ierror)
         call getptyp(inampdud,itydud,ierror)
         nchild=0
         do i=1,npoints
            itypmax=0
            if(itp1(i).eq.itypar ) then
              nxpt=isn1(i)
              if(nxpt.eq.0.or.itp1(nxpt).eq.itydud.or.isn1(nxpt).eq.0)
     *               then
                itp1(i)=itydud
                isn1(i)=0
              else
                do while (itp1(nxpt).ne.itypar.and.itp1(nxpt).ne.itydud)
                   nchild=nchild+1
                   itypmax=max(itypmax,itp1(nxpt))
                   itp1(nxpt)=itydud
                   nxpt=isn1(nxpt)
                 enddo
                 itp1(i)=itypmax
                 isn1(i)=0
              endif
            endif
         enddo
C
C        Fix up itet array replace references to child with parent.
         do i=1, ntets
           do j=1,nelmnen(itettyp(i))
              itet1(itetoff(i)+j)=iparent(itet1(itetoff(i)+j))
           enddo
        enddo
 
      elseif (coption(1:3).eq.'itp') then
c.....reset the itp array
C     set the external boundary node type based on boundary faces.
 
         cbuff='geniee ; finish'
         call dotaskx3d(cbuff,ierror)
C
         length=npoints
         if(length .le. 0) goto 9999
         call mmgetblk('idone',isubname,ipidone,length,1,icscode)
C
         do i=1,npoints
            idone(i)=0
            if (itp1(i).lt.ifitpst3) itp1(i)=0
         enddo
C
c        ntets=number of tet
c        nelmnef= face number on tet
c        mbndry=ntets*nelmnef + 1 (or greater)
c        jtet1= boundary flag
c        itp1=boundary flag(0,10,2,12,etc)
c        ielmface0=node numbers on face
 
         ipnt=0
         do it=1,ntets
            do i=1,nelmnef(itettyp(it))
               if (jtet1(jtetoff(it)+i).eq.mbndry) then
c              this is a boundary node
                  do j=1,ielmface0(i,itettyp(it))
                     node1 = itet1(itetoff(it)+
     *                              ielmface1(j,i,itettyp(it)))
                     itp1(node1)=ifitprfl
                  enddo
               endif
            enddo
         enddo
C
         do it=1,ntets
            do i=1,nelmnef(itettyp(it))
c            check if on an interface
             if (jtet1(jtetoff(it)+i).gt.0.and.
     *           jtet1(jtetoff(it)+i).ne.mbndry) then
               if (jtet1(jtetoff(it)+i).lt.mbndry) then
               jt=1+(jtet1(jtetoff(it)+i)-1)/nelmnef(itettyp(it))
               else
               jt=1+((jtet1(jtetoff(it)+i)-1)-mbndry)
     *             /nelmnef(itettyp(it))
               endif
                  if(itetclr(it).ne.itetclr(jt)) then
                     do j=1,ielmface0(i,itettyp(it))
                        node1=itet1(itetoff(it)+
     *                               ielmface1(j,i,itettyp(it)))
                        if(idone(node1).eq.0) then
                           idone(node1)=1
                           if(itp1(node1).eq.ifitprfl) then
                            itp1(node1)=ifitpinb
                           else
                              itp1(node1)=ifitpini
                           endif
                        endif
                     enddo
                  endif
               endif
            enddo
         enddo
         call mmrelblk('idone',isubname,ipidone,icscode)
         do 920 j=1,npoints
920      continue
 
         ierror=0
 
      elseif(coption(1:5).eq.'color') then
c.....reset itetclr
         do it=1,ntets
            itetclr(it)=0
         enddo
 
      elseif(coption(1:7).eq.'fixitet') then
c.....reset itet
         do it=1,ntets
            do nf=1,nef
               if(nf.eq.1) then
                 ip1=2
                 ip2=3
               elseif(nf.eq.2) then
                 ip1=3
                 ip2=1
               else
                 ip1=1
                 ip2=2
               endif
               jt1=jtet1((it-1)*nef+nf)
               if(jt1.eq.0.or.jt1.eq.mbndry) then
                 print *,'bad neibor ',it,nf
               else
                 if(jt1.gt.mbndry) jt1=jt1-mbndry
 
                 jt=1+(jt1-1)/nef
                 jf=jt1-(jt-1)*nef
                 if(jf.eq.1) then
                    jp1=2
                    jp2=3
                 elseif(jf.eq.2) then
                    jp1=3
                    jp2=1
                 else
                    jp1=1
                    jp2=2
                 endif
                 iip1=itet1((it-1)*nef+ip1)
                 iip2=itet1((it-1)*nef+ip2)
                 jjp1=itet1((jt-1)*nef+jp1)
                 jjp2=itet1((jt-1)*nef+jp2)
                 if(jjp1.ne.iip2) then
                   print *,'reverse triangle ',it,nf,jt,jf
 
                   itet1((jt-1)*nef+jp1)=jjp2
                   itet1((jt-1)*nef+jp2)=jjp1
                 endif
              endif
           enddo
         enddo
 
      elseif(coption(1:10).eq.'cell_color') then
c.....reset imt
c     reset node imt based on element color
c
       if (nwds .eq. 3)then
c
c      CHANGE CHOSEN NODES TO ITETCLR VALUES
c      Reset imt to element color if the node is the user specified imt value.
c      Loop through elements 1 to ntets (default)
c      Note, this will introduce a bias since the loop is based
c      on going from 1 to ntets. The bias can be changed by using
c      a negative value for the input imt value. Then loop will
c      go from ntets to 1.
c
c      resetpts / cell_color / inode_color_change
c      resetpts / cell_color / -inode_color_change
c
         inode_color_change = abs(imsgin(3))
         if(imsgin(3) .ge. 0)then
            istart  = 1
            iend    = ntets
            istride = 1
         elseif(imsgin(3) .lt. 0)then
            istart  = ntets
            iend    = 1
            istride = -1
         endif
      icount = 0
      do it = istart, iend, istride
        do in = 1, nelmnen(itettyp(it))
          if(imt1(itet1(itetoff(it)+in)) .eq. inode_color_change)then
             imt1(itet1(itetoff(it)+in))=itetclr(it)
             icount = icount + 1
          endif
        enddo
      enddo
      write(logmess,
     1  "(i8,' nodes of imt value',i8,' reset to their cell color')")
     2    icount,                  inode_color_change
      call writloga('default',0,logmess,0,ierror)
 
 
      else
c
c     CHANGE NODE IMT TO CHOSEN ITETCLR VALUES
C     Reset node imt to element colors.
c     Loop through itetclr 1 to maxclr (default)
c     istart,istride,iend can be used to loop through subset of colors
c      ie. for a grid with 14 colors
c          default will loop through colors from 1  to 14
c          1,12, 1 will loop through colors from 1  to 12
c          0,1,-1 will loop through colors from  14 to 1
c
c      resetpts / cell_color /
c      resetpts / cell_color / istart,iend,istride
c        1           2           3     4     5
C
C     READ PARSER VALUES
      icount=0
 
c      this sets loop for element colors, not element set (iend=maxclr)
       istride   = 1
       istart = 1
       iend   = 0
 
c      identify min and max of element colors
       do it = 1, ntets
         istart = min(istart, itetclr(it))
         iend   = max(iend  , itetclr(it))
       enddo
       maxclr = iend
c
       if (nwds .eq. 4)then
         istart = imsgin(3)
         iend   = imsgin(4)
 
       elseif (nwds .eq. 5)then
         istart = imsgin(3)
         iend   = imsgin(4)
         istride  = imsgin(5)
       endif
       if (istart.eq.0) istart = maxclr
       if (iend.eq.0) iend = maxclr
       if (istride.eq.0) istride = 1
       if (istart.gt.iend .and. istride.gt.0) istride = -1*istride
 
c
c     INITIALIZE imat
      length=npoints
      call mmgetblk('idone',isubname,ipidone,length,1,icscode)
      do i = 1,length
        idone(i)=0
      enddo
c
      do ic = istart, iend, istride
      do it = 1, ntets
        if(itetclr(it) .eq. ic)then
           do in = 1, nelmnef(itettyp(it))
                  imt1(itet1(itetoff(it)+in))=itetclr(it)
                  idone(itetclr(it)) = 1
           enddo
        endif
      enddo
      enddo
      do i = 1,length
        if (idone(i) .ne. 0) icount=icount+1
      enddo
c
      call mmrelblk('idone',isubname,ipidone,icscode)
      write(logmess,'(i12,a,i12)')
     *icount,' materials reset for imt1 out of a total of ',maxclr
      call writloga('default',0,logmess,0,ics)
 
      endif
c
c
      elseif(coption(1:10).eq.'intrface') then
c.....reset itp and jtet
        length=npoints
        do it=1,ntets
           do i=1,nelmnef(itettyp(it))
c     check if material types of all nodes on the face
C     if there are more than one interior node material type
C     we have a multimaterial tet - make this face an
c     interface face and change point types
              imt0=0
              ibadface=.false.
              do j=1,ielmface0(i,itettyp(it))
                 node1=itet1(itetoff(it)+
     *                               ielmface1(j,i,itettyp(it)))
                 if(itsttp('interior',itp1(node1)).and.
     *              .not.itsttp('intrface',itp1(node1))) then
                    if(imt0.eq.0) then
                       imt0=imt1(node1)
                    else
                       if(imt1(node1).ne.imt0.or.ibadface) then
                          ibadface=.true.
                          if(itsttp('boundary',itp1(node1))) then
                             itp1(node1)=ifitpinb
                          else
                             itp1(node1)=ifitpini
                          endif
                       endif
                    endif
                 endif
               enddo
               if(ibadface) then
                 if(jtet1(jtetoff(it)+i).lt.mbndry) then
                   jtet1(jtetoff(it)+i)=jtet1(jtetoff(it)+i)+mbndry
                   jt1=jtet1(jtetoff(it)+i)-mbndry
                   jt=1+(jt1-1)/nef
                   jf=jt1-(jt-1)*nef
                   jtet1(jtetoff(jt)+jf)=jtet1(jtetoff(jt)+jf)+mbndry
                 endif
               endif
            enddo
         enddo
 
      else
c.....invalid reset option
 
        write(logmess,"(' Option ',a,' not implemented')") coption
        call writloga('default',0,logmess,0,ierror)
        go to 9999
      endif
 
 9999 call mmrelprt(isubname,ics)
      return
      end
