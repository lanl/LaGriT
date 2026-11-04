      subroutine connect_order_opt(imsgin,xmsgin,cmsgin,msgtype,nwds)
C #####################################################################
C     PURPOSE -
C        Read the connect keywords old_order, noadd, check_interface
C        and exact
C        in any position, remove them from the command (so the
C        positional arguments are read the same way with or without
C        them), and record them: old_order for the insertion order
C        (lg_connect_order_set, lg_brio_order.c), noadd and
C        check_interface for connect (lg_connect_opts_set), exact for
C        the 3D in-sphere tests (lg_connect_exact_set).
C
C        Token 2 holds the algorithm name, delaunay (the only one). When
C        it is left out and token 2 is a point number or pset
C        (connect / 1,0,0 or connect / pset,get,name), delaunay is put
C        back in so the point range is read from tokens 3-5.
C
C        Before Nov 2026 noadd and check_interface were recognised only
C        as the last token in 3D and noadd only as token 18 in 2D.
C     INPUT ARGUMENTS -
C        imsgin,xmsgin,cmsgin,msgtype,nwds - the parsed command
C     OUTPUT ARGUMENTS -
C        the same, without the keywords
C #####################################################################
      implicit none
      include 'commands_lg.h'
      integer nwds
      integer imsgin(*),msgtype(*)
      real*8 xmsgin(*)
      character*(*) cmsgin(*)
      integer i,j,iold,inoadd,ichk,iexact
      character*32 ckey
C
      iold=0
      inoadd=0
      ichk=0
      iexact=0
      i=2
      do while (i.le.nwds)
         ckey=' '
         if (msgtype(i).eq.3) ckey=cmsgin(i)
         if (ckey.eq.'old_order' .or. ckey.eq.'noadd' .or.
     *       ckey.eq.'check_interface' .or. ckey.eq.'exact') then
            if (ckey.eq.'old_order') iold=1
            if (ckey.eq.'noadd') inoadd=1
            if (ckey.eq.'check_interface') ichk=1
            if (ckey.eq.'exact') iexact=1
            do j=i,nwds-1
               imsgin(j)=imsgin(j+1)
               xmsgin(j)=xmsgin(j+1)
               cmsgin(j)=cmsgin(j+1)
               msgtype(j)=msgtype(j+1)
            enddo
            nwds=nwds-1
         else
            i=i+1
         endif
      enddo
C
      if (nwds.ge.2 .and. nwds.lt.maxcmd_args) then
         if (msgtype(2).ne.3 .or. cmsgin(2).eq.'pset') then
            do j=nwds,2,-1
               imsgin(j+1)=imsgin(j)
               xmsgin(j+1)=xmsgin(j)
               cmsgin(j+1)=cmsgin(j)
               msgtype(j+1)=msgtype(j)
            enddo
            imsgin(2)=0
            xmsgin(2)=0.0d0
            cmsgin(2)='delaunay'
            msgtype(2)=3
            nwds=nwds+1
         endif
      endif
      call lg_connect_order_set(iold)
      call lg_connect_opts_set(inoadd,ichk)
      call lg_connect_exact_set(iexact)
      return
      end
