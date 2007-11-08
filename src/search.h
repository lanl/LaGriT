*cd,search
C ######################################################################
C
C     PURPOSE -
C
C       Declarations for search information
C
C     CHANGE HISTORY -
C
C  $Log:   /pvcs.config/t3d/src/search.h_a
CPVCS
CPVCS       Rev 1.6   Tue Oct 26 14:08:18 1999   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Tue May 04 16:53:48 1999   jtg
CPVCS    fixed error that had "saved" the blocks twice
CPVCS
CPVCS       Rev 1.4   Thu Jan 21 20:59:16 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
C ######################################################################
      integer lenblkd
      parameter (lenblkd=64)
      pointer (iplstfal,lstfail(*)   )
      pointer (iptemp  ,itemp1(*)    )
      pointer (ipvol, vol(*))
      pointer (ipxvor  ,xvor(*)      )
      pointer (ipyvor  ,yvor(*)      )
      pointer (ipzvor  ,zvor(*)      )
      pointer (iplsttts,lsttts(*)    )
C        *** POINTERS RELATED TO CONNECTIONS.
      pointer (iplstcns1,lstcns1(*)    )
      pointer (iplstcns2,lstcns2(*)    )
C        *** POINTER TO TET LIST
      pointer (ipibint, ibint(*) )
      integer lstfail,itemp1,lsttts,
     * lstcns1,lstcns2,ibint
      integer istep,nstepdgn,idrastic,nlstfail,nlstptl,
     *  ntetmaxl,ntetexcl,ifail,ifailv,ifailr,ifailc,nlstnew,
     *  nlstold,lenold,lennew,ibigtet,iabort,imatint,idud,
     *  idelaun,nfacnew,nfacold,nfacout,iaddpts,iremove,
     *  itetstrt,lstptlen,ittstrt1,imtmax,iaddpass,
     *  lenmatx,lenmatmx,matblks,nsall
      real*8 xvor,yvor,zvor,vol,smldistp
      real*8 rl,xl,yl,zl,voltet,voltetr,ax,ay,az,
     *  delxb,delyb,delzb,xbigtet(4),ybigtet(4),zbigtet(4),
     *  small,smalluse,smaldist,smalarea,smalvol,smlttvol,
     *  smalfrac,boxsizex,boxsizey,boxsizez,addeps
      logical lifadd
      character*8 nname
      data nname/'nn3dn'/
 
      common/search_reals/ rl(4,lenblkd), xl(4,lenblkd), yl(4,lenblkd),
     *               zl(4,lenblkd),voltet(lenblkd), voltetr(lenblkd),
     *               ax(4,lenblkd) ,ay(4,lenblkd) ,az(4,lenblkd),
     *               delxb(6,lenblkd),delyb(6,lenblkd),delzb(6,lenblkd),
     *               xbigtet,ybigtet,zbigtet,
     *               small,smalluse,smaldist,smalarea,smalvol,smlttvol,
     *               smalfrac,boxsizex,boxsizey,boxsizez,addeps,smldistp
      common/search_ints/ istep,nstepdgn,idrastic,nlstfail,nlstptl,
     *               ntetmaxl,ntetexcl,ifail,ifailv,ifailr,ifailc,
     *               nlstnew,nlstold,lenold,lennew,ibigtet,iabort,idud,
     *               idelaun,nfacnew,nfacold,nfacout,iremove,
     *               itetstrt,lstptlen,ittstrt1,lenmatx,imatint,
     *               lenmatmx,matblks,nsall,imtmax,
     *               iaddpts,iaddpass
      common/search_logical/lifadd
 
      save/search_reals/
      save/search_ints/
      save/search_logical/
C ######################################################################
