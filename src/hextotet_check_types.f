      subroutine hextotet_check_types(ioption,ihextyp,ihexclr,numhex,
     &           nsdtopo,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        Refuse a hextotet / grid2grid option that does not fit the
C        cells of the source mesh, before anything is created.
C
C        Options 3, 5, 6, 7, 14, 18 and 24 split every cell as if it
C        had the cell type of the option; they do not look at the
C        type. On other cells they produced meaningless tets
C        (hextotet / 3 on hexes gave 3 "tets" per hex) or wrote past
C        their work arrays (hextotet / 14 on hexes: MMVERIFY PANIC,
C        then an abort). Options 2 and 4 (2D) and the hybrid options
C        -1 and 0 check the cell type themselves and are not checked
C        here. Cells with itetclr = 0 are dropped by hextotet before
C        it splits, so they are not checked either.
C
C        option   cells it needs
C          3, 14, 18      prism
C          4 (3D mesh)    pyramid
C          5, 6, 7, 24    hex
C
C     INPUT ARGUMENTS -
C
C        ioption - the hextotet option (after the default is chosen)
C        ihextyp - cell types of the source mesh (itettyp)
C        ihexclr - cell materials of the source mesh (itetclr)
C        numhex  - number of source cells
C        nsdtopo - topological dimension of the source mesh
C
C     OUTPUT ARGUMENTS -
C
C        ierror  - 0 if the option fits every cell, 1 otherwise
C
C #####################################################################
C
      implicit none
      include 'local_element.h'
C
      integer ioption,numhex,nsdtopo,ierror
      integer ihextyp(*),ihexclr(*)
C
      integer i,ineed,nbad,ifirst,ierrw
      character*8 cneed,chave
      character*132 logmess
C
      ierror = 0
      ineed = 0
      if(ioption.eq.3 .or. ioption.eq.14 .or. ioption.eq.18)then
         ineed = ifelmpri
         cneed = 'prism'
      elseif(ioption.eq.5 .or. ioption.eq.6 .or. ioption.eq.7
     &       .or. ioption.eq.24)then
         ineed = ifelmhex
         cneed = 'hex'
      elseif(ioption.eq.4 .and. nsdtopo.eq.3)then
         ineed = ifelmpyr
         cneed = 'pyramid'
      endif
      if(ineed .eq. 0) return
C
      nbad = 0
      ifirst = 0
      do i = 1, numhex
         if(ihexclr(i).gt.0 .and. ihextyp(i).ne.ineed)then
            nbad = nbad + 1
            if(ifirst .eq. 0) ifirst = i
         endif
      enddo
      if(nbad .eq. 0) return
C
      ierror = 1
      call hextotet_type_name(ihextyp(ifirst),chave)
      write(logmess,'(a,i3,a,a,a)')
     &  'ERROR: hextotet: option ',ioption,' needs ',
     &  cneed(1:len_trim(cneed)),' cells'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a,i12,a,a,a,i12)')
     &  '  ',nbad,' cells are not; the first is a ',
     &  chave(1:len_trim(chave)),', cell ',ifirst
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)')
     &  '  options: 2, 4 quad; 3, 14, 18 prism; 4 pyramid;'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)')
     &  '  5, 6, 7, 24 hex. Nothing done.'
      call writloga('default',0,logmess,1,ierrw)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hextotet_type_name(ityp,cname)
C #####################################################################
C     PURPOSE - The name of a LaGriT cell type, for messages.
C #####################################################################
      implicit none
      include 'local_element.h'
      integer ityp
      character*8 cname
C
      cname = 'unknown'
      if(ityp.eq.ifelmpnt) cname = 'point'
      if(ityp.eq.ifelmlin) cname = 'line'
      if(ityp.eq.ifelmtri) cname = 'triangle'
      if(ityp.eq.ifelmqud) cname = 'quad'
      if(ityp.eq.ifelmtet) cname = 'tet'
      if(ityp.eq.ifelmpyr) cname = 'pyramid'
      if(ityp.eq.ifelmpri) cname = 'prism'
      if(ityp.eq.ifelmhex) cname = 'hex'
C
      return
      end
