      subroutine quality(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        check various measures of quality of a mesh
C
C     INPUT ARGUMENTS -
C
C        quality [{aspect/y|n}|
C                 {edge_ratio/y|n}|
C                 {edge_min/y|n}|
C                 {edge_max/y|n}|
C                 {volume/number-of-bins}|
C                 {angle/gt|lt/value}
C                 {taylor/fieldname/value}]^4
C
C        quality [quality_type] [quality_type options]
C
C
c        quality_type can be ASPECT, EDGE_RATIO, VOLUME, ANGLE, TAYLOR, or PCC
C
C        VOLUME [nbins] [material | itetclr] [lt|gt|eq|ne xvalue ] [eltset]
C              - nbins are the number of bins to distribute values
C              - ITETCLR will give volume according to element values
C              - lt|gt|eq|ne will report volumes based on compare to xvalue
C              - eltset,get,ename will report volumes on chosen eltset
C
c        VOLUME examples
C              quality volume  output with 5 bins
C              quality volume 2 output with 2 bins
C              quality volume itetclr  output 5 bins, give itetclr summary
C              quality volume 0 itetclr output 0 bins, give itetclr summary
C              quality volume lt .03 count number of elements volume lt .03
C              quality volume itetclr lt .03 count vols lt .03 by itetclr
C
C         eltset/e2/itetclr/eq/2
C              quality/volume/eltset,get,e2
C              quality/volume/itetclr/eltset,get,e2
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C     CHANGE HISTORY -
C
C $Log: quality.f,v $
C Revision 2.00  2007/11/09 20:03:58  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.26   20 Jun 2006 18:16:58   gable
CPVCS    Added options to detect sliver, cap, needle and wedge elements.
CPVCS    
CPVCS       Rev 1.25   23 Mar 2005 09:39:40   dcg
CPVCS    fix declarations for linux
CPVCS
CPVCS       Rev 1.24   30 Sep 2004 11:02:38   dcg
CPVCS    use variable pie from chydro.h, set pi = pie
CPVCS
CPVCS       Rev 1.23   30 Sep 2004 09:21:00   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS
CPVCS       Rev 1.22   02 May 2001 10:38:04   dcg
CPVCS    cosmetic change
CPVCS
CPVCS       Rev 1.21   27 Apr 2001 10:18:46   gable
CPVCS    Small change to output format of volume information.
CPVCS
CPVCS       Rev 1.20   10 Apr 2001 13:50:16   dcg
CPVCS    change declaration of ifound -it is integer
CPVCS
CPVCS       Rev 1.19   26 Apr 2000 10:52:46   dcg
CPVCS    change volume option to skip bins with
CPVCS    no members
CPVCS
CPVCS       Rev 1.18   Thu Apr 06 13:36:50 2000   dcg
CPVCS    replace call to get_info_i
CPVCS    remove unused declarations
CPVCS
CPVCS       Rev 1.17   16 Mar 2000 10:55:12   dcg
CPVCS    initialize iinc - don't know why code says these
CPVCS    can be user defined?
CPVCS
CPVCS       Rev 1.16   Fri Sep 24 14:33:50 1999   murphy
CPVCS    Took out code to test positive coupling coefficients and made
CPVCS    a call to pcc_test.f
CPVCS
CPVCS
CPVCS       Rev 1.15   Thu Sep 23 14:03:48 1999   murphy
CPVCS    Activated PCC option.
CPVCS
CPVCS       Rev 1.14   Mon Mar 15 20:17:52 1999   tam
CPVCS    print elements found to screen if idebug >=1
CPVCS
CPVCS       Rev 1.13   Fri Jan 08 17:18:12 1999   tam
CPVCS    clarified negative vol vs zero volume screen output
CPVCS
CPVCS       Rev 1.12   Wed Dec 23 12:53:50 1998   kuprat
CPVCS    Changed variable name NZEROVOL to more-appropriate NONPOSVOL.
CPVCS
CPVCS       Rev 1.10   Tue Sep 22 11:48:34 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.9   Wed Sep 16 16:06:10 1998   dcg
CPVCS    change loop index max to nelements for
CPVCS    initializing aratio
CPVCS
CPVCS       Rev 1.8   Mon Sep 14 12:20:22 1998   tam
CPVCS    added option to volume to control nbins, fixed
CPVCS    infinite loop possibility in reading parsed commands
CPVCS
CPVCS       Rev 1.7   Wed Mar 11 12:09:52 1998   tam
CPVCS    added volume options numbins, itetclr, lt|gt|eq|ne xvalue
CPVCS          and eltset,get,ename
CPVCS
CPVCS       Rev 1.6   Fri Nov 14 16:07:22 1997   dcg
CPVCS    make attribute for aspect ratios element based
CPVCS
CPVCS       Rev 1.5   Fri Oct 10 09:25:28 1997   dcg
CPVCS    calculate interval correctly if there are negative
CPVCS    volumes - use log steps
CPVCS
CPVCS       Rev 1.4   Fri Oct 03 11:02:28 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.3   Wed Sep 03 11:18:08 1997   dcg
CPVCS    declare epsilons as real*8
CPVCS
CPVCS       Rev 1.2   Wed Sep 03 10:28:28 1997   dcg
CPVCS    fix length of cbuff
CPVCS
CPVCS       Rev 1.1   Wed Sep 03 09:26:24 1997   dcg
CPVCS    add finish to dotask commands
CPVCS
CPVCS       Rev 1.0   Fri Aug 29 14:05:58 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
*
      include 'local_element.h'
      include 'chydro.h'
*
      real*8 pi
 
      integer nwds,ierr1,ier,ics,ilen,ityp
      integer imsgin(nwds),msgtyp(nwds)
      integer ntets,nfind,npoints,nerr,nneg,ierror
      integer i,j,ii, mbndry
      integer iprev
      character*32 cmsgin(nwds)
      character*256 cbuff
      real*8 xmsgin(nwds)
      real*8 xmin,xmax,ymin,ymax,zmin,zmax,epsilonarea
      real*8 xica(8),yica(8),zica(8)
      logical idone
C  pcc computation vars
      logical ifpcc
      real*8 dist2
C  Angle computation vars
      real*8 xice(8),yice(8),zice(8)
      integer badcount,badcountelm(nelmtypes)
      character*10 badcountname(nelmtypes)
C  Angle operation: 0 = lt, 1 = gt
      integer angleop
      real*8 angleval,anglevalrad
      real*8 minangle,maxangle,e_minangle,e_maxangle
C  New aspect ratio binning variables
      integer numarbins, arbincnts(1000000)
      parameter (numarbins = 7)
      real*8 arbinends(numarbins)
      character*20 arbinchars(numarbins)
      pointer (iparbincnts,arbincnts)
C  local variables
      logical ichk
      integer  iwrite, nwrite
      pointer(ipxic,xic(1000000))
      pointer(ipyic,yic(1000000))
      pointer(ipzic,zic(1000000))
      pointer(ipitetoff,itetoff(1000000))
      pointer(ipjtetoff,jtetoff(1000000))
      pointer(ipitettyp,itettyp(1000000))
      pointer(ipitet,itet(1000000))
      pointer(ipjtet,jtet(1000000))
      pointer(ipitetclr,itetclr(*))
      pointer(ipvol,vol)
      real*8 aratio(1000000)
      pointer(iparatio,aratio)
      character*32 cmo, isubname
      character* 132 lgms 
      integer i1,it,ic,icolor
      integer id_edge_min, id_edge_max, ielm_invalid,nelm_invalid
      real*8  edge_min, edge_max, edge_min_max_ratio
      real*8 srat,voltest
      real*8 xic,yic,zic,sratmin,sratmax
      integer jtetoff, jtet, itetclr
      integer itetoff,itet,itettyp,nelements,length,ifound

C  variables for volume computation
      integer nbins,nsmallvol,nsmallvoleq,nonposvol,nonzero,ielmtyp
      integer totbyclr(*)
      integer totbyop(*),iclr(*)
      real*8 xicv(8),yicv(8),zicv(8),vol(*)
      real*8 sumbyclr(1000000)
      real*8 volmin, volmax, volsum
      pointer(iptotbyclr,totbyclr)
      pointer(iptotbyop,totbyop)
      pointer(ipiclr,iclr)
      pointer(ipsumbyclr,sumbyclr)

C  New volume variables
      logical ifposvol, ifbins, ifnormalized
      integer istart,iinc,nclrs,icount
      integer ifirst,ilast
      real*8 volposmin
      real*8 logincrement, endinters(1000000)
C      real*8 logvolmin
      integer neleints(1000000)
      pointer (ipendinters,endinters)
      pointer (ipneleints,neleints)
      character*2 opstr
C  End of volume variables
C  Taylor error vars
      real*8 taylorval
      character*32 fieldname
      integer icharlnf
      integer t_errorlen, t_errors(1000000)
      pointer (ipt_errors,t_errors)
      integer isn(1000000), itp(1000000)
      integer iparent(1000000)
      pointer (ipisn,isn), (ipitp,itp)
      pointer (ipiparent,iparent)
c  Use these to report individual edges -- or something else if better.
c      integer numbadedges
c      integer badedges(1000000,1000000)
c      pointer (ipbadedges,badedges)
C  End of Taylor vars
      real*8 epsilonvol,epsilonaspect
      logical ifaspect,ifedge_ratio,ifedge_min,ifedge_max
      logical ifvolume,ifattrib,ifangle,iftaylor, ifquad
      real*8 epsilon,volumelt
      logical ifdebug, iflogs, iftrace
C  Element sets
      logical ifbyclr,ifeltset,ifcompare
      integer ipt1,ipt2,ipt3
      real*8  compval
      character*32 ch1,ch2,ch3,chtmp
      integer mpary, mpno
      pointer (ipmpary , mpary(1000000))
      integer xtetwd
      pointer (ipxtetwd,xtetwd(1000000))
C  Parameter reading vars
      logical ifaspseen, ifvolseen, ifangseen,iftaylorargs
      integer parcount
      integer ierrw, icscode

C     Quad quality measures: quality, regularity, and flag. See
C     quad_quality.c for details.
      pointer (ipqquality, qquality)
      pointer (ipqregularity, qregularity)
      pointer (ipqflag, qflag)

      real*8 qquality(1000000)
      real*8 qregularity(1000000)
      integer qflag(1000000)

      integer quad_bins(7)
      real*8 quad_bin_ends(8)

      integer nnormal_quads
      integer ndegenerate_quads
      integer nwarped_quads
C Macros
      real*8 a11,a12,a13,a21,a22,a23
      dist2(a11,a12,a13,a21,a22,a23)  =
     *  (a11-a21)**2 + (a12-a22)**2 + (a13-a23)**2
 
 
C BEGIN
c
      pi=pie
      isubname='quality'
      idone = .false.
      iflogs = .true.
      ifeltset = .false.
      ifdebug = .false.
      ifcompare = .false.
      iftrace = .false.
      iwrite = 0
      nwrite = 20
      nfind=0
      ipt1=1
      ipt2=0
      ipt3=0


      badcountname(ifelmtet) = 'tets'
      badcountname(ifelmpyr) = 'pyramids'
      badcountname(ifelmpri) = 'prisms'
      badcountname(ifelmhex) = 'hexes'
      badcountname(ifelmhyb) = 'hybrids'
      badcountname(ifelmply) = 'polygons'
      badcountname(ifelmtri) = 'triangles'
      badcountname(ifelmqud) = 'quads'

 
C  get mesh object name
      call cmo_get_name(cmo,ier)
      if (ier.ne.0) then
         write(lgms ,"('No current mesh object')")
         call writloga('default',0,lgms ,0,ierrw)
         return
      endif
      call cmo_get_info('idebug',cmo,idebug,ilen,ityp,ier)
      if(idebug.gt.1) nwrite = length
      if(idebug.eq.1) nwrite = 20
      if(idebug.eq.0) nwrite = 1
      if(idebug.gt.0) ifdebug=.true.
 
C  Determine calling parameters
      ifaspseen = .false.
      ifvolseen = .false.
      ifangseen = .false.
      iftaylorargs = .false.
      parcount = 0
      ifaspect = .false.
      ifedge_ratio = .false.
      ifattrib = .false.
      ifvolume = .false.
      ifpcc = .false.
      ifangle = .false.
      iftaylor = .false.
      ifquad = .false.
      nbins = 5
c 
c PARSE command string
c

c 1) quality
c 2) quality_type = aspect, volume, angle, taylor, pcc
c    2+i) options for quality_type
c inext)  eltset,get,ename
c inext)  eltset,make,ename and/or attribute,make,aname
c
C     Set defaults
      ifedge_min = .false.
      ifedge_max = .false.
      ifbyclr = .false.
      ifbins = .true.
      nbins = 5
      ielm_invalid = 0
      nelm_invalid = 0

C     the default is to normalize values for log scale bin numbers 0 to 1 
C     do not normalize ascpect or edge_ratio
      ifnormalized = .true.
 
      if (nwds.eq.1) then
        ifaspect = .true.
        ifvolume = .true.
        ifnormalized = .false.
      endif
 
c     loop through command words
      i  = 2
      do while (nwds.ge.i)
         iprev = i
 
         if (msgtyp(i).ne.3) then
           write(lgms ,'("bad parameter ",i3,": must be a string")')i
           call writloga('default',0,lgms ,0,ierrw)
           goto 9999
         endif
 
c........aspect command
         if (cmsgin(i)(1:6) .eq. 'aspect') then
            ifaspseen = .true.
            ifaspect = .true.
            ifnormalized = .false.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'y') then
                 ifattrib=.true.
                 i = i + 1
              elseif (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'n') then
                 ifattrib=.false.
                 i = i + 1
              endif
            endif
 
c........edge_ratio command
         elseif (cmsgin(i)(1:10) .eq. 'edge_ratio') then
            ifaspseen = .true.
            ifedge_ratio = .true.
            ifnormalized = .false.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'y') then
                 ifattrib=.true.
                 i = i + 1
              elseif (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'n') then
                 ifattrib=.false.
                 i = i + 1
              endif
            endif
 
c........edge_min command
         elseif (cmsgin(i)(1:10) .eq. 'edge_min') then
            ifaspseen = .true.
            ifedge_min = .true.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'y') then
                 ifattrib=.true.
                 i = i + 1
              elseif (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'n') then
                 ifattrib=.false.
                 i = i + 1
              endif
            endif
 
c........edge_max command
         elseif (cmsgin(i)(1:10) .eq. 'edge_max') then
            ifaspseen = .true.
            ifedge_max = .true.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'y') then
                 ifattrib=.true.
                 i = i + 1
              elseif (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'n') then
                 ifattrib=.false.
                 i = i + 1
              endif
            endif
 
c........positive coeff command
         elseif (cmsgin(i)(1:6) .eq. 'pcc') then
            call pcc_test(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr1)
            i = i + 1
            goto 9999

c........Quad quality command
         elseif (cmsgin(i)(1:4) .eq. 'quad') then
            ifquad = .true.
            i = i + 1
 
c........volume command
c        volume [nbins] [material|itetclr] [lt|gt|eq|ne xvalue]
         elseif (cmsgin(i)(1:6) .eq. 'volume') then
            ifvolseen = .true.
            ifvolume = .true.
            i = i + 1
 
c           set number of bins
            if (nwds.ge.i .and. msgtyp(i).eq.1) then
              nbins = imsgin(i)
              if (nbins.le.0) then
                nbins=1
                ifbins = .false.
              endif
              i = i + 1
            endif
 
c           report volumes by material, color of element
            if (nwds.ge.i .and. msgtyp(i).eq.3 .and.
     >         (cmsgin(i)(1:8).eq.'material'   .or.
     >          cmsgin(i)(1:7).eq.'itetclr')   ) then
              ifbyclr = .true.
              i = i + 1
            endif
 
c           find values by operator and value
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:2).eq.'lt'  .or.
     >            cmsgin(i)(1:2).eq.'gt'  .or.
     >            cmsgin(i)(1:2).eq.'ge'  .or.
     >            cmsgin(i)(1:2).eq.'le'  .or.
     >            cmsgin(i)(1:2).eq.'ne'  .or.
     >            cmsgin(i)(1:2).eq.'eq' ) then
 
                ifcompare = .true.
                opstr = cmsgin(i)
                i=i+1
                compval = xmsgin(i)
                i=i+1
 
              endif
 
            endif
 
c........angle command
         elseif (cmsgin(i)(1:5) .eq. 'angle') then
            ifangseen = .true.
            ifangle = .true.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i) (1:2) .eq. 'gt') then
                angleop = 1
                i = i + 1
              elseif (cmsgin(i) (1:2) .eq. 'lt') then
                angleop = 0
                i = i + 1
              endif
            endif
 
            if (nwds.ge.i .and. msgtyp(i).ne.3) then
              if (msgtyp(i).eq.1) then
                 angleval = dble(imsgin(i))
              else
                 angleval = xmsgin(i)
              endif
              i = i + 1
            endif
 
            do while (angleval .lt. 0.0)
               angleval = angleval + 360.0
            enddo
            do while (angleval .gt. 360.0)
               angleval = angleval - 360.0
            enddo
 
 
c........taylor command
c        quality/taylor/ fieldname / value /

         elseif (cmsgin(i) (1:6) .eq. 'taylor') then
            iftaylor = .true.
            i = i + 1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              fieldname = cmsgin(i) (1:icharlnf(cmsgin(i)))
              iftaylorargs = .true.
              i = i + 1
            endif
 
            if (nwds.ge.i .and. msgtyp(i).ne.3) then
              if (msgtyp(i).eq.1) then
                taylorval = dble(imsgin(i))
              else
                taylorval = xmsgin(i)
              endif
              i = i + 1
            else
              iftaylorargs = .false.
            endif
 
c........eltset command
         elseif (cmsgin(i) (1:6) .eq. 'eltset') then
            ifeltset=.true.
            i=i+1
            if (nwds.ge.i .and. msgtyp(i).eq.3) then
              if (cmsgin(i)(1:3).eq.'get') then
                ch1=cmsgin(i-1)
                ch2=cmsgin(i)
                ch3=cmsgin(i+1)
                i = i + 2
              endif
            endif
 
        endif
c       END quality command options
 
c       check for undefined command words
        if (iprev .eq. i ) then
          write (lgms ,'(i2,a)')i, 'th word is undefined for quality'
          call writloga('default',0,lgms ,1,ierrw)
          goto 9999
        endif
      enddo
c     END while loop over commands

      if (iftaylor .eqv. .true. .and. iftaylorargs .eqv. .false.) then
         write(lgms ,'(a)')
     *   'Error: quality/taylor missing arguments: /field/value'
         call writloga('default',1,lgms ,1,ierrw)
         goto 9999
      endif

 
c
c SETUP prepare the mesh object and memory 
c

c     Get memory for current cmo
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ier)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ier)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,ityp,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ier)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ier)
      length=nelements*nelmnef(ifelmtri)
      mpno=nelements
      ntets=nelements
      if (ntets .le. 0) then
        write (lgms ,'(i10,a)')ntets,' total elements in mesh.'
        call writloga('default',0,lgms ,1,ierrw)
        goto 9999
      endif
 
c     Get indexed element set
      call mmgetblk('mpno',isubname,ipmpary,ntets,1,ics)
        if (ifeltset) then
 
          call cmo_get_info('xtetwd',cmo,ipxtetwd,ilen,ityp,ier)
          if(ier.ne.0) call x3d_error(isubname,'eltset,get')
          ch1='eset'
          ch2='get'
          call eltlimc(ch1,ch2,ch3,ipmpary,mpno,ntets,xtetwd)
 
          if (mpno.le.0) then
            write(lgms ,'(i10,a,a)')mpno,' elements in eltset: ',ch3
            call writloga('default',0,lgms ,0,ierrw)
            goto 9998
          endif
 
        else
 
          if (ipt2.le.0) ipt2=ntets
          ipt1=max(1,min(ipt1,ntets))
          ipt2=max(1,min(ipt2,ntets))
          ipt3=max(1,min(ipt3,ntets))
          mpno=0
          do i=ipt1,ipt2,ipt3
             mpno=mpno+1
             mpary(mpno)=i
          enddo
        endif
 
      if (ifcompare) then
        call mmgetblk('totbyop',isubname,iptotbyop,nelements,1,ier)
      endif
 
c     setup arrays if writing by color of itetclr
      if (ifbyclr) then
        call mmgetblk('sumbyclr',isubname,ipsumbyclr,nelements,2,ier)
        call mmgetblk('totbyclr',isubname,iptotbyclr,nelements,1,ier)
        call mmgetblk('iclr',isubname,ipiclr,nelements,1,ier)
c     count number of distinct colors
        do i = 1, nelements
          totbyclr(i) = itetclr(i)
        enddo
        call hpsorti(nelements,totbyclr)
        istart=1
        nclrs=1
        iinc=1
        iclr(1)=totbyclr(1)
        do i=2,nelements
          if(totbyclr(i).ne.totbyclr(i-1)) then
             nclrs=nclrs+1
             iclr(nclrs)=totbyclr(i)
          endif
        enddo
        volsum = 0.0
        do i = 1, nelements
          sumbyclr(i) = 0.0
          totbyclr(i) = 0
        enddo
        if (ifbins) then
        write (lgms ,"('QUALITY TOTALS for ',i10,' MATERIALS:')")
     *        nclrs
        call writloga('default',2,lgms ,0,ierrw)
        write (lgms ,
     *"('...........................................................')")
        call writloga('default',0,lgms ,0,ierrw)
        endif
 
      else
c     do not loop through by color, do all elements
        istart = 1
        nclrs = 1
        iinc = 1
 
      endif
c     end setup for volumes by material(itetclr)
 
c     loop through material colors and setup element sets
c     the loop will happen once if itetclr option not chosen
      icount = 0
      do icolor = istart, nclrs, iinc
      if (ifbyclr) then
 
        call writset('stat','tty','off',ierrw)
        write(cbuff,'(a,i10,a)')
     >  'eltset/-etmp-/itetclr/eq/',iclr(icolor),'/ ; finish '
        call dotaskx3d(cbuff,ier)
        chtmp='-etmp-'
 
        if (ifeltset) then
          write(cbuff,'(a,a,a)')
     >    'eltset/-etmp2-/inter/ -etmp- ',ch3(1:icharlnf(ch3)),
     >    ' / ; finish '
          call dotaskx3d(cbuff,ier)
          chtmp='-etmp2-'
        endif
        call writset('stat','tty','on',ierrw)
 
        if (ier.ne.0 .or. ierr1.ne.0) then
           write (lgms ,"('error during eltset create -etmp-')")
           call writloga('default',0,lgms ,0,ierrw)
        endif
 
 
c       nelement is length of mpary, mpno = num pts in mpary set
        call cmo_get_info('xtetwd',cmo,ipxtetwd,ilen,ityp,ier)
        if(ier.ne.0) call x3d_error(isubname,'get info xtetwd')
        ch1='eltset'
        ch2='get'
        mpno=0
        call eltlimc(ch1,ch2,chtmp,ipmpary,mpno,ntets,xtetwd)
 
        if (icolor .eq. istart) ifirst = mpary(1)
 
      endif
c     end setup for element sets by material itetclr
 
 
 
c
c     SWITCH on quality options
c
 
c.....options aspect and edge_ratio and edge_min and edge_max

      if ((ifaspect).or.(ifedge_ratio).or.
     *    (ifedge_min).or.(ifedge_max)) then

C    aspect (aspect ratio) 
C    Computes the ratio of the radius of the circumsphere to
C    the radius of the inscribed sphere of a tetrahedron.
C    Ratio is multiplied by 3 so that a value of one
C    indicates a regular tetrahedron. If the ratio is
C    smaller than xrefine(1) the tet and edges are tagged.
C    The ratio should never be greater than one.
C    Valid types are tet, tri, hex (diagonals), and quad
C    
C    edge_ratio
C    computes the ratio (shortest element edge/longest element edge)
C    All element types are valid
C
C    edge_min and edge_max
C    finds the min or max edge length for each element
C    All element types are valid
 

c.....option aspect 

      ielm_invalid = 0
      nelm_invalid = 0
      if(ifattrib) then
        if(ifaspect)then
         cbuff='cmo/addatt//aratio/VDOUBLE/scalar/nelements' //
     *     ' ; finish'
         call dotaskx3d(cbuff,ierror)
         call mmgetpr('aratio',cmo,iparatio,icscode)
        elseif(ifedge_ratio)then
         cbuff='cmo/addatt//eratio/VDOUBLE/scalar/nelements' //
     *     ' ; finish'
         call dotaskx3d(cbuff,ierror)
         call mmgetpr('eratio',cmo,iparatio,icscode)
        elseif(ifedge_min)then
         cbuff='cmo/addatt//edgemin/VDOUBLE/scalar/nelements' //
     *     ' ; finish'
         call dotaskx3d(cbuff,ierror)
         call mmgetpr('edgemin',cmo,iparatio,icscode)
        elseif(ifedge_max)then
         cbuff='cmo/addatt//edgemax/VDOUBLE/scalar/nelements' //
     *     ' ; finish'
         call dotaskx3d(cbuff,ierror)
         call mmgetpr('edgemax',cmo,iparatio,icscode)
        else
C        Use a temporary array
         call mmgetblk('aratio',isubname,iparatio,nelements,2,ier)
        endif
      else
C        Use a temporary array
         call mmgetblk('aratio',isubname,iparatio,nelements,2,ier)
      endif
C  Initialize aratio array
      do i=1,nelements
         aratio(i)=0.
      enddo
C
      call get_epsilon('epsilonl',epsilon)
      epsilonaspect = epsilon**3
      write(lgms ,'("epsilonl, epsilonaspect: ",2(1pe15.7))')
     * epsilon, epsilonaspect
      call writloga('default',1,lgms ,0,ierrw)
 
C  prepare bins for storing the ratio counts
C  bins are numbers between 0 and 1 using log scale
      call mmgetblk('arbincnts',isubname,iparbincnts,numarbins,1,ier)
      DATA arbinends/0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0/
      DATA arbinchars/'.01','.02','.05','.1','.2','.5','1.'/
      do i=1,numarbins
         arbincnts(i) = 0
      enddo
      nerr = 0
      nneg = 0
 
      sratmin =  100
      sratmax = -100
      do ii=1,mpno
         it=mpary(ii)
         ielmtyp=itettyp(it)
         do i=1,nelmnen(ielmtyp)
            i1 = itet (itetoff(it)+i)
            xica(i)=xic(i1)
            yica(i)=yic(i1)
            zica(i)=zic(i1)
         enddo
 
C
C     for aspect, currently valid types include:
C     tet hex tri and quad

      if(ifaspect)then

         if (ielmtyp.eq.ifelmtet .or. ielmtyp.eq.ifelmtri .or. 
     *       ielmtyp.eq.ifelmhex .or. ielmtyp.eq.ifelmqud ) then 

         call aratio_element(ielmtyp,xica,yica,zica,srat,epsilonaspect)

         else
             srat = 0.0
             nelm_invalid = nelm_invalid + 1
             ielm_invalid = ielmtyp
         endif

c.....option edge_ratio and edge_min and edge_max
      elseif((ifedge_ratio).or.(ifedge_min).or.(ifedge_max))then

         call aratio_element_edge(ielmtyp,xica,yica,zica,
     *          id_edge_min,id_edge_max,
     *          edge_min,edge_max,edge_min_max_ratio)
         if(ifedge_ratio) then
            srat = edge_min_max_ratio
         elseif(ifedge_min) then
            srat = edge_min
         elseif(ifedge_max) then
            srat = edge_max
         endif
      endif
C
C  Put srat into aratio array.
C
         aratio(it)=srat
C
C  Compute min/max of variable
C
         if (srat .lt. sratmin)
     *     sratmin = srat
         if (srat .gt. sratmax)
     *     sratmax = srat
      enddo

C     Split the loop so we know the min/max during bin distribution.
C
      do ii=1,mpno
         it=mpary(ii)

         if (sratmax .eq. 0.) then
             srat = 0.
         else
             if (ifnormalized) then
               srat = aratio(it)/sratmax
c      print*,"srat set norm: ",srat," from ",aratio(it)
             else
               srat = aratio(it)
c      print*,"srat set real: ",srat," from ",aratio(it)
             endif
         endif
C
C  Calculate some distribution here
C  Using simple linear search to find appropriate bins

         if (srat .lt. 0.0) then
            nneg = nneg + 1
         else
            i = 1
            do while (arbinends(i) .lt. srat .and. i .lt. numarbins)
               i = i + 1
            enddo
            if (srat .le. arbinends(i)) then
               arbincnts(i) = arbincnts(i) + 1
            else

c           print*,"srat, arbin : ",srat,arbinends(i)
c           print*,"abs diff: ",abs(srat-arbinends(i))

c              check for values greater than 1 within epsilon
               if (abs(srat-arbinends(i)) .gt. epsilon) then
                 nerr = nerr + 1
                 if(iwrite.le.nwrite) then
                 write(lgms ,"('gt 1 aspect ratio for elem ',i10,
     *          ' is ',e11.4)") it,srat
                 call writloga('default',0,lgms ,0,ierrw)
                 iwrite=iwrite+1
                 endif
               else
c         put values within epsilon back into the bin count 
                 arbincnts(i) = arbincnts(i) + 1
               endif 
            endif
         endif
       enddo

       if (nerr.gt.0) then
         write(lgms ,"('Total elem w/ aspect ratio > 1: ',i10)") nerr
         call writloga('default',0,lgms ,0,ierrw)
       endif
C
C  Write totals for quantities
C

      write (lgms ,"('--------------------------------------------')")
      call writloga('default',0,lgms ,0,ierrw)
C
C     Aspect Ratio Output
      if(ifaspect)then

c     report only if there are some valid numbers

      if (sratmin .eq. 0. .and. sratmax .eq. 0.) then

        write(lgms ,"('elements with aspect ratio not set: ',i10)") 
     >  nelm_invalid
        call writloga('default',0,lgms ,0,ierrw)
      
      else

      if (ifnormalized) then
         write(lgms ,'(a)')
     *   'Aspect ratio normalized by dividing with max value.'
         call writloga('default',0,lgms ,0,ierrw)
      endif

        write(lgms ,"('elements with aspect ratio < ',a3,
     *     ':           ',i10)") arbinchars(1), arbincnts(1)
        call writloga('default',0,lgms ,0,ierrw)

        do i=2,numarbins
             write(lgms ,"('elements with aspect ratio b/w ',a3,
     *       ' and ',a3,': ',i10)") arbinchars(i-1),
     *       arbinchars(i), arbincnts(i)
             call writloga('default',0,lgms ,0,ierrw)
        enddo
 
        if (nneg .gt. 0) then
          write(lgms ,"('elements with aspect ratio < 0.: ',i10)") nneg
          call writloga('default',0,lgms ,0,ierrw)
        endif
        if (nerr .gt. 0) then
          write(lgms ,"('elements with aspect ratio > 1.: ',i10)") nerr
          call writloga('default',0,lgms ,0,ierrw)
        endif
      endif
 
      write(lgms ,"('min aspect ratio = ',e11.4,
     *  '  max aspect ratio = ',e11.4)") sratmin,sratmax
      call writloga('default',0,lgms ,0,ierrw)

c     report invalid elements
      if (nelm_invalid .gt. 0) then
         write (lgms ,'(a,a)')
     >    "Warning: No aspect ratio for invalid element type: ",
     >   badcountname(ielm_invalid)
         call writloga('default',0,lgms ,0,ierrw)

         write(lgms ,'(a,i14)')
     >   "Warning: Number of invalid elements: ", nelm_invalid
         call writloga('default',0,lgms ,0,ierrw)
 
         write(lgms ,'(i10,a,i10,a)')mpno-nelm_invalid,
     >    ' out of ',mpno,' total elements evaluated.'
         call writloga('default',0,lgms ,1,ierrw)
      endif

C     end aspect ratio summary for valid types

c     min/max Edge Ratio Output

      elseif(ifedge_ratio)then

      if (ifnormalized) then
         write(lgms ,'(a)')
     *   'Edge ratio normalized by dividing with max value.'
         call writloga('default',0,lgms ,0,ierrw)
      endif


      write(lgms ,"('elements with min/max edge ratio < ',a3,
     *     ':           ',i10)") arbinchars(1), arbincnts(1)
      call writloga('default',0,lgms ,0,ierrw)
      do i=2,numarbins
         write(lgms ,"('elements with min/max edge ratio b/w ',a3,
     *        ' and ',a3,': ',i10)") arbinchars(i-1),
     *        arbinchars(i), arbincnts(i)
         call writloga('default',0,lgms ,0,ierrw)
      enddo
 
      if (nneg .gt. 0) then
        write(lgms ,
     *  "('element norm min/max edge ratio < 0.: ',i10)") nneg
        call writloga('default',0,lgms ,0,ierrw)
      endif
      if (nerr .gt. 0) then
        write(lgms ,
     *  "('element norm min/max edge ratio > 1.: ',i10)") nerr
        call writloga('default',0,lgms ,0,ierrw)
      endif
 
      write(lgms ,"('min edge ratio = ',e11.4,
     *  '  max edge ratio = ',e11.4)") sratmin,sratmax
      call writloga('default',0,lgms ,0,ierrw)
C
C     min Edge length Output
C
      elseif(ifedge_min)then

      if (ifnormalized) then
         write(lgms ,'(a)')
     *   'Edge min normalized by dividing with max value.'
         call writloga('default',0,lgms ,0,ierrw)
      endif

      write(lgms ,"('element norm min edge length < ',a3,
     *     ':       ',i10)") arbinchars(1), arbincnts(1)
      call writloga('default',0,lgms ,0,ierrw)
      do i=2,numarbins
         write(lgms ,"('element norm edge length b/w ',a3,
     *        ' and ',a3,': ',i10)") arbinchars(i-1),
     *        arbinchars(i), arbincnts(i)
         call writloga('default',0,lgms ,0,ierrw)
      enddo
 
      if (nneg .gt. 0) then
        write(lgms ,"('element norm edge length < 0.: ',i10)") nneg
        call writloga('default',0,lgms ,0,ierrw)
      endif
      if (nerr .gt. 0) then
        write(lgms ,
     *  "('elements edge  normalized length > 1.: ',i10)") nerr
        call writloga('default',0,lgms ,0,ierrw)
      endif
 
      write(lgms ,"('min min edge length = ',e11.4,
     *  '  max min edge length = ',e11.4)") sratmin,sratmax
      call writloga('default',0,lgms ,0,ierrw)
C
C     max Edge Length Output
C
      elseif(ifedge_max)then

      if (ifnormalized) then
         write(lgms ,'(a)')
     *   'Edge max normalized by dividing with max value.'
         call writloga('default',0,lgms ,0,ierrw)
      endif

      write(lgms ,"('element norm max edge length < ',a3,
     *     ':       ',i10)") arbinchars(1), arbincnts(1)
      call writloga('default',0,lgms ,0,ierrw)
      do i=2,numarbins
         write(lgms ,"('element norm edge length b/w ',a3,
     *        ' and ',a3,': ',i10)") arbinchars(i-1),
     *        arbinchars(i), arbincnts(i)
         call writloga('default',0,lgms ,0,ierrw)
      enddo
 
      if (nneg .gt. 0) then
        write(lgms ,"('element norm edge length < 0.: ',i10)") nneg
        call writloga('default',0,lgms ,0,ierrw)
      endif
      if (nerr .gt. 0) then
        write(lgms ,"('element norm edge length > 1.: ',i10)") nerr
        call writloga('default',0,lgms ,0,ierrw)
      endif
 
      write(lgms ,"('min max edge length = ',e11.4,
     *  '  max max edge length = ',e11.4)") sratmin,sratmax
      call writloga('default',0,lgms ,0,ierrw)
      endif
      endif
C     END aspect END edge_ratio
 
c.....option volume distribution of elements
      if (ifvolume) then
c
c  if no elements for this color skip to end
c
      if(mpno.eq.0) go to 50
 
      call mmgetindex('endinters',isubname,ifound,ics)
      if (ifound.le.0) then
         i = nbins
         if(nbins.eq.0) i=1
         call mmgetblk('endinters',isubname,ipendinters,
     *                  i+1, 2,ier)
         call mmgetblk('neleints',isubname,ipneleints,
     *                  i  ,1,ier)
         call mmgetblk('vol',isubname,ipvol,nelements,2,ier)
      endif
 
      nsmallvol = 0
      nsmallvoleq = 0
      nonposvol = 0
      nonzero = 0
      call setsize()
      call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilonarea,
     *             epsilonvol)
      if (ifound.le.0) then
        write(lgms ,'("epsilonvol: ",1pe15.7)') epsilonvol
        call writloga('default',1,lgms ,0,ierrw)
      endif
 
c     loop through elements counting volumes
      do ii=1,mpno
         it=mpary(ii)
 
         ic=itetclr(it)
         ielmtyp=itettyp(it)
         do i=1,nelmnen(ielmtyp)
            i1 = itet (itetoff(it)+i)
            xicv(i)=xic(i1)
            yicv(i)=yic(i1)
            zicv(i)=zic(i1)
         enddo
 
         call volume_element(ielmtyp,xicv,yicv,zicv,volumelt)
 
C        Count volumes by itetclr value
         if (ifbyclr) then
           sumbyclr(ic) = sumbyclr(ic) + volumelt
           totbyclr(ic)  = totbyclr(ic)  + 1
           volsum = volsum + volumelt
         endif
 
C        Count volumes lt and eq to epsilonvol
         if(volumelt.lt.epsilonvol) nsmallvol=nsmallvol+1
         if(volumelt.le.epsilonvol) nsmallvoleq=nsmallvoleq+1
 
C        Count non-positive and zero volumes
C        NOTE vol() index is set by mpary 1 to mpno
         if(volumelt.lt.0.d0) nonzero=nonzero+1
         if(volumelt.le.0.d0) nonposvol=nonposvol+1
         vol(it)=volumelt
 
c        Count volumes with compare operator
         if (ifcompare) then
           if (opstr(1:2).eq.'lt') then
             if (volumelt .lt. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
           elseif (opstr(1:2).eq.'gt') then
             if (volumelt .gt. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
           elseif (opstr(1:2).eq.'ne') then
             if (volumelt .ne. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
           elseif (opstr(1:2).eq.'ge') then
             if (volumelt .ge. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
           elseif (opstr(1:2).eq.'le') then
             if (volumelt .le. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
 
           else
             if (volumelt .eq. compval) then
               nfind= nfind+1
               totbyop(nfind) = it
             endif
           endif
         endif
 
      enddo
c     end count of volumes
 
C     Check that the volumes of the elements are not all the same
      ichk = .false.
      voltest=vol(mpary(1))
      do i=2,mpno
         it=mpary(i)
         if(abs(voltest-vol(it)).gt.epsilonvol) ichk = .true.
      enddo
 
      if(ichk) then
         volmin=1000000.
         volposmin=1000000.
         volmax=-1000000.
         do ii=1,mpno
            it=mpary(ii)
            if (vol(it) .lt. volmin) volmin=vol(it)
            if (vol(it) .lt. volposmin .and. vol(it) .gt. epsilonvol)
     *      then
               volposmin = vol(it)
            endif
            if (vol(it) .gt. volmax) volmax=vol(it)
         enddo
 
C  Check that there is a positive element
         if (volmax .gt. epsilonvol) then
            ifposvol = .true.
         else
            ifposvol = .false.
         endif
 
C  Fill bins and material arrays
         if (ifposvol .and. nbins.ne.0) then
 
C           Set counts to 0.
            do i=1,nbins
               neleints(i)=0
            enddo
 
C           Create bins
            if (iflogs) then
               logincrement=log10(abs(volmax)/abs(volposmin))
     *                      /float(nbins)
            else
               logincrement = (volmax-volmin)/float(nbins)
            endif
            endinters(1)=volposmin
            endinters(nbins+1)=volmax
            do i=2,nbins
               if (iflogs) then
                  endinters(i) = volposmin *
     *                           (10.0 ** (float(i-1)*logincrement))
               else
                  endinters(i) = volposmin + float(i-1)*logincrement
               endif
            enddo
 
C           Put counts into bins
C           Using simple linear search to find the appropriate bin
            do ii=1,mpno
               it=mpary(ii)
 
               if (vol(it) .gt. epsilonvol) then
                  i = 1
                  do while (vol(it).ge.endinters(i+1) .and.
     *                      i.lt.nbins)
                     i = i+1
                  enddo
                  if (i.gt.nbins .and. ifdebug) then
                     write(lgms ,'("i overflow!!!")')
                     call writloga('default',0,lgms ,0,ierrw)
                  endif
 
                  if ((vol(it).lt.endinters(i)      .or.
     *                 vol(it).gt.endinters(i+1)    .or.
     *                 (vol(it) .ge. endinters(i+1) .and.
     *                 i .lt. nbins)) .and. ifdebug)
     *            then
                     write(lgms ,'("bad logic in bins.")')
                     call writloga('default',0,lgms ,0,ierrw)
                     return
                  endif
                  neleints(i) = neleints(i) + 1
               endif
            enddo
         endif
c        end filling bins
 
         if (ifbins) then

         write (lgms ,"('---------------------------------------')")
         call writloga('default',0,lgms ,0,ierrw)
         if (ifbyclr .and. (ifbins)) then
           write (lgms ,"('  VOLUMES for ELEMENT COLOR ',i10)")
     *       iclr(icolor)
           call writloga('default',1,lgms ,0,ierrw)
           write (lgms ,"('...........................')")
           call writloga('default',0,lgms ,0,ierrw)
         endif
         endif
 
C  Write totals for element volumes
         if (ifposvol .and. ifbins) then
            do i=1,nbins
               write (lgms ,"('element volumes b/w ',e11.4,' and ',
     *                e11.4,':',i10)")
     *                endinters(i),endinters(i+1),neleints(i)
               call writloga('default',0,lgms ,0,ierrw)
            enddo
         endif
 
         if (nonposvol.gt.0 ) then
           write (lgms ,"('element volumes <= 0.0:',i10)") nonposvol
           call writloga('default',0,lgms ,0,ierrw)
           write (lgms ,"('element volumes <  0.0:',i10)") nonzero
           call writloga('default',0,lgms ,0,ierrw)
         endif
 
         if (nsmallvoleq.gt.0 ) then
           write (lgms ,"('element volumes <=  epsilonvol:',i10)")
     &         nsmallvoleq
           call writloga('default',0,lgms ,0,ierrw)
           write (lgms ,"('element volumes <   epsilonvol:',i10)")
     &         nsmallvol
           call writloga('default',0,lgms ,0,ierrw)
         endif
 
         write (lgms ,"('min volume = ',1pe15.7,
     *   '  max volume = ',1pe15.7)") volmin,volmax
         call writloga('default',0,lgms ,0,ierrw)
 
c     all volumes are same, ichk is false
      else
 
         if (ifbins) then
         write (lgms ,"('---------------------------------------')")
         call writloga('default',0,lgms ,0,ierrw)
         if (ifbyclr) then
           write (lgms ,"('  VOLUMES for ELEMENT COLOR ',i10)")
     *       iclr(icolor)
           call writloga('default',1,lgms ,0,ierrw)
           write (lgms ,"('...........................')")
           call writloga('default',0,lgms ,0,ierrw)
         endif

         write (lgms ,"('All elements have volume'
     *        ,1pe15.7)") vol(mpary(1))
         call writloga('default',0,lgms ,0,ierrw)
         endif
 
      endif
 
 50   continue
      endif
c     END volume
 
 
c.....option angle 

C     angle
C     Finds the max and min dihedral angles between adjacent
C     faces of a 3D element or edges of a 2D element in radians
C     If the compare  option is used, it displays a count of 
C     the number of elements with a dihedral angle that is 
C     greater than or less than the supplied value.

      if (ifangle) then
 
      minangle = 8.0
      maxangle = -8.0
 
      badcount = 0
      do i=1,nelmtypes
         badcountelm(i) = 0
      enddo
      anglevalrad = angleval * pi / 180.0
 
      do ii=1,mpno
         it=mpary(ii)
         ielmtyp = itettyp(it)
         do i=1,nelmnen(ielmtyp)
            i1 = itet(itetoff(it)+i)
            xice(i) = xic(i1)
            yice(i) = yic(i1)
            zice(i) = zic(i1)
         enddo
         call dihangle_element(ielmtyp,xice,yice,zice,
     *                         e_minangle,e_maxangle)
         if ((angleop .eq. 0 .and. e_minangle .lt. anglevalrad) .or.
     *       (angleop .eq. 1 .and. e_maxangle .gt. anglevalrad)) then
            badcount = badcount + 1
            badcountelm(ielmtyp) = badcountelm(ielmtyp) + 1
         endif
         if (e_minangle .lt. minangle) then
            minangle = e_minangle
         endif
         if (e_maxangle .gt. maxangle) then
            maxangle = e_maxangle
         endif
      enddo
 
      minangle = minangle * 180 / pi
      maxangle = maxangle * 180 / pi
C
C  Write total number of elements with bad angles
      write (lgms ,"('------------------------------------------')")
      call writloga('default',0,lgms ,0,ierrw)
      if (angleop .eq. 1) then
         write (lgms ,"('elements with angles > ', f7.3,': ',
     *                    i10)") angleval, badcount
      else if (angleop .eq. 0) then
         write (lgms ,"('elements with angles < ', f7.3,': ',
     *                    i10)") angleval, badcount
      endif
      call writloga('default',0,lgms ,0,ierrw)
      badcountname(ifelmtet) = 'tets'
      badcountname(ifelmpyr) = 'pyramids'
      badcountname(ifelmpri) = 'prisms'
      badcountname(ifelmhex) = 'hexes'
      badcountname(ifelmhyb) = 'hybrids'
      badcountname(ifelmply) = 'polygons'
      badcountname(ifelmtri) = 'triangles'
      badcountname(ifelmqud) = 'quads'
      do i=1,nelmtypes
         if (badcountelm(i).gt.0) then
            if (angleop .eq. 1) then
               write (lgms ,"(A, ' with angles > ',
     *              f7.3,': ',i10)")
     *              badcountname(i), angleval, badcountelm(i)
            else if (angleop .eq. 0) then
               write (lgms ,"(A, ' with angles < ',
     *              f7.3,': ',i10)")
     *              badcountname(i), angleval, badcountelm(i)
            endif
            call writloga('default',0,lgms ,0,ierrw)
         endif
      enddo
 
      write(lgms ,"('min angle = ',f7.3,
     *     '  max angle = ',f7.3)") minangle, maxangle
      call writloga('default',0,lgms ,0,ierrw)
 
      endif
c     END angle
 
c.....option taylor

      if (iftaylor) then

C     taylor / field/ value
C     see taylor_error() which generates an attribute with edges 
C     whose H1 seminorm of the error between a function with 
C     specified piecewise constant Hessian and the piecewise linear 
C     approximation over the tetrahedra is greater than badval.  
C     The attribute contains 10* element number + edge number.  
C     The name of the attribute is quality_taylor.  The length of 
C     this array is stored in  the attribute quality_taylor_len.
C     The seminorm functional is evaluated over the polyhedron
C     consisting of the union of tetrahedra sharing node NODE.
C     The functional being used
C     here is a generalization to 3D of the 2D analysis
C     of Bank and Smith.)
 
c     This needs to be checked, to see if can be called with
c     an eltset set of elements
c     for now, will use entire set of elements

C     Assume by here that field and value are set  

      if (mpno.ne.nelements) then
         write(lgms ,'(a)')'eltset not implemented for taylor option.'
         call writloga('default',0,lgms ,0,ierrw)
      endif
 
      call mmgetblk('iparent',isubname,ipiparent,npoints,1,ier)
      call cmo_get_info('isn1',cmo,ipisn,ilen,ityp,ier)
      call cmo_get_info('itp1',cmo,ipitp,ilen,ityp,ier)
 
      call unpackpc(npoints,itp,isn,iparent)
 
      call taylor_error(fieldname,taylorval,iparent)
 
      call cmo_get_info('quality_taylor_len',cmo,
     *                   t_errorlen,ilen,ityp,ier)
      if (ier .ne. 0) then
         write (lgms ,"('Couldnt get length')")
         call writloga('default',0,lgms ,0,ierrw)
      endif
      if (t_errorlen .gt. 0) then
         call cmo_get_info('quality_taylor',cmo,
     *                   ipt_errors,ilen,ityp,ier)
         if (ier .ne. 0) then
            write (lgms ,"('Couldnt get attribute')")
            call writloga('default',0,lgms ,0,ierrw)
         endif
      endif
 
c  Figure real numbadedges here
 
C
C  Write total number of edges with bad errors
      write (lgms ,"('------------------------------------------')")
      call writloga('default',0,lgms ,0,ierrw)
 
      write(lgms ,"('Num edges with Taylor error ',
     *                'exceeding abs(',f11.7,'):          ',i5)")
     *                taylorval, t_errorlen
      call writloga('default',0,lgms ,0,ierrw)
 
      write (lgms ,"('-------------------------------------------')")
      call writloga('default',0,lgms ,0,ierrw)
 
      cbuff='cmo/delatt//quality_taylor_len ; finish'
      call dotaskx3d(cbuff,ier)
      if (ier .ne. 0) then
         write (lgms ,"('Couldnt delete attribute length')")
         call writloga('default',0,lgms ,0,ierrw)
      endif
      if (t_errorlen .gt. 0) then
         cbuff='cmo/delatt//quality_taylor ; finish'
         call dotaskx3d(cbuff,ier)
         if (ier .ne. 0) then
            write (lgms ,"('Couldnt delete attribute')")
            call writloga('default',0,lgms ,0,ierrw)
         endif
      endif

c     END taylor

      elseif (ifquad) then
C
C        First, verify that we are working with a quad mesh.
C
         do i = 1, nelements
            if (itettyp(i) .ne. ifelmqud) then
                write (lgms, '(a,a)')'quality: Error - the quad mesh ',
     *              'must consist solely of quads.'
                call writloga('default',0,lgms,0,ierrw)
                goto 9998
            endif
         enddo

C
C        Generate the attributes we will display.
C
         write(cbuff, '(a,a,a)') 'cmo/addatt/', cmo,
     *      '/quad_quality/_quality _regularity _qflag; finish'
         call dotask(cbuff, ier)

         call cmo_get_info('_quality', cmo, ipqquality, ilen, ityp, ier)
         call cmo_get_info('_regularity', cmo, ipqregularity, ilen,
     *      ityp, ier)
         call cmo_get_info('_qflag', cmo, ipqflag, ilen, ityp, ier)

         nbins = 7
         quad_bin_ends(1) = 0.0
         quad_bin_ends(2) = 0.01
         quad_bin_ends(3) = 0.02
         quad_bin_ends(4) = 0.05
         quad_bin_ends(5) = 0.1
         quad_bin_ends(6) = 0.2
         quad_bin_ends(7) = 0.5
         quad_bin_ends(8) = 1.000001
C        We make the last end point slightly greater than one so that we
C        don't exclude any quads because of numerical inaccuracies.

C        Zero out the bins just to be safe.
         do i = 1, 7
            quad_bins(i) = 0
         enddo

C        Fill the bins
         do i = 1, nelements
C           Linear search. In the future this could be improved by
C           switching to binary search.
            do j = 1, 7
                if (qquality(i) .le. quad_bin_ends(j + 1)) then
                    goto 1000
                endif
            enddo
 1000       quad_bins(j) = quad_bins(j) + 1
         enddo

         write(lgms, '(a)')'              Quad Quality'
         call writloga('default',0,lgms,0,ierrw)
         write(lgms, '(a)')'------------------------------------------'
         call writloga('default',0,lgms,0,ierrw)

         do i = 1, 7
            write(lgms, '(f4.2,a,f4.2,a,i5)') quad_bin_ends(i), ' to ',
     *          quad_bin_ends(i+1), ': ', quad_bins(i)
            call writloga('default',0,lgms,0,ierrw)
         enddo
 
         write(lgms, '(a)')'------------------------------------------'
         call writloga('default',0,lgms,0,ierrw)
         write(lgms, '(a)')'              Quad Regularity'
         call writloga('default',0,lgms,0,ierrw)
         write(lgms, '(a)')'------------------------------------------'
         call writloga('default',0,lgms,0,ierrw)

C        Zero out the bins just to be safe.
         do i = 1, 7
            quad_bins(i) = 0
         enddo

C        Fill the bins
         do i = 1, nelements
C           Linear search. In the future this could be improved by
C           switching to binary search.
            do j = 1, 7
                if (qregularity(i) .le. quad_bin_ends(j + 1)) then
                    goto 1010
                endif
            enddo
 1010       quad_bins(j) = quad_bins(j) + 1
         enddo

         do i = 1, 7
            write(lgms, '(f4.2,a,f4.2,a,i5)') quad_bin_ends(i), ' to ',
     *          quad_bin_ends(i+1), ': ', quad_bins(i)
            call writloga('default',0,lgms,0,ierrw)
         enddo

         nnormal_quads = 0
         ndegenerate_quads = 0
         nwarped_quads = 0

         write(lgms, '(a)')'------------------------------------------'
         call writloga('default',0,lgms,0,ierrw)

         do i = 1, nelements
            if (qflag(i) .eq. 0) then
               nnormal_quads = nnormal_quads + 1
            elseif (qflag(i) .eq. 1) then
               ndegenerate_quads = ndegenerate_quads + 1
            elseif (qflag(i) .eq. 2) then
               nwarped_quads = nwarped_quads + 1
            endif
         enddo

         write(lgms, '(a20,i5)')'Normal quads:', nnormal_quads
         call writloga('default',0,lgms,0,ierrw)
         write(lgms, '(a20,i5)')'Degenerate quads:', ndegenerate_quads
         call writloga('default',0,lgms,0,ierrw)
         write(lgms, '(a20,i5)')'Warped quads:', nwarped_quads
         call writloga('default',0,lgms,0,ierrw)

      endif
C     END switch for option types
C***********************************************************************
      if (ifbyclr) then
        call writset('stat','tty','off',ierr1)
        call dotaskx3d('eltset/-etmp-/delete/ ; finish ',ier)
        if (chtmp(1:7).eq.'-etmp2-')
     >    call dotaskx3d('eltset/-etmp2-/delete/ ; finish ',ier)
        call writset('stat','tty','on',ierr1)
 
        if (ier.ne.0 .or. ierr1.ne.0) then
           write (lgms ,"('error during eltset delete -etmp-')")
           call writloga('default',0,lgms ,0,ierrw)
        endif
      endif
 
      icount=icount+mpno
 
      enddo
C     END loop through itetclr colors
      idone = .true.
 
C***********************************************************************
C***********************************************************************
 
C     Write volumes by material(itetclr) values
      if (ifbyclr) then
 
        write (lgms ,"('---------------------------------------')")
        call writloga('default',0,lgms ,0,ierrw)
        write(lgms ,'(a,a,i10,2x,i10)')
     >    '  SUMMARY VOLUME/AREA of ELEMENT COLORS for ',
     >    cmo(1:icharlnf(cmo)),istart,nclrs
        call writloga('default',1,lgms ,0,ierrw)
        write (lgms ,
     *"('...........................................................')")
        call writloga('default',0,lgms ,0,ierrw)
        write(lgms ,'(a,i10,2x,i10)')
     >    'Color      Num. Elements   Volume          Fractional Volume'
        call writloga('default',0,lgms ,0,ierrw)
 
        do ic = istart, nclrs, iinc
          write(lgms ,'(i5,2x,i10,9x,e14.7,2x,f14.9)')
     >    iclr(ic),totbyclr(iclr(ic)),sumbyclr(iclr(ic)),
     >    sumbyclr(iclr(ic))/volsum
          call writloga('default',0,lgms ,0,ierrw)
        enddo
 
        if (nclrs .ne. 1) then
          write(lgms ,'(a,2x,i10,9x,a,e14.7)')
     >    ' Total elements: ',icount,
     >    ' Total Volume: '  ,volsum
          call writloga('default',0,lgms ,0,ierrw)
        endif
 
      endif
c     end write for material volumes
 
c     report on compare
      if (ifcompare) then
        write (lgms ,"('Found ',i10,' elements ',a2,' ',f14.9)")
     >         nfind,opstr,compval
        call writloga('default',1,lgms ,0,ierrw)
        if(idebug.ge.1) then
          print*,(totbyop(i),i=1,nfind)
        endif
      endif
 
      ilast = mpary(mpno)
      if (ifbins) then
        write (lgms ,
     *"('-----------------------------------------------------------')")
        call writloga('default',0,lgms ,0,ierrw)
      endif
 
c     report on total elements
      if (ifeltset) then
        write (lgms ,'(i10,a,a)')
     >         icount,' total elements evaluated in eltset ',ch3
        call writloga('default',0,lgms ,1,ierrw)
      else
        write (lgms ,'(i10,a)')icount,' total elements evaluated.'
        call writloga('default',0,lgms ,1,ierrw)
      endif
 
 9998 call mmrelprt(isubname,ier)

      if (.not.idone .and. ifbyclr) then
        call writset('stat','tty','off',ierr1)
        call dotaskx3d('eltset/-etmp-/delete/ ; finish ',ier)
        if (chtmp(1:6).eq.'etmp2')
     >    call dotaskx3d('eltset/-etmp2-/delete/ ; finish ',ier)
        call writset('stat','tty','on',ierr1)
      endif
 
 9999 return
      end
