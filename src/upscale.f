
      subroutine upscale(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C      Take a fine grid and upscale (interpolate) the attributes
C      of a fine grid (cmosrc) onto the course grid (cmo current).
C
C      The subroutine finds every node of the fine mesh within the
C      vornoi cell of every node in the course mesh.  Fine nodes on
C      cell boundaries are assigned to two or more course nodes.  Then the
C      attributes of all the fine cells within a course node's cell are
C      averaged.
C
C     SYNTAX -
C      NOTE:  the old command is 
C      upscale/
C      avetype/attsink/1,0,0/cmosrc/attsrc
C      sink and source attributes assumed to be nnodes VDOUBLE
C      current MO assumed to be sink object
C
C
C      upscale /
C      scale_method/cmosink,attsink/1,0,0/cmosrc,attsrc/ [keepatt][set_id][method_options]
C
C      scale_method     
C                ariave  - arithmatic average
C                      sink_val = (x(1) + x(i)... + x(n)) / n
C                      for 4 values; 1,2,3,4 ariave = 2.5
C                geoave  - geometric average
C                      sink_val = ( x(1) * x(i)... * x(n) )**(1/n)
C                      for 4 values; 1,2,3,4 geoave = 2.21336 
C                harave  - harmonic average
C                      sink_val = n / ( 1/x(1) + 1/x(i)... + 1/x(n) )
C                      for 4 values; 1,2,3,4 harave = 1.92 
C                sum - sum of source node attributes
C                      sink_val = x(1) + x(i)... + x(n)
C                      for 4 values; 1,2,3,4 sum = 10 
C                min - choice of singl node attribute value
C                      sink_val = min(x(1),x(i),x(n))
C                      for 4 values; 1,2,3,4 min = 1
C                max - choice of singl node attribute value
C                      sink_val = max(x(1),x(i),x(n))
C                      for 4 values; 1,2,3,4 max = 4 
C
C      KEEPATT   will keep added attributes and pt_gtg
C      set_id    will create source attribute pt_gtg and force a redo if exists 
C
C      options for sum
C                - default, all nodes in Voronoi volume used 
C                  regardless of nodes sharing Voronoi boundaries with others 
C                - single, use node values once, then not again
C                  so that each Voronoi volume has single source sets
C                - divide, shared bndry values are detirmined and are
C                  divided by the number of voronoi volumes they occupy              
C      
C     EXAMPLE:
C       cmo / select / cmoquad
C       upscale/ ariave / xval /1,0,0/ cmopt s/ tval
C
C       This will compute arithmatic average on tval for all 
C       nodes from CMO cmopt that are within each vornoi cell in 
C       CMO cmoquad. 
C       Values will be written to attribute xval in cmoquad. 
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
C $Log: upscale.f,v $
C Revision 2.00  2007/11/09 20:04:05  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   18 Jun 2007 07:18:02   tam
CPVCS    better logic regarding search and keepatt option
CPVCS    better reporting regarding setup and options
CPVCS    
CPVCS       Rev 1.4   13 Jun 2007 07:13:10   tam
CPVCS    new version adds options sum, min, and max
CPVCS    syntax and code updated to mirror interpolate
CPVCS    debug and error checking added
CPVCS    option single added to conserve number of nodes used
CPVCS    so nodes sharing voronoi boundaries are used only once
CPVCS    this version is runs fine on test, but needs more work
CPVCS    
CPVCS       Rev 1.3   Thu Apr 06 14:28:24 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.2   Tue Sep 22 13:45:36 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.1   Fri Jun 20 16:01:36 1997   dcg
CPVCS    separate out kdtree0 and nearestpoint0
C
C
C ######################################################################
C
      implicit none
      
      integer nplen, ntlen
      parameter (nplen=1000000)
      parameter (ntlen=1000000)
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character*132 logmess, cbuff
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(nplen), yic(nplen), zic(nplen)
C
      character*32 ich1,ich2,ich3
      character*32 isubname
      character*32 intrp_opt, sum_opt, keep_opt, set_opt
      character*32  blkname, cmosnk, cmosrc
      character*32  attsnk, attsrc, cpers, cio
      character*32  crank_snk,cinter_snk,ctype_snk,clen_snk
      character*32  crank_src,cinter_src,ctype_src,clen_src
      character*132 cbuf
      real*8 alargenumber
      data alargenumber/1.d99/
C
C     xfield (ifield) is source, xout (iout) is sink 
C     ptmp is for checking attributes
C     xsource is for assiging mmgetblk to VINT or VDOUBLE
      pointer(iptmp, ptmp)
      pointer(ipxout, xout)
      pointer(ipxfield, xfield)
      pointer(ipxsource, xsource)
      real*8 ptmp(nplen),xout(nplen),xfield(nplen),xsource(nplen)

C     integer pointers for VINT attributes
      pointer(ipiout, iout)
      pointer(ipifield, ifield)
      integer iout(nplen), ifield(nplen)

C     may not need element pointers
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      integer itettyp(nplen),itet(nplen),itetoff(nplen)

C     value and vor_bndry are accumulated for sink
      pointer (ipvalue, value)
      pointer (ipvor_bndry, vor_bndry)
      real*8 value(nplen),vor_bndry(nplen)
C
      pointer (ipxic_src, xic_src)
      pointer (ipyic_src, yic_src)
      pointer (ipzic_src, zic_src)
      real*8  xic_src(nplen), yic_src(nplen), zic_src(nplen)

      pointer (ipisetwd, isetwd)
      pointer (ipitp1, itp1)
      integer isetwd(nplen), itp1(nplen)

      pointer (ipdistpossleaf, distpossleaf)
      real*8 distpossleaf(nplen)

      pointer (ipnneigh, nneigh)
      pointer(ipmpary, mpary)
      pointer (ipsbox, sbox)
      pointer (iplinkt, linkt)
      pointer (ipitfound, itfound)
      integer nneigh(nplen),mpary(nplen),
     * sbox(nplen),linkt(nplen),itfound(nplen)

C     counters for source nodes in sink voronoi volumes 
      pointer (ipnnum, nnum)
      pointer (ipndups, ndups)
      pointer (ipnbndry, nbndry)
      integer nnum(nplen),ndups(nplen),nbndry(nplen)

C     optional SOURCE mesh object attributes
C     pt_gtg has assigned node id 
C     dups_gtg are in duplicate Voronoi boundaries 
      pointer (ippt_gtg, pt_gtg)
      pointer (ipdups_gtg, dups_gtg)
      integer  pt_gtg(nplen), dups_gtg(nplen)

      integer iisrc,ntets,iisnk, ikeep, set_id,
     * nsdtopo, nsdgeom, nsdtopo_src, nsdgeom_src, 
     * length, length_snk, length_src, 
     * npoints, npoints_src, nwd_pset,
     * nen_src,nef_src

      integer ipt,iwrite,nwrite,iperc,totsrchd,totfind,istep,
     * totflag,idone,idebug,icntsink,icntsrc,mbndry,idx,
     * ndups_tot,ipt_exist,idups_exist,if_search,mk_ptatt

      integer j,i,ii,iff,ipt1,ipt2,ipt3,mpno,len,
     * ipointi,ipointj,ilen,ityp, mtfound,inxt,
     * ics,ics2,ics3, ierr,ierrw,ntype,icmotype


      integer bndrypt,bndryid,ipt_gtg,idups_gtg

      integer icharlnf

      real*8 xperc, maxval,minval,testval
      real*8 xq,yq,zq,xs,ys,zs,eps
C     real*8 xdist,ydist,zdist,dis,dmin

C
C#######################################################################
C
C
C
      isubname='upscale'
C
      ierror = 0
      idebug=0
      ndups_tot=0
      keep_opt = "delatt" 
      sum_opt = "multiple" 
      set_opt = "set_id" 
      set_id = 0
      mk_ptatt = 1
      if_search = 1
      ikeep = 0
C
C     ******************************************************************
C     PARSE ARGUMENTS 
C     scale_method/cmosnk,attsink/1,0,0/cmosrc,attsrc/ [keepatt] [method_options]
C            2      3       4     5 6  7     8     9        
C     intrp_opt/attsink     /i j k    /cmosrc/attsrc 

C     BAD SYNTAX
      if (nwds.lt.7) then
        write(logmess,'(a)') 
     *   'UPSCALE/scale_opt/cmosnk,attsink/1,0,0/cmosrc,attsrc'
        call writloga('default',0,logmess,1,ierrw)
        ierror = -1
        goto 9999

C     OLD SYNTAX uses current mesh object as sink
      elseif (nwds.le.8) then
        call cmo_get_name(cmosnk,ierror)
        if(ierror.ne.0) then
          write(logmess,'(a)') 'Current mesh object not found.'
          call writloga('default',0,logmess,0,ierrw)
          goto 9999
        endif
        intrp_opt=cmsgin(2)
        attsnk=cmsgin(3)
        cmosrc=cmsgin(7)(1:icharlnf(cmsgin(7)))
        if(nwds.lt.8) then
           attsrc=attsnk
        else
           attsrc=cmsgin(8)(1:icharlnf(cmsgin(8)))
        endif
        nwd_pset=4
      else

C     NEW SYNTAX similar to interoplate
        intrp_opt=cmsgin(2)
        cmosnk=cmsgin(3)
        attsnk=cmsgin(4)
        nwd_pset=5
        cmosrc=cmsgin(8)(1:icharlnf(cmsgin(8)))
        attsrc=cmsgin(9)(1:icharlnf(cmsgin(9)))


C     Loop through remaining optional command tokens
C     The first 9 are neccessary, the remainder are not
C     token ordering for remainder is ignored
C     i increments through each of the remainder tokens
C     inxt increments past i if a token is paired with a value
C       keepatt - attributes not deleted
C       set_id
C       sum_opt = single, divide, none

      inxt = 10
      do i = 10,nwds
        inxt = max(i,inxt)
        if(msgtype(inxt).eq.3) then
          if((cmsgin(inxt)(1:4).eq.'keep') .or.
     *      (cmsgin(inxt)(1:4).eq.'KEEP')) then
            ikeep = 1
            keep_opt="keepatt"
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'delatt') .or.
     *      (cmsgin(inxt)(1:6).eq.'DELATT')) then
            ikeep = 0
            keep_opt="delatt"
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'single') .or.
     *      (cmsgin(inxt)(1:6).eq.'SINGLE')) then
            sum_opt = 'single' 
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'divide') .or.
     *      (cmsgin(inxt)(1:6).eq.'DIVIDE')) then
C           NOT IMPLEMENTED YET
C           sum_opt = 'divide' 
            sum_opt = 'none' 
            inxt = inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'set_id') .or.
     *           (cmsgin(inxt)(1:6).eq.'SET_ID')) then 
            set_id = 1 
            set_opt="set_id"
            inxt=inxt+1
          endif
        endif
      enddo
      if (ikeep.eq.1 .and. set_id.eq.0) set_opt="  "
      endif
C     End command processing - Done with message arrays

C     Check the mesh object names
      call cmo_exist(cmosnk,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a)')
     *     'UPSCALE: Not a valid mesh object: ',cmosnk
         call writloga('default',1,logmess,1,ierrw)
         goto 9000
      endif

      call cmo_exist(cmosrc,ics)
      if(ics.ne.0) then
         write(logmess,'(a,a)')
     *   'UPSCALE: Not a valid mesh object: ',cmosrc
         call writloga('default',1,logmess,1,ierrw)
         goto 9000
      endif

C     Check mesh object attributes with temporary pointer iptmp
      len=icharlnf(attsrc)
      if(len.eq.3) then
        if(attsrc(1:3).eq.'itp') attsrc='itp1'
        if(attsrc(1:3).eq.'imt') attsrc='imt1'
        if(attsrc(1:3).eq.'icr') attsrc='icr1'
        if(attsrc(1:3).eq.'isn') attsrc='isn1'
      endif
      len=icharlnf(attsnk)
      if(len.eq.3) then
        if(attsnk(1:3).eq.'itp') attsnk='itp1'
        if(attsnk(1:3).eq.'imt') attsnk='imt1'
        if(attsnk(1:3).eq.'icr') attsnk='icr1'
        if(attsnk(1:3).eq.'isn') attsnk='isn1'
      endif
      call mmfindbk(attsrc,cmosrc,iptmp,ilen,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *      '  cmo= ',cmosrc(1:icharlnf(cmosrc)),
     *      '  att= ',attsrc(1:icharlnf(attsrc))
         call writloga('default',0,logmess,0,ierrw)

         call mmfindbk(attsnk,cmosnk,iptmp,ilen,ics)
         if(ics.ne.0) then
           write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *        '  cmo= ',cmosnk(1:icharlnf(cmosnk)),
     *        '  att= ',attsnk(1:icharlnf(attsnk))
           call writloga('default',0,logmess,0,ierrw)
         endif
         if (ierr.ne.0 .or. ics.ne.0) goto 9000
      endif


C     DETERMINE SCALE METHOD TYPE 
      ntype=0
      if (intrp_opt(1:6).eq.'harave') then 
         ntype=1
      elseif (intrp_opt(1:6).eq.'ariave') then  
         ntype=2
      elseif (intrp_opt(1:6).eq.'geoave') then  
         ntype=3
      elseif (intrp_opt(1:3).eq.'sum') then  
         ntype=4
      elseif (intrp_opt(1:3).eq.'max') then  
         ntype=5
      elseif (intrp_opt(1:3).eq.'min') then  
         ntype=6
      endif

      if (ntype.eq.0) then
         ierror=1
         write(logmess,6000) intrp_opt
 6000    format('  ERROR - INVALID UPSCALE TYPE ',a8)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif

C     ******************************************************************
C     GET INFO COARSE SINK CMO and ATTRIBUTE 

      call cmo_select(cmosnk,ierr)
      call cmo_get_intinfo('nnodes',cmosnk,npoints,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosink')
      call cmo_get_intinfo('nelements',cmosnk,ntets,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements cmosink')
      length_snk = npoints
      if (ierr.ne.0) goto 9000

      call cmo_get_info('idebug',cmosnk,idebug,len,ityp,ierr)

      call cmo_get_intinfo('mbndry',cmosnk,mbndry,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo mbndry cmosink')
      if (ierr.ne.0) goto 9000
      call cmo_get_intinfo('ndimensions_geom',cmosnk,
     *                      nsdgeom,ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmosnk,
     *                      nsdtopo,ilen,ityp,ierr)

      call cmo_get_info('xic',cmosnk,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmosnk,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmosnk,ipzic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xyz cmosink')
      call cmo_get_info('itettyp',cmosnk,ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmosnk,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmosnk,ipitet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itet cmosink')
      if (ierr.ne.0) goto 9000

      call cmo_get_info('isetwd',cmosnk,ipisetwd,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info isetwd cmosink')
      call cmo_get_info('itp1',cmosnk,ipitp1,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itp1 cmosink')
      if (ierr.ne.0) goto 9000

      call cmo_get_info('ipointi',cmosnk,ipointi,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'ipointi cmosink')
      call cmo_get_info('ipointj',cmosnk,ipointj,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'ipointj cmosink')
      if (ierr.ne.0) goto 9000
C
      if(ipointj.eq.0) ipointj=npoints
      if(ipointj.gt.npoints) ipointj=npoints

c     Get sink attribute info and assign attribute length
      call cmo_get_attparam(attsnk,cmosnk,idx,ctype_snk,crank_snk,
     *    clen_snk,cinter_snk,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam sink')

C     Assign sink attribute to xout or iout array
C     INT attribute will be converted and copied to xout
      length=npoints
      len=icharlnf(attsnk)
      blkname=' '
      blkname(1:len)=attsnk
      if(ctype_snk(1:4).eq.'VINT') then
        call mmgetpr(blkname,cmosnk,ipiout,ics)
        call mmgetlen(ipiout,length_snk,ics2)
        if(ics.ne.0 .or. ics2.ne.0) then
          call x3d_error(isubname,'mmgetpr sink attribute')
          goto 9000
        endif
      elseif(ctype_snk(1:7).eq.'VDOUBLE') then
        call mmgetpr(blkname,cmosnk,ipxout,ics)
        call mmgetlen(ipxout,length_snk,ics2)
        if(ics.ne.0 .or. ics2.ne.0) then
          call x3d_error(isubname,'mmgetpr sink attribute')
          goto 9000
        endif
      else
        write(logmess,'(a,a,a,a)')
     *  'Invalid attribute type for ',cmosnk(1:icharlnf(cmosnk)),
     *   ' ',blkname(1:len)
         call writloga('default',1,logmess,0,ierrw)
         goto 9000
      endif

C     
C     ******************************************************************
C     set the point index boundaries for sink points
      length=length_snk
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(nwd_pset).eq.1) then
         ipt1=imsgin(nwd_pset)
         ipt2=imsgin(nwd_pset+1)
         ipt3=imsgin(nwd_pset+2)
      else
         ich1=cmsgin(nwd_pset)
         ich2=cmsgin(nwd_pset+1)
         ich3=cmsgin(nwd_pset+2)
      endif

c     For node point set
      length=npoints
      call mmgetblk('mpary',isubname,ipmpary,length,2,ics)
      if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
      if (ics.ne.0) goto 9000

      if(msgtype(nwd_pset).eq.1) then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
      if (mpno.ne.0) then
        if (mpno.ne.npoints) then
           write(logmess,'(a,i10)')
     *      'nodes in selected point set  = ',mpno
           call writloga('default',0,logmess,0,ierrw)
         endif
      else
           write(logmess,'(a)') 'No points in selected point set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9000
      endif

C     ******************************************************************
C     GET INFO FINE SOURCE CMO and ATTRIBUTE 
C
      call cmo_select(cmosrc,ierr)
      call cmo_get_intinfo('nnodes',cmosrc,npoints_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosource')
      call cmo_get_intinfo('ndimensions_geom',cmosrc,
     *                      nsdgeom_src,ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmosrc,
     *                      nsdtopo_src,ilen,ityp,ierr)
      if (ierr.ne.0) goto 9000

      call cmo_get_info('xic',cmosrc,ipxic_src,ilen,ityp,ierr)
      call cmo_get_info('yic',cmosrc,ipyic_src,ilen,ityp,ierr)
      call cmo_get_info('zic',cmosrc,ipzic_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xyz cmosource')
      if (ierr.ne.0) goto 9000

c     get source attribute info and assign attribute length
      call cmo_get_attparam(attsrc,cmosrc,idx,ctype_src,crank_src,
     *    clen_src,cinter_src,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam source')
      if (ierr.ne.0) goto 9000
      if (clen_src(1:5).eq.'nnode') then
         length_src = npoints_src
      else
        write(logmess,'(a)')"Error: Source cmo must use node attribute."
        call writloga('default',0,logmess,0,ierrw)
        goto 9000 
      endif

C     Assign source attribute to xfield or ifield array
C     INT attribute will be converted and copied to xfield
      length=npoints_src
      len=icharlnf(attsrc)
      blkname=' '
      blkname(1:len)=attsrc
      if(len.eq.3) then
        if(blkname(1:3).eq.'itp') blkname='itp1'
        if(blkname(1:3).eq.'imt') blkname='imt1'
        if(blkname(1:3).eq.'icr') blkname='icr1'
        if(blkname(1:3).eq.'isn') blkname='isn1'
        len=icharlnf(blkname)
      endif
      if(ctype_src(1:4).eq.'VINT') then
        call mmgetpr(blkname,cmosrc,ipifield,ics)
        call mmgetlen(ipifield,length_src,ics2)
        call mmgetblk('xsource',isubname,ipxsource,length,2,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk xsource')
        ipxfield = ipxsource
      elseif(ctype_src(1:7).eq.'VDOUBLE') then
        call mmgetpr(blkname,cmosrc,ipxfield,ics)
        call mmgetlen(ipxfield,length_src,ics2)
        if(ics.ne.0) call x3d_error(isubname,'mmgetpr xfield')
      else
        write(logmess,'(a,a,a,a)')
     *  'Invalid attribute type for ',cmosrc(1:icharlnf(cmosrc)),
     *   ' ',blkname(1:len)
         call writloga('default',1,logmess,0,ierrw)
         goto 9000
      endif
      if (ics.ne.0) goto 9000

C     check to see if the two cmo's have the same geometric dimension
C     old code stopped here, allow to continue 
      if( nsdgeom .ne. nsdgeom_src) then
         write(logmess,'(a)')"WARNING: cmo's need same dimension"
         call writloga('default',0,logmess,0,ierrw)
      endif
 
C     ******************************************************************
C     WORK ARRAYS

C        allocate SINK work arrays
         ilen=npoints
         call mmgetblk('value',isubname,ipvalue,ilen,2,ics2)
         call mmgetblk('nnum',isubname,ipnnum ,ilen,1,ics)
         if(ics.ne.0 .or. ics2.ne.0) then
            call x3d_error(isubname,'mmgetblk work arrays for sink')
            goto 9000
         endif

C        special arrays for sum
         call mmgetblk('vor_bndry',isubname,ipvor_bndry,ilen,2,ics)
         call mmgetblk('ndups',isubname,ipndups ,ilen,1,ics2)
         call mmgetblk('nbndry',isubname,ipnbndry ,ilen,1,ics3)
         if(ics.ne.0 .or. ics2.ne.0 .or. ics3.ne.0) then
            call x3d_error(isubname,'mmgetblk work arrays for sum')
            goto 9000
         endif

C        TAM - nneigh not being used
C        allocate SOURCE work arrays
C        length=npoints_src
C        call mmgetblk('nneigh',isubname,ipnneigh,length,1,ics)
C        if(ics.ne.0 ) then
C           call x3d_error(isubname,'mmgetblk work arrays for nneigh')
C           goto 9000
C        endif

C     ******************************************************************
C     add attributes to SOURCE cmo to track nodes used for sink Voronoi
C     pt_gtg fills source points with corresponding sink node id 
C     dups_gtg tags nodes in multiple sink Voronoi volumes 

C     pt_gtg will use kdtree search loop to fill source points with
C     associated sink node points
C     Note, this is a one-to-one correlation and does not handle
C     duplicate nodes in shared Voronoi volumes
      call mmgetpr('pt_gtg',cmosrc,ippt_gtg,ipt_exist)
      if (ipt_exist.eq.0) then
        call cmo_get_info('pt_gtg',cmosrc,ippt_gtg,ilen,ityp,ics)
        if(ics.ne.0) then
          write(logmess,'(a,a,a,a)')
     *    'upscale attribute does not exist: ',
     *    '  cmo= ',cmosrc(1:icharlnf(cmosrc)),'  att= pt_gtg '
          call writloga('default',1,logmess,1,ierrw)
          goto 9000
        endif
        if_search = 0
        ipt_exist = 1
      else
        if_search = 1
        ipt_exist = 0
      endif
C     force removal of pt_gtg and fill again
      if (ipt_exist.eq.1 .and. set_id.eq.1) then
        len=icharlnf(cmosrc)
        cbuff = 'cmo DELATT '//cmosrc(1:len)//'/pt_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
        ipt_exist=0
      endif 

C     These are flagged counts for duplicate nodes on cell boundaries
C     if set_id, force removal of dups_gtg and fill again
      call mmgetpr('dups_gtg',cmosrc,ipdups_gtg,idups_exist)
      if (idups_exist.eq.0) then
        call cmo_get_info('dups_gtg',cmosrc,ipdups_gtg,ilen,ityp,ics)
        if(ics.ne.0) then
          write(logmess,'(a,a,a,a)')
     *    'upscale attribute does not exist: ',
     *    '  cmo= ',cmosrc(1:icharlnf(cmosrc)),'  att= dups_gtg '
          call writloga('default',1,logmess,1,ierrw)
          goto 9000
        endif
        idups_exist = 1
      else
        idups_exist = 0
      endif
C     force removal of dups_gtg and fill again
      if (idups_exist.eq.1 .and. set_id.eq.1) then
        len=icharlnf(cmosrc)
        cbuff = 'cmo DELATT '//cmosrc(1:len)//'/dups_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
        idups_exist=0
      endif

C     add attributes if not exist
      if (ipt_exist.eq.0) then
        cbuff = 'cmo/addatt/'//cmosrc(1:icharlnf(cmosrc))//
     *  '/pt_gtg/' //
     *  'VINT/scalar/nnodes//permanent/agfx/0 ; finish'
        call dotaskx3d(cbuff,ierr)
        if(ierr.ne.0) then
          call x3d_error(isubname,'make att pt_gtg')
          goto 9000
        endif
        call mmgetpr('pt_gtg',cmosrc,ippt_gtg,ics)
        ipt_exist = 1
        if_search = 1
      endif
      if (idups_exist.eq.0) then
        cbuff = 'cmo/addatt/'//cmosrc(1:icharlnf(cmosrc))//
     *  '/dups_gtg/' //
     *  'VINT/scalar/nnodes//permanent/agfx/0 ; finish'
        call dotaskx3d(cbuff,ierr)
        if(ierr.ne.0) then
          call x3d_error(isubname,'make att dups_gtg')
          goto 9000
        endif
        call mmgetpr('dups_gtg',cmosrc,ipdups_gtg,ics)
        idups_exist = 1
      endif

C     report attribute status for source cmo being kept and set
      if (ikeep.eq.1 .and. if_search.eq.1) then
        write(logmess,'(a)')
     * 'pt_gtg attribute added to source cmo.'
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     * 'nodes on Voronoi boundaries used by multiple volumes.'
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     * 'dups_gtg attribute added to source cmo.'
        call writloga('default',0,logmess,0,ierrw)
      endif

C     report attribute status for source cmo being kept and already set
      if (ikeep.eq.1 .and. if_search.eq.0) then
        sum_opt = 'single'
        write(logmess,'(a)')
     * 'nodes on Voronoi boundaries set to a single source volume.'
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     * 'pt_gtg being used for sink node id numbers.'
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     * 'dups_gtg being used for nodes on duplicate cell boundaries.'
        call writloga('default',0,logmess,0,ierrw)
      endif


C     GET POINTERS for added attributes 
      call cmo_get_info('pt_gtg',cmosrc,ippt_gtg,ilen,ityp,ierr)
      if (ierr.ne.0) then
        write(logmess,"(a,a)")
     * 'WARNING: could not addatt pt_gtg for ',cmosrc
        call writloga('default',0,logmess,0,ierrw)
        ipt_exist=0
      else
        ipt_exist=1
      endif
      call cmo_get_info('dups_gtg',cmosrc,ipdups_gtg,ilen,ityp,ierr)
      if (ierr.ne.0) then
        write(logmess,"(a,a)")
     * 'WARNING: could not addatt dups_gtg for ',cmosrc
        call writloga('default',0,logmess,0,ierrw)
        idups_exist=0
      else
        idups_exist=1
      endif

C     ******************************************************************
c     If source attribute is INT, convert and copy into xfield
      if(ctype_src(1:4).eq.'VINT') then
         do i=1,length_src
            xfield(i) = dble(ifield(i))
         enddo
      endif

C     ******************************************************************
C     Done with setup and error checking, start work for upscale
c     Write summary of interpolation setup and type

      write(logmess,'(a,a10,a,a10)')
     * 'UPSCALE METHOD: ',intrp_opt(1:icharlnf(intrp_opt))
      call writloga('default',1,logmess,0,ierrw)

      write(logmess,'(a, a10,a10,a10)')
     * '       options: ',keep_opt(1:icharlnf(keep_opt)),
     *  sum_opt(1:icharlnf(sum_opt)),
     *  set_opt(1:icharlnf(set_opt))
      call writloga('default',0,logmess,0,ierrw)

      write(logmess,'(2x,i10,a,a,a,a)')
     * npoints,'  Sink Nodes of ',attsnk(1:icharlnf(attsnk)),
     * ' in course mesh: ',cmosnk(1:icharlnf(cmosnk))
      call writloga('default',0,logmess,0,ierrw)
      if (mpno.ne.npoints) then
        write(logmess,'(2x,i10,a)')
     *   mpno,' Selected Set of Nodes will be written ' 
      call writloga('default',0,logmess,0,ierrw)
      endif 

      write(logmess,'(2x,i10,a,a,a,a)')
     * npoints_src,'  Source Nodes of ',attsrc(1:icharlnf(attsrc)),
     * ' in fine mesh: ',cmosrc(1:icharlnf(cmosrc))
      call writloga('default',0,logmess,0,ierrw)


C     ******************************************************************
C     INITIALIZE WORK ARRAYS and VARIABLES 
       length=5*npoints_src
       call mmgetblk('itfound',isubname,ipitfound,length,1,ics)
       if(ics.ne.0 ) call x3d_error(isubname,' get itfound')

C  move these into loop where pt_gtg may be used instead    
C      length=12*npoints
C      call mmgetblk('sbox',isubname,ipsbox,length,2,ics)
C      length=2*npoints
C      call mmgetblk('linkt',isubname,iplinkt,length,1,ics)
 
c      initialize work arrays for sink 
       do ii=1,npoints
         value(ii)=0.
         nnum(ii)=0
       enddo

c      variables for progress reporting
       iwrite=0
       ipt=0
       istep=1
       totsrchd = 0
       totfind = 0
       totflag = 0
       if ( max(npoints,npoints_src) .gt. 500000 ) then
         xperc = 50.
         iperc = 2
       elseif ( max(npoints,npoints_src) .gt. 10000 ) then
         xperc = 20.
         iperc = 5
       else
         xperc = 4.
         iperc = 25
       endif
       nwrite=nint(dble(npoints_src)/ xperc )

C     ******************************************************************
C     SEARCH FOR NEAREST SOURCE NODES
C .... Use A. kuprat's kdtree routines here instead of local search (below)
C      
c      fill coordinates with current point 
c      TAM note: do we really want to assign zs by checking nsdgeom?
C     Build the search tree for nearest points

      
c       Build kdtree to search for nearest source points
        if (if_search.eq.0) then
          write(logmess,"(a)")
     *    'SKIPPING POINT SEARCH... using lookup attribute pt_gtg'
          call writloga('default',0,logmess,0,ierrw)

        else
          length=12*npoints
          call mmgetblk('sbox',isubname,ipsbox,length,2,ierr)
          length=2*npoints
          call mmgetblk('linkt',isubname,iplinkt,length,1,ics)
          if(ierr.ne.0 .or. ics.ne.0)
     *       call x3d_error(isubname,' get linkt and sbox')
          call kdtree0(xic,yic,zic,npoints,linkt,sbox,ierr)
          if(ierr.ne.0 ) call x3d_error(isubname,' kdtree0 ')
          eps=-1.
          mtfound=0
          xs=alargenumber
          ys=alargenumber
          zs=0.
          if(nsdgeom_src.eq.3) zs=alargenumber
        endif


C#######################################################################
C         INTEROPLATE FOUND NODES ON TO THE SOURCE MESH NODES
C         ntype
C         1 harave  - harmonic average
C         2 ariave  - arithmatic average
C         3 geoave  - geometric average
C         4 sum     - sum the values
C         5 max     - assign max value 
C         6 min     - assign min value 
C
C      FOR EACH SOURCE NODE
C           LOOP THROUGH SINK POINTS 
C           COMPUTE NODES FOR FOUND VORONOI VOLUME 
C
       length=npoints
       call mmgetblk('distpossleaf',isubname,ipdistpossleaf,
     * length,2,ierr)

       icntsrc = 0
       iisnk=0
       do ii=1,npoints_src
            if(if_search.eq.1) pt_gtg(ii)=-1
            if(if_search.eq.1) dups_gtg(ii)=0
            ipt=ii
            xq=xic_src(ii)
            yq=yic_src(ii)
            zq=zic_src(ii)

C          Find points within Voronoi volume of sink point
c          xs,ys,zs  - spatial coordinates of PREVIOUS nearest point
c          eps  -    epsilon for length comparisons
c          mtfound - number of points  returned
c          itfound - array of found points 
c          linkt,sbox -   k-D tree arrays

c          Get nearest point number iisnk from attribute pt_gtg
           if(if_search.eq.0) then
              iisnk=pt_gtg(ipt)
              if(iisnk.gt.0) then
                mtfound = 1
                itfound(1) = iisnk
              else
                mtfound = -1
              endif
           else

             call nearestpoint1(xq,yq,zq,xs,ys,zs,linkt,sbox,eps,
     *       npoints,distpossleaf,mtfound,itfound,ierr)

           if(ierr.ne.0 ) call x3d_error(isubname,' nearestpoint1')

           endif
           if (mtfound.gt.0) then
              totfind=totfind+mtfound
           else
              totflag=totflag+1
           endif
            
C           FOR EACH SOURCE NODE FOUND
C                LOOP THROUGH SINK POINTS WITHIN VORONOI VOLUME 
c           do the various types of averaging
c           ......iisnk is node id in the course sink mesh
c           ..... iisrc is node id in the source fine mesh
c           ..... nnum(iisnk) count of source points for each sink point 
c           ..... value(iisnk) holds calculated value so far 
c           ..... xfield(iisrc) is the source attribute 
c           ..... dups_gtg(iisrc) counts duplicate Voronoi source nodes 

            do iff=1,mtfound
              iisnk=itfound(iff)
              iisrc=ii

C             IF SEARCH IS TRUE
C             keep track of source nodes used for each sink node
C             count source nodes in multiple Voronoi volumes
C             pt_gtg will have the value of node first used
C             ...nbndry() are the number of shared voronoi boundaries
C             ...ndups() are the number of times node is used 
C             ...bndryid is the id number of sink volume already set 
C             ...bndrypt is the id number of current source node
              bndrypt=0

C             point id already is assigned for this source node
              if (pt_gtg(iisrc).gt.0) then
                if( if_search.eq.1) then  
                  bndrypt=iisrc
                  bndryid=pt_gtg(iisrc)
                  nbndry(iisnk)= max(dups_gtg(iisrc),nbndry(iisnk))
                  ndups(iisnk)=ndups(iisnk)+1
                 endif
C             point id not yet assigned for this source node
              else 
                pt_gtg(iisrc) = iisnk
              endif
              if (if_search.eq.1) then
                dups_gtg(iisrc) = dups_gtg(iisrc)+1
              endif
              nnum(iisnk)=nnum(iisnk)+1

              if (idebug.gt.6 .and. bndrypt.gt.0) then
                print*,iisnk," totals so far: "
                print*,"   accumulated points ",nnum(iisnk) 
                print*,"   accumulated bndry points ",ndups(iisnk) 
                print*,"   number shared boundaries ",nbndry(iisnk) 
                print*,"   shared with sink node ",bndryid
                print*,"   shared with source node ",bndrypt
              endif

C             OPTION harave - harmonic average
              if(ntype.eq.1)then
                 if (xfield(iisrc).eq.0.) then
                    write(logmess,908)iisrc
908                 format(2x,"ERROR DIV ZERO: attribute of node ",2x,
     *              i15," equal to zero")
                    call writloga('default',0,logmess,0,ierrw)
                    stop
                 endif
                 value(iisnk)=value(iisnk)+1./xfield(iisrc)

C             OPTION ariave - arithmatic average
              elseif(ntype.eq.2)then
                 value(iisnk)=value(iisnk)+xfield(iisrc)

C             OPTION geoave - geometric average
              elseif(ntype.eq.3) then
                 if(xfield(iisrc).le.0.)then
                   write(logmess,909)iisrc
909                format(2x,"ERROR DIV ZERO: attribute of node",2x,
     *             i15," less than or equal to zero")
                    call writloga('default',0,logmess,0,ierrw)
                   stop
                 endif
                 value(iisnk)=value(iisnk)+log(xfield(iisrc))

C             OPTION sum 
              elseif(ntype.eq.4)then

C               for single option, do not use a source point if
C               it has already been used for another Voronoi volumes  
                if (bndrypt.gt.0) then
                  vor_bndry(iisnk)=vor_bndry(iisnk)+xfield(iisrc)
                  if (sum_opt(1:3).eq."sin") then
                    nnum(iisnk)=nnum(iisnk)-1
                  else
                    value(iisnk)=value(iisnk)+xfield(iisrc)
                  endif

                else 
                  value(iisnk)=value(iisnk)+xfield(iisrc)

                endif

C             OPTION max 
              elseif(ntype.eq.5)then
                 maxval = value(iisnk) 
                 if (nnum(iisnk).eq.1) maxval=xfield(iisrc)
                 testval = xfield(iisrc) 
                 value(iisnk)= max(testval,maxval)

C             OPTION min
              elseif(ntype.eq.6)then
                 minval = value(iisnk)
                 if (nnum(iisnk).eq.1) minval=xfield(iisrc)
                 testval = xfield(iisrc)
                 value(iisnk)= min(testval,minval)

              endif

            enddo
C           END LOOP OVER FOUND CANIDATES 
 
            xs=xic(itfound(1))
            ys=yic(itfound(1))
            zs=zic(itfound(1))

C           Report progress so far
C           ii  is current node number in the fine source points
C           itfound is the coarse sink node (or nodes) for that point
            if(ipt.eq.1) then
               write(logmess,"(a)")
     *  '  Source Nodes    Percent Done'
               call writloga('default',1,logmess,0,ierrw)
            endif

            if((iwrite.eq.nwrite).and.(ipt.le.npoints_src)) then
               iwrite = 0
               write(logmess,"(i15,i9,a2)")
     *         ipt,istep*iperc,' %'
               call writloga('default',0,logmess,0,ierrw)
               totfind=0
               istep = istep + 1
            endif
            if (ipt.eq.npoints_src) then
                write(logmess,"(i15,a)")
     *         ipt,' Total source nodes searched.'
               call writloga('default',0,logmess,0,ierrw)
            endif
            iwrite=iwrite+1
            idone=ipt
            if (if_search.eq.0) then
                if (dups_gtg(ii).gt.1) ndups_tot=ndups_tot+1
            endif

         enddo
C        END LOOP through all source points 

C       Release memory block
        call mmrelblk('distpossleaf' ,isubname,ipdistpossleaf,ierr)
 
C       NOW ASSIGN FINAL VALUES based on nnum, dups_gtg, and value
C       avoid divide by zero where 0 nodes found for sink
C       do nothing if no source points were found for the sink node
C       ..... iout and xout(iisrc) are the sink attributes 
C       ..... value() was accumulated during the voronoi volume search
C       ..... nnum() are the number of src nodes for this sink node
C             if 0, then use initialized value()
        icntsink = 0
        icntsrc = 0
        do j=1,mpno
          ii=mpary(j)
        
          if (nnum(ii).eq.0) then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint( value(ii) )
               else
                  xout(ii)= value(ii)
               endif

          else  if (nnum(ii).gt.0) then
            icntsrc=icntsrc+nnum(ii)

c           harave
            if(ntype.eq.1) then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint( nnum(ii)/value(ii) )
               else
                  xout(ii)=nnum(ii)/value(ii)
               endif

c           ariave
            elseif(ntype.eq.2)then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint( value(ii)/nnum(ii) )
               else
                  xout(ii)=value(ii)/nnum(ii)
               endif

c           geoave
            elseif(ntype.eq.3) then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint(exp(dble(value(ii)/nnum(ii))))
               else
                  xout(ii)=exp(dble(value(ii)/nnum(ii)))
               endif

c           sum 
            elseif(ntype.eq.4) then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint( value(ii) )
               else
                  xout(ii)=value(ii)
               endif

c           max and min 
            elseif(ntype.eq.5 .or. ntype.eq.6) then
               if(ctype_snk.eq.'VINT')then
                  iout(ii)= nint( value(ii) )
               else
                  xout(ii)=value(ii)
               endif
            endif

C---        Report values per source node
            if (if_search.eq.1) then
              ndups_tot=ndups_tot+ndups(ii)
            endif
            if (idebug.gt.0 ) then

              if (idebug.gt.5) then

               if (ii.eq.1) then
                 write(logmess,"(a)")
     *      "---     Sink id   # of nodes used   calculated value ---" 
                 call writloga('default',0,logmess,0,ierrw)
               endif
               if(ctype_snk.eq.'VINT')then
                 write(logmess,"(i15,2x,i15,3x,i15)")
     *           ii,nnum(ii),iout(ii)
               else
                 write(logmess,"(i15,2x,i15,3x,e14.7)")
     *           ii,nnum(ii),xout(ii)
               endif
               call writloga('default',0,logmess,0,ierrw)
              endif

              if (idebug.gt.1) then

                if (ndups(ii).gt.0) then
                 if(ctype_snk.eq.'VINT')then
                   write(logmess,"(a,i15,3x,i15)")
     *            "Duplicate nodes  ",ndups(ii),nint(vor_bndry(ii))
                 else
                   write(logmess,"(a,i15,3x,e14.7)")
     *            "Duplicate nodes  ", ndups(ii),vor_bndry(ii)
                 endif
                 call writloga('default',0,logmess,0,ierrw)
               endif

              endif

            endif
C---        end debug report

          endif
C         end switch assigning values based on numm nodes found
          if(nnum(ii) .ge. 1) icntsink=icntsink+1

        enddo
C       END LOOP through sink points to assign attribute values
 
C
C     ******************************************************************
C     DONE
C
      go to 9999
 9999 continue

C     Report error conditions
      if (ierror.ne.0) then
         write(logmess,'(a)') 'upscale quit with errors.'
         call writloga('default',1,logmess,0,ierrw)
      endif
      if (idone.ne.npoints_src) then
          write(logmess,"(a,i15,a,i15)") 'WARNING: ',
     *    idone,' source nodes searched out of ',npoints_src
          call writloga('default',1,logmess,0,ierrw)
      endif

C     Report number of sink points assigned values
      if (icntsink.gt.0) then
          write(logmess,"(i15,a,i15,a)") icntsink,
     * ' sink nodes out of   ',npoints,' assigned values.'
      else
        write(logmess,"(a)")
     * 'ERROR: UPSCALE found zero sink points for source points.'
      endif
      call writloga('default',0,logmess,0,ierrw)

C     Report number of source points used to assign values 
      if (icntsrc.gt.0) then
          write(logmess,"(i15,a,i15,a)") icntsrc,
     * ' source nodes out of ',npoints_src,' used as source values.'
      else
        write(logmess,"(a)")
     * 'ERROR: UPSCALE found zero source nodes within Voronoi volumes.'
      endif
      call writloga('default',0,logmess,0,ierrw)

      if (ndups_tot.gt.0 .and. if_search.eq.1) then
         if (sum_opt(1:3).eq."sin") then
           write(logmess,"(i15,a)") ndups_tot,
     *     ' duplicate nodes on Voronoi boundaries used only once.'
         else
           write(logmess,"(i15,a)") ndups_tot,
     *     ' duplicate nodes on Voronoi boundaries used multiple times.'
         endif
         call writloga('default',0,logmess,0,ierrw)
      else if (ndups_tot.gt.0 .and. if_search.eq.0) then
          write(logmess,"(i15,a)") ndups_tot,
     *    ' duplicate nodes on Voronoi boundaries used only once.'
          call writloga('default',0,logmess,0,ierrw)
      endif

      write(logmess,"(a,a,a,a,a,a,a)")
     * 'UPSCALE/',intrp_opt(1:icharlnf(intrp_opt)),'/ from ',
     * attsrc(1:icharlnf(attsrc)), ' to ', attsnk(1:icharlnf(attsnk)),
     * ' Done.'
      call writloga('default',0,logmess,1,ierrw)
      ierror=0
      if(idebug.gt.0) call mmverify()

c     if attributes were created and not keeping, delete them now
      if(ikeep.ne.1 .and. ipt_exist.ne.0) then
        len=icharlnf(cmosrc)
        cbuff = 'cmo DELATT '//cmosrc(1:len)//'/pt_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
      endif
      if(ikeep.ne.1 .and. idups_exist.ne.0) then
        len=icharlnf(cmosrc)
        cbuff = 'cmo DELATT '//cmosrc(1:len)//'/dups_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
      endif

      len=icharlnf(cmosnk)
      cbuff = 'cmo select '//cmosnk(1:len)//' ;  finish'
      call dotaskx3d(cbuff,ierr)


C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C     call mmrelblk('mpary' ,isubname,ipmpary ,ics)
C     call mmrelblk('nneigh',isubname,ipnneigh,ics)
C     call mmrelblk('value',isubname,ipvalue,ics)
C     call mmrelblk('vor_bndry',isubname,ipvalue,ics)
C     call mmrelblk('nnum',isubname,ipnnum,ics)
C     call mmrelblk('ndups',isubname,ipnnum,ics)
C     call mmrelblk('nbndry',isubname,ipnnum,ics)
C     call mmrelblk('sbox' ,isubname,ipsbox,ics)
C     call mmrelblk('linkt' ,isubname,iplinkt,ics)
C     call mmrelblk('itfound' ,isubname,ipitfound,ics)

c     Return here if error is found before setup is done
 9000 if(ierror.ne.0) then
         write(logmess,'(a)')'FATAL ERROR: UPSCALE unable to begin.'
         call writloga('default',1,logmess,1,ierrw)
      endif
      if(if_search.gt.0) then
          call cmo_select(cmosnk,ierr)
          cbuff = 'cmo kdtree release ; finish'
          call dotaskx3d(cbuff,ierr)
      endif
      call mmrelprt(isubname,ics)
      call cmo_select(cmosnk,ierr)
C
      return
      end
