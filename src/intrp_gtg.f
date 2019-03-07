*dk,intrp_gtg
      subroutine intrp_gtg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C         This routine is a general version of interpolation from one
C     grid to another. It replaces options in doping that call integer1,
C     integer2, and table.
C     All interpolations are done on a set of x,y,z points provided by the
C     sink cmo. The sink and source grid attributes can be either node or
C     element. If the sink attribute is element type, then the element
C     centroids are used as the sink x,y,z points.
C
C     The interpolation values are written from  source grid attribute
C     to sink grid attribute. The sink grid is assumed to be located
C     in the same space as the source grid. Sink points that are found
C     outside the source grid are flagged with a special value detirmined
C     by flag_option.
C
C     For the sink attribute the following intrp options are available:
C     If source attribute is element
C        MAP - copy the source element value to the enclosed sink point
C     If source attribute is node
C        VORONOI    - Use nearest point to map node value to point
C        CONTINUOUS - Use the points of the enclosing element to
C                     compute interpolation on to each sink point
C
C
C     The interpolation assigns a single value to each sink point.
C     If the sink attribute is of element type, the centroids of
C     each element are used as the sink points.
C
C     The source attribute can be either node or element type. If
C     the source attribute is node type, then interpolation is
C     done from the nearest point (voronoi method) or using the
C     points of element containing the point (continuous).
C     If the source attribute is element, then element values
C     are mapped to the point contained in the element.
C
C     kdtree  is used to locate enclosing element.
C     kdtree0 is used to locate nearest points.
c       xs,ys,zs  - spatial coordinates of PREVIOUS nearest point
c       eps  -    epsilon for length comparisons
c       mtfound or mefound - number of objects  returned
c       itfound or iefound - array of pts or elems returned
c       linkt,sbox -   k-D tree arrays
c
C     After the search using kdtree, a lookup attribute is created that
C     corresponds the sink point to its source point and/or element.
C     The point attribute is named pt_gtg, element is el_gtg.
C     These attributes are created and deleted within this routine.
C     If att_option keepatt is included on the command line, then
C     the attribute is not deleted. This allows the kdtree search
C     to be skipped and corresponding objects are found by looking
C     at the attribute numbers. Multiple calls can then be made
C     with the time consuming search only in the first call when
C     the lookup attributes are created. The attributes should be
C     allowed to be deleted when done since these numbers will be
C     incorrect if the source grid is changed.
C     Default uses the attribute if it exists, otherwise no create.
C     Default will delete the lookup attributes at end of routine.
C     Only by adding keepatt option will attribute be created.
C
C     SYNTAX -
C
C  intrp (or interpolate)/
C     intrp_method/cmosink,attsink/1,0,0/cmosrc,attsrc/
C     [tie_option] [flag_option] [keep_attopt] [intrp_func]
C
C
C /intrp_method    These options detirmine what method of interpolation
C                  will be used. These methods differ from interpolation
C                  functions which are applied to the field values.
C     VORONOI    - (or NEAREST) use nearest node value
C     MAP        - (or COPY) use element value or nearest node.
C     CONTINUOUS - use nodes of located element to interpolate
C                  a value that is the sum of node values multiplied
C                  by relative volume of tet formed by the point
C                  and the three vertices oposite the node.
C                  MUST BE QUAD, TRI or TET for source grid.
C     -default-    If source attribute is element type then MAP
C                  If source attribute is node type then CONTINUOUS
C
C /cmosink,attsink - is the grid and attribute to write to
C /cmosrc, attsrc  - is the grid and attribute to interpolate from
C
C /tie_option      These options provide a method of choosing one candidate
C                  point or element when more than one are found for a point.
C    TIEMIN      - Make a choice by either the min or max value of the objects.
C    TIEMAX        Note this option is ignored if tabled values in pt_gtg or el_gtg
C                  are used since one-to-one correspondance has already been done.
C    TIEMAT      - This option will allow candidate elements only if the
C                  element's material matches the query point's material.
C                  Source attribute itetclr must match sink point imt.
C                  If multiple candidates pass this test, then TIEMIN or
C                  TIEMAX will be used to find a single solution.
C
C    Default     - For some cases finding inside element can result
C                  in multiple reasonable answers. In this case the
C                  return flag from inside element is used to pick
C                  the element with best confidence.
C                  
C
C /flag_option     Errors flag sink attribute with a special value.
C    value       - integer or real value to assign as flag value
C    PLUS1       - will find the max value of source attribute and add 1
C    NEAREST,ATT - will find the nearest source point and assign ATT value
C                  the keyword nearest must be followed a node attribute name
C                  default = PLUS1
C
C /keep_attopt      This routine uses kdtree to setup a lookup table
C                  that pairs sink points to their source objects.
C                  pt_gtg attribute are the nearest source points
C                  el_gtg attribute are the enclosing source elements
C                  If the attribute exists, it will be used to lookup
C                  point or element numbers, otherwise it is created.
C    keepatt     - create and keep attributes pt_gtg and/or el_gtg.
C                  use this for multiple calls using attribute for lookup
C    delatt      - delete attribute pt_gtg or el_gtg at end of routine
C                  default creates the attribute, then deletes when done
C
C /intrp_func    - The source attribute interpolate value which is the
C                  function applied to final field value after or
C                  during interpolation routines.
C                  A function named here will override the source
C                  attribute's interpolate type.
C                  Valid interpolate types include: linear, asinh, log,
C                  copy, sequence, min, incmin, max, incmax, and, or, user
C
C                  idebug can be set for more screen output, use 5 or larger
C
C  EXAMPLES:
C
C
C  intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/
C        assign nearest point imt1 to sink imt1
C  intrp/voronoi/cmo_sink imtreal/pset,get,psmall/cmo_src imtreal
C        assign nearest point imt1real to sink imt1real
C        only change values in selected pset for cmo_sink
C
C  intrp/map/cmo_sink imt1/1,0,0/cmo_src itetclr/
C        find an element which sink point is inside
C        assign element itetclr to sink imt1
C  intrp/map/cmo_sink itetclr/1,0,0/cmo_src itetclr/
C        find an element which sink centroid is inside
C        assign element itetclr to sink itetclr
C
C  intrp/continuous/cmo_sink xval/1,0,0/cmosrc xval
C        find an element which sink point is inside
C        interpolate the element node values to sink xval
C
C  intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/keepatt
C  intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1
C        assign nearest point imt1 to sink imt1 in first call
C        keep attribute pt_gtg which has the nearest point numbers
C        during second call, search is skipped and pt_gtg values are used
C        pt_gtg attribute is removed from cmo_sink in second call
C
C
C
C
c#######################################################################
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
c#######################################################################
C
C     CHANGE HISTORY -
C
C        $Log: intrp_gtg.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.15   23 Aug 2007 08:00:44   gable
CPVCS    Modified so that if source or sink MO exists but has zero nodes, command will
CPVCS    exit with no action instead of crashing when it tries to allocate a zero length array.
CPVCS    
CPVCS       Rev 1.14   04 Jan 2007 09:59:06   gable
CPVCS    Add header log to file.
C
c#######################################################################
C
      implicit none
C
      include "local_element.h"
      include 'consts.h'
C
      integer nplen
      parameter (nplen=1000000)
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror
C
C#######################################################################
C
 
C-----Sink Mesh Object
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipimat, imat)
      integer itettyp(nplen),itet(nplen),itetoff(nplen),imat(nplen)
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(nplen), yic(nplen), zic(nplen)
 
      pointer (ipxic_cntr, xic_cntr)
      pointer (ipyic_cntr, yic_cntr)
      pointer (ipzic_cntr, zic_cntr)
      real*8  xic_cntr(nplen), yic_cntr(nplen), zic_cntr(nplen)
 
 
C
C-----Source Mesh Object
      pointer (ipitettyp_src, itettyp_src)
      pointer (ipitetoff_src, itetoff_src)
      pointer (ipitet_src, itet_src)
      pointer (ipimat_src, imat_src)
      integer itettyp_src(nplen),itetoff_src(nplen),
     *        itet_src(nplen),imat_src(nplen)
 
      pointer (ipxic_src, xic_src)
      pointer (ipyic_src, yic_src)
      pointer (ipzic_src, zic_src)
      real*8 xic_src(nplen), yic_src(nplen), zic_src(nplen)
 
C
C-----Work pointers
      pointer (ipiattsnk, iattsnk)
      pointer (ipiattsrc, iattsrc)
      pointer (ipiattsrc_near, iattsrc_near)
      pointer (ippt_gtg, pt_gtg)
      pointer (ipel_gtg, el_gtg)
      pointer (ipout, out)
      pointer (ipmpary, mpary)
      pointer (ipisetwd, isetwd)
      pointer (ipxtetwd, xtetwd)
      pointer (ipitp1, itp1)
      pointer (iplinkt, linkt)
      pointer (ipitfound, itfound)
      pointer (ipiefound, iefound)
      integer iattsnk(nplen), iattsrc(nplen), iattsrc_near(nplen)
      integer pt_gtg(nplen),el_gtg(nplen),out(nplen),mpary(nplen),
     * isetwd(nplen),itp1(nplen),linkt(nplen),itfound(nplen),
     * iefound(nplen), xtetwd(nplen)
 
      pointer (ipxattsnk, xattsnk)
      pointer (ipxattsrc, xattsrc)
      pointer (ipxattsrc_near, xattsrc_near)
      pointer (ipwork, work)
      pointer (ipxsource, xsource)
      pointer (ipsbox, sbox)
      pointer (ipxvals, xvals)
      pointer (ipyvals, yvals)
      pointer (ipzvals, zvals)
      pointer (ipdistpossleaf, distpossleaf)
      real*8 xattsnk(nplen), xattsrc(nplen), work(nplen),xsource(nplen),
     * xattsrc_near(nplen),xvals(nplen),yvals(nplen),zvals(nplen)
      real*8 sbox(2,3,nplen)
      real*8 distpossleaf(nplen)
 
C
      integer ikeep, mk_elatt, mk_ptatt, if_elsearch, if_ptsearch
      integer ierr,ierrw,ics,ics2,idx,inxt,i,ii,j,jj,ipt,idone,idebug
      integer mtfound,nefound,inelement,ipt_exist,iel_exist,iisrc,iisnk
      integer lenout,len,ilen,ityp,ipt1,ipt2,ipt3,mpno,mbndry,nen
      integer iout,inflag,inflag_prev,index_save,index_prev,index_end
      integer inflag_save,intie,icscode

      integer npoints,nelements,length,ipointi,ipointj,
     * attsrc_len,attsnk_len,npts_src,npts_snk,nelm_src,nelm_snk,
     * nsdgeom_snk,num_src,num_snk,num_snk_all,nwrite,istep,iwrite,
     * nsdgeom_src,nen_snk,nen_src,nef_snk,nef_src,iperc,totfind,
     * totsrchd,nsdtopo_snk,nsdtopo_src,ielmtyp,ielmtyp2,
     * ifirst,ifound,totflag,just_pt_gtg,if_centroid,volzero, 
     * ierr_eps
 
      real*8 eps,epsilonm,epsilonvol,epsilonlen,xs,ys,zs,xp,yp,zp,xperc
      real*8 xmi,xma,ymi,yma,zmi,zma,epsarea,epsvol,volelm
      real*8 xflag, maxval,minval,val_end,val_try,val_save,val_prev

C     local arrays for search objects
      real*8 xnew1(maxnen),ynew1(maxnen),znew1(maxnen),xfield(maxnen)
      real*8 xnew2(maxnen),ynew2(maxnen),znew2(maxnen)

      real*8 xicelm1(8), yicelm1(8), zicelm1(8)
      real*8 xic1(8), yic1(8), zic1(8)
      real*8 xic2(8), yic2(8), zic2(8)

C     for evaluating precision issues
      real*8 xfac,xcntr,ycntr,zcntr
      real*8 xcntr0,ycntr0,zcntr0 
      real*8  bb_xcntr,bb_ycntr,bb_zcntr,
     *        bb_xcntr2,bb_ycntr2,bb_zcntr2
      real*8 xtrans,ytrans,ztrans,
     *       xmin,ymin,zmin,xmax,ymax,zmax
      real*8 maxnum,maxdist,maxdiff
      
 
C
      character*32 ich1,ich2,ich3,blkname,attsrc_near
      character*32 cmosnk, cmosrc, attsrc, attsnk
      character*32 ctype_snk,ctype_src,clen_snk,clen_src,clen_pts
      character*32 cpers,cio,crank,cinter_snk,cinter_src
      character*32 cinter_pts,ctype_pts
      character*32 cout
      character*32 intrp_opt,tie_opt,tie_opt2, flag_opt, intrp_func
      character*32 isubname
      character*132 logmess, cbuff
C
      integer icharlnf
      real*8 cinterpolate, cinterpolate_elem
      real*8 alargenumber
      real*8 local_epsilon
      parameter(local_epsilon=1.0d-10)
      data alargenumber/1.d+99/
 
C
C ######################################################################
C
 
c     set defaults
      isubname='intrp_gtg'
      ierror = -1
      tie_opt = 'maxtie'
      tie_opt2 = 'notset'
      flag_opt = 'plus1'
      intrp_func = 'notset'
      attsrc_near = 'notset'
      ikeep = 0
      just_pt_gtg = 0
      if_centroid = 0
 
C     ******************************************************************
c     get command line values
C
c        1       2          3      4    5 6 7  8      9
c      intrp/intrp_opt/cmosnk,attsnk/1,0,0/cmosrc,attsrc/
c       [tie_opt] / [flag_opt] / [keepatt | delatt] [intrp_func]
c
 
      if (nwds.lt.9) then
         write(logmess,"('INTRP: Incorrect syntax.')")
         call writloga('default',1,logmess,1,ierrw)
         goto 9000
      endif
      intrp_opt = cmsgin(2)
      cmosnk = cmsgin(3)
      attsnk = cmsgin(4)
      cmosrc = cmsgin(8)
      attsrc = cmsgin(9)
 
C     Check the mesh object names
      call cmo_exist(cmosnk,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a)')
     *     'INTRP: Not a valid mesh object: ',cmosnk
         call writloga('default',1,logmess,1,ierrw)
 
         call cmo_exist(cmosrc,ics)
         if(ics.ne.0) then
            write(logmess,'(a,a)')
     *       'INTRP: Not a valid mesh object: ',cmosrc
            call writloga('default',1,logmess,1,ierrw)
         endif
         if (ierr.ne.0 .or. ics.ne.0) goto 9000
      endif
 
C     Check the mesh object attributes

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

      call mmfindbk(attsrc,cmosrc,ipout,lenout,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *      '  cmo= ',cmosrc(1:icharlnf(cmosrc)),
     *      '  att= ',attsrc(1:icharlnf(attsrc))
         call writloga('default',0,logmess,0,ierrw)
 
         call mmfindbk(attsnk,cmosnk,ipout,lenout,ics)
         if(ics.ne.0) then
           write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *        '  cmo= ',cmosnk(1:icharlnf(cmosnk)),
     *        '  att= ',attsnk(1:icharlnf(attsnk))
           call writloga('default',0,logmess,0,ierrw)
         endif
         if (ierr.ne.0 .or. ics.ne.0) goto 9000
      endif
 
C     Loop through remaining optional command tokens
C     The first 9 are neccessary, the remainder are not
C     token ordering for remainder is ignored
C     i increments through each of the remainder tokens
C     inxt increments past i if a token is paired with a value
C       tie_opt is the decision method when choosing a value
C       flag_opt - the method to assign a value to unfound objects
C       intrp_func overides attribute's interpolate type
C       keepatt - attribute pt_gtg and/or el_gtg not deleted
 
      inxt = 10
      do i = 10,nwds
        inxt = max(i,inxt)
 
        if(msgtype(inxt).eq.3) then
          if((cmsgin(inxt)(1:4).eq.'keep') .or.
     *      (cmsgin(inxt)(1:4).eq.'KEEP')) then
            ikeep = 1
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'delatt') .or.
     *      (cmsgin(inxt)(1:6).eq.'DELATT')) then
            ikeep = 0
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:4).eq.'plus') .or.
     *      (cmsgin(inxt)(1:4).eq.'PLUS')) then
            flag_opt = 'plus1'
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:7).eq.'nearest') .or.
     *      (cmsgin(inxt)(1:7).eq.'NEAREST')) then
            flag_opt = 'nearest'
            inxt = inxt+1
            if (msgtype(inxt).eq.3) then
               attsrc_near = cmsgin(inxt)
               inxt=inxt+1
            else
              write(logmess,'(a)')
     *        'ERROR: nearest option needs node attribute.'
              call writloga('default',0,logmess,1,ierrw)
              goto 9000
            endif
          elseif((cmsgin(inxt)(1:6).eq.'tiemin') .or.
     *           (cmsgin(inxt)(1:6).eq.'TIEMIN') .or.
     *           (cmsgin(inxt)(1:6).eq.'mintie') ) then
            tie_opt = 'mintie'
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'tiemax') .or.
     *           (cmsgin(inxt)(1:6).eq.'TIEMAX') .or.
     *           (cmsgin(inxt)(1:6).eq.'maxtie') ) then
            tie_opt = 'maxtie'
            inxt=inxt+1
          elseif((cmsgin(inxt)(1:6).eq.'tiemat') .or.
     *           (cmsgin(inxt)(1:6).eq.'TIEMAT') .or.
     *           (cmsgin(inxt)(1:6).eq.'mattie') ) then
            tie_opt2 = 'mattie'
            inxt=inxt+1
          else
            intrp_func = cmsgin(inxt)
            inxt=inxt+1
          endif
        else
          if (msgtype(inxt).eq.1) then
            xflag = dble(imsgin(inxt))
            flag_opt = 'user'
            inxt=inxt+1
          elseif (msgtype(inxt).eq.2) then
            xflag = xmsgin(inxt)
            flag_opt = 'user'
            inxt=inxt+1
          endif
        endif
      enddo
C     End command processing - Done with message arrays
 
 
C     ******************************************************************
C     Get Sink Mesh Object
C
      call cmo_select(cmosnk,ierr)
      call cmo_get_intinfo('nnodes',cmosnk,npoints,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosink')
      if(npoints .le. 0)then
        write(logmess,'(a)') 'WARNING: No nodes in SINK mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        goto 9000
      endif

      call cmo_get_intinfo('nelements',cmosnk,nelements,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements cmosink')
      npts_snk = npoints
      nelm_snk = nelements
      if (ierr.ne.0) goto 9000
 
      call cmo_get_info('idebug',cmosnk,idebug,len,ityp,ierr)
      call cmo_get_attinfo('epsilon',cmosnk,iout,epsilonm,cout,
     *        ipout,ilen,ityp, ierr)
      call cmo_get_attinfo('epsilonl',cmosnk,iout,epsilonlen,cout,
     *        ipout,ilen,ityp, ierr)
      call cmo_get_attinfo('epsilonv',cmosnk,iout,epsilonvol,cout,
     *        ipout,ilen,ityp, ierr)

      call cmo_get_intinfo('mbndry',cmosnk,mbndry,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo mbndry cmosink')
      if (ierr.ne.0) goto 9000
      call cmo_get_intinfo('ndimensions_geom',cmosnk,
     *                      nsdgeom_snk,ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmosnk,
     *                      nsdtopo_snk,ilen,ityp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmosnk,
     *                      nen_snk,ilen,ityp,ierr)
      call cmo_get_intinfo('faces_per_element',cmosnk,
     *                      nef_snk,ilen,ityp,ierr)
 
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
 
c     get sink attribute info and assign attribute length
      call cmo_get_attparam(attsnk,cmosnk,idx,ctype_snk,crank,
     *    clen_snk,cinter_snk,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam sink')
      if (clen_snk(1:8).eq.'nelement') then
        if_centroid = 1
        num_snk = nelm_snk
      else
        num_snk = npts_snk
      endif
 
C     ******************************************************************
C     set the point index boundaries
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(5).eq.1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
      endif
 
c     For node point set
      if(if_centroid.ne.1) then
        length=npts_snk
        call mmgetblk('mpary',isubname,ipmpary,length,2,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
        if (ics.ne.0) goto 9000
 
        if(msgtype(5).eq.1) then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
        else
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
        endif
        if (mpno.gt.0) then
           write(logmess,'(a,i10)')
     *      'nodes in indexed point set  = ',mpno
           call writloga('default',0,logmess,0,ierrw)
        else
           write(logmess,'(a)') 'No points in indexed point set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9000
        endif
 
c     For element point set
      else
 
        length=nelm_snk
        call cmo_get_info('xtetwd',cmosnk,ipxtetwd,length,ityp,ics)
        if (ics.ne.0) call x3d_error(isubname,'get xtetwd')
        length=nelm_snk
        call mmgetblk('mpary',isubname,ipmpary,length,1,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
        if (ics.ne.0) goto 9000
 
        if(msgtype(5).eq.1) then
          mpno=0
          if(ipt2.le.0) ipt2=nelm_snk
          if(ipt3.le.0) ipt3=1
          do i = ipt1,ipt2,ipt3
            mpno=mpno+1
            mpary(mpno) = i
          enddo
        else
          call eltlimc(ich1,ich2,ich3,ipmpary,mpno,nelm_snk,xtetwd)
        endif
        if (mpno.gt.0) then
           write(logmess,'(a,i10)')
     *      'elements in indexed set  = ',mpno
           call writloga('default',0,logmess,0,ierrw)
        else
           write(logmess,'(a)') 'No elements in indexed set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9000
        endif
 
      endif
 
 
C     ******************************************************************
C     Get Source Mesh Object
C
      call cmo_select(cmosrc,ierr)
      call cmo_get_intinfo('nnodes',cmosrc,npts_src,ilen,ityp,ierr)
      if(npts_src .le. 0)then
        write(logmess,'(a)') 'WARNING: No nodes in SOURCE mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        goto 9000
      endif
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosource')
      call cmo_get_intinfo('nelements',cmosrc,nelm_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelems cmosource')
      call cmo_get_intinfo('ndimensions_geom',cmosrc,
     *                      nsdgeom_src,ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmosrc,
     *                      nsdtopo_src,ilen,ityp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmosrc,
     *                      nen_src,ilen,ityp,ierr)
      call cmo_get_intinfo('faces_per_element',cmosrc,
     *                      nef_src,ilen,ityp,ierr)
      if (ierr.ne.0) goto 9000
 
 
      call cmo_get_info('xic',cmosrc,ipxic_src,ilen,ityp,ierr)
      call cmo_get_info('yic',cmosrc,ipyic_src,ilen,ityp,ierr)
      call cmo_get_info('zic',cmosrc,ipzic_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xyz cmosource')
      if (ierr.ne.0) goto 9000
      call cmo_get_info('itettyp',cmosrc,ipitettyp_src,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmosrc,ipitetoff_src,ilen,ityp,ierr)
      call cmo_get_info('itet',cmosrc,ipitet_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itet cmosource')
      if (ierr.ne.0) goto 9000
 
c     get source attribute info and assign attribute length
      call cmo_get_attparam(attsrc,cmosrc,idx,ctype_src,crank,
     *    clen_src,cinter_src,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam source')
      if (ierr.ne.0) goto 9000
      if (clen_src(1:5).eq.'nnode') then
         num_src = npts_src
      else
         num_src = nelm_src
      endif
 
 
C     ******************************************************************
C     Attribute setup for sink and source
C     Sink grid is a point attribute or element(centroid points)
C     For source grid:
C     If method is VORONOI then get point attribute
C     If method is CONTINUOUS   get point attributes
C     If method is MAP     then get element attribute
C
 
C     Assign sink attribute to xattsnk or iattsnk array
      len=icharlnf(attsnk)
      blkname=' '
      blkname(1:len)=attsnk
      if(len.eq.3) then
        if(blkname(1:3).eq.'itp') blkname='itp1'
        if(blkname(1:3).eq.'imt') blkname='imt1'
        if(blkname(1:3).eq.'icr') blkname='icr1'
        if(blkname(1:3).eq.'isn') blkname='isn1'
        len=icharlnf(blkname)
      endif
      If(ctype_snk(1:4).eq.'VINT') then
        call mmgetpr(blkname,cmosnk,ipiattsnk,ics)
        call mmgetlen(ipiattsnk,attsnk_len,ics2)
        if(ics.ne.0) then
          call x3d_error(isubname,'mmgetpr iatt sink')
          goto 9000
        endif
      elseif(ctype_snk(1:7).eq.'VDOUBLE') then
        call mmgetpr(blkname,cmosnk,ipxattsnk,ics)
        call mmgetlen(ipxattsnk,attsnk_len,ics2)
        if(ics.ne.0) then
          call x3d_error(isubname,'mmgetpr xatt sink')
          goto 9000
        endif
      else
        write(logmess,'(a,a,a,a)')
     *  'Invalid attribute type for ',cmosnk(1:icharlnf(cmosnk)),
     *   ' ',blkname(1:icharlnf(blkname))
         call writloga('default',1,logmess,0,ierrw)
         goto 9000
      endif
C     get memory that will hold temporary sink attribute values
      length=num_snk
      call mmgetblk('work',isubname,ipwork,length,2,ics)
      if(ics.ne.0) then
         call x3d_error(isubname,'mmgetblk work')
         goto 9000
      endif
 
C     Assign source attribute to xattsrc or iattsrc array
C     INT attribute will be converted and copied to xattsnk
      length=num_src
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
        call mmgetpr(blkname,cmosrc,ipiattsrc,ics)
        call mmgetlen(ipiattsrc,attsrc_len,ics2)
        call mmgetblk('xsource',isubname,ipxsource,length,2,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk xsource')
        ipxattsrc = ipxsource
      elseif(ctype_src(1:7).eq.'VDOUBLE') then
        call mmgetpr(blkname,cmosrc,ipxattsrc,ics)
        call mmgetlen(ipxattsrc,attsrc_len,ics2)
        if(ics.ne.0) call x3d_error(isubname,'mmgetpr xatt src')
      else
        write(logmess,'(a,a,a,a)')
     *  'Invalid attribute type for ',cmosrc(1:icharlnf(cmosrc)),
     *   ' ',blkname(1:len)
         call writloga('default',1,logmess,0,ierrw)
         goto 9000
      endif
      if (ics.ne.0) goto 9000
 
c     get source pts attribute info for nearest point flag
      if(flag_opt(1:7).eq.'nearest') then
        call cmo_get_attparam(attsrc_near,cmosrc,idx,ctype_pts,crank,
     *      clen_pts,cinter_pts,cpers,cio,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_attparam source')
 
        len=icharlnf(attsrc_near)
        blkname=' '
        blkname(1:len)=attsrc_near
        if(len.eq.3) then
          if(blkname(1:3).eq.'itp') blkname='itp1'
          if(blkname(1:3).eq.'imt') blkname='imt1'
          if(blkname(1:3).eq.'icr') blkname='icr1'
          if(blkname(1:3).eq.'isn') blkname='isn1'
          len=icharlnf(blkname)
        endif
        if(ctype_pts(1:4).eq.'VINT') then
          call mmgetpr(blkname,cmosrc,ipiattsrc_near,ics)
          call mmgetlen(ipiattsrc_near,attsrc_len,ics2)
          if(ics.ne.0) call x3d_error(isubname,'mmgetpr iattsrc_near')
        elseif(ctype_pts(1:7).eq.'VDOUBLE') then
          call mmgetpr(blkname,cmosrc,ipxattsrc_near,ics)
          call mmgetlen(ipxattsrc_near,attsrc_len,ics2)
          if(ics.ne.0) call x3d_error(isubname,'mmgetpr xattsrc_near')
        else
          write(logmess,'(a,a,a,a)')
     *     'Invalid attribute type for ',cmosrc(1:icharlnf(cmosrc)),
     *     ' ',blkname(1:len)
           call writloga('default',1,logmess,0,ierrw)
           goto 9000
        endif
        if (clen_pts(1:5).ne.'nnode') then
           write(logmess,'(a,a,a)')
     *    'nearest point flag option must be of length nnodes: ',
     *     attsrc_near(1:icharlnf(attsrc_near))
           call writloga('default',1,logmess,0,ierrw)
           goto 9000
        endif
        if (ics.ne.0) goto 9000
      endif
 
c     get material attributes itetclr and imt for tiebreaker
c     this is a secondary tiebreaker used to break tie with
c     other first pass tiebreakers
      if(tie_opt2(1:3).eq.'mat') then
        if (intrp_opt(1:7).eq.'voronoi') then
          call cmo_get_info('imt',cmosrc,ipimat_src,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'get_info imt src')
        else
          call cmo_get_info('itetclr',cmosrc,ipimat_src,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'get_info itetclr src')
        endif
        if (clen_snk(1:5).eq.'nnode') then
          call cmo_get_info('imt',cmosnk,ipimat,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'get_info imt sink')
        else
          call cmo_get_info('itetclr',cmosnk,ipimat,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'get_info itetclr sink')
        endif
        if (ierr.ne.0) goto 9000
      endif
 
C     Do some error checking and final setup of commands
      if(idebug.gt.0) then
        write(logmess,'(a,a15,a10,a10,a10)')
     *     'sink attribute:   ',attsnk,ctype_snk,clen_snk,cinter_snk
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,a15,a10,a10,a10)')
     *     'source attribute: ',attsrc,ctype_src,clen_src,cinter_src
        call writloga('default',0,logmess,0,ierrw)
      endif
 
      if(ctype_snk.ne.ctype_src) then
         write(logmess,'(a,a,a)')
     *   'WARNING: attribute types differ: ',ctype_snk,ctype_src
         call writloga('default',1,logmess,1,ierrw)
      endif
      if(intrp_opt(1:3).eq.'def') then
        if(clen_src(1:8).eq.'nelement') then
          intrp_opt = 'map'
        else
          intrp_opt = 'continuous'
        endif
      endif
 
      if(intrp_opt(1:10).eq.'continuous') then
 
        if(clen_src(1:5).ne.'nnode') then
          write(logmess,'(a,a)')
     *     'continuous option must have node attribute type:. '
     *     ,attsrc(1:icharlnf(attsrc))
          call writloga('default',1,logmess,0,ierrw)
          goto 9000
        endif
 
        if (itettyp_src(1).eq.ifelmhex) then
          write(logmess,'(a,a)')
     *'WARNING: continuous interpolation may not work for hex elements:'
     *     ,cmosrc(1:icharlnf(cmosrc))
          call writloga('default',1,logmess,0,ierrw)
          write(logmess,'(a)')
     *     'Use hextotet to convert hex elements to tetrahedra. '
          call writloga('default',0,logmess,1,ierrw)
        endif
 
      endif
 
      if(intrp_opt(1:7).eq.'voronoi') then
        if(clen_src(1:5).ne.'nnode') then
          write(logmess,'(a,a)')
     *     'voronoi option must have node attribute type: '
     *     ,attsrc(1:icharlnf(attsrc))
          call writloga('default',1,logmess,0,ierrw)
          goto 9000
        endif
      endif
 
      if(intrp_opt(1:3).eq.'map') then
        if(clen_src(1:8).ne.'nelement') then
          write(logmess,'(a,a)')
     *     'MAP option must have element attribute type: ',
     *     attsrc(1:icharlnf(attsrc))
          call writloga('default',1,logmess,0,ierrw)
          goto 9000
        endif
      endif
 
 
      if(intrp_opt(1:7).eq.'nearest') intrp_opt = 'voronoi'
      if(intrp_opt(1:4).eq.'copy') intrp_opt = 'map'
      if((intrp_opt(1:7).ne.'voronoi') .and.
     *   (intrp_opt(1:3).ne.'map') .and.
     *   (intrp_opt(1:10).ne.'continuous')) then
         write(logmess,'(a,a)')'Invalid intrp option: ',intrp_opt
         call writloga('default',1,logmess,0,ierrw)
         goto 9000
      endif
      if(intrp_func(1:6).eq.'notset' .or.
     *         icharlnf(intrp_func).eq.0) then
        intrp_func = cinter_src
      endif
 
C     ******************************************************************
c     look for gtg attributes for sink-point and sink-element pairs
c     if lookup attributes exist, use them instead of using search
c     mmgetpr returns 0 if attribute exists
c
 
      call mmgetpr('pt_gtg',cmosnk,ippt_gtg,ipt_exist)
      if (ipt_exist.eq.0) then
        call cmo_get_info('pt_gtg',cmosnk,ippt_gtg,ilen,ityp,ics)
        if(ics.ne.0) then
          write(logmess,'(a,a,a,a)')
     *    'intrp attribute does not exist: ',
     *    '  cmo= ',cmosnk(1:icharlnf(cmosnk)),'  att= pt_gtg '
          call writloga('default',1,logmess,1,ierrw)
          goto 9000
        endif
        if_ptsearch = 0
        ipt_exist = 1
      else
        ipt_exist = 0
        if_ptsearch = 1
      endif
 
      call mmgetpr('el_gtg',cmosnk,ipel_gtg,iel_exist)
      if (iel_exist.eq.0) then
        call cmo_get_info('el_gtg',cmosnk,ipel_gtg,ilen,ityp,ics)
        if(ics.ne.0) then
          write(logmess,'(a,a,a,a)')
     *    'intrp attribute does not exist: ',
     *    '  cmo= ',cmosnk(1:icharlnf(cmosnk)),' att= el_gtg '
          call writloga('default',1,logmess,1,ierrw)
          goto 9000
        endif
        if_elsearch = 0
        iel_exist = 1
      else
        iel_exist = 0
        if_elsearch = 1
      endif
 
 
C     ******************************************************************
c     Set and check valid options for interpolation methods
      mk_ptatt = 0
      mk_elatt = 0
      len=icharlnf(intrp_opt)
      if(intrp_opt(1:len).eq.'voronoi' .and. ikeep.eq.1) then
        if(if_ptsearch.eq.1) mk_ptatt = 1
      elseif(intrp_opt(1:len).eq.'map' .and. ikeep.eq.1) then
        if(if_elsearch.eq.1) mk_elatt = 1
      elseif(intrp_opt(1:len).eq.'continuous' .and. ikeep.eq.1) then
        if(if_elsearch.eq.1) mk_elatt = 1
      endif
      if(flag_opt(1:7).eq.'nearest') mk_ptatt = 1

      len=icharlnf(tie_opt)
      if( (tie_opt(1:3).ne.'min') .and.
     *  (tie_opt(1:3).ne.'max') ) then
        write(logmess,'(a,a)')
     *  'Invalid tie_option for VORONOI: ',tie_opt(1:len)
        call writloga('default',1,logmess,0,ierrw)
        goto 9000
      endif
 
c     make lookup attributes if using them
c     initialize to zero
      if (mk_ptatt.eq.1 .and. ipt_exist.eq.0) then
        if_ptsearch = 1
        if (if_centroid.eq.1) then
           cbuff = 'cmo/addatt/'//cmosnk(1:icharlnf(cmosnk))//
     >     '/pt_gtg/' //
     >     'VINT/scalar/nelements//permanent/agfx/0 ; finish'
        else
           cbuff = 'cmo/addatt/'//cmosnk(1:icharlnf(cmosnk))//
     >     '/pt_gtg/' //
     >     'VINT/scalar/nnodes//permanent/agfx/0 ; finish'
        endif
        call dotaskx3d(cbuff,ierr)
        if(ierr.ne.0) then
          call x3d_error(isubname,'make att pt_gtg')
          goto 9000
        endif
        call mmgetpr('pt_gtg',cmosnk,ippt_gtg,ics)
        ipt_exist = 1
      endif
 
      if (mk_elatt.eq.1 .and. iel_exist.eq.0) then
        if_elsearch = 1
        if (if_centroid.eq.1) then
           cbuff = 'cmo/addatt/'//cmosnk(1:icharlnf(cmosnk))//
     >     '/el_gtg/' //
     >     'VINT/scalar/nelements//permanent/agfx/0 ; finish'
        else
           cbuff = 'cmo/addatt/'//cmosnk(1:icharlnf(cmosnk))//
     >     '/el_gtg/' //
     >     'VINT/scalar/nnodes//permanent/agfx/0 ; finish'
        endif
        call dotaskx3d(cbuff,ierr)
        if(ierr.ne.0) then
          call x3d_error(isubname,'make att el_gtg')
          goto 9000
        endif
        call mmgetpr('el_gtg',cmosnk,ipel_gtg,ics)
        iel_exist = 1
      endif
 
 
C     ******************************************************************
C     Done with setup and error checking, start work for interpolation
c     find max and min values of source attribute
c     If source att is INT, convert and copy into xattsrc
      if(ctype_src(1:4).eq.'VINT') xattsrc(1) = dble(iattsrc(1))
      maxval=xattsrc(1)
      minval=xattsrc(1)
      do i=1,num_src
        if (ctype_src(1:4).eq.'VINT') then
          xattsrc(i) = dble(iattsrc(i))
        endif
        minval = min(minval,xattsrc(i))
        maxval = max(maxval,xattsrc(i))
      enddo
      if(flag_opt(1:5).eq.'plus1') xflag=maxval + one
 
c     Write summary of interpolation setup and type
      write(logmess,'(a,a10,a,a10)')
     * 'INTRP METHOD: ',intrp_opt(1:icharlnf(intrp_opt)),
     *  '   FUNCTION: ',intrp_func(1:icharlnf(intrp_func))
      call writloga('default',1,logmess,0,ierrw)
 
      if (flag_opt(1:8).ne.'nearest') then
        if (tie_opt2(1:3).eq.'mat') then
         write(logmess,'(a,a8,a8,a,a10,e20.12)')
     *   'TIEBREAKER: ',tie_opt(1:icharlnf(tie_opt)),
     *   tie_opt2(1:icharlnf(tie_opt2)),
     *   '   FLAG: ',flag_opt(1:icharlnf(flag_opt)),xflag
         call writloga('default',0,logmess,1,ierrw)
        else
         write(logmess,'(a,a10,a,a10,e20.12)')
     *   '      TIEBREAKER: ',tie_opt(1:icharlnf(tie_opt)),
     *   '   FLAG: ',flag_opt(1:icharlnf(flag_opt)),xflag
         call writloga('default',0,logmess,1,ierrw)
        endif
      else
        write(logmess,'(a,a10,a,a10,a,a)')
     *  '      TIEBREAKER: ',tie_opt(1:icharlnf(tie_opt)),
     *  '   FLAG: ',flag_opt(1:icharlnf(flag_opt)),' ',
     *              attsrc_near(1:icharlnf(attsrc_near))
        call writloga('default',0,logmess,1,ierrw)
      endif
 
      if (ikeep.eq.1 .and. mk_ptatt.eq.1) then
      write(logmess,'(a)')'pt_gtg attribute will be added and kept.'
      call writloga('default',0,logmess,0,ierrw)
      elseif (ikeep.eq.0 .and. mk_ptatt.eq.1) then
      write(logmess,'(a)')'pt_gtg attribute will be added and deleted.'
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      if (ikeep.eq.1 .and. mk_elatt.eq.1) then
      write(logmess,'(a)')'el_gtg attribute will be added and kept.'
      call writloga('default',0,logmess,0,ierrw)
      elseif (ikeep.eq.0 .and. mk_elatt.eq.1) then
      write(logmess,'(a)')'el_gtg attribute will be added and deleted.'
      call writloga('default',0,logmess,0,ierrw)
      endif
 
 
C     ******************************************************************
c     Assign coordinate values to intended coordinates
c     If sink attribute is element, put centroids into coord vals
      if (if_centroid.eq.1) then
 
        length=num_snk
        call mmgetblk('xic_cntr',isubname,ipxic_cntr,length,2,ics)
        call mmgetblk('yic_cntr',isubname,ipyic_cntr,length,2,ics)
        call mmgetblk('zic_cntr',isubname,ipzic_cntr,length,2,ics)
        if (ics.ne.0) call x3d_error(isubname,'mmgetblk zic_cntr')


C       use centroids to check possible precision errors
        call cmo_get_minmax(cmosnk,xmin,ymin,zmin,xmax,ymax,zmax,ierr)
        ierr_eps=0

        do j=1,mpno
           idx=mpary(j)
           xcntr= zero
           ycntr= zero
           zcntr= zero
           xcntr0= zero
           ycntr0= zero
           zcntr0= zero
           nen=nelmnen(itettyp(idx))
           do i=1,nen
              ipt=itet(itetoff(idx)+i)
              xcntr=xcntr+xic(ipt)
              ycntr=ycntr+yic(ipt)
              zcntr=zcntr+zic(ipt)

              xcntr0=xcntr0+(xic(ipt) - xmin)
              ycntr0=ycntr0+(yic(ipt) - ymin)
              zcntr0=zcntr0+(zic(ipt) - zmin)
           enddo

C          find averages for centroid
           xfac= one /dble(nen)
           xic_cntr(idx)=xfac*xcntr
           yic_cntr(idx)=xfac*ycntr
           zic_cntr(idx)=xfac*zcntr

           xcntr0=xfac*xcntr0
           ycntr0=xfac*ycntr0
           zcntr0=xfac*zcntr0

C          check precision of centroid calculation
           ierr=0
           if (xic_cntr(idx)-xmin .ne. xcntr0 ) then
            ierr=ierr+1
              if(idebug.gt.9) then
              print*,"Centroid  x = ", xic_cntr(idx)-xmin," - ",xcntr0
     &        ,"Difference: ",(xic_cntr(idx)-xmin)-xcntr0
              endif
           endif
           if (yic_cntr(idx)-ymin .ne. ycntr0 ) then
            ierr=ierr+1
            if(idebug.gt.9) then
              print*,"Centroid  y = ", yic_cntr(idx)-ymin," - ",ycntr0
     &        ,"Difference: ",(yic_cntr(idx)-ymin)-ycntr0
            endif
           endif
           if (zic_cntr(idx)-zmin .ne. zcntr0 ) then
            ierr=ierr+1
            if(idebug.gt.9) then
              print*,"Centroid  z = ", zic_cntr(idx)-zmin," - ",zcntr0
     &        ,"Difference: ",(zic_cntr(idx)-zmin)-zcntr0
            endif
           endif
          if (ierr.gt.0 .and. idebug.gt.0) then
            ierr_eps=ierr_eps+1
            print*,"Precision Diff: ",ierr_eps," Sink Element: ",idx
          endif
          ierr = 0


        enddo
        ipxvals = ipxic_cntr
        ipyvals = ipyic_cntr
        ipzvals = ipzic_cntr


C     use coordinate points
      else
        ipxvals = ipxic
        ipyvals = ipyic
        ipzvals = ipzic
      endif

 
C***********************************************************************
c  Do grid to grid interpolation.
c
c    If finding source POINT:
c      voronoi    - assign value of nearest point
c      if flag_opt = nearest point, fill pt_gtg attribute
c    If finding source ELEMENT:
c      map        - assign value of enclosing element
c      continuous - assign interpolated value from element point field
c
C    Note: All attribute values are real type at this point
c
C***********************************************************************
 
c     setup for writing progress messeges
      num_snk_all = num_snk
      num_snk = mpno
      iwrite = 0
      totsrchd = 0
      totfind = 0
      totflag = 0
      istep = 1
      if ( max(num_snk,num_src) .gt. 500000 ) then
        xperc = 50.
        iperc = 2
      elseif ( max(num_snk,num_src) .gt. 10000 ) then
        xperc = 20.
        iperc = 5
      else
        xperc = 4.
        iperc = 25
      endif
      nwrite = nint(dble(num_snk)/ xperc )

C     Make sink cmo current object
C     update epsilon and min max values
C     object values such as epsilon are taken from current cmo
      call cmo_select(cmosnk,ierr)
      call setsize()

 
C***********************************************************************
C     PAIR POINT TO NEAREST SOURCE POINT
C     DO NEAREST POINT SEARCH or READ LOOKUP ATTRIBUTE pt_gtg
C
C     Valid interpolation method is voronoi (or nearest point)
C     or just fill pt_gtg for other methods using nearest point flag
C
C***********************************************************************
 
      if((intrp_opt(1:7).eq.'voronoi') .or.
     *   (mk_ptatt.eq.1)                      ) then
 
        if(intrp_opt(1:7).ne.'voronoi') then
          just_pt_gtg = 1
          write(logmess,"(a)")
     * 'Building pt_gtg for nearest point flag, element search follows.'
          call writloga('default',0,logmess,0,ierrw)
        endif
 
 
c       Build kdtree to search for nearest source points
        length=5*npts_src
        call mmgetblk('itfound',isubname,ipitfound,length,1,ics)
        if(ics.ne.0 ) call x3d_error(isubname,' get itfound')
        if (if_ptsearch.eq.0) then
          write(logmess,"(a)")
     *    'SKIPPING POINT SEARCH... using lookup attribute pt_gtg'
          call writloga('default',0,logmess,0,ierrw)
 
        else
 
          length=12*npts_src
          call mmgetblk('sbox',isubname,ipsbox,length,2,ierr)
          length=2*npts_src
          call mmgetblk('linkt',isubname,iplinkt,length,1,ics)
          if(ierr.ne.0 .or. ics.ne.0)
     *       call x3d_error(isubname,' get linkt and sbox')
 
          call kdtree0(xic_src,yic_src,zic_src,npts_src,linkt,sbox,ierr)
          if(ierr.ne.0 ) call x3d_error(isubname,' kdtree0 ')
          eps=-1.
          mtfound=0
          xs=alargenumber
          ys=alargenumber
          zs=0.
          if(nsdgeom_src.eq.3) zs=alargenumber
        endif
C     Allocate a work array used in nearestpoint1
C     TODO: Might need to be deallocated at end of subroutine
      length=npts_src
      call mmgetblk('distpossleaf',isubname,ipdistpossleaf,
     $     length,2,icscode)
 
C       Loop through sink points to find nearest source candidates
        do ipt=1,num_snk
         iisnk=mpary(ipt)
         xp=xvals(iisnk)
         yp=yvals(iisnk)
         zp=zvals(iisnk)
 
c        preset work array in case of errors
         work(iisnk)=xflag
 
c        Get nearest point number iisrc from attribute pt_gtg
         if(if_ptsearch.eq.0) then
            iisrc=pt_gtg(ipt)
            if(iisrc.ne.0) then
              mtfound = 1
              itfound(1) = iisrc
            else
              mtfound = -1
            endif
         else
           call mmfindbk('itfound',isubname,ipitfound,ilen,ics)
           if(ics.ne.0 )call x3d_error(isubname,' mmfindbk itfound')
           call nearestpoint1(xp,yp,zp,xs,ys,zs,linkt,sbox,eps,
     *                         npts_src,mtfound,itfound,ierr)
           if(ierr.ne.0 ) call x3d_error(isubname,' nearestpoint1')
         endif
 
C       -----------------------------------------------------------------
C       NEAREST POINT FOUND
c
c        Loop through nearest point candidates
c        choose a single value with tiebreaker option
c          idx is the index to a candidate source point
c          ipt is the index to current sink point
c          iisrc is in the source att, iisnk is the sink attribute
 
         ifirst=1
         do idx=1,mtfound
           iisrc=itfound(idx)
           if (iisrc.le.0) then
              write(logmess,"(a,i15)")
     *        "Using kdtree - invalid source node: ",iisrc
              call writloga('default',0,logmess,0,ierrw)
           endif
 
c          set next value val_try with current iisrc index
c          check to see if we are using requested attribute
c          or just filling pt_gtg for nearest point flag
c
           if(just_pt_gtg.eq.1) then
             if(ctype_pts(1:4).eq.'VINT') then
               val_try = dble(iattsrc_near(iisrc))
             else
               val_try = xattsrc_near(iisrc)
             endif
           else
             val_try = xattsrc(iisrc)
           endif

           if(ifirst.eq.1) then 
              index_prev = iisrc
              val_prev = val_try
              index_end = iisrc
              val_end = val_try
           endif
 
c          use tiebreaker for multiple candidate values
c          save source number and value of chosen candidate 
           if(tie_opt(1:3).eq.'min' .and. ifirst.eq.0) then
               if (val_try.le.val_prev) then
                  index_end = iisrc
                  val_end = val_try
               else
                  index_end = index_prev
                  val_end = val_prev
               endif
               if(idebug.gt.3) then
                write(logmess,'(a,i15,a,1pe14.5e3)')
     *          ' min TIE ASSIGN INDEX: ',index_end,
     *          ' associated value: ',val_end
                call writloga('default',0,logmess,0,ierrw)
               endif
           endif
           if(tie_opt(1:3).eq.'max' .and. ifirst.eq.0) then
               if (val_try.ge.val_prev) then
                  index_end = iisrc
                  val_end = val_try
               else
                  index_end = index_prev
                  val_end = val_prev
               endif
               if(idebug.gt.3) then
                write(logmess,'(a,i15,a,1pe14.5e3)')
     *          ' max TIE ASSIGN INDEX: ',index_end,
     *          ' associated value: ',val_end
                call writloga('default',0,logmess,0,ierrw)
               endif
           endif

           ifirst=0
           index_prev = index_end
           val_prev = val_end
           totsrchd = totsrchd+1

c        assign index of last found element
c        we will assume best is last
c        Fill pt_gtg attribute with found point index
         if(mk_ptatt.eq.1) then
             pt_gtg(iisnk)=index_end
         endif

         enddo
c        End idx Loop through candidates


c        Assign value from final candidate element
c        or flag with special value or nearest point value
c        Unless filling pt_gtg attribute for nearest point flag
         if(mtfound.gt.0) totfind=totfind+1
         if (just_pt_gtg.ne.1) then
           if(mtfound.lt.1) then
             work(iisnk) = xflag
             totflag = totflag + 1
           else
             work(iisnk)=cinterpolate('function',intrp_func,val_end)
           endif
         endif
 
        if(ipt.eq.1) then
           write(logmess,"(a)")
     *'     Sink point   Points searched   Points Found  Percent Done'
           call writloga('default',1,logmess,0,ierrw)
        endif
 
        if((iwrite.eq.nwrite).and.(ipt.ne.num_snk)) then
           iwrite = 0
           write(logmess,"(i15,i17,i15,i9,a2)")
     *     ipt,totsrchd,totfind,istep*iperc,' %'
           call writloga('default',0,logmess,0,ierrw)
           istep = istep + 1
        elseif(ipt .eq. num_snk) then
           write(logmess,"(i15,i17,i15,a)")
     *     ipt,totsrchd,totfind,'    Total'
           call writloga('default',0,logmess,1,ierrw)
        endif
        if(idebug.ge.5) then
         write(logmess,"(i17,a,1pe14.5e3,1pe14.5e3,1pe14.5e3,a)")
     *     iisnk,' sink point at ( ',xp,yp,zp,' )'
           call writloga('default',0,logmess,0,ierrw)
           if (mtfound.lt.1 .and. just_pt_gtg.ne.1) then
             write(logmess,"(f17.5,a,i17)")
     *       work(iisnk),'  error FLAG assigned to:  ',iisnk
             call writloga('default',0,logmess,0,ierrw)
           elseif (just_pt_gtg.ne.1) then
             write(logmess,"(f17.5,a,i17)")
     *       work(iisnk),' value assigned from point: ',index_end
             call writloga('default',0,logmess,0,ierrw)
           else
             write(logmess,"(i15,a,i17)")
     *       iisnk,' pt_gtg assigned source point: ',index_end
             call writloga('default',0,logmess,0,ierrw)
           endif
         endif
 
 
        iwrite = iwrite+1
        idone = ipt
        enddo
C       End ipt Loop through sink points
 
      endif
C     End POINT
C     End filling values for work array for VORONOI method
C     and/or filling nearest point values for attribute pt_gtg
C***********************************************************************
 
 
C***********************************************************************
C     PAIR POINTS TO SOURCE ELEMENTS
C     DO ELEMENT SEARCH or READ LOOKUP ATTRIBUTE el_gtg
C     Valid interpolation methods are:
C       map        - uses source element value
C       continuous - uses source element point values
C***********************************************************************
      if ( (intrp_opt(1:3).eq.'map')      .or.
     *     (intrp_opt(1:10).eq.'continuous') ) then
 
        call cmo_select(cmosnk,ierr)
        iwrite = 0
        totsrchd = 0
        totfind = 0
        totflag = 0
        volzero = 0
        istep = 1
 
c       Build kdtree to search source elements, else use el_gtg
        if(if_elsearch.eq.0) then
          write(logmess,"(a)")
     *    'SKIPPING ELEMENT SEARCH... using lookup attribute el_gtg'
          call writloga('default',0,logmess,0,ierrw)
 
        else
 
          if(idebug.le.1) call writset('stat','tty','off',ierrw)
 
          len=icharlnf(cmosrc)
          cbuff = 'cmo select '//cmosrc(1:len)//' ;  finish'
          call dotaskx3d(cbuff,ierr)
          cbuff = 'cmo kdtree build ; finish'
          call dotaskx3d(cbuff,ierr)
          call cmo_get_info('linkt',cmosrc,iplinkt,ilen,ityp,ierr)
          call cmo_get_info('sbox',cmosrc,ipsbox,ilen,ityp,ics)
          if(ierr.ne.0 .or. ics.ne.0)
     *     call x3d_error(isubname,' get linkt and sbox')
 
          if(idebug.le.1) call writset('stat','tty','on',ierrw)
 
        endif
        length=5*nelm_src
        call mmgetblk('iefound',isubname,ipiefound,length,1,ics)
        if(ics.ne.0 ) call x3d_error(isubname,' get iefound')
 
c       Loop through each sink point to find enclosing source element
        do ipt = 1,num_snk
          iisnk=mpary(ipt)
          xp=xvals(iisnk)
          yp=yvals(iisnk)
          zp=zvals(iisnk)
 
c         preset work array in case of errors
          if(flag_opt(1:7).eq.'nearest') then
            jj = pt_gtg(iisnk)
            if(ctype_pts(1:4).eq.'VINT') then
                xflag = dble(iattsrc_near(jj))
            else
                xflag = xattsrc_near(jj)
            endif
          endif
          work(iisnk) = xflag
 
c         Get enclosing element number iisrc from attribute
          iisrc = 0
          if(if_elsearch.eq.0) then
            iisrc=el_gtg(iisnk)
            if(iisrc.gt.0) then
              nefound = 1
              iefound(1) = iisrc
              inelement = 1
            else
              nefound = -1
              inelement = -1
            endif

C         otherwise search through candidate elemements
          else
            inelement = -1
            call mmfindbk('iefound',isubname,ipiefound,ilen,ics)
            call cmo_select(cmosnk,ierr)
            call get_epsilon('epsilonl',eps)
            call retrieve_within_eps(xp,yp,zp,linkt,sbox,eps,
     *                           nefound,iefound,ierr)

          endif
          if (idebug.ge.5) then
           write(logmess,"(a)")
     *     '----------------------------------------------------------'
           call writloga('default',0,logmess,0,ierrw)
           write(logmess,"(a,i14)")
     *     'Search Point: ',iisnk
           call writloga('default',0,logmess,0,ierrw)
           write(logmess,"(a,e20.12,a,i9)")
     *     'Retrieve within epsilonl: ',eps,' candidates: ',nefound
           call writloga('default',0,logmess,0,ierrw)
          endif
 
 
C       -----------------------------------------------------------------
C       SINK POINT FOUND CANDIDATES for INSIDE ELEMENT
C       If el_gtg used, element is known and there is no search
c       Loop through each of the candidate elements (or known element)
c       A value is calculated for each valid candidate
c       and tiebreaker applied if more than one solution
c         inelement is set to 1 if point is inside, else -1
c         ipt is the index to current sink point
c         idx is the index to a candidate source element
c         iisrc is in the source att, iisnk is the sink attribute

          call cmo_select(cmosnk,ierr)
          ifirst=1
          ifound=0
          val_save=0.0
          index_save = 0
          index_end = 0
          do idx=1,nefound
            inflag = 0
            iisrc=iefound(idx)
            index_end = iisrc
            if (iisrc.le.0) then
              write(logmess,"(a,i15)")
     *        "Using kdtree - invalid source element: ",iisrc
              call writloga('default',0,logmess,0,ierrw)
            endif

C           copy into local element arrays
            ielmtyp = itettyp_src(iisrc)
            do i = 1,nelmnen(ielmtyp)
              j = itet_src(itetoff_src(iisrc)+i)
              xnew1(i) = xic_src(j)
              ynew1(i) = yic_src(j)
              znew1(i) = zic_src(j)
              if(clen_src(1:5).eq.'nnode') xfield(i)=xattsrc(j)
            enddo
            xnew2(1) = xp
            ynew2(1) = yp
            znew2(1) = zp


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c           check that src point is inside or on element candidate
c           check for point inside the element 
c           inelement < 0 are points found outside the object 
c           the second object is always the query point
            if(if_elsearch.ne.0) then

               if (idebug.gt.0) then
                 write(logmess,"(a,i10,a,i3,a,i10)")
     &           "*** SEARCH SINK Pnt: ",ipt," Candidate: ",idx
     &           ,"   SOURCE Elem: ",iisrc
                 call writloga('default',1,logmess,0,ierror)
               endif

               ielmtyp2 = ifelmpnt
               inelement=idebug
               call inside_element(ielmtyp,xnew1,ynew1,znew1,
     *                        xnew2(1),ynew2(1),znew2(1),inelement)
               inflag = inelement
            endif

c           flag indicates point or edge that succeeded for inside
            if (idebug.ge.5 .and. inelement.ge.0) then
              if (inelement.lt.20) then
                write(logmess,"(a,i14,a,i5)")
     *          'FOUND in element: ',iisrc,
     *          ' flag: ',inflag
              else
                write(logmess,"(a,i14,a,i5)")
     *          'FOUND in element near point: ',iisrc,
     *          ' flag: ',inflag
              endif
              call writloga('default',0,logmess,0,ierrw)
            endif

            if (idebug.ge.5) then

              write(logmess,"(a,1pe20.12e2,1pe20.12e2,1pe20.12e2)")
     &        "Element xyz(1):  ", xnew1(1),ynew1(1),znew1(1)
              call writloga('default',0,logmess,0,ierror)
              write(logmess,"(a,1pe20.12e2,1pe20.12e2,1pe20.12e2)")
     &        "Element xyz(2):  ", xnew1(2),ynew1(2),znew1(2)
              call writloga('default',0,logmess,0,ierror)
              write(logmess,"(a,1pe20.12e2,1pe20.12e2,1pe20.12e2)")
     &        "Element xyz(3):  ", xnew1(3),ynew1(3),znew1(3)
              call writloga('default',0,logmess,0,ierror)

              write(logmess,"(a,1pe20.12e2,1pe20.12e2,1pe20.12e2)")
     &        "Query Pnt:   ", xnew2(1),ynew2(1),znew2(1)
              call writloga('default',0,logmess,0,ierror)

             endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            if (tie_opt2(1:3).eq.'mat' ) inelement = 1
 
c           check that this element has volume, skip if continuous intrp
            call volume_element(ielmtyp,xnew1,ynew1,znew1,volelm)
            if(volelm.lt.epsilonvol) then
              if (intrp_opt(1:10).eq.'continuous') inelement = -2
              volzero = volzero+1
            endif

 
            if (tie_opt2(1:3).eq.'mat' ) then
            if (idebug.ge.9 .and. inelement.lt.0) then
              write(logmess,"(a,i14,a,i14,a,i14)")
     *        'NOT in element: ',iisrc,' mat: ',imat_src(iisrc),
     *        ' point mat: ',imat(iisnk)
              call writloga('default',0,logmess,0,ierrw)
            endif
            endif
 
c           choose candidate elements with material equal to imt
            if(tie_opt2(1:3).eq.'mat' .and. inelement.ge.0) then
 
              if (imat(iisnk) .ne. imat_src(iisrc)) then
                inelement = -1
                if (idebug.ge.9) then
                  write(logmess,"(a,i14,a,i14,a,i14)")
     *            'NOT element: ',iisrc,' mat: ',imat_src(iisrc),
     *            ' point mat: ',imat(iisnk)
                  call writloga('default',0,logmess,0,ierrw)
                endif
              endif
            endif

 
c           candidate element confirmed
c           interpolate values, apply tie-breaker

            if (inelement.ge.0) then
              ifound = 1

              if (idebug.ge.3) then
                if (tie_opt2(1:3).eq.'mat' ) then
                write(logmess,"(a,i14,a,i14,a,i14)")
     *          'GOOD  element: ',iisrc,' mat: ',imat_src(iisrc),
     *          ' point mat: ',imat(iisnk)
                call writloga('default',0,logmess,0,ierrw)
                endif
              endif
 
C             ***************************************************
C             MAP METHOD FROM ELEMENT VALUE
C             attribute is type element
 
              if(intrp_opt(1:3).eq.'map') then
                val_try = xattsrc(iisrc)
                val_try = cinterpolate('function',intrp_func,val_try)
 
C             ***************************************************
C             CONTINUOUS METHOD FROM ELEMENT POINT FIELD
C             attribute is type node
C             interpolation function is applied inside function
              else
 
                val_try=cinterpolate_elem('continuous',intrp_func,
     *            xp,yp,zp,xnew1,ynew1,znew1,xfield,ielmtyp)
 
              endif

C             ***************************************************
C             END INTERPOLATION METHODS
 
c             Use tie min or max to choose a value from candidates
c             These are usually on edge so either elem is valid
c             pick best of those found inside
c             val_try is current
c             val_end is best so far

c TAM
c             check flag from inside element to select
c             result with best confidence
c             inflag as 0 is best
c             if inflag > 30  possible but least confidence
c             if inflag 21,22,23 - more possible than 30's
c             In general, the closer to 0, the higher the confidence

c             save first value as previous
              if(ifirst.eq.1) then
                index_prev = iisrc
                val_prev = val_try 
                inflag_prev = inflag
                index_save = iisrc
                val_save = val_try 
                inflag_save = inflag

c             compare any new candidates against previous 
              else 

                intie = 0

                if (inflag_prev.ne.0 .and. inflag.eq.0) then
                  index_save = iisrc
                  val_save = val_try
                  inflag_save = inflag
                elseif (inflag_prev.gt.20 .and. inflag.lt.20) then
                  index_save = iisrc
                  val_save = val_try
                  inflag_save = inflag
                elseif (inflag_prev.gt.30 .and. inflag.lt.30) then
                  index_save = iisrc
                  val_save = val_try
                  inflag_save = inflag
                endif

                if (inflag_prev.eq.0 .and. inflag.eq.0) then
                   intie = 1
                endif
                if (inflag_prev.gt.0 .and. inflag.gt.0 .and.
     *              inflag_prev.lt.20 .and. inflag.lt.20 ) then
                   intie = 1
                endif
                if (inflag_prev.ge.20 .and. inflag.ge.20 .and.
     *              inflag_prev.lt.30 .and. inflag.lt.30 ) then
                   intie = 1
                endif
                if (inflag_prev.ge.30 .and. inflag.ge.30 ) then 
                   intie = 1
                endif

c               break tie with min or max value of associated element
                if (intie.gt.0) then
                  if(tie_opt(1:3).eq.'min') then
                     if (val_try.lt.val_prev) then
                         index_save = iisrc
                         val_save = val_try
                         inflag_save = inflag
                      else
                         index_save = index_prev
                         val_save = val_prev
                         inflag_save = inflag_prev
                      endif
                   endif
                   if(tie_opt(1:3).eq.'max') then
                     if (val_try.gt.val_prev) then
                         index_save = iisrc
                         val_save = val_try
                         inflag_save = inflag
                      else
                         index_save = index_prev
                         val_save = val_prev
                         inflag_save = inflag_prev
                      endif
                   endif
                   if(idebug.gt.0) then
                   write(logmess,'(a3,a,i15,a,i5,1pe14.5e3)')
     *             tie_opt(1:3),' TIE ASSIGN INDEX: ',index_save,
     *             ' with flag and value: ',inflag_save,val_save
                   call writloga('default',0,logmess,0,ierrw)
                   endif
                 endif

              endif

c             overwrite previous with winning index and value
              ifirst=0
              index_prev = index_save
              val_prev = val_save
              inflag_prev = inflag_save
              totsrchd=totsrchd+1

            endif
c           End found element

          enddo
c         End idx Loop through candidate elements
          index_end = index_save
          val_end = val_save

c         Assign value from final candidate element
c         or flag with special value or nearest point value
          if(ifound.lt.1) then
            work(iisnk) = xflag
            totflag = totflag+1
          else
            work(iisnk) = val_end
            totfind = totfind+1
            if(mk_elatt .eq. 1) then
               el_gtg(iisnk) = index_end
            endif
          endif
 
          if(ipt.eq.1) then
            write(logmess,"(a)")
     *'     Sink point   Elems Searched  Elements Found  Percent Done'
           call writloga('default',1,logmess,0,ierrw)
          endif
 
          if((iwrite.eq.nwrite).and.(ipt.ne.num_snk)) then
            iwrite = 0
            write(logmess,"(i15,i17,i15,i9,a2)")
     *      ipt,totsrchd,totfind,istep*iperc,' %'
            call writloga('default',0,logmess,0,ierrw)
            istep = istep + 1
          elseif(ipt .eq. num_snk) then
            write(logmess,"(i15,i17,i15,a)")
     *      ipt,totsrchd,totfind,'    Total'
            call writloga('default',0,logmess,1,ierrw)
            if (volzero.gt.0) then
              write(logmess,"(a,i17)")
     *        'WARNING: Negative-volume source elements: ',volzero
              call writloga('default',0,logmess,0,ierrw)
            endif
          endif

c         use end source elem number which is 0 if no candidates
c         old code would wrongly write elem number of last found
          if(idebug.gt.0 ) then
            write(logmess,"(i17,a,1pe14.5e3,1pe14.5e3,1pe14.5e3,a)")
     *      iisnk,' sink point at ( ',xp,yp,zp,' )'
            call writloga('default',0,logmess,0,ierrw)

            if (ifound.lt.1) then

              if (iisrc.le.0) then
              write(logmess,"(f17.5,a)")
     *      work(iisnk),'  FLAG assigned, element NOT found. '
              call writloga('default',0,logmess,0,ierrw)
              else
              write(logmess,"(f17.5,a,i5)")
     *      work(iisnk),'  FLAG assigned, NOT in elem: ',iisrc
              call writloga('default',0,logmess,0,ierrw)
              endif

            else
              write(logmess,"(f17.5,a,i17)")
     *      work(iisnk),' value assigned from element: ',index_end
              call writloga('default',0,logmess,0,ierrw)
            endif

          endif
 
          iwrite = iwrite+1
          idone = ipt
        enddo
c       End ipt Loop through sink points
 
        if (idebug.le.1) call writset('stat','tty','off',ierrw)
C       if(if_elsearch.gt.0) then
C         call cmo_select(cmosrc,ierr)
C         cbuff = 'cmo kdtree release ; finish'
C         call dotaskx3d(cbuff,ierr)
C         call cmo_select(cmosnk,ierr)
C       endif
 
      endif
C     End ELEMENT
C***********************************************************************
 
C     Assign the final interpolated values to the sink attribute
      do ipt=1,num_snk
         ii=mpary(ipt)
         if(ctype_snk.eq.'VINT')then
           iattsnk(ii)= nint(work(ii))
         else
           xattsnk(ii)=work(ii)
         endif
      enddo
 
      ierror=0
 9999 continue
 
c     if attributes were created and not keeping, delete them now
      if (idebug.le.1) call writset('stat','tty','off',ierrw)
      if(ikeep.ne.1 .and. ipt_exist.ne.0) then
        len=icharlnf(cmosnk)
        cbuff = 'cmo DELATT '//cmosnk(1:len)//'/pt_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
      endif
      if(ikeep.ne.1 .and. iel_exist.ne.0) then
        len=icharlnf(cmosnk)
        cbuff = 'cmo DELATT '//cmosnk(1:len)//'/el_gtg ;  finish'
        call dotaskx3d(cbuff,ierr)
      endif
 
      len=icharlnf(cmosnk)
      cbuff = 'cmo select '//cmosnk(1:len)//' ;  finish'
      call dotaskx3d(cbuff,ierr)
 
 
c     Final screen output
      call writset('stat','tty','on',ierrw)
 
      if (num_snk.ne.num_snk_all) then
         write(logmess,"(a,i15,a,i15)") 'Indexed sink points: ',
     *      num_snk,' Total unchanged: ',num_snk_all-num_snk
         call writloga('default',0,logmess,0,ierrw)
      endif
 
      if (totfind.le.0) then
        write(logmess,"(a)")
     * 'ERROR: INTRP found zero sink points inside source grid.'
        call writloga('default',0,logmess,0,ierrw)
      elseif ((idone-totfind).gt.0) then
         write(logmess,"(a,i15)")
     * 'WARNING: Sink points not inside source grid: ',idone-totfind
         call writloga('default',0,logmess,0,ierrw)
      endif
 
      if (totflag.gt.0) then
        if (flag_opt(1:7).eq.'nearest') then
          write(logmess,"(a,i15)")
     *    'Total sink points flagged with nearest point value: ',totflag
          call writloga('default',0,logmess,0,ierrw)
        else
          write(logmess,"(a,i15,a,f14.2)")
     *    'Total sink points marked: ',totflag,' with flag: ',xflag
          call writloga('default',0,logmess,0,ierrw)
        endif
      endif
 
      write(logmess,"(a,a,a)")
     * 'intrp/',intrp_opt(1:icharlnf(intrp_opt)),' done.'
      call writloga('default',0,logmess,1,ierrw)
 
      if(idebug.gt.0) call mmverify()
 
c     Return here if error is found before setup is done
 9000 if(ierror.ne.0) then
         write(logmess,'(a)')'FATAL ERROR: INTRP unable to begin.'
         call writloga('default',1,logmess,1,ierrw)
      endif
      if(if_elsearch.gt.0 .or. if_ptsearch.gt.0) then
          call cmo_select(cmosrc,ierr)
          cbuff = 'cmo kdtree release ; finish'
          call dotaskx3d(cbuff,ierr)
      endif
      call mmrelprt(isubname,ics)
      call cmo_select(cmosnk,ierr)
 
      return
      end
C     END intrp_gtg
 
 
 
