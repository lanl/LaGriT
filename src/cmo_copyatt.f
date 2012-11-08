c#######################################################################
 
      subroutine cmo_copyatt(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c     this routine copies an attribute field to another
c        (see also subroutine copyatt_mpary_lg below)
c
c     there is currently no provision for indexed sets
c
c     copy element to node: ! Not for 2 different mesh objects !
c     This routine allows the value of an element attribute 
c     to be copied to each of the element's node vertices. 
C     It is assumed that each element has its own set of node points. 
c     These chains can be formed by using cmo/set_id and settets. 
c     An error messege will be displayed indicating two attributes
c     with differing lengths, this can be ignored for this option.
c
c             ..........................................................
c     syntax -
c
c     cmo/copyatt  / cmosink /cmo_src/ attnam_sink / attnam_src
c
c        cmosink attnam_sink   - is the cmo and attribute
c                                that values will be written to
c        cmo_src attnam_src    - is the cmo and attribute
c                                that values will be copied from
c
c     examples:
c
c       cmo / copyatt / cmosnk / cmosrc / itetclr / itetclr
c       cmo / copyatt / cmosnk / cmosrc / itetclr
c         - both versions will copy itetclr field from cmosrc to cmosnk
c
c       cmo / addatt / cmosnk / elevation
c       cmo / copyatt / cmosnk cmosrc / elevation zic /
c         - will copy the zic field of cmosrc to the
c           elevation field of cmosnk
c
c       reserved words -all- -xyx- nnode nelement are recognized
c
c       cmo / copyatt / cmo / cmo / itetsav / itetclr
c       cmo set_id cmo element itetclr
c       settets color_points
c       resetpts itp
c       settets
c       cmo /copyatt / cmo / cmo / imt / itetsav 
c          - copy element itetclr values into itetsav
c            assign itetclr its element number
c            set parent-child chains so each elem has own set of nodes
c            copy the saved element values to each element node imt
c
c     input arguments -
c
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
c
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c     change history -
C
C   $Log: cmo_copyatt.f,v $
C   Revision 2.00  2007/11/05 19:45:48  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   17 Jun 2004 10:11:44   gable
CPVCS    Allow copy of node attribute into an element attribute
CPVCS    or element attribute into a node attribute, if and only
CPVCS    if they have the same lenght.
CPVCS    
CPVCS       Rev 1.6   28 Nov 2001 18:37:16   tam
CPVCS    Allow value of an element attribute to be copied
CPVCS    to each of the element's node vertices.
CPVCS     is assumed that each element has its own set
CPVCS    of parent-child chains.
CPVCS    
CPVCS       Rev 1.5   07 Aug 2001 13:47:20   dcg
CPVCS    use same pointer for character attributes as others
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 08:49:42 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Tue Feb 01 13:46:30 2000   dcg
CPVCS
CPVCS       Rev 1.4   20 Jan 2000 10:11:16   jtg
CPVCS    fixed Log line, changes is_network to jtet_cycle_max
CPVCS
CPVCS       Rev 1.2   Wed Dec 01 13:34:16 1999   jtg
CPVCS    added Log line
CPVCS    Initial revision.
 
c
c#######################################################################
c
       include "local_element.h"
c
c#######################################################################
c
c
      !not used! pointer ( ipmpary1 , mpary1)
      !not used! integer mpary1
 
c      pointer (ipisetwd,isetwd), (ipxtetwd,xtetwd)
c      integer itp1(*),isetwd(*),xtetwd(*)
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer nlen_copy,icmotype,ierror,ilen,lentyp_snk
 
      integer indxtyp, i,idx,nen,ipt, inxt, ilast, istart, iend
c      integer ipt_start, ipt_stride, ipointi, ipointj
      integer ics,ierr,ier2,ierrw,
     * len,ier,lentyp_src,itin,attlen,
     * ipt1,ipt2,ipt3, ipt1_sav,ipt2_sav,ipt3_sav,
     * k,i1,i2,l, elem_to_vertices,
     * ityp,itotal,nset,ivalue
c     integer mpno,ifound,imin,imax
 
      integer attyp,attyp2
      integer irank_src,irank_sink,index,index2
      integer nelem_src,nnode_src,nnode_snk,nelem_snk
c
c      real*8  xvalue,xmin,xmax
      integer printopt
      integer NOWRITE, VALUES, LIST, MINMAX
      logical ivalid, mmset
      integer icharlnf
c
      pointer (ipitet, itet)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      integer itet(*),itettyp(*),itetoff(*)
c
      pointer(ipxvalsink,xvalsink)
      pointer(ipxvalsrc,xvalsrc)
      pointer(ipxvalsink,cvalsink)
      pointer(ipxvalsrc,cvalsrc)
      pointer(ipxvalsink,ivalsink)
      pointer(ipxvalsrc,ivalsrc)
      real*8 xvalsink(*),xvalsrc(*)
      integer ivalsrc(*),ivalsink(*)

c
      character*32 attnam, attnam2
      character*32 csrcnam, csnknam
      character*32 isubname
      character*32 ich1,ich2,ich3
      character*32 cmosrc,crank,clength,ctype,cinter,cpers,cio2,cio
      character*32 cmosink, ctype2,crank2,clength2,cinter2,cpers2
      character*32 cvalsrc(*),cvalsink(*)
      character*132 logmess, cbuff
 
      data NOWRITE, VALUES, LIST, MINMAX /0,1,2,3/
c
c
c#######################################################################
c
c
c     ******************************************************************
c
      isubname = 'cmo_copyatt'
      mmset     = .false.
      printopt  =  NOWRITE
      indxtyp   = 1
      ierror    = 1
      elem_to_vertices = 0
      
C
c     ******************************************************************
c
C     Parse the required commands
c      1     2         3         4          5          6
c     cmo / copyatt / cmosink / cmosrc / sink_att /  src_att
c     or
c     cmo / copyatt / cmosink / cmosrc / sink and src att
c
c     ******************************************************************
 
      ilast=nwds
 
c     3 - get mesh object sink
      if (msgtype(3).eq.3) then
        cmosink=cmsgin(3)
        ilen=icharlnf(cmosink)
        if (cmosink(1:ilen).eq.'-def-') then
           call cmo_get_name(cmosink,ierror)
           if(ierror.ne.0) then
             write(logmess,'(a)') 'CMO found bad mesh object'
             call writloga('default',0,logmess,0,ierrw)
             go to 9999
           endif
        endif
      else
         ierr=1
         goto 9991
      endif
 
c     4 - get mesh object source
      if (msgtype(4).eq.3) then
        cmosrc=cmsgin(4)
        ilen=icharlnf(cmosrc)
        if (cmosrc(1:ilen).eq.'-def-') then
           call cmo_get_name(cmosrc,ierror)
           if(ierror.ne.0) then
             write(logmess,'(a)') 'CMO ADDATT: found bad mesh object'
             call writloga('default',0,logmess,0,ierrw)
             go to 9999
           endif
        endif
      else
         ierr=1
         goto 9991
      endif
 
      inxt = 5
c     5 - get sink attribute
      if (nwds.gt.4 .and. msgtype(5).eq.3 ) then
         attnam2 = cmsgin(5)
         inxt = inxt+1
      else
         attnam2  = '-all-'
      endif
 
c     6 - get source attribute if different from sink
      if (nwds.gt.5 .and. msgtype(6).eq.3) then
         attnam = cmsgin(6)
         inxt = inxt+1
      else
         attnam  = attnam2
      endif
 
 
C     Not implemented yet
C     define users selected point set
      ipt1=1
      ipt2=0
      ipt3=0
      ich1=' '
      ich2=' '
      ich3=' '
      if (inxt.le.ilast) then
        if (ilast.ge.inxt .and. msgtype(inxt).eq.1 ) then
          ipt1=imsgin(inxt)
          inxt=inxt+1
          if (ilast.ge.inxt .and. msgtype(inxt).eq.1 ) ipt2=imsgin(inxt)
          inxt=inxt+1
          if (ilast.ge.inxt .and. msgtype(inxt).eq.1 ) ipt3=imsgin(inxt)
          indxtyp=1
        elseif (ilast.ge.inxt .and. msgtype(inxt).eq.3) then
          ich1=cmsgin(inxt)
          inxt=inxt+1
          if (ilast.ge.inxt .and. msgtype(inxt).eq.3 ) ich2=cmsgin(inxt)
          inxt=inxt+1
          if (ilast.ge.inxt .and. msgtype(inxt).eq.3 ) ich3=cmsgin(inxt)
          indxtyp=3
          write(logmess,'(a)') 'CMO ADDATT: point set not implemented.'
          call writloga('default',0,logmess,0,ierrw)
          go to 9999
        endif
      endif
      ierr=0
 9991 if (ierr.ne.0) then
        write(logmess,'(a)')'cmo/copyatt: not a valid syntax'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999
      endif

C     ******************************************************************
C     END Parse commands
 
c     check mesh cmo's
c     the  mesh objects must have nodes -- if none return
c     both mesh objects must have nodes
c     they do not need to have elements
c     set nnode_src and nelem_src lengths to copy from

      call cmo_get_info('nnodes',cmosink,nnode_snk,ilen,icmotype,ierr)
      call cmo_get_info('nnodes',cmosrc,nnode_src,ilen,icmotype,ier2)
      if (ierr.ne.0 .or. ier2 .ne. 0)
     *       call x3d_error('cmo get_info nnodes',isubname)

      if(nnode_src.eq.0) then
        write(logmess,'(a,a)')'nnode source = 0 in cmo ',cmosrc
        call writloga('default',0,logmess,0,ierrw)
        go to 9999
      endif
      if(nnode_snk.eq.0) then
        write(logmess,'(a,a)')'nnode sink = 0 in cmo ',cmosink
        call writloga('default',0,logmess,0,ierrw)
        go to 9999
      endif

      call cmo_get_info('nelements',cmosink,nelem_snk,ilen,icmotype,ier)
      call cmo_get_info('nelements',cmosrc,nelem_src,ilen,icmotype,ier2)
      if(nelem_src.eq.0) then
        write(logmess,'(a,a)')'Warning: nelements = 0 in cmo ',cmosrc
        call writloga('default',0,logmess,0,ierrw)
      endif
      if(nelem_snk.eq.0) then
        write(logmess,'(a,a)')'Warning: nelements = 0 in cmo ',cmosink
        call writloga('default',0,logmess,0,ierrw)
      endif
 
c     figure out loop through all attributes
c     usually this is one to one, but we support multiple attribute copy
      istart = 1
      if (attnam(1:5).eq.'nnode' .or. attnam(1:8).eq.'nelement' .or.
     *    attnam(1:5).eq.'-all-'  )  then
        call cmo_get_info('number_of_attributes',cmosrc,iend,
     *                     ilen,ityp,ierror)
        if (ierror.ne.0) call x3d_error(isubname,'number_of_attributes')
 
      elseif (attnam(1:5).eq.'-xyz-') then
        iend = 3
      else
        iend = 1
      endif
 
c     ******************************************************************
C     loop through all cmo attributes
C     usually 1 to 1, but also -all-, nnode, nelement, or -xyz-
C     take action on those chosen by user
 
      itotal=0
      ivalid=.false.
      ipt1_sav=ipt1
      ipt2_sav=ipt2
      ipt3_sav=ipt3
 
      do i = istart, iend
        ier=0
        nset=0
 
C       ................................................................
C       decide if there is output for this attribute based on src cmo

        if(attnam(1:5).eq.'-all-') then
c       ...get name for each of all attributes except scalar

          call cmo_get_attribute_name(cmosrc,i,csrcnam,ier)
          call cmo_get_attparam(csrcnam,cmosrc,index,ctype,crank
     *       ,clength,cinter,cpers,cio,ierror)
          if (clength(1:6).eq.'scalar' ) then
           write(logmess,'( a,a,a,a )')
     *            'CMO_COPYATT: Uncopied attribute: ',
     *            cmosrc(1:icharlnf(cmosrc)), '  ',
     *            csrcnam(1:icharlnf(csrcnam))
           call writloga('default',0,logmess,0,ierr)
          else
            ivalid=.true.
            csnknam = csrcnam
          endif
 
        elseif(attnam(1:5).eq.'nnode') then
c       ...get name for each of attributes of length nnode

          call cmo_get_attribute_name(cmosrc,i,csrcnam,ier)
          call cmo_get_attparam(csrcnam,cmosrc,index,ctype,crank,
     *       clength,cinter,cpers,cio,ierror)
          csnknam = csrcnam
          if (clength(1:4).eq.'nnod') ivalid=.true.
 
        elseif(attnam(1:8).eq.'nelement') then
c       ...copy all attributes of length nelement
         inxt = inxt+2
          call cmo_get_attribute_name(cmosrc,i,csrcnam,ier)
          call cmo_get_attparam(csrcnam,cmosrc,index,ctype,crank,
     *       clength,cinter,cpers,cio,ierror)
          csnknam = csrcnam
          if (clength(1:4).eq.'nele') ivalid=.true.
 
        elseif(attnam(1:5).eq.'-xyz-' .and. i.eq.1) then
c       ...get the coordinate names

          csrcnam='xic'
          csnknam = csrcnam
          ivalid=.true.
        elseif(attnam(1:5).eq.'-xyz-' .and. i.eq.2) then
          csrcnam='yic'
          csnknam = csrcnam
          ivalid=.true.
        elseif(attnam(1:5).eq.'-xyz-' .and. i.eq.3) then
          csrcnam='zic'
          csnknam = csrcnam
          ivalid=.true.
        else
          csrcnam=attnam
          csnknam=attnam2
          ivalid=.true.
 
c         to avoid problems with sbnloc not recognizing att name
          if(csrcnam(1:icharlnf(csrcnam)).eq.'imt')csrcnam='imt1'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'itp')csrcnam='itp1'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'icr')csrcnam='icr1'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'isn')csrcnam='isn1'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'ign')csrcnam='ign1'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'xic1')csrcnam='xic'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'yic1')csrcnam='yic'
          if(csrcnam(1:icharlnf(csrcnam)).eq.'zic1')csrcnam='zic'
 
          if(csnknam(1:icharlnf(csnknam)).eq.'imt')csnknam='imt1'
          if(csnknam(1:icharlnf(csnknam)).eq.'itp')csnknam='itp1'
          if(csnknam(1:icharlnf(csnknam)).eq.'icr')csrcnam='icr1'
          if(csnknam(1:icharlnf(csnknam)).eq.'isn')csnknam='isn1'
          if(csnknam(1:icharlnf(csnknam)).eq.'ign')csnknam='ign1'
          if(csnknam(1:icharlnf(csnknam)).eq.'xic1')csnknam='xic'
          if(csnknam(1:icharlnf(csnknam)).eq.'yic1')csnknam='yic'
          if(csnknam(1:icharlnf(csnknam)).eq.'zic1')csnknam='zic'
 
        endif
        if (ier.ne.0) goto 9999
 
C       ................................................................
C       get attributes based on current csrcnam and csnknam  
        if (ivalid) then
 
C         SOURCE CMO STORAGE
c         get block name, pointer,mem name, and len of attribute
          call cmo_get_attparam(csrcnam,cmosrc,index,ctype,crank,
     *       clength,cinter,cpers,cio,ierror)
 
          if(ierror.eq.0) then
c           ...attribute exists
            len=icharlnf(csrcnam)
          else
c           ...can not copy from non-existing attribute
            write(logmess,'(a,a,a)')
     *      'CMO_COPYATT error: attribute does not exist: ',
     *       cmosrc, csrcnam
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
          endif
 
C         SINK CMO STORAGE
C         get length and adjust sink cmo attribute if needed
c         for now warn user so they can define cmo, add code here late
          call cmo_get_attparam(csnknam,cmosink,index2,ctype2,crank2,
     *       clength2,cinter2,cpers2,cio2,ierror)
 
          if(ierror.eq.0) then
c            ...attribute exists
            len=icharlnf(csnknam)
          else
c           ...need to create attribute in sink cmo
            write(logmess,'(a,a,a)')
     *      'CMO_COPYATT WARNING: attribute does not exist: 
     *       attribute now created: ', csnknam
            call writloga('default',0,logmess,0,ierrw)
            call cmo_get_attparam(csrcnam,cmosrc,index,ctype,crank,
     *         clength,cinter,cpers,cio,ierror)
            cbuff = 'cmo/addatt/' // cmosink(1:icharlnf(cmosink)) //
     *      '/' // csnknam(1:icharlnf(csnknam)) // '/'
     *      // ctype(1:icharlnf(ctype)) // '/'
     *      // crank(1:icharlnf(crank)) // '/'
     *      // clength(1:icharlnf(clength)) // '/'
     *      // cinter(1:icharlnf(cinter)) // '/'
     *      // cpers(1:icharlnf(cpers)) // '/'
     *      // cio(1:icharlnf(cio)) // ';finish'
            call dotaskx3d(cbuff, ierror)
            call cmo_get_attparam(csnknam,cmosink,index2,ctype2,crank2,
     *         clength2,cinter2,cpers2,cio2,ierror)
          endif
 
C          Done getting Storage Block info for attributes
c          get mesh object type=attyp, length=lentyp_src and rank=irank_src
c          ...attributes exist, continue definitions
 
C         SET lentyp_snk and lentyp_src to detirmine copy type
          call cmo_get_info(clength2,cmosink,attlen,ityp,itin,ierror)
          if(clength2(1:6).eq.'nnodes') lentyp_snk=1
          if(clength2(1:9).eq.'nelements') lentyp_snk=2
          if(clength2(1:6).eq.'scalar') lentyp_snk=3
 
          call cmo_get_info(clength,cmosrc,attlen,ityp,itin,ierror)
          if(clength(1:6).eq.'nnodes') lentyp_src=1
          if(clength(1:9).eq.'nelements') lentyp_src=2
          if(clength(1:6).eq.'scalar') lentyp_src=3

c         some of this is reported below so this may be redundant
          if (lentyp_src.ne.lentyp_snk) then
           if((lentyp_snk .eq. 1).and.(lentyp_src .eq. 2)) then
             if(nnode_snk .eq. nelem_src)then
               call x3d_error('WARNING:copy element att into node att ',
     *                          isubname)
             else
               call x3d_error('incompatible att length types for ',
     *                          isubname)
             endif
           elseif((lentyp_snk .eq. 2).and.(lentyp_src .eq. 1)) then
             if(nnode_src .eq. nelem_snk)then
               call x3d_error('WARNING:copy node att into elem att ',
     *                          isubname)
             else
               call x3d_error('incompatible att length types for ',
     *                          isubname)
             endif
           else
               call x3d_error('incompatible att length types for ',
     *                          isubname)
           endif
          endif

C         SET attyp and attyp2 for copy of int, real, or char
 
          if (ctype(1:1).ne.'V') then
c              ...type for source attribute not found
               write(logmess,'(a,a)')
     *         'CMO_COPYATT: attribute type not found for ',
     *         csrcnam(1:icharlnf(csrcnam))
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
          endif
          if (ctype2(1:1).ne.'V') then
c              ...type for sink attribute not found
               write(logmess,'(a,a)')
     *         'CMO_COPYATT: attribute type not found for ',
     *         csnknam(1:icharlnf(csnknam))
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
           endif
 
           if(ctype(1:4).eq.'VINT') then
                attyp=1
           elseif(ctype(1:7).eq.'VDOUBLE') then
                attyp=2
           elseif(ctype(1:7).eq.'VCHAR') then
                attyp=4
           else
                attyp=3
                ctype='UNKNOWN'
           endif
           if(ctype2(1:4).eq.'VINT') then
                attyp2=1
           elseif(ctype2(1:7).eq.'VDOUBLE') then
                attyp2=2
           elseif(ctype2(1:7).eq.'VCHAR') then
                attyp2=4
           else
                attyp2=3
                ctype2='UNKNOWN'
           endif
 
           if (attyp.ne.attyp2) then
               write(logmess,'(a,a,a,a)')
     *         'Warning: Attribute type ',ctype(1:7),' written to ',
     *          ctype2(1:7)
               call writloga('default',0,logmess,0,ierrw)
           endif
 
c          SET RANK FIELD for attributes
           call cmo_get_info(crank,cmosink,irank_sink,ilen,itin,ier)
           if (ier.ne.0) call x3d_error('get sink rank:',isubname)
           if (ier.ne.0) goto 9999
           call cmo_get_info(crank2,cmosrc,irank_src,ilen,itin,ierr)
           if (ierr.ne.0) call x3d_error('get source rank:',isubname)
           if (ierr.ne.0) goto 9999
           if (irank_src .ne. irank_sink) then
               call x3d_error('incompatible att ranks for ',isubname)
               goto 9999
           endif
 
C          SET pointers to attributes
C          report if error, continue if this is a loop, otherwise return

           len=icharlnf(csnknam)
           call mmgetpr(csnknam(1:len),cmosink,ipxvalsink,ics)
           if (ics.ne.0) then
              call x3d_error('mmgetpr sink value',isubname)
              if (iend.eq.1) then
                 write(logmess,'(a,a,a)')
     *           'CMO_COPYATT ERROR: can not get attribute pointer: ',
     *            cmosink, csnknam
                 call writloga('default',0,logmess,0,ierrw)
                 goto 9999
              endif          
           endif

           len=icharlnf(csrcnam)
           call mmgetpr(csrcnam(1:len),cmosrc,ipxvalsrc,ics)
           if (ics.ne.0) then
              call x3d_error('mmgetpr source value',isubname)
              if (iend.eq.1) then
                 write(logmess,'(a,a,a)')
     *          'CMO_COPYATT ERROR: can not get attribute pointer: ',
     *           cmosrc, csrcnam
                 call writloga('default',0,logmess,0,ierrw)
                 goto 9999
              endif
           endif
           ierror=0
 
C       ................................................................
c         check length and combinations to copy 

          if(lentyp_src.eq.1 .and. lentyp_snk.eq.1) then
c         ...node to node  
 
            nlen_copy=nnode_src
 
c           ...if sink points are less than source
            if (nnode_snk .lt. nnode_src) then
              nlen_copy=nnode_snk
              write(logmess,'(a,i5)')
     *  'Warning: sink nodes lt source nodes by ',nnode_src-nnode_snk
              call writloga('default',0,logmess,0,ierrw)
 
c           ...if sink points are greater than source
            elseif (nnode_snk .gt. nnode_src) then
              write(logmess,'(a,i5)')
     *  'Warning: sink nodes gt source nodes by ',nnode_snk-nnode_src
              call writloga('default',0,logmess,0,ierrw)
 
            endif
 
          elseif(lentyp_src.eq.2 .and. lentyp_snk.eq.2) then
c         ...element to element 
 
            nlen_copy=nelem_src
            if (nelem_src.le.0) then
              write(logmess,'(a,a)')' 0 element attribute: ',csrcnam
              call writloga('default',0,logmess,0,ierrw)
              goto 9998
            endif
 
c           ...if sink elements are less than source
            if (nelem_snk .lt. nelem_src) then
              nlen_copy=nelem_snk
              write(logmess,'(a,i5)')
     * 'Warning: sink elems lt source elems by ',nelem_src-nelem_snk
              call writloga('default',0,logmess,0,ierrw)
 
c           ...if sink elements are greater than source
            elseif (nelem_snk .gt. nelem_src) then
              write(logmess,'(a,i5)')
     * 'Warning: sink elems gt source elems by ',nelem_snk-nelem_src
              call writloga('default',0,logmess,0,ierrw)
 
            endif

c         non-standard combinations give warnings and pick smallest length
          elseif(lentyp_src.eq.1 .and. lentyp_snk.eq.2) then
c         ...node to element 

            nlen_copy=nnode_src

c           ...if sink elements are less than source points
            if (nelem_snk .lt. nnode_src) then
              nlen_copy=nelem_snk
              write(logmess,'(a,i5)')
     *  'Warning: sink elems lt source nodes by ',nnode_src-nelem_snk
              call writloga('default',0,logmess,0,ierrw)

c           ...if sink elements are greater than source points
            elseif (nelem_snk .gt. nnode_src) then
              write(logmess,'(a,i5)')
     *  'Warning: sink nodes gt source nodes by ',nelem_snk-nnode_src
              call writloga('default',0,logmess,0,ierrw)

c           ...if sink elements are equal to source points 
            elseif (nelem_snk .eq. nnode_src) then
              write(logmess,'(a,i5)')
     *  'sink elements and source nodes are equal length: ',nlen_copy
              call writloga('default',0,logmess,0,ierrw)

            endif

c         non-standard combinations give warnings and pick smallest length

          elseif(lentyp_src.eq.2 .and. lentyp_snk.eq.1) then
c         ...element to node (where cmo sink differs from cmo source) 
C         !!!! element into node is done below (element to vertice copy) !!!!

c         first check if this should go into special case element to vertice
c         otherwise copy here

            nlen_copy=nelem_src

            if ( cmosrc(1:icharlnf(cmosrc)) .eq.
     *          cmosink(1:icharlnf(cmosink))  ) then

               elem_to_vertices = 1
               print*,'Special Copy...'

            else

            elem_to_vertices = 0

c           ...if sink nodes are less than source elements
            if (nnode_snk .lt. nelem_src) then
              nlen_copy=nnode_snk
              write(logmess,'(a,i5)')
     *  'Warning: sink nodes lt source elements by ',nelem_src-nnode_snk
              call writloga('default',0,logmess,0,ierrw)

c           ...if sink nodes are greater than source elements
            elseif (nnode_snk .gt. nelem_src) then
              write(logmess,'(a,i5)')
     *  'Warning: sink nodes gt source elements by ',nnode_snk-nelem_src
              call writloga('default',0,logmess,0,ierrw)

c           ...if sink elements are equal to source points
            elseif (nnode_snk .eq. nelem_src) then
              write(logmess,'(a,i5)')
     *  'sink elements and source nodes are equal length: ',nlen_copy
              call writloga('default',0,logmess,0,ierrw)

            endif
            endif
 
          elseif(lentyp_src.eq.3) then
c         ...scalar type 
 
            nlen_copy=1
 
          else
c         ...unknown type index to selected set combination
            nlen_copy=0
          endif
 
c         pset not supported - for now start is 1 and stride is 1
          ipt1 = 1
          ipt2 = nlen_copy
          ipt3 = 1
c         .............................................END selected set
 
          if (nlen_copy.le.0) then
             len=icharlnf(csrcnam)
             write(logmess,'(a,a)')
     * 'CMO ADDATT ERROR: copy length is 0 from source ',csrcnam(1:len)
             call writloga('default',0,logmess,0,ierrw)
             go to 9999
          endif
 
c         COPY LOOP
c         Copy valid src attribute to current sink attribute
 
C         ***************************************************************
C         COPY ATTRIBUTE VALUES
          if (ivalid .and. elem_to_vertices.eq.0 ) then
 
            nset=0
            do k=1,nlen_copy
 
               do l=1,irank_src
                  i1=(k-1)*irank_src+l
                  i2=(k-1)*irank_sink+l
 
                  if(attyp.eq.2 .and. attyp2.eq.2 ) then
                     xvalsink(i2) = xvalsrc(i1)
                     nset=nset+1
                  elseif(attyp.eq.1 .and. attyp2.eq.1 ) then
                     ivalsink(i2) = ivalsrc(i1)
                     nset=nset+1
                  elseif(attyp.eq.1 .and. attyp2.eq.2 ) then
                     xvalsink(i2) = ivalsrc(i1)
                     nset=nset+1
                  elseif(attyp.eq.2 .and. attyp2.eq.1 ) then
                     ivalsink(i2) = xvalsrc(i1)
                     nset=nset+1
                  elseif(attyp.eq.4 .and. attyp2.eq.4 ) then
                     cvalsink(i2) = cvalsrc(i1)
                     nset=nset+1
                  elseif(attyp.eq.3) then
                    call cmo_set_info(attnam,cmosrc,ivalue,1,1,
     *                     ierror)
                  endif
               enddo
            enddo

            write(logmess,'(i10,a,a,a,a,a,a,a,a)')
     *      nset,' copied from ',cmosrc(1:icharlnf(cmosrc)),' ',
     *      csrcnam(1:icharlnf(csrcnam)),
     *      ' to -> ',cmosink(1:icharlnf(cmosink)),' ',
     *      csnknam(1:icharlnf(csnknam))
            call writloga('default',0,logmess,0,ierrw)
 
          endif
        endif

c       ...............................................................
c       END COPY loop for attributes
 
 9998   ivalid=.false.
        if (mmset) then
           call mmrelprt(isubname,ics)
           mmset=.false.
        endif
c
      enddo
c     ******************************************************************
c     END Loop through all possible attributes

C     NOTE, Special case - this is outside of the loop through attributes
C     Loop through elements
C     Allow element attribute to be copied to vertice node attribute
C     if each element has valid parent-child chains
C     parent points are not assigned a value
C
9990  if ( elem_to_vertices .eq. 1 ) then

        nlen_copy=nelem_src

        call cmo_get_info('itettyp',cmosrc,ipitettyp,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info itettyp')
        call cmo_get_info('itet',cmosrc,ipitet,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info itet')
        call cmo_get_info('itetoff',cmosrc,ipitetoff,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info itetoff')

        len=icharlnf(csnknam)
        call mmgetpr(csnknam(1:len),cmosink,ipxvalsink,ics)
        if (ics.ne.0) call x3d_error('mmgetpr sink value',isubname)
        len=icharlnf(csrcnam)
        call mmgetpr(csrcnam(1:len),cmosrc,ipxvalsrc,ics)
        if (ics.ne.0) call x3d_error('mmgetpr source value',isubname)
        ierror=0

        do idx = 1, nlen_copy 
          nen=nelmnen(itettyp(idx))
          do i=1,nen
            ipt=itet(itetoff(idx)+i)
            if(ctype(2:4).eq.'DOU' .and. ctype2(2:4).eq.'DOU' ) then
               xvalsink(ipt) = xvalsrc(idx)
               nset=nset+1
            elseif(ctype(2:4).eq.'INT' .and. ctype2(2:4).eq.'INT' ) then
               ivalsink(ipt) = ivalsrc(idx)
               nset=nset+1
            elseif(ctype(2:4).eq.'INT' .and. ctype2(2:4).eq.'DOU' ) then
               xvalsink(ipt) = ivalsrc(idx)
               nset=nset+1
            elseif(ctype(2:4).eq.'DOU' .and. ctype2(2:4).eq.'INT' ) then
               ivalsink(ipt) = xvalsrc(idx)
               nset=nset+1
           elseif(ctype(2:4).eq.'CHA' .and. ctype2(2:4).eq.'CHA' ) then
               cvalsink(ipt) = cvalsrc(idx)
               nset=nset+1
            else
              call cmo_set_info(attnam,cmosrc,ivalue,1,1,ierror)
            endif
          enddo
        enddo

        write(logmess,'(i10,a,a,a,a,a,a,a,a)')
     *   nset,' copied from ',cmosrc(1:icharlnf(cmosrc)),
     *   ' element ',csrcnam(1:icharlnf(csrcnam)),
     *   ' to -> ',cmosink(1:icharlnf(cmosink)),
     *   ' node vertices ',csnknam(1:icharlnf(csnknam))
        call writloga('default',0,logmess,0,ierrw)

      endif
c     ******************************************************************
c     END Loop through elements for special elem to vertice copy

      logmess = ' '
      call writloga('default',0,logmess,0,ierrw)
      if (mmset) call mmrelprt(isubname,ics)
 
c     error returns transfer to this statement 9999
      goto 9999
 9999 continue
c
      return
      end
 
