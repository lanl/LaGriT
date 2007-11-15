      subroutine math(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
c
c#######################################################################
c
c     purpose -
c
c        this routine performs simple mathematical operations on
c        mesh objects
c
c             ..........................................................
c math/operation/cmo_sink/attr_sink/RANGE/cmo_src/attr_src/VALUE
c  1      2         3         4     5-7      8       9     [10-11]
c
c where operation is one of the arithmetic operations:
c               plus, add
c               minus, sub, subtract
c               times, multiply, mult
c               divide
c
c     or a function
c               min, max
c     or a function like
c               sin, cos, tan, ln (natural log), log10, floor,
c               ceiling, exp, exp10, power, abs, others?
c and RANGE is one of:
c               /ifirst,ilast,istride/
c               /pset,get,pset_name/
c               /eltset,get,eltset_name/
c and VALUE is one of:
c               /cmo_src2/attr_src2/
c               /constant/ (not allowed for exp and exp10, however)
c               (nothing for unary operations)
c
c for 'power', the usage is extended to be:
c    math/power/cmo_sink/attr_sink/RANGE/cmo_src/attr_src/VALUE (as above)
c                         OR
c    math/power/cmo_sink/attr_sink/RANGE/constant/cmo_src/attr_src/
c  That is, for power, we allow for the attribute to be the exponent -
c      i.e. constant**attribute_value
c  This does not preclude the more usual attribute_value**constant or
c      attrib1**attrib2
c
c
c   for attributes with length = 'nnodes', pset can be supplied
c   for attributes with length = 'nelements', eltset can be supplied
c   for other attributes ifirst,ilast, istride must be numbers
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
C        $Log: math.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 13:34:42 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.0   Mon Jan 31 11:55:56 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Fri Jan 22 15:18:38 1999   dcg
CPVCS    set ipt1,ipt2,ipt3 for scalars
CPVCS
CPVCS       Rev 1.2   Mon Jan 26 15:46:54 1998   mcgavran
CPVCS    Added exp, exp10, and power
CPVCS
CPVCS       Rev 1.1   Wed Dec 10 12:32:38 1997   mcgavran
CPVCS    Fixed 'floor' and 'ceiling' commands to work with a second source attribute
CPVCS
CPVCS       Rev 1.0   Thu Dec 04 13:13:08 1997   mcgavran
CPVCS    Initial revision.
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror
c
      include "consts.h"
c
C#######################################################################
c
c
      pointer (ipmpary1 , mpary1(1000000))
      pointer (ipitp1,itp1(1000000))
      pointer (ipisetwd,isetwd(1000000))
      pointer (ipxtetwd,xtetwd(1000000))
      integer itp1,isetwd,xtetwd
      integer mpary1
      pointer(ipxfield_src,xfield_src)
      real*8 xfield_src(1000000)
      pointer(ipxfield_src,ifield_src)
      integer ifield_src(1000000)
      pointer(ipxfield_src2,xfield_src2)
      real*8 xfield_src2(1000000)
      pointer(ipxfield_src2,ifield_src2)
      integer ifield_src2(1000000)
      pointer(ipxfield_sink,xfield_sink)
      real*8 xfield_sink(1000000)
      pointer(ipxfield_sink,ifield_sink)
      integer ifield_sink(1000000)
c
      integer ntets_sink,npts_sink,ntets_src,npts_src
      integer npts_src2, ntets_src2
      integer length,icmotype,cmonmlen,error
      integer msglen,fnlen
      integer i,nfuns,secondop
C
      integer icscode,ierrw,ivalue,
     * len,ilen_sink,ilen_src,ilen_src2,
     * ipointi,ipointj,ipt1,ipt2,ipt3,
     * it,mpno,j1,i1,irank_sink,irank_src,irank_src2,l,
     * ityp_sink,ityp_src,ityp_src2,ilen,nwdsout,
     * iattvalue,iatttyp,iattvalue2,iatttyp2,isinkarg,isrc1arg,
     * isrc2arg,itpsink,itpsrc1,itpsrc2
 
      integer icharlnf
 
      real*8 value,rattvalue,rattvalue2,xsrc1arg,xsrc2arg,xsinkarg
 
      character*8 op
      character*32 isubname
      character*32 ich1,ich2,ich3,cvalue
      character*32 cmo_sink,attr_sink,attr_src,
     *             cmo_src,attr_src2,cmo_src2
 
      character fnlist(20)*15
      character*132 logmess
      logical isintsrc, isintsrc2,isconstant1,isconstant2
      pointer (ip,ip1)
      integer ip1(*)
c
c
c#######################################################################
 
c     set the memory management path name to be the subroutine name.
c
      isubname='math'
      cmo_sink  = 'notset'
      cmo_src  =  'notset'
      cmo_src2 =  'notset'
      attr_sink = 'notset'
      attr_src =  'notset'
      attr_src2 = 'notset'
      nwdsout = nwds
 
C
c     ******************************************************************
c
c  Initialize the list of supported function
      fnlist(1)='sin'
      fnlist(2)='cos'
      fnlist(3)='tan'
      fnlist(4)='ln'
      fnlist(5)='log10'
      fnlist(6)='floor'
      fnlist(7)='ceiling'
      fnlist(8)='exp'
      fnlist(9)='exp10'
      fnlist(10)='power'
      fnlist(11)='abs'
      nfuns=11
c
c
c  Get the command parameters
c  math/operation/cmo_sink/attr_sink/RANGE/cmo_src/attr_src/VALUE
c  1      2         3         4     5-7      8       9     [10-11]
c
 
c     parse the math operations
 
      msglen=icharlnf(cmsgin(2))
      op = 'undefine'
 
      if(cmsgin(2)(1:9).eq.'integrate') then
         op='integrate'
 
c        cmo_src and cmo_sink must be the same mesh object
c        allow cmo_src token to be left off command line
         if (nwds.ne.9 .and. nwds.ne.8) then
            write(logmess,'(a)')
     *      'Need 8 arguments for integrate:'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *      'math/integrate/cmo_sink/attr_sink/RANGE/attr_src'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
 
         call math_integrate(imsgin,xmsgin,cmsgin,msgtype,nwds,error)
         ierror = error
         goto 9999
 
      elseif(cmsgin(2)(1:3).eq.'dot') then
         op='dotproduct'
         if (nwds.ne.11) then
            write(logmess,'(a)')
     *      'Need 11 arguments for dotproduct:'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     * 'math/dotproduct/cmosink/attsink/RANGE/cmov1/attv1/cmov2/attv2'
            call writloga('default',0,logmess,0,ierrw)
         endif
         write(logmess,'(a)')
     *   'dotproduct not yet implemented.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
 
      elseif(cmsgin(2)(1:5).eq.'cross') then
         op='crossproduct'
         if (nwds.ne.9) then
            write(logmess,'(a)')
     *      'Need 8 arguments for crossproduct:'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     * 'math/crossproduct/cmosink/attsink/RANGE/cmov1/attv1/cmov2/attv2'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
         write(logmess,'(a)')
     *   'crossproduct not yet implemented.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
 
      elseif(cmsgin(2)(1:5).eq.'sum') then
         op='sum'
         if (nwds.ne.9) then
            write(logmess,'(a)')
     *      'Need 8 arguments for sum:'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *      'math/sum/cmosink/attsink/RANGE/cmosrc/attsrc'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
 
         call math_sum(imsgin,xmsgin,cmsgin,msgtype,nwds,error)
         ierror = error
         goto 9999
 
c     parse for arithmetic operators
      elseif((cmsgin(2)(1:4).eq.'plus') .or.
     *       (cmsgin(2)(1:3).eq.'add')) then
         op='+'
      elseif((cmsgin(2)(1:5).eq.'minus').or.
     *       (cmsgin(2)(1:3).eq.'sub')
     *  .or. (cmsgin(2)(1:8).eq.'subtract')) then
         op='-'
      elseif((cmsgin(2)(1:5).eq.'times').or.
     *       (cmsgin(2)(1:4).eq.'mult')) then
	 op='*'
      elseif(cmsgin(2)(1:6).eq.'divide') then
	 op='/'
      elseif(cmsgin(2)(1:5).eq.'power') then
	 op='^'
      elseif(cmsgin(2)(1:3).eq.'min') then
	 op='('
      elseif(cmsgin(2)(1:3).eq.'max') then
	 op=')'
c     parse for mathmatical function
      else
         do i = 1,nfuns
	     fnlen=icharlnf(fnlist(i))
             if (cmsgin(2)(1:msglen).eq.fnlist(i)(1:fnlen)) then
	         op = cmsgin(2)	
	     endif
	 enddo
      endif
      if (op.eq.'undefine') then
	 write(logmess,9001) cmsgin(2)(1:msglen)
 9001    format(a, ': illegal option or unsupported function in MATH ')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
 9000 format(' Not enough arguments for the specified MATH operation')
c
c  get npoints and ntets for sink mesh object
c
      cmonmlen=icharlnf(cmsgin(3))
      cmo_sink=cmsgin(3)(1:cmonmlen)
      call get_mo_info(cmo_sink, npts_sink, ntets_sink, error)
      if (error .ne. 0) go to 9999
c
c  get sink attribute
c
      len=icharlnf(cmsgin(4))
      attr_sink=cmsgin(4)(1:len)
      call get_mo_attr(ipxfield_sink,cmo_sink,attr_sink,
     *                 ilen_sink,ityp_sink,irank_sink,error)
      if (error .ne. 0) go to 9999
 
c
c     ******************************************************************
c
c     set the point index boundaries based on cmo_sink.
c
      ipt1 = 1
      ipt2 = 0
      ipt3 = 0
      ich1=' '
      ich2=' '
      ich3=' '
      if((msgtype(5).eq.1).and.(msgtype(6).eq.1).and.
     *   (msgtype(7).eq.1)) then
        ipt1=imsgin(5)
        ipt2=imsgin(6)
        ipt3=imsgin(7)
      elseif((msgtype(5).eq.3).and.(msgtype(6).eq.3).and.
     *   (msgtype(7).eq.3)) then
        ich1=cmsgin(5)
        ich2=cmsgin(6)
        ich3=cmsgin(7)
      else
        write(logmess,'(a)') 'Bad point limit set input'
        call writloga('default',0,logmess,0,ierrw)
	goto 9999
      endif
 
      if(ilen_sink.eq. 1) then
c     nnodes
         ipointi=0
         ipointj=0
         call cmo_get_info('ipointi',cmo_sink,ipointi,
     *                    length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_ipointi')
         call cmo_get_info('ipointj',cmo_sink,ipointj,
     *                   length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_ipointj')
         call cmo_get_info('itp1',cmo_sink,ipitp1,length,icmotype,
     *                    icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_itp1')
         call cmo_get_info('isetwd',cmo_sink,ipisetwd,length,icmotype,
     *          icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_isetwd')
C
C
	 if (ipt1 .eq. 0) ipt1 = max(1, ipointi)
	 if (ipt2 .eq. 0) then
	     if (ipointj. le. 0 .or. ipt1 .eq. 1) then
		 ipt2 = npts_sink
	     else
		 ipt2 = ipointj
	     endif
	 endif
	 if (ipt3 .eq. 0) ipt3 = 1
	 ipt1 = max(1, min(ipt1, npts_sink))
	 ipt2 = max(1, min(ipt2, npts_sink))
	 ipt3 = max(1, min(ipt3, npts_sink))
c
c     ******************************************************************
c
c     check point limits and translate to valid limits if necessary.
c
         length=npts_sink
         call mmgetblk('mpary1',isubname,ipmpary1,length,1,icscode)
         if(msgtype(5).eq.1) then
            call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npts_sink,isetwd,
     *         itp1)
         elseif(msgtype(5).eq.3) then
            call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npts_sink,isetwd,
     *         itp1)
         endif
      elseif(ilen_sink.eq.2) then
c     nelements
         call cmo_get_info('xtetwd',cmo_sink,ipxtetwd,length,icmotype,
     *          icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_xtetwd')
         length=ntets_sink
         if (ntets_sink.le.0) then
             write(logmess,'(a)') 'No elements in mesh'
             call writloga('default',0,logmess,0,ierrw)
             go to 9999
         endif
         call mmgetblk('mpary1',isubname,ipmpary1,length,1,icscode)
         if(msgtype(5).eq.1) then
	   if (ipt2 .le. 0) ipt2 = ntets_sink
           ipt1=max(1,min(ipt1,ntets_sink))
           ipt2=max(1,min(ipt2,ntets_sink))
           ipt3=max(1,min(ipt3,ntets_sink))
           mpno=0
           do it=ipt1,ipt2,ipt3
              mpno=mpno+1
              mpary1(mpno)=it
           enddo
         else
           call eltlimc(ich1,ich2,ich3,ipmpary1,mpno,ntets_sink,xtetwd)
         endif
c     elseif(ilen_sink.eq.3) then
c        scalar attribute
c        Here we have an attribute of real length 1 (scalar), but it could
c        have type vector (VINT or VDOUBLE). It must have rank 1, or
c        it would overrun its storage, I think.
c        At any rate, we'll set the values later.
      elseif(ilen_sink .ne. 3) then
c     attribute neither nodes, elements, nor scalar (ilen_sink .ne. (1,2,3))
         ipt1=1
         ipt2=1
         ipt3=1
         mpno=0
         do it=ipt1,ipt2,ipt3
            mpno=mpno+1
            mpary1(mpno)=it
         enddo
         length=ipt2-ipt1+1
         call mmgetblk('mpary1',isubname,ipmpary1,length,1,icscode)
      endif
c
c     ******************************************************************
C     Get src cmo info
c
      isconstant1=.false.
      isconstant2=.false.
      secondop=0
c  check if first operand a constant
      if(nwds.ge.8.and.msgtype(8).ne.3) then
         secondop=9
         isconstant1=.true.
         if(msgtype(8).eq.1) then
            iattvalue=imsgin(8)
            rattvalue=dble(imsgin(8))
            isintsrc=.true.
            isconstant1=.true.
         elseif(msgtype(8).eq.2) then
            rattvalue=xmsgin(8)
            iattvalue=nint(xmsgin(8))
            isintsrc=.false.
         endif
         if(nwds.eq.8) go to 200
      else
         secondop=10
c  get first operand attribute
         cmonmlen=icharlnf(cmsgin(8))
         cmo_src=cmsgin(8)(1:cmonmlen)
         call get_mo_info(cmo_src, npts_src, ntets_src, error)
         if (error .ne. 0) go to 9999
c
C     Get src attribute info
c
         len=icharlnf(cmsgin(9))
         attr_src=cmsgin(9)(1:len)
         call get_mo_attr(ipxfield_src,cmo_src,attr_src,
     *                 ilen_src,ityp_src,irank_src,error)
         if (error .ne. 0) go to 9999
c
C     Compare sink & src attributes - should be the same
c     or if sink is not scalar src can be scalar
c
         call check_attrs(ilen_src, ityp_src, irank_src,
     *            ilen_sink, ityp_sink, irank_sink, ierror)
         if (ierror .ne. 0) goto 9999
c
c  if src attribute is scalar get its value and save for later
c
         if(ilen_src.eq.3) then
	    call cmo_get_attinfo(attr_src,cmo_src,
     *        iattvalue,rattvalue,cvalue,ip,ilen,iatttyp,error)
            isintsrc=.false.
            if(ityp_src.eq.3) then
               isintsrc=.true.
               rattvalue=dble(iattvalue)
            else
               iattvalue=nint(rattvalue)
            endif
         endif
      endif
c  check if second operand exists
      if (nwds.lt.secondop) go to 200
c  check for second constant operand
      if (nwds.ge.secondop.and.msgtype(secondop).ne.3)
     *   then
         if(msgtype(secondop).eq.1) then
            iattvalue2=imsgin(secondop)
            rattvalue2=dble(imsgin(secondop))
            isintsrc2=.true.
            isconstant2=.true.
         elseif(msgtype(secondop).eq.2) then
            rattvalue2=xmsgin(secondop)
            iattvalue2=nint(xmsgin(secondop))
            isintsrc2=.false.
            isconstant2=.true.
         endif
         go to 200
c  second operand is an attribute
      elseif (nwds.ge.secondop+1) then
         cmonmlen=icharlnf(cmsgin(secondop))
         cmo_src2=cmsgin(secondop)(1:cmonmlen)
         call get_mo_info(cmo_src2, npts_src2, ntets_src2, error)
         if (error .ne. 0) go to 9999
         len=icharlnf(cmsgin(secondop+1))
         attr_src2=cmsgin(secondop+1)(1:len)
         call get_mo_attr(ipxfield_src2,cmo_src2,attr_src2,
     *                     ilen_src2,ityp_src2,irank_src2,error)
         if (error .ne. 0) go to 9999
c
c  if src2 attribute is scalar get its value and save for later
c
         if(ilen_src2.eq.3) then
	    call cmo_get_attinfo(attr_src2,cmo_src2,
     *      iattvalue2,rattvalue2,cvalue,ip,ilen,iatttyp2,error)
            isintsrc2=.false.
            if(ityp_src2.eq.3) then
               isintsrc2=.true.
               rattvalue2=dble(iattvalue)
            endif
         endif
      elseif (nwds.eq.secondop.and.msgtype(secondop).eq.3) then
         go to 9050
      endif
      if (nwds .gt. secondop+1) then
c          error in input
           write(logmess,'(a)') 'Too many input tokens'
           call writloga('default',0,logmess,0,ierrw)
           go to 9999
      endif
c
C     Perform the remaining operations and functions
c
c     ******************************************************************
c     Sink   type   = INT or VINT
c     Sink   length = scalar
c     Source length = scalar 3
c     Source rank   = scalar 1
c     Source type   = VINT   2
c     INT; it doesn't make sense to implement the functions sin, cos,
c     log, etc. on them. It does make sense to implement 10**i
c     ******************************************************************
 200  continue
      itpsink=0
      itpsrc1=0
      itpsrc2=0
      if(ilen_sink.eq.3) then
        if(ityp_sink.eq.3) then
           itpsink=1
        elseif(ityp_sink.eq.4) then
           itpsink=2
        else
           go to 9050
        endif
        if(ityp_src.eq.1) then
            isrc1arg=ifield_src((i1-1)*irank_src+l)
            itpsrc1=1
        elseif(ityp_src.eq.2) then
            xsrc1arg=xfield_src((i1-1)*irank_src+l)
            itpsrc1=2
        elseif(isconstant1.and.isintsrc) then
            isrc1arg=iattvalue
            itpsrc1=1
        elseif(isconstant1.and..not.isintsrc) then
            xsrc1arg=rattvalue
            itpsrc1=2
        elseif(ityp_src.eq.3) then
            isrc1arg=iattvalue
            itpsrc1=1
        elseif(ityp_src.eq.4) then
            xsrc1arg=rattvalue
            itpsrc1=2
        else
            go to 9050
        endif
        if(nwds.eq.secondop+1) then
            if(ityp_src2.eq.1) then
               isrc2arg=ifield_src2((i1-1)*irank_src2+l)
               itpsrc2=1
            elseif(ityp_src2.eq.2) then
               xsrc2arg=xfield_src2((i1-1)*irank_src2+l)
               itpsrc2=2
            elseif(ityp_src2.eq.3) then
               isrc2arg=iattvalue2
               itpsrc2=1
            elseif(ityp_src2.eq.4) then
               xsrc2arg=rattvalue2
               itpsrc2=2
            else
               go to 9050
           endif
        elseif(isconstant2) then
           if(isintsrc2) then
              isrc2arg=iattvalue2
              itpsrc2=1
            else
              xsrc2arg=rattvalue2
              itpsrc2=2
           endif
         endif
         call set_math_value(ivalue,value,itpsink,
     *   op,isrc1arg,xsrc1arg,itpsrc1,isrc2arg,xsrc2arg,itpsrc2,ierror)
         call cmo_set_attinfo(attr_sink,cmo_sink,
     *            ivalue,value,cvalue,itpsink,icscode)
	 goto 9999
c     end treatment for type INT or real
      endif
		
c     ******************************************************************
c     Sink    type    not= INT or VINT
c     Sink    length     = nnodes or nelements
c
c     non-scalar attributes
c     length is non-scalar and type is a vector
c     ******************************************************************
      if (mpno.le.0) then
         write(logmess,'(a)') 'No attribute values selected'
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
c
c
      do j1=1,mpno
       do l=1,irank_sink
        i1=mpary1(j1)
        if(ityp_sink.eq.1) then
            isinkarg=ifield_sink((i1-1)*irank_sink+l)
            itpsink=1
        elseif(ityp_sink.eq.2) then
            xsinkarg=xfield_sink((i1-1)*irank_sink+l)
            itpsink=2
        else
            go to 9050
        endif
        if(ityp_src.eq.1) then
            isrc1arg=ifield_src((i1-1)*irank_src+l)
            itpsrc1=1
        elseif(ityp_src.eq.2) then
            xsrc1arg=xfield_src((i1-1)*irank_src+l)
            itpsrc1=2
        elseif(isconstant1.and.isintsrc) then
            isrc1arg=iattvalue
            itpsrc1=1
        elseif(isconstant1.and..not.isintsrc) then
            xsrc1arg=rattvalue
            itpsrc1=2
        elseif(ityp_src.eq.3) then
            isrc1arg=iattvalue
            itpsrc1=1
        elseif(ityp_src.eq.4) then
            xsrc1arg=rattvalue
            itpsrc1=2
        else
            go to 9050
        endif
        if(nwds.ge.11.or.(nwds.ge.10.and.isconstant1)) then
            if(ityp_src2.eq.1) then
               isrc2arg=ifield_src2((i1-1)*irank_src2+l)
               itpsrc2=1
            elseif(ityp_src2.eq.2) then
               xsrc2arg=xfield_src2((i1-1)*irank_src2+l)
               itpsrc2=2
            elseif(ityp_src2.eq.3) then
              isrc2arg=iattvalue2
              itpsrc2=1
            elseif(ityp_src2.eq.4) then
              xsrc2arg=rattvalue2
              itpsrc2=2
           else
               go to 9050
           endif
        elseif(isconstant2) then
           if(isintsrc2) then
              isrc2arg=iattvalue2
              itpsrc2=1
            else
              xsrc2arg=rattvalue2
              itpsrc2=2
           endif
         endif
         call set_math_value(isinkarg,xsinkarg,itpsink,
     *   op,isrc1arg,xsrc1arg,itpsrc1,isrc2arg,xsrc2arg,itpsrc2,ierror)
         if(ityp_sink.eq.1) then
            ifield_sink((i1-1)*irank_sink+l)=isinkarg
         elseif(ityp_sink.eq.2) then
            xfield_sink((i1-1)*irank_sink+l)=xsinkarg
         endif
       enddo
      enddo
 
c     We finished OK; set error value
      ierror = 0
c
c     release any local memory allocated for this routine.
c     also error returns transfer to this statement
      go to 9999
 9050 write(logmess,9051)
 9051 format (' error in math command arguments')
      call writloga('default',0,logmess,0,ierrw)
c
 9999 call mmrelprt(isubname,icscode)
c
      return
      end
c     end math routine
c     ******************************************************************
 
C     Supporting routines for math routine
 
c     ******************************************************************
C     SUBROUTINE: get_mo_info
c     PURPOSE: gets the point and element attributes for a mesh obj
c     ******************************************************************
      subroutine get_mo_info(mo, npoints, nels, error)
      implicit none
      character*32 mo
      integer npoints, nels
      integer error
c
      integer len,ierror,ierrw
      integer len_attr, type_attr
      integer icharlnf
      character*132 logmess
c
      error = 0
      len = icharlnf(mo)
      if (mo(1:len).eq.'-def-') then
	  call cmo_get_name(mo,ierror)
	  if (ierror.ne.0) then
	    write(logmess,'(a)') 'CMO found bad mesh object in MATH'
	    call writloga('default',0,logmess,0,ierrw)
	    error = ierror
	    return
	  endif
      elseif (mo(1:len).eq.'-all-') then
	  write(logmess,'(a)') '-all- makes no sense in MATH'
	  call writloga('default',0,logmess,0,ierrw)
	  error = 1
	  return
      endif
c
      call cmo_get_info('nnodes',mo,
     *                  npoints, len_attr, type_attr, ierror)
      if (npoints.eq.0) then
	 write(logmess, '(a,a)') 'npoints = 0 in subroutine MATH'
	 call writloga('default',0,logmess,0,ierrw)
	 error = 1
	 return
      elseif (ierror .ne. 0) then
c        Note: We don't print an error message because in do_power, this
c        failure may be OK. (We call this routine to see if we have a mo.)
	 error = ierror
	 return
      endif
      call cmo_get_info('nelements',mo,
     *                  nels, len_attr, type_attr, ierror)
      if (ierror .ne. 0) then
	 error = ierror
	 return
      endif
c
      return
      end
 
c     ******************************************************************
c     SUBROUTINE: get_mo_attr
c     PURPOSE: gets attribute type, length, and rank information for
c     the mesh object
c     ******************************************************************
      subroutine get_mo_attr(ipxfield,mo,attr_name,ilen,ityp,
     *                 irank,error)
      implicit none
      pointer(ipxfield,xfield)
      real*8 xfield(1000000)
      pointer(ipxfield,ifield)
      integer ifield(1000000)
      character*32 mo,attr_name,cinter,cpers,cio
      integer ilen, ityp, irank, index
      integer error, ierrw, len
c
      integer icharlnf
c
      integer ierror
      integer icscode
      integer lin,itin
c
 
      character*32 crank, ctype,clen
      character*132 logmess
c
c
      error = 0
      len = icharlnf(mo)
      if (mo(1:len).eq.'-def-') then
	  call cmo_get_name(mo,ierror)
	  if (ierror.ne.0) then
	    write(logmess,'(a)') 'CMO found bad mesh object in MATH'
	    call writloga('default',0,logmess,0,ierrw)
	    error = ierror
	    return
	  endif
      elseif (mo(1:len).eq.'-all-') then
	  write(logmess,'(a)') '-all- makes no sense in MATH'
	  call writloga('default',0,logmess,0,ierrw)
	  error = 1
	  return
      endif
C   found existing attribute
C   get type, length and rank
      call cmo_get_attparam(attr_name,mo,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierror)
         if(ierror.eq.0) then
            ilen=4
            if(clen(1:6).eq.'nnodes') ilen=1
            if(clen(1:9).eq.'nelements') ilen=2
            if(clen(1:6).eq.'scalar') ilen=3
         endif
         if(ierror.eq.0) then
            if(ctype(1:4).eq.'VINT') then
                 ityp=1
            elseif(ctype(1:7).eq.'VDOUBLE') then
                 ityp=2
            elseif(ctype(1:4).eq.'REAL') then
                 ityp=4
            elseif(ctype(1:4).eq.'CHAR') then
                 ityp=5
	    else
		 ityp = 3
	    endif
         endif
         call mmgetpr(attr_name,mo,ipxfield,icscode)
         call cmo_get_info(crank,mo,irank,lin,itin,ierror)
         if (ierror.ne.0) then
            write(logmess,9005) attr_name,mo
 9005       format(' cannot find attribute ',a,' in ',a)
            call writloga('default',0,logmess,0,ierrw)
            error = ierror
         endif
      return
      end
 
 
c     ******************************************************************
c     SUBROUTINE: check_attrs
C     PURPOSE: Compare sink & src attributes - should be the same
c     or if sink is not scalar src can be
c     ******************************************************************
      subroutine check_attrs(len_src, typ_src, rank_src,
     *                       len_sink, typ_sink, rank_sink, error)
      implicit none
      integer len_src, rank_src, typ_src,
     *        len_sink, rank_sink, typ_sink, error,ierrw
c
      character*132 logmess
c
      error = 0
c  test first if everything matches
      if (len_src .eq. len_sink.and.rank_src .eq. rank_sink
     *   .and.typ_src .eq. typ_sink) then
c  test next if both vector - then must have same rank
      elseif (len_sink .le. 2 .and. len_src.le.2.and.
     *    rank_sink.eq.rank_src) then
c  if sink vector then src can be scalar
      elseif (len_sink .le. 2 .and.typ_src.ge.3) then
c  if sink vector is scalar then src can be scalar
      elseif (typ_sink .eq. 3 .and.typ_src.ge.3) then
c  else we have an error
      else
          write(logmess,'(a)')
     *        'Source and sink attributes are not compatible'
          call writloga('default',0,logmess,0,ierrw)
          error = 1
          return
      endif
      return
      end
c
c
      subroutine set_math_value(isink,xsink,sink_type,
     *   op,isrc1,xsrc1,src1_type,isrc2,xsrc2,src2_type,ierror)
c  set the sink output from the source input.
c  type is 1 = integer
c  type is 2 = real
c  type is 0 = nonexistent
      implicit none
      include 'consts.h'
      real*8 xsink,xsrc1,xsrc2
      integer isink,isrc1,isrc2,sink_type,src1_type,src2_type,ierror
     *  ,ier
      character*128 logmess
      character *(*) op
c
      if(sink_type.eq.0.or.(src1_type.eq.0.and.
     *   src2_type.eq.0)) go to 9040
      if(sink_type.eq.1) then
         if(src1_type.eq.1.and.src2_type.eq.0) then
            if(op(1:3).eq.'sin') then
               isink=sin(dble(isrc1))
            elseif(op(1:3).eq.'cos') then
               isink=cos(dble(isrc1))
            elseif(op(1:3).eq.'tan') then
               isink=tan(dble(isrc1))
            elseif((op(1:3).eq.'exp').and.(op(4:5).ne.'10'))then
               isink=exp(dble(isrc1))
            elseif(op(1:5).eq.'exp10') then
               isink=10.**isrc1
            elseif(op(1:2).eq.'ln') then
               isink=log(dble(isrc1))
            elseif(op(1:5).eq.'log10') then
               isink=log10(dble(isrc1))
            elseif(op(1:3).eq.'abs') then
               isink=abs(isrc1)
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.0) then
            if(op(1:3).eq.'sin') then
               isink=nint(sin(xsrc1))
            elseif(op(1:3).eq.'cos') then
               isink=nint(cos(xsrc1))
            elseif(op(1:3).eq.'tan') then
               isink=nint(tan(xsrc1))
            elseif((op(1:3).eq.'exp').and.(op(4:5).ne.'10'))then
               isink=nint(exp(xsrc1))
            elseif(op(1:5).eq.'exp10') then
               isink=nint(10.**xsrc1)
            elseif(op(1:2).eq.'ln') then
               isink=nint(log(xsrc1))
            elseif(op(1:5).eq.'log10') then
               isink=nint(log10(xsrc1))
            elseif(op(1:3).eq.'abs') then
               isink=nint(abs(xsrc1))
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.1.and.src2_type.eq.1) then
            if(op(1:1).eq.'+') then
               isink=isrc1+isrc2
            elseif(op(1:1).eq.'-') then
               isink=isrc1-isrc2
            elseif(op(1:1).eq.'*') then
               isink=isrc1*isrc2
            elseif(op(1:1).eq.'^') then
               isink=isrc1**isrc2
            elseif(op(1:1).eq.'(') then
               isink=min(isrc1,isrc2)
            elseif(op(1:1).eq.')') then
               isink=max(isrc1,isrc2)
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               isink=isrc1/isrc2
            elseif(op(1:5).eq.'floor') then
               if(isrc1.lt.isrc2) isink=isrc2
            elseif(op(1:4).eq.'ceil') then
               if(isrc1.gt.isrc2) isink=isrc2
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.1.and.src2_type.eq.2) then
            if(op(1:1).eq.'+') then
               isink=nint(isrc1+xsrc2)
            elseif(op(1:1).eq.'-') then
               isink=nint(isrc1-xsrc2)
            elseif(op(1:1).eq.'*') then
               isink=nint(isrc1*xsrc2)
            elseif(op(1:1).eq.'^') then
               isink=nint(dble(isrc1)**xsrc2)
            elseif(op(1:1).eq.'(') then
               isink=nint(min(dble(isrc1),xsrc2))
            elseif(op(1:1).eq.')') then
               isink=nint(max(dble(isrc1),xsrc2))
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               isink=nint(isrc1/xsrc2)
            elseif(op(1:5).eq.'floor') then
               if(isrc1.lt.xsrc2) isink=nint(xsrc2)
            elseif(op(1:4).eq.'ceil') then
               if(isrc1.gt.isrc2) isink=nint(xsrc2)
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.1) then
            if(op(1:1).eq.'+') then
               isink=nint(xsrc1+isrc2)
            elseif(op(1:1).eq.'-') then
               isink=nint(xsrc1-isrc2)
            elseif(op(1:1).eq.'*') then
               isink=nint(xsrc1*isrc2)
            elseif(op(1:1).eq.'^') then
               isink=nint(xsrc1**isrc2)
            elseif(op(1:1).eq.'(') then
               isink=min(nint(xsrc1),isrc2)
            elseif(op(1:1).eq.')') then
               isink=max(nint(xsrc1),isrc2)
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               isink=nint(xsrc1/isrc2)
            elseif(op(1:5).eq.'floor') then
               if(xsrc1.lt.isrc2) isink=isrc2
            elseif(op(1:4).eq.'ceil') then
               if(xsrc1.gt.isrc2) isink=isrc2
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.2) then
            if(op(1:1).eq.'+') then
               isink=nint(xsrc2+xsrc2)
            elseif(op(1:1).eq.'-') then
               isink=nint(xsrc2-xsrc2)
            elseif(op(1:1).eq.'*') then
               isink=nint(xsrc2*xsrc2)
            elseif(op(1:1).eq.'^') then
               isink=nint(xsrc1**xsrc2)
            elseif(op(1:1).eq.'(') then
               isink=nint(min(xsrc1,xsrc2))
            elseif(op(1:1).eq.')') then
               isink=nint(max(xsrc1,xsrc2))
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               isink=nint(xsrc1/xsrc2)
            elseif(op(1:5).eq.'floor') then
               if(xsrc1.lt.xsrc2) xsink=xsrc2
            elseif(op(1:4).eq.'ceil') then
               if(xsrc1.gt.xsrc2) xsink=xsrc2
            else
               ierror=1
               go to 9040
            endif
         else
            ierror=1
            go to 9040
         endif
      elseif(sink_type.eq.2) then
         if(src1_type.eq.1.and.src2_type.eq.0) then
            if(op(1:3).eq.'sin') then
               xsink=sin(dble(isrc1))
            elseif(op(1:3).eq.'cos') then
               xsink=cos(dble(isrc1))
            elseif(op(1:3).eq.'tan') then
               xsink=tan(dble(isrc1))
            elseif((op(1:3).eq.'exp').and.(op(4:5).ne.'10'))then
               xsink=exp(dble(isrc1))
            elseif(op(1:5).eq.'exp10') then
               xsink=10.**isrc1
            elseif(op(1:2).eq.'ln') then
               xsink=log(dble(isrc1))
            elseif(op(1:5).eq.'log10') then
               xsink=log10(dble(isrc1))
            elseif(op(1:3).eq.'abs') then
               xsink=dble(abs(isrc1))
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.0) then
            if(op(1:3).eq.'sin') then
               xsink=sin(xsrc1)
            elseif(op(1:3).eq.'cos') then
               xsink=cos(xsrc1)
            elseif(op(1:3).eq.'tan') then
               xsink=tan(xsrc1)
            elseif((op(1:3).eq.'exp').and.(op(4:5).ne.'10'))then
               xsink=exp(xsrc1)
            elseif(op(1:5).eq.'exp10') then
               xsink=10.**xsrc1
            elseif(op(1:2).eq.'ln') then
               xsink=log(xsrc1)
            elseif(op(1:5).eq.'log10') then
               xsink=log10(xsrc1)
            elseif(op(1:3).eq.'abs') then
               xsink=abs(xsrc1)
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.1.and.src2_type.eq.1) then
            if(op(1:1).eq.'+') then
               xsink=isrc1+isrc2
            elseif(op(1:1).eq.'-') then
               xsink=isrc1-isrc2
            elseif(op(1:1).eq.'*') then
               xsink=isrc1*isrc2
            elseif(op(1:1).eq.'^') then
               xsink=dble(isrc1)**isrc2
            elseif(op(1:1).eq.'(') then
               xsink=min(isrc1,isrc2)
            elseif(op(1:1).eq.')') then
               xsink=max(isrc1,isrc2)
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               xsink=isrc1*isrc2
            elseif(op(1:5).eq.'floor') then
               if(isrc1.lt.isrc2) xsink=isrc2
            elseif(op(1:4).eq.'ceil') then
               if(isrc1.gt.isrc2) xsink=isrc2
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.1) then
            if(op(1:1).eq.'+') then
               xsink=xsrc1+isrc2
            elseif(op(1:1).eq.'-') then
               xsink=xsrc1-isrc2
            elseif(op(1:1).eq.'*') then
               xsink=xsrc1*isrc2
           elseif(op(1:1).eq.'^') then
               xsink=xsrc1**isrc2
            elseif(op(1:1).eq.'(') then
               xsink=min(xsrc1,dble(isrc2))
            elseif(op(1:1).eq.')') then
               xsink=max(xsrc1,dble(isrc2))
            elseif(op(1:1).eq.'/') then
               if(isrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               xsink=xsrc1/isrc2
            elseif(op(1:5).eq.'floor') then
               if(xsrc1.lt.isrc2) xsink=isrc2
            elseif(op(1:4).eq.'ceil') then
               if(xsrc1.gt.isrc2) xsink=isrc2
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.1.and.src2_type.eq.2) then
            if(op(1:1).eq.'+') then
               xsink=isrc1+xsrc2
            elseif(op(1:1).eq.'-') then
               xsink=isrc1-xsrc2
            elseif(op(1:1).eq.'*') then
               xsink=isrc1*xsrc2
            elseif(op(1:1).eq.'^') then
               xsink=dble(isrc1)**xsrc2
            elseif(op(1:1).eq.'(') then
               xsink=min(dble(isrc1),xsrc2)
            elseif(op(1:1).eq.')') then
               xsink=max(dble(isrc1),xsrc2)
            elseif(op(1:1).eq.'/') then
               if(xsrc2.eq.0) then
                  ierror=1
                  go to 9050
               endif
               xsink=isrc1/xsrc2
            elseif(op(1:5).eq.'floor') then
               if(isrc1.lt.xsrc2) xsink=xsrc2
            elseif(op(1:4).eq.'ceil') then
               if(isrc1.gt.xsrc2) xsink=xsrc2
            else
               ierror=1
               go to 9040
            endif
         elseif(src1_type.eq.2.and.src2_type.eq.2) then
            if(op(1:1).eq.'+') then
               xsink=xsrc1+xsrc2
            elseif(op(1:1).eq.'-') then
               xsink=xsrc1-xsrc2
            elseif(op(1:1).eq.'*') then
               xsink=xsrc1*xsrc2
            elseif(op(1:1).eq.'^') then
               xsink=xsrc1**xsrc2
            elseif(op(1:1).eq.'(') then
               xsink=min(xsrc1,xsrc2)
            elseif(op(1:1).eq.')') then
               xsink=max(xsrc1,xsrc2)
            elseif(op(1:1).eq.'/') then
               if(xsrc2.eq.zero) then
                  ierror=1
                  go to 9050
               endif
               xsink=xsrc1/xsrc2
            elseif(op(1:5).eq.'floor') then
               if(xsrc1.lt.xsrc2) xsink=xsrc2
            elseif(op(1:4).eq.'ceil') then
               if(xsrc1.gt.xsrc2) xsink=xsrc2
            else
               ierror=1
               go to 9040
            endif
         else
            ierror=1
            go to 9040
         endif
      else
         ierror=1
         go to 9040
      endif
      go to 9999
 9040 write(logmess,9041)
 9041 format(' Illegal option or arguments to math')
      call writloga('default',0,logmess,0,ier)
      go to 9999
 9050 write(logmess,9051)
 9051 format(' Divide by zero')
      call writloga('default',0,logmess,0,ier)
      go to 9999
 9999 continue
      return
      end
