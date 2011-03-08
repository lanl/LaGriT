      subroutine cmo_setatt(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        this routine sets or prints mesh quantities
c
c             ..........................................................
c     syntax -
c
c  cmo/setatt  / cmoname /attribute_name/ value
c  cmo/setatt  / cmoname /attribute_name/ index_set / value
c
c  cmo/set_var  / cmoname /attribute_name/ cmo_var
c  cmo/set_var  / cmoname /attribute_name/ index_set / cmo_var
c
c  cmo/printatt/ cmoname /attribute_name
c  cmo/printatt/ cmoname /attribute_name /index_set
c  cmo/printatt/ cmoname /attribute_name /print_opt / index_set
c
c   setatt      - sets the field of chosen attribute to value
c   printatt    - prints the values of chosen attribute
c   attribute_name - name of a valid cmo attribute
c                 or set of attributes:
c                 nnodes = all attributes of nnodes length
c                 nelements = all attributes of nelements length
c                 -all- = all attributes of the cmo
c                 -xyz- = xic, yic, and zic attributes of the cmo
c   value       - for resetting the values of the attribute field
c   index_set   - can be of type ifirst,ilast, istride integers,
c                 or previously defined pset or eset
c   print_opt   - 'value' or 'list' or 'minmax'
c                 value = default type prints field values
c                 list = print attribute name along with its length
c                 minmax = print name, min and max of the attribute
c                          field, and its length
c   examples:
c
c   print field values:
c     cmo printatt cmo1 zic 3,8,0
c     cmo printatt cmo1 itetclr eset,get,e1
c   print attribute names:
c     cmo printatt cmo1 -all- list
c     cmo printatt cmo1 nnode list
c   print min and max of attribute fields:
c     cmo printatt cmo1 -all- minmax
c     cmo printatt cmo1 -xyz- minmax
c     cmo printatt cmo1 xic minmax 7,10,0
c   set attribute fields to value:
c     cmo setatt   cmo1 zic 3,8,0  99
c     cmo setatt   cmo1 itetclr eset,get,e1 9999
c     cmo setatt   cmo1 zic 5.0
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
C        $Log: cmo_setatt.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.16   03 Jan 2007 15:14:22   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.15   30 Sep 2004 09:00:58   dcg
CPVCS    change call to real to call to dble
CPVCS    correct syntax of if statements using logical variable iset
CPVCS    
CPVCS       Rev 1.14   21 May 2003 11:27:54   dcg
CPVCS    use a longer format when printing a real scalar attribute
CPVCS    
CPVCS       Rev 1.13   12 May 2003 11:05:50   dcg
CPVCS    fix error in test for 'value' option
CPVCS    allow for selection of values to print for attributes of arbitary length
CPVCS    
CPVCS       Rev 1.12   04 Dec 2001 15:29:28   gable
CPVCS    Made changes to format statements when a single attribute is printed out.
CPVCS    
CPVCS       Rev 1.11   27 Jul 2001 11:04:54   tam
CPVCS    print or set cmo mesh_type
CPVCS    
CPVCS       Rev 1.10   07 Dec 2000 09:18:10   dcg
CPVCS    use rank for vector attributes
CPVCS    skip character and vchar attributes
CPVCS    keep going if error so as to check and print all attributes
CPVCS    
CPVCS       Rev 1.9   06 Dec 2000 16:25:16   dcg
CPVCS    skip isetwd and xtetwd in min-max print
CPVCS    do rest of attributes if problem with one
CPVCS    
CPVCS       Rev 1.8   Wed Apr 05 13:34:16 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.7   Tue Apr 04 17:02:20 2000   dcg
CPVCS    check for CHARACTER attribute type
CPVCS    
CPVCS       Rev 1.6   Tue Apr 04 13:17:16 2000   dcg
CPVCS    check for INT and CHAR types
CPVCS    
CPVCS       Rev 1.5   Tue Mar 21 14:57:28 2000   dcg
CPVCS    use mmggetbk in place of mmgetblk
CPVCS    
CPVCS       Rev 1.4   17 Feb 2000 18:21:12   jtg
CPVCS    added test that attlen>0 and abort printing current attribute if not.
CPVCS    Also, attname was incorrectly used instead of cname in 2 places
CPVCS    and I fixed that.
CPVCS    
CPVCS       Rev 1.3   Thu Feb 03 08:50:18 2000   dcg
CPVCS    
CPVCS       Rev 1.2   Tue Feb 01 13:49:08 2000   dcg
CPVCS
CPVCS       Rev 1.12   Fri Nov 12 14:20:32 1999   dcg
CPVCS    don't abort if no nodes in mesh
CPVCS
CPVCS       Rev 1.11   Fri Oct 09 12:33:30 1998   dcg
CPVCS    use nodes, elements as keywords
CPVCS
CPVCS       Rev 1.10   Tue Jul 28 11:14:14 1998   tam
CPVCS    to avoid problems with sbnloc not recognizing att name
CPVCS    make imt=imt1, zic1=zic etc.
CPVCS
CPVCS       Rev 1.9   Tue Jul 21 08:49:56 1998   dcg
CPVCS    allow for character values for itp or itp1 attribute
CPVCS    such as dud to work
CPVCS
CPVCS       Rev 1.8   Fri Apr 10 13:57:50 1998   tam
CPVCS    added difference to minmax output
CPVCS
CPVCS       Rev 1.7   Tue Mar 03 10:00:10 1998   tam
CPVCS    enabled option -xyz- to be recognized
CPVCS
CPVCS       Rev 1.6   Fri Feb 27 12:49:32 1998   dcg
CPVCS    allocate temporary space for printing and storing of
CPVCS    user defined attributes.
CPVCS
CPVCS       Rev 1.5   Tue Jan 27 11:58:32 1998   dcg
CPVCS    allow setting of scalar attributes
CPVCS
CPVCS       Rev 1.4   Tue Jan 27 11:54:18 1998   dcg
CPVCS    print and set user length attributes
CPVCS
CPVCS       Rev 1.3   Thu Nov 13 10:27:12 1997   mcgavran
CPVCS    fixed the logic in the loops that look like
CPVCS         do it=ipt1,ipt2,ipt3
CPVCS    so that they work for element based attributes
CPVCS
CPVCS       Rev 1.2   Mon Nov 03 16:12:24 1997   tam
CPVCS    added loop through all attributes
CPVCS    added list and minmax print options
CPVCS    added more error checking
CPVCS
CPVCS       Rev 1.1   Wed Sep 10 16:38:20 1997   dcg
CPVCS    add cmo argument
CPVCS    add set,print for scalar variables
CPVCS
CPVCS       Rev 1.0   Mon Sep 08 13:52:58 1997   dcg
CPVCS    Initial revision.
 
c
c#######################################################################
c
      include 'cmo_lg.h'
c
c#######################################################################
c
c
      pointer ( ipmpary1 , mpary1(1) )
      pointer (ipitp1,itp1(1000000))
      pointer (ipisetwd,isetwd(1000000))
      pointer (ipxtetwd,xtetwd(1000000))
      integer itp1,isetwd,xtetwd
      integer mpary1
c
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      pointer(ipxfield,xfield)
      real*8 xfield(*)
      pointer(ipxfield,ifield)
      integer ifield(*)
      pointer(ipxfield,cfield)
      character*32 cfield(*)
C
      integer  ntets,length,icmotype,ierror,cmonmlen
      integer indxtyp, i, inxt, ilast, istart, iend
      integer ics,ierr,ierrw,lenattnam,
     * len,ier,ilength,ilen,itin,attlen,index,nmcmoat,
     * ipointi,ipointj,npoints,itype,j,ierror_return,
     * ipt1,ipt2,ipt3, ipt1_sav,ipt2_sav,ipt3_sav,
     * it,mpno,j1,i1,irank,l,iout,lout,isetvar,
     * attyp,ityp,itotal,nset,ifound
      pointer (ipout,out)
      real*8 out(*)
c
      integer ivalue,imin,imax
      real*8  xvalue,xmin,xmax,rout
      integer printopt
      integer NOWRITE, VALUES, LIST, MINMAX
      logical iset, ivalid, mmset
      integer icharlnf
c
      character*32 attnam,cout,cvalue
      character*32 clength, cname,att_name,cinter,cpers,cio
      character*32 isubname
 
      character*32 ich1,ich2,ich3, cmo, crank, ctype,clen
      character*132 logmess
 
      data NOWRITE, VALUES, LIST, MINMAX /0,1,2,3/
c
c
c#######################################################################
c
c
c     ******************************************************************
c
      isetvar   = 0
      iset      = .false.
      mmset     = .false.
      printopt  =  NOWRITE
      indxtyp   = 1
      ierror    = 1
C
C     ******************************************************************
c     set the memory management path name to be the subroutine name.
 
      isubname  = 'cmo_setatt'
 
 
C
c     ******************************************************************
c
C  Parse the required commands
 
      ilast=nwds
 
C     2 - see if setting or printing
      if(cmsgin(2)(1:6).eq.'setatt') then
         iset=.true.
         printopt=NOWRITE
      elseif(cmsgin(2)(1:7).eq.'set_var') then
         isetvar=1 
         iset=.true.
         printopt=NOWRITE
      elseif(cmsgin(2)(1:8).eq.'printatt') then
         iset=.false.
         printopt=VALUES
      else
         write(logmess,'(a,a)') ' CMO illegal option ',cmsgin(2)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
 
c     3 - get mesh object
      if (msgtype(3).eq.3) then
        cmonmlen=icharlnf(cmsgin(3))
        cmo=cmsgin(3)(1:cmonmlen)
        if (cmo(1:cmonmlen).eq.'-def-') then
           call cmo_get_name(cmo,ierror)
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
       call cmo_exist(cmo,ierr)
       if(ierr .ne. 0)then      
         write(logmess,'(a)')
     *   "ERROR: MO DOES NOT EXIST: "//cmo
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')
         ierr=1
         go to 9999
       endif            

 
c     set ntets and npoints
c     check that there are points -- if none return
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      if(npoints.eq.0) then
        write(logmess,'(a,a)')'npoints = 0 in subroutine cmo_setatt'
        call writloga('default',0,logmess,0,ierrw)
c        go to 9999
      endif
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      if(ntets.eq.0) then
        write(logmess,'(a)') 'Warning: nelements = 0 '
        call writloga('default',0,logmess,0,ierrw)
      endif
 
c     4 - get attribute name or set of attributes
      if (msgtype(4).eq.3) then
         attnam=cmsgin(4)
         lenattnam=icharlnf(cmsgin(4))
      else
         ierr=1
         goto 9991
      endif
 
      inxt=5
 
c     if setatt or if set_var - this will be last value
c     this is value to assign to attribute field
      if (iset .and. isetvar.eq.0) then
        if (msgtype(nwds).eq.1) then
          ivalue=imsgin(nwds)
          xvalue=dble(ivalue)
          ilast=ilast-1
        elseif (msgtype(nwds).eq.2) then
          xvalue=xmsgin(nwds)
          ivalue=int(xvalue)
          ilast=ilast-1
        elseif (msgtype(nwds).eq.3) then
          cvalue=cmsgin(nwds)
          ilast=ilast-1
          if((attnam(1:lenattnam).eq.'itp1'.or.
     *        attnam(1:lenattnam).eq.'itp')) then
              call getptyp(cmsgin(nwds),ivalue,ierror)
              if(ierror.ne.0 ) then
                 ierr=1
                 go to 9991
              endif
           endif
        endif

c     if set_var option is used, get the value from
c     the cmo variable name given at this position
c     cmo variable has length=rank=1 INT,REAL, or CHARACTER
c     assign ivalue, xvalue, or cvalue that will be assigned
      else

      endif
 
C     define print options if different from default VALUES
c     options include  VALUES | LIST | MINMAX
 
      if (inxt.le.ilast) then
        if (msgtype(inxt).eq.3 .and.
     *      cmsgin(inxt)(1:6).eq.'minmax') then
          printopt=MINMAX
          inxt=inxt+1
        elseif (msgtype(inxt).eq.3 .and.
     *          cmsgin(inxt)(1:4).eq.'list') then
          printopt=LIST
          inxt=inxt+1
        elseif (msgtype(inxt).eq.3 .and.
     *          cmsgin(inxt)(1:5).eq.'value') then
          printopt=VALUES
          inxt=inxt+1
        elseif (msgtype(inxt).eq.3 .and.
     *          cmsgin(inxt)(1:5).eq.'-def-') then
          printopt=VALUES
          inxt=inxt+1
        endif
      endif
 
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
        endif
      endif
 
C     END Parse commands
      ierr=0
 
c     These are options that are not accessed through cmo attributes
c     return before loop through cmo attributes
c     set or print the mesh_type
      if (attnam(1:lenattnam).eq.'mesh_type') then
        if (iset)  then
          call cmo_set_mesh_type(cmo, cvalue, ierror)
          if (ierror.ne.0) call x3d_error(isubname,' set_mesh_type')
        else
          call cmo_get_mesh_type(cmo, cvalue, ivalue, ierror)
          if (ierror.ne.0) call x3d_error(isubname,' get_mesh_type')
          len = icharlnf(cvalue)
          write(logmess,'(a,a,i5)')
     *    'Attribute:  mesh_type ',cvalue(1:len),ivalue
          call writloga('default',0,logmess,1,ierrw)
        endif
        goto 9999
      endif
 
 
 9991 if (ierr.ne.0) then
        if(iset) then
          write(logmess,'(a)')'cmo/setatt: not a valid syntax'
          call writloga('default',0,logmess,1,ierrw)
c         call dotaskx3d('help/cmo/setatt ; finish ',ierrw)
          goto 9999
        else
          write(logmess,'(a)')'cmo/printatt: not a valid syntax'
          call writloga('default',0,logmess,1,ierrw)
c         call dotaskx3d('help/cmo/printatt ; finish ',ierrw)
          goto 9999
        endif
      endif
 
      istart = 1
      if (attnam(1:lenattnam).eq.'node' .or.
     *    attnam(1:lenattnam).eq.'element' .or.
     *    attnam(1:lenattnam).eq.'-all-'  )  then
        call cmo_get_info('number_of_attributes',cmo,iend,
     *                     ilen,ityp,ierror)
        if (ierror.ne.0) call x3d_error(isubname,'number_of_attributes')
 
      elseif (attnam(1:lenattnam).eq.'-xyz-') then
        iend = 3
      else
        iend = 1
      endif
 
c     ******************************************************************
C     loop through all cmo attributes
C     take action on those defined as valid by user
 
      itotal=0
      ivalid=.false.
      ipt1_sav=ipt1
      ipt2_sav=ipt2
      ipt3_sav=ipt3
 
      do i = istart, iend
        ier=0
        nset=0
 
C       ................................................................
C       decide if there is output for this attribute
        if(attnam(1:lenattnam).eq.'-all-') then
          call cmo_get_attribute_name(cmo,i,cname,ier)
          ivalid=.true.
        elseif(attnam(1:lenattnam).eq.'node') then
          call cmo_get_attribute_name(cmo,i,cname,ier)
          call cmo_get_attparam(att_name,cmo,index,ctype,
     *      crank,clength,cinter,cpers,cio,ier)
          if (clength(1:4).eq.'nnod') ivalid=.true.
        elseif(attnam(1:lenattnam).eq.'element') then
          call cmo_get_attribute_name(cmo,i,cname,ier)
          call cmo_get_attparam(att_name,cmo,index,ctype,
     *      crank,clength,cinter,cpers,cio,ier)
          if (clength(1:4).eq.'nele') ivalid=.true.
        elseif(attnam(1:lenattnam).eq.'-xyz-' .and. i.eq.1) then
          cname='xic'
          ivalid=.true.
        elseif(attnam(1:lenattnam).eq.'-xyz-' .and. i.eq.2) then
          cname='yic'
          ivalid=.true.
        elseif(attnam(1:lenattnam).eq.'-xyz-' .and. i.eq.3) then
          cname='zic'
          ivalid=.true.
        else
          cname=attnam
          ivalid=.true.
 
c         to avoid problems with sbnloc not recognizing att name
          if(cname(1:icharlnf(cname)).eq.'imt')cname='imt1'
          if(cname(1:icharlnf(cname)).eq.'itp')cname='itp1'
          if(cname(1:icharlnf(cname)).eq.'icr')cname='icr1'
          if(cname(1:icharlnf(cname)).eq.'isn')cname='isn1'
          if(cname(1:icharlnf(cname)).eq.'ign')cname='ign1'
          if(cname(1:icharlnf(cname)).eq.'xic1')cname='xic'
          if(cname(1:icharlnf(cname)).eq.'yic1')cname='yic'
          if(cname(1:icharlnf(cname)).eq.'zic1')cname='zic'
 
        endif
        if (ier.ne.0) goto 9999
 
C       ................................................................
C       if a valid attribute
C       look up and setup current attribute
 
        if (ivalid .or. iset) then
 
          len=icharlnf(cname)
          call cmo_get_info('number_of_attributes',cmo,nmcmoat,ilen,
     *      itype,ics)
          do j=1,nmcmoat
             call cmo_get_attribute_name(cmo,j,att_name,ics)
             if(att_name.eq.cname) then
 
C          found existing attribute cname -
c          get type=attyp, length=ilength and rank=irank
 
                call cmo_get_attparam(att_name,cmo,index,ctype,
     *           crank, clen,cinter,cpers,cio,ierror_return)
                ilength=4
                if(clen(1:6).eq.'nnodes') ilength=1
                if(clen(1:9).eq.'nelements') ilength=2
                if(clen(1:6).eq.'scalar') ilength=3
                call cmo_get_info(clen,cmo,attlen,ityp,itin,ierror)
                if (attlen.le.0.or.ierror.ne.0) then
                   write(logmess,'(a,a)')' 0 length attribute: ',cname
                   call writloga('default',0,logmess,0,ierrw)
                   goto 9998
                endif
                if(ctype(1:4).eq.'VINT') then
                   attyp=1
                elseif(ctype(1:7).eq.'VDOUBLE') then
                   attyp=2
                elseif(ctype(1:7).eq.'VCHAR') then
                   attyp=4
                else
                   attyp=3
                endif
c            get field for current attribute
                call mmgetpr(cname(1:len),cmo,ipxfield,ics)
                if (ics.ne.0 .and. attyp.ne.3) then
                   call x3d_error(' get field in ',isubname)
                   goto 9999
                endif
                call cmo_get_info(crank,cmo,irank,ilen,itin,ierr)
                if (ierr.ne.0) call x3d_error(' get rank in ',isubname)
                if (ierr.ne.0) goto 9999
                go to 55
              endif
           enddo
           write(logmess,9005) cname(1:len),cmo
 9005      format(' cannot attribute ',a,' in ',a)
           call writloga('default',0,logmess,0,ierrw)
           go to 9999
 55        ierror=0
 
C       ................................................................
C         set index boundaries- node or element or other
c         then fill mpary1 with index numbers
c         already defined-
c            cname   = attribute name
c            ilength = length type (1=node,2=elem,3=other)
c            attlen  = integer length of attribute field
c            attyp   = integer type (1=VINT,2=VDOUBLE,3=INT,4=VCHAR)
c            npoints = number of points in mesh
c            ntets   = number of elements in mesh
c            xfield or ifield = the attribute values
c            ipt1 ipt2 ipt3 = integer defined point or elem set
c            ich1 ich2 ich3 = character defined point or elem set
 
c.........node type index to selected set
          if(ilength.eq.1) then
 
            ipointi=0
            ipointj=0
            call cmo_get_info('ipointi',cmo,ipointi,ilen,itype,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,ipointj,ilen,itype,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,ics)
            if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
C
            if(ipt1.eq.0) ipt1=max(1,ipointi)
            if(ipt2.eq.0) then
              if(ipointj.le.0 .or. ipt1.eq.1) then
                 ipt2=npoints
              else
                 ipt2=ipointj
              endif
            endif
            if(ipt3.eq.0) ipt3=1
            ipt1=max(1,min(ipt1,npoints))
            ipt2=max(1,min(ipt2,npoints))
            ipt3=max(1,min(ipt3,npoints))
            if (attlen.eq.1) then
              ipt1=1
              ipt2=1
              ipt3=1
            endif
 
            length=npoints
            if (npoints.le.0) then
              write(logmess,'(a,a)')' 0 length attribute: ',cname
              call writloga('default',0,logmess,0,ierrw)
              goto 9998
            endif
 
            call mmggetbk('mpary1',isubname,ipmpary1,length,1,ics)
            if (ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
            mmset=.true.
 
            if(indxtyp.eq.1) then
               call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
            else
               call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
            endif
 
c.........element type index to selected set
          elseif(ilength.eq.2) then
 
            length=ntets
            if (ntets.le.0) then
              write(logmess,'(a,a)')' 0 element attribute: ',cname
              call writloga('default',0,logmess,0,ierrw)
              goto 9998
            endif
 
            call mmggetbk('mpary1',isubname,ipmpary1,length,1,ics)
            if (ics .ne. 0) call x3d_error(isubname,'mmgetblk mpary')
            mmset=.true.
 
            if(indxtyp.eq.1) then
              if (ipt2.le.0) ipt2=ntets
              ipt1=max(1,min(ipt1,ntets))
              ipt2=max(1,min(ipt2,ntets))
              ipt3=max(1,min(ipt3,ntets))
              mpno=0
              do it=ipt1,ipt2,ipt3
                 mpno=mpno+1
                 mpary1(mpno)=it
              enddo
 
            else
              call cmo_get_info('xtetwd',cmo,ipxtetwd,length,icmotype,
     *          ics)
              if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
              call eltlimc(ich1,ich2,ich3,ipmpary1,mpno,ntets,xtetwd)
            endif
 
c.........scalar type index to selected set
          elseif(ilength.eq.3) then
 
            length=1
            call mmggetbk('mpary1',isubname,ipmpary1,length,1,ics)
            if (ics .ne. 0) call x3d_error(isubname,'mmgetblk mpary')
            mmset=.true.
            mpary1(1)=1
            mpno=1
 
 
c.........unknown type index to selected set
          else
            if(ipt2.eq.0) ipt2=attlen
            if(ipt3.eq.0) ipt3=1
            call mmggetbk('mpary1',isubname,ipmpary1,attlen,1,ics)
            mmset=.true.
            mpno=0
            do it=ipt1,ipt2,ipt3
               mpno=mpno+1
               mpary1(mpno)=it
            enddo
            length=ipt2-ipt1+1
          endif
c.........END selected set
c
          if (mpno.le.0) then
             write(logmess,'(a,a)')
     *       'No values acquired for indexed set of ',cname(1:len)
             call writloga('default',0,logmess,0,ierrw)
             call mmrelblk('mpary1',isubname,ipmpary1,ics)
             go to 9999
          endif
 
 
c         Do selected work on the attribute
c         New options can be added in this part of the loop
 
C         ***************************************************************
C         SET ATTRIBUTE VALUES
          if (iset .and. ivalid) then
 
            nset=0
            do j1=1,mpno
               i1=mpary1(j1)
 
               if(attyp.eq.2) then
                  do l=1,irank
                     xfield((i1-1)*irank+l)=xvalue
                     nset=nset+1
                  enddo
               elseif(attyp.eq.1) then
                  do l=1,irank
                     ifield((i1-1)*irank+l)=ivalue
                     nset=nset+1
                  enddo
               elseif(attyp.eq.4) then
                  do l=1,irank
                     cfield((i1-1)*irank+l)=cvalue
                     nset=nset+1
                  enddo
               elseif(attyp.eq.3) then
                  if(ctype.eq.'INT') then
                    call cmo_set_info(cname,cmo,ivalue,1,1,
     *                     ierror)
                  elseif(ctype.eq.'REAL') then
                    call cmo_set_attinfo(cname,cmo,ivalue,xvalue,cvalue,
     *                     2,ierror)
                  elseif(ctype.eq.'CHARACTER') then
                    call cmo_set_attinfo(cname,cmo,ivalue,xvalue,cvalue,
     *                     3,ierror)
                  else
                  endif
                  nset=1
               endif
 
            enddo
 
            write(logmess,'(i10,a,a)')
     *      nset,' values reset for attribute ',cname(1:len)
            call writloga('default',0,logmess,0,ierrw)
 
C         ****************************************************************
C         PRINT ATTRIBUTE VALUES
          elseif (printopt.eq.VALUES .and. ivalid) then
 
            write(logmess,'(a,a)')'Attribute: ',cname(1:icharlnf(cname))
            call writloga('default',0,logmess,0,ierrw)
 
            do j1=1,mpno
               i1=mpary1(j1)
 
               if(attyp.eq.2) then
                  do l=1,irank
                    write(logmess,'(i10,1x,1pe12.5)')
     *              i1,xfield((i1-1)*irank+l)
                    call writloga('default',0,logmess,0,ierrw)
                  enddo
               elseif(attyp.eq.4) then
                  do l=1,irank
                    write(logmess,'(i10,1x,a32)')
     *              i1,cfield((i1-1)*irank+l)
                    call writloga('default',0,logmess,0,ierrw)
                  enddo
               elseif(attyp.eq.1) then
                  do l=1,irank
                  write(logmess,'(i10,1x,i10)')i1,ifield((i1-1)*irank+l)
                    call writloga('default',0,logmess,0,ierrw)
                  enddo
               elseif(attyp.eq.3) then
                    call cmo_get_attinfo(cname,cmo,iout,rout,cout,    ! attname vs cname
     *                        ipout,lout,itype,ierror_return)
                    if(itype.eq.1)write(logmess,'(i10,i10)') i1,iout
                    if(itype.eq.2)write(logmess,'(i10,1pe14.5)') i1,rout
                    if(itype.eq.3)write(logmess,'(i10,a32)') i1,cout
                    call writloga('default',0,logmess,0,ierrw)
               endif
            enddo
 
C         ****************************************************************
C         PRINT ATTRIBUTE NAME LIST
          elseif (printopt.eq.LIST .and. ivalid) then
            if (itotal.eq.0) then
              write(logmess,'(a)')
     >        'INDEX   ATTRIBUTE NAME          LENGTH'
              call writloga('default',0,logmess,0,ierr)
            endif
 
            write(logmess,'(i5,a,a18,i10)') i, ') ', cname(1:17),attlen
            if (irank.gt.1) write(logmess,'(i5,a,a18,i10,a,i3)
     *         ') i, ') ', cname(1:17),attlen,' x',irank
            call writloga('default',0,logmess,0,ierrw)
            itotal=itotal+1
 
 
C         ****************************************************************
C         PRINT ATTRIBUTE MINMAX
          elseif (printopt.eq.MINMAX .and. ivalid) then
 
            if (itotal.eq.0) then
              write(logmess,'(a)')
     >       'ATTRIBUTE NAME              MIN               MAX    '//
     >       '     DIFFERENCE    LENGTH'
              call writloga('default',0,logmess,0,ierrw)
            endif
 
c           check if mpary block exists
            call mmgetindex('mpary1',isubname,ifound,ics)
            if (ifound .le. 0) then
              write(logmess,'(a,a)') 'mpary not set in ',isubname
              call writloga('default',0,logmess,0,ierrw)
              ierror=1
              go to 9999
            endif
 
            call cmo_minmax(cmo,cname,xmin,xmax,imin,imax,
     >                    mpno, mpary1, attyp, ierr)
            if (ierr .ne. 0) then
               write(logmess,'(a,a)')
     >         'minmax failed for attribute ',cname
               call writloga('default',0,logmess,1,ierrw)
               goto 9998
            endif
 
            if (attyp.eq.1 ) then
              write(logmess,'(a18,2(1x,i16),1x,i15,i10)')
     >        cname(1:17),imin,imax,imax-imin,attlen
              if (irank.gt.1)
     >          write(logmess,'(a18,2(1x,i16),1x,i15,i10,a1,i1)')
     >          cname(1:17),imin,imax,imax-imin,attlen,'x',irank
              call writloga('default',0,logmess,0,ierrw)
              itotal=itotal+1
 
            elseif (attyp.eq.2 ) then
              write(logmess,'(a18,2(1x,1pe16.9),1x,1pe15.9,i10)')
     >        cname(1:17),xmin,xmax,xmax-xmin,attlen
              call writloga('default',0,logmess,0,ierrw)
              itotal=itotal+1
            endif
 
          endif
c
        endif
c       ...............................................................
c       END PRINT or SET loop for attributes
 
 9998   ivalid=.false.
        ipt1=ipt1_sav
        ipt2=ipt2_sav
        ipt3=ipt3_sav
        if (mmset) then
           call mmrelprt(isubname,ics)
           mmset=.false.
        endif
c
      enddo
c     ******************************************************************
c     END Loop through attributes
 
      logmess = ' '
      call writloga('default',0,logmess,0,ierrw)
      if (mmset) call mmrelprt(isubname,ics)
c
 
c     error returns transfer to this statement 9999
      goto 9999
 9999 continue
c
      return
      end
