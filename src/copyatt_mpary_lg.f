      subroutine copyatt_mpary_lg(cmo_source,cmo_sink
     &                 ,c_attr_len,mpary,mpno,ierror)
 
C#######################################################################
C
C     PURPOSE -
C
C        This subroutine is used to copy all user-defined
C        attributes with the specified c_attr_len as the length
C        field from one mesh object (cmo_source)
C        to another (cmo_sink).
C        If the attributes do not exist, they are created.
C        It is assumed the nodes or elements (or whatever)
C        in the sink mesh already exist, so fields such as
C        itp1,xic,itet,etc are not copied (see reserve name
C        list cresnm defined below), and, eg, points are not created.
C        Compare cmo_copyatt above and copypts.
C
C     INPUT ARGUMENTS -
C
C        cmo_source - source cmo
C        cmo_sink - sink cmo
C        c_attr_len - character field to specify which attributes
C           to copy (and mpary refers to) based on the "length"
C           field of the atribute. Allowed character strings:
C               [ncon50|scalar|nnodes|NNODES|nelements
C                      |(any other defined attribute length)]
C           Note: NNODES is similar to nnodes, except that besides
C               all nnodes length attributes, also all scalar attributes
C               and the icr constraint table are copied.
C           Note: since it is assumed the nodes|elements already
C               exist, mpary ranges outside the sink range are truncated,
C               and no new points or elements (or whatever) are created.
C           Note: mpary is also ignored if c_attr_len='scalar'
C               or 'ncon50', which are handled slightly differently.
C           Note: as mpary can only refer to a single field,
C               there is no '-all-' option allowed
C        mpary - the sink node to source node translation table:
C               source mesh data source(irank,mpary(1:mpno))
C               is copied onto sink mesh data sink(irank,1:mpno)
C               (the presumption is "sink" is smaller, and
C                 "all sink data" needs to be filled in from source)
C        mpno - the number of entries in mpary
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     PROGRAMMING NOTES -
C
C        - compare copyatt wich doesn't accept indexed sets
C          or create the attribute if it doesn't already exist
C        - compare copypts which creates new points in the sink cmo
C        - also has an expanded set of reserved names re lower_d
C        - The assumption is that cmo_sink starts off as a "new"
C          mesh object (has no user defined attributes, and so all
C          copied attributes are created), hence it does not
C          check that the attribute type, rank, and length are
C          correct, which could lead to problems, although the code
C          should work if the sink mesh already has the attributes
C          with the correct type,rank, and length, just overwriting
C          any existing values.
C        - passing mpary used instead of specifing pset name
C          to make easier to call from other routines.
C        - modify so that only copies "permanent" attributes
C          (if desired)?
C        - modify so that '-all-' allowed, but then ignore mpary.
C          Q: how to do loop over all possible lengths??
C        - add option to reverse meaning of mpary?
C          Or have 2 mpary's - 1 for sink and 1 for source?
C
C     CHANGE HISTORY -
C
C original version modified from copypts.f version 1.26
C
C Q: does adding the attribute automatically set length??
C#######################################################################
 
      implicit none
 
      integer ierror, mpno
 
      character*(*) cmo_sink,cmo_source,c_attr_len
 
      ! do I need to pass the pointer and isubname??
      ! (re addatt possibly messing up?)
      ! copypts coding idicates this coding (no pointer) OK...??
      ! character*(*) mparyname(2)
      ! pointer(ipmpary, mpary)
      integer mpary(*)
 
      character*32 ctype, clength
      character*32 cname, crank, cinterp
     &           , cpersistence, cioflag
      character*300  cbuf
      character*40 cattlen
 
      integer nmcmoatt_sink
      pointer (ipcmoatt_sink, cmoatt_sink)
      character*32 cmoatt_sink(*)
 
      integer nmcmoatt_src
      pointer (ipcmoatt_src, cmoatt_src)
      character*32 cmoatt_src(*)
 
      pointer (ipicontab, icontab), (ipicontabn, icontabn)
      integer icontab(*), icontabn(*)
      pointer (ipicmo_source, icmo_source1), (ipicmo_sink, icmo_sink1)
      pointer (ipxcmo_source, xcmo_source1), (ipxcmo_sink, xcmo_sink1)
      integer icmo_source1(*), icmo_sink1(*)
      real*8 xcmo_source1(*), xcmo_sink1(*)
      real*8 xval
      pointer (ipcmo_attparam_rdefault,cmo_attparam_rdefault)
      real*8 cmo_attparam_rdefault(*)
 
      integer icscode, ndata_sink, ndata_src, nconbnd, ncon50
     &      , ityp, local_debug
     &      , lenn, lent, lenl, ival, len1, len2, len3, len4
     &      , irank_src, irank_sink,lcattl
     &      , i, j, k, i1, i2, l, index
     &      , length_source, length_sink, ierr
 
      integer nresnm
      parameter (nresnm=71)
      character*32 cresnm(nresnm)
      data cresnm/
     &  '-def-','scalar','vector','ncon50','mbndry'
     &  ,'nnodes','nelements','nfaces','nedges','nconbnd'
     &  ,'ndimensions_topo','ndimensions_geom','nodes_per_element'
     &  ,'edges_per_element','faces_per_element'
     &  ,'icontab'
     &  ,'isetwd','xtetwd'
     &  ,'imt1','itp1','icr1','isn1','ign1','ialias'
     &  ,'xic','yic','zic'
     &  ,'itetclr','itettyp','itetoff','itet','jtetoff','jtet'
     &  ,'jtet_cycle_max','lower_d_flag','d0_node_topo'
     &  ,'d0_nclrs','d0_clrlen','d0_clroff','d0_clrtab'
     &  ,'d1_nnodes','d1_nelements'
     &  ,'d1_nen_cmo','d1_nee_cmo','d1_nef_cmo'
     &  ,'d1_itetclr','d1_itettyp'
     &  ,'d1_itetoff','d1_itet','d1_jtetoff','d1_jtet'
     &  ,'d1_jtet_cycle_max'
     &  ,'d2_nnodes','d2_nelements'
     &  ,'d2_nen_cmo','d2_nee_cmo','d2_nef_cmo'
     &  ,'d2_itetclr','d2_itettyp'
     &  ,'d2_itetoff','d2_itet','d2_jtetoff','d2_jtet'
     &  ,'d2_jtet_cycle_max'
     &  ,'d3_nnodes','d3_nelements'
     &  ,'d3_nen_cmo','d3_nee_cmo','d3_nef_cmo'
     &  ,'d0_nfilters','d0_filters'
     &  /

      character*32 cmsgout(12)
      integer imsgout(12),tmsgout(12)
      real*8 xmsgout(12)
      integer nwdsout,iwd
 
      integer icharlnf
 
C#######################################################################

      nwdsout=12
      do iwd=1,nwdsout
         cmsgout(iwd)=' '
         imsgout(iwd)=0
         xmsgout(iwd)=0.d0
         tmsgout(iwd)=3
      enddo
      tmsgout(11)=2
      tmsgout(12)=1
      cmsgout(1)='cmo'
      cmsgout(2)='addatt'
 
      ! for use in debugger....
      local_debug=0
      if (local_debug.eq.1) then
         write(*,*) 'add option to copy lower d color table...?'
         write(*,*) 'only copy if permanent??'
         stop 'what to do re type character attributes?'
      endif
 
      lcattl=icharlnf(c_attr_len)
      if (lcattl.gt.40) lcattl=40
      cattlen=c_attr_len(1:lcattl)
      if (local_debug.eq.1) then
         ! truncated to 32 anyway??
         write(*,*) 'if name too long, truncate or abort?'
      endif
      if (cattlen(1:lcattl).eq.'NNODES') cattlen='nnodes'
 
C.... Get the Sink Mesh Object storage block info.
 
      call cmo_get_info('number_of_attributes',cmo_sink
     &                     ,nmcmoatt_sink,len1,ityp,icscode)
      if (icscode.ne.0) goto 9999
 
 
C.... Get the Source Mesh Object storage block info.
 
      call cmo_get_info('number_of_attributes',cmo_source
     &                     ,nmcmoatt_src,len1,ityp,icscode)
      if (icscode.ne.0) goto 9999
 
C.... abort if trying to copy mesh to itself
      if (cmo_sink(1:icharlnf(cmo_sink))
     &       .eq. cmo_source(1:icharlnf(cmo_source))) goto 9999
 
C     ..................................................................
C
C     Add all non-reserved "INT" type attributes in the source to the sink.
C     Also set the sink value to equal the source value.
 
      call mmfindbk('cmo_attparam_rdefault'
     &                ,cmo_source,ipcmo_attparam_rdefault,
     &                len1,ierr)
 
      if (cattlen(1:lcattl).ne.'scalar'
     &       .and. c_attr_len(1:lcattl).ne.'NNODES') goto 150
 
      ! do non-reserved INT types so defined if needed
      do i=1,nmcmoatt_src
         call cmo_get_attribute_name(cmo_source,i,cname,icscode)
         lenn=icharlnf(cname)
         call cmo_get_attparam(cname,cmo_source,index,ctype,crank,
     &        clength,cinterp,cpersistence,cioflag,icscode)
         lent=icharlnf(ctype)
         if (ctype(1:lent).ne.'INT') goto 100
         do k=1,nresnm
            if(cname(1:lenn).eq.cresnm(k)) go to 100
         enddo
 
         crank='scalar'
         clength='scalar'
         cinterp='constant'
 
C        ! default value
         xval=cmo_attparam_rdefault(i)
 
         len1=icharlnf(crank)
         lenl=icharlnf(clength)
         len2=icharlnf(cinterp)
         len3=icharlnf(cpersistence)
         len4=icharlnf(cioflag)
C$$      write(cbuf,*) 'cmo/addatt/'
C$$  &         //cmo_sink(1:icharlnf(cmo_sink))
C$$  &         //'/'//cname(1:lenn)  //'/'//ctype(1:lent)
C$$  &         //'/'//crank(1:len1)  //'/'//clength(1:lenl)
C$$  &         //'/'//cinterp(1:len2)//'/'//cpersistence(1:len3)
C$$  &         //'/'//cioflag(1:len4)//'/',xval
C$$  &         ,'/0 ; finish '
C$$      call dotask(cbuf,ierror)
         cmsgout(3)=cmo_sink(1:icharlnf(cmo_sink))
         cmsgout(4)=cname(1:lenn)
         cmsgout(5)=ctype(1:lent)
         cmsgout(6)=crank(1:len1)
         cmsgout(7)=clength(1:lenl)
         cmsgout(8)=cinterp(1:len2)
         cmsgout(9)=cpersistence(1:len3)
         cmsgout(10)=cioflag(1:len4)
         xmsgout(11)=xval
         call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
         ! get source value
         call cmo_get_info(cname(1:lenn),cmo_source
     &                ,ival,len1,ityp,ierr)
         ! set sink value
         len1=1
         ityp=1
         call cmo_set_info(cname(1:lenn),cmo_sink
     &                ,ival,len1,ityp,ierr)
 
100      continue
      enddo
 
      if (cattlen(1:lcattl).eq.'scalar') goto 9000
 
150   continue
 
C     ..................................................................
 
C     Add all non-reserved c_attr_len length attributes in the source to the sink,
C     and assign their values.
 
      do i=1,nmcmoatt_src
         call cmo_get_attribute_name(cmo_source,i,cname,icscode)
         lenn=icharlnf(cname)
         call cmo_get_attparam(cname,cmo_source,index,ctype,crank,
     &        clength,cinterp,cpersistence,cioflag,icscode)
         lent=icharlnf(ctype)
         if (ctype(1:lent).eq.'INT') goto 200
         lenl=icharlnf(clength)
         if (clength(1:lenl).ne.cattlen(1:lcattl)) goto 200
         do k=1,nresnm
            if(cname(1:lenn).eq.cresnm(k)) go to 200
         enddo
 
         crank='scalar'
         clength='scalar'
         cinterp='constant'
 
C        ! default value
         xval=cmo_attparam_rdefault(i)
 
         len1=icharlnf(crank)
         len2=icharlnf(cinterp)
         len3=icharlnf(cpersistence)
         len4=icharlnf(cioflag)
         ! should we nstead check if the attribute exists,
         ! and if it does, delete it and re-create it?
C$$      write(cbuf,*) 'cmo/addatt/' //cmo_sink(1:icharlnf(cmo_sink))
C$$  &         //'/'//cname(1:lenn)  //'/'//ctype(1:lent)
C$$  &         //'/'//crank(1:len1)  //'/'//clength(1:lenl)
C$$  &         //'/'//cinterp(1:len2)//'/'//cpersistence(1:len3)
C$$  &         //'/'//cioflag(1:len4)//'/',xval
C$$  &         ,'/0 ; finish '
C$$      call dotask(cbuf,ierror)
         cmsgout(3)=cmo_sink(1:icharlnf(cmo_sink))
         cmsgout(4)=cname(1:lenn)
         cmsgout(5)=ctype(1:lent)
         cmsgout(6)=crank(1:len1)
         cmsgout(7)=clength(1:lenl)
         cmsgout(8)=cinterp(1:len2)
         cmsgout(9)=cpersistence(1:len3)
         cmsgout(10)=cioflag(1:len4)
         xmsgout(11)=xval
         call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
 
C  copy the attribute values: source mesh data source(irank,mpary(1:mpno))
C  copied onto sink mesh data sink(irank,1:mpno)
 
         call cmo_get_info(cattlen(1:lcattl),cmo_source,ndata_src
     &                     ,len1,ityp,ierr)
         call cmo_get_info(cattlen(1:lcattl),cmo_sink,ndata_sink
     &                     ,len1,ityp,ierr)
         call cmo_get_length(ipcmoatt_src,cname,len1
     &                          ,irank_src,ierr)
         if (len1.lt.ndata_src) then
            if (local_debug.ne.0) goto 9999
            goto 200 ! vs ndata_src=len1 ??
         endif
         call cmo_get_length(ipcmoatt_sink,cname,len1
     &                          ,irank_sink,ierr)
         if (len1.lt.ndata_sink) then
            if (local_debug.ne.0) goto 9999
            goto 200 ! vs ndata_src=len1 ??
         endif
         ! ndata_sink and mpno should be the same, but press on ...
         if (ndata_sink.gt.mpno) ndata_sink=mpno
         if(ctype(1:lent).eq.'VINT') then
            call mmfindbk(cname,
     &                 cmo_source,
     &                 ipicmo_source,length_source,
     &                 ierr)
            call mmfindbk(cname,
     &                    cmo_sink,
     &                    ipicmo_sink,length_sink,
     &                    ierr)
            do k=1,min(mpno,ndata_sink)
               i1=mpary(k)
               if (i1.le.ndata_src.and.i1.ge.0) then
                  i1=(i1-1)*irank_src
                  i2=(k-1)*irank_sink
                  do l=1,min(irank_sink,irank_src)
                     icmo_sink1(i2+l)=icmo_source1(i1+l)
                  enddo
               endif
            enddo
         elseif(ctype(1:lent).eq.'VDOUBLE') then
            call mmfindbk(cname,
     &                    cmo_source,
     &                    ipxcmo_source,length_source,
     &                    ierr)
            call mmfindbk(cname,
     &                    cmo_sink,
     &                    ipxcmo_sink,length_sink,
     &                    ierr)
            do k=1,ndata_sink
               i1=mpary(k)
               if (i1.le.ndata_src.and.i1.ge.0) then
                  i1=(i1-1)*irank_src
                  i2=(k-1)*irank_sink
                  do l=1,min(irank_sink,irank_src)
                     xcmo_sink1(i2+l)=xcmo_source1(i1+l)
                  enddo
               endif
            enddo
         else
            ! Q: what to do in "UNKNOWN" case???
            continue
         endif
 
200      continue
 
      enddo
 
      if (cattlen(1:lcattl).ne.'ncon50'
     &    .and. c_attr_len(1:lcattl).ne.'NNODES') goto 9000
 
C     ..................................................................
 
C        Copy the constraint table into the new mesh object.
 
      call cmo_get_info('nconbnd',cmo_source,nconbnd,len1,ityp,ierr)
      if (ierr.ne.0 .or. nconbnd.le.0) goto 9000
 
C$$   cbuf = 'cmo/addatt/' // cmo_sink //
C$$  &    '/ncon50/int/scalar/scalar/constant/permanent/x/0/0; finish'
C$$   call dotask (cbuf, ierror)
      cmsgout(3)=cmo_sink(1:icharlnf(cmo_sink))
      cmsgout(4)='ncon50'
      cmsgout(5)='int'
      cmsgout(6)='scalar'
      cmsgout(7)='scalar'
      cmsgout(8)='constant'
      cmsgout(9)='permanent'
      cmsgout(10)='x'
      xmsgout(11)=0.d0
      call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
      call cmo_get_info ('ncon50',cmo_source,ncon50,len1,ityp,ierror)
      call cmo_set_info ('ncon50',cmo_sink,ncon50,i,1,ierror)
 
C$$   cbuf = 'cmo/addatt/' // cmo_sink //
C$$  &    '/nconbnd/int/scalar/scalar/constant/permanent/x/0/0; finish'
C$$   call dotask (cbuf, ierror)
      cmsgout(4)='nconbnd'
      call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
      call cmo_get_info ('nconbnd',cmo_source,  nconbnd
     &                  , len1,ityp,ierror)
      call cmo_set_info ('nconbnd',cmo_sink, nconbnd, 1,1,ierror)
 
C$$   cbuf = 'cmo/addatt/' // cmo_sink //
C$$  &    '/icontab/vint/scalar/ncon50/constant/permanent/x/0/0; finish'
C$$   call dotask (cbuf, ierror)
      cmsgout(4)='icontab'
      cmsgout(5)='VINT'
      cmsgout(7)='ncon50'
      call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
      call cmo_get_info ('icontab',cmo_source,  ipicontab
     &                  ,  len1,ityp,ierror)
      call cmo_get_info ('icontab',cmo_sink, ipicontabn
     &                  , len1,ityp,ierror)
      do j = 1, ncon50
        icontabn(j) = icontab(j)
      end do
 
C     ..................................................................
 
9000  ierror=0
      return
 
9999  continue
      if (local_debug.ne.0) then
         stop 'copyatt_mpary_lg failed'
      endif
      ierror=1
      return
 
      end
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
