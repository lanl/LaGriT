      subroutine cmo_interpolate(cmo_sink,cmo_src,cmolength,
     *                           nlist,nvalue,
     *                           list,
     *                           ilist,xweight,
     *                           ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE INCREMENTS THE CMO (CURRENT_MESH_OBJECT)
C            TO A NEW LENGTH.
C
C      INPUT ARGUMENTS -
C
C         cmo_sink      - SINK MESH_OBJECT NAME.
C         cmo_src       - SOURCE MESH_OBJECT NAME.
C         cmolength     - CMO_LENGTH OPTION NAME
C                            (nnodes, nelements, etc.)
C                              if cmolength = 'user_added' all user added
C                                  attributes are interpolated (those not in
C                                  the reserved names list (cresnm)
C         nlist         - THE NUMBER OF INDICES TO INTERPOLATE.
C         nvalue        - THE NUMBER OF VALUES TO USE IN THE
C                             INTERPOLATION.
C         list          - THE LIST OF INDICES TO INTERPOLATE.
C         ilist         - THE LIST OF INDICES TO USE IN THE
C                             INTERPOLATION.
C         xweight       - THE LIST OF WEIGHTS TO USE IN THE
C                             INTERPOLATION.
C
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
C        $Log: cmo_interpolate.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.13   04 Dec 2001 10:25:42   dcg
CPVCS    check replacement index against correct array length - source length or
CPVCS    sink length
CPVCS    
CPVCS       Rev 1.12   04 Dec 2001 09:05:46   dcg
CPVCS    test against length of array not maxlen
CPVCS    
CPVCS       Rev 1.11   03 Dec 2001 10:32:52   dcg
CPVCS    fix test on length of arrays (is there enough room for new nodes)
CPVCS    
CPVCS       Rev 1.10   30 Nov 2001 09:27:00   tam
CPVCS    changed maxlen to nnodes+nlist instead of nnodes
CPVCS    also added checks with mmgetlen and error messege
CPVCS    
CPVCS       Rev 1.8   26 Mar 2001 17:24:42   dcg
CPVCS    change arguments to user_interpolate call
CPVCS    
CPVCS       Rev 1.7   26 Mar 2001 16:33:40   dcg
CPVCS    add hooks for user interpolation routine
CPVCS    
CPVCS       Rev 1.6   Fri Apr 07 10:10:04 2000   nnc
CPVCS    Implicit none statement was out-of-order.
CPVCS    
CPVCS       Rev 1.5   Wed Apr 05 13:34:14 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.4   22 Mar 2000 08:38:00   dcg
CPVCS    implicit none
CPVCS    use local_eps in place of epsilon
CPVCS    
CPVCS       Rev 1.3   Tue Feb 22 10:24:28 2000   dcg
CPVCS    augment list of default attributes not to interpolate
CPVCS    
CPVCS       Rev 1.2   Tue Feb 01 13:47:34 2000   dcg
CPVCS
CPVCS       Rev 1.24   Fri Aug 27 17:05:30 1999   dcg
CPVCS    check for 'and' interpolation type and attribute
CPVCS    name 'isetwd'  - handle as special case
CPVCS
CPVCS       Rev 1.23   Tue Aug 03 14:34:04 1999   dcg
CPVCS    accept 'constant' interpolation type as 'default'
CPVCS    allow linear, log, asinh for integer type attributes
CPVCS
CPVCS       Rev 1.22   Fri Jan 22 10:03:18 1999   dcg
CPVCS    set floor for log operation to 1.d-100
CPVCS
CPVCS       Rev 1.21   Thu Jan 21 16:28:14 1999   dcg
CPVCS    allow min and max interpolation types for VDOUBLE attributes
CPVCS
CPVCS       Rev 1.20   Tue Nov 03 11:53:52 1998   dcg
CPVCS    fix interpolation for isetwd
CPVCS
CPVCS       Rev 1.19   Mon Apr 14 16:41:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.18   Mon Feb 24 07:59:50 1997   het
CPVCS    Correct a misspelled loop parameter.
CPVCS
CPVCS       Rev 1.17   Tue Jul 16 14:57:22 1996   dcg
CPVCS    add option for interpolating only user defined attributes
CPVCS    if cmolength is 'user_add'
CPVCS
CPVCS       Rev 1.16   Mon Feb 05 16:30:54 1996   dcg
CPVCS    fix incmin and incmax as special cases of min and max
CPVCS
CPVCS       Rev 1.15   Fri Feb 02 10:10:16 1996   dcg
CPVCS    don't use default value in linear,log,asinh interpolation
CPVCS
CPVCS       Rev 1.14   Thu Feb 01 17:08:02 1996   dcg
CPVCS    use source and sink ranks
CPVCS
CPVCS       Rev 1.13   11/14/95 16:01:44   dcg
CPVCS    add special case for
CPVCS    isetwd and 'or'
CPVCS
CPVCS       Rev 1.12   09/20/95 16:00:40   dcg
CPVCS    HP changes and to iand and or to ior
CPVCS
CPVCS       Rev 1.11   09/15/95 15:53:20   dcg
CPVCS    fix looping over nlist
CPVCS
CPVCS       Rev 1.10   09/14/95 16:38:24   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.9   09/14/95 10:44:32   dcg
CPVCS    remove character literals from calling sequences
CPVCS
CPVCS       Rev 1.8   09/13/95 11:27:42   dcg
CPVCS    repair problems with uninitialized variable, nmcmoatt,
CPVCS    ipcmoatt, cmo_name, lentype, clength etc.
CPVCS
CPVCS       Rev 1.7   09/12/95 14:46:34   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.6   09/11/95 14:44:08   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.5   08/15/95 18:22:24   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.4   07/14/95 10:20:56   het
CPVCS    Correct the calling sequence arguments and averaging
CPVCS
CPVCS       Rev 1.3   02/10/95 14:07:54   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.1   01/30/95 12:41:28   het
CPVCS    Correct syntax error for the SGI
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:30   dcg
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
C
      integer nresnm
      parameter (nresnm=71)
C
C
      include 'cmo_lg.h'
      include "cmo.h"
      include "machine.h"
C
C arguments cmo_interpolate(cmo_sink,cmo_src,cmolength,
C             nlist, nvalue, list, ilist,xweight, ierror_return) 
C
      character*(*) cmo_sink, cmo_src, cmolength
      integer nlist, nvalue, ierror_return
      integer list(nlist)
      integer ilist(nvalue,nlist)
      real*8 xweight(nvalue,nlist)

C
C#######################################################################
C variables

      pointer (ipcmo_sink, ccmo_sink1)
      character*32 ccmo_sink1(*)

      pointer (ipxtmp, xtmp)
      pointer (ipcmo_sink, icmo_sink1)
      pointer (ipcmo_sink, xcmo_sink)
      integer icmo_sink1(*)
      REAL*8 xtmp(*), xcmo_sink(*)

      pointer (ipcmo_src, icmo_src1)
      pointer (ipcmo_src, xcmo_src)
      pointer (ipcmo_src, ccmo_src1)
      integer icmo_src1(*)
      REAL*8 xcmo_src(*)

      real*8 xaverage3,xaverage2,xvalue,xsum1,xaverage1,tweight,
     * xval,cinterpolate

      integer i, j, k, i1, icmolen, icmotyp,ival,maxlen2
      integer ivalue, iaverage2,l,i2,ierror,irank,len,len1,lentype,
     * length,index,nameln,icharlnf,nen,nef,ierr,icmo_sink,icmo_src,
     * ilen,itype,icscode,nmcmoatt_sink,irank_src,maxlen,ifdebug,ibad

      logical luser

      character*132 logmess
      character*32 coption
      character*32 ccmo_src1(*)
      character*32 clength, ctype, cinterp, ctabinterp, cname
      character*32 crank,cpers,cio,cval
C
      character*32 cresnm(nresnm)
      data cresnm/'nnodes','nelements','nfaces','nedges','mbndry',
     *  'ndimensions_topo','ndimensions_geom','nodes_per_element',
     *  'edges_per_element','faces_per_element','isetwd','imt1',
     *  'itp1','icr1','isn1','ign1','xic','yic','zic','itetclr',
     *  'itettyp','xtetwd','itetoff','jtetoff','itet','jtet',
     *  'dens','pres','ener','vels','ialias','-def-','scalar','vector',
     * 'ipolydat','vor2d','vor3d','local_eps','epsilonl','epsilona',
     * 'epsilonv','ipointi','ipointj','idebug','itypconv_sm',
     * 'maxiter_sm','tolconv_sm','nnfreq','ivoronoi','iopt2to2',
     * 'dumptype','velname','densname','presname','enername','xmin',
     * 'xmax','ymin','ymax','zmin','zmax','kdtree_level',
     * 'max_number_sets','number_of_psets','number_of_eltsets',
     * 'psetnames','eltsetnames','geom_name','ncon50','nconbnd',
     * 'icontab'/
C
C#######################################################################
C BEGIN begin
C
C
      ierror_return=-1
C
C.... Check to see if the SINK-MO exists.
C
      call cmo_get_index(cmo_sink,icmo_sink,ierror_return)
C
      if(icmo_sink.eq.0) then
C
         ierror_return=1
         write(logmess,'(a,a)')
     *      'SINK mesh_object name does not exist: ',cmo_sink
         call writloga('default',0,logmess,0,ierr)
         goto 9999
C
      endif
C
C
C.... Check to see if the SINK-MO exists.
C
      call cmo_get_index(cmo_src,icmo_src,ierror_return)
C
      if(icmo_src.eq.0) then
C
         ierror_return=1
         write(logmess,'(a,a)')
     *      'SOURCE mesh_object name does not exist: ',cmo_src
         call writloga('default',0,logmess,0,ierr)
         goto 9999
C
      endif
C
         ierror_return=0
C
C
C....    Get the Sink Mesh Object storage block info.
C
C
         call cmo_get_info('nnodes',cmo_sink,nnodes,ilen,itype,icscode)
         call cmo_get_info('nelements',cmo_sink,nelements,
     *                   ilen,itype,icscode)
         call cmo_get_info('nodes_per_element',cmo_sink,
     *                   nen,ilen,itype,icscode)
         call cmo_get_info('faces_per_element',cmo_sink,
     *                   nef,ilen,itype,icscode)
         call cmo_get_info('number_of_attributes',cmo_sink,
     *                   nmcmoatt_sink,ilen,itype,icscode)
         call cmo_get_info('idebug',cmo_sink,ifdebug,ilen,itype,icscode)
         call cmo_get_info('xic',cmo_sink,ipxtmp,ilen,itype,icscode)
         call mmgetlen(ipxtmp,ilen,icscode)
         maxlen = ilen
         ibad = 1
 
         if (ifdebug.gt.0 .or. (nnodes.gt.ilen)) then
           if (nnodes.gt.ilen) then
             write(logmess,'(a)') 'WARNING: Bad array length.'
             call writloga('default',0,logmess,0,ierr)
           endif
           write(logmess,'(a,i14,a,i14,a,i14)')
     *     'nlist = ',nlist,' sink nnodes = ',nnodes,' mmgetlen = ',ilen
           call writloga('default',0,logmess,0,ierr)
         endif
 
C
         do i=1,nmcmoatt_sink
C
C
C....       NAME Field.
C
            call cmo_get_attribute_name(cmo_sink,i,
     *                     cname,icscode)
 
C
C  CHECK IF WE ARE TO PROCESS ONLY USER ADDED ATTRIBUTES
C  IF SO SKIP IF NAME IN RESERVED NAME LIST
C
            nameln=icharlnf(cname)
            luser=.false.
            if(cmolength(1:8).eq.'user_add') then
               luser=.true.
               do j=1,nresnm
                  if(cname(1:nameln).eq.cresnm(j)) go to 100
               enddo
            endif
C
C....       INTERPOLATION, type,length Field.
C
            call cmo_get_attparam(cname,cmo_sink,index,ctype,
     *          crank,clength,ctabinterp,cpers,cio,ierror_return)
 
 
C....       DEFAULT VALUE Field.
C
            call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_sink,ipcmo_attparam_rdefault,
     *                length,icscode)
            xval=cmo_attparam_rdefault(i)
            call mmfindbk('cmo_attparam_cdefault'
     *                ,cmo_sink,ipcmo_attparam_cdefault,
     *                length,icscode)
            cval=cmo_attparam_cdefault(i)
            call mmfindbk('cmo_attparam_idefault'
     *                ,cmo_sink,ipcmo_attparam_idefault,
     *                length,icscode)
            ival=cmo_attparam_idefault(i)
C
            lentype=icharlnf(ctype)
            len1=icharlnf(ctabinterp)
            len=max(icharlnf(cmolength),
     *              icharlnf(clength))
C
            if((cmolength(1:len).eq.clength(1:len).or.luser).and.
     *         ctype(1:lentype).eq.'VCHAR') then
C
               call cmo_get_length(cname,cmo_sink,length,irank,
     *                             ierror_return)
               call cmo_get_length(cname,cmo_src,length,irank_src,
     *                             ierror_return)
               if(irank.ne.irank_src) then
                  write(logmess,1000) irank,irank_src
                  call writloga('default',0,logmess,0,ierr)
               endif
C
               call cmo_get_info(cname,
     *                           cmo_sink,
     *                           ipcmo_sink,icmolen,icmotyp,
     *                           ierror)
               call cmo_get_info(cname,
     *                           cmo_src,
     *                           ipcmo_src,icmolen,icmotyp,
     *                           ierror)
               call mmgetlen(ipcmo_sink,maxlen,ierr)
               call mmgetlen(ipcmo_src,maxlen2,ierr)
               if(ctabinterp(1:len1).eq.'default'.or.
     *            ctabinterp(1:len1).eq.'constant') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        ccmo_sink1(k+irank*(i1-1))=
     *                     cval
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'copy') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        i2=ilist(1,j)
                        ccmo_sink1(k+irank*(i1-1))=
     *                     ccmo_src1(min(k,irank_src)+irank_src*(i2-1))
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'user') then
                  call user_interpolate(cmo_sink,cmo_src,cmolength,
     *                           cname,nlist,
     *                           list,
     *                           ierror_return)
               else
                 go to 5000
               endif
 
C
            elseif((cmolength(1:len).eq.clength(1:len).or.luser).and.
     *         ctype(1:lentype).eq.'VINT') then
C
               call cmo_get_length(cname,cmo_sink,length,irank,
     *                             ierror_return)
               call cmo_get_length(cname,cmo_src,length,irank_src,
     *                             ierror_return)
               if(irank.ne.irank_src) then
                  write(logmess,1000) irank,irank_src
 1000             format(' source rank not same as sink rank ',2i4)
                  call writloga('default',0,logmess,0,ierr)
               endif
C
               call cmo_get_info(cname,
     *                           cmo_sink,
     *                           ipcmo_sink,icmolen,icmotyp,
     *                           ierror)
               call cmo_get_info(cname,
     *                           cmo_src,
     *                           ipcmo_src,icmolen,icmotyp,
     *                           ierror)
C
               call mmgetlen(ipcmo_sink,maxlen,ierr)
               call mmgetlen(ipcmo_src,maxlen2,ierr)
               if(ctabinterp(1:len1).eq.'default'.or.
     *            ctabinterp(1:len1).eq.'constant') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        icmo_sink1(k+irank*(i1-1))=
     *                     ival
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'copy') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        i2=ilist(1,j)
                        icmo_sink1(k+irank*(i1-1))=
     *                     icmo_src1(min(k,irank_src)+irank_src*(i2-1))
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'sequence') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        icmo_sink1(k+irank*(i1-1))=k+irank*(i1-1)
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif((ctabinterp(1:len1).eq.'min') .or.
     *               (ctabinterp(1:len1).eq.'incmin')) then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=9999999
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if(i2.gt.0 .and. i2.le.maxlen2) then
                             ivalue=icmo_src1(irank_src*(i2-1)
     *                                  +min(l,irank_src))
                             iaverage2=min(iaverage2,ivalue)
                           else
                             ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                         if(ctabinterp(1:len1).eq.'incmin')
     *                     icmo_sink1(l+irank*(i1-1))=
     *                     icmo_sink1(l+irank*(i1-1))+1
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif((ctabinterp(1:len1).eq.'max') .or.
     *               (ctabinterp(1:len1).eq.'incmax')) then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=-9999999
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if(i2.gt.0 .and. i2.le.maxlen2) then
                             ivalue=icmo_src1(irank_src*(i2-1)
     *                                  +min(l,irank_src))
                              iaverage2=max(iaverage2,ivalue)
                           else
                             ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                         if(ctabinterp(1:len1).eq.'incmax')
     *                     icmo_sink1(l+irank*(i1-1))=
     *                     icmo_sink1(l+irank*(i1-1))+1
                         else
                           ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'and') then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=0
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if(i2.gt.0 .and. i2.le.maxlen2) then
                             ivalue=icmo_src1(irank_src*(i2-1)
     *                                  +min(l,irank_src))
                             iaverage2=iand(iaverage2,ivalue)
                           else
                             ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'or') then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=0
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if ((i2.gt.0 ) .and. (i2.le.maxlen2)) then
                            ivalue=icmo_src1(irank_src*(i2-1)
     *                                +min(l,irank_src))
                            iaverage2=ior(iaverage2,ivalue)
                           else
                            ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'linear' .or.
     *                ctabinterp(1:len1).eq.'asinh' .or.
     *                ctabinterp(1:len1).eq.'log') then
C
                  cinterp=ctabinterp
                  do j=1,nlist
                     do l=1,irank
                        tweight=0.0
                        xaverage1=0.0
                        xsum1=0.0
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if ((i2.gt.0 ) .and. (i2.le.maxlen2)) then
                            coption='function'
                            xvalue=icmo_src1(irank_src*(i2-1)+
     *                         min(l,irank_src))
                            xsum1=xsum1 + xweight(k,j)*
     *                         cinterpolate(coption,cinterp,xvalue)
                            tweight=tweight+xweight(k,j)
                           else
                            ibad = i2
                           endif
                        enddo
                        if(abs(tweight).ne.0.0) then
                           xaverage1=xaverage1+xsum1/tweight
                        else
                           xaverage1=0.0
                        endif
                        coption='inverse'
                        xaverage2=
     *                      cinterpolate(coption,cinterp,xaverage1)
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=nint(xaverage2)
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
              elseif(ctabinterp(1:len1).eq.'user') then
                  call user_interpolate(cmo_sink,cmo_src,cmolength,
     *                           cname,nlist,
     *                           list,
     *                           ierror_return)
               else
                 go to 5000
C
               endif
C
            elseif((cmolength(1:len).eq.clength(1:len).or.luser)
     *             .and.
     *             ctype(1:lentype).eq.'VDOUBLE') then
C
               call cmo_get_length(cname,cmo_sink,length,irank,
     *                             ierror_return)
               call cmo_get_length(cname,cmo_src,length,irank_src,
     *                             ierror_return)
               if(irank.ne.irank_src) then
                  write(logmess,1000) irank,irank_src
                  call writloga('default',0,logmess,0,ierr)
               endif
C
               call cmo_get_info(cname,
     *                           cmo_sink,
     *                           ipcmo_sink,icmolen,icmotyp,
     *                           ierror)
               call cmo_get_info(cname,
     *                           cmo_src,
     *                           ipcmo_src,icmolen,icmotyp,
     *                           ierror)
C
               call mmgetlen(ipcmo_sink,maxlen,ierr)
               call mmgetlen(ipcmo_src,maxlen2,ierr)
               if(ctabinterp(1:len1).eq.'default' .or.
     *            ctabinterp(1:len1).eq.'constant') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        xcmo_sink(k+irank*(i1-1))=
     *                     xval
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'copy') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        i2=ilist(1,j)
                        xcmo_sink(k+irank*(i1-1))=
     *                     xcmo_src(min(k,irank_src)+irank_src*(i2-1))
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'sequence') then
                  do j=1,nlist
                     i1=list(j)
                     if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                     do k=1,irank
                        xcmo_sink(k+irank*(i1-1))=k+irank*(i1-1)
                     enddo
                     else
                        ibad = i1
                     endif
                  enddo
               elseif(ctabinterp(1:len1).eq.'min') then
                  do j=1,nlist
                     do l=1,irank
                        i1=ilist(1,j)
                        if (i1.gt.0) then
                         xaverage2=xcmo_src(irank_src*(ilist(1,j)-1)+
     *                                   min(l,irank_src))
                        endif
                        do k=2,nvalue
                         i2=ilist(k,j)
                         if ((i2.gt.0 ) .and. (i2.le.maxlen2)) then
                           xvalue=xcmo_src(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                           xaverage2=min(xaverage2,xvalue)
                         else
                           ibad = i2
                         endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         xcmo_sink(l+irank*(i1-1))=xaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'max') then
                  do j=1,nlist
                     do l=1,irank
                        i1=ilist(1,j)
                        if (i1.gt.0) then
                         xaverage2=xcmo_src(irank_src*(ilist(1,j)-1)+
     *                                   min(l,irank_src))
                        endif
                        do k=2,nvalue
                           i2=ilist(k,j)
                           if(i2.gt.0 .and. i2.le.maxlen2) then
                              xvalue=xcmo_src(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                              xaverage2=max(xaverage2,xvalue)
                           else
                              ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         xcmo_sink(l+irank*(i1-1))=xaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'and'.and.
     *             cname(1:6).ne.'isetwd') then
                  do j=1,nlist
                     do l=1,irank
                        xaverage2=0
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if(i2.gt.0 .and. i2.le.maxlen2) then
                              xvalue=xcmo_src(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                              xaverage2=xvalue
                              call fpand(KNBPW,xaverage2,xvalue,
     *                                         xaverage3)
                              xaverage2=xaverage3
                           else
                              ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         xcmo_sink(l+irank*(i1-1))=xaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'and'.and.
     *             cname(1:6).eq.'isetwd') then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=0
                        do k=1,nvalue
                          i2=ilist(k,j)
                          if ((i2.gt.0 ) .and. (i2.le.maxlen2)) then
                           ivalue=icmo_src1(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                           iaverage2=iand(iaverage2,ivalue)
                          else
                           ibad = i2
                          endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
C
               elseif(ctabinterp(1:len1).eq.'or'.and.
     *             cname(1:6).ne.'isetwd') then
                  do j=1,nlist
                     do l=1,irank
                        xaverage2=0
                        do k=1,nvalue
                           i2=ilist(k,j)
                           if(i2.ne.0 .and. i2.le.maxlen2) then
                              xvalue=xcmo_src(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                              call fpor(KNBPW,xaverage2,xvalue,
     *                                       xaverage3)
                              xaverage2=xaverage3
                           else
                              ibad = i2
                           endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         xcmo_sink(l+irank*(i1-1))=xaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
               elseif(ctabinterp(1:len1).eq.'or'.and.
     *             cname(1:6).eq.'isetwd') then
                  do j=1,nlist
                     do l=1,irank
                        iaverage2=0
                        do k=1,nvalue
                         i2=ilist(k,j)
                         if ((i2.gt.0 ) .and. (i2.le.maxlen2)) then
                           ivalue=icmo_src1(irank_src*(i2-1)+
     *                                   min(l,irank_src))
                           iaverage2=ior(iaverage2,ivalue)
                         else
                           ibad = i2
                         endif
                        enddo
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         icmo_sink1(l+irank*(i1-1))=iaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
C
               elseif(ctabinterp(1:len1).eq.'linear' .or.
     *                ctabinterp(1:len1).eq.'asinh' .or.
     *                ctabinterp(1:len1).eq.'log') then
C
                  cinterp=ctabinterp
                  do j=1,nlist
                     do l=1,irank
                        tweight=0.0
C                       xaverage1=xval
                        xaverage1=0.0
                        xsum1=0.0
                        do k=1,nvalue
                         i2=ilist(k,j)
                         if(i2.ne.0 .and. i2.le.maxlen2) then
                           coption='function'
                           xvalue=xcmo_src(irank_src*(i2-1)+
     *                           min(l,irank_src))
                           xsum1=xsum1+
     *                           xweight(k,j)*
     *                           cinterpolate(coption,cinterp,xvalue)
                           tweight=tweight+xweight(k,j)
                         else
                           ibad = i2
                         endif
                        enddo
                        if(abs(tweight).ne.0.0) then
                           xaverage1=xaverage1+xsum1/tweight
                        else
                           xaverage1=0.0
                        endif
                        coption='inverse'
                        xaverage2=
     *                      cinterpolate(coption,cinterp,xaverage1)
                        i1=list(j)
                        if ((i1.gt.0 ) .and. (i1.le.maxlen)) then
                         xcmo_sink(l+irank*(i1-1))=xaverage2
                        else
                         ibad = i1
                        endif
                     enddo
                  enddo
              elseif(ctabinterp(1:len1).eq.'user') then
                  call user_interpolate(cmo_sink,cmo_src,cmolength,
     *                           cname,nlist,
     *                           list,
     *                           ierror_return)
               else
                 go to 5000
C
               endif
            endif
 100        continue
         enddo
      goto 9999
c
c  error return - undefined interpolation type
c
 5000 write(logmess,5001) cmo_sink,cname,ctabinterp
 5001 format(' mesh object ',a20,' attribute ',a15,
     *   ' undefined interpolation ',a10)
      call writloga('default',0,logmess,0,ierr)
 9999 continue
 
      if (ibad.ne.1) then
         write(logmess,'(a,i14,a,i14)')
     *   'ERROR: Interpolation indices outside cmo length ',maxlen
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a,i14)')
     *   'Last bad indice: ',ibad
         call writloga('default',0,logmess,1,ierr)
      endif
 
      return
      end
*dk,cinterpolate
      function cinterpolate(coption,ctype,xvalue)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE PROVIDES A GENERIC INTERPOLATION FOR REAL VALUES.
C
C     INPUT ARGUMENTS -
C
C        coption - DO THE FUNCTION OR INVERSE-FUNCTION: function,inverse
C        ctype   - TYPE OF FUNCTION TO APPLY TO FIELD: linear,log,asinh.
C        xvalue  - VALUE TO BE OPERATED ON.
C
C     OUTPUT ARGUMENTS -
C
C        cinterpolate - RESULANT FUNCTION VALUE.
C
C     CHANGE HISTORY -
C
C        $Log: cmo_interpolate.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
C
C ######################################################################
C
C
      implicit none
C
      real*8 cinterpolate,xvalue,local_eps,yvalue,xone,yvalue1
      integer ierrw,icharlnf,len2,len1
      character*132 logmess
C
C ######################################################################
C
      character*32 coption, ctype
C
C ######################################################################
C
C
C
      local_eps=1.d-100
      if(coption(1:8).eq.'function') then
C
         if(ctype(1:3).eq.'log') then
            yvalue=log(abs(max(local_eps,abs(xvalue))))
         elseif(ctype(1:5).eq.'asinh') then
            xone=1.0d+00
            yvalue = sign(log(abs(xvalue)+
     *                    sqrt(xvalue**2+xone)),xvalue)
            yvalue1 = xvalue+sqrt(xvalue*xvalue+xone)
         else
            yvalue=xvalue
         endif
C
      elseif(coption(1:7).eq.'inverse') then
C
         if(ctype(1:3).eq.'log') then
            yvalue = exp(xvalue)
            yvalue = sign(yvalue,xvalue)
         elseif(ctype(1:5).eq.'asinh') then
            yvalue = sinh(xvalue)
         else
            yvalue = xvalue
         endif
C
      else
C
         len1=icharlnf(coption)
         len2=icharlnf(ctype)
         write(logmess,9000) coption(1:len1),ctype(1:len2),xvalue
 9000    format("Warning:Incorrect interpolation scheme: ",
     *          a,' ',a,' ',1pe15.7)
         call writloga('default',0,logmess,0,ierrw)
         yvalue = xvalue
C
      endif
C
      cinterpolate=yvalue
C
      goto 9999
 9999 continue
      return
      end
c
c
      subroutine user_interpolate(cmo_sink,cmo_src,cmolength,
     *                           cname,nlist,
     *                           list,
     *                           ierror_return)
C      INPUT ARGUMENTS -
C
C         cmo_sink      - SINK MESH_OBJECT NAME.
C         cmo_src       - SOURCE MESH_OBJECT NAME.
C         cmolength     - CMO_LENGTH OPTION NAME
C                            (nnodes, nelements, etc.)
C         cname         - Name of the attribute to be interpolated
C         nvalue        - THE NUMBER OF VALUES TO USE IN THE
C                             INTERPOLATION.
C         xweight       - THE LIST OF WEIGHTS TO USE IN THE
C                             INTERPOLATION.
      implicit none
      character *(*) cmo_sink,cmo_src,cmolength,cname
      integer nlist,ierror_return
      integer list(nlist)
      return
      end
 
 
