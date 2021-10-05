      subroutine zq(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        this routine sets mesh quantities
c
c             ..........................................................
c             common 1 flags.
c
c             isq  - point sequence number (fitword entry)
c             imd  - model part identifer (fitword entry)
c             itp  - point type identifer (fitword entry)
c             ihb  - hydro boundary condition flag (fitword entry)
c             irb  - radiation boundary condition flag (fitword entry)
c             nn1  - nearest neighbor search flag (no neighbors are ever
c                       changed after the initial search).
c             nn2  - indicates which neighbors have been searched in a
c                       given calculational cycle.
c             icr  - constraint index to applies to this point.
c             iah  - active hydro flag (=0 ==> active, =1 ==> inactive).
c             npf  - neighbor point type flag.
c
c        format: zq/   isq/istart/iend/istep/ sequence #
c        format: zq/   imd/istart/iend/istep/ part name
c        format: zq/   itp/istart/iend/istep/ point type
c        format: zq/   ihb/istart/iend/istep/ hydro b.c.
c        format: zq/   irb/istart/iend/istep/ radiation b.c.
c        format: zq/   nn1/istart/iend/istep/ permanent n.n. flag.
c        format: zq/   nn2/istart/iend/istep/ dynamic n.n. flag.
c        format: zq/   icr/istart/iend/istep/ constraint flag.
c        format: zq/   iah/istart/iend/istep/ active hydro flag.
c        format: zq/   npf/istart/iend/istep/ neighbor point flag.
c
c             ..........................................................
c             common 2 flags.
c
c             isn  - sequence number.
c
c             ..........................................................
c             source flags.
c
c             iss  - energy source index
c             its  - temperature source index
c             ips  - pressure source index
c             ivs  - velocity (or acceleration) source index
c
c        format: zq/   iss/istart/iend/istep/ source # / start time
c        format: zq/   its/istart/iend/istep/ source # / start time
c        format: zq/   ips/istart/iend/istep/ source # / start time
c        format: zq/   ivs/istart/iend/istep/ source # / start time
c
c             ..........................................................
c             primary variables.
c
c               x  - x-coordinate for a specific mass point
c               y  - y-coordinate for a specific mass point
c               z  - z-coordinate for a specific mass point
c               u  - x-velocity for a specific mass point
c               v  - y-velocity for a specific mass point
c               w  - z-velocity for a specific mass point
c            pmat  - material pressure
c             rho  - material density
c           einter - internal energy
c             tmat - material temperature
c            trad  - radiation temperature
c            prad  - radiation pressure
c           xmass  - mass associated with a point
c          volume  - volume associated with a point
c         velocity - radial velocity relative to an arbitrary center
c           radius - radial position relative to an arbitrary center
c
c        format: zq/     x/istart/iend/istep/  x-coordinate
c        format: zq/     y/istart/iend/istep/  y-coordinate
c        format: zq/     z/istart/iend/istep/  z-coordinate
c        format: zq/     u/istart/iend/istep/  x-velocity
c        format: zq/     v/istart/iend/istep/  y-velocity
c        format: zq/     w/istart/iend/istep/  z-velocity
c        format: zq/  pmat/istart/iend/istep/  pressure
c        format: zq/   rho/istart/iend/istep/  density
c        format: zq/einter/istart/iend/istep/  internal energy
c        format: zq/  tmat/istart/iend/istep/  material temperature
c        format: zq/  trad/istart/iend/istep/  radiation temperature
c        format: zq/  prad/istart/iend/istep/  radiation pressure
c        format: zq/ xmass/istart/iend/istep/  mass
c        format: zq/volume/istart/iend/istep/  volume
c        format: zq/velocity/istart/iend/istep/velocity/xcen/ycen/zcen
c        format: zq/  radius/istart/iend/istep/  radius/xcen/ycen/zcen
c
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
C        $Log: zq.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Thu Feb 03 08:42:38 2000   dcg
CPVCS    
CPVCS       Rev 1.1   25 Jan 2000 09:36:20   dcg
CPVCS
CPVCS       Rev 1.47   Fri Sep 03 15:49:20 1999   dcg
CPVCS    get rid of unused options
CPVCS
CPVCS       Rev 1.46   Tue Aug 24 13:35:28 1999   llt
CPVCS    testing
CPVCS
CPVCS       Rev 1.45   Fri Mar 12 12:33:10 1999   dcg
CPVCS    if nwds < 6 print out basic info if not special case
CPVCS
CPVCS       Rev 1.44   Tue Jul 21 08:29:08 1998   dcg
CPVCS    treat ipt1 as itp, icr1 as icr
CPVCS
CPVCS       Rev 1.43   Wed Dec 17 14:33:46 1997   dcg
CPVCS    remove unused cmo_get_info calls
CPVCS
CPVCS       Rev 1.42   Thu Dec 04 17:31:08 1997   dcg
CPVCS    fix bad go to if no attribute found
CPVCS
CPVCS       Rev 1.41   Mon Dec 01 16:31:56 1997   dcg
CPVCS    remove a few obsolete variables
CPVCS
CPVCS       Rev 1.40   Mon Nov 24 16:38:28 1997   dcg
CPVCS     use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.38   Mon Nov 17 10:39:54 1997   dcg
CPVCS    remove obsolete code
CPVCS
CPVCS       Rev 1.37   Fri Oct 31 10:51:32 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.36   Mon Apr 14 17:06:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.35   Mon Apr 14 16:15:38 1997   llt
CPVCS    No change.
CPVCS
CPVCS       Rev 1.34   Fri May 24 16:10:10 1996   dcg
CPVCS    fix dimension on vels, get rid of int
CPVCS
CPVCS       Rev 1.33   Sat Mar 09 10:28:38 1996   dcg
CPVCS    replace hollerith in format statements
CPVCS
CPVCS       Rev 1.32   Tue Mar 05 12:50:34 1996   dcg
CPVCS    remove int1, icn1
CPVCS
CPVCS       Rev 1.31   Mon Feb 26 13:44:54 1996   dcg
CPVCS    fixed bug with printing velocities
CPVCS
CPVCS       Rev 1.30   Fri Feb 23 16:38:10 1996   dcg
CPVCS    use dictionary to retrieve velocity names
CPVCS
CPVCS       Rev 1.29   Mon Feb 12 16:15:56 1996   dcg
CPVCS    list option works for added attributes
CPVCS    rank is now looked at for added attributes
CPVCS
CPVCS       Rev 1.28   11/20/95 09:10:08   dcg
CPVCS    test for npoints=0 then return
CPVCS
CPVCS       Rev 1.27   11/17/95 15:21:54   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.26   11/16/95 15:23:06   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.25   11/07/95 17:29:16   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.24   09/29/95 09:13:00   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.23   09/19/95 08:43:48   dcg
CPVCS    allow integer type for added attribute
CPVCS
CPVCS       Rev 1.22   09/18/95 19:43:42   dcg
CPVCS    look for mesh object added attributes
CPVCS
CPVCS       Rev 1.21   08/31/95 11:54:12   dcg
CPVCS    fix default points limits (ipointi,ipointj)
CPVCS
CPVCS       Rev 1.20   08/29/95 12:15:32   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.19   08/23/95 06:59:40   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.18   08/22/95 06:51:36   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.17   07/17/95 16:00:24   dcg
CPVCS    use names for point types
CPVCS
CPVCS       Rev 1.16   07/15/95 02:21:20   het
CPVCS    Correct an error with ipointi and ipointj
CPVCS
CPVCS       Rev 1.15   06/27/95 16:38:48   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.14   06/20/95 15:41:52   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.13   06/19/95 16:43:58   dcg
CPVCS    add blank after literal in calling sequence to savpart
CPVCS
CPVCS       Rev 1.12   06/13/95 09:04:22   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.11   06/07/95 15:32:34   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.10   05/15/95 13:36:56   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.9   05/01/95 08:37:00   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.8   03/31/95 09:11:30   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.7   03/30/95 05:01:02   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.6   03/23/95 22:59:44   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.5   03/23/95 15:08:54   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.4   03/10/95 17:12:50   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.3   02/18/95 06:57:38   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.2   01/04/95 22:06:46   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:34   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:42   pvcs
CPVCS    Original version.
C
c
c#######################################################################
c
c
c#######################################################################
c
      integer nplen
      parameter (nplen=1000000)
c
      character*132 logmess
 
c
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer isetwd(nplen)
      integer imt1(nplen), itp1(nplen),
     *        icr1(nplen), isn1(nplen)
      real*8 xic(nplen), yic(nplen), zic(nplen)
      pointer ( ipmpary1 , mpary1(1) )
      integer mpary1
 
c
      pointer ( ipies , ies1(1) )
      pointer ( iptmes , tmes1(1) )
      pointer ( ipits , its1(1) )
      pointer ( iptmts , tmts1(1) )
      pointer ( ipips , ips1(1) )
      pointer ( iptmps , tmps1(1) )
      pointer ( ipivs , ivs1(1) )
      pointer ( iptmvs , tmvs1(1) )
      pointer (ip,out)
      real*8 out(*)
c
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
      character*32 isubname,cnamz
      character*32 ich1,ich2,ich3, cmo, crank, ctype
 
      pointer(ipxfield,xfield)
      real*8 xfield(1000000)
      pointer(ipxfield,ifield)
      pointer (ipvels,vels)
      real*8 vels(3,1000000)
      integer ifield(1000000)
C
      character*32 iword, iword1, cvelnm
      character*8 cpart
      character*132 iformat
      real*8 tmdvs,tmdss,uradius,radius,emat,rho,pmat,prad,tmat,trad,
     *  wa,va,ua,rout,xa,ya,za,tmdps,tmdts,tmvs1,tmps1,tmts1,
     * tmes1
      integer ivs,ips,its,iss,nptstmp,iout,isn,nn2,nn1,irb,ihb,itp,imt,
     * isq,ier2,ier,lin,itin,ier1,irank,ierror_return,j1,i1,len,
     * icharlnf,icr,if1,if2,i,mpno,ipt1,ipt2,ipt3,itype,icscode,ipointi,
     * ipointj,mmrel2,iunpk,iprt5,iprt4,iprt1,iprt2,iprt3,ihead1,
     * ihead2,ihead3,ihead4,ihead5,ierr,ityp,icmotype,length,npoints,
     * ierrw,ivs1,ips1,l,iatt_index,ilen,ntets,ies1,its1
      character*32 cio,cpers,cinter,clen
 
c
c#######################################################################
c
c
c     ******************************************************************
c
c     set the memory management path name to be the subroutine name.
c
      isubname='zq'
      cnamz='zq2'
      cpart='part'
      isubname='zq'
c
c  get mesh object
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'ZQ found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        go to 9999
      endif
C
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
c  check that there are points -- if none return
      if(npoints.eq.0) then
        write(logmess,'(a)') 'npoints = 0 in subroutine z'
        call writloga('default',0,logmess,0,ierrw)
        go to 9999
      endif
      call cmo_get_info('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
c     initialize some variables.
c
      ihead1=0
      ihead2=0
      ihead3=0
      ihead4=0
      ihead5=0
      iprt1=0
      iprt2=0
      iprt3=0
      iprt4=0
      iprt5=0
      iunpk=0
      mmrel2=0
c
c     ******************************************************************
c
c     set the point index boundaries.
c
      ipointi=0
      ipointj=0
      call cmo_get_info('ipointi',cmo,ipointi,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      if(msgtype(3).eq.1.and.imsgin(3).eq.0) then
         imsgin(3)=max(1,ipointi)
      endif
      if(msgtype(4).eq.1.and.imsgin(4).eq.0) then
         if(ipointj.le.0.or.imsgin(3).eq.1) then
            imsgin(4)=npoints
         else
            imsgin(4)=ipointj
         endif
      endif
      if(msgtype(5).eq.1.and.imsgin(5).eq.0) then
         imsgin(5)=1
      endif
C
      ich1=' '
      ich2=' '
      ich3=' '
      if(msgtype(3).eq.1) then
        ipt1=max(1,min(imsgin(3),npoints))
        ipt2=max(1,min(imsgin(4),npoints))
        ipt3=max(1,min(imsgin(5),npoints))
      else
        ich1=cmsgin(3)
        ich2=cmsgin(4)
        ich3=cmsgin(5)
      endif
c
c     ******************************************************************
c
c     check point limits and translate to valid limits if necessary.
c
      length=npoints
      call mmgetblk('mpary1',isubname,ipmpary1,length,2,icscode)
      if(msgtype(3).eq.1) then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npoints,isetwd,itp1)
      elseif(msgtype(3).ne.1) then
         call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npoints,isetwd,itp1)
      endif
c
c     ******************************************************************
c
c     set requested values for mass points.
c
      do 100 j1=1,mpno
         i1=mpary1(j1)
         iword=cmsgin(2)
         len=icharlnf(iword)
c
c        _______________________________________________________________
c
c        write data for points.
c
         if(iword(1:len).eq.'all') then
            iprt1=1
            iprt2=1
            iprt3=1
            goto 9995
         endif
         if(iword(1:len).eq.'imt') then
            len=len+1
            iword(1:len)='imt1'
         endif
         if(iword(1:len).eq.'isn') then
            len=len+1
            iword(1:len)='isn1'
         endif
c
c
c        set type in common.
c
         if(iword(1:len).eq.'itp'.or.iword(1:len).eq.'itp1') then
            ierror=0
            if(nwds.lt.6) then
               iprt1=1
               goto 9995
            endif
            if(msgtype(6).eq.1) then
              itp1(i1)=imsgin(6)
            elseif (msgtype(6).eq.3) then
               call getptyp(cmsgin(6),itp1(i1),ierror)
               if (ierror.ne.0) call x3d_error(isubname,'getptyp')
            else
               write(logmess,'(a30)') 'illegal argument in zq command'
               call writloga ('default',0,logmess,0,ierr)
            endif
            goto 9998
         endif
c
c
c        set constriants index in common.
c
         if(iword(1:len).eq.'icr'.or.iword(1:len).eq.'icr1') then
            ierror=0
            if(nwds.lt.6) then
               iprt1=1
               goto 9995
            endif
            if(msgtype(6).le.2) then
               icr=imsgin(6)
            elseif(msgtype(6).eq.3) then
               iword=cmsgin(6)
               if1=5
               if2=8
               do 120 i=if1,if2
                  if(iword(i:i).eq.' ') then
                     iformat=' '
                     write(iformat,"('(a',i1,',i',i1,')')") if1-1,i-if1
                     goto 121
                  endif
 120           continue
 121           continue
               read(iword,iformat) iword1,icr
            endif
            icr1(i1)=imsgin(6)
            goto 9998
         endif
c
c        _______________________________________________________________
c
c        set xyz-coordinates.
c
         if(iword(1:len).eq.'xyz') then
            ierror=0
            if(nwds.lt.6) then
               iprt2=1
               goto 9995
            endif
            xic(i1)=xmsgin(6)
            yic(i1)=xmsgin(7)
            zic(i1)=xmsgin(8)
            goto 9998
         endif
c
c        _______________________________________________________________
c
c        set x-coordinate.
c
         if(iword(1:len).eq.'x'.or.iword(1:len).eq.'xic') then
            ierror=0
            if(nwds.lt.6) then
               iprt2=1
               goto 9995
            endif
            xic(i1)=xmsgin(6)
            goto 9998
         endif
c
c        _______________________________________________________________
c
c        set y-coordinate.
c
         if(iword(1:len).eq.'y'.or.iword(1:len).eq.'yic') then
            ierror=0
            if(nwds.lt.6) then
               iprt2=1
               goto 9995
            endif
            yic(i1)=xmsgin(6)
            goto 9998
         endif
c
c        _______________________________________________________________
c
c        set z-coordinate.
c
         if(iword(1:len).eq.'z'.or.iword(1:len).eq.'zic') then
            ierror=0
            if(nwds.lt.6) then
               iprt2=1
               goto 9995
            endif
            zic(i1)=xmsgin(6)
            goto 9998
         endif
c        _______________________________________________________________
c      look for added attribute
c
         if(nwds.le.5) then
           iprt1=1
           iprt2=1
           go to 9995
         endif
         if(j1.eq.1) then
            call  cmo_get_attparam(iword,cmo,iatt_index,ctype,crank,
     *      clen,cinter,cpers,cio,ierror_return)
            if(icscode.eq.0) then
C   found existing attribute
               if(ctype(1:4).eq.'VINT') ityp=1
               if(ctype(1:7).eq.'VDOUBLE') ityp=2
               call mmgetpr(iword(1:len),cmo,ipxfield,icscode)
               call cmo_get_info(crank,cmo,irank,lin,itin,ier1)
            else
               write(logmess,9005) iword(1:len),cmo
               call writloga('default',0,logmess,0,ierrw)
               go to 9998
            endif
         endif
         ierror=0
         if((j1.eq.1.and.icscode.eq.0.and.ier.eq.0.and.ier1.eq.0.and.
     *            ier2.eq.0).or.j1.ne.1) then
            if(msgtype(6).eq.2) then
               do l=1,irank
                  xfield((i1-1)*irank+l)=xmsgin(6)
               enddo
            elseif(msgtype(6).eq.1) then
               do l=1,irank
                  ifield((i1-1)*irank+l)=imsgin(6)
               enddo
            elseif(nwds.lt.6) then
               do l=1,irank
                  if(ityp.eq.1) write(logmess,'(i6,i6)') i1,
     *                 ifield((i1-1)*irank+l)
                  if(ityp.eq.2) write(logmess,'(i6,e10.3)') i1,
     *                 xfield((i1-1)*irank+l)
                  call writloga('default',0,logmess,0,ierrw)
               enddo
            endif
         else
            write(logmess,9005) iword(1:len),cmo
 9005       format(' zq cannot find attribute ',a,' in ',a)
            call writloga('default',0,logmess,0,ierrw)
            go to 9997
         endif
c        _______________________________________________________________
c
c        invalid option used if this location reached.
c
         goto 9998
c
c        _______________________________________________________________
c
c        write point data to log files.
c
 9995    continue
c
         ierror=0
         if(ihead1.eq.0) then
            ihead1=1
            if(iprt1.eq.1) then
               write(logmess,9000)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
         if(ihead2.eq.0) then
            ihead2=1
            if(iprt2.eq.1) then
               write(logmess,9030)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
         if(ihead3.eq.0) then
            ihead3=1
            if(iprt3.eq.1) then
               write(logmess,9050)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
         if(ihead4.eq.0) then
            ihead4=1
            if(iprt4.eq.1) then
               write(logmess,9070)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
         isq=0
         imt=imt1(i1)
         itp=itp1(i1)
         ihb=0
         irb=0
         nn1=0
         nn2=0
Cdcg     int=int1(i1)
         icr=icr1(i1)
Cdcg     icn=icn1(i1)
         isn=isn1(i1)
c
         if(iprt1.eq.1) then
            write(logmess,9010) i1,isq,imt,itp,icr,isn
            call writloga('default',0,logmess,0,ierr)
         endif
         if(iprt2.eq.1) then
            xa=xic(i1)
            ya=yic(i1)
            za=zic(i1)
            if(j1.eq.1)
     *        call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *         ip,length,itype,ier)
            if(ier.eq.0.and.j1.eq.1)
     *        call cmo_get_info(cvelnm,cmo,ipvels,length,itype,ier)
            if(ier.eq.0) then
               ua=vels(1,i1)
               va=vels(2,i1)
               wa=vels(3,i1)
            else
               ua=0.
               va=0.
               wa=0.
            endif
            write(logmess,9040) i1,xa,ya,za,ua,va,wa
            call writloga('default',0,logmess,0,ierr)
         endif
Cdcg     pmat=pic(i1)
Cdcg     rho=ric(i1)
Cdcg     emat=eic(i1)
         tmat=0.0
         trad=0.0
         prad=0.0
         if(iprt3.eq.1) then
            write(logmess,9040) i1,rho,emat,tmat,trad,pmat,prad
            call writloga('default',0,logmess,0,ierr)
         endif
c
         if(iprt4.eq.1) then
            radius=sqrt(xic(i1)*xic(i1)+yic(i1)*yic(i1)
     *              +zic(i1)*zic(i1) )
            call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *       ip,length,itype,ier)
            call cmo_get_info(cvelnm,cmo,ipvels,length,itype,ier)
            if(ier.eq.0) then
               uradius=
     *          sqrt(vels(1,i1)*vels(1,i1)+vels(2,i1)*vels(2,i1)
     *              +vels(3,i1)*vels(3,i1) )
            else
               uradius=0.
            endif
            write(logmess,9040) i1,radius,uradius
            call writloga('default',0,logmess,0,ierr)
         endif
 
        if(iprt5.eq.1) then
           if(nptstmp.le.0) then
              write(logmess,9082)
              call writloga('default',0,logmess,0,ierr)
9082          format('no source points have been assigned.')
           else
             iss=ies1(i1)
              tmdss=tmes1(i1)
              its=its1(i1)
              tmdts=tmts1(i1)
              ips=ips1(i1)
              tmdps=tmps1(i1)
              ivs=ivs1(i1)
              tmdvs=tmvs1(i1)
              write(logmess,9080) i1,iss,tmdss,its,tmdts,ips,tmdps,ivs,
     *                            tmdvs
              call writloga('default',0,logmess,0,ierr)
9080          format(i5,2x,4(i4,2x,1pe12.4))
           endif
        endif
 9000    format('      ipt',' isq',' imt',' itp',' icr',' isn')
 9010    format(i9,4i4,i9)
 9030    format(1x,'ipt',9x,'xa',10x,'ya',10x,'za',
     *                         10x,'ua',10x,'va',10x,'wa')
 9040    format(i5,2x,6(1pe12.4))
 9050    format(1x,'ipt',8x,'rho ',8x,'emat',8x,'tmat',
     *                         8x,'trad',8x,'pmat',8x,'prad')
 9070    format(1x,'ipt',5x,'radius',4x,'velocity')
         goto 9998
 9998    continue
c
c        _______________________________________________________________
c
 100  continue
c
c     ******************************************************************
c
c     release any local memory allocated for this routine.
c
 9997 call mmrelblk('mpary1',isubname,ipmpary1,icscode)
c
c     if (mmrel2 .ne. 0) call mmrelprt(cnamz, ics)
c
c
c     ******************************************************************
c
c     error returns transfer to this statement 9999
c
      goto 9999
 9999 continue
c
c     ******************************************************************
c
      return
      end
 
