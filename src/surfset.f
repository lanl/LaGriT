      subroutine surfset()
C
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS POINTS THAT ARE WITHIN A MINIMUM SEARCH RANGE
C     FROM SURFACE BOUNDARIES AND SETS itp FOR THE POINTS.
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: surfset.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   27 Apr 2000 10:56:26   dcg
CPVCS    check if surfaces exist but no regions or mregions
CPVCS    then the geometry is inconsistent and code will
CPVCS    terminate.
CPVCS    
CPVCS       Rev 1.8   Fri Apr 07 10:34:28 2000   dcg
CPVCS    replace use of KNWPN for length calculation with mmgetblk type 3
CPVCS    remove machine.h
CPVCS    
CPVCS       Rev 1.7   Thu Feb 17 10:24:12 2000   dcg
CPVCS    make a default mregion from bounding box if no geometry exists
CPVCS    
CPVCS       Rev 1.6   Mon Feb 07 16:41:02 2000   dcg
CPVCS    
CPVCS       Rev 1.5   Wed Feb 02 11:45:44 2000   dcg
CPVCS    
CPVCS       Rev 1.4   28 Jan 2000 09:38:44   dcg
CPVCS
CPVCS       Rev 1.43   Wed Sep 01 15:11:44 1999   dcg
CPVCS    replace calls to dotaskgen with calls to dotask
CPVCS
CPVCS       Rev 1.42   Tue Mar 30 16:55:00 1999   dcg
CPVCS    change second call of mmgetblk('csurfnm' to mmggetbk
CPVCS    this is necessary if there are no reflective surfaces
CPVCS
CPVCS       Rev 1.41   Thu Apr 30 11:46:46 1998   dcg
CPVCS    move savpart call to before get_surfaces and get_regions
CPVCS    this call was corrupting the values of the pointers
CPVCS    stored in the pointer_array.
CPVCS
CPVCS       Rev 1.40   Mon Nov 24 16:35:20 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.39   Fri Oct 31 10:50:38 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.38   Mon Apr 14 17:02:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.37   Thu Mar 06 21:49:36 1997   het
CPVCS    Make 2D planes +/-4 epsilon thick.
CPVCS
CPVCS       Rev 1.36   Fri Jan 24 13:41:48 1997   het
CPVCS    Add a default box geometry (using 6 planes) if no geometry exists.
CPVCS
CPVCS       Rev 1.35   Fri May 24 10:47:10 1996   dcg
CPVCS    test nconbnd before calling bndpts
CPVCS
CPVCS       Rev 1.34   Thu May 16 10:28:58 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.33   11/16/95 17:12:10   het
CPVCS    Create the getregv1 routine as a special case.
CPVCS
CPVCS       Rev 1.32   11/07/95 17:27:04   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.31   10/06/95 11:35:40   dcg
CPVCS    move call to get pointers to region data after call
CPVCS    to savpart
CPVCS
CPVCS       Rev 1.30   10/03/95 08:38:38   dcg
CPVCS    move calls to get pointers to surface data after
CPVCS    call to savpart to ensure that pointers do not
CPVCS    change
CPVCS
CPVCS       Rev 1.29   08/29/95 12:11:08   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.28   08/23/95 06:59:08   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.27   08/22/95 06:51:10   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.26   07/14/95 10:16:06   het
CPVCS    Correct errors with point types
CPVCS
CPVCS       Rev 1.25   06/20/95 15:45:12   dcg
CPVCS    remove character literal from argument list to savpart
CPVCS
CPVCS       Rev 1.24   06/19/95 16:43:52   dcg
CPVCS    add blank after literal in calling sequence to savpart
CPVCS
CPVCS       Rev 1.23   06/07/95 15:32:04   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.22   05/26/95 13:16:28   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.21   05/23/95 06:50:20   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.20   05/18/95 12:28:38   ejl
CPVCS    Fixed memory management error
CPVCS
CPVCS       Rev 1.19   05/15/95 13:37:00   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.18   05/11/95 13:53:32   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.17   05/11/95 13:14:08   het
CPVCS    Add new point types for combining free, reflective and interface boundaries
CPVCS
CPVCS       Rev 1.16   05/01/95 08:34:22   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.15   03/31/95 09:10:40   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.14   03/30/95 05:00:58   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.13   03/23/95 22:59:30   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.12   03/23/95 15:08:48   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.11   03/22/95 16:40:54   het
CPVCS    Correct an error with writing 6 doubles into an internal file
CPVCS
CPVCS       Rev 1.10   03/06/95 14:58:38   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.9   02/23/95 23:16:56   het
CPVCS    Construct a default bounding box using the setpts command
CPVCS
CPVCS       Rev 1.8   02/16/95 07:39:16   het
CPVCS    Tries to reconstruct boundary surface information from nodes
CPVCS
CPVCS       Rev 1.7   02/13/95 13:06:10   het
CPVCS    Fix some errors for the CRAY
CPVCS
CPVCS       Rev 1.6   02/13/95 00:13:50   het
CPVCS    Add the match option to the addmesh command
CPVCS
CPVCS       Rev 1.5   02/12/95 08:41:48   het
CPVCS    Correct an error in setting itp1() and icr1() values
CPVCS
CPVCS       Rev 1.4   01/04/95 22:05:38   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   12/24/94 10:52:20   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/19/94 08:27:32   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/17/94 21:30:42   het
CPVCS    Corrected an error comparing the length to two character strings.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:02   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
      include "chydro.h"
      include "consts.h"
      include 'geom_lg.h'
C
C#######################################################################
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer imt1(1000000), itp1(1000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipsurfnum, surfnum(*))
      integer surfnum
      character*32  csurfnam, iword, coperator
      pointer (ipsurft, isurftst(1000000))
      integer isurftst
      character*32 itest,ipartnam
      character*32 isubname, cmo,geom_name
      character*4096 x3d_command, line2, line3, line4
C
      pointer (iplint, lint)
      pointer (iplini, lini)
      pointer (iplinv, linv)
      pointer (iplinc, linc)
      pointer (iplrfl, lrfl)
      pointer (iplfre, lfre)
      logical lint(1000000),lini(1000000),lrfl(1000000),lfre(1000000),
     *          linv(1000000),linc(1000000)
C
C#######################################################################
C
      real*8 epsmin,rout
      parameter ( epsmin=1.0d-08)
C
      integer ierror,i,nsint,length,npoints,ilen,ityp,nconbnd,
     * icscode,idx,iout,lout,itype
      pointer (ipout,out)
      real*8 out(*)
      real*8 srchval
      integer index,len2,len3,len4,ip,npts,lenmm1,imd,
     * idefreg,len0,nxsurf,is,ir,ndef,len1
      real*8 xmin1,xmax1,ymin1,ymax1,xdiff,ydiff,
     * zmin1,zmax1,zdiff
      integer icharlnb,ismin,ismax,icharlnf,ioff
C
C
C#######################################################################
C
C
C
      isubname='surfset'
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,ityp,ierror)
      call cmo_get_info('nconbnd',cmo,nconbnd,length,ityp,ierror)
      if(ierror.ne.0) nconbnd=0
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
 
C
C     ******************************************************************
C  get mesh object name
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C     GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', srchval)
      if(ierror.ne.0) call x3d_error('surfset','cmo_get_name')
C
C     ******************************************************************
C
C     CHECK THAT SURFACE BOUNDARIES EXIST - if not make up surfaces
c     and region, mregion based on a bounding box
c
      if(nsurf.eq.0) then
C
C     ******************************************************************
C
         index=ismin(npoints,xic,1)
         xmin1=xic(index)
         index=ismax(npoints,xic,1)
         xmax1=xic(index)
         xdiff=xmax1-xmin1
         if(abs(xdiff).lt.srchval) then
            xmin1=-10.0*srchval
            xmax1= 10.0*srchval
         endif
C
         index=ismin(npoints,yic,1)
         ymin1=yic(index)
         index=ismax(npoints,yic,1)
         ymax1=yic(index)
         ydiff=ymax1-ymin1
         if(abs(ydiff).lt.srchval) then
            ymin1=-10.0*srchval
            ymax1= 10.0*srchval
         endif
C
         index=ismin(npoints,zic,1)
         zmin1=zic(index)
         index=ismax(npoints,zic,1)
         zmax1=zic(index)
         zdiff=zmax1-zmin1
         if(abs(zdiff).lt.srchval) then
            zmin1=-10.0*srchval
            zmax1= 10.0*srchval
         endif
C
C
C        ***************************************************************
C        ASSIGN SURFACE NODE TYPES BASE ON GEOMETRY
C
         write(line2,*) xmin1,ymin1,zmin1
         write(line3,*) xmax1,ymin1,zmin1
         write(line4,*) xmin1,ymax1,zmin1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEZMIN/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         write(line2,*) xmin1,ymin1,zmax1
         write(line3,*) xmax1,ymin1,zmax1
         write(line4,*) xmin1,ymax1,zmax1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEZMAX/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         write(line2,*) xmin1,ymin1,zmin1
         write(line3,*) xmin1,ymax1,zmin1
         write(line4,*) xmin1,ymin1,zmax1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEXMIN/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         write(line2,*) xmax1,ymin1,zmin1
         write(line3,*) xmax1,ymax1,zmin1
         write(line4,*) xmax1,ymin1,zmax1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEXMAX/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         write(line2,*) xmin1,ymin1,zmin1
         write(line3,*) xmin1,ymin1,zmax1
         write(line4,*) xmax1,ymin1,zmin1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEYMIN/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         write(line2,*) xmin1,ymax1,zmin1
         write(line3,*) xmin1,ymax1,zmax1
         write(line4,*) xmax1,ymax1,zmin1
         len2=icharlnb(line2)
         len3=icharlnb(line3)
         len4=icharlnb(line4)
         x3d_command='surface/PLANEYMAX/reflect/plane/'
     *               // line2(1:len2) // ' '
     *               // line3(1:len3) // ' '
     *               // line4(1:len4) // ' '
     *               // ' ; '
     *               // 'finish'
         call dotask(x3d_command,ierror)
         x3d_command='region/SURFMXMN/ge PLANEZMIN and le PLANEZMAX ' //
     *                               'ge PLANEXMIN and le PLANEXMAX ' //
     *                               'ge PLANEYMIN and le PLANEYMAX ' //
     *               ' ; ' //
     *               'finish'
         call dotask(x3d_command,ierror)
         x3d_command='mregion/mSURFMXMN/' //
     *                               'ge PLANEZMIN and le PLANEZMAX ' //
     *                               'ge PLANEXMIN and le PLANEXMAX ' //
     *                               'ge PLANEYMIN and le PLANEYMAX ' //
     *               ' ; ' //
     *               'finish'
         call dotask(x3d_command,ierror)
C
         call cmo_get_info('nconbnd',cmo,nconbnd,length,ityp,ierror)
         if(ierror.ne.0) nconbnd=0
C
      else
        if(nregs.eq.0.or.nmregs.eq.0) then
           line2=' Inconsistent geometry - '//
     *        'surfaces defined but no regions'//
     *        ' code will stop'
           call writloga('default',0,line2,0,icscode)
           call termcode()
        endif
      endif
C
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
C
C     ******************************************************************
C
      length=npoints
      call mmgetblk('lint',isubname,iplint,length,2,icscode)
      call mmgetblk('lini',isubname,iplini,length,2,icscode)
      call mmgetblk('lrfl',isubname,iplrfl,length,2,icscode)
      call mmgetblk('lfre',isubname,iplfre,length,2,icscode)
      call mmgetblk('linc',isubname,iplinc,length,2,icscode)
      call mmgetblk('linv',isubname,iplinv,length,2,icscode)
C
C
C     ******************************************************************
C
C     SET THE LOGICAL FLAG FOR INTERIOR POINTS. ONLY INTERIOR POINTS ARE
C        CANDIDATES FOR HAVING THEIR POINT-TYPE FLAGS CHANGED.
C
      do ip=1,npoints
         lini(ip)=.false.
         linc(ip)=.false.
         linv(ip)=.false.
         lrfl(ip)=.false.
         lfre(ip)=.false.
         if(itp1(ip).eq.ifitpint) then
            lint(ip)=.true.
         else
            lint(ip)=.false.
         endif
      enddo
C
C        ---------------------------------------------------------------
C        SET 'intrface' IN mregion NAMES AND material number for
c        intrface nodes is one plus number of materials.
C
      ipartnam='intrface'
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,ierror)
      call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierror)
      call mmfindbk('mregdef',geom_name,ipmregdef,length,ierror)
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      nmregs=nmregs+1
      if(nmregs.gt.length) then
         call mmincblk('cmregs',geom_name,ipcmregs,10,ierror)
         call mmincblk('offmregdef',geom_name,ipoffmregdef,10,ierror)
         call mmincblk('ndefmregs',geom_name,ipndefmregs,10,ierror)
         call mmincblk('mregdef',geom_name,ipmregdef,10,ierror)
         call mmincblk('matregs',geom_name,ipmatregs,10,ierror)
      endif
      cmregs(nmregs)='intrface'
      ndefmregs(nmregs)=0
      offmregdef(nmregs)=lastmregdef
      matregs(nmregs)=nmregs
      imd=nmregs
 
      if (nsurf.eq.0) go to 9999
C     ******************************************************************
C
C     GET TEMPORARY MEMORY
C
      npts=npoints
      lenmm1=npts+100
      call mmgetblk('isurftst',isubname,ipsurft,lenmm1,2,icscode)
C
C     ...............................................................
C     GET REGION NUMBER FOR THE REQUESTED REGION AND NO. 0F REGIONS.
C     SET THE REGION NAMES, POINTERS AND NO. ELEMENTS FOR ALL REGIONS
C
      call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
      call mmfindbk('offregdef',geom_name,ipoffregdef,length,ierror)
      call mmfindbk('ndefregs',geom_name,ipndefregs,length,ierror)
      call mmfindbk('regdef',geom_name,ipregdef,length,ierror)
C
C     ******************************************************************
C
C     SET itp FOR POINTS THAT FALL ON INTERFACE SURFACES
C        SET INTERFACE SURFACES DATA POINTERS.
C
      nsint=0
      length=nsurf
      call mmgetblk('surfnum',isubname,ipsurfnum,length,2,icscode)
      do i=1,nsurf
         if(ibtype(i)(1:8).eq.'intrface') then
            nsint=nsint+1
            surfnum(nsint)=i
         endif
      enddo
      if(nsint.ne.0) then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH INTERFACE SURFACES TO FIND POINTS WITHIN srchval
C        FROM THE SURFACE.
C
         do 20 is=1,nsint
            idx=surfnum(is)
C
C           ............................................................
C           CHECK TO SEE IF THIS SURFACE IS USED IN A LE OR GE CONTEXT.
C              IF NOT THEN DON'T TEST THIS SURFACE. BECAUSE BY
C              DEFINITION NO POINTS CAN LIE ON THIS SURFACE IF IT IS
C              NEVER USED.
C
C
C           ************************************************************
C
C           LOOP THROUGH REGION DEFINITIONS.
C
            do ir=1,nregs
C
C              ---------------------------------------------------------
C              SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM
C                 iregs.
C
               ioff=offregdef(ir)
               ndef=ndefregs(ir)
C
C              ---------------------------------------------------------
C              LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C              SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C              ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C              AND LOGICAL OPERATORS(and,or,not).
C
               nxsurf=0
               idefreg=1
               do i=1,ndef
                  iword=regdef(ioff+i)
                  idefreg=idefreg+1
                  len0=icharlnf(iword)
                  if (iword(1:len0) .eq. 'lt' .or.
     &                iword(1:len0) .eq. 'gt' .or.
     &                iword(1:len0) .eq. 'le' .or.
     &                iword(1:len0) .eq. 'ge') then
                     nxsurf=i+1
                     coperator=iword
                   else
                     if (i .eq. nxsurf) then
                        len0=icharlnf(coperator)
                        csurfnam=iword
                        len1=icharlnf(csurfnam)
                        len2=max(len1,icharlnf(csall(idx)))
                        if((coperator(1:len0).eq.'le' .or.
     *                   coperator(1:len0).eq.'ge' .or.
     *                   coperator(1:len0).eq.'eq') .and.
     *                   csall(idx)(1:len2).eq.csurfnam(1:len2))
     *                       then
C
C                          .............................................
C                          USE surftstv TO SEE IF THE POINTS LIE ON THE
C                              SURFACE.
C
                              itest='eq      '
                              call surftstv(xic,yic,zic,npts,srchval,
     *                                      cmo,istype(idx),
     *                                      surfparam(offsparam(idx)+1),
     *                                      sheetnm(idx),
     *                                      itest,isurftst)
C
C                          .............................................
C                          LOOP THROUGH POINTS TO SET itp AND imt IN
C                             FITWORD EXCEPT FOR DUDDED POINTS.
C
                           do ip=1,npts
                              if (.not.lini(ip) .and.
     *                            isurftst(ip) .eq. 1) then
                                 imt1(ip)=imd
                                 lini(ip)=.true.
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            enddo
C
   20    continue
C
      endif
C
C     ******************************************************************
C
C     SET itp FOR POINTS THAT FALL ON CONSTRAINED INTERFACE SURFACEC
C        SET INTERFACE SURFACES DATA POINTERS.
C
      nsint=0
      do i=1,nsurf
         if(ibtype(i)(1:8).eq.'intrcons') then
            nsint=nsint+1
            surfnum(nsint)=i
         endif
      enddo
      if(nsint.ne.0) then
C        ---------------------------------------------------------------
C        LOOP THROUGH INTERFACE SURFACES TO FIND POINTS WITHIN srchval
C        FROM THE SURFACE.
C
         do 40 is=1,nsint
            idx=surfnum(is)
C
C           ............................................................
C           CHECK TO SEE IF THIS SURFACE IS USED IN A LE OR GE CONTEXT.
C              IF NOT THEN DON'T TEST THIS SURFACE. BECAUSE BY
C              DEFINITION NO POINTS CAN LIE ON THIS SURFACE IF IT IS
C              NEVER USED.
C
C
C           ************************************************************
C
C           LOOP THROUGH REGION DEFINITIONS.
C
            do ir=1,nregs
C
C              ---------------------------------------------------------
C              SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM
C                 iregs.
C
               ndef=ndefregs(ir)
               ioff=offregdef(ir)
C
C              ---------------------------------------------------------
C              LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C              SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C              ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C              AND LOGICAL OPERATORS(and,or,not).
C
               nxsurf=0
               idefreg=1
               do i=1,ndef
                  iword=regdef(ioff+i)
                  len0=icharlnf(iword)
                  if (iword(1:len0) .eq. 'lt' .or.
     &                iword(1:len0) .eq. 'gt' .or.
     &                iword(1:len0) .eq. 'le' .or.
     &                iword(1:len0) .eq. 'ge') then
                     nxsurf=i+1
                     coperator=iword
                   else
                     if (i .eq. nxsurf) then
                        len0=icharlnf(coperator)
                        csurfnam=iword
                        len1=icharlnf(csurfnam)
                        len2=max(len1,icharlnf(csall(idx)))
                        if((coperator(1:len0).eq.'le' .or.
     *                      coperator(1:len0).eq.'ge' .or.
     *                      coperator(1:len0).eq.'eq') .and.
     *                      csall(idx)(1:len2).eq.csurfnam(1:len2)) then
C
C                          .............................................
C                          USE surftstv TO SEE IF THE POINTS LIE ON THE
C                              SURFACE.
C
                              itest='eq      '
                              call surftstv(xic,yic,zic,npts,srchval,
     *                                      cmo,istype(idx),
     *                                      surfparam(offsparam(idx)+1),
     *                                      sheetnm(idx),
     *                                      itest,isurftst)
C
C                          .............................................
C                          LOOP THROUGH POINTS TO SET itp AND imt IN
C                             FITWORD EXCEPT FOR DUDDED POINTS.
C
                           do ip=1,npts
                              if (.not.linc(ip) .and.
     *                            isurftst(ip) .eq. 1) then
                                 imt1(ip)=imd
                                 linc(ip)=.true.
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            enddo
C
   40    continue
C
C
      endif
C     ******************************************************************
C
C     SET itp FOR POINTS THAT FALL ON VIRTUAL SURFACES
C        SET VIRTUAL SURFACES DATA POINTERS.
C
      nsint=0
      do i=1,nsurf
         if(ibtype(i)(1:7).eq.'virtual') then
            nsint=nsint+1
            surfnum(nsint)=i
         endif
      enddo
      if(nsint.ne.0) then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH INTERFACE SURFACES TO FIND POINTS WITHIN srchval
C        FROM THE SURFACE.
C
         do 30 is=1,nsint
            idx=surfnum(is)
C
C           ............................................................
C           CHECK TO SEE IF THIS SURFACE IS USED IN A LE OR GE CONTEXT.
C              IF NOT THEN DON'T TEST THIS SURFACE. BECAUSE BY
C              DEFINITION NO POINTS CAN LIE ON THIS SURFACE IF IT IS
C              NEVER USED.
C
C
C           ************************************************************
C
C           LOOP THROUGH REGION DEFINITIONS.
C
            do ir=1,nregs
C
C              ---------------------------------------------------------
C              SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM
C                 iregs.
C
               ndef=ndefregs(ir)
               ioff=offregdef(ir)
C
C              ---------------------------------------------------------
C              LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C              SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C              ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C              AND LOGICAL OPERATORS(and,or,not).
C
               nxsurf=0
               idefreg=1
               do i=1,ndef
                  iword=regdef(ioff+i)
                  len0=icharlnf(iword)
                  if (iword(1:len0) .eq. 'lt' .or.
     &                iword(1:len0) .eq. 'gt' .or.
     &                iword(1:len0) .eq. 'le' .or.
     &                iword(1:len0) .eq. 'ge') then
                     nxsurf=i+1
                     coperator=iword
                   else
                     if (i .eq. nxsurf) then
                        len0=icharlnf(coperator)
                        csurfnam=iword
                        len1=icharlnf(csurfnam)
                        len2=max(len1,icharlnf(csall(idx)))
                        if((coperator(1:len0).eq.'le' .or.
     *                     coperator(1:len0).eq.'ge' .or.
     *                     coperator(1:len0).eq.'eq') .and.
     *                     csall(idx)(1:len2).eq.csurfnam(1:len2)) then
C
C                          .............................................
C                          USE surftstv TO SEE IF THE POINTS LIE ON THE
C                              SURFACE.
C
                              itest='eq      '
                              call surftstv(xic,yic,zic,npts,srchval,
     *                                      cmo,istype(idx),
     *                                      surfparam(offsparam(idx)+1),
     *                                      sheetnm(idx),
     *                                      itest,isurftst)
C
C                          .............................................
C                          LOOP THROUGH POINTS TO SET itp AND imt IN
C                             FITWORD EXCEPT FOR DUDDED POINTS.
C
                           do ip=1,npts
                              if (.not.linv(ip) .and.
     *                            isurftst(ip) .eq. 1) then
                                 imt1(ip)=imd
                                 linv(ip)=.true.
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            enddo
C
   30    continue
C
      endif
C
C     ******************************************************************
C
C     CALL conbld TO SET REFLECTIVE BOUNDARY POINTS AND CONSTRAINTS
C
      if (nconbnd .gt. 0) call conbld()
C
C     ******************************************************************
C
C     SET itp FOR POINTS THAT FALL ON REFLECTIVE SURFACES
C        SET REFLECTIVE SURFACES DATA POINTERS.
C
      nsint=0
      do i=1,nsurf
         if(ibtype(i)(1:7).eq.'reflect') then
            nsint=nsint+1
            surfnum(nsint)=i
         endif
      enddo
      if(nsint.ne.0) then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH REFLECTIVE SURFACES TO FIND POINTS WITHIN srchval
C        FROM THE SURFACE.
C
         do is=1,nsint
            idx=surfnum(is)
C
C           ............................................................
C           CHECK TO SEE IF THIS SURFACE IS USED IN A LE OR GE CONTEXT.
C              IF NOT THEN DON'T TEST THIS SURFACE. BECAUSE BY
C              DEFINITION NO POINTS CAN LIE ON THIS SURFACE IF IT IS
C              NEVER USED.
C
C
C           ************************************************************
C
C           LOOP THROUGH REGION DEFINITIONS.
C
            do ir=1,nregs
C
C              ---------------------------------------------------------
C              SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM
C                 iregs.
C
               ndef=ndefregs(ir)
               ioff=offregdef(ir)
C
C              ---------------------------------------------------------
C              LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C              SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C              ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C              AND LOGICAL OPERATORS(and,or,not).
C
               nxsurf=0
               idefreg=1
               do i=1,ndef
                  iword=regdef(ioff+i)
                  len0=icharlnf(iword)
                  if (iword(1:len0) .eq. 'lt' .or.
     &                iword(1:len0) .eq. 'gt' .or.
     &                iword(1:len0) .eq. 'le' .or.
     &                iword(1:len0) .eq. 'ge') then
                     nxsurf=i+1
                     coperator=iword
                   else
                     if (i .eq. nxsurf) then
                        len0=icharlnf(coperator)
                        csurfnam=iword
                        len1=icharlnf(csurfnam)
                        len2=max(len1,icharlnf(csall(idx)))
                        if((coperator(1:len0).eq.'le' .or.
     *                     coperator(1:len0).eq.'ge' .or.
     *                     coperator(1:len0).eq.'eq') .and.
     *                     csall(idx)(1:len2).eq.csurfnam(1:len2)) then
C
C                          .............................................
C                          USE surftstv TO SEE IF THE POINTS LIE ON THE
C                              SURFACE.
C
                              itest='eq      '
                              call surftstv(xic,yic,zic,npts,srchval,
     *                                      cmo,istype(idx),
     *                                      surfparam(offsparam(idx)+1),
     *                                      sheetnm(idx),
     *                                      itest,isurftst)
C
C                          .............................................
C                          LOOP THROUGH POINTS TO SET itp AND imt IN
C                             FITWORD EXCEPT FOR DUDDED POINTS.
C
                           do ip=1,npts
                              if (.not.lrfl(ip) .and.
     *                            isurftst(ip) .eq. 1) then
                                  lrfl(ip)=.true.
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            enddo
C
         enddo
C
         call mmrelblk('surfnum',isubname,ipsurfnum,icscode)
C
      endif
C
C     ******************************************************************
C
C     SET itp FOR POINTS THAT FALL ON FREE SURFACES
C
      nsint=0
      length=nsurf
      call mmggetbk('surfnum',isubname,ipsurfnum,length,1,icscode)
      do i=1,nsurf
         if(ibtype(i)(1:4).eq.'free') then
            nsint=nsint+1
            surfnum(nsint)=i
         endif
      enddo
      if(nsint.gt.0) then
C
         do is=1,nsint
           idx=surfnum(is)
C
C           ............................................................
C           CHECK TO SEE IF THIS SURFACE IS USED IN A LE OR GE CONTEXT.
C              IF NOT THEN DON'T TEST THIS SURFACE. BECAUSE BY
C              DEFINITION NO POINTS CAN LIE ON THIS SURFACE IF IT IS
C              NEVER USED.
C
C
C           ************************************************************
C
C           LOOP THROUGH REGION DEFINITIONS.
C
            do ir=1,nregs
C
C              ---------------------------------------------------------
C              SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM
C                 iregs.
C
               ndef=ndefregs(ir)
               ioff=offregdef(ir)
C
C              ---------------------------------------------------------
C              LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C              SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C              ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C              AND LOGICAL OPERATORS(and,or,not).
C
               nxsurf=0
               idefreg=1
               do i=1,ndef
                  iword=regdef(ioff+i)
                  len0=icharlnf(iword)
                  if (iword(1:len0) .eq. 'lt' .or.
     &                iword(1:len0) .eq. 'gt' .or.
     &                iword(1:len0) .eq. 'le' .or.
     &                iword(1:len0) .eq. 'ge') then
                     nxsurf=i+1
                     coperator=iword
                   else
                     if (i .eq. nxsurf) then
                        len0=icharlnf(coperator)
                        csurfnam=iword
                        len1=icharlnf(csurfnam)
                        len2=max(len1,icharlnf(csall(idx)))
                        if((coperator(1:len0).eq.'le' .or.
     *                      coperator(1:len0).eq.'ge' .or.
     *                      coperator(1:len0).eq.'eq') .and.
     *                      csall(idx)(1:len2).eq.csurfnam(1:len2)) then
C
C                          .............................................
C                          USE surftstv TO SEE IF THE POINTS LIE ON THE
C                              SURFACE.
C
                              itest='eq      '
                              call surftstv(xic,yic,zic,npts,srchval,
     *                                      cmo,istype(idx),
     *                                      surfparam(offsparam(idx)+1),
     *                                      sheetnm(idx),
     *                                      itest,isurftst)
C
C                          .............................................
C                          LOOP THROUGH POINTS TO SET itp AND imt IN
C                             FITWORD EXCEPT FOR DUDDED POINTS.
C
                           do ip=1,npts
                              if (.not.lfre(ip) .and.
     *                            isurftst(ip) .eq. 1) then
                                 lfre(ip)=.true.
                              endif
                           enddo
                        endif
                     endif
                  endif
               enddo
            enddo
C
         enddo
C
      endif
C
      call mmrelblk('surfnum',isubname,ipsurfnum,icscode)
C
C     ******************************************************************
C
C     SET THE POINT TYPES BASED ON A TRUTH TABLE.
C   NOTE: set lini if linc is set -- same results on point type
C
C     NAME                         INT  INI  RFL  FRE VRT
C     ************************     ***  ***  ***  *** ***
C
C     ifitpint: INT (INTERIOR)     T    F    F    F    F
C     ifitpini: INI (INTERFACE)    T    T    F    F    F
C     ifitprfl: RFL (REFLECTIVE)   T    F    T    F    F
C     ifitpfre: FRE (FREE)         T    F    F    T    F
C     ifitpirb: INI-RFL            T    T    T    F    F
C     ifitpifb: INI-FRE            T    T    F    T    F
C     ifitprfb: RFL-FRE            T    F    T    T    F
C     ifitpirf: INI-RFL-FRE        T    T    T    T    F
C     ifitpvrt: VRT(VIRTUAL)       T    F    F    F    T
C     ifitpvin: VRT-INI            T    T    F    F    T
C     ifitpvrb: VRT-RFL            T    F    T    F    T
C     ifitpvfb: VRT-FRE            T    F    F    T    T
C     ifitpvrf: VRT-FRE-RFL        T    F    T    T    T
C     ifitpvir: VRT-INI-RFL        T    T    T    F    T
C     ifitpvif: VRT-INI-FRE        T    T    F    T    T
C     ifitpalb: VRT-INI-FRE-RFL    T    T    T    T    T
C
      do ip=1,npoints
         if(lint(ip)) then
            if (linc(ip)) lini(ip)=.true.
             if(.not.lini(ip).and..not.lrfl(ip).and..not.lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpint
                go to 9998
             endif
             if(     lini(ip).and..not.lrfl(ip).and..not.lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpini
                go to 9998
             endif
             if(.not.lini(ip).and.     lrfl(ip).and..not.lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitprfl
                go to 9998
             endif
             if(.not.lini(ip).and..not.lrfl(ip).and.     lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpfre
                go to 9998
             endif
             if(     lini(ip).and..not.lrfl(ip).and.     lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpifb
                go to 9998
             endif
             if(     lini(ip).and.     lrfl(ip).and..not.lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpirb
                go to 9998
             endif
             if(     lini(ip).and.     lrfl(ip).and.     lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitpirf
                go to 9998
             endif
             if(.not.lini(ip).and.     lrfl(ip).and.     lfre(ip).and.
     *          .not.linv(ip)) then
                itp1(ip)=ifitprfb
                go to 9998
             endif
             if(.not.lini(ip).and..not.lrfl(ip).and..not.lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvrt
                go to 9998
             endif
             if(     lini(ip).and..not.lrfl(ip).and..not.lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvin
                go to 9998
             endif
             if(.not.lini(ip).and.     lrfl(ip).and..not.lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvrb
                go to 9998
             endif
             if(.not.lini(ip).and..not.lrfl(ip).and.     lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvfb
                go to 9998
             endif
             if(     lini(ip).and..not.lrfl(ip).and.     lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvif
                go to 9998
             endif
             if(     lini(ip).and.     lrfl(ip).and..not.lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvir
                go to 9998
             endif
             if(     lini(ip).and.     lrfl(ip).and.     lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpalb
                go to 9998
             endif
             if(.not.lini(ip).and.     lrfl(ip).and.     lfre(ip).and.
     *               linv(ip)) then
                itp1(ip)=ifitpvrf
                go to 9998
             endif
         endif
 9998    continue
      enddo
C     ******************************************************************
C
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
C     RELEASE TEMPORARY MEMORY
C
      call mmrelprt(isubname,icscode)
C
C     ******************************************************************
C
      return
      end
