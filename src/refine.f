      subroutine refine(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine call the correct grid refinement routine
C           depending on the type and dimensionality of the CMO.
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
C
C        $Log: refine.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.13   05 Jan 2007 12:34:58   tam
CPVCS    removed lines with integer idebug, already declared in
CPVCS    chydro.h which the mac does not like
CPVCS    
CPVCS       Rev 1.12   30 Oct 2006 14:05:34   gable
CPVCS    Added debugging IO withing addpts parts of code.
CPVCS    
CPVCS       Rev 1.11   24 Dec 2003 10:23:18   tam
CPVCS    add 'amr' iprd option to the refine command 
CPVCS    change screen messege to indicate element type instead of 'tet'
CPVCS    
CPVCS       Rev 1.10   17 Dec 2001 09:54:34   dcg
CPVCS    fix errors in faceedge and tetedge options
CPVCS 
CPVCS       Rev 1.9   17 Dec 2001 06:59:48   gable
CPVCS    Added option to refine on element sets.
CPVCS    
CPVCS       Rev 1.8   29 May 2001 17:52:34   kuprat
CPVCS    We now pass PSETNAME=-def- in the case of 2-D with no surface.
CPVCS    In this case the entire 2-D grid will be in the 'pset' for refinement.
CPVCS    
CPVCS       Rev 1.7   16 Nov 2000 15:24:26   tam
CPVCS    make equivalent node attributes imt,imt1, itp,itp1,icr,icr1,isn,isn1
CPVCS    
CPVCS       Rev 1.6   03 Oct 2000 09:45:00   dcg
CPVCS    remove unused references to ialias
CPVCS    
CPVCS       Rev 1.5   21 Jun 2000 11:44:40   dcg
CPVCS    don't pass cmo name - get active cmo name in gradient
CPVCS    
CPVCS       Rev 1.4   20 Jun 2000 10:33:14   dcg
CPVCS    get volumes of all types of elements - 
CPVCS    modify amount of temp space used
CPVCS    
CPVCS       Rev 1.3   Wed Apr 05 11:14:48 2000   dcg
CPVCS    add 'spawn' option
CPVCS    
CPVCS       Rev 1.2   28 Jan 2000 16:51:12   dcg
CPVCS    
CPVCS       Rev 1.1   26 Jan 2000 16:26:52   dcg
CPVCS    
CPVCS       Rev 1.0   25 Jan 2000 15:44:04   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.76   Wed Nov 10 15:22:02 1999   dcg
CPVCS    declare nnfreq as local variable - get value from storage block
CPVCS
CPVCS       Rev 1.75   Fri Jul 23 09:39:20 1999   dcg
CPVCS    remove calls to open_face
CPVCS    this code was never implemented
CPVCS    was meant to do de-refinement
CPVCS
CPVCS       Rev 1.74   Fri Jun 11 15:34:58 1999   dcg
CPVCS    make refine/../edge for 2d grids work like refine/../face
CPVCS    in 2d edges are facets hence original thinking but
CPVCS    edges are always edges so the 'edge' syntax should work
CPVCS
CPVCS       Rev 1.73   Wed Feb 10 11:24:06 1999   dcg
CPVCS    add refine/interface option
CPVCS
CPVCS       Rev 1.73   Wed Feb 10 11:20:56 1999   dcg
CPVCS
CPVCS       Rev 1.72   Mon Jan 04 16:26:48 1999   dcg
CPVCS    put error message and quit if mesh has no elements
CPVCS
CPVCS       Rev 1.71   Wed Dec 23 15:41:48 1998   dcg
CPVCS    add command option refine/edge_list
CPVCS
CPVCS       Rev 1.69   Mon Nov 16 10:40:26 1998   kuprat
CPVCS    Added explicit calls to 'recon', after calls to 'cel' and 'cel_chain'.
CPVCS
CPVCS       Rev 1.68   Fri Jul 24 15:45:28 1998   dcg
CPVCS    fix test on options
CPVCS
CPVCS       Rev 1.67   Fri Jul 24 15:17:44 1998   dcg
CPVCS    add refine/rivara_boundary option
CPVCS
CPVCS       Rev 1.66   Thu Jul 02 16:02:50 1998   dcg
CPVCS    add call to truncated rivara refine
CPVCS
CPVCS       Rev 1.64   Tue Jun 23 15:17:38 1998   dcg
CPVCS    changes for refine/rivara command
CPVCS
CPVCS       Rev 1.63   Fri Oct 17 11:18:26 1997   dcg
CPVCS    replace print statement
CPVCS
CPVCS       Rev 1.62   Fri Oct 10 12:46:38 1997   tam
CPVCS    added check for nnfreq value before calling recon
CPVCS
CPVCS       Rev 1.61   Fri Oct 03 11:02:42 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS
CPVCS       Rev 1.59   Mon Apr 14 16:58:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.58   Sun Feb 23 10:37:36 1997   het
CPVCS    Add the refinement for quads.
CPVCS
CPVCS       Rev 1.57   Fri Jan 24 14:25:22 1997   het
CPVCS    Correct an error in the refine/edge/ command.
CPVCS
CPVCS       Rev 1.56   Mon Nov 11 20:55:08 1996   het
CPVCS    Make the call for unimplemented refine CMO anyway.
CPVCS
CPVCS       Rev 1.55   Wed Jul 24 17:33:46 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.54   Thu Jun 27 14:55:22 1996   het
CPVCS    For addpts use the names of points without duplicating the points.
CPVCS
CPVCS       Rev 1.53   Fri May 24 13:58:24 1996   het
CPVCS    Correct an error in the addpts option for edge points.
CPVCS
CPVCS       Rev 1.52   Tue Apr 30 07:26:22 1996   het
CPVCS    Change the refine_add_pts option for imt1=0 points.
CPVCS
CPVCS       Rev 1.51   Tue Apr 09 16:42:10 1996   dcg
CPVCS    declare coption_len to be integer
CPVCS
CPVCS       Rev 1.50   Tue Apr 02 02:29:02 1996   het
CPVCS    Change this routine to give new nodes names.
CPVCS
CPVCS       Rev 1.49   Thu Mar 14 13:38:48 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.48   Tue Mar 05 12:32:56 1996   het
CPVCS    Add reconnection for tets and tris when using the addpts option.
CPVCS
CPVCS       Rev 1.47   Fri Feb 16 21:51:32 1996   het
CPVCS    Fix an error with the exclusive/inclusive option
CPVCS
CPVCS       Rev 1.46   Fri Feb 02 14:23:48 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.45   Thu Feb 01 01:45:10 1996   het
CPVCS    Fix an error in qualifying a xfield.
CPVCS
CPVCS       Rev 1.44   Wed Jan 31 12:57:58 1996   het
CPVCS    Correct an error with the field variable.
CPVCS
CPVCS       Rev 1.43   Mon Jan 29 22:21:28 1996   het
CPVCS
CPVCS    Fix some errors with the refine/addpts option
CPVCS
CPVCS       Rev 1.42   Wed Jan 24 06:07:34 1996   het
CPVCS    Fix an error in the calculation of element refinement coordinates.
CPVCS
CPVCS       Rev 1.41   Tue Jan 23 09:18:20 1996   het
CPVCS    Fix an error in copying isn1 numbers.
CPVCS
CPVCS       Rev 1.40   Fri Dec 22 14:18:26 1995   het
CPVCS    Correct errors for inside_ routines.
CPVCS
CPVCS       Rev 1.39   11/22/95 09:07:36   dcg
CPVCS    fix confusion between ielmface and ielmedge
CPVCS
CPVCS       Rev 1.38   11/17/95 15:22:32   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.37   11/16/95 17:12:22   het
CPVCS    Start to add all the functions for refine.
CPVCS
CPVCS       Rev 1.36   11/07/95 17:24:02   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.35   11/07/95 11:28:04   het
CPVCS    Modify the 2D triangle refinement algorithms.
CPVCS
CPVCS       Rev 1.34   10/22/95 13:46:20   het
CPVCS    Insert some cmo_get_info calles
CPVCS
CPVCS       Rev 1.33   10/22/95 13:17:42   het
CPVCS    Correct a memory management error
CPVCS
CPVCS       Rev 1.32   10/20/95 10:48:44   het
CPVCS    Fix iparent memory management error and add new refine options.
CPVCS
CPVCS       Rev 1.31   10/19/95 17:01:30   het
CPVCS    Add the refine/edge refine/face refine/tet commands
CPVCS
CPVCS       Rev 1.30   10/18/95 12:15:34   het
CPVCS    Dud out the original points that are being added
CPVCS
CPVCS       Rev 1.29   10/05/95 15:46:44   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.28   10/04/95 07:44:36   het
CPVCS    Add the addpts option to the refine commands
CPVCS
CPVCS       Rev 1.27   09/29/95 09:13:42   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.26   09/11/95 14:41:48   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.25   08/30/95 21:09:04   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.24   08/29/95 12:03:06   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.23   08/16/95 00:23:36   het
CPVCS    Correct errors in recon and refine calles
CPVCS
CPVCS       Rev 1.22   08/15/95 18:24:30   het
CPVCS    Cleanup code and correct errors
C
C ######################################################################
C
      implicit none
 
C
      character*132 logmess,cbuf
C
      include "local_element.h"
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,nen,ilen,itype,icscode,nef,nsd,npoints,
     * npoints_save,ipointi,ipointj,imesh_type
      integer icharlnf
      integer iprd
C
      character*32 cmo,isubname,mesh_type
C
C#######################################################################
C
C
      isubname='refine'
      iprd=0
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,9000)
 9000    format('No CMOs defined')
         call writloga('default',1,logmess,1,ierror)
         goto 9999
      endif
C
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
      call cmo_get_mesh_type(cmo,mesh_type,imesh_type,icscode)
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierror)
      npoints_save=npoints
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
c 
      if((cmsgin(2) .eq. 'element_set').or.
     *   (cmsgin(2) .eq. 'eltset'))then
C
C----------------------------------------------------------------
C
C        The following lagrit command will refine all elements
C        int the specified element set.
C        Syntax:
C              refine/element_set /eltset get eltset_name
C              refine/eltset      /eltset get eltset_name
C
C        Internally the two lagrit commands will be issued.
c        The first command creates a point set (pset) of
c        all nodes in the specified element set.  
c
c   pset/refine_eltset_pset_tmp_abc_xyz/eltset/eltset_name
c
C        The second command refines all elements all of whose nodes
c        are in the point set.  We send to refine the
c        criterion of 'constant'  and
c        test the value of imt.  All nodes in the point set
c        will satisfy this criterion because all material numbers
c        are always >=0.
C
C    refine/constant/imt1/linear/element/pset,get,p_fault/-1.,0.,0./exclusive
C
C          Create a temporary pset from the eltset
C
           cbuf='pset/refine_eltset_pset_tmp_abc_xyz/eltset/'//
     *      cmsgin(5)(1:icharlnf(cmsgin(5)))//
     *      '/ ; finish'
            call dotaskx3d(cbuf,ierror)
C
C        End of creating pset from eltset
C
C     Refine on the pset
C
           cbuf='refine/constant/imt1/linear/element/pset,get,'//
     *      'refine_eltset_pset_tmp_abc_xyz/'//
     *      '-1.,0.,0./exclusive'//
     *      '/ ; finish'
            call dotaskx3d(cbuf,ierror)
 
C
C     Release temporary pset
C
         cbuf = 'pset/refine_eltset_pset_tmp_abc_xyz/delete'//
     *      '/ ; finish'
            call dotaskx3d(cbuf,ierror)
C
 
C----------------------------------------------------------------
C

      elseif(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         call refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      elseif(nen.eq.nelmnen(ifelmqud).and.nef.eq.nelmnef(ifelmqud).and.
     *       nsd.eq.2) then
         call refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      elseif(nen.eq.nelmnen(ifelmhex).and.nef.eq.nelmnef(ifelmhex).and.
     *       nsd.eq.3) then
         call refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      else
         write(logmess,9010) cmo(1:icharlnf(cmo))
 9010    format('Refine on this CMO type is not implemented: ',a)
         call writloga('default',1,logmess,0,ierror)
         write(logmess,9015) mesh_type(1:8)
 9015    format('mesh_type:         ',a10)
         call writloga('default',0,logmess,0,ierror)
         write(logmess,9011) nen
 9011    format('nodes_per_element: ',i10)
         call writloga('default',0,logmess,0,ierror)
         write(logmess,9012) nef
 9012    format('faces_per_element: ',i10)
         call writloga('default',0,logmess,0,ierror)
         write(logmess,9014) nsd
 9014    format('ndimensions_topo:  ',i10)
         call writloga('default',0,logmess,1,ierror)

C*****   goto 9999
 
         call refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      endif
C
      call cmo_get_info('nnodes',cmo,
     *                  npoints,ilen,itype,ierror)
      ipointi=npoints_save+1
      ipointj=npoints
C
      call cmo_set_info('ipointi',cmo,ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_set_info('ipointj',cmo,ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      goto 9999
 
 9999 continue
      return
      end
c
      subroutine refine3d(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Mark tets that need to be refined according to the
C        criterion given on the input line.
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
C ######################################################################
C
      implicit none
C
C ######################################################################
C
C
      include "local_element.h"
      include "chydro.h"
C
C ######################################################################
C
      integer nwds, imsgin(12), msgtype(12)
      real*8 xmsgin(12)
      character*(*) cmsgin(12)
C
      integer ierror
      integer npoints,ilen,itype,icscode,ntets,nen,nef,nsd,ierr,
     *  length,len,interp,mpno,ipt1,ipt2,ipt3,inclusive,
     *  it,if,i1,nee,nadd,ie,i2,j,isum,
     *  jt,jf,j1,j2,nadd1,ierrw,k,
     *  mbndry,ityp,ipointi,ipointj,ierrwrt,
     *  jadd,imesh_type,
     *  lenc,isum12,ier,npoints_save,
     *  naddelm,ierr2,ics,lenout,i
      real*8 ref_distance,xsum,distance,xedge,yedge,zedge,
     * xface,yface,zface
C
C#######################################################################
C
 
      pointer (ipout,iout(*))
      integer iout
      pointer (ipisetwd, isetwd(*))
      pointer (ipimt1, imt1(*))
      pointer (ipitp1, itp1(*))
      pointer (ipicr1, icr1(*))
      pointer (ipisn1, isn1(*))
      pointer (ipxic, xic(*))
      pointer (ipyic, yic(*))
      pointer (ipzic, zic(*))
      integer isetwd,imt1,itp1,icr1,isn1
      real*8 xic,yic,zic
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
C
C
      pointer (ipitadd, itadd)
      pointer (ipitadd2, itadd2)
      pointer (ipifadd, ifadd)
      pointer (ipieadd, ieadd)
      pointer (ipitdone, itdone)
      pointer (iplist, list)
      integer itadd(*), ifadd(*), ieadd(*),
     *        list(*), itdone(*),itadd2(*)
      pointer (ipiadd, iadd)
      integer iadd(*)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(*), yadd(*), zadd(*)
C
      pointer (ipnedge_bin, nedge_bin)
      pointer (ipnedge_off, nedge_off)
      pointer (ipxedge1, xedge1)
      pointer (ipyedge1, yedge1)
      pointer (ipzedge1, zedge1)
      integer nedge_bin(*), nedge_off(*)
      real*8 xedge1(*), yedge1(*), zedge1(*)
C
      real*8 xrefine(3)
C
      pointer (ipmpary, mpary(*))
      pointer (ipxfield, xfield(*))
      integer mpary
      real*8 xfield, toldamage
C
      pointer (ipiarray, iarray)
      integer iarray(*)
C
      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipintp,intp)
      integer intp(*)
c
      character*32 ich1,ich2,ich3,ctype,crank,clen,cinter,cpers,cio
C
      character*132 logmess,cbuf
C
      character*32 iblknam, cmo ,cmode, surfname,psetname
C
C
      character*32 coption, itopo, cinterp, mesh_type
      integer coption_len,ioption,ierror_return,index,iprd
      character*32 isubname,cnewx,cnewy,cnewz
      integer icharlnf
 
C
C ######################################################################
C
C
C
      isubname='refine'
c
      ierror = 0
c
c
c           ************************************************************
 
      coption=cmsgin(2)
      coption_len=icharlnf(coption)
      if(coption(1:coption_len).eq.'junction') ioption=1
      if(coption(1:coption_len).eq.'constant') ioption=2
      if(coption(1:coption_len).eq.'maxsize') ioption=3
      if(coption(1:coption_len).eq.'minsize') ioption=-4
      if(coption(1:coption_len).eq.'aspect') ioption=5
      if(coption(1:coption_len).eq.'delta') ioption=6
      if(coption(1:coption_len).eq.'grading') ioption=-7
      if(coption(1:coption_len).eq.'lambda') ioption=8
      if(coption(1:coption_len).eq.'lambdade') ioption=-9
      if(coption(1:coption_len).eq.'testing') ioption=-10
      if(coption(1:coption_len).eq.'errormax') ioption=-11
      if(coption(1:coption_len).eq.'addpts') ioption=12
      if(coption(1:coption_len).eq.'rmelements') ioption=-13
      if(coption(1:coption_len).eq.'point') ioption=-14
      if(coption(1:coption_len).eq.'edge') ioption=-15
      if(coption(1:coption_len).eq.'face') ioption=-16
      if(coption(1:coption_len).eq.'element' .or.
     *   coption(1:coption_len).eq.'tet') ioption=-17
      if(coption(1:coption_len).eq.'rivara'.or.
     *   coption(1:coption_len).eq.'rivara_boundary'.or.
     *   coption(1:coption_len).eq.'rivara_truncated') ioption=18
      if(coption(1:coption_len).eq.'cel') ioption=19
      if(coption(1:coption_len).eq.'roughness') ioption=20
      if(coption(1:coption_len).eq.'edge_list') ioption=21
      if(coption(1:coption_len).eq.'interface') ioption=22
      if(coption(1:coption_len).eq.'intrface') ioption=22
      if(coption(1:coption_len).eq.'spawn') ioption=23
      if(ioption.eq.0) then
         write(logmess,'(a,a)') 'illegal refine option: ',
     *                          coption(1:coption_len)
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      if(ioption.lt.0) then
         write(logmess,'(a,a)') 'unimplemented option: ',
     *                          coption(1:coption_len)
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif

c AMR PRD - principal refine direction based on topology (het version) 
c get prd value to pass into the amr refine command
c this is the original syntax for the refine command
c look for keyword 'amr' then iprd value after that
c iprd 0 = xyz using refine_hex_add()
c iprd 4 or 123 = xyz using refine_hex_prd()
c iprd 1 = x   2 = y   3 = z
c iprd 12 = xy 13 = xz 23 = yz 
c improved syntax should include options for alternate
c topology or geometry and use of a refine level attribute 
      iprd=0
      do i= 2, nwds-1
        if (msgtype(i).eq.3) then
          if(cmsgin(i)(1:3).eq.'amr') then
C
            if(msgtype(i+1).eq.1) then
               iprd=imsgin(i+1)
               if (iprd.eq.123) iprd = 4
            elseif(msgtype(i+1).eq.2) then
               iprd=nint(xmsgin(i))
            elseif(msgtype(i+1).eq.3) then
               iprd=0
            endif
            iprd=max(0,min(23,iprd))
          endif
        endif
      enddo
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)') 'REFINE found bad mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,icscode)
      if(ntets.le.0) then
         write(logmess,'(a)') 'mesh has no elements cannot refine '
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('edges_per_element',cmo,nee,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,icscode)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
      call cmo_get_intinfo('idebug',cmo,idebug,ilen,ityp,ierr)
C
C
C     ************************************************************
C
C     Get the parents for each node.
C
      length=npoints
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      len=icharlnf(cmsgin(4))
      cinterp(1:len)=cmsgin(4)
      interp=1
      if(cinterp(1:6).eq.'linear') interp=1
      if(cinterp(1:3).eq.'log') interp=2
      if(cinterp(1:5).eq.'asinh') interp=3
      itopo=cmsgin(5)
      len=icharlnf(itopo)
      length=npoints
      call mmgetblk('mpary' ,isubname,ipmpary,length,1,icscode)
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(6).eq.1) then
         ipt1=imsgin(6)
         ipt2=imsgin(7)
         ipt3=imsgin(8)
C
         call cmo_get_info('ipointi',cmo,
     *                   ipointi,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
         call cmo_get_info('ipointj',cmo,
     *                   ipointj,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
         if(ipt1.eq.0) ipt1=ipointi
         if(ipt2.eq.0) ipt2=ipointj
         if(ipt3.eq.0) ipt3=1
         ich3='-def-'
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      else
         ich1=cmsgin(6)
         ich2=cmsgin(7)
         ich3=cmsgin(8)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      endif
C
      if (msgtype(9).eq.3.and.coption.eq.'spawn') then
        cnewx=cmsgin(9)
        cnewy=cmsgin(10)
        cnewz=cmsgin(11)
      else
        xrefine(1)=xmsgin(9)
        if (msgtype(10).eq.3.and.coption.eq.'roughness')
     *   surfname=cmsgin(10)
        xrefine(2)=xmsgin(10)
        xrefine(3)=xmsgin(11)
      endif
C
      inclusive=0
      if(nwds.ge.12 .and.
     *   cmsgin(12)(1:9).eq.'inclusive') inclusive=1
C
C
C  Branch on option
C
      if(coption(1:coption_len).eq.'spawn') then
         call refine_spawn_lg(mpno,mpary,cnewx,cnewy,cnewz,ierror)
      elseif(coption(1:coption_len).eq.'cel'
     *     .and.itopo(1:len).eq.'edge') then
         call get_epsilon('epsilonl',toldamage)
         call cel(cmo,xrefine(1),toldamage,mpary,mpno,inclusive,
     *            ich3,ierror)
         write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &      ,'/ ; finish'
         call dotaskx3d(cbuf,ierr)
         go to 9998
      elseif(coption(1:coption_len).eq.'rivara_truncated'
     *     .and.itopo(1:len).eq.'edge') then
         call get_epsilon('epsilonl',toldamage)
         cmode='truncated'
         call cel_chain(cmo,xrefine(1),toldamage,mpary,mpno,inclusive,
     *        ich3,cmode,ierror)
         write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &      ,'/ ; finish'
         call dotaskx3d(cbuf,ierr)
         go to 9998
      elseif(coption(1:coption_len).eq.'rivara_boundary'
     *     .and.itopo(1:len).eq.'edge') then
         call get_epsilon('epsilonl',toldamage)
         cmode='boundary'
         call cel_chain(cmo,xrefine(1),toldamage,mpary,mpno,inclusive,
     *        ich3,cmode,ierror)
         write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &      ,'/ ; finish'
         call dotaskx3d(cbuf,ierr)
         go to 9998
      elseif(coption(1:coption_len).eq.'rivara'
     *     .and.itopo(1:len).eq.'edge') then
         call get_epsilon('epsilonl',toldamage)
         cmode='full'
         call cel_chain(cmo,xrefine(1),toldamage,mpary,mpno,inclusive,
     *        ich3,cmode,ierror)
         write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &      ,'/ ; finish'
         call dotaskx3d(cbuf,ierr)
         go to 9998
      elseif(coption(1:coption_len).eq.'roughness'
     *     .and.itopo(1:len).eq.'edge') then
         call get_epsilon('epsilonl',toldamage)
         if (nsd.ne.2.or.surfname(1:icharlnf(surfname)).ne.'-def-') then
            cbuf = 'pset/-rruf-/surface/'//
     *         surfname(1:icharlnf(surfname))//'/'//
     *         cmsgin(6)(1:icharlnf(cmsgin(6)))//'/'//
     *         cmsgin(7)(1:icharlnf(cmsgin(7)))//'/'//
     *         cmsgin(8)(1:icharlnf(cmsgin(8)))//
     *         '/ ; finish'
            call dotaskx3d(cbuf,ierr)
            psetname='-rruf-'
         else
            psetname='-def-'
         endif
         call mmgetblk('intp',isubname,ipintp,npoints,1,icscode)
         do i=1,mpno
            intp(mpary(i))=1
         enddo
         cmode='truncated'
         call cer_chain(cmo,xrefine(1),toldamage,mpary,mpno,inclusive,
     *        psetname,cmode,ierror)
         go to 9998
      elseif(coption(1:coption_len).eq.'edge_list'
     *     .and.itopo(1:len).eq.'edge') then
         call refine_edge_list_lg(nwds,imsgin,xmsgin,cmsgin,ierror)
         go to 9998
      elseif((coption(1:coption_len).eq.'interface'.or.
     *        coption(1:coption_len).eq.'intrface').and.
     *        itopo(1:len).eq.'edge') then
         call refine_interface_elements_lg(imsgin,xmsgin,cmsgin
     *     ,msgtype,nwds,ierror)
         go to 9998
      endif
C
C
      if(coption(1:coption_len).eq.'addpts') then
         length=mpno
         call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
         call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
         call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
         call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
         nadd=mpno
         do i=1,mpno
            i1=mpary(i)
            if(imt1(i1).lt.0) then
               iadd(i)=0
               itp1(i1)=ifitpdud
            elseif(imt1(i1).eq.0) then
               iadd(i)=-i1
            elseif(imt1(i1).gt.0) then
               iadd(i)=i1
            endif
            xadd(i)=xic(i1)
            yadd(i)=yic(i1)
            zadd(i)=zic(i1)
         enddo
         if(idebug .ge. 3)then
         write(logmess,'(a,i9)')
     *        'refine addpts: #candidates = ',mpno
         call writloga('default',0,logmess,0,ierrw)
         endif
         
         call refine_add_pts(cmo,itopo,
     *               mpno,ipiadd,ipxadd,ipyadd,ipzadd)
         goto 9998
      endif
C
      if(itopo(1:len).eq.'tet'.or.
     *   itopo(1:len).eq.'element') then
         write(logmess,'(a)') "refine elements: "
         call writloga('default',0,logmess,0,ierrw)
         if(coption(1:coption_len).ne.'addpts' .and.
     *      coption(1:coption_len).ne.'rmelements') then
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           CHECK TO SEE IF THE SPECIFIED FIELD EXISTS
C
            call cmo_get_name(cmo,ierror)
            len=icharlnf(cmsgin(3))
            iblknam=' '
            iblknam(1:len)=cmsgin(3)
            if(len.eq.3) then
              if(iblknam(1:3).eq.'itp') iblknam='itp1'
              if(iblknam(1:3).eq.'imt') iblknam='imt1'
              if(iblknam(1:3).eq.'icr') iblknam='icr1'
              if(iblknam(1:3).eq.'isn') iblknam='isn1'
              len=icharlnf(iblknam)
            endif
            call mmfindbk(iblknam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               ctype=' '
               call cmo_get_attparam(iblknam,cmo,index,ctype,
     *           crank,clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(iblknam,cmo,ipiarray,lenout,icscode)
                  length=npoints
                  call mmgetblk('xfield',isubname,ipxfield,length,2,ics)
                  do i=1,npoints
                     xfield(i)=iarray(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(iblknam,cmo,ipxfield,lenout,icscode)
               endif
            else
               write(logmess,'(a,a)') 'Field does not exist: ',
     *                                iblknam(1:len)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9998
            endif
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
            length=nee*ntets
            call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
            call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
            call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
            call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
            call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
            call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
            call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
            nadd=0
            call gradient(itopo,coption,
     *                    inclusive,
     *                    npoints,ntets,
     *                    ipxfield,interp,
     *                    xrefine,
     *                    mpno,ipmpary,
     *                    nadd,ipitadd,ipifadd,ipieadd)
         elseif(coption(1:coption_len).eq.'rmelements') then
            length=ntets
            call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
            length=mpno
            call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
            call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
            call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
            call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
            nadd=mpno
            do i=1,mpno
               i1=mpary(i)
               iadd(i)=0
               xadd(i)=xic(i1)
               yadd(i)=yic(i1)
               zadd(i)=zic(i1)
            enddo
            call table_element(cmo,
     &                         ipxadd,ipyadd,ipzadd,nadd,
     &                         ipitadd,
     &                         ierr2)
            naddelm=0
            do i=1,nadd
               if(itadd(i).gt.0) then
                  naddelm=naddelm+1
                  itadd(naddelm)=itadd(i)
                  iadd(naddelm)=0
                  xadd(naddelm)=xadd(i)
                  yadd(naddelm)=yadd(i)
                  zadd(naddelm)=zadd(i)
               endif
            enddo
            nadd=naddelm
         endif
         if(coption(1:coption_len).eq.'minsize' .or.
     *      coption(1:coption_len).eq.'lambdade' .or.
     *      coption(1:coption_len).eq.'rmelements') then
            write(logmess,201)  coption(1:coption_len)
 201        format('Option not implemented ',a)
            call writloga('default',0,logmess,0,ierrw)
            go to 9999
         else
            do i=1,nadd
               it=itadd(i)
               xsum=0.0
               iadd(i)=0
               xadd(i)=0.0
               yadd(i)=0.0
               zadd(i)=0.0
               do j=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+j)
                  xsum=xsum+1.0
                  xadd(i)=xadd(i)+xic(i1)
                  yadd(i)=yadd(i)+yic(i1)
                  zadd(i)=zadd(i)+zic(i1)
               enddo
               xadd(i)=xadd(i)/xsum
               yadd(i)=yadd(i)/xsum
               zadd(i)=zadd(i)/xsum
            enddo

c           Write type of elements to refine 
            mesh_type = 'notset'
            call cmo_get_mesh_type(cmo,mesh_type,imesh_type,ier)
            if (ier.ne.0 .or. imesh_type.le.0) then
             write(logmess,'(a,a,a,a)')'WARNING: Undefined mesh type: ',
     >       mesh_type(1:8),' for ',cmo
             call writloga('default',0,logmess,1,ierrw)
            else
             write(logmess,'(a,a,i10)') 
     >        'refine ',mesh_type(1:8)//': ',nadd
              call writloga('default',0,logmess,0,ierrw)
            endif
            



            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_element_add(cmo, 
     *                                 iprd, nadd,
     *                                 ipitadd, 
     *                                 ipiadd,ipxadd,ipyadd,ipzadd)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
               call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *               ityp,ier)
               call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
               if(npoints_save.ne.npoints) then
                  ipointi=npoints_save+1
                  ipointj=npoints
                  call cmo_set_info('ipointi',cmo,
     &                            ipointi,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
                  call cmo_set_info('ipointj',cmo,
     &                            ipointj,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
               endif
            endif
         endif
         call mmrelblk("itadd",isubname,ipitadd,icscode)
         call mmrelblk("iadd",isubname,ipiadd,icscode)
         call mmrelblk("xadd",isubname,ipxadd,icscode)
         call mmrelblk("yadd",isubname,ipyadd,icscode)
         call mmrelblk("zadd",isubname,ipzadd,icscode)
      elseif(itopo(1:len).eq.'face') then
         write(logmess,'(a)') "refine faces: "
         call writloga('default',0,logmess,0,ierrw)
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           CHECK TO SEE IF THE SPECIFIED FIELD EXISTS
C
            call cmo_get_name(cmo,ierror)
            len=icharlnf(cmsgin(3))
            iblknam=' '
            iblknam(1:len)=cmsgin(3)
            if(len.eq.3) then
              if(iblknam(1:3).eq.'itp') iblknam='itp1'
              if(iblknam(1:3).eq.'imt') iblknam='imt1'
              if(iblknam(1:3).eq.'icr') iblknam='icr1'
              if(iblknam(1:3).eq.'isn') iblknam='isn1'
              len=icharlnf(iblknam)
            endif
            call mmfindbk(iblknam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(iblknam,cmo,index,ctype,
     *           crank,clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(iblknam,cmo,ipiarray,lenout,icscode)
                  length=npoints
                  call mmgetblk('xfield',isubname,ipxfield,length,2,ics)
                  do i=1,npoints
                     xfield(i)=iarray(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(iblknam,cmo,ipxfield,lenout,icscode)
               endif
            else
               write(logmess,'(a,a)') 'Field does not exist: ',
     *                                iblknam(1:len)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9998
            endif
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
         length=nef*ntets
         call mmgetblk("itdone",isubname,ipitdone,length,1,
     *                 icscode)
         call mmgetblk("list",isubname,iplist,length,1,icscode)
         call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
         call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
         call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
         call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
         call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
         call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
         call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
         nadd=0
         call gradient(itopo,coption,
     *                 inclusive,
     *                 npoints,ntets,
     *                 ipxfield,interp,
     *                 xrefine,
     *                 mpno,ipmpary,
     *                 nadd,ipitadd,ipifadd,ipieadd)
         if(coption(1:coption_len).eq.'minsize' .or.
     *      coption(1:coption_len).eq.'lambdade') then
         else
            do it=1,ntets
               do i=1,nelmnef(itettyp(it))
                  itdone(jtetoff(it)+i)=0
               enddo
            enddo
            nadd1=0
            do i=1,nadd
               it=itadd(i)
               if=ifadd(i)
               if(nadd1.gt.0) then
                  if(jtet1(jtetoff(it)+if).gt.0 .and.
     *               jtet1(jtetoff(it)+if).lt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+if)-1)/nef
                     jf=jtet1(jtetoff(it)+if)-
     *                        nef*(jt-1)
                     if(itdone(jtetoff(jt)+jf).ne.0) goto 200
                  elseif(jtet1(jtetoff(it)+if).gt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+if)-mbndry-1) /
     *                     nef
                     jf=jtet1(jtetoff(it)+if)-
     *                     mbndry-
     *                     nef*(jt-1)
                     if(itdone(jtetoff(jt)+jf).ne.0) goto 200
                  endif
               endif
               xsum=0.0
               xface=0.0
               yface=0.0
               zface=0.0
               do j=1,ielmface0(if,itettyp(it))
                  i1=itet1(itetoff(it)+
     *                     ielmface1(j,if,itettyp(it)))
                  xsum=xsum+1.0
                  xface=xface+xic(i1)
                  yface=yface+yic(i1)
                  zface=zface+zic(i1)
               enddo
               xface=xface/xsum
               yface=yface/xsum
               zface=zface/xsum
               nadd1=nadd1+1
               itadd(nadd1)=it
               ifadd(nadd1)=if
               iadd(nadd1)=0
               xadd(nadd1)=xface
               yadd(nadd1)=yface
               zadd(nadd1)=zface
               itdone(jtetoff(it)+if)=nadd1
 200           continue
            enddo
            nadd=nadd1
            write(logmess,'(a,i10)') "refine faces: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_face_add(cmo,nadd,ipitadd,ipifadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
               call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *               ityp,ier)
               call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
               if(npoints_save.ne.npoints) then
                  ipointi=npoints_save+1
                  ipointj=npoints
                  call cmo_set_info('ipointi',cmo,
     &                            ipointi,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
                  call cmo_set_info('ipointj',cmo,
     &                            ipointj,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
 
               endif
            endif
         endif
         call mmrelblk("itdone",isubname,ipitdone,icscode)
         call mmrelblk("itadd",isubname,ipitadd,icscode)
         call mmrelblk("ifadd",isubname,ipifadd,icscode)
         call mmrelblk("list",isubname,iplist,icscode)
         call mmrelblk("iadd",isubname,ipiadd,icscode)
         call mmrelblk("xadd",isubname,ipxadd,icscode)
         call mmrelblk("yadd",isubname,ipyadd,icscode)
         call mmrelblk("zadd",isubname,ipzadd,icscode)
      elseif(itopo(1:len).eq.'edge') then
 
         write(logmess,'(a)') "refine edges: "
         call writloga('default',0,logmess,0,ierrw)
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           CHECK TO SEE IF THE SPECIFIED FIELD EXISTS
C
            call cmo_get_name(cmo,ierror)
            len=icharlnf(cmsgin(3))
            iblknam=' '
            iblknam(1:len)=cmsgin(3)
            if(len.eq.3) then
              if(iblknam(1:3).eq.'itp') iblknam='itp1'
              if(iblknam(1:3).eq.'imt') iblknam='imt1'
              if(iblknam(1:3).eq.'icr') iblknam='icr1'
              if(iblknam(1:3).eq.'isn') iblknam='isn1'
              len=icharlnf(iblknam)
            endif
            call mmfindbk(iblknam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(iblknam,cmo,index,ctype,
     *           crank,clen,cinter,cpers,cio,ierror_return)
 
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(iblknam,cmo,ipiarray,lenout,icscode)
                  length=npoints
                  call mmgetblk('xfield',isubname,ipxfield,length,2,ics)
                  do i=1,npoints
                     xfield(i)=iarray(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(iblknam,cmo,ipxfield,lenout,icscode)
               endif
            else
               write(logmess,'(a,a)') 'Field does not exist: ',
     *                                iblknam(1:len)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9998
            endif
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
 
         length=nee*ntets
         call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
         call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
         call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
 
         call mmgetblk("list",isubname,iplist,length,1,icscode)
         call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
 
         call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
         call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
         call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
         nadd=0
         call gradient(itopo,coption,
     *                 inclusive,
     *                 npoints,ntets,
     *                 ipxfield,interp,
     *                 xrefine,
     *                 mpno,ipmpary,
     *                 nadd,ipitadd,ipifadd,ipieadd)
         if(coption(1:coption_len).eq.'minsize' .or.
     *      coption(1:coption_len).eq.'lambdade') then
         else
 
            length=2*npoints
            call mmgetblk('nedge_bin',isubname,
     *                    ipnedge_bin,length,1,icscode)
            call mmgetblk('nedge_off',isubname,
     *                    ipnedge_off,length,1,icscode)
            do i=1,2*npoints
               nedge_bin(i)=0
               nedge_off(i)=0
            enddo
            do i=1,nadd
               it=itadd(i)
               ie=ieadd(i)
               i1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
               i2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
               isum12=iparent(i1)+iparent(i2)
               nedge_bin(isum12)=nedge_bin(isum12)+1
            enddo
            isum=0
            do i=1,2*npoints
               if(nedge_bin(i).gt.0) then
                  nedge_off(i)=isum
                  isum=isum+nedge_bin(i)
               endif
               nedge_bin(i)=0
            enddo
            length=isum+1
            call mmgetblk('xedge1',isubname,
     *                    ipxedge1,length,2,icscode)
            call mmgetblk('yedge1',isubname,
     *                    ipyedge1,length,2,icscode)
            call mmgetblk('zedge1',isubname,
     *                    ipzedge1,length,2,icscode)
            do i=1,length
               xedge1(i)=0.0
               yedge1(i)=0.0
               zedge1(i)=0.0
            enddo
            call get_epsilon('epsilonl',ref_distance)
            nadd1=0
            do i=1,nadd
               it=itadd(i)
               ie=ieadd(i)
               j1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
               j2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
               xedge=(xic(j1)+xic(j2))/2.0d+00
               yedge=(yic(j1)+yic(j2))/2.0d+00
               zedge=(zic(j1)+zic(j2))/2.0d+00
               isum12=iparent(j1)+iparent(j2)
               if(nedge_bin(isum12).gt.0) then
                  do jadd=nedge_off(isum12)+1,
     *                    nedge_off(isum12)+nedge_bin(isum12)
                     distance=(xedge-xedge1(jadd))**2 +
     *                        (yedge-yedge1(jadd))**2 +
     *                        (zedge-zedge1(jadd))**2
                     if(distance.lt.ref_distance) goto 300
                  enddo
               endif
               nedge_bin(isum12)=nedge_bin(isum12)+1
               xedge1(nedge_off(isum12)+nedge_bin(isum12))=xedge
               yedge1(nedge_off(isum12)+nedge_bin(isum12))=yedge
               zedge1(nedge_off(isum12)+nedge_bin(isum12))=zedge
               nadd1=nadd1+1
               itadd(nadd1)=it
               ieadd(nadd1)=ie
               iadd(nadd1)=0
               xadd(nadd1)=xedge
               yadd(nadd1)=yedge
               zadd(nadd1)=zedge
 300           continue
            enddo
            nadd=nadd1
            call mmrelblk('nedge_bin',isubname,ipnedge_bin,icscode)
            call mmrelblk('nedge_off',isubname,ipnedge_off,icscode)
            call mmrelblk('xedge1',isubname,ipxedge1,icscode)
            call mmrelblk('yedge1',isubname,ipyedge1,icscode)
            call mmrelblk('zedge1',isubname,ipzedge1,icscode)
            write(logmess,'(a,i10)') "Refine edges: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_edge_add(cmo,nadd,ipitadd,ipieadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
               call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *               ityp,ier)
               call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
               if(npoints_save.ne.npoints) then
                  ipointi=npoints_save+1
                  ipointj=npoints
                  call cmo_set_info('ipointi',cmo,
     &                            ipointi,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
                  call cmo_set_info('ipointj',cmo,
     &                            ipointj,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
               endif
            endif
         endif
         call mmrelblk("itadd",isubname,ipitadd,icscode)
         call mmrelblk("ieadd",isubname,ipieadd,icscode)
         call mmrelblk("list",isubname,iplist,icscode)
         call mmrelblk("iadd",isubname,ipiadd,icscode)
         call mmrelblk("xadd",isubname,ipxadd,icscode)
         call mmrelblk("yadd",isubname,ipyadd,icscode)
         call mmrelblk("zadd",isubname,ipzadd,icscode)
 
      elseif(itopo(1:len).eq.'faceedge') then
         write(logmess,'(a)') "refine face edges: "
         call writloga('default',0,logmess,0,ierrw)
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           CHECK TO SEE IF THE SPECIFIED FIELD EXISTS
C
            call cmo_get_name(cmo,ierror)
            len=icharlnf(cmsgin(3))
            iblknam=' '
            iblknam(1:len)=cmsgin(3)
            if(len.eq.3) then
              if(iblknam(1:3).eq.'itp') iblknam='itp1'
              if(iblknam(1:3).eq.'imt') iblknam='imt1'
              if(iblknam(1:3).eq.'icr') iblknam='icr1'
              if(iblknam(1:3).eq.'isn') iblknam='isn1'
              len=icharlnf(iblknam)
            endif
            call mmfindbk(iblknam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(iblknam,cmo,index,ctype,
     *           crank,clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(iblknam,cmo,ipiarray,lenout,icscode)
                  length=npoints
                  call mmgetblk('xfield',isubname,ipxfield,length,2,ics)
                  do i=1,npoints
                     xfield(i)=iarray(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(iblknam,cmo,ipxfield,lenout,icscode)
               endif
            else
               write(logmess,'(a,a)') 'Field does not exist: ',
     *                                iblknam(1:len)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9998
            endif
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
         length=12*ntets
         call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
         call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
         call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
         call mmgetblk("itadd2",isubname,ipitadd2,length,1,icscode)
         call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
         call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
         call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
         call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
         nadd=0
         call gradient(itopo,coption,
     *                 inclusive,
     *                 npoints,ntets,
     *                 ipxfield,interp,
     *                 xrefine,
     *                 mpno,ipmpary,
     *                 nadd,ipitadd2,ipifadd,ipieadd)
         if(coption(1:coption_len).eq.'minsize' .or.
     *      coption(1:coption_len).eq.'lambdade') then
         else
            call get_epsilon('epsilonl',ref_distance)
            nadd1=0
            do i=1,nadd
              it=itadd2(i)
              if=ifadd(i)
              do k=1,ielmface0(if,itettyp(it)) 
                 ie=ielmface2(k,if,itettyp(it))
                 if(ie.le.0) go to 301
                 j1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                 j2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                 xedge=(xic(j1)+xic(j2))/2.0d+00
                 yedge=(yic(j1)+yic(j2))/2.0d+00
                 zedge=(zic(j1)+zic(j2))/2.0d+00
                 if(nadd1.gt.0) then
                   do jadd=1,nadd1
                     distance=(xedge-xadd(jadd))**2 +
     *                        (yedge-yadd(jadd))**2 +
     *                        (zedge-zadd(jadd))**2
                     if(distance.lt.ref_distance) goto 301
                   enddo
                 endif
                 nadd1=nadd1+1
                 itadd(nadd1)=it
                 ieadd(nadd1)=ie
                 iadd(nadd1)=0
                 xadd(nadd1)=xedge
                 yadd(nadd1)=yedge
                 zadd(nadd1)=zedge
 301             continue
               enddo
            enddo
            nadd=nadd1
            write(logmess,'(a,i10)') "Refine edges: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_edge_add(cmo,nadd,ipitadd,ipieadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ier)
               call cmo_get_info('nelements',cmo,ntets,ilen,itype,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *               ityp,ier)
               call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ier)
               if(npoints_save.ne.npoints) then
                  ipointi=npoints_save+1
                  ipointj=npoints
                  call cmo_set_info('ipointi',cmo,
     &                            ipointi,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
                  call cmo_set_info('ipointj',cmo,
     &                            ipointj,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
 
               endif
            endif
         endif
         call mmrelblk("itadd",isubname,ipitadd,icscode)
         call mmrelblk("ieadd",isubname,ipieadd,icscode)
         call mmrelblk("iadd",isubname,ipiadd,icscode)
         call mmrelblk("xadd",isubname,ipxadd,icscode)
         call mmrelblk("yadd",isubname,ipyadd,icscode)
         call mmrelblk("zadd",isubname,ipzadd,icscode)
      elseif(itopo(1:len).eq.'tetedge') then
         write(logmess,'(a)') "refine tet edges: "
         call writloga('default',0,logmess,0,ierrw)
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           CHECK TO SEE IF THE SPECIFIED FIELD EXISTS
C
            call cmo_get_name(cmo,ierror)
            len=icharlnf(cmsgin(3))
            iblknam=' '
            iblknam(1:len)=cmsgin(3)
            if(len.eq.3) then
              if(iblknam(1:3).eq.'itp') iblknam='itp1'
              if(iblknam(1:3).eq.'imt') iblknam='imt1'
              if(iblknam(1:3).eq.'icr') iblknam='icr1'
              if(iblknam(1:3).eq.'isn') iblknam='isn1'
              len=icharlnf(iblknam)
            endif
            call mmfindbk(iblknam,cmo,ipout,lenout,icscode)
            if(icscode.eq.0) then
               call cmo_get_attparam(iblknam,cmo,index,ctype,
     *           crank,clen,cinter,cpers,cio,ierror_return)
               lenc=icharlnf(ctype)
               if(ctype(1:lenc).eq.'VINT') then
                  call mmfindbk(iblknam,cmo,ipiarray,lenout,icscode)
                  length=npoints
                  call mmgetblk('xfield',isubname,ipxfield,length,2,ics)
                  do i=1,npoints
                     xfield(i)=iarray(i)
                  enddo
               elseif(ctype(1:lenc).eq.'VDOUBLE') then
                  call mmfindbk(iblknam,cmo,ipxfield,lenout,icscode)
               endif
            else
               write(logmess,'(a,a)') 'Field does not exist: ',
     *                                iblknam(1:len)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9998
            endif
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C
         length=nee*ntets
         call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
         call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
         call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
         call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
         call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
         call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
         call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
         nadd=0
c
c note we use space for faces (ifadd) to store element
c numbers -  itadd will be filled later
c
         call gradient(itopo,coption,
     *                 inclusive,
     *                 npoints,ntets,
     *                 ipxfield,interp,
     *                 xrefine,
     *                 mpno,ipmpary,
     *                 nadd,ipifadd,ipitadd,ipieadd)
         if(coption(1:coption_len).eq.'minsize' .or.
     *      coption(1:coption_len).eq.'rmelements') then
         else
            call get_epsilon('epsilonl',ref_distance)
            nadd1=0
            do i=1,nadd
              it=ifadd(i)
              do j=1,nelmnee(itettyp(it))
                ie=j
                j1=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                j2=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                xedge=(xic(j1)+xic(j2))/2.0d+00
                yedge=(yic(j1)+yic(j2))/2.0d+00
                zedge=(zic(j1)+zic(j2))/2.0d+00
                if(nadd1.gt.0) then
                  do jadd=1,nadd1
                     distance=(xedge-xadd(jadd))**2 +
     *                        (yedge-yadd(jadd))**2 +
     *                        (zedge-zadd(jadd))**2
                     if(distance.lt.ref_distance) goto 302
                  enddo
                endif
                nadd1=nadd1+1
                itadd(nadd1)=it
                ieadd(nadd1)=ie
                iadd(nadd1)=0
                xadd(nadd1)=xedge
                yadd(nadd1)=yedge
                zadd(nadd1)=zedge
 302           continue
              enddo
            enddo
            nadd=nadd1
            write(logmess,'(a,i10)') "Refine edges: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_edge_add(cmo,nadd,ipitadd,ipieadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
               call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *               ityp,ier)
               call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
               if(npoints_save.ne.npoints) then
                  ipointi=npoints_save+1
                  ipointj=npoints
                  call cmo_set_info('ipointi',cmo,
     &                            ipointi,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
                  call cmo_set_info('ipointj',cmo,
     &                            ipointj,1,1,icscode)
                  if (icscode .ne. 0)
     &               call x3d_error(isubname,'get_info_i')
 
               endif
            endif
         endif
         call mmrelblk("itadd",isubname,ipitadd,icscode)
         call mmrelblk("ieadd",isubname,ipieadd,icscode)
         call mmrelblk("iadd",isubname,ipiadd,icscode)
         call mmrelblk("xadd",isubname,ipxadd,icscode)
         call mmrelblk("yadd",isubname,ipyadd,icscode)
         call mmrelblk("zadd",isubname,ipzadd,icscode)
      endif
C
 9998 continue
C
 
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
      call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
      call cmo_set_info('nelements',cmo,ntets,1,1,ier)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
      call cmo_newlen(cmo,ier)
      call mmrelprt(isubname,icscode)
C
      goto 9999
 9999 continue
C
      return
      end
c
c***********************************************************************
      subroutine refine_add_pts(cmo,ctopo,
     *                       naddpts,ipiaddpts,
     *                       ipxaddpts,ipyaddpts,ipzaddpts)
C
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      include "chydro.h"
      include "local_element.h"
C
C ######################################################################
C
      character*(*) cmo, ctopo
      integer naddpts
C
      pointer (ipiaddpts, iaddpts)
      integer iaddpts(naddpts)
      pointer (ipxaddpts, xaddpts)
      pointer (ipyaddpts, yaddpts)
      pointer (ipzaddpts, zaddpts)
      real*8 xaddpts(naddpts), yaddpts(naddpts), zaddpts(naddpts)
C
      pointer (ipitp1, itp1)
      integer itp1(*)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(4*1000000), jtet1(4*1000000)
C
      pointer (ipitadd, itadd)
      integer itadd(*)
      pointer (ipifadd, ifadd)
      integer ifadd(*)
      pointer (ipieadd, ieadd)
      integer ieadd(*)
C
      pointer (ipiadd, iadd)
      integer iadd(*)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(*), yadd(*), zadd(*)
C
      pointer (ipiadd1, iadd1)
      integer iadd1(*)
      pointer (ipxadd1, xadd1)
      pointer (ipyadd1, yadd1)
      pointer (ipzadd1, zadd1)
      real*8 xadd1(*), yadd1(*), zadd1(*)
C
      pointer (ipiadd2, iadd2)
      integer iadd2(*)
      pointer (ipxadd2, xadd2)
      pointer (ipyadd2, yadd2)
      pointer (ipzadd2, zadd2)
      real*8 xadd2(*), yadd2(*), zadd2(*)
C
      pointer (ipitcheck, itcheck)
      integer itcheck(*)
C
      integer ipointi,ipointj,icscode,ilen,itype,ics,length,
     * npoints_start,j3p,j2p,je,i3p,i2p,ie,iedge,k,
     * l2,l1,k2,k1,in_face,j3,j2,j1,iface,nen,nef,nee,ierror,
     *  npoints,ityp,npoints_save,nsdgeom,j,icmotype
      integer i,naddtotal,nadd2,naddelm,irecon,nsdtopo,nnfreq,
     * ier,ierr2,naddelm1,it,in_element,idist,idup,
     *  i1,i2,i3,i4,i5,i6,i7,i8,ntets,itcount,ierrw,
     *  jt,jf,mbndry

      real*8 xa,ya,za,dist,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *  x4,y4,z4,x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *  dsa1,dsa2,ds12
      character*32 isubname
 
C
      data irecon / 1 /
C
      isubname='refine_add_pts'
 
C
      call  cmo_get_info('ipointi',cmo,ipointi,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call  cmo_get_info('ipointj',cmo,ipointj,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      npoints_start=ipointj
C
C     old code used nnfreq as follows:
C     nnfreq=-1 - do recon, nnfreq=-2 -do special recon
C     nnfreq=0 - do not do recon, else do recon nnfreq times
      call cmo_get_info('nnfreq',cmo,
     *   nnfreq,ilen,itype,icscode)
      if (nnfreq.eq.0) irecon=0
C
      call cmo_get_info('ndimensions_geom',cmo,nsdgeom,ilen,itype,ics)
      call cmo_get_info('ndimensions_topo',cmo,nsdtopo,ilen,itype,ics)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('edges_per_element',cmo,nee,ilen,itype,icscode)
      call cmo_get_intinfo('idebug',cmo,idebug,ilen,ityp,icscode)
C
      length=naddpts
      call mmgetblk("iadd1",isubname,ipiadd1,length,1,icscode)
      call mmgetblk("xadd1",isubname,ipxadd1,length,2,icscode)
      call mmgetblk("yadd1",isubname,ipyadd1,length,2,icscode)
      call mmgetblk("zadd1",isubname,ipzadd1,length,2,icscode)
      call mmgetblk("iadd2",isubname,ipiadd2,length,1,icscode)
      call mmgetblk("xadd2",isubname,ipxadd2,length,2,icscode)
      call mmgetblk("yadd2",isubname,ipyadd2,length,2,icscode)
      call mmgetblk("zadd2",isubname,ipzadd2,length,2,icscode)
      call mmgetblk("iadd",isubname,ipiadd,length,1,icscode)
      call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
      call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
      call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
      call mmgetblk("itadd",isubname,ipitadd,length,1,icscode)
      call mmgetblk("ifadd",isubname,ipifadd,length,1,icscode)
      call mmgetblk("ieadd",isubname,ipieadd,length,1,icscode)
C
      do i=1,naddpts
         itadd(i)=0
         iadd(i)=iaddpts(i)
         xadd(i)=xaddpts(i)
         yadd(i)=yaddpts(i)
         zadd(i)=zaddpts(i)
      enddo
C
      naddtotal=0
C
      nadd2=0
      naddelm=naddpts
C
 100  continue
C
      dowhile((ctopo(1:3).eq.'tet' .or.
     *         ctopo(1:7).eq.'element'.or.
     *         ctopo(1:5).eq.'-all-') .and.
     *         naddelm.gt.0)
C
         if(irecon.eq.1) then
            if(nsdtopo.eq.2 .and.
     *         nen.eq.nelmnen(ifelmtri) .and.
     *         nef.eq.nelmnef(ifelmtri)) then
               call dotaskx3d('recon/0 ; finish',ierror)
            elseif(nsdtopo.eq.3 .and.
     *             nen.eq.nelmnen(ifelmtet) .and.
     *             nef.eq.nelmnef(ifelmtet)) then
               call dotaskx3d('recon ; finish',ierror)
            endif
         else
            write(logmess,'(a)') 'REFINE NOT USING RECON.'
            call writloga('default',0,logmess,0,ierrw)
         endif
C
         call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ier)
         call cmo_get_info('xic',cmo,ipxic,ilen,itype,ier)
         call cmo_get_info('yic',cmo,ipyic,ilen,itype,ier)
         call cmo_get_info('zic',cmo,ipzic,ilen,itype,ier)
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,ilen,itype,ier)
         call cmo_get_info('itettyp',cmo,
     *                     ipitettyp,ilen,itype,ier)
         call cmo_get_info('itetoff',cmo,
     *                     ipitetoff,ilen,itype,ier)
         call cmo_get_info('jtetoff',cmo,
     *                     ipjtetoff,ilen,itype,ier)
         call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierror)
C
         call table_element(cmo,
     &                      ipxadd,ipyadd,ipzadd,naddelm,
     &                      ipitadd,
     &                      ierr2)
         if(idebug .ge. 4)then
           do i = 1, naddelm
            write(logmess,'(a,i8,i8)')
     *      'Vol. Candidate Elements', i, itadd(i)
            call writloga('default',0,logmess,0,ier)
           enddo
         endif
         naddelm1=0
         do i=1,naddelm
            if(itadd(i).gt.0) then
               it=itadd(i)
               xa=xadd(i)
               ya=yadd(i)
               za=zadd(i)
               in_element=-1
               idist=0
               do j=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+j)
                  dist=(xic(i1)-xa)**2+(yic(i1)-ya)**2+(zic(i1)-za)**2
                  if(dist.lt.1.0e-10) then
                     idup=i1
                     idist=idist+1
                  endif
               enddo
               if(idist.gt.0) then
               elseif(itettyp(it).eq.ifelmtri) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                              xa,ya,za,
     *                              in_element)
               elseif(itettyp(it).eq.ifelmqud) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_quad2d(x1,y1,z1,x2,y2,z2,
     *                               x3,y3,z3,x4,y4,z4,
     *                               xa,ya,za,
     *                               in_element)
               elseif(itettyp(it).eq.ifelmtet) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            xa,ya,za,
     *                            in_element)
               elseif(itettyp(it).eq.ifelmhex) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  i5=itet1(itetoff(it)+5)
                  i6=itet1(itetoff(it)+6)
                  i7=itet1(itetoff(it)+7)
                  i8=itet1(itetoff(it)+8)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  x5=xic(i5)
                  y5=yic(i5)
                  z5=zic(i5)
                  x6=xic(i6)
                  y6=yic(i6)
                  z6=zic(i6)
                  x7=xic(i7)
                  y7=yic(i7)
                  z7=zic(i7)
                  x8=xic(i8)
                  y8=yic(i8)
                  z8=zic(i8)
                  call inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                            xa,ya,za,
     *                            in_element)
               endif
               if(idist.gt.0) then
                  write(logmess,'(a,4i8)') 'Throw out duplicate point',
     *                    i,idist,iadd(i),idup
            call writloga('default',0,logmess,0,ier)
                  if(iabs(iadd(i)).gt.0) then
                     itp1(iabs(iadd(i)))=ifitpdud
                  endif
               elseif(in_element.eq.0) then
                  naddelm1=naddelm1+1
                  itadd(naddelm1)=itadd(i)
                  iadd(naddelm1)=iadd(i)
                  xadd(naddelm1)=xadd(i)
                  yadd(naddelm1)=yadd(i)
                  zadd(naddelm1)=zadd(i)
               else
                  nadd2=nadd2+1
                  iadd2(nadd2)=iadd(i)
                  xadd2(nadd2)=xadd(i)
                  yadd2(nadd2)=yadd(i)
                  zadd2(nadd2)=zadd(i)
               endif
            else
               if(iabs(iadd(i)).gt.0) then
                  itp1(iabs(iadd(i)))=ifitpdud
               endif
            endif
         enddo
C
         naddelm=naddelm1
C
         if(naddelm.gt.0) then
C
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            length=ntets
            call mmgetblk("itcheck",isubname,ipitcheck,length,1,icscode)
C
            do it=1,ntets
               itcheck(it)=0
            enddo
            naddelm1=0
            itcount=0
            do i=1,naddelm
               it=itadd(i)
               if(itcheck(it).eq.0) then
                  itcheck(it)=i
                  itcount=itcount+1
                  itadd(itcount)=itadd(i)
                  iadd(itcount)=iadd(i)
                  xadd(itcount)=xadd(i)
                  yadd(itcount)=yadd(i)
                  zadd(itcount)=zadd(i)
               else
                  naddelm1=naddelm1+1
                  iadd1(naddelm1)=iadd(i)
                  xadd1(naddelm1)=xadd(i)
                  yadd1(naddelm1)=yadd(i)
                  zadd1(naddelm1)=zadd(i)
               endif
            enddo
C
            naddelm=itcount
C
            naddtotal=naddtotal+naddelm
            npoints_save=npoints
            if(idebug .ge. 3)then
            write(logmess,'(a,i9)')
     *        'Refine addpts Volume:#candidates = ',naddelm
            call writloga('default',0,logmess,0,ierrw)
            endif
            call refine_tet_add(cmo,
     *                          naddelm,
     *                          ipitadd,
     *                          iadd,xadd,yadd,zadd)
C*****            call refine_element_add(cmo,
C*****     *                              naddelm,
C*****     *                              ipitadd,
C*****     *                              ipiadd,ipxadd,ipyadd,ipzadd)
C
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ier)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *            ityp,ier)
            call cmo_get_info('itettyp',cmo,ipitettyp,ilen,
     *            ityp,ier)
            call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,
     *            ityp,ier)
            call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
            if(npoints_save.ne.npoints) then
               ipointi=npoints_save+1
               ipointj=npoints
               call cmo_set_info('ipointi',cmo,
     &                         ipointi,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
               call cmo_set_info('ipointj',cmo,
     &                         ipointj,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
            endif
C
            do it=1,ntets
               do i=1,nelmnef(itettyp(it))
                  if(jtet1(jtetoff(it)+i).ge.mbndry) then
                  else
                    jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                    jf=jtet1(jtetoff(it)+i)-nef*(jt-1)
                    if(itetclr(it).ne.itetclr(jt)) then
                       jtet1(jtetoff(it)+i)=mbndry+jtet1(jtetoff(it)+i)
                       jtet1(jtetoff(jt)+jf)=mbndry+
     *                                  nef*(it-1)+i
                    endif
                  endif
               enddo
            enddo
C
            naddelm=0
            do i=1,naddelm1
               naddelm=naddelm+1
               iadd(naddelm)=iadd1(i)
               xadd(naddelm)=xadd1(i)
               yadd(naddelm)=yadd1(i)
               zadd(naddelm)=zadd1(i)
            enddo
            do i=1,nadd2
               naddelm=naddelm+1
               iadd(naddelm)=iadd2(i)
               xadd(naddelm)=xadd2(i)
               yadd(naddelm)=yadd2(i)
               zadd(naddelm)=zadd2(i)
            enddo
            nadd2=0
C
            call mmrelblk("itcheck",isubname,ipitcheck,icscode)
C
         endif
C
      enddo
C
      if(nadd2.gt.0) then
         naddelm=nadd2
         do i=1,nadd2
            iadd(i)=iadd2(i)
            xadd(i)=xadd2(i)
            yadd(i)=yadd2(i)
            zadd(i)=zadd2(i)
         enddo
      endif
C
      nadd2=0
C
      dowhile(((ctopo(1:4).eq.'face'.or.ctopo(1:5).eq.'-all-') .or.
     *         (ctopo(1:4).eq.'edge'.and.nsdtopo.eq.2))
     *        .and.naddelm.gt.0)
 
         if(irecon.eq.1) then
            if(nsdtopo.eq.2 .and.
     *         nen.eq.nelmnen(ifelmtri) .and.
     *         nef.eq.nelmnef(ifelmtri)) then
               call dotaskx3d('recon/0 ; finish',ierror)
            elseif(nsdtopo.eq.3 .and.
     *             nen.eq.nelmnen(ifelmtet) .and.
     *             nef.eq.nelmnef(ifelmtet)) then
               call dotaskx3d('recon ; finish',ierror)
            endif
         else
            write(logmess,'(a)') 'REFINE NOT USING RECON.'
            call writloga('default',0,logmess,0,ier)
         endif
C
         call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
         call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
         call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,ilen,icmotype,ier)
         call cmo_get_info('itettyp',cmo,
     *                     ipitettyp,ilen,icmotype,ier)
         call cmo_get_info('itetoff',cmo,
     *                     ipitetoff,ilen,icmotype,ier)
         call cmo_get_info('jtetoff',cmo,
     *                     ipjtetoff,ilen,icmotype,ier)
         call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
         call table_element(cmo,
     &                      ipxadd,ipyadd,ipzadd,naddelm,
     &                      ipitadd,
     &                      ierr2)
         if(idebug .ge. 4)then
           do i = 1, naddelm
            write(logmess,'(a,i8,i8)')
     *      'Face Candidate Elements', i, itadd(i)
            call writloga('default',0,logmess,0,ier)
           enddo
         endif
         naddelm1=0
         do i=1,naddelm
            if(itadd(i).gt.0) then
               it=itadd(i)
               xa=xadd(i)
               ya=yadd(i)
               za=zadd(i)
               if(itettyp(it).eq.ifelmtri) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                              xa,ya,za,
     *                              in_element)
               elseif(itettyp(it).eq.ifelmqud) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_quad2d(x1,y1,z1,x2,y2,z2,
     *                               x3,y3,z3,x4,y4,z4,
     *                               xa,ya,za,
     *                               in_element)
               elseif(itettyp(it).eq.ifelmtet) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            xa,ya,za,
     *                            in_element)
               elseif(itettyp(it).eq.ifelmhex) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  i5=itet1(itetoff(it)+5)
                  i6=itet1(itetoff(it)+6)
                  i7=itet1(itetoff(it)+7)
                  i8=itet1(itetoff(it)+8)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  x5=xic(i5)
                  y5=yic(i5)
                  z5=zic(i5)
                  x6=xic(i6)
                  y6=yic(i6)
                  z6=zic(i6)
                  x7=xic(i7)
                  y7=yic(i7)
                  z7=zic(i7)
                  x8=xic(i8)
                  y8=yic(i8)
                  z8=zic(i8)
                  call inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                            xa,ya,za,
     *                            in_element)
               endif
               if(in_element.gt.0) then
                  if(itettyp(it).eq.ifelmtri) then
                     iface=in_element
                     j1=itet1(itetoff(it)+
     *                        ielmface1(1,iface,itettyp(it)))
                     j2=itet1(itetoff(it)+
     *                        ielmface1(2,iface,itettyp(it)))
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)
                     y2=yic(j2)
                     z2=zic(j2)
                     xa=xadd(i)
                     ya=yadd(i)
                     za=zadd(i)
                     ds12=(x2-x1)**2+(y2-y1)**2+(z2-z1)**2
                     dsa2=(x2-xa)**2+(y2-ya)**2+(z2-za)**2
                     dsa1=(xa-x1)**2+(ya-y1)**2+(za-z1)**2
                     if(dsa2.lt.1.0e-06*ds12 .or.
     *                  dsa1.lt.1.0d-06*ds12) then
                        nadd2=nadd2+1
                        iadd2(nadd2)=iadd(i)
                        xadd2(nadd2)=xadd(i)
                        yadd2(nadd2)=yadd(i)
                        zadd2(nadd2)=zadd(i)
                     else
                        naddelm1=naddelm1+1
                        itadd(naddelm1)=itadd(i)
                        ifadd(naddelm1)=in_element
                        iadd(naddelm1)=iadd(i)
                        xadd(naddelm1)=xadd(i)
                        yadd(naddelm1)=yadd(i)
                        zadd(naddelm1)=zadd(i)
                     endif
                  elseif(itettyp(it).eq.ifelmqud) then
                     nadd2=nadd2+1
                     iadd2(nadd2)=iadd(i)
                     xadd2(nadd2)=xadd(i)
                     yadd2(nadd2)=yadd(i)
                     zadd2(nadd2)=zadd(i)
                  elseif(itettyp(it).eq.ifelmtet) then
                     iface=in_element
                     j1=itet1(itetoff(it)+
     *                        ielmface1(1,iface,itettyp(it)))
                     j2=itet1(itetoff(it)+
     *                        ielmface1(2,iface,itettyp(it)))
                     j3=itet1(itetoff(it)+
     *                        ielmface1(3,iface,itettyp(it)))
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)
                     y2=yic(j2)
                     z2=zic(j2)
                     x3=xic(j3)
                     y3=yic(j3)
                     z3=zic(j3)
                     xa=xadd(i)
                     ya=yadd(i)
                     za=zadd(i)
                     call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                                 xa,ya,za,
     *                                 in_face)
                     if(in_face.eq.0) then
                        naddelm1=naddelm1+1
                        itadd(naddelm1)=itadd(i)
                        ifadd(naddelm1)=in_element
                        iadd(naddelm1)=iadd(i)
                        xadd(naddelm1)=xadd(i)
                        yadd(naddelm1)=yadd(i)
                        zadd(naddelm1)=zadd(i)
                     else
                        nadd2=nadd2+1
                        iadd2(nadd2)=iadd(i)
                        xadd2(nadd2)=xadd(i)
                        yadd2(nadd2)=yadd(i)
                        zadd2(nadd2)=zadd(i)
                     endif
                  elseif(itettyp(it).eq.ifelmhex) then
                     nadd2=nadd2+1
                     iadd2(nadd2)=iadd(i)
                     xadd2(nadd2)=xadd(i)
                     yadd2(nadd2)=yadd(i)
                     zadd2(nadd2)=zadd(i)
                  endif
               endif
            else
               if(iabs(iadd(i)).gt.0) then
                  itp1(iabs(iadd(i)))=ifitpdud
               endif
            endif

            if(idebug .ge. 4)then
            write(logmess,'(a,i8,a,i8)')
     *      'Face Search:in_element =',in_element,' in_face=',in_face
            call writloga('default',0,logmess,0,ier)
            endif
         enddo
C
         naddelm=naddelm1
C
         if(idebug .ge. 4)then
            write(logmess,'(a,i8)')'Face Search:naddelm =',naddelm
            call writloga('default',0,logmess,0,ier)
         endif
         if(naddelm.gt.0) then
C
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            length=ntets
            call mmgetblk("itcheck",isubname,ipitcheck,length,1,icscode)
C
            do it=1,ntets
               itcheck(it)=0
            enddo
            naddelm1=0
            itcount=0
            do i=1,naddelm
               it=itadd(i)
               if(itcheck(it).eq.0) then
                  itcheck(it)=i
                  itcount=itcount+1
                  itadd(itcount)=itadd(i)
                  ifadd(itcount)=ifadd(i)
                  iadd(itcount)=iadd(i)
                  xadd(itcount)=xadd(i)
                  yadd(itcount)=yadd(i)
                  zadd(itcount)=zadd(i)
               else
                  naddelm1=naddelm1+1
                  iadd1(naddelm1)=iadd(i)
                  xadd1(naddelm1)=xadd(i)
                  yadd1(naddelm1)=yadd(i)
                  zadd1(naddelm1)=zadd(i)
               endif
            enddo
C
            if(idebug .ge. 4)then
            write(logmess,'(a,i8,a,i8)')
     *      'Face Search:itcount =',itcount,' naddelm1=',naddelm1
            call writloga('default',0,logmess,0,ier)
            endif
C
            naddelm=itcount
C
            naddtotal=naddtotal+naddelm
            npoints_save=npoints
            if(idebug .ge. 3)then
            write(logmess,'(a,i9)')
     *        'Refine addpts Face:#candidates = ',naddelm
            call writloga('default',0,logmess,0,ierrw)
            endif
            call refine_face_add(cmo,
     *                           naddelm,
     *                           ipitadd,ipifadd,
     *                           ipiadd,ipxadd,ipyadd,ipzadd)
C
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ier)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *            ityp,ier)
            call cmo_get_info('itettyp',cmo,ipitettyp,ilen,
     *            ityp,ier)
            call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,
     *            ityp,ier)
            call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
            if(npoints_save.ne.npoints) then
               ipointi=npoints_save+1
               ipointj=npoints
               call cmo_set_info('ipointi',cmo,
     &                         ipointi,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
               call cmo_set_info('ipointj',cmo,
     &                         ipointj,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
            endif
            do it=1,ntets
               do i=1,nelmnef(itettyp(it))
                  if(jtet1(jtetoff(it)+i).ge.mbndry) then
                  else
                    jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                    jf=jtet1(jtetoff(it)+i)-nef*(jt-1)
                    if(itetclr(it).ne.itetclr(jt)) then
                       jtet1(jtetoff(it)+i)=mbndry+jtet1(jtetoff(it)+i)
                       jtet1(jtetoff(jt)+jf)=mbndry+
     *                                  nef*(it-1)+i
                    endif
                  endif
               enddo
            enddo
C
            naddelm=0
            do i=1,naddelm1
               naddelm=naddelm+1
               iadd(naddelm)=iadd1(i)
               xadd(naddelm)=xadd1(i)
               yadd(naddelm)=yadd1(i)
               zadd(naddelm)=zadd1(i)
            enddo
            do i=1,nadd2
               naddelm=naddelm+1
               iadd(naddelm)=iadd2(i)
               xadd(naddelm)=xadd2(i)
               yadd(naddelm)=yadd2(i)
               zadd(naddelm)=zadd2(i)
            enddo
            nadd2=0
C
            call mmrelblk("itcheck",isubname,ipitcheck,icscode)
            goto 100
C
         endif
C
      enddo
C
      if(nadd2.gt.0) then
         naddelm=nadd2
         do i=1,nadd2
            iadd(i)=iadd2(i)
            xadd(i)=xadd2(i)
            yadd(i)=yadd2(i)
            zadd(i)=zadd2(i)
         enddo
      endif
C
      nadd2=0
C
      dowhile((ctopo(1:4).eq.'edge'.or.ctopo(1:5).eq.'-all-') .and.
     *        naddelm.gt.0)
 
         if(irecon.eq.1) then
            if(nsdtopo.eq.2 .and.
     *         nen.eq.nelmnen(ifelmtri) .and.
     *         nef.eq.nelmnef(ifelmtri)) then
               call dotaskx3d('recon/0 ; finish',ierror)
            elseif(nsdtopo.eq.3 .and.
     *             nen.eq.nelmnen(ifelmtet) .and.
     *             nef.eq.nelmnef(ifelmtet)) then
               call dotaskx3d('recon ; finish',ierror)
            endif
         else
            write(logmess,'(a)') 'REFINE NOT USING RECON.'
            call writloga('default',0,logmess,0,ier)
         endif
C
         call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
         call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
         call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,ilen,icmotype,ier)
         call cmo_get_info('itettyp',cmo,
     *                     ipitettyp,ilen,icmotype,ier)
         call cmo_get_info('itetoff',cmo,
     *                     ipitetoff,ilen,icmotype,ier)
         call cmo_get_info('jtetoff',cmo,
     *                     ipjtetoff,ilen,icmotype,ier)
         call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
         call table_element(cmo,
     &                      ipxadd,ipyadd,ipzadd,naddelm,
     &                      ipitadd,
     &                      ierr2)
         if(idebug .ge. 4)then
           do i = 1, naddelm
            write(logmess,'(a,i8,i8)')
     *      'Edge Candidate Elements', i, itadd(i)
            call writloga('default',0,logmess,0,ier)
           enddo
         endif
         naddelm1=0
         do i=1,naddelm
            if(itadd(i).gt.0) then
               it=itadd(i)
               xa=xadd(i)
               ya=yadd(i)
               za=zadd(i)
               if(itettyp(it).eq.ifelmtri) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                              xa,ya,za,
     *                              in_element)
               elseif(itettyp(it).eq.ifelmqud) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_quad2d(x1,y1,z1,x2,y2,z2,
     *                               x3,y3,z3,x4,y4,z4,
     *                               xa,ya,za,
     *                               in_element)
               elseif(itettyp(it).eq.ifelmtet) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  call inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            xa,ya,za,
     *                            in_element)
               elseif(itettyp(it).eq.ifelmhex) then
                  i1=itet1(itetoff(it)+1)
                  i2=itet1(itetoff(it)+2)
                  i3=itet1(itetoff(it)+3)
                  i4=itet1(itetoff(it)+4)
                  i5=itet1(itetoff(it)+5)
                  i6=itet1(itetoff(it)+6)
                  i7=itet1(itetoff(it)+7)
                  i8=itet1(itetoff(it)+8)
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  x5=xic(i5)
                  y5=yic(i5)
                  z5=zic(i5)
                  x6=xic(i6)
                  y6=yic(i6)
                  z6=zic(i6)
                  x7=xic(i7)
                  y7=yic(i7)
                  z7=zic(i7)
                  x8=xic(i8)
                  y8=yic(i8)
                  z8=zic(i8)
                  call inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                            x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                            xa,ya,za,
     *                            in_element)
               endif
               if(in_element.gt.0) then
                  if(itettyp(it).eq.ifelmtri) then
                  elseif(itettyp(it).eq.ifelmtet) then
                     iface=in_element
                     j1=itet1(itetoff(it)+
     *                        ielmface1(1,iface,itettyp(it)))
                     j2=itet1(itetoff(it)+
     *                        ielmface1(2,iface,itettyp(it)))
                     j3=itet1(itetoff(it)+
     *                        ielmface1(3,iface,itettyp(it)))
                     x1=xic(j1)
                     y1=yic(j1)
                     z1=zic(j1)
                     x2=xic(j2)
                     y2=yic(j2)
                     z2=zic(j2)
                     x3=xic(j3)
                     y3=yic(j3)
                     z3=zic(j3)
                     xa=xadd(i)
                     ya=yadd(i)
                     za=zadd(i)
                     call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                                 xa,ya,za,
     *                                 in_face)
                     if(in_face.gt.0) then
                        if(in_face.eq.1) then
                           k1=j2
                           k2=j3
                        elseif(in_face.eq.2) then
                           k1=j3
                           k2=j1
                        elseif(in_face.eq.3) then
                           k1=j1
                           k2=j2
                        endif
                        do k=1,6
                           l1=itet1(itetoff(it)+
     *                              ielmedge1(1,k,itettyp(it)))
                           l2=itet1(itetoff(it)+
     *                              ielmedge1(2,k,itettyp(it)))
                           if((k1.eq.l1.and.k2.eq.l2) .or.
     *                        (k2.eq.l1.and.k1.eq.l2)) then
                              iedge=k
                           endif
                        enddo
                        naddelm1=naddelm1+1
                        itadd(naddelm1)=itadd(i)
                        ieadd(naddelm1)=iedge
                        iadd(naddelm1)=iadd(i)
                        xadd(naddelm1)=xadd(i)
                        yadd(naddelm1)=yadd(i)
                        zadd(naddelm1)=zadd(i)
                     else
                        nadd2=nadd2+1
                        iadd2(nadd2)=iadd(i)
                        xadd2(nadd2)=xadd(i)
                        yadd2(nadd2)=yadd(i)
                        zadd2(nadd2)=zadd(i)
                     endif
                  endif
               endif
            else
               if(iabs(iadd(i)).gt.0) then
                  itp1(iabs(iadd(i)))=ifitpdud
               endif
            endif
         enddo
C
         naddelm=naddelm1
C
         if(naddelm.gt.0) then
C
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            length=ntets
            call mmgetblk("itcheck",isubname,ipitcheck,length,1,icscode)
C
            do it=1,ntets
               itcheck(it)=0
            enddo
            naddelm1=0
            itcount=0
            do i=1,naddelm
               it=itadd(i)
               if(itcheck(it).eq.0) then
                  itcheck(it)=i
                  itcount=itcount+1
                  itadd(itcount)=itadd(i)
                  ieadd(itcount)=ieadd(i)
                  iadd(itcount)=iadd(i)
                  xadd(itcount)=xadd(i)
                  yadd(itcount)=yadd(i)
                  zadd(itcount)=zadd(i)
               else
                  naddelm1=naddelm1+1
                  iadd1(naddelm1)=iadd(i)
                  xadd1(naddelm1)=xadd(i)
                  yadd1(naddelm1)=yadd(i)
                  zadd1(naddelm1)=zadd(i)
               endif
            enddo
C
         if(idebug .ge. 4)then
            write(logmess,'(a,i8,a,i8)')
     *      'Face Search:itcount =',itcount,' naddelm1=',naddelm1
            call writloga('default',0,logmess,0,ier)
         endif

            naddelm=itcount
C
            itcount=0
            do i=1,naddelm
               it=itadd(i)
               if(it.gt.0) then
                  ie=ieadd(i)
                  i2p=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
                  i3p=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
                  do j=i+1,naddelm
                     jt=itadd(j)
                     if(jt.gt.0) then
                        je=ieadd(j)
                        j2p=itet1(itetoff(jt)+
     *                            ielmedge1(1,je,itettyp(jt)))
                        j3p=itet1(itetoff(jt)+
     *                            ielmedge1(2,je,itettyp(jt)))
                        if((j2p.eq.i2p.and.j3p.eq.i3p) .or.
     *                     (j3p.eq.i2p.and.j2p.eq.i3p)) then
                           naddelm1=naddelm1+1
                           iadd1(naddelm1)=iadd(j)
                           xadd1(naddelm1)=xadd(j)
                           yadd1(naddelm1)=yadd(j)
                           zadd1(naddelm1)=zadd(j)
                           itadd(j)=0
                           ieadd(j)=0
                        endif
                     endif
                  enddo
                  itcount=itcount+1
                  itadd(itcount)=itadd(i)
                  ieadd(itcount)=ieadd(i)
                  iadd(itcount)=iadd(i)
                  xadd(itcount)=xadd(i)
                  yadd(itcount)=yadd(i)
                  zadd(itcount)=zadd(i)
               endif
            enddo
C
            naddelm=itcount
C
            naddtotal=naddtotal+naddelm
            npoints_save=npoints
            if(idebug .ge. 3)then
            write(logmess,'(a,i9)')
     *        'Refine addpts Edge:#candidates = ',naddelm
            call writloga('default',0,logmess,0,ier)
            endif
            call refine_edge_add(cmo,
     *                           naddelm,
     *                           ipitadd,ipieadd,
     *                           ipiadd,ipxadd,ipyadd,ipzadd)
C
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
            call cmo_get_info('nelements',cmo,ntets,ilen,ityp,ier)
            call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,ier)
            call cmo_get_info('itetclr',cmo,ipitetclr,ilen,
     *            ityp,ier)
            call cmo_get_info('itettyp',cmo,ipitettyp,ilen,
     *            ityp,ier)
            call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,
     *            ityp,ier)
            call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ier)
            if(npoints_save.ne.npoints) then
               ipointi=npoints_save+1
               ipointj=npoints
               call cmo_set_info('ipointi',cmo,
     &                         ipointi,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
               call cmo_set_info('ipointj',cmo,
     &                         ipointj,1,1,icscode)
               if (icscode .ne. 0)
     &            call x3d_error(isubname,'get_info_i')
            endif
            do it=1,ntets
               do i=1,nelmnef(itettyp(it))
                  if(jtet1(jtetoff(it)+i).ge.mbndry) then
                  else
                    jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                    jf=jtet1(jtetoff(it)+i)-nef*(jt-1)
                    if(itetclr(it).ne.itetclr(jt)) then
                       jtet1(jtetoff(it)+i)=mbndry+jtet1(jtetoff(it)+i)
                       jtet1(jtetoff(jt)+jf)=mbndry+
     *                                  nef*(it-1)+i
                    endif
                  endif
               enddo
            enddo
C
            naddelm=0
            do i=1,naddelm1
               naddelm=naddelm+1
               iadd(naddelm)=iadd1(i)
               xadd(naddelm)=xadd1(i)
               yadd(naddelm)=yadd1(i)
               zadd(naddelm)=zadd1(i)
            enddo
            do i=1,nadd2
               naddelm=naddelm+1
               iadd(naddelm)=iadd2(i)
               xadd(naddelm)=xadd2(i)
               yadd(naddelm)=yadd2(i)
               zadd(naddelm)=zadd2(i)
            enddo
            nadd2=0
C
            call mmrelblk("itcheck",isubname,ipitcheck,icscode)
            goto 100
C
         endif
C
      enddo
C
      if(irecon.eq.1) then
         if(nsdtopo.eq.2 .and.
     *      nen.eq.nelmnen(ifelmtri) .and.
     *      nef.eq.nelmnef(ifelmtri)) then
            call dotaskx3d('recon/0 ; finish',ierror)
         elseif(nsdtopo.eq.3 .and.
     *          nen.eq.nelmnen(ifelmtet) .and.
     *          nef.eq.nelmnef(ifelmtet)) then
            call dotaskx3d('recon ; finish',ierror)
         endif
      else
         write(logmess,'(a)') 'REFINE NOT USING RECON.'
         call writloga('default',0,logmess,0,ier)
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ier)
      ipointi=npoints_start+1
      ipointj=npoints
      call cmo_set_info('ipointi',cmo,ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_set_info('ipointj',cmo,ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      call mmrelprt(isubname,icscode)
C
      goto 9999
 9999 continue
C
      return
      end
