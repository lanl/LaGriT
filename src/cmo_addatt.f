      subroutine cmo_addatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Adds Attributes to an existing Mesh Object.
C         It can also fill or initialize the attribute according to
C         the keywords below.
C
C cmo/addatt in general form will add a new attribute to a mesh object.
C If a keyword is used after the cmo name,
C the attribute will not only be created,
C but it will also be filled with values indicated from the keyword.
C If an attribute already exists and is appropriate type for the keyword,
C the values will be overwritten with the new keyword values.
C
C  general syntax:
C  cmo/addatt/mo_name/att_name/type/rank/length/interpolate/persistence/io/value
C
C  keyword syntax:
C  cmo/fillatt/cmoname/keyword
C                   -  will skip the call to cmo_addatt()
C                   - avoids error reports from trying to add existing attribute
C
C Vector of form      A[i1 j1 k1 i2 j2 k2 ... in jn kn]
C Scalar from Vector  Ai[1-n] Aj[1-n] Ak[1-n] 
C
C  KEYWORD OPTIONS:
C===============================================================
C  area_normal      - creates vector attribute
C                     fills with x,y,z area normal for each face
C                     implemented only for triangles
C                     implemented only for xyz, not rtp and rtz
C  unit_area_normal - creates vector attribute
C                     fills with x,y,z unit area normal for each face
C                     implemented only for triangles
C  synth_normal     - creates vector attribute using offsetsurf
C  node_normal        fills with xyz angle weighted average normal for each node
C                     vector components are put into scalar arrays
C                     x_n_norm, y_n_norm, z_n_norm
C                     same as node_normal
C                     tri elements only
C  synth_normal_angle - creates vector attribute using offsetsurf
C                     fills with xyz angle weighted average normal for each node
C                     vector components are put into scalar arrays
C                     x_n_norm, y_n_norm, z_n_norm
C                     tri elements only
C  synth_normal_area - creates vector attribute using offsetsurf
C                     fills with xyz angle weighted average normal for each node
C                     vector components are put into scalar arrays
C                     x_n_norm, y_n_norm, z_n_norm
C                     tri elements only
C  volume           - creates element attribute
C                     fills with volume(3D), area(2D) or length(lines)
C                     implemented only for triangle areas
C  voronoi_varea     - creates node attributes xn_varea, yn_varea, zn_varea
C                     same as computed for outside area
C  voronoi_volume   - creates node attribute storing volumes of Voronoi
C                     cells
C  old was vor_volume  fills with voronoi volume from getvoronoivolumes()
C
C  hybrid_volume    - creates node attribute storing volumes of hybrid
C                     median-Voronoi control volumes
C                  
C  vector           - creates one vector attribute
C                     fills with values from 3 scalar attributes
C  scalar           - creates three scalar attributes
C                     fills with values from one vector attribute
C  ang_mind         - creates one scalar attribute
C                     and fills it with the minimum dihedral angle (degrees) of the element
C  ang_minr         - creates one scalar attribute
C                     and fills it with the minimum dihedral angle (radian) of the element
C  ang_maxd         - creates one scalar attribute
C                     and fills it with the maximum dihedral angle (degrees) of the element
C  ang_maxr         - creates one scalar attribute
C                     and fills it with the maximum dihedral angle (radian) of the element
C  ang_mind_solid   - creates one scalar attribute
C                     and fills it with the minimum solid angle (degrees) of the element
C  ang_maxd_solid   - creates one scalar attribute
C                     and fills it with the minimum solid angle (radian) of the element
C  ang_minr_solid   - creates one scalar attribute
C                     and fills it with the maximum solid angle (degrees) of the element
C  ang_maxr_solid   - creates one scalar attribute
C                     and fills it with the maximum solid angle (radian) of the element
C  xyz_rtp          - creates three scalar attribute
C                     and fills them with the spherical coordinates r,theta,phi with theta and phi in radians.
C  xyz_rtz          - creates three scalar attribute
C                     and fills them with the cylindrical coordinates r,theta,z with theta in radians.
C  edge_connection  - creates one scalar attribute
C                     and fills it with the number of edges connected to a node.
C  node_num_diff    - creates one scalar attribute
C                     and fills it with the maximum node number difference between the node number
C                     and all connected nodes.
C
C                     conversion_factor = 180.0d+00/acos(-1.0d+00) to convert to degrees
C
C  Note: voronoi and median options are from codes voronoipts_lg medianpts_lg
C  voronoi          - x,y,z coordinates of the voronoi nodes for each element
C                     default xvor,yvor,zvor type vdouble and length nelements
C  median           - x,y,z coordinates of the median nodes for each element
C                     default xvor,yvor,zvor type vdouble and length nelements
C
C        Element attributes assigned associated node values:
C  avgnode          - assigns elem attrib the avg value of its node attributes
C  sumnode          - assigns elem attrib the sum value of its node attributes
C  minnode          - assigns elem attrib the min value of its node attributes
C  maxnode          - assigns elem attrib the max value of its node attributes
C
C
C             1    2     3
C Syntax for cmo/addatt/cmoname
C       4          5    6     7        8         9          10     11
C   / att_name  /type/rank/length/interpolate/persistence/ioflag/value
C
C Syntax for keyword options to fill attribute
C
C  cmo/addatt/cmoname/ ...plus syntax below...
C
C   / area_normal       / xyz | rtz | rtp / v_attname
C   / unit_area_normal  / xyz | rtz | rtp / v_attname
C   / node_normal      / v_attname
C   / synth_normal     / (will create x_n_norm y_n_norm z_n_norm) 
C   / volume       / attname /
C   / ang_mind   / elem_attname /
C   / ang_minr   / elem_attname /
C   / ang_maxd   / elem_attname /
C   / ang_maxr   / elem_attname /
C   / ang_mind_solid   / elem_attname /
C   / ang_maxd_solid   / elem_attname /
C   / ang_minr_solid   / elem_attname /
C   / ang_maxr_solid   / elem_attname /
C   / xyz_rtp          / node_attname_r node_attname_theta node_attname_phi | default c_r c_theta c_phi
C   / xyz_rtz          / node_attname_r node_attname_theta node_attname_z   | default c_r c_theta c_z
C   / vector       / V_attname   / x_attname, y_attname, z_attname
C   / scalar       / x_attname, y_attname, z_attname / V_attname
C   / vor_vector   / x_attname, y_attname, z_attname | default xvor,yvor,zvor 
C   / voronoi      / x_attname, y_attname, z_attname | default xvor,yvor,zvor
C   / voronoi_varea / x_attname, y_attname, z_attname | default xn_varea,yn_varea,zn_varea
C   / vor_volume   / attname / (old keyword)
C   / vorornoi_volume   / attname /
C   / median       / x_attname, y_attname, z_attname | default xmed,ymed,zmed
C   / sumnode      / elem_attname / node_attname
C   / avgnode      / elem_attname / node_attname
C   / minnode      / elem_attname / node_attname
C   / maxnode      / elem_attname / node_attname
C
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_addatt.f,v $
C         Revision 2.00  2007/11/05 19:45:47  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.18   07 Sep 2007 19:05:26   gable
CPVCS    Add keyword option node_num_diff that computes the max. difference
CPVCS    of node numbers connected to each node. This quantity impacts some
CPVCS    solvers matrix bandwidth. Each node i is visited, and all connected
CPVCS    edges, j1, j2, ... are computed. A node attribute is created that
CPVCS    contains max(abs(i-j)).
CPVCS    
CPVCS       Rev 1.17   22 Aug 2007 15:02:46   gable
CPVCS    Added edge_connections keyword option to create an integer attribute that contains the number of edge connections to each node.
CPVCS    
CPVCS       Rev 1.16   25 Jul 2007 10:02:26   gable
CPVCS    Add xyz_rtp and xyz_rtz options.
CPVCS    xyz_rtp computes coordinates in r,theta,phi coordinate system
CPVCS    xyz_rtz computes coordinates in r,theta,z   coordinate system
CPVCS    
CPVCS       Rev 1.15   24 Oct 2006 15:18:10   gable
CPVCS    Corrected command parser so that default mesh object is handled.
CPVCS    Added some error checking. Modified so that the mesh object that
CPVCS    is acted upon becomes the active mesh object when exiting from
CPVCS    the command.
CPVCS    
CPVCS       Rev 1.14   01 Sep 2006 13:43:52   gable
CPVCS    Added option of area weighted or angle weighted synthetic normal vectors.
CPVCS    
CPVCS       Rev 1.13   07 Jul 2006 08:50:08   gable
CPVCS    Added synth_normal and node_normal option via calls to offsetsurf command.
CPVCS    Note however that this implementation will not treat parent/child interfaces
CPVCS    as continuous and shared. Nodes on either side of the interface will compute
CPVCS    normals as if they are not connected. To account for parent/child interfaces
CPVCS    as shared nodes use of the synthnormal.f routine would give correct results.
CPVCS    
CPVCS       Rev 1.12   20 Jun 2006 18:19:40   gable
CPVCS    Added options to compute min/max solid/dihedral angle and make
CPVCS    an element attribute with these various quantities.
CPVCS    
CPVCS       Rev 1.11   23 Mar 2005 09:48:12   dcg
CPVCS    fix declarations for linux
CPVCS
CPVCS       Rev 1.10   26 Jan 2005 13:00:04   dcg
CPVCS    compute volumes for all types of elements
CPVCS
CPVCS       Rev 1.9   18 Jan 2005 07:47:54   tam
CPVCS    added new keyword to assign elem node values to element
CPVCS    avgnode, sumnode, minnode, maxnode are the keywords
CPVCS    Clarified error checking between CMO_ADDATT and ADDATT
CPVCS
CPVCS       Rev 1.8   18 Sep 2002 13:50:38   dcg
CPVCS    implement volume option for 3d grids
CPVCS
CPVCS       Rev 1.7   20 Dec 2001 16:29:52   dcg
CPVCS    fix type REAL should be real
CPVCS
CPVCS       Rev 1.6   28 Nov 2001 18:38:20   tam
CPVCS    add keyword option to fill newly created attributes with
CPVCS    area_normal, unit_area_normal, volume, vector, scalar
CPVCS
CPVCS       Rev 1.5   10 Apr 2001 11:06:12   dcg
CPVCS    get rid of bad line
CPVCS
CPVCS       Rev 1.4   17 Feb 2000 20:42:54   jtg
CPVCS    changed hardwired nwds=11 to only if <11 and use diff variable
CPVCS    so that can pass verbosity falg in 12th position to cmo_addatt_cmo
CPVCS
CPVCS       Rev 1.3   Tue Feb 01 13:38:36 2000   dcg
CPVCS
CPVCS       Rev 1.12   Tue Nov 02 19:00:16 1999   jtg
CPVCS    fixed hardwired character*32 for cmsgin
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 16:39:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   09/29/95 09:12:52   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.9   09/14/95 12:08:46   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.8   09/11/95 14:43:06   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.7   07/31/95 13:38:20   dcg
CPVCS    accept lower case for vdouble and vint and int
CPVCS
CPVCS       Rev 1.6   03/15/95 15:22:10   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.5   02/23/95 14:52:10   ejl
CPVCS    Fixed so defaults will work:
CPVCS    COMMAND////input//.
CPVCS
CPVCS       Rev 1.4   02/16/95 11:04:50   ejl
CPVCS    Fixed bug in defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 09:55:32   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.2   02/10/95 14:06:22   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:22   dcg
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
C
      include 'consts.h'
      include 'local_element.h'
c
c     compile errors from this include statement
c     define dcros functions in body instead
c      include 'statementfunctions.h'
C
C#######################################################################
C
      integer nwds, nwdsout, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return,nodeidx(8)
C
      integer nplen
      parameter (nplen=1000000)
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr, len, iexist, ioff, idebug, idone, imin, imax
      integer i1,i2,i3,i,j,k,jn,ipt,idx, ij_max
      integer index,irank,ilen,ityp,nen,nlength,flen
      integer itype_angle, if_valid_element, inclusive
      integer io_type, ncoef, ifcompress, ifhybrid
      integer nnode, nelem
c
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipisn, isn)
      pointer (ipitp, itp)
      integer itettyp(nplen),itet(nplen),itetoff(nplen)
      integer isn(nplen), itp(nplen)
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(nplen), yic(nplen), zic(nplen)
C
      pointer (ipvalue,  value)
      real*8  value(nplen)
      pointer (ipvalsrc,  valsrc)
      real*8  valsrc(nplen)
      pointer (ipivalsrc,  ivalsrc)
      integer  ivalsrc(nplen)
      pointer (ipivalue,  ivalue)
      integer  ivalue(nplen)
      pointer (ipiedges, iedges)
      integer  iedges(nplen)
      pointer (ipiedges_first, iedges_first)
      integer  iedges_first(nplen)
      pointer (ipmpary, mpary)
      integer  mpary(nplen)
      pointer (ipiparent, iparent)
      integer  iparent(nplen)

C     Attributes which will be used with the quad_quality option.
      pointer (ipregularity, regularity)
      real*8  regularity(nplen)
      pointer (ipquadflag, quadflag)
      integer quadflag(nplen)
 
      pointer (ipxvec, xvec)
      pointer (ipyvec, yvec)
      pointer (ipzvec, zvec)
      real*8  xvec(nplen), yvec(nplen), zvec(nplen)

c     pointer (ipvolic,volic)
c     real*8  volic
c     dimension volic(1000000)
c     real*8  volmin,volmax,voltot

 
      real*8 xnorm,ynorm,znorm,xsum,ysum,zsum,rvalue,
     *       xmin,ymin,zmin,xmax,ymax,zmax,epsarea,epsvol,
     *       mag,darea, xarea,yarea,zarea,
     *       x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *       a1x,a1z,a1y,xicvol(8),yicvol(8),zicvol(8)
      real*8 solid_angle(4)
 
      real*8 anglemin,anglemax,conversion_factor
      character*32 cmo_name,cmo_type,fill_type,norm_type
      character*32 aname
      character*32 att_list(3), att_name, vec_name, job_type
      character*32 ctype,
     *             crank,
     *             clength,
     *             cinterp,
     *             cpersistence,
     *             cioflag
      character*32 ctyp2,
     *             crank2,
     *             clen2,
     *             cintrp2,
     *             cpers2,
     *             cio2
 
      character*32 isubname
      character*132 logmess
 
      integer icharlnf
 
C#######################################################################
c     Copied from statementfunctions.h
c     Statement function and dummy variable declarations
 
      real*8 dcrosx,dcrosy,dcrosz
      real*8 x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3
 
      dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(y_a2-y_a1)
     &   *(z_a3-z_a1)-(z_a2-z_a1)*(y_a3-y_a1)
      dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(z_a2-z_a1)
     &   *(x_a3-x_a1)-(x_a2-x_a1)*(z_a3-z_a1)
      dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(x_a2-x_a1)
     &   *(y_a3-y_a1)-(y_a2-y_a1)*(x_a3-x_a1)
 
C
C
C#######################################################################
C
C
      isubname  = 'cmo_addatt'
      idone     = 0
      irank     = 1
      fill_type = 'notset'
      norm_type = 'notset'
      cmo_type  = 'notset'
      att_list(1)  = 'notset'
      att_list(2)  = 'notset'
      att_list(3)  = 'notset'
 
C.... Extract the CMO-name and get default settings
      cmo_name=cmsgin(3)
      if((cmo_name(1:5).ne.'-def-') .and.
     *   (cmo_name(1:5).ne.'-cmo-') .and.
     *   (cmo_name(1:9).ne.'-default-')) then
         call cmo_exist(cmo_name,ierr)
            if(ierr.ne.0) then
            write(logmess,*) 'ADDATT error: cannot find mesh object '
     &         //cmo_name(1:icharlnf(cmo_name))
            call writloga('default',1,logmess,0,ierr)
            return
            endif
         call cmo_set_name(cmo_name,ierr)
      else
         call cmo_get_name(cmo_name,ierr)
         call cmo_exist(cmo_name,ierr)
         cmsgin(3) = cmo_name
            if(ierr.ne.0) then
            write(logmess,*) 'ADDATT error: no default mesh object '
     &         //cmo_name(1:icharlnf(cmo_name))
            call writloga('default',1,logmess,0,ierr)
            return
            endif
      endif
      
      call cmo_get_mesh_type(cmo_name,cmo_type,ityp,ierr)
      call cmo_get_default_attparam(ctype,crank,clength,cinterp,
     *   cpersistence,cioflag,ierror_return)
 
C.... Extract the attribute-name or keyword and job addatt or fillatt
 
      att_name=cmsgin(4)
      job_type=cmsgin(2)
C
C.... Extract parameters for keywords Fill messeges for cmo_addatt
 
C.... keyword area_normal or unit_area_normal
      if (att_name(1:icharlnf(att_name)).eq.'area_normal' .or.
     *    att_name(1:icharlnf(att_name)).eq.'unit_area_normal' ) then
 
         fill_type = att_name
         flen = icharlnf(fill_type)

         if (cmo_type(1:3).ne.'tri') then
           write(logmess,'(a,a,a,a)')
     *     'ADDATT error: ',
     *     fill_type(1:icharlnf(fill_type)),
     *     ' not implemented for mesh elements: ',cmo_type(1:3)
           call writloga('default',1,logmess,0,ierr)
           ierror_return = 0
           goto 9999
         endif
         if(nwds.lt.5) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *      ' attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1 
            goto 9999
         endif
         if (cmsgin(5).eq.'xyz') then
           norm_type = 'xyz'
           att_name=cmsgin(6)
         elseif (cmsgin(5).eq.'rtz') then
           norm_type = 'rtz'
           att_name=cmsgin(6)
         elseif (cmsgin(5).eq.'rtp') then
           norm_type = 'rtp'
           att_name=cmsgin(6)
         else
           att_name=cmsgin(5)
           norm_type = 'xyz'
         endif
 
         if (norm_type(1:3).ne.'xyz') then
           write(logmess,'(a,a,a,a)')
     *     'ADDATT error: ',
     *     fill_type(1:icharlnf(fill_type)),
     *     ' not implemented for: ',norm_type(1:icharlnf(norm_type))
           call writloga('default',1,logmess,0,ierr)
           ierror_return = 0
           goto 9999
         endif
 
         cmsgin(5) ='VDOUBLE'
         cmsgin(6) = 'vector'
         cmsgin(7) = 'nelements'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero

C.... keyword synth_normal or node_normal
      elseif(
     1  (att_name(1:icharlnf(att_name)).eq.'synth_normal').or.
     2  (att_name(1:icharlnf(att_name)).eq.'synth_normal_angle').or.
     3  (att_name(1:icharlnf(att_name)).eq.'synth_normal_area').or.
     4  (att_name(1:icharlnf(att_name)).eq.'node_normal'))then
C
C      For backwards compatibility, the default is synth_normal
C      will result in angle weighted normal.
C 
         fill_type = 'synth_normal' 
         flen = icharlnf(fill_type)

         if (cmo_type(1:3).ne.'tri') then
           write(logmess,'(a,a,a,a)')
     *     'ADDATT/synth_normal warning: ',
     *     fill_type(1:icharlnf(fill_type)),
     *     ' not implemented for mesh elements: ',cmo_type(1:3)
           call writloga('default',1,logmess,0,ierr)
           ierror_return = 0
           goto 9999
         endif


       if(att_name(1:icharlnf(att_name)).eq.'synth_normal_angle')
     1   fill_type = 'synth_normal_angle' 
       if(att_name(1:icharlnf(att_name)).eq.'synth_normal_area')
     1   fill_type = 'synth_normal_area' 

         flen = icharlnf(fill_type)

C        offsetsurf will create these attributes, but assign them
C        here to be consistent with rest of code
C        adding them again will give a warning, then fill as usual
         irank = nwds - 4
         att_list(1) = "x_n_norm"
         att_list(2) = "y_n_norm"
         att_list(3) = "z_n_norm"
         if (irank.ne.0) then
            write(logmess,'(a,a)') 'ADDATT warning: ',
     * 'user attribute names ignored, offsetsurf creates attributes.'
            call writloga('default',1,logmess,0,ierr)
         endif

         irank = 3
         att_name = att_list(1)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nnodes'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero

C.... keyword voronoi_volume formerly vor_volume 
C.... keyword voronoi_varea  
      elseif( att_name(1:icharlnf(att_name)).eq.'vor_volume' .or.
     *   att_name(1:icharlnf(att_name)).eq.'voronoi_volume'  .or. 
     *   att_name(1:icharlnf(att_name)).eq.'voronoi_varea'   .or.
     *   att_name(1:icharlnf(att_name)).eq.'hybrid_volume') then

       if (cmo_type(1:3).eq.'tet') then
           if (att_name(1:icharlnf(att_name)).eq.'hybrid_volume') then
               fill_type = 'hybrid_volume'
           else
               fill_type = 'voronoi_volume'
           endif
       else if (cmo_type(1:3).eq.'tri') then
           fill_type = 'voronoi_varea'
       else
           write(logmess,'(a,a,a,a)')
     *     'ADDATT warning: ',
     *     fill_type(1:icharlnf(fill_type)),
     *     ' not implemented for mesh elements: ',cmo_type(1:3)
           call writloga('default',1,logmess,0,ierr)
           ierror_return = 0
           goto 9999
       endif
       att_name  = cmsgin(5)
       flen = icharlnf(fill_type)

       if(nwds.lt.5) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *      ' attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
       endif

C      for tet 
C      create single attribute with voronoi or hybrid volumes
       if (fill_type(1:flen) .eq. 'voronoi_volume' .or.
     *          fill_type(1:flen) .eq. 'hybrid_volume')  then
           cmsgin(5) = 'VDOUBLE'
           cmsgin(6) = 'scalar'
           cmsgin(7) = 'nnodes'
           cmsgin(8) = cinterp
           cmsgin(9) = cpersistence
           cmsgin(10)= cioflag
           xmsgin(11)= zero
       endif

C     for tri 
C     create x,y,z area attributes for triangles
C     this is the same computation used for outside tet nodes
      if (fill_type(1:flen) .eq. 'voronoi_varea' ) then

         irank = nwds - 4
         if (irank.eq.0) then
             att_list(1) = "xn_varea"
             att_list(2) = "yn_varea"
             att_list(3) = "zn_varea"
         else if (irank.eq.3) then
             att_list(1) = cmsgin(5) 
             att_list(2) = cmsgin(6) 
             att_list(3) = cmsgin(7) 
         else
            write(logmess,'(a,a)') 'ADDATT error: ',
     *      '3 x,y,z attribute names required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif

         irank = 3
         att_name = att_list(1)
         cmsgin(5) ='VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nnodes'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
      endif

      elseif (att_name(1:icharlnf(att_name)).eq.'quad_quality') then
        fill_type = att_name
        flen = icharlnf(fill_type)

         if(nwds.lt.7) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *        'three attribute names required'
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
         att_list(1) = cmsgin(5)
         att_list(2) = cmsgin(6)
         att_list(3) = cmsgin(7)

         irank = 3
         att_name = att_list(1)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nelements'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
      elseif (att_name(1:icharlnf(att_name)).eq.'volume' .or.
     *        att_name(1:icharlnf(att_name)).eq.'area'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_mind'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_minr'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_maxd'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_maxr'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_mind_solid'.or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_minr_solid'.or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_maxd_solid'.or.
     *        att_name(1:icharlnf(att_name)).eq.'ang_maxr_solid'.or.
     *        att_name(1:icharlnf(att_name)).eq.'length') then
 
         fill_type = att_name
         flen = icharlnf(fill_type)

         if (fill_type(1:4) .ne. 'ang_') then
           if (cmo_type(1:3).eq.'tri') then
             fill_type = 'area'
           else
             fill_type = 'volume'
           endif
         endif
         flen = icharlnf(fill_type)

         if(nwds.lt.5) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *        ' attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
         att_name  = cmsgin(5)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nelements'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
C.... keyword sumnode or avgnode or minnode or maxnode
      elseif (att_name(1:icharlnf(att_name)).eq.'sumnodes' .or.
     *        att_name(1:icharlnf(att_name)).eq.'avgnode'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'avgnodes'  .or.
     *        att_name(1:icharlnf(att_name)).eq.'minnode'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'minnodes'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'maxnode'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'maxnodes'   .or.
     *        att_name(1:icharlnf(att_name)).eq.'sumnode'  ) then
 
         if(nwds.lt.6) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *        ' attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
         fill_type = att_name
         flen = icharlnf(fill_type)
         att_name  = cmsgin(5)
 
C        Check and use type of src node based attribute
         att_list(1) = cmsgin(6)
         call cmo_get_attparam(att_list(1),cmo_name,index,ctype,
     *          crank,clength,cinterp,cpersistence,cioflag,ierr)
         if(ierr.ne.0) then
            call x3d_error(isubname,'get_info for node attribute')
            ierror_return = ierr
            goto 9999
         endif
         if(clength(1:5).ne.'nnode') then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *        ' source attribute must be of length nnode. '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
 
         if (fill_type(1:3).eq.'min' .or. fill_type(1:3).eq.'max') then
            i = icharlnf(att_name)
            if(att_name(1:i).eq.'xmin'.or.att_name(1:i).eq.'xmax'.or.
     *      att_name(1:i).eq. 'ymin'.or.att_name(1:i).eq. 'ymax'.or.
     *      att_name(1:i).eq. 'zmin'.or.att_name(1:i).eq. 'zmax') then
                write(logmess,'(a,a,a)') 'ADDATT error: ',
     *          'cmo attribute name reserved: ',att_name(1:i)
                 call writloga('default',1,logmess,0,ierr)
                ierror_return = -1
                goto 9999
            else
              cmsgin(5) = ctype
            endif
         else
            cmsgin(5) = 'VDOUBLE'
         endif
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nelements'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
C.... keyword scalar
      elseif (att_name(1:icharlnf(att_name)).eq.'scalar') then
         fill_type = 'scalar'
         flen = icharlnf(fill_type)

         if (nwds .eq. 8 ) then
           vec_name=cmsgin(8)
         else
           write(logmess,'(a)')
     *    'ADDATT error: scalar must have source vector attribute: '
           call writloga('default',1,logmess,0,ierr)
           write(logmess,'(a)')
     *    'cmo/addatt/cmoname/scalar/ v1_snk, v2_snk, v3_snk / v_src'
           call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
           goto 9999
         endif

         irank = 0
         do i = 5, nwds-1
           irank = irank+1
           att_list(irank) = cmsgin(i)
         enddo

         if (irank.ne.3) then
           write(logmess,'(a,i5)')
     *    'ADDATT error: scalar must have 3 attributes, got: ',irank
           call writloga('default',1,logmess,0,ierr)
           write(logmess,'(a)')
     *    'cmo/addatt/cmoname/scalar/ v1_snk, v2_snk, v3_snk / v_src'
           call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
           goto 9999
         endif
 
c        let rest of routine add first of 3 attributes att_name
         call cmo_get_attparam(vec_name,cmo_name,idx,ctyp2,crank2,
     *    clen2,cintrp2,cpers2,cio2,ierr)

         if (ierr.ne.0) then
           ierror_return = ierr
           write(logmess,'(a)')
     *     'ADDATT/scalar error: problem with source vector attribute:'
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,a,a,a)')
     *     "Error with cmo attribute:" ,cmo_name(1:icharlnf(cmo_name)),
     *     " : ",vec_name(1:icharlnf(vec_name)) 
           call writloga('default',0,logmess,1,ierr)
           goto 9999
         endif

         att_name = att_list(1)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = clen2
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
C.... keyword vector
      elseif (att_name(1:icharlnf(att_name)).eq.'vector') then
         fill_type = 'vector'
         flen = icharlnf(fill_type)
         if(nwds.lt.5) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *     ' mesh object name and attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
         att_name=cmsgin(5)
         irank = 0
         do i = 6, nwds
           irank = irank+1
           att_list(irank) = cmsgin(i)
         enddo
         if (irank.ne.3) then
           write(logmess,'(a,i5)')
     *    'ADDATT error: vector must have 3 attributes, got: ',irank
           call writloga('default',1,logmess,0,ierr)
           write(logmess,'(a)')
     *    'cmo/addatt/cmoname/vector / v_snk / v1_src, v2_src, v3_src '
           call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
           goto 9999
         endif
 
         call cmo_get_attparam(att_list(1),cmo_name,idx,ctyp2,crank2,
     *    clen2,cintrp2,cpers2,cio2,ierr)
 
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'vector'
         cmsgin(7) = clen2
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
 
C.... keyword voronoi or median (coordinates of)
      elseif (att_name(1:icharlnf(att_name)).eq.'voronoi' .or.
     *        att_name(1:icharlnf(att_name)).eq.'median' ) then
 
         fill_type = att_name(1:icharlnf(att_name))
         flen = icharlnf(fill_type)
         irank = 0
         do i = 5, nwds
           irank = irank+1
           att_list(irank) = cmsgin(i)
         enddo
 
         if (irank.eq.0) then
           if (fill_type(1:icharlnf(fill_type)).eq.'voronoi') then
             att_list(1) = "xvor"
             att_list(2) = "yvor"
             att_list(3) = "zvor"
           else
             att_list(1) = "xmed"
             att_list(2) = "ymed"
             att_list(3) = "zmed"
           endif
         elseif (irank.ne.3) then
           write(logmess,'(a,a,a,i5)')
     *      'ADDATT error: ',fill_type(1:icharlnf(fill_type)),
     *      ' needs 3 attributes, got: ',irank
           call writloga('default',1,logmess,0,ierr)
           write(logmess,'(a,a,a)')
     *      'cmo/addatt/cmoname/',fill_type(1:icharlnf(fill_type)),
     *      '/ vx_att, vy_att, vz_att '
           call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
           goto 9999
         endif
 
         irank = 3
         att_name = att_list(1)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nelements'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
 
C.... keyword xyz_rtp or xyz_rtz
      elseif (att_name(1:icharlnf(att_name)).eq.'xyz_rtp' .or.
     *        att_name(1:icharlnf(att_name)).eq.'xyz_rtz' ) then
 
         fill_type = att_name(1:icharlnf(att_name))
         flen = icharlnf(fill_type)
         irank = 0
         do i = 5, nwds
           irank = irank+1
           att_list(irank) = cmsgin(i)
         enddo
 
         if (irank.eq.0) then
           if (fill_type(1:icharlnf(fill_type)).eq.'rtp') then
             att_list(1) = "c_r"
             att_list(2) = "c_theta"
             att_list(3) = "c_phi"
           else
             att_list(1) = "c_r"
             att_list(2) = "c_theta"
             att_list(3) = "c_z"
           endif
         elseif (irank.ne.3) then
           write(logmess,'(a,a,a,i5)')
     *      'ADDATT error: ',fill_type(1:icharlnf(fill_type)),
     *      ' needs 3 attributes, got: ',irank
           call writloga('default',1,logmess,0,ierr)
           write(logmess,'(a,a,a)')
     *      'cmo/addatt/cmoname/',fill_type(1:icharlnf(fill_type)),
     *      '/ vx_att, vy_att, vz_att '
           call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
           goto 9999
         endif
 
         irank = 3
         att_name = att_list(1)
         cmsgin(5) = 'VDOUBLE'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nnodes'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
C
C     Number of edge connections to each node
C
      elseif((att_name(1:icharlnf(att_name)).eq.'edge_connections') .or.
     1       (att_name(1:icharlnf(att_name)).eq.'node_num_diff'))then 
         fill_type = att_name
         flen = icharlnf(fill_type)
         if(nwds.lt.5) then
            write(logmess,'(a,a)') 'ADDATT error: ',
     *        ' attribute name required: '
            call writloga('default',1,logmess,0,ierr)
            ierror_return = 1
            goto 9999
         endif
         att_name  = cmsgin(5)
         cmsgin(5) = 'VINT'
         cmsgin(6) = 'scalar'
         cmsgin(7) = 'nnodes'
         cmsgin(8) = cinterp
         cmsgin(9) = cpersistence
         cmsgin(10)= cioflag
         xmsgin(11)= zero
      else
C     done with keywords
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C.... Fill message parameters for general form of syntax
         if((nwds.lt.5) .or. (cmsgin(5)(1:5).eq.'-def-') .or.
     *                    (cmsgin(5)(1:9).eq.'-default-')) then
C        Default TYPE.
            cmsgin(5)=ctype
         endif
         if(nwds.ge.5) then
             if(cmsgin(5)(1:7).eq.'vdouble') cmsgin(5)='VDOUBLE'
             if(cmsgin(5)(1:4).eq.'vint') cmsgin(5)='VINT'
             if(cmsgin(5)(1:3).eq.'int') cmsgin(5)='INT'
             if(cmsgin(5)(1:9).eq.'character') cmsgin(5)='CHARACTER'
             if(cmsgin(5)(1:4).eq.'real') cmsgin(5)='REAL'
             if(cmsgin(5)(1:5).eq.'vchar') cmsgin(5)='VCHAR'
 
         endif
C
         if((nwds.lt.6) .or. (cmsgin(6)(1:5).eq.'-def-') .or.
     *                    (cmsgin(6)(1:9).eq.'-default-')) then
C        Default RANK.
            cmsgin(6)=crank
         endif
C
         if((nwds.lt.7) .or. (cmsgin(7)(1:5).eq.'-def-') .or.
     *                    (cmsgin(7)(1:9).eq.'-default-')) then
C        Default LENGTH.
            cmsgin(7)=clength
         endif
C
         if((nwds.lt.8) .or. (cmsgin(8)(1:5).eq.'-def-') .or.
     *                    (cmsgin(8)(1:9).eq.'-default-')) then
C        Default INTERPOLATION.
            cmsgin(8)=cinterp
         endif
C
         if((nwds.lt.9) .or. (cmsgin(9)(1:5).eq.'-def-') .or.
     *                    (cmsgin(9)(1:9).eq.'-default-')) then
C        Default PERSISTENCE.
            cmsgin(9)=cpersistence
         endif
C
         if((nwds.lt.10) .or. (cmsgin(10)(1:5).eq.'-def-') .or.
     *                     (cmsgin(10)(1:9).eq.'-default-')) then
C        Default IO.
            cmsgin(10)=cioflag
         endif
C
         if((nwds.lt.11) .or. (cmsgin(11)(1:5).eq.'-def-') .or.
     *                     (cmsgin(11)(1:9).eq.'-default-')) then
C        Default VALUE.
            xmsgin(11)= zero
         endif
C
      endif
c     end filling message parameters
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
      cmsgin(4) = att_name
      nwdsout=nwds
      if (nwds.lt.11) nwdsout=11
 
C.... Option to skip call to cmo_addatt, but still fill attribute
      if (job_type(1:7).ne.'fillatt') then
 
C.... Add new mesh attribute to Default or Named Mesh Object
      len=icharlnf(cmo_name)
      if(cmo_name(1:len).eq.'-default-') then
C
C....    Add Attribute to the Default Mesh Object Table.
C
         call cmo_addatt_def(imsgin,xmsgin,cmsgin,msgtype,nwdsout,
     *                       ierror_return)
         if(ierror_return.eq.0) then
            call cmo_verify_def(ierror_return)
C
            if(ierror_return.ne.0) then
               write(logmess,'(a,a,a)')
     *               'ADDATT error: ',
     *               'Mesh Object is not consistent: ', cmo_name
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
C
      else
C
C....    Add Attribute to a Mesh Object.
C
C        Use the Current Mesh Object.
         if((cmo_name(1:len).eq.'-cmo-') .or.
     *      (cmo_name(1:len).eq.'-def-')) then
C
            call cmo_get_name(cmo_name,ierror_return)
            cmsgin(3)=cmo_name
C
         endif
C
         call cmo_exist(cmo_name,iexist)
         if(iexist.ne.0) then
            ierror_return=-1
            write(logmess,'(a,a)')
     *            '     Mesh Object does not exist: ', cmo_name
            call writloga('default',0,logmess,0,ierr)
         else
C
            call cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *                          nwdsout,ierror_return)
            if(ierror_return.eq.0) then
               call cmo_verify_cmo(cmo_name,ierror_return)
               if(ierror_return.ne.0) then
                  write(logmess,'(a,a,a)')
     *                  'ADDATT error: ',
     *                  'Mesh Object is not consistent: ', cmo_name
                  call writloga('default',0,logmess,0,ierr)
               endif
            endif
C
         endif
      endif
      endif
C     second endif for skipping addatt call
C
C     End adding the attribute att_name to the Mesh Object

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     setup and add attribute done, now fill the attributes
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C     Notify User if we are going to write to a new or old attribute
      if (ierror_return.eq.-1) iexist = 1
      if (fill_type(1:6).ne.'notset') then

C tam - print job_type but do not need to check addatt or fillatt here
C     - just check iexist and write information
C     - removed WARNING as it already appears earlier

         if (iexist.eq.1) then

           if (irank .eq. 3) then
             write(logmess,'(a,a,a,a,2x,a,2x,a)')
     *        'ADDATT/',fill_type(1:icharlnf(fill_type)),
     *        ': writing to existing attributes: ',
     *         att_list(1)(1:icharlnf(att_list(1) )),
     *         att_list(2)(1:icharlnf(att_list(2) )),
     *         att_list(3)(1:icharlnf(att_list(3) ))
               call writloga('default',0,logmess,0,ierr)
           else
            write(logmess,'(a,a,a,a)')
     *      'ADDATT/',fill_type(1:icharlnf(fill_type)),
     *      ': writing to existing attribute: ',
     *      att_name(1:icharlnf(att_name))
            call writloga('default',0,logmess,0,ierr)
           endif

         else

           if (irank .eq. 3) then
              write(logmess,'(a,a,a,a,2x,a,2x,a)')
     *         'ADDATT/',fill_type(1:icharlnf(fill_type)),
     *         ': creating 3 new attributes: ',
     *          att_list(1)(1:icharlnf(att_list(1) )),
     *          att_list(2)(1:icharlnf(att_list(2) )),
     *          att_list(3)(1:icharlnf(att_list(3) ))
               call writloga('default',0,logmess,0,ierr)
           else
               write(logmess,'(a,a,a,a)')
     *         'ADDATT/',fill_type(1:icharlnf(fill_type)),
     *         ': creating new attribute: ',
     *          att_name(1:icharlnf(att_name))
               call writloga('default',0,logmess,0,ierr)
           endif

         endif
         job_type='fillatt'
      endif
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C-------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Fill new attribute based on fill_type keyword
C     check for valid sink attribute, check types, get cmo ptrs
 
      if (job_type(1:7).eq.'fillatt') then
 
C....   Setup for fill_type with keyword options
 
        call cmo_get_attparam(att_name,cmo_name,idx,ctype,crank,
     *       clength,cinterp,cpersistence,cioflag,ierr)
        if (ierr.ne.0) then
           ierror_return = ierr
           write(logmess,'(a,a,a)')
     *     'ADDATT error: ',
     *     'Attribute to fill does not exist: ',att_name
           call writloga('default',0,logmess,0,ierr)
           goto 9999
        endif
 
c       most attributes need to be of type vdouble
c
c       'edge_connections' is integer.
c
        if (fill_type(1:7).ne.'minnode' .and.
     *      fill_type(1:7).ne.'maxnode' .and.
     *      fill_type(1:13).ne.'node_num_diff' .and.
     *      fill_type(1:16).ne.'edge_connections')     then
           if ( ctype(1:8).ne.'VDOUBLE') then
             write(logmess,'(a,a,a,a)')
     *       'ADDATT: ',
     *       'Attribute needs to be VDOUBLE: ',att_name,ctype
             call writloga('default',0,logmess,0,ierr)
             goto 9999
             ierror_return = 1
           endif
        endif
 
c       some attributes must be vector rank
        if(fill_type(1:icharlnf(fill_type)).eq.'area_normal' .or.
     *     fill_type(1:icharlnf(fill_type)).eq.'unit_area_normal' .or.
     *     fill_type(1:icharlnf(fill_type)).eq.'vector' ) then
           if (crank(1:6).ne.'vector') then
             write(logmess,'(a,a,a,a)')
     *       'ADDATT: ',
     *       'Attribute needs to be vector: ',att_name,crank
             call writloga('default',0,logmess,0,ierr)
             ierror_return = 1
             goto 9999
           endif
        endif
 
        call cmo_get_info('xic',cmo_name,ipxic,ilen,ityp,ierr)
        call cmo_get_info('yic',cmo_name,ipyic,ilen,ityp,ierr)
        call cmo_get_info('zic',cmo_name,ipzic,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info xyz')
        call cmo_get_info('itettyp',cmo_name,ipitettyp,ilen,ityp,ierr)
        call cmo_get_info('itetoff',cmo_name,ipitetoff,ilen,ityp,ierr)
        call cmo_get_info('itet',cmo_name,ipitet,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info itet')
        call cmo_get_info('idebug',cmo_name,idebug,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info idebug')
 
c       get epsilon
        call setsize()
        call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsarea,epsvol)
        if (idebug.gt.0) then
          write(logmess,'("epsilon area: ",1pe15.7)') epsarea
          call writloga('default',1,logmess,0,ierr)
          write(logmess,'("epsilon vol:  ",1pe15.7)') epsvol
          call writloga('default',0,logmess,1,ierr)
        endif
 
c     end setup for fill_type with keyword options
      endif
 
C     SWITCH on the keyword types to fill sink attribute
 
C.... keyword area_normal or unit_area or area
      flen = icharlnf(fill_type)
C     Fill each triangle attribute with normal
      if (fill_type(1:flen).eq. 'area_normal'  .or.
     *  fill_type(1:flen).eq.  'unit_area_normal' .or.
     *  fill_type(1:flen).eq.  'area'          ) then
 
       call cmo_get_info(att_name,cmo_name,ipvalue,ilen,ityp,ierr)
       if(ierr.ne.0) call x3d_error(isubname,'get_info new attribute')
       call cmo_get_intinfo('nelements',cmo_name,nlength,ilen,ityp,ierr)
       if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
 
c      Calculate the area normal for each triangle.
       do i = 1, nlength
 
         i1=itet(itetoff(i)+1)
         i2=itet(itetoff(i)+2)
         i3=itet(itetoff(i)+3)
         x_a1=xic(i1)
         y_a1=yic(i1)
         z_a1=zic(i1)
         x_a2=xic(i2)
         y_a2=yic(i2)
         z_a2=zic(i2)
         x_a3=xic(i3)
         y_a3=yic(i3)
         z_a3=zic(i3)
c
         a1x = dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)
         a1y = dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)
         a1z = dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)
         xnorm = a1x
         ynorm = a1y
         znorm = a1z
 
         if (idebug.ge.9) then
           write(logmess,'(a,i15,a,e14.6,e14.6,e14.6)')
     *     'element norm',i,': ',xnorm,ynorm,znorm
           call writloga('default',0,logmess,0,ierr)
         endif
 
         if (abs(xnorm).lt.epsarea) xnorm = zero
         if (abs(ynorm).lt.epsarea) ynorm = zero
         if (abs(znorm).lt.epsarea) znorm = zero
 
         darea = 0.5d0*sqrt(a1x**2+a1y**2+a1z**2)
         mag = sqrt( (xnorm*xnorm)+(ynorm*ynorm)+(znorm*znorm) )
 
c        Fill vector attribute with face normals
c        TAM - area normals appear twice as large as expected
C        so take half of cross product before assignment 
C        keep direction sign

         flen = icharlnf(fill_type)
         if (fill_type(1:flen).eq. 'area_normal') then
           idx = (i-1)*3 + 1
           value(idx) = 0.5d0* xnorm
           idx = (i-1)*3 + 2
           value(idx) = 0.5d0* ynorm
           idx = (i-1)*3 + 3
           value(idx) = 0.5d0* znorm
 
c        Fill vector attribute with unit normals
         elseif (fill_type(1:4).eq. 'unit') then
           idx = (i-1)*3 + 1
           value(idx) = xnorm/mag
           idx = (i-1)*3 + 2
           value(idx) = ynorm/mag
           idx = (i-1)*3 + 3
           value(idx) = znorm/mag
 
c        Fill array attribute with area
         else
           value(i) =  darea
         endif
 
         if (idebug.ge.9) then
           write(logmess,'(a,i15,a,e14.6,e14.6,e14.6)')
     *     'element unorm',i,': ',xnorm/mag,ynorm/mag,znorm/mag
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,i15,a,e14.6)')
     *     'element area',i,': ',darea
           call writloga('default',0,logmess,0,ierr)
         endif
 
       enddo
c      end loop through all triangles
        ierror_return = 0
 
 
C.... keyword synth_normal
C     Fill node attribute with average or synthetic normal
      elseif (fill_type(1:flen).eq. 'synth_normal') then
 
C This needs to be written?what?
C       NODE -- central node.
C       NELTS -- number of surrounding triangles.
C       IELTS -- array of triangle numbers.
C       IPARENT -- parent node array.
C       ITET -- triangle-node relation.
C       ITETOFF -- offset array for triangle node relation.
C       XIC,YIC,ZIC -- x,y,z node coordinate arrays.
C
C      call synthnormal(node,nelts,ielts,iparent,itet,itetoff,
C     &   xic,yic,zic,epsln,synthx,synthy,synthz,lsomereversed)
C
C       This implementation does not deal with parent/child data structure.
C       It assumes there are no parent child chains.
C
C       Use the offsetsurf routine to compute angle weighted normals and
C       just keep the attribute around.
C
C       Old version defaults to angle weighted normal.
C

C       offsetsurf will addatt to cmo_name during routine
        write(logmess,'(a)')
     *  'offsetsurf filling synth normal attributes for '
        call writloga('default',0,logmess,0,ierr)
        write(logmess,'(2x,a,a)')
     *  cmo_name(1:icharlnf(cmo_name)),': x_n_norm, y_n_norm, z_n_norm'
        call writloga('default',0,logmess,0,ierr)

        logmess = 
     1   'offsetsurf/-tmp_wrk_mo-/'//cmo_name//'/0.0/keepatt;finish'

C
        flen = icharlnf(fill_type)
        if (fill_type(1:icharlnf(fill_type)).eq.
     1            'synth_normal_angle') then
        logmess = 
     1   'offsetsurf/-tmp_wrk_mo-/'//cmo_name//'/0.0/keep_angle;finish'
        elseif (fill_type(1:icharlnf(fill_type)).eq.
     1            'synth_normal_area') then
        logmess = 
     1   'offsetsurf/-tmp_wrk_mo-/'//cmo_name//'/0.0/keep_area;finish'
        endif

C       fill and create attributes with offsetsurf
        call dotask(logmess, ierr)

C       clean up
        logmess = 'cmo/delete/-tmp_wrk_mo-;finish'
        call dotask(logmess, ierr)
        logmess = 'cmo/select/'//cmo_name//';finish'
        call dotask(logmess, ierr)
        ierror_return = 0
 
C.... keyword voronoi_varea (vector area)
C     Fill node attribute with voronoi area vector 
      elseif (fill_type(1:flen).eq. 'voronoi_varea' ) then

c       add the remaining 2 attributes
        do i = 2,3
         cmsgin(4)= att_list(i)
         att_name= att_list(i)

         call cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *             nwdsout,ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *       'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           else
            write(logmess,'(a,a,a,a)')
     *      'ADDATT/',fill_type(1:icharlnf(fill_type)),
     *      ': creating new attribute: ',
     *      att_name(1:icharlnf(att_name))
            call writloga('default',0,logmess,0,ierr)
           endif
          endif
        enddo

        call cmo_get_info(att_list(1),cmo_name,ipxvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(2),cmo_name,ipyvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(3),cmo_name,ipzvec,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info new attribute')

       
        call cmo_get_intinfo('nnodes',cmo_name,nnode,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')

        call cmo_get_intinfo('nelements',
     *                        cmo_name,nlength,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements')

        if (nnode .gt. 0) then

C          loop through elements and add areas to respective nodes
C          reverse sign, triangles are inward facing by x3d convention
           do i=1, nlength

             i1=itet(itetoff(i)+1)
             i2=itet(itetoff(i)+2)
             i3=itet(itetoff(i)+3)
             x1=xic(i1)
             y1=yic(i1)
             z1=zic(i1)
             x2=xic(i2)
             y2=yic(i2)
             z2=zic(i2)
             x3=xic(i3)
             y3=yic(i3)
             z3=zic(i3)

C            point 1
             call voronoi_vector_area(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *          xarea,yarea,zarea)
               xvec(i1) = xvec(i1) + (-xarea)
               yvec(i1) = yvec(i1) + (-yarea)
               zvec(i1) = zvec(i1) + (-zarea)

             if (idebug.ge.5) then
               write(logmess,'(a,i15,a,e14.6,e14.6,e14.6)')
     *         'node voronoi_varea ',i1,': ',xarea,yarea,zarea
               call writloga('default',0,logmess,0,ierr)
             endif

C            point 2
             call voronoi_vector_area(x2,y2,z2,x3,y3,z3,x1,y1,z1,
     *          xarea,yarea,zarea)
               xvec(i2) = xvec(i2) + (-xarea)
               yvec(i2) = yvec(i2) + (-yarea)
               zvec(i2) = zvec(i2) + (-zarea)
             if (idebug.ge.5) then
               write(logmess,'(a,i15,a,e14.6,e14.6,e14.6)')
     *         'node voronoi_varea ',i2,': ',xarea,yarea,zarea
               call writloga('default',0,logmess,0,ierr)
             endif

C            point 3
             call voronoi_vector_area(x3,y3,z3,x1,y1,z1,x2,y2,z2,
     *          xarea,yarea,zarea)
               xvec(i3) = xvec(i3) + (-xarea)
               yvec(i3) = yvec(i3) + (-yarea)
               zvec(i3) = zvec(i3) + (-zarea)
             if (idebug.ge.5) then
               write(logmess,'(a,i15,a,e14.6,e14.6,e14.6)')
     *         'node voronoi_varea ',i3,': ',xarea,yarea,zarea
               call writloga('default',0,logmess,0,ierr)
             endif

            enddo
C           end loop through nelement triangles

       endif

C.... keyword vor_volume (also voronoi_volume)
C     Fill node attribute with voronoi volumes
C     let anothermatbld3d_wrapper do the work
C     flag io to write to attribute instead of file

      elseif (fill_type(1:flen) .eq. 'voronoi_volume' .or.
     *        fill_type(1:flen) .eq. 'hybrid_volume') then

       call cmo_get_intinfo('nnodes',cmo_name,nlength,ilen,ityp,ierr)
       if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')

       if (nlength .gt. 0) then
         io_type = 5
         ncoef = 1
         ifcompress = 0
         if (fill_type(1:flen) .eq. 'hybrid_volume') then
             ifhybrid = 1
         else
             ifhybrid = 0
         endif
         aname = att_name

         call anothermatbld3d_wrapper(aname,io_type,ncoef,ifcompress,
     *      ifhybrid)
         goto 9998

       else
         call x3d_error(isubname,'zero length attribute '//att_name)
       endif

C.... keyword volume
C     Fill element attribute with volume or length
C     This needs to be finished
C     area is done above with triangle work, do volume and length here
     
      elseif (fill_type(1:icharlnf(fill_type)).eq. 'volume') then
         call cmo_get_intinfo('nelements',cmo_name,nlength,ilen,ityp,
     *      ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
         call cmo_get_intinfo('nodes_per_element',cmo_name,nen,ilen,
     *     ityp, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
         call cmo_get_info(att_name,cmo_name,ipvalue,ilen,ityp,ierr)
         if(ierr.ne.0) call x3d_error(isubname,
     *      'get_info new attribute')

         do i=1,nlength
            do j=1,nelmnen(itettyp(i))
               nodeidx(j)=itet(itetoff(i)+j)
               xicvol(j)=xic(nodeidx(j))
               yicvol(j)=yic(nodeidx(j))
               zicvol(j)=zic(nodeidx(j))
            enddo
            call volume_element(itettyp(i),xicvol,yicvol,zicvol,
     *        value(i))
         enddo
 
C.... keyword ang_mind, ang_minr, ang_maxd, ang_maxr
C.... keyword ang_mind_solid, ang_minr_solid, ang_maxd_solid, ang_maxr_solid
C     Fill element attribute with min or max dihedral angle in degrees or radians
      elseif ((fill_type(1:flen).eq. 'ang_mind') .or. 
     *        (fill_type(1:flen).eq. 'ang_minr') .or. 
     *        (fill_type(1:flen).eq. 'ang_maxd') .or. 
     *        (fill_type(1:flen).eq. 'ang_maxr') .or.
     *        (fill_type(1:flen).eq. 'ang_mind_solid') .or. 
     *        (fill_type(1:flen).eq. 'ang_minr_solid') .or. 
     *        (fill_type(1:flen).eq. 'ang_maxd_solid') .or. 
     *        (fill_type(1:flen).eq. 'ang_maxr_solid') )then

         if(fill_type(10:14).eq. 'solid')then
            itype_angle = 2
         else
            itype_angle = 1
         endif
C
C        Convert radians to degrees : value * 180./pi
C        Convert radians to degrees : value * 180./3.141592653589793
C
      conversion_factor = 180.0d+00/acos(-1.0d+00)
      
         call cmo_get_intinfo('nelements',cmo_name,nlength,ilen,ityp,
     *      ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
         call cmo_get_intinfo('nodes_per_element',cmo_name,nen,ilen,
     *     ityp, ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
         call cmo_get_info(att_name,cmo_name,ipvalue,ilen,ityp,ierr)
         if(ierr.ne.0) call x3d_error(isubname,
     *      'get_info new attribute')

         if_valid_element = 0
         do i=1,nlength
            do j=1,nelmnen(itettyp(i))
               nodeidx(j)=itet(itetoff(i)+j)
               xicvol(j)=xic(nodeidx(j))
               yicvol(j)=yic(nodeidx(j))
               zicvol(j)=zic(nodeidx(j))
            enddo
            if(itype_angle .eq. 1)then
            call dihangle_element(itettyp(i),xicvol,yicvol,zicvol,
     *                            anglemin,anglemax)
            elseif((itype_angle .eq. 2).and.
     *             (itettyp(i).ne.ifelmtet)) then
C
C              Invalid option, Solid Angle only supported for Tets
C
               if_valid_element = if_valid_element + 1
               anglemin =  99999.d0
               anglemax = -99999.d0
            elseif((itype_angle .eq. 2).and.
     *             (itettyp(i).eq.ifelmtet)) then
C
C           Compute solid angles of tet
C
            call solid_angle_lg(solid_angle(1),
     *          xicvol(2), yicvol(2), zicvol(2),
     *          xicvol(3), yicvol(3), zicvol(3),
     *          xicvol(4), yicvol(4), zicvol(4),
     *          xicvol(1), yicvol(1), zicvol(1))
            call solid_angle_lg(solid_angle(2),
     *          xicvol(1), yicvol(1), zicvol(1),
     *          xicvol(4), yicvol(4), zicvol(4),
     *          xicvol(3), yicvol(3), zicvol(3),
     *          xicvol(2), yicvol(2), zicvol(2))
            call solid_angle_lg(solid_angle(3),
     *          xicvol(1), yicvol(1), zicvol(1),
     *          xicvol(2), yicvol(2), zicvol(2),
     *          xicvol(4), yicvol(4), zicvol(4),
     *          xicvol(3), yicvol(3), zicvol(3))
            call solid_angle_lg(solid_angle(4),
     *          xicvol(1), yicvol(1), zicvol(1),
     *          xicvol(3), yicvol(3), zicvol(3),
     *          xicvol(2), yicvol(2), zicvol(2),
     *          xicvol(4), yicvol(4), zicvol(4))

            anglemin =  1.e20
            anglemax = -1.e20
            do j = 1,4
              anglemin = min(anglemin, solid_angle(j))
              anglemax = max(anglemax, solid_angle(j))
            enddo
            endif
C
C        Save either the min or max value.
C
            if(fill_type(5:7).eq. 'min')then
               value(i) = anglemin
            else
               value(i) = anglemax
            endif
C
C        Convert radians to degrees : value * 180./pi
C        Convert radians to degrees : value * 180./3.141592653589793
C
            if(fill_type(8:8).eq. 'd') then
               value(i) = conversion_factor * value(i)
            endif
         enddo

         if(if_valid_element .ne. 0)then
           write(logmess,'(a)')
     *     'WARNING: Solid Angle Calculation Only Supported for Tets'
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,i12)')
     *     'Number of Elements Tested            = ', nlength
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,i12)')
     *     'Number of Invalid (non-tet) Elements = ', if_valid_element
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,e20.12)')
     *     'Special Value (radians) : anglemin = ', 99999.d0
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a,e20.12)')
     *     'Special Value (radians) : anglemax = ',-99999.d0
           call writloga('default',0,logmess,0,ierr)
           write(logmess,'(a)')
     *     'WARNING: Solid Angle Calculation Only Supported for Tets'
           call writloga('default',0,logmess,0,ierr)
         endif
         
C.... keyword sumnode or avgnode or minnode or maxnode
C     Fill element attribute with values using node attribute
      elseif (fill_type(1:flen).eq. 'sumnode'  .or.
     *        fill_type(1:flen).eq. 'avgnode'  .or.
     *        fill_type(1:flen).eq. 'minnode'  .or.
     *        fill_type(1:flen).eq. 'maxnode'       ) then
 
c       get definitions of source node attribute and assign pointers
        call cmo_get_attparam(att_list(1),cmo_name,index,ctyp2,
     *       crank2,clen2,cintrp2,cpers2,cio2,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo src attribute ')
        if (ctyp2(1:4).eq.'VINT') then
         call cmo_get_info(att_list(1),cmo_name,ipivalsrc,i,ityp,ierr)
        else
         call cmo_get_info(att_list(1),cmo_name,ipvalsrc,i,ityp,ierr)
        endif
        if(ierr.ne.0) call x3d_error(isubname,'get_info src attribute')
        if (ierr.ne.0) goto 9999
 
 
c       get definitions of new element attribute and assign pointers
C       new attribute is VDOUBLE when using avg or sum of nodes
        call cmo_get_attparam(att_name,cmo_name,index,ctype,
     *          crank,clength,cinterp,cpersistence,cioflag,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo new attribute ')
        if (fill_type(1:flen).eq. 'sumnode' .or.
     *      fill_type(1:flen).eq. 'avgnode') then
            if (ctype(1:flen).ne.'VDOUBLE') then
              call x3d_error(isubname,'New attribute not VDOUBLE.')
              goto 9999
            endif
        endif
c       sink attribute must be type element
        if(clength(1:8).ne.'nelement') then
           write(logmess,'(a,a)') 'ADDATT error: ',
     *       ' sink attribute must be of length nelement. '
           call writloga('default',1,logmess,0,ierr)
           ierror_return = 1
           goto 9999
        endif
 
        if (ctype(1:4).eq.'VINT') then
          call cmo_get_info(att_name,cmo_name,ipivalue,ilen,ityp,ierr)
        else
          call cmo_get_info(att_name,cmo_name,ipvalue,ilen,ityp,ierr)
        endif
        call cmo_get_info('itettyp',cmo_name,ipitettyp,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info cmo')
        if (ierr.ne.0) goto 9999
 
c       get length and pointers of new sink element attribute
        call cmo_get_intinfo('nelements',cmo_name,nlength,i,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
        if (ierr.ne.0) goto 9999
 
 
C       loop through elements using node values from src attribute
        do idx = 1,nlength
           xsum = zero
           nen = nelmnen(itettyp(idx))
 
c      ....sum or average value of nodes assumes sink attribute is vdouble
           if (fill_type(1:flen).eq. 'sumnode' .or.
     *       fill_type(1:flen).eq. 'avgnode') then
 
             do i = 1,nen
                 ipt = itet(itetoff(idx)+i)
                 if (ctyp2(1:4).eq.'VINT') then
                   xsum = xsum + real(ivalsrc(ipt))
                 else
                   xsum = xsum + valsrc(ipt)
                 endif
             enddo
             if (fill_type(1:flen).eq. 'avgnode') then
                 value(idx) = xsum / real(nen)
             elseif (fill_type(1:flen).eq. 'sumnode') then
                 value(idx) = xsum
             endif
 
c      ....min or max value of nodes, att_name inherits type from src
           elseif (fill_type(1:flen).eq. 'minnode' .or.
     *         fill_type(1:flen).eq. 'maxnode') then
 
C             integer attributes for min max
              if (ctype(1:4).eq.'VINT') then
 
                do i = 1,nen
                    ipt = itet(itetoff(idx)+i)
                    if (i.eq.1) then
                       imin = ivalsrc(ipt)
                       imax = ivalsrc(ipt)
                    endif
                    if (ivalsrc(ipt).lt.imin) imin = ivalsrc(ipt)
                    if (ivalsrc(ipt).gt.imax) imax = ivalsrc(ipt)
                enddo
                if (fill_type(1:flen).eq. 'minnode') then
                   ivalue(idx) = imin
                elseif (fill_type(1:flen).eq. 'maxnode') then
                   ivalue(idx) = imax
                endif
 
C             real attributes for min max
              else
 
                do i = 1,nen
                    ipt = itet(itetoff(idx)+i)
                    if (i.eq.1) then
                       xmin = valsrc(ipt)
                       xmax = valsrc(ipt)
                    endif
                    if (valsrc(ipt).lt.imin) xmin = valsrc(ipt)
                    if (valsrc(ipt).gt.imax) xmax = valsrc(ipt)
                enddo
                if (fill_type(1:flen).eq. 'minnode') then
                   value(idx) = xmin
                elseif (fill_type(1:flen).eq. 'maxnode') then
                   value(idx) = xmax
                endif
              endif
 
           endif
c       end loop through elements in new attribute
        enddo
 
c
C.... keyword vector
C     Fill vector attribute from list of scalar attributes
      elseif (fill_type(1:flen).eq. 'vector') then
 
        if (clen2(1:5).eq.'nnode') then
          call cmo_get_intinfo('nnodes',
     *         cmo_name,nlength,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')
        else
          call cmo_get_intinfo('nelements',
     *         cmo_name,nlength,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
        endif
        call cmo_get_info(att_list(1),cmo_name,ipxvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(2),cmo_name,ipyvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(3),cmo_name,ipzvec,ilen,ityp,ierr)
        call cmo_get_info(att_name,cmo_name,ipvalue,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info new attribute')
 
c       assumes vector of rank 3
        do i = 1,nlength
            idx = (i-1)*irank + 1
            value(idx) = xvec(i)
            idx = (i-1)*irank + 2
            value(idx) = yvec(i)
            idx = (i-1)*irank + 3
            value(idx) = zvec(i)
        enddo
        ierror_return = 0
 
 
C.... keyword scalar
C     Fill list of scalar attributes from vector attribute
      elseif (fill_type(1:flen).eq. 'scalar') then
 
c       create the remaining two attributes
        do i = 2,3
         cmsgin(4)= att_list(i)
         call cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *             nwdsout,ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *       'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           endif
          endif
         enddo
 
        if (clen2(1:5).eq.'nnode') then
          call cmo_get_intinfo('nnodes',
     *         cmo_name,nlength,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')
        else
          call cmo_get_intinfo('nelements',
     *         cmo_name,nlength,ilen,ityp,ierr)
          if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
        endif
        call cmo_get_info(att_list(1),cmo_name,ipxvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(2),cmo_name,ipyvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(3),cmo_name,ipzvec,ilen,ityp,ierr)
        if(ierr.ne.0) then
           ierror_return = ierr
           call x3d_error(isubname,'get_info new scalar attribute')
           go to 9999
        endif
        call cmo_get_info(vec_name,cmo_name,ipvalue,ilen,ityp,ierr)
        if(ierr.ne.0) then
           ierror_return = ierr
           call x3d_error(isubname,'get_info vector attribute')
           go to 9999
        endif
 
        do i = 1,nlength
            idx = (i-1)*irank + 1
            xvec(i) = value(idx)
            idx = (i-1)*irank + 2
            yvec(i) = value(idx)
            idx = (i-1)*irank + 3
            zvec(i) = value(idx)
        enddo
 
        ierror_return = 0
 
C.... keyword voronoi
C     Fill x,y,z element attributes with voronoi points
      elseif (fill_type(1:flen) .eq. 'voronoi' .or.
     *        fill_type(1:flen) .eq. 'median' ) then
 
c       create the remaining two attributes
        do i = 2,3
         cmsgin(4)= att_list(i)
         call cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *             nwdsout,ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *       'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           endif
          endif
         enddo
 
        call cmo_get_info(att_list(1),cmo_name,ipxvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(2),cmo_name,ipyvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(3),cmo_name,ipzvec,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info new attribute')
 
        call cmo_get_intinfo('nelements',cmo_name,nlength,i,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')
 
        if (fill_type(1:flen) .eq. 'voronoi') then
c
c         calculate voronoi points and fill the element attributes
c
          do idx=1,nlength
             ioff=itetoff(idx)
             ityp=itettyp(idx)
             if (ityp.eq.ifelmtet) then
                call voronoi_center(
     *          xic(itet(ioff+1)),yic(itet(ioff+1)),zic(itet(ioff+1)),
     *          xic(itet(ioff+2)),yic(itet(ioff+2)),zic(itet(ioff+2)),
     *          xic(itet(ioff+3)),yic(itet(ioff+3)),zic(itet(ioff+3)),
     *          xic(itet(ioff+4)),yic(itet(ioff+4)),zic(itet(ioff+4)),
     *          xvec(idx),yvec(idx),zvec(idx),rvalue)
             elseif(ityp.eq.ifelmtri) then
                call voronoi_center_2d(
     *          xic(itet(ioff+1)),yic(itet(ioff+1)),zic(itet(ioff+1)),
     *          xic(itet(ioff+2)),yic(itet(ioff+2)),zic(itet(ioff+2)),
     *          xic(itet(ioff+3)),yic(itet(ioff+3)),zic(itet(ioff+3)),
     *          xvec(idx),yvec(idx),zvec(idx) )
             else
               logmess='Voronoi: element not triangle or tetrahedron'
               call writloga('default',0,logmess,0,ierr)
               ierror_return = -1
               go to 9999
             endif
          enddo
 
c       fill_type(1:flen) .eq. 'median'
        else
c
c         calculate median points
c
          do idx=1,nlength
             ioff=itetoff(idx)
             ityp=itettyp(idx)
             nen = nelmnen(ityp)
             xsum = 0.0
             ysum = 0.0
             zsum = 0.0
             do i = 1, nen
               j = itet(ioff + i)
               xsum = xsum + xic(j)
               ysum = ysum + yic(j)
               zsum = zsum + zic(j)
             enddo
             xvec(idx) = xsum/float(nelmnen(ityp))
             yvec(idx) = ysum/float(nelmnen(ityp))
             zvec(idx) = zsum/float(nelmnen(ityp))
          enddo
        endif
 
        ierror_return = 0
C
C.... keyword xyz_rtz or xyz_rtp
C
C     Fill node attribues with spherical (r,theta,phi) or cylindrical (r,theta,z)
C
      elseif (fill_type(1:flen) .eq. 'xyz_rtp' .or.
     *        fill_type(1:flen) .eq. 'xyz_rtz' ) then
 
c       create the remaining two attributes
        do i = 2,3
         cmsgin(4)= att_list(i)
         call cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *             nwdsout,ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *       'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           endif
          endif
         enddo
 
        call cmo_get_info(att_list(1),cmo_name,ipxvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(2),cmo_name,ipyvec,ilen,ityp,ierr)
        call cmo_get_info(att_list(3),cmo_name,ipzvec,ilen,ityp,ierr)
        if(ierr.ne.0) call x3d_error(isubname,'get_info new attribute')
       
        call cmo_get_intinfo('nnodes',
     *         cmo_name,nlength,ilen,ityp,ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')
 
 
        if (fill_type(1:flen) .eq. 'xyz_rtp') then
c
c         calculate voronoi points and fill the element attributes
c
         do idx=1,nlength
         xvec(idx) = sqrt(xic(idx)**2 + yic(idx)**2 + zic(idx)**2 )
         call angle3v(zero,zero,zero,
     1                xic(idx),yic(idx),zic(idx),
     2                yvec(idx),zvec(idx))
         enddo
 
c       fill_type(1:flen) .eq. 'xyz_rtz'
        else
c
c         calculate rtz coordinates
c
        do idx=1,nlength
         xvec(idx) = sqrt( xic(idx)**2 + yic(idx)**2 )
         call angle3v(zero,zero,zero,
     1                xic(idx),yic(idx),zic(idx),
     2                yvec(idx),zvec(idx))
         zvec(idx) = zic(idx)
        enddo
       endif
C
C     Fill node attribtue with number of edge connections to each node.
C
      elseif((fill_type(1:flen) .eq. 'edge_connections') .or.
     1       (fill_type(1:flen) .eq. 'node_num_diff'))then
C     PURPOSE -
C
C        Get the parent mass point - parent mass point relation.
C        That is, we compile the list of edges whose endpoints
C        are both in the mass point array MPARY.  It is assumed
C        that MPARY only contains parent nodes.
C
C     INPUT ARGUMENTS -
C
C        MPARY  - Mass point array.
C        NNODES    - No. of nodes in the mesh
C        NELEMENTS - No. of elements in the mesh
C        ITET      - The element-node relation
C        ITETOFF   - Offset array for ITET
C        ITETTYP   - Array of element types
C        IPARENT   - Array giving parent nodes
C        ISUBNAME1 - Partition that output IEDGES array belongs to.
C        INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
          call cmo_get_intinfo('nnodes',
     1                          cmo_name,nnode,ilen,ityp,ierr)
          call cmo_get_intinfo('nelements',
     1                          cmo_name,nelem,ilen,ityp,ierr)
      
        call cmo_get_info('itettyp',cmo_name,ipitettyp,ilen,ityp,ierr)
        call cmo_get_info('itetoff',cmo_name,ipitetoff,ilen,ityp,ierr)
        call cmo_get_info('itet',cmo_name,ipitet,ilen,ityp,ierr)
        call cmo_get_info('itp1',cmo_name,ipitp,ilen,ityp,ierr)
        call cmo_get_info('isn1',cmo_name,ipisn,ilen,ityp,ierr)
        call cmo_get_info(att_name,cmo_name,ipivalue,ilen,ityp,ierr)
        call mmgetblk('iparent',
     1                 isubname,ipiparent,nnode,1,ierr)
        call mmgetblk('mpary',
     1                 isubname,ipmpary,nnode,1,ierr)
        call mmgetblk('iedges_first',
     1                 isubname,ipiedges_first,12*nnode,1,ierr)
        call unpackpc(nnode,itp,isn,iparent)
        do i = 1, nnode
           mpary(i) = i
        enddo
        inclusive = 1

        call getedges_d(mpary,nnode,nnode,nelem,itet,itetoff,
     &   itettyp,iparent,isubname,inclusive,ipiedges,iedges_first)

        if(fill_type(1:flen) .eq. 'edge_connections')then
        do i = 1, nnode
           ivalue(i) = iedges_first(i+1) - iedges_first(i)
        enddo
        elseif(fill_type(1:flen) .eq. 'node_num_diff')then
C
C       Compute the max difference in node numbers between
C       node i and all nodes connected to i. 
        do i = 1, nnode
           k = iedges_first(i)
           ij_max = 0
           do j = 1, iedges_first(i+1) - iedges_first(i)
              jn = iedges(k+j-1)
              ij_max = max(ij_max, abs(jn-i))
           enddo
           ivalue(i) = ij_max
        enddo
        endif

        logmess='cmo/printatt/'//cmo_name//att_name//'/minmax ; finish'
        call dotask(logmess, ierr)

      elseif(fill_type(1:flen) .eq. 'quad_quality') then
         call cmo_get_intinfo('nelements',cmo_name,nlength,ilen,ityp,
     *      ierr)
         if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements ')

C
C        Create the remaining two attributes
C
         cmsgin(4) = att_list(2)
         call cmo_addatt_cmo(imsgin, xmsgin, cmsgin, msgtype, nwdsout,
     *     ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *        'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           endif
         endif

         cmsgin(4) = att_list(3)
         cmsgin(5) = 'VINT'
         call cmo_addatt_cmo(imsgin, xmsgin, cmsgin, msgtype, nwdsout,
     *     ierror_return)
         if(ierror_return.eq.0) then
           call cmo_verify_cmo(cmo_name,ierror_return)
           if(ierror_return.ne.0) then
              write(logmess,'(a,a,a)')'ADDATT error: ',
     *        'Mesh Object is not consistent: ', cmo_name
              call writloga('default',0,logmess,0,ierr)
           endif
         endif

C
C        Access the three new attributes
C
         call cmo_get_info(att_list(1),cmo_name,ipvalue,ilen,ityp,ierr)
         if(ierr.ne.0) call x3d_error(isubname,
     *      'get_info new attribute')

         call cmo_get_info(att_list(2),cmo_name,ipregularity,ilen,ityp,
     *      ierr)
         if(ierr.ne.0) call x3d_error(isubname,
     *      'get_info new attribute')

         call cmo_get_info(att_list(3),cmo_name,ipquadflag,ilen,ityp,
     *      ierr)
         if(ierr.ne.0) call x3d_error(isubname,
     *      'get_info new attribute')

C
C        Verify that the mesh consists solely of quads.
C
         do i = 1, nlength
             if (itettyp(i) .ne. ifelmqud) then
                 write(logmess,'(a, a)')
     *           'ADDATT error: quad_quality requires the mesh to ',
     *           'consist solely of quads.'
                 call writloga('default',0,logmess,0,ierr)
                 ierror_return = -1
                 goto 9999
             endif
         enddo

         if (nlength .ne. 0) then
            call quad_quality(itet(1), nlength, xic(1), yic(1), zic(1),
     *              value(1), regularity(1), quadflag(1))
         endif
      endif
 
        ierror_return = 0
 
 
C     End filling new or old attribute from keyword selection
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C
      idone = 1
      ierror_return = 0
9999  if(idone.ne.1) then
        write(logmess,'(a,i5)')
     *   'ADDATT ERRORS BEFORE COMPLETION ierror_return = ',
     *   ierror_return
        call writloga('default',1,logmess,1,ierr)
      endif
      call mmrelprt(isubname,ierr)

9998  ierror_return = 0
      return
      end
