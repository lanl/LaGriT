      subroutine surface(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE DEFINES A BOUNDARY SURFACE OF THE SPECIFIED TYPE.
C        THE SURFACE TYPE IS SPECIFIED BY istype AND DESCRIBED
C        BY x1 to z4.
C
C
C     FORMAT: SURFACE/ISURNAME/IBTYPE/ISTYPE/X1/Y1/Z1/X2/Y2/Z2/
C                                          X3/Y3/Z3/X4/Y4/Z4
C      SPECIFICALLY:
C        SURFACE/ISURNAME/IBTYPE/plane/x1,y1,z1/x2,y2,z2/x3,y3,z3
C        SURFACE/isurname/IBTYPE/PLANEXYZ/x1,y1,z1/x2,y2,z2/x3,y3,z3
C        SURFACE/isurname/IBTYPE/PLANERTZ/radius1,theta1,z1,
C                                         radius2,theta2,z2,
C                                         radius3,theta3,z3,
C                                         xcen,ycen
C        SURFACE/isurname/IBTYPE/PLANERTP/radius1,theta1,phi1,
C                                         radius2,theta2,phi2,
C                                         radius3,theta3,phi3,
C                                         xcen,ycen,zcen
C        SURFACE/ISURNAME/IBTYPE/box/xmin,ymin,zmin/xmax,ymax,zmax
C        SURFACE/ISURNAME/IBTYPE/paralell/x1,y1,z1/x2,y2,z2/x3,y3,z3/
C                                         x4,y4,z4
C             WHERE POINTS 1, 2,  3 ARE THE FRONT LEFT, FRONT RIGHT
C                   AND BACK LEFT POINTS OF THE BASE AND POINT 4 IS
C                   THE UPPER LEFT POINT OF THE FRONT FACE.
C        SURFACE/ISURNAME/IBTYPE/sphere/xcen,ycen,zcen/radius
C        SURFACE/ISURNAME/IBTYPE/cylinder/x1,y1,z1/x2,y2,z2/radius
C             WHERE POINT 1 IS THE BOTTOM CENTER AND POINT 2 IS THE TOP
C                   CENTER OF THE CYLINDER.
C        SURFACE/ISURNAME/IBTYPE/cone/x1,y1,z1/x2,y2,z2/radius
C             WHERE POINT 1 IS THE VERTEX AND POINT 2 IS THE TOP
C                   CENTER OF THE CONE WITH RADIUS FROM THIS POINT.
C        SURFACE/ISURNAME/IBTYPE/ellipse/x1,y1,z1/x2,y2,z2/x3,y3,z3/
C                                        ar,br,cr
C             WHERE POINT 1 IS THE CENTER OF THE ELLIPSOID, POINT 2 IS
C                   ON THE a SEMI-AXIS, POINT 3 IS ON THE b SEMI-AXIS,
C                   AND ar, br, cr ARE RADII ALONG THEIR RESPECTIVE
C                   SEMI-AXES.
C        SURFACE/ISURNAME/IBTYPE/tabular/x1,y1,z1/x2,y2,z2/igeom
C                r1,z1, r2,z2, r3,z3,....
C                ...., rn,zn, end
C                       OR
C                r1,theta1, r2,theta2, r3,theta3,....
C                ...., rn,thetan, end
C             WHERE POINT 1 AND POINT 2 DEFINE THE AXIS OF ROTAION
C                   FOR THE TABULAR PROFILE WITH POINT 1 AS THE ORIGIN.
C                   THIS IS FOLLOWED BY PAIRS OF PROFILE DESCRIPTORS
C                   DEPENDING ON THE VALUE OF igeom.  IF igeom IS SET
C                   TO rz THEN THE R VALUE IS A RADIUS NORMAL TO THE
C                   AXIS OF ROTAION AND Z IS THE DISTANCE ALONG THE
C      REFLECTED BOUNDARY-POINT INFORMATION.
C                   NEW AXIS OF ROTATION.  IF igeom IS SET TO rt
C                   THEN THETA IS THE ANGLE FROM THE AXIS OF ROTATION
C                   AT POINT 1 AND R IS THE DISTANCE FROM POINT 1 ALONG
C                   THETA.  THE FIRST PAIR MUST START ON A NEW LINE AND
C                   ALL LINES MUST CONTAIN PAIRS OF DATA.  THE LAST PAIR
C                   OF DATA MUST BE FOLLOWED BY end.
C        SURFACE/ISURNAME/IBTYPE/sheet/nx/ny
C                 x1 y1 z1
C                 x2 y2 z2
C                 .
C                 xn yn zn
C                    This format is useful for reading a file that
C                    is a set of xyz triplets with the SURFACE command
C                    as the first line of the file. The file can be
C                    read in with the INPUT / filename command.
C
C        SURFACE/ISURNAME/IBTYPE/sheet/x1,y1,z1/x2,y2,z2/x3,y3,z3/igeom/
C                                      nx/ny
C                x1,y1,z1, x2,y2,z2, x3,y3,z3, ...
C                ...., xn,yn,zn, end
C                       OR
C                theta1,z1,r1, theta2,z2,r2, theta3,z3,r3, ...
C                ...., thetan,zn,rn, end
C                       OR
C                theta1,phi1,r1, theta2,phi2,r2, theta3,phi3,r3, ...
C                ...., thetan,phin,rn, end
C                       OR
C                points/ipt1,ipt2,ipt3
C             SHEET SURFACES ARE OPEN SURFACES THAT ARE DEFINED BY A
C             LOGICAL GRID WITH nx*ny GRID POINTS.  nx IS THE NUMBER OF
C             ROWS AND ny IS THE NUMBER OF COLUMNS IN THE GRID.  ALL
C             DATA IS ENTERED IN ROW-WISE ORDER AND THE TRIPLETS DEF-
C             INING EACH POINT IS OF THE TYPE AND ORDER SET IN igeom.
C             THE ALLOWABLE igeom VALUES ARE 'xyz', 'tzr' and 'tpr'.
C             THE 3 INPUT POINTS (x1 TO z3) DEFINE A NEW COORDINATE
C             SYSTEM.  POINT 1 (x1,y1,z1) IS THE NEW ORIGIN, THE LINE
C             BETWEEN POINTS 1 AND 2 IS THE NEW X-AXIS AND POINT 3
C             LIES ON THE NEW XY PLANE.  THE NEW Z-AXIS IS NORMAL TO
C             THE NEW XY PLANE.
C             WHEN igeom IS xyz THEN THE Z VALUES ARE MEASURDED FROM
C                   THE NEW XY PLANE.  THERE CAN ONLY BE ONE Z VALUE
C                   PER XY PAIR.  ANYTHING BELOW THE SHEET WITHIN
C                   THE GRID BOUNDS IS INSIDE THE SURFACE.
C             WHEN igeom IS tzr THEN THE r VALUE IS MEASURED FROM
C                   THE NEW Z AXIS, z IS MEASURED FROM THE ORIGIN
C                   AND theta IS MEASURED FROM THE NEW XZ PLANE.
C                   r VALUES MUST BE POSITIVE AND THERE CAN ONLY BE
C                   ONE PER tz PAIR.  ANYTHING BETWEEN THE REFERENCE
C                   LINE AND THE SHEET IS INSIDE THE SURFACE.
C             WHEN igeom IS tpr THEN THE r VALUE IS MEASURED FROM
C                   THE REFERNCE POINT, theta IS MEASURED FROM THE NEW
C                   X-AXIS AND phi IS MEASURED FROM THE NEW Z-AXIS.
C                   r VALUES MUST BE POSITIVE AND THERE CAN ONLY BE
C                   ONE PER tp PAIR.  ANYTHING BETWEEN THE REFERENCE
C                   POINT AND THE SHEET IS INSIDE THE SURFACE.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C        THESE ARE CONVERTED TO THE FOLLOWING:
C
C           isurnam - NAME FOR THIS SURFACE USED AS PATH TO STORAGE BLOCK
C           ibtype  - TYPE OF BOUNDARY: FREE, INTRFACE, REFLECT,
C                                    VIRTUAL OR INTRCONS
C           istype  - TYPE OF SURFACE: PLANE, SPHERE, CYLINDER, CONE,
C                                  BOX, PARALLELpiped
C           x1--z4  - PHYSICAL DESCRIPTION OF THE SURFACE, 4 POINTS
C                     FOR A PLANE, CENTER POINT AND RADIUS FOR A SPHERE,
C                     MIN. AND MAX. POINTS FOR A BOX, 4 POINTS FOR A
C                     PARALLELPIPED, 2 END CENTER POINTS AND RADIUS FOR
C                     A CYLINDER, 2 END CENTER POINTS AND 2 RADII FOR A
C                     CONE.
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/surface_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.12   23 Mar 2001 15:02:02   dcg
CPVCS    fix error with cube type - save at correct offset
CPVCS    
CPVCS       Rev 1.11   18 Apr 2000 13:24:34   dcg
CPVCS    implement release option for surfaces, regions and mregions
CPVCS
CPVCS       Rev 1.10   13 Apr 2000 16:50:16   dcg
CPVCS    add release surface option
CPVCS
CPVCS       Rev 1.9   Wed Apr 05 16:34:00 2000   dcg
CPVCS    allow 'interface' as well as 'intrface' for surface type
CPVCS
CPVCS       Rev 1.8   Wed Apr 05 13:35:08 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.7   20 Mar 2000 13:42:04   dcg
CPVCS    check for duplicate name - if so then print warning and skip command
CPVCS
CPVCS       Rev 1.6   24 Feb 2000 12:57:22   dcg
CPVCS    use geom_name when increasing mm surface definition block
CPVCS
CPVCS       Rev 1.5   24 Feb 2000 11:16:36   dcg
CPVCS    use type=3 for character arrays
CPVCS
CPVCS       Rev 1.4   Wed Feb 02 11:55:44 2000   dcg
CPVCS
CPVCS       Rev 1.3   13 Jan 2000 14:49:28   dcg
CPVCS
CPVCS       Rev 1.2   06 Jan 2000 12:55:16   dcg
CPVCS
CPVCS       Rev 1.35   Wed Nov 10 09:35:08 1999   dcg
CPVCS    remove test on miscellaneous storage block
CPVCS
CPVCS       Rev 1.34   Fri Sep 03 15:48:16 1999   dcg
CPVCS    tabular surface data read in as part of command
CPVCS
CPVCS       Rev 1.33   Fri Apr 17 10:09:48 1998   gable
CPVCS    Add some comments about the /sheet/nx/ny option.
CPVCS
CPVCS       Rev 1.32   Fri Oct 31 10:50:32 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.31   Mon Apr 14 17:02:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.30   Tue Feb 04 10:45:04 1997   dcg
CPVCS    normalize a,b,c,d representation of plane such that d=+/-1.0
CPVCS
CPVCS       Rev 1.29   Wed May 29 08:43:24 1996   het
CPVCS    Fix an error when checking input arguments for tabular.
CPVCS
CPVCS       Rev 1.28   Thu May 23 08:49:26 1996   dcg
CPVCS    use icscode to test for sb existence
CPVCS
CPVCS       Rev 1.27   Thu May 16 10:28:30 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.26   11/07/95 17:26:52   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.25   10/18/95 17:11:20   het
CPVCS    Fix an error with nstbout
CPVCS
CPVCS       Rev 1.24   10/18/95 12:17:08   het
CPVCS    Allow greater that 8 character names for sheets in the surface command.
CPVCS
CPVCS       Rev 1.23   09/19/95 13:09:58   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.22   08/29/95 12:10:56   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.21   08/28/95 11:32:04   ahmed
CPVCS    Adjust the location of mmrelprt in the routine
CPVCS
CPVCS       Rev 1.20   08/25/95 15:39:58   dcg
CPVCS    select 'active' mesh object when finished with sheet
CPVCS
CPVCS       Rev 1.19   08/23/95 16:05:16   dcg
CPVCS    changes for sheet routines as mesh objects
CPVCS
CPVCS       Rev 1.18   08/23/95 06:59:04   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.17   08/22/95 06:51:04   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.16   06/13/95 12:34:20   ejl
CPVCS    Fixed error with input
CPVCS
CPVCS       Rev 1.15   06/13/95 09:02:52   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.14   06/07/95 15:31:54   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.13   05/12/95 11:40:22   ejl
CPVCS    Put error checking in SURFACE
CPVCS
CPVCS       Rev 1.12   05/01/95 08:34:16   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.11   03/31/95 09:10:30   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.10   03/30/95 05:00:52   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.9   03/23/95 15:08:38   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.8   02/20/95 15:54:40   ahmed
CPVCS    Double the memory length for 'sheet'  to handle NURBS
CPVCS
CPVCS       Rev 1.6   02/17/95 19:14:16   het
CPVCS    Adjust memory for tri and quad cmo
CPVCS
CPVCS       Rev 1.5   02/12/95 08:41:00   het
CPVCS    Add the quad_cmo and tri_cmo functions.
CPVCS
CPVCS       Rev 1.4   01/25/95 16:11:36   dcg
CPVCS     Activate sheet surface type
CPVCS
CPVCS       Rev 1.3   01/04/95 22:05:34   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   12/24/94 10:52:16   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'geom_lg.h'
      include 'chydro.h'
      include 'consts.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,lenigeom,iout,i,istrt,lengthsparam,ntab,iend,
     *  ickrfl,lenibtype,lenistype,lenisurfnm,j
      integer icharlnf,length,ierrw,lout,itype,n
      pointer (ipout,out)
      real*8 out(*)
      real*8 ax,cz,bz,az,dist,xc,yc,zc,r,xdiff,ydiff,zdiff,dx,dy,dz,
     *  x4,y4,z4,x2,y2,z2,x3,y3,z3,x1,y1,z1,x4in,y4in,z4in,
     *  xmax1,ymax1,zmax1,xmx,ymx,zmx,xmin1,ymin1,zmin1,ai,bi,ci,
     *  a,b,c,d,distx,disty,aty,bty,denom,numer,angle,
     *  cty,theta,thetain,rr,zz,bx,cx,ay,by,cy,xmn,ymn,zmn,
     *  absofd,phi1,phi2,phi3,xcen,ycen,zcen,theta3,radius3,theta2,
     *  radius2,theta1,radius1,x1in,y1in,z1in,x2in,y2in,z2in,x3in,
     *  y3in,z3in
C
C#######################################################################
C
      REAL*8 x(3,6), y(3,6), z(3,6),rout
 
C
      character*32 isurfnm,geom_name
      character*32 ibtypein, istypein, igeom, cmosheet
      character*32 isubname, cmo
C
C
      character*132 logmess
      integer ierr
      real*8 eps
C
C#######################################################################
C
C
C
      isubname='surface'
C
      ierror = 0
C
C     Get length epsilon
C
      call get_epsilon('epsilonl',eps)
C
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_name')
       call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C     GET NAME AND TYPE INFORMATION
C
      isurfnm = cmsgin(2)
      ibtypein  = cmsgin(3)
      if(ibtypein.eq.'interface') ibtypein='intrface'
      istypein  = cmsgin(4)
C
      lenisurfnm = icharlnf(isurfnm)
      lenibtype  = icharlnf(ibtypein)
      lenistype  = icharlnf(istypein)
c
c  make sure we have enough space
c
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      if (ierror.ne.0 ) then
         length=100
         call mmgetblk('csall',geom_name,ipcsall,length,3,ierror)
         call mmgetblk('istype',geom_name,ipistype,length,3,ierror)
         call mmgetblk('ibtype',geom_name,ipibtype,length,3,ierror)
         call mmgetblk('sheetnm',geom_name,ipsheetnm,length,3,ierror)
         call mmgetblk('surfparam',geom_name,ipsurfparam,20*length,
     *          2,ierror)
         call mmgetblk('offsparam',geom_name,ipoffsparam,length,1,
     *          ierror)
      elseif(length.lt.nsurf+1) then
         call mmincblk('csall',geom_name,ipcsall,100,ierror)
         call mmincblk('istype',geom_name,ipistype,100,ierror)
         call mmincblk('ibtype',geom_name,ipibtype,100,ierror)
         call mmincblk('sheetnm',geom_name,ipsheetnm,100,ierror)
         call mmincblk('offsparam',geom_name,ipoffsparam,100,ierror)
      else
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      endif
c
c  check for release or delete
c
      if(ibtypein.eq.'release'.or.ibtypein.eq.'delete'.or.
     *   ibtypein.eq.'remove') then
         do i=1,nsurf
            if(csall(i).eq.isurfnm) then
               if(ibtype(i).eq.'reflect'.or.ibtype(i).eq.'intrcons')
     *                 call condel(i)
               if(i.lt.nsurf) then
                  length=offsparam(i+1)-offsparam(i)
                  do j=offsparam(i),lastsparam-length
                     surfparam(j+1)=surfparam(j+length+1)
                  enddo
                  do j= i+1,nsurf
                     offsparam(j)=offsparam(j)-length
                  enddo
                  do j=i,nsurf-1
                     csall(j)=csall(j+1)
                     istype(j)=istype(j+1)
                     ibtype(j)=ibtype(j+1)
                     sheetnm(j)=sheetnm(j+1)
                     offsparam(j)=offsparam(j+1)
                  enddo
                  lastsparam=lastsparam-length
               else
                  lastsparam=0
                  if(nsurf.gt.1)lastsparam=offsparam(nsurf)
                  offsparam(nsurf)=0
                  csall(nsurf)=' '
               endif
               nsurf=nsurf-1
               go to 9999
            endif
         enddo
      endif
C
C     ******************************************************************
C     CHECK FOR VALID BOUNDARY TYPES.
C
      if ((ibtypein(1:lenibtype) .ne. 'free'    ) .and.
     &    (ibtypein(1:lenibtype) .ne. 'intrface') .and.
     &    (ibtypein(1:lenibtype) .ne. 'intrcons') .and.
     &    (ibtypein(1:lenibtype) .ne. 'virtual') .and.
     &    (ibtypein(1:lenibtype) .ne. 'reflect' )) then
C
C        ***************************************************************
C        ILLEGAL BOUNDARY TYPE.
C
         write(logmess,9000) ibtypein
 9000    format(' ERROR - Illegal Boundary Type: ', a)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
C
      endif
c
c  check for illegal surface type
c
      if ((istypein(1:lenistype) .ne. 'plane'   ) .and.
     &    (istypein(1:lenistype) .ne. 'planexyz') .and.
     &    (istypein(1:lenistype) .ne. 'planertz') .and.
     &    (istypein(1:lenistype) .ne. 'planertp') .and.
     &    (istypein(1:lenistype) .ne. 'box'     ) .and.
     &    (istypein(1:lenistype) .ne. 'cone'    ) .and.
     &    (istypein(1:lenistype) .ne. 'cylinder') .and.
     &    (istypein(1:lenistype) .ne. 'ellipse')  .and.
     &    (istypein(1:lenistype) .ne. 'parallel') .and.
     &    (istypein(1:lenistype) .ne. 'sphere'  ) .and.
     &    (istypein(1:lenistype) .ne. 'tabular' ) .and.
     &    (istypein(1:lenistype) .ne. 'sheet'   )) then
         write(logmess,9120) istypein
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
c
c  increment number of surfaces
c
      nsurf=nsurf+1
 
c
c  check for duplicate names - if duplicate warn and ignore
c
      n=nsurf-1
      do i=1,n
         if(csall(i).eq.isurfnm) then
            nsurf=nsurf-1
            write(logmess,'(a,a)') 'duplicate surface - ignored: '
     *           ,isurfnm
            call writloga('default',0,logmess,0,ierror)
            go to 9999
         endif
      enddo
c
c  store names and types and offset
c
      istype(nsurf)=istypein(1:lenistype)
      ibtype(nsurf)=ibtypein(1:lenibtype)
      csall(nsurf)=isurfnm(1:lenisurfnm)
      if(nsurf.eq.1) lastsparam=0
      offsparam(nsurf)=lastsparam
c
c  get length of surfparam for later checks
c
      call mmfindbk('surfparam',geom_name,ipsurfparam,lengthsparam
     *      ,ierror)
C
      ickrfl=0
C
      if ((istypein(1:lenistype) .eq. 'plane'   ) .or.
     &    (istypein(1:lenistype) .eq. 'planexyz') .or.
     &    (istypein(1:lenistype) .eq. 'planertz') .or.
     &    (istypein(1:lenistype) .eq. 'planertp')) then
C
C        ***************************************************************
C        HANDLE PLANE TYPE SURFACE
C
         if ((istypein(1:lenistype) .eq. 'plane'   ) .or.
     *       (istypein(1:lenistype) .eq. 'planexyz')) then
C
            call test_argument_type(9,2,5,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            x1in=xmsgin(5)
            y1in=xmsgin(6)
            z1in=xmsgin(7)
C
            x2in=xmsgin(8)
            y2in=xmsgin(9)
            z2in=xmsgin(10)
C
            x3in=xmsgin(11)
            y3in=xmsgin(12)
            z3in=xmsgin(13)
C
         elseif(istypein(1:lenistype) .eq. 'planertz' ) then
C
            call test_argument_type(12,2,5,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            radius1=xmsgin(5)
            theta1 =xmsgin(6)*pie/180.0
            z1in   =xmsgin(7)
C
            radius2=xmsgin(8)
            theta2 =xmsgin(9)*pie/180.0
            z2in   =xmsgin(10)
C
            radius3=xmsgin(11)
            theta3 =xmsgin(12)*pie/180.0
            z3in   =xmsgin(13)
C
            xcen=xmsgin(14)
            ycen=xmsgin(15)
            zcen=xmsgin(16)
C
            x1in=radius1*cos(theta1)+xcen
            y1in=radius1*sin(theta1)+ycen
C
            x2in=radius2*cos(theta2)+xcen
            y2in=radius2*sin(theta2)+ycen
C
            x3in=radius3*cos(theta3)+xcen
            y3in=radius3*sin(theta3)+ycen
C
         elseif(istypein(1:lenistype) .eq. 'planertp' ) then
C
            call test_argument_type(12,2,5,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            radius1=xmsgin(5)
            theta1 =xmsgin(6)*pie/180.0
            phi1   =xmsgin(7)*pie/180.0
C
            radius2=xmsgin(8)
            theta2 =xmsgin(9)*pie/180.0
            phi2   =xmsgin(10)*pie/180.0
C
            radius3=xmsgin(11)
            theta3 =xmsgin(12)*pie/180.0
            phi3   =xmsgin(13)*pie/180.0
C
            xcen=xmsgin(14)
            ycen=xmsgin(15)
            zcen=xmsgin(16)
C
            x1in=radius1*sin(theta1)*cos(phi1)+xcen
            y1in=radius1*sin(theta1)*sin(phi1)+ycen
            z1in=radius1*cos(theta1)+zcen
C
            x2in=radius2*sin(theta2)*cos(phi2)+xcen
            y2in=radius2*sin(theta2)*sin(phi2)+ycen
            z2in=radius2*cos(theta2)+zcen
C
            x3in=radius3*sin(theta3)*cos(phi3)+xcen
            y3in=radius3*sin(theta3)*sin(phi3)+ycen
            z3in=radius3*cos(theta3)+zcen
C
         endif
C
C        ---------------------------------------------------------------
C        CONVERT INPUT POINTS FROM CURRENT COORDINATE SYSTEM TO
C        NORMAL COORDINATE SYSTEM.
C
         call xyznorm(x1in,y1in,z1in,x(1,1),y(1,1),z(1,1))
         call xyznorm(x2in,y2in,z2in,x(2,1),y(2,1),z(2,1))
         call xyznorm(x3in,y3in,z3in,x(3,1),y(3,1),z(3,1))
C
C        ---------------------------------------------------------------
C        SAVE TRANSPOSED INPUT DATA IN surfparam
C
         if(lengthsparam.lt.offsparam(nsurf)+13) then
            call mmincblk('surfparam',geom_name,ipsurfparam,500,ierr)
            lengthsparam=lengthsparam+500
         endif
         surfparam(offsparam(nsurf)+1)=x(1,1)
         surfparam(offsparam(nsurf)+2)=y(1,1)
         surfparam(offsparam(nsurf)+3)=z(1,1)
C
         surfparam(offsparam(nsurf)+4)=x(2,1)
         surfparam(offsparam(nsurf)+5)=y(2,1)
         surfparam(offsparam(nsurf)+6)=z(2,1)
C
         surfparam(offsparam(nsurf)+7)=x(3,1)
         surfparam(offsparam(nsurf)+8)=y(3,1)
         surfparam(offsparam(nsurf)+9)=z(3,1)
C
C        ---------------------------------------------------------------
C        SET UP THE EQUATION OF THE PLANE FROM THE 3 POINTS.
C
         a=  (y(2,1)-y(1,1))*(z(3,1)-z(1,1)) -
     &       (y(3,1)-y(1,1))*(z(2,1)-z(1,1))
         b=-((x(2,1)-x(1,1))*(z(3,1)-z(1,1)) -
     &       (x(3,1)-x(1,1))*(z(2,1)-z(1,1)))
         c=  (x(2,1)-x(1,1))*(y(3,1)-y(1,1)) -
     &       (x(3,1)-x(1,1))*(y(2,1)-y(1,1))
         d=a*x(1,1)+b*y(1,1)+c*z(1,1)
         if(abs(d).gt.eps ) then
            absofd=abs(d)
            a=a/absofd
            b=b/absofd
            c=c/absofd
            d=d/absofd
         endif
C
C        ---------------------------------------------------------------
C        SAVE COEFFICIENTS IN stbout
C        SAVE COEFFICIENTS IN stbout
C
         surfparam(offsparam(nsurf)+10)=a
         surfparam(offsparam(nsurf)+11)=b
         surfparam(offsparam(nsurf)+12)=c
         surfparam(offsparam(nsurf)+13)=d
         lastsparam=offsparam(nsurf)+13
C
C        ---------------------------------------------------------------
C        PRINT UNIT VECTOR FOR THE PLANE
C
         ai=a/sqrt(a*a + b*b + c*c)
         bi=b/sqrt(a*a + b*b + c*c)
         ci=c/sqrt(a*a + b*b + c*c)
C
         write(logmess,9060) isurfnm,ai,bi,ci
 9060    format('  The unit vector for ',a8,' is ',
     &          f10.7,'i  ',f10.7,'j  ',f10.7,'k')
         call writloga('default',0,logmess,0,ierrw)
C
C        ---------------------------------------------------------------
C        IF REFLECTIVE PLANE, SET UP BOUNDARY INFO
C
         if (ibtypein(1:lenibtype) .eq. 'reflect') then
C
            ickrfl=1
            nb=nb+1
            call flbound(1,nb,3,x(1,1),y(1,1),z(1,1),x(2,1),
     &                   y(2,1),z(2,1),x(3,1),y(3,1),z(3,1))
            call flbound(2,nb,3,zero,zero,zero,zero,zero,zero,zero,zero,
     &             zero)
C
         endif
C
      elseif ((istypein(1:lenistype) .eq. 'box'     ) .or.
     &        (istypein(1:lenistype) .eq. 'parallel')) then
C
C        ***************************************************************
C        HANDLE BOX OR PARALLELPIPED TYPE SURFACE
C
C        ---------------------------------------------------------------
C        IF BOX, CONVERT xmin, ymin TO x1 THROUGH z4 OF PARALLELPIPED
C
         if (istypein(1:lenistype) .eq. 'box') then
C
            if(lengthsparam.lt.offsparam(nsurf)+36) then
               call mmincblk('surfparam',geom_name,ipsurfparam,500,ierr)
               lengthsparam=lengthsparam+500
            endif
C
            call test_argument_type(6,2,5,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            xmn=xmsgin(5)
            ymn=xmsgin(6)
            zmn=xmsgin(7)
C
            xmx=xmsgin(8)
            ymx=xmsgin(9)
            zmx=xmsgin(10)
C
C           ............................................................
C           CONVERT INPUT DATA FROM LOCAL COORD. SYSTEM TO NORMAL
C           COORD. SYSTEM AND SAVE IN stbout
C           COORD. SYSTEM AND SAVE IN stbout
C
            call xyznorm(xmn,ymn,zmn,xmin1,ymin1,zmin1)
            call xyznorm(xmx,ymx,zmx,xmax1,ymax1,zmax1)
C
            surfparam(offsparam(nsurf)+1)=xmin1
            surfparam(offsparam(nsurf)+2)=ymin1
            surfparam(offsparam(nsurf)+3)=zmin1
C
            surfparam(offsparam(nsurf)+4)=xmax1
            surfparam(offsparam(nsurf)+5)=ymax1
            surfparam(offsparam(nsurf)+6)=zmax1
C
C           ............................................................
C           NOW CONVERT FROM BOX TO PARALLELPIPED IN CURRENT COORD. SYS.
C           THEN TRANSFORM THE CORNERS TO NORMAL COORD. SYSTEM.
C
            x1in=xmn
            y1in=ymn
            z1in=zmn
C
            x2in=xmx
            y2in=ymn
            z2in=zmn
C
            x3in=xmn
            y3in=ymx
            z3in=zmn
C
            x4in=xmn
            y4in=ymn
            z4in=zmx
C
            call xyznorm(x1in,y1in,z1in,x1,y1,z1)
            call xyznorm(x2in,y2in,z2in,x2,y2,z2)
            call xyznorm(x3in,y3in,z3in,x3,y3,z3)
            call xyznorm(x4in,y4in,z4in,x4,y4,z4)
C
         else
C
            call test_argument_type(14,2,5,imsgin,xmsgin,cmsgin,
     *                              msgtype,nwds)
            x1in=xmsgin(5)
            y1in=xmsgin(6)
            z1in=xmsgin(7)
C
            x2in=xmsgin(8)
            y2in=xmsgin(9)
            z2in=xmsgin(10)
C
            x3in=xmsgin(11)
            y3in=xmsgin(12)
            z3in=xmsgin(13)
C
            x4in=xmsgin(14)
            y4in=xmsgin(15)
            z4in=xmsgin(16)
C
C           ............................................................
C           CONVERT INPUT DATA FROM LOCAL COORD. SYSTEM TO NORMAL
C           COORD. SYSTEM AND SAVE IN stbout
C           COORD. SYSTEM AND SAVE IN stbout
C
            call xyznorm(x1in,y1in,z1in,x1,y1,z1)
            call xyznorm(x2in,y2in,z2in,x2,y2,z2)
            call xyznorm(x3in,y3in,z3in,x3,y3,z3)
            call xyznorm(x4in,y4in,z4in,x4,y4,z4)
C
            surfparam(offsparam(nsurf)+1)=x1
            surfparam(offsparam(nsurf)+2)=y1
            surfparam(offsparam(nsurf)+3)=z1
C
            surfparam(offsparam(nsurf)+4)=x2
            surfparam(offsparam(nsurf)+5)=y2
            surfparam(offsparam(nsurf)+6)=z2
C
 
            surfparam(offsparam(nsurf)+7)=x3
            surfparam(offsparam(nsurf)+8)=y3
            surfparam(offsparam(nsurf)+9)=z3
C
            surfparam(offsparam(nsurf)+10)=x4
            surfparam(offsparam(nsurf)+11)=y4
            surfparam(offsparam(nsurf)+12)=z4
C
         endif
C
C        ---------------------------------------------------------------
C        CALCULATE OFFSETS, XDIFF IS THE X DISTANCE BETWEEN POINTS
C        ALONG AN UPPER OR LOWER EDGE, DX IS THE X DISTANCE BETWEEN
C        POINTS ALONG A VERTICAL EDGE, ETC.
C
         dx=x4-x1
         dy=y4-y1
         dz=z2-z1
C
         xdiff=x2-x1
         ydiff=y3-y1
         zdiff=z4-z1
C
C        ---------------------------------------------------------------
C        SET UP 3 POINTS ON EACH OF THE 6 PLANES SO THAT THE RIGHT
C        HAND SYSTEM POINTS OUTWARD.
C
         x(1,1)=x1
         y(1,1)=y1
         z(1,1)=z1
C
         x(2,1)=x2
         y(2,1)=y2
         z(2,1)=z2
C
         x(3,1)=x4
         y(3,1)=y4
         z(3,1)=z4
C
         x(1,2)=x4+xdiff
         y(1,2)=y2+dy
         z(1,2)=z2+zdiff
C
         x(2,2)=x2
         y(2,2)=y2
         z(2,2)=z2
C
         x(3,2)=x3+xdiff
         y(3,2)=y2+ydiff
         z(3,2)=z3+dz
C
         x(1,3)=x(3,2)
         y(1,3)=y(3,2)
         z(1,3)=z(3,2)
C
         x(2,3)=x3
         y(2,3)=y3
         z(2,3)=z3
C
         x(3,3)=x3+dx
         y(3,3)=y3+dy
         z(3,3)=z3+zdiff
C
         x(1,4)=x1
         y(1,4)=y1
         z(1,4)=z1
C
         x(2,4)=x4
         y(2,4)=y4
         z(2,4)=z4
C
         x(3,4)=x3
         y(3,4)=y3
         z(3,4)=z3
C
         x(1,5)=x4
         y(1,5)=y4
         z(1,5)=z4
C
         x(2,5)=x(1,2)
         y(2,5)=y(1,2)
         z(2,5)=z(1,2)
C
         x(3,5)=x3+dx
         y(3,5)=y3+dy
         z(3,5)=z3+zdiff
C
         x(1,6)=x1
         y(1,6)=y1
         z(1,6)=z1
C
         x(2,6)=x3
         y(2,6)=y3
         z(2,6)=z3
C
         x(3,6)=x2
         y(3,6)=y2
         z(3,6)=z2
C
C        ---------------------------------------------------------------
C        LOOP THROUGH 6 PLANES TO GET EQUATIONS AND SAVE IN stbout
C        LOOP THROUGH 6 PLANES TO GET EQUATIONS AND SAVE IN stbout
C
         if (istypein(1:lenistype) .eq. 'box') then
            istrt=6
            lastsparam=lastsparam+6
         else
            istrt=12
            lastsparam=lastsparam+12
         endif
C
         do i=1,6
C
C           ............................................................
C           SET UP THE EQUATION OF THE PLANE FROM THE 3 POINTS.
C
            a=  (y(2,i)-y(1,i))*(z(3,i)-z(1,i)) -
     &          (y(3,i)-y(1,i))*(z(2,i)-z(1,i))
            b=-((x(2,i)-x(1,i))*(z(3,i)-z(1,i)) -
     &          (x(3,i)-x(1,i))*(z(2,i)-z(1,i)))
            c=  (x(2,i)-x(1,i))*(y(3,i)-y(1,i)) -
     &          (x(3,i)-x(1,i))*(y(2,i)-y(1,i))
            d=a*x(1,i)+b*y(1,i)+c*z(1,i)
            if(abs(d).gt.eps ) then
               absofd=abs(d)
               a=a/absofd
               b=b/absofd
               c=c/absofd
               d=d/absofd
            endif
C
C           ............................................................
C           SAVE COEFFICIENTS IN stbout
C           SAVE COEFFICIENTS IN stbout
C
            iout=istrt+(i-1)*4
            surfparam(offsparam(nsurf)+iout+1)=a
            surfparam(offsparam(nsurf)+iout+2)=b
            surfparam(offsparam(nsurf)+iout+3)=c
            surfparam(offsparam(nsurf)+iout+4)=d
C
C           ............................................................
C           IF REFLECTIVE PLANE, SET UP BOUNDARY INFO
C
            if (ibtypein(1:lenibtype) .eq. 'reflect') then
               ickrfl=1
               nb=nb+1
               call flbound(1,nb,3,x(1,i),y(1,i),z(1,i),x(2,i),
     &                      y(2,i),z(2,i),x(3,i),y(3,i),z(3,i))
               call flbound(2,nb,3,zero,zero,zero,zero,zero,zero,zero,
     &          zero,zero)
            endif
C
         enddo
C
         lastsparam=lastsparam+24
C
      elseif (istypein(1:lenistype) .eq. 'sphere') then
C
C        ***************************************************************
C        HANDLE SPHERE TYPE SURFACE
C
         call test_argument_type(4,2,5,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         x1in=xmsgin(5)
         y1in=xmsgin(6)
         z1in=xmsgin(7)
         r=xmsgin(8)
         if(lengthsparam.lt.offsparam(nsurf)+4) then
               call mmincblk('surfparam',geom_name,ipsurfparam,500,ierr)
               lengthsparam=lengthsparam+500
         endif
C
C        ---------------------------------------------------------------
C        CONVERT INPUT POINTS FROM CURRENT COORDINATE SYSTEM TO
C        NORMAL COORDINATE SYSTEM AND SAVE IN stbout
C        NORMAL COORDINATE SYSTEM AND SAVE IN stbout
C
         call xyznorm(x1in,y1in,z1in,xc,yc,zc)
C
         surfparam(offsparam(nsurf)+1)=xc
         surfparam(offsparam(nsurf)+2)=yc
         surfparam(offsparam(nsurf)+3)=zc
         surfparam(offsparam(nsurf)+4)=r
         lastsparam=lastsparam+4
C
      elseif ((istypein(1:lenistype) .eq. 'cylinder') .or.
     &        (istypein(1:lenistype) .eq. 'cone'    ) .or.
     &        (istypein(1:lenistype) .eq. 'tabular' )) then
C
C        ***************************************************************
C        HANDLE CYLINDER, CONE AND ROTATED TABULAR PROFILE TYPE SURFACE
C
         if ((istypein(1:lenistype) .eq. 'cylinder') .or.
     &       (istypein(1:lenistype) .eq. 'cone'    )) then
             call test_argument_type(7,2,5,imsgin,xmsgin,cmsgin,
     *                               msgtype,nwds)
         elseif (istypein(1:lenistype) .eq. 'tabular' ) then
             call test_argument_type(6,2,5,imsgin,xmsgin,cmsgin,
     *                               msgtype,nwds)
         endif
         if(lengthsparam.lt.offsparam(nsurf)+14) then
               call mmincblk('surfparam',geom_name,ipsurfparam,500,ierr)
               lengthsparam=lengthsparam+500
         endif
         x1in=xmsgin(5)
         y1in=xmsgin(6)
         z1in=xmsgin(7)
C
         x2in=xmsgin(8)
         y2in=xmsgin(9)
         z2in=xmsgin(10)
C
         if (istypein(1:lenistype) .ne. 'tabular') then
            r=xmsgin(11)
         else
            igeom=cmsgin(11)
            lenigeom = icharlnf(igeom)
         endif
C
C        ---------------------------------------------------------------
C        CONVERT INPUT DATA FROM LOCAL COORD. SYSTEM TO NORMAL
C        COORD. SYSTEM AND SAVE IN stbout
C        COORD. SYSTEM AND SAVE IN stbout
C
         call xyznorm(x1in,y1in,z1in,x1,y1,z1)
         call xyznorm(x2in,y2in,z2in,x2,y2,z2)
C
         surfparam(offsparam(nsurf)+1)=x1
         surfparam(offsparam(nsurf)+2)=y1
         surfparam(offsparam(nsurf)+3)=z1
C
         surfparam(offsparam(nsurf)+4)=x2
         surfparam(offsparam(nsurf)+5)=y2
         surfparam(offsparam(nsurf)+6)=z2
C
         surfparam(offsparam(nsurf)+7)=xmsgin(11)
C
C        ---------------------------------------------------------------
C        ROTATE THE AXES SO THAT THE LINE BETWEEN POINT 1 AND POINT 2 IS
C        THE NEW Z AXIS
C
         dist=sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE INPUT POINTS (NEW Z AXIS)
C
         az=(x2-x1)/dist
         bz=(y2-y1)/dist
         cz=(z2-z1)/dist
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW X AXIS WHICH LIES ON
C        THE OLD X-Y PLANE
C
         if ((az*az+bz*bz) .eq. 0) then
            ax=1
            bx=0
         else
            ax=sqrt(bz*bz/(az*az+bz*bz))
            bx=-sqrt(az*az/(az*az+bz*bz))
         endif
C
         cx=0
         if (az .lt. 0 .and. bz .gt. 0) bx=-bx
         if (az .lt. 0 .and. bz .lt. 0) then
            ax=-ax
            bx=-bx
         endif
C
         if (az .gt. 0 .and. bz .lt. 0) ax=-ax
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW Y AXIS WHICH LIES
C        PERPENDICULAR TO THE NEW Z AND X AXIS (Z CROSS X)
C
         ay=bz*cx-bx*cz
         by=cz*ax-cx*az
         cy=az*bx-ax*bz
C
C        ---------------------------------------------------------------
C        STORE THE CENTER OR VERTEX POINT, THE END POINT, AND THE
C        RADIUS IN THE STORAGE BLOCK
C
         surfparam(offsparam(nsurf)+8)=x1
         surfparam(offsparam(nsurf)+9)=y1
         surfparam(offsparam(nsurf)+10)=z1
C
         surfparam(offsparam(nsurf)+11)=x2
         surfparam(offsparam(nsurf)+12)=y2
         surfparam(offsparam(nsurf)+13)=z2
C

C
C        ---------------------------------------------------------------
C        FOR A CONE, STORE dist FOR C IN THE EQUATION OF A CONE
C        FOR A CONE or cylinder store radius
c        For tabular store sheet name
C
         if (istypein(1:lenistype) .eq. 'cone') then
            surfparam(offsparam(nsurf)+14)=r
            surfparam(offsparam(nsurf)+15)=dist
            lastsparam=lastsparam+15
         elseif (istypein(1:lenistype) .eq. 'tabular') then
            sheetnm(nsurf)=igeom
            lastsparam=lastsparam+13
         else
            surfparam(offsparam(nsurf)+14)=r
            lastsparam=lastsparam+14
         endif
C
C        ---------------------------------------------------------------
C        FOR A ROTATED TABULAR SURFACE, GET AND STORE THE PROFILE PAIRS
C
         if (istypein(1:lenistype) .eq. 'tabular') then
C
            iend=0
            ntab=nwds-12
            if(lengthsparam.lt.offsparam(nsurf)+24+ntab) then
               call mmincblk('surfparam',geom_name,ipsurfparam,ntab+500,
     *                   ierr)
               lengthsparam=lengthsparam+500+ntab
            endif
C
C           ............................................................
C           LOOP THROUGH INPUT DATA UNTIL end FOUND
C
            do i=12,nwds-1
              surfparam(offsparam(nsurf)+i+4)=xmsgin(i)
            enddo
C
            surfparam(offsparam(nsurf)+15)=nwds-12
            lastsparam=lastsparam+2+ntab
C
C           ............................................................
C           CONVERT zr TO rz OR tr TO rt GEOMETRY
C
            if ((igeom(1:lenigeom).eq.'zr') .or.
     &          (igeom(1:lenigeom).eq.'tr')) then
C
               if (igeom(1:lenigeom) .eq. 'zr') igeom='rz'
               if (igeom(1:lenigeom) .eq. 'tr') igeom='rt'
C
               sheetnm(nsurf)=igeom
C
               do i=1,ntab,2
                  zz=surfparam(offsparam(nsurf)+15+i)
                  rr=surfparam(offsparam(nsurf)+15+i+1)
                  surfparam(offsparam(nsurf)+15+i)=rr
                  surfparam(offsparam(nsurf)+15+i+1)=zz
               enddo
C
            endif
C
C           ............................................................
C           CONVERT rt GEOMETRY TO rz GEOMETRY
C
            if (igeom(1:lenigeom) .eq. 'rt') then
C
               sheetnm(nsurf)='rz'
C
               do i=1,ntab,2
                  r=surfparam(offsparam(nsurf)+15+i)
                  thetain=surfparam(offsparam(nsurf)+15+i+1)
                  theta=thetain*pie/180.
C
                  if (thetain .eq. 0) then
                     surfparam(offsparam(nsurf)+15+i)=0
                     surfparam(offsparam(nsurf)+15+i+1)=r
                  elseif (thetain .eq. 90) then
                     surfparam(offsparam(nsurf)+15+i)=r
                     surfparam(offsparam(nsurf)+15+i+1)=0
                  elseif (thetain .eq. 180) then
                     surfparam(offsparam(nsurf)+15+i)=0
                     surfparam(offsparam(nsurf)+15+i+1)=-r
                  else
                     surfparam(offsparam(nsurf)+15+i)=r*sin(theta)
                     surfparam(offsparam(nsurf)+15+i+1)=r*cos(theta)
                  endif
               enddo
C
            endif
C
         endif
C
C        ---------------------------------------------------------------
C        STORE THE ROTATION VECTORS AS A MATRIX
C
         surfparam(lastsparam+1)=ax
         surfparam(lastsparam+2)=bx
         surfparam(lastsparam+3)=cx
C
         surfparam(lastsparam+4)=ay
         surfparam(lastsparam+5)=by
         surfparam(lastsparam+6)=cy
C
         surfparam(lastsparam+7)=az
         surfparam(lastsparam+8)=bz
         surfparam(lastsparam+9)=cz
C
         lastsparam=lastsparam+9
C
      elseif (istypein(1:lenistype) .eq. 'ellipse') then
C
C        ***************************************************************
C        HANDLE ELLIPSOID TYPE SURFACE
C
         call test_argument_type(12,2,5,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         if(lengthsparam.lt.offsparam(nsurf)+27) then
               call mmincblk('surfparam',geom_name,ipsurfparam,500,ierr)
               lengthsparam=lengthsparam+500
         endif
         x1in=xmsgin(5)
         y1in=xmsgin(6)
         z1in=xmsgin(7)
C
         x2in=xmsgin(8)
         y2in=xmsgin(9)
         z2in=xmsgin(10)
C
         x3in=xmsgin(11)
         y3in=xmsgin(12)
         z3in=xmsgin(13)
C
         a=xmsgin(14)
         b=xmsgin(15)
         c=xmsgin(16)
C
C        ---------------------------------------------------------------
C        CONVERT INPUT DATA FROM LOCAL COORD. SYSTEM TO NORMAL
C        COORD. SYSTEM AND SAVE IN stbout
C        COORD. SYSTEM AND SAVE IN stbout
C
         call xyznorm(x1in,y1in,z1in,x1,y1,z1)
         call xyznorm(x2in,y2in,z2in,x2,y2,z2)
         call xyznorm(x3in,y3in,z3in,x3,y3,z3)
C
         surfparam(offsparam(nsurf)+1)=x1
         surfparam(offsparam(nsurf)+2)=y1
         surfparam(offsparam(nsurf)+3)=z1
C
         surfparam(offsparam(nsurf)+4)=x2
         surfparam(offsparam(nsurf)+5)=y2
         surfparam(offsparam(nsurf)+6)=z2
C
         surfparam(offsparam(nsurf)+7)=x3
         surfparam(offsparam(nsurf)+8)=y3
         surfparam(offsparam(nsurf)+9)=z3
C
         surfparam(offsparam(nsurf)+10)=a
         surfparam(offsparam(nsurf)+11)=b
         surfparam(offsparam(nsurf)+12)=c
C
C        ---------------------------------------------------------------
C        ROTATE THE AXES SO THAT THE LINE BETWEEN POINT 1 AND POINT 2 IS
C        THE NEW X AXIS AND THE LINE BETWEEN 1 AND 3 IS THE NEW Y AXIS
C
         distx=sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)
         disty=sqrt((x3-x1)**2 + (y3-y1)**2 + (z3-z1)**2)
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE INPUT POINTS
C
         ax=(x2-x1)/distx
         bx=(y2-y1)/distx
         cx=(z2-z1)/distx
C
         aty=(x3-x1)/disty
         bty=(y3-y1)/disty
         cty=(z3-z1)/disty
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW Z AXIS WHICH LIES
C        PERPENDICULAR TO THE NEW X AND Y AXIS (X CROSS Y)
C
         az=bx*cty-bty*cx
         bz=cx*aty-cty*ax
         cz=ax*bty-aty*bx
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW Y AXIS WHICH LIES
C        PERPENDICULAR TO THE NEW Z AND X AXIS (Z CROSS X)
C
         ay=bz*cx-bx*cz
         by=cz*ax-cx*az
         cy=az*bx-ax*bz
C
C        ...............................................................
C        CHECK THAT THE ORIGINAL SEMI-AXIS WERE PERPENDICULAR.
C
         numer=ay*aty + by*bty + cy*cty
         denom=sqrt(ay**2+by**2+cy**2) * sqrt(aty**2+bty**2+cty**2)
         angle=acos(numer/denom)
C
         if (angle .gt. .08725) then
            write(logmess,9080)
 9080       format(' Warning - the semi-axis points are not ',
     &             'perpendicular.')
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,9100)
 9100       format('           The y semi-axis will be recalculated.')
            call writloga('default',0,logmess,0,ierrw)
         endif
C
C        ---------------------------------------------------------------
C        STORE THE CENTER AND THE RADII IN THE STORAGE BLOCK.
C
         surfparam(offsparam(nsurf)+13)=x1
         surfparam(offsparam(nsurf)+14)=y1
         surfparam(offsparam(nsurf)+15)=z1
C
         surfparam(offsparam(nsurf)+16)=a
         surfparam(offsparam(nsurf)+17)=b
         surfparam(offsparam(nsurf)+18)=c
C
C        ---------------------------------------------------------------
C        STORE THE ROTATION MATRIX
C
         surfparam(offsparam(nsurf)+19)=ax
         surfparam(offsparam(nsurf)+20)=bx
         surfparam(offsparam(nsurf)+21)=cx
C
         surfparam(offsparam(nsurf)+22)=ay
         surfparam(offsparam(nsurf)+23)=by
         surfparam(offsparam(nsurf)+24)=cy
C
         surfparam(offsparam(nsurf)+25)=az
         surfparam(offsparam(nsurf)+26)=bz
         surfparam(offsparam(nsurf)+27)=cz
C
         lastsparam=lastsparam+27
C
      elseif (istypein(1:lenistype) .eq. 'sheet') then
C
C        ***************************************************************
C        HANDLE SHEET TYPE SURFACE
C
         call sheet(xmsgin,cmsgin,imsgin,msgtype,nwds,cmosheet)
         sheetnm(nsurf)=cmosheet
         call cmo_select(cmo,ierr)
C
      else
C
C        ***************************************************************
C        ILLEGAL SURFACE TYPE.
C
         write(logmess,9120) istypein
 9120    format(' ERROR - Illegal Surface Type: ', a)
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
C
      endif
C
c  call conadd for constrained surface types
c
      if(ibtypein(1:7).eq.'reflect'.or.ibtypein(1:7).eq.'virtual'
     *  .or.ibtypein(1:8).eq.'intrcons') call conadd(nsurf)
 9999 continue
C
      return
      end
