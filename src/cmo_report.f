
      subroutine cmo_report(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierr_return)

C
C
C#######################################################################
C
C     PURPOSE -
C           non-evasive written reports on current cmo
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_report.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
CPVCS  Original version - tcherry - 9/97
CPVCS
CPVCS
C
c#######################################################################
c     The REPORT Command writes info on current cmo, no changes are
c     made to that cmo 
c
c     FORMAT:
C
C     REPORT / VOLUME   / MATERIAL  / [ matno | start,end,stride ] 
C     REPORT / VOLUME   / THRESHOLD / [ xvalue ] / [ COLOR ]
C     REPORT / ATTRIBUTES | attnam | XYZ / [MINMAX]  / [ pset | 1,0,0 ]     
C
C
C     -----------------------------------------------------------------
C     VOLUME/MATERIAL USAGE:
C
C     REPORT/VOLUME/ MATERIAL / [ matno | start,end,stride ]
C
C       MATERIAL        - writes a list of volumes for each material 
C                         which are defined by integers in itetclr array.
C                         Total volume of the mesh then is written.
C       matno           - writes volume for one selected material.
C       start,end,stride- writes volumes for set of materials  using
C                         start, stop, stride parameters.
C     EXAMPLES:
C
C     report/volume/material
C     report/volume/material/ 3
C     report/volume/material/ 3,6,1 
C
C     EXAMPLE OUTPUT:
C     _____
C    |
C    |report/volume/material
C    |
C    |volume/area of materials for cmo1         1          11 
C    |Material   Num. Elements   Volume          Fractional Volume
C    |    1          84          0.2296907E+06     0.237398255   
C    |    2          84          0.1552035E+06     0.160411489  
C    |    3          84          0.3628033E+05     0.037497762 
C    |    4          84          0.1016645E+06     0.105076001
C    |    5          84          0.2241828E+06     0.231705541
C    |    6          72          0.9774200E+05     0.101021844
C    |    7          54          0.1011868E+06     0.104582198
C    |    8           0          0.0000000E+00     0.000000000
C    |    9           0          0.0000000E+00     0.000000000 
C    |   10           0          0.0000000E+00     0.000000000
C    |   11         156          0.2158268E+05     0.022306910 
C    |Total              702          0.9675333E+06           
C    |
C    |_____
C
C     -----------------------------------------------------------------
C     VOLUME/THRESHOLD USAGE:
C
C     REPORT / VOLUME   / THRESHOLD / [ xvalue ] / [ COLOR ]
C
C          xvalue  -  user defined lower bound for element volumes
C                     any volumes below this are reported. The
C                     default value is 0.0
C          COLOR   -  character string option indicating that elements
C                     should be colored such that
C                     element volumes > threshold value -> 1
C                     element volumes = threshold value -> 2
C                     element volumes < threshold value -> 3
C     EXAMPLE OUTPUT:
C     
C     report/volume/threshold
C     report/volume/threshold/ 0.5
C     report/volume/threshold/color
C
C     EXAMPLE OUTPUT:
C     _____
C    |
C    | Enter a command
C    |report/volume/threshold/ 1.0
C    |
C    |Looking for volume of elements .le.     1.0000000000000
C    | Volume    316   0.50770984862851
C    |  91    169736.27608300    232100.00000000    666.28329778760
C    |  92    169736.17628500    232100.00000000    673.72754260650
C    |  590    169736.35616210    232100.00000000    670.48469733350
C    |
C    |       1 elements le:   0.100000E+01
C    |_____
C
C     -----------------------------------------------------------------
C     ATTRIBUTES USAGE:
C
C     REPORT / ATTRIBUTES | attname | XYZ/ [MINMAX]  / [ pset | 1,0,0 ]     
C
C                    - default writes a list of all attributes in cmo.
C       attname      - writes only selected attribute name.
C       NODE         - writes a list of all node attributes in cmo
C       ELEM         - writes a list of all elem attributes in cmo
C       MINMAX       - writes min/max values for all or selected attnam
C       pset or 1,0,0 - writes min and max values for all or selected
C                       attributes within selected point set
C                           
C     EXAMPLES:
C     
C     report/attributes
C     report/attribute/node
C     report/attribute/element
C     report/attribute/minmax
C     report/ attribute/node /minmax/ 1,50,0 /
C     report/ xic /minmax/ 1,50,0 /
C     report/ xyz /minmax
C
C     EXAMPLE OUTPUT:
C      ____
C     |
C     |report/attribute/node/minmax/1,4,0
C     |
C     |INDEX   ATTRIBUTE NAME            MIN            MAX     LENGTH
C     |1)     -def-              0.000000E+00  0.000000E+00       842
C     |14)    isetwd             0.000000E+00  0.000000E+00       842
C     |15)    ialias                        0             0       842
C     |16)    imt1                          1             1       842
C     |17)    itp1                          0            10       842
C     |18)    icr1                          0             0       842
C     |19)    isn1                          0             0       842
C     |20)    ign1                          0             0       842
C     |21)    xic                1.694353E+05  1.695731E+05       842
C     |22)    yic                2.321000E+05  2.321000E+05       842
C     |23)    zic                2.611929E+02  3.727260E+02       842
C     |
C     |
C     |report imt1 minmax
C     |
C     |INDEX   ATTRIBUTE NAME            MIN            MAX     LENGTH
C     | 1)    imt1                         1            11       303
C     |
C     |_____
C
C
C     -----------------------------------------------------------------
C     note that these commands come from geo subs:
C           report / volume / material  (volume_material)
C           report / volume / threshold (test_volume)
C           report / attribute /        (attributes)
C           report / attribute /        (attributes)
C           report / xyz / minmax       (minmax)
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
C ARGS
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      integer ierr_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr, ierrw, ics
      integer ilen, ityp
      integer iopt

      integer icharlnf
C
      integer imin,imax
      real*8 xopt
      real*8 xmin, xmax, ymin, ymax, zmin, zmax

      character*32 cmo, attnam
      character*32 report_opt, copt
      character*32 isubname
      character*32  ctype
      character*132 logmess
C
      character*8  sbname
      character*16 parname
C
C#######################################################################
C
C
C#######################################################################
C BEGIN
C
      isubname = 'report'
      ierr_return=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierr_return)
      if(ierr_return.ne.0) then
         write(logmess,'(a,a)')'REPORT: not a valid mesh object: ',cmo
         call writloga('default',0,logmess,1,ierrw)
         goto 9999
      endif


      if (msgtype(2) .eq. 3) then
         report_opt= cmsgin(2)
      else
         write(logmess,'(a)')'REPORT: not a valid syntax'
         call writloga('default',0,logmess,1,ierrw)
         call dotaskx3d('geohelp/report ; finish ',ierrw)
         goto 9999
      endif
      ilen=icharlnf(report_opt)
C
      if(report_opt(1:ilen) .eq. 'volume') then
C     ******************************************************************
C     VOLUME : WRITE VOLUME MATERIALS, OR ELEMENT THRESHOLD

C.....VOLUME / MATERIAL
        if (msgtype(3).eq.3 .and. cmsgin(3)(1:3).eq.'mat') then
          call volume_material(
     *          imsgin,xmsgin,cmsgin,msgtype,nwds,ierr_return)
      
C.....VOLUME / THRESHOLD
        elseif ( msgtype(3).eq.3   .and. 
     *           cmsgin(3)(1:9).eq.'threshold') then
          call test_volume(
     *         imsgin,xmsgin,cmsgin,msgtype,nwds,ierr_return)

        else
         write(logmess,'(a)')'REPORT: not a valid syntax'
         call writloga('default',0,logmess,1,ierrw)
         call dotaskx3d('geohelp/report ; finish ',ierrw)
         goto 9999

        endif


      elseif (report_opt(1:9) .eq. 'attribute' .or. 
     >        cmsgin(3)(1:6).eq.'minmax' ) then
C     ******************************************************************
C     ATTRIBUTE : WRITE LIST OR DESCRIPTION OF AN ATTRIBUTE

C..... ATTRIBUTES 
C      ATTRIBUTES / MINMAX 
C      ATTRIBUTES / MINMAX / pset 
C      attribute_name / MINMAX 
C      XYZ / MINMAX

       call attributes(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
       if (ierr .ne. 0) then
           write(logmess,'(a,a)')
     >     'minmax failed for attribute ',attnam
           call writloga('default',0,logmess,1,ierrw)
           goto 9999
       endif

      else
C     ******************************************************************
C     ERROR
         write(logmess,'(a,a,a)')'REPORT: ',report_opt(1:ilen),
     *      ' unrecognized report option'
         call writloga('default',0,logmess,0,ierrw)
         call dotaskx3d('geohelp/report ; finish ',ierrw)
         goto 9999
      endif

9999  if (ierr.ne.0 .or. ierr_return.ne.0)
     > call x3d_error('error during REPORT for ',report_opt(1:ilen)) 
      return
      end

