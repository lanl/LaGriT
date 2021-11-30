      subroutine cmo_status(cmo_name_in,coption,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine prints the status of the Mesh Objects.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Mesh Object name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_status.f,v $
C         Revision 2.00  2007/11/05 19:45:50  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   27 Jul 2001 11:08:12   tam
CPVCS    add coption to arguments enabling choice of brief
CPVCS    which writes just the status header
CPVCS    changed write formats
CPVCS    
CPVCS       Rev 1.6   10 Apr 2001 11:04:22   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.5   13 Apr 2000 09:46:12   dcg
CPVCS    change headings
CPVCS
CPVCS       Rev 1.4   06 Apr 2000 10:07:16   gable
CPVCS    Changed format statement for real valued attributes.
CPVCS
CPVCS       Rev 1.4   06 Apr 2000 10:05:28   gable
CPVCS    Changed format statement for real valued attributes.
CPVCS
CPVCS       Rev 1.3   28 Mar 2000 14:09:02   dcg
CPVCS    remove include 'machine.h'
CPVCS
CPVCS       Rev 1.2   Tue Feb 01 13:49:54 2000   dcg
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:41:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   09/14/95 12:09:38   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:44:50   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:24:00   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 09:57:18   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.2   02/10/95 14:09:08   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:34   dcg
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'
C
C#######################################################################
C
      character*(*) cmo_name_in
      character*(*) coption
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, j, len, len2,  ierr, ifound, icscode
      integer index, inum
C
      integer nnodes, nelements, mbndry,natts,lenattlist,itype,
     *        nsd_geom, nsd_topo, nen, nef, nee,ilen,
     *        posname,postype,posrank,
     *        posint,posio,pospers,poslen
      real*8 xdefault
C
      character*32  isubname, partname,cmo_name_cur,cmotmp
      character*32  cname, ctype, crank, clength, cinterp,cdefault,
     *              cpersistence, cioflag, cmo_name, cstatus
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      isubname='cmo_status'
C
C
      if(number_of_mesh_objects.gt.1) then
C
        call cmo_get_name(cmo_name_cur,icscode)
        write(logmess,'(a,a)')
     *     'The current-mesh-object(CMO) is: ',cmo_name_cur
        call writloga('default',1,logmess,0,ierr)
C
        len=icharlnf(cmo_name)
C
        if((cmo_name_in(1:len).eq.'-cmo-') .or.
     *     (cmo_name_in(1:len).eq.'-def-')) then
C
C....      Use the Current Mesh Object.
C
           cmo_name_in=cmo_name_cur
C
           len=icharlnf(cmo_name)
C
        endif
C
        ifound=0
C
c.... Find postions of name, type, rank and length
c
        partname='define_cmo_lg'
        call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
        do i=1,number_of_default_attparam_name
           if(defcmo_attparam_names(i).eq.'name') posname=i
           if(defcmo_attparam_names(i).eq.'type') postype=i
           if(defcmo_attparam_names(i).eq.'rank') posrank=i
           if(defcmo_attparam_names(i).eq.'length') poslen=i
           if(defcmo_attparam_names(i).eq.'interpolation') posint=i
           if(defcmo_attparam_names(i).eq.'persistence') pospers=i
           if(defcmo_attparam_names(i).eq.'ioflag') posio=i
        enddo
        partname='define_cmo_lg'
        call mmfindbk('cmo_natts',partname,ipcmo_natts,ilen,
     *    icscode)
        call mmfindbk('cmo_names',partname,ipcmo_names,ilen,
     *    icscode)

C       loop through the cmo mesh objects
        inum = 0
        do i=1,number_of_mesh_objects

           cmo_name=cmo_names(i)
           inum = inum+1
           if(cmo_name(1:9).eq.'-default-' .and.
     *        cmo_name_in(1:9).ne.'-default-') then 
              inum = inum - 1
              goto 50
           endif
           if(cmo_name_in .ne. '-all-') then
              if(cmo_name_in.ne.cmo_name) go to 50
           endif
           ifound=1
           if(i.eq.1) then
              partname='default_cmo_lg'
              natts=number_of_default_attributes
           else
              partname=cmo_name
              natts=cmo_natts(i)
           endif

C       Get and Write the header information
C       note: cmo_get calls change cmo name to current cmo
C       if the cmo name is -default- show default cmo values

           if(cmo_name(1:9).eq.'-default-') then
             nnodes = 0
             nelements = 0
             mbndry = 16000000
             nsd_geom = 3
             nsd_topo = 3
             nen = 4
             nef = 4
             nee = 6
             ctype = 'tet'
           else
           cmotmp = cmo_name
             call cmo_get_info('nnodes',cmotmp,
     *                        nnodes,ilen,itype,icscode)
             call cmo_get_info('nelements',cmotmp,
     *                        nelements,ilen,itype,icscode)
             call cmo_get_info('mbndry',cmotmp,
     *                        mbndry,ilen,itype,icscode)
             call cmo_get_info('ndimensions_geom',cmotmp,
     *                        nsd_geom,ilen,itype,icscode)
             call cmo_get_info('ndimensions_topo',cmotmp,
     *                        nsd_topo,ilen,itype,icscode)
             call cmo_get_info('nodes_per_element',cmotmp,
     *                       nen,ilen,itype,icscode)
             call cmo_get_info('faces_per_element',cmotmp,
     *                       nef,ilen,itype,icscode)
             call cmo_get_info('edges_per_element',cmotmp,
     *                       nee,ilen,itype,icscode)
             call cmo_get_mesh_type(cmotmp,ctype,itype,icscode)
           endif

           len = icharlnf(cmo_name)
           len2 = icharlnf(cmo_name_cur)
           cstatus='inactive'
           if(cmo_name(1:len).eq.cmo_name_cur(1:len2))cstatus='  active'

           if(cmo_name(1:9).eq.'-default-') then
             write(logmess,8000) 0,cmo_name
           else
             write(logmess,8000) inum,cmo_name
           endif
           call writloga('default',1,logmess,0,ierr)
 8000      format(i3,' Mesh Object name: ',a)

c       write 1 column 8 lines
        if (coption(1:6).eq.'long') then

           write(logmess,7010) cstatus(1:8)
           call writloga('default',0,logmess,0,ierr)
 7010      format('    status  =                ',a)

           write(logmess,7020) nnodes
           call writloga('default',0,logmess,0,ierr)
 7020      format('    number of nodes =      ',i10)

           write(logmess,7030) nelements
           call writloga('default',0,logmess,0,ierr)
 7030      format('    number of nelements =  ',i10)

           write(logmess,7040) nsd_geom
           call writloga('default',0,logmess,0,ierr)
 7040      format('    dimensions geometry =        ',i4)

           write(logmess,7050) nsd_topo
           call writloga('default',0,logmess,0,ierr)
 7050      format('    dimensions topology =        ',i4)

           write(logmess,7070) ctype(1:3)
           call writloga('default',0,logmess,0,ierr)
 7070      format('    element type =                ',a)

           write(logmess,7080) nen,nef,nee 
           call writloga('default',0,logmess,0,ierr)
 7080      format('  ',i4,' nodes   ',i4,' faces  ',i4,' edges')

           write(logmess,7060) mbndry
           call writloga('default',0,logmess,0,ierr)
 7060      format('    boundary flag =        ',i10)


c       write 2 columns with 4 lines
        else

           write(logmess,8010) nnodes,nelements
           call writloga('default',0,logmess,0,ierr)
 8010      format('    number of nodes =    ',i10,
     *            '        number of elements =   ',i10)

           write(logmess,8030) nsd_geom,ctype(1:3) 
           call writloga('default',0,logmess,0,ierr)
 8030      format('    dimensions geometry =      ',i4,
     *      '        element type =                ',a)

           write(logmess,8040) nsd_topo,nen,nef,nee
           call writloga('default',0,logmess,0,ierr)
 8040      format('    dimensions topology =      ',i4,
     *     '     ',i4,' nodes   ',i4,' faces   ',i4,' edges ')

           write(logmess,8020) mbndry,cstatus(1:8)
           call writloga('default',0,logmess,0,ierr)
 8020      format('    boundary flag =      ',i10,
     *     '        status =                 ',a)

         endif

C        end cmo header formats
c---------------------------------------------------------------------
c old write statements
c           write(logmess,9000) 
c     *     i,cmo_name,ctype(1:3),cstatus
c           call writloga('default',2,logmess,0,ierr)
c 9000      format(i3,' Mesh Object name: ',a,
c     *            'type= ',a,' status= ',a)
c
c           write(logmess,9010) nnodes,nelements,mbndry
c           call writloga('default',0,logmess,0,ierr)
c 9010      format('   nnodes=',i10,' elements=',i10,' mbndry=',i10)
cC
c           write(logmess,9020) nsd_geom,
c     *                         nsd_topo,
c     *                         nen,
c     *                         nef,
c     *                         nee
c           call writloga('default',0,logmess,0,ierr)
c 9020      format('   nsdgeom=',i4,' nsdtopo=',i4,
c     *            ' nen=',i4,' nef=',i4,' nee=',i4)
c
cc---------------------------------------------------------------------

C       Write the cmo attribute information
         if(coption(1:5).ne.'brief') then

            call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   lenattlist,icscode)
            if(icscode.ne.0) call x3d_error(isubname,'mmfindbk')
C
            write(logmess,9030) 'NAME     ', 'TYPE     ',
     *                          'RANK     ', 'LENGTH   ',
     *                          'INTERPOL ', 'PERSIST  ',
     *                          'IO  '     , 'VALUE    '
            call writloga('default',1,logmess,1,ierr)
 9030       format(4x,a15,a5,4x,a5,7x,a8,3x,2(1x,a5),1x,a3,1x,a10)
c
c....loop through the attributes
c
            call mmfindbk( 'cmo_attparam_idefault',partname,
     *             ipcmo_attparam_idefault,len,icscode)
            call mmfindbk( 'cmo_attparam_rdefault',partname,
     *             ipcmo_attparam_rdefault,len,icscode)
            call mmfindbk( 'cmo_attparam_cdefault',partname,
     *             ipcmo_attparam_cdefault,len,icscode)
            do j=1,natts
C....       NAME Field.
C
               cname=cmo_attlist(number_of_params_per_att*(j-1)
     *           +posname)
               index=j
               ctype=cmo_attlist(number_of_params_per_att*(j-1)
     *           +postype)
               crank=cmo_attlist(number_of_params_per_att*(j-1)
     *           +posrank)
               clength=cmo_attlist(number_of_params_per_att*(j-1)
     *           +poslen)
               cinterp=cmo_attlist(number_of_params_per_att*(j-1)
     *           +posint)
               cpersistence=cmo_attlist(number_of_params_per_att*(j-1)
     *           +pospers)
               cioflag=cmo_attlist(number_of_params_per_att*(j-1)
     *           +posio)
               if(ctype.eq.'INT'.or.ctype.eq.'VINT') then
                 xdefault=cmo_attparam_idefault(j)
               elseif(ctype.eq.'REAL'.or.ctype.eq.'VDOUBLE') then
                 xdefault=cmo_attparam_rdefault(j)
               elseif(ctype.eq.'CHARACTER'.or.ctype.eq.'VCHAR') then
                 cdefault=cmo_attparam_cdefault(j)
               else
                 xdefault=0.0
               endif
C
               if(ctype.eq.'CHARACTER'.or.ctype.eq.'VCHAR') then
                  write(logmess,9039) index,
     *                          cname(1:icharlnf(cname)),
     *                          ctype(1:icharlnf(ctype)),
     *                          crank(1:icharlnf(crank)),
     *                          clength(1:icharlnf(clength)),
     *                          cinterp(1:icharlnf(cinterp)),
     *                          cpersistence(1:icharlnf(cpersistence)),
     *                          cioflag(1:icharlnf(cioflag)),
     *                          cdefault
 9039             format(i3,1x,a15,1x,a4,1x,a10,1x,a15,2(1x,a5),
     *                 (1x,a3),(1x,a10))
               else
                  write(logmess,9040) index,
     *                          cname(1:icharlnf(cname)),
     *                          ctype(1:icharlnf(ctype)),
     *                          crank(1:icharlnf(crank)),
     *                          clength(1:icharlnf(clength)),
     *                          cinterp(1:icharlnf(cinterp)),
     *                          cpersistence(1:icharlnf(cpersistence)),
     *                          cioflag(1:icharlnf(cioflag)),
     *                          xdefault
 9040             format(i3,1x,a15,1x,a4,1x,a10,1x,a15,2(1x,a5),
     *                 (1x,a3),(1x,1pe10.3))
               endif
               call writloga('default',0,logmess,0,ierr)
C
            enddo
c           end natts loop
C
            endif
c           end not brief version of status

  50        continue
         enddo
c        end loop through cmo mesh objects 
C
         if(ifound.le.0) then
C
C....       Mesh Object does not exist.

            ierror_return=0
            write(logmess,9050) cmo_name
            call writloga('default',0,logmess,0,ierr)
 9050       format(' Mesh Object does not exist: ',a32)
        endif
C
      else
C
         ierror_return=0
         write(logmess,'(a)') 'No Mesh Objects defined.'
         call writloga('default',0,logmess,0,ierr)
C
      endif
      ierror_return=0

C     add a space line if printing just header information
      if(coption(1:5).eq.'brief') then
         write(logmess,'(a)') ' '
         call writloga('default',0,logmess,1,ierr)
      endif
C
      goto 9999
 9999 continue
C
      return
      end
