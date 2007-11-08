      subroutine cmo_verify_def(ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine verifies the Default Mesh Object Data.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/cmo_verify_def_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.3   10 Apr 2001 11:04:22   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 13:34:18 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.1   Thu Feb 03 13:17:36 2000   dcg
CPVCS
CPVCS       Rev 1.0   Thu Jan 20 14:51:06 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:42:08 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Nov 18 10:29:32 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/14/95 16:38:42   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.4   09/11/95 14:45:12   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.3   03/15/95 15:24:12   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:57:28   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:09:36   ejl
CPVCS    Fix bugs left from last update.
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
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
 
      integer i, len, ierr
C
      integer ifound_length, ifound_rank
C
      character*132 logmess
C
C#######################################################################
C
      integer icscode
C
      integer  posname,postype,posrank,poslen,
     *  posint,pospers,posio,j,lenattlist
      character*32 cname, ctype, clength, crank,partname
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      ierror_return=0
      partname='default_cmo_lg'
C
C.... Loop over all Attributes for the Default Mesh Object.
C
      call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   lenattlist,icscode)
C
c.... Find postions of name, type, rank and length
      partname='define_cmo_lg'
 10   call mmfindbk( 'defcmo_attparam_names',partname,
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
      do i=1,number_of_default_attributes
         cname=cmo_attlist(number_of_params_per_att*(i-1)+posname)
         ctype=cmo_attlist(number_of_params_per_att*(i-1)+postype)
         clength=cmo_attlist(number_of_params_per_att*(i-1)+poslen)
         crank=cmo_attlist(number_of_params_per_att*(i-1)+posrank)
C....    TYPE Field.
C
         if(ctype(1:1).eq.'V') then
C
C....       Check for a valid type.
C
            len=icharlnf(ctype)
C
            if ((ctype(1:len) .ne. 'VINT') .and.
     *          (ctype(1:len) .ne. 'VDOUBLE').and.
     *          (ctype(1:len) .ne. 'VCHAR')) then
C
C....          Invalid Type.
C
               ierror_return=-3
C
               write(logmess,'(a,a)')
     *               'Problems with the default TYPE field. ',
     *               ctype
               call writloga('default',0,logmess,0,ierr)
C
            endif
C
C....       Check that all fields are defined.
C
            ifound_length=0
            ifound_rank=0
            do j=1,number_of_default_attributes
                  if(cmo_attlist(number_of_params_per_att*(j-1)+1)
     *             .eq.clength) ifound_length=1
                 if(cmo_attlist(number_of_params_per_att*(j-1)+1)
     *             .eq.crank) ifound_rank=1
             enddo
C
            if (ifound_length.eq.0) then
C
C....          Problems with length field.
C
               ierror_return=-1
C
               write(logmess,'(a,a)')
     *            'Problems with the LENGTH default field: ',
     *             clength
               call writloga('default',0,logmess,0,ierr)
C
            endif
C
            if (ifound_rank.eq.0) then
C
C....          Problems with rank field.
C
               ierror_return=-2
C
               write(logmess,'(a,a)')
     *               'Problems with the default RANK field: ',
     *                crank
               call writloga('default',0,logmess,0,ierr)
C
            endif
C
         endif
C
      enddo
C
      return
      end
