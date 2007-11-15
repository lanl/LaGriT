      subroutine cmo_get_attparam(att_name_in,cmo_name,index,ctype,
     *    crank,clen,cinter,cpers,cio,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine returns Mesh Object information.
C
C     INPUT ARGUMENTS -
C
C        att_name   - (character) The attribute to find
C        cmo_name  - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        index         - attribute number
C        ctype         - type
C        crank         -  rank
C        clen          -  length
C        cinter        -  interpolation
C        cpers         - persistence
C        cio           - ioflag
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_attparam.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   13 Apr 2006 14:20:20   tam
CPVCS    Illegal error messege removed and replaced with messege only if
CPVCS    idebug is greater than 1. This allows this routine to check for
CPVCS    existance of attribute without reporting an error all the time
CPVCS    
CPVCS       Rev 1.4   10 Apr 2001 11:04:16   dcg
CPVCS    shorten too long name
CPVCS    
CPVCS       Rev 1.3   22 Mar 2001 09:57:14   dcg
CPVCS    look for special name imt, itp, icr, isn and append '1' to end of name
CPVCS    to match default mesh object attribute name
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 13:34:12 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   Mon Jan 31 13:23:24 2000   dcg
CPVCS
CPVCS       Rev 1.19   Tue Oct 06 16:48:40 1998   dcg
CPVCS    make equivalent node attributes imt,imt1, itp,itp1,
CPVCS    icr,icr1,...
CPVCS
CPVCS       Rev 1.18   Wed Dec 17 11:25:10 1997   dcg
CPVCS    declare iout as a pointe
CPVCS
CPVCS       Rev 1.17   Mon Apr 14 16:41:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.16   Wed Mar 06 16:44:04 1996   dcg
CPVCS    print error messages if idebug=1
CPVCS
CPVCS       Rev 1.15   09/14/95 12:09:18   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.14   09/13/95 14:32:16   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.13   09/11/95 14:44:00   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.12   08/30/95 21:08:46   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.11   05/22/95 15:28:24   ejl
CPVCS    Added nfaces and nedges.
CPVCS
CPVCS       Rev 1.10   03/15/95 15:22:58   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.9   02/16/95 09:56:14   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.8   02/10/95 14:07:24   ejl
CPVCS    Fix bugs left from last update
CPVCS
CPVCS       Rev 1.6   01/30/95 06:22:12   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.5   01/24/95 08:52:42   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/04/95 22:01:34   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   12/09/94 22:50:58   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:58:44   het
CPVCS    Added a data variable type to the call.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/28/94 14:14:44   het
CPVCS    Add the "mbndry" option.
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:50   het
CPVCS    Original Version
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
      character*(*)  cmo_name,att_name_in
      character*32 partname,ctype,cpers,cio,clen,crank,cinter,name,
     *    att_name
      integer posname,postype,posrank,posio,poslen,posint,pospers
      integer i,len1,len2, natts
      integer icharlnf
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len,ierr,icscode,icmo_index,index, idebug
C
      character*132 logmess
 
C
C#######################################################################
C
      if((cmo_name.eq.'-cmo-') .or.
     *   (cmo_name.eq.'-default-') .or.
     *   (cmo_name.eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
C
C.... Search table for Mesh Object.
C
      call cmo_exist(cmo_name,icscode)
C
C
      if(icscode.ne.0) then
C
         ierror_return=-1
C
         write(logmess,'(a,a)')
     *            'CMO_GET_INFO: Mesh Object does not exist: ', cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
c
c   look for special names
c
C
         call cmo_get_intinfo('idebug',cmo_name,idebug,len,i,icscode)

         att_name=att_name_in
         if(att_name_in.eq.'imt') att_name='imt1'
         if(att_name_in.eq.'itp') att_name='itp1'
         if(att_name_in.eq.'isn') att_name='isn1'
         if(att_name_in.eq.'icr') att_name='icr1'
C
c.... Find postions of name, type, rank and length
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
C
C        Loop through the attributes look for matching attribute
C           name - then check type
C
         partname='define_cmo_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         partname=cmo_name
         call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
         call cmo_get_index(cmo_name,icmo_index,ierror_return)
         if(ierror_return.ne.0) go to 9998
         natts=cmo_natts(icmo_index)
         do i=1,natts
            name=cmo_attlist(number_of_params_per_att*(i-1)+posname)
            len1=icharlnf(name)
            len2=icharlnf(att_name)
            if(name(1:len1)
     *             .eq.att_name(1:len2)) then
              index=i
              ctype=cmo_attlist(number_of_params_per_att*(i-1)+postype)
              crank=cmo_attlist(number_of_params_per_att*(i-1)+posrank)
              clen=cmo_attlist(number_of_params_per_att*(i-1)+poslen)
              cinter=cmo_attlist(number_of_params_per_att*(i-1)+posint)
              cpers=cmo_attlist(number_of_params_per_att*(i-1)+pospers)
              cio=cmo_attlist(number_of_params_per_att*(i-1)+posio)
              ierror_return=0
              go to 9999
            endif
         enddo
         ierror_return=1
C
C        ierror_return is 0 if there are no errors  
 9998    if(ierror_return.ne.0) then
           if (idebug.ge.1) then
               write(logmess,9000) cmo_name,att_name
               call writloga('default',0,logmess,0,ierr)
 9000          format('Attribute Error in cmo_get_attparam: ',2a32)
           endif
         endif
C
      endif
C
 9999 return
      end
