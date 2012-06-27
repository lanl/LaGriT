      subroutine cmo_delatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Deletes an Attribute from an existing Mesh Object.
C         but only if it is a temporary attribute.
C
C         Compare cmo_delatt_all_lg below which deletes the attribute
C         even if is it permanent, and if it is a scalar, also deletes
C         any attributes with this attribute as rank or length.
C
C      Note: The reserved names for a mesh object are
C      -def- is default argument, current mesh object is used
C      -cmo- is the current mesh object
C      -default- is the template object and is modified with cmo_delatt_def
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
C         $Log: cmo_delatt.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   13 Jun 2007 07:34:16   tam
CPVCS    changes to argument handling now check for reserved keywords
CPVCS    -cmo- -def- -default- and handles them correctly
CPVCS    error reporting has been clarified with subroutine name
CPVCS    and type of error encounterd when using keyword arguments
CPVCS    
CPVCS       Rev 1.4   16 Aug 2006 10:46:56   gable
CPVCS    Added support of -def- argument for mesh object name.
CPVCS    
CPVCS       Rev 1.3   04 Nov 2002 11:30:40   gable
CPVCS    Fixed typo in first few lines where cmo name was defined.
CPVCS    
CPVCS       Rev 1.2   26 Jan 2000 13:49:12   jtg
CPVCS    Forgot to add change re assuming delatt just packs the att list is OK
CPVCS    
CPVCS       Rev 1.1   26 Jan 2000 13:17:40   jtg
CPVCS    subroutine cmo_delatt_all_lg added which deletes even if permanent
CPVCS    and also, if scalar, deletes fields with this attribute as rank or length
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:02:58   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 16:41:00 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   03/15/95 15:22:40   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:55:56   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:06:52   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:22   dcg
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
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr,len,len2
C
      integer icharlnf
      external icharlnf
C
      character*32 cmo_name, att_name
C
      character*132 logmess
C
C#######################################################################
C     begin cmo_delatt which calls cmo_delatt_cmo or cmo_delatt_def 
C
      len=icharlnf(cmsgin(3))
      if (len.gt.32) len=32
      cmo_name = cmsgin(3)(1:len)

      len2=icharlnf(cmsgin(4))
      if (len2.gt.32) len2=32
      att_name = cmsgin(4)(1:len2)

C.... Delete Attribute in the Default Mesh Object Table
      if(cmo_name(1:9).eq.'-default-') then
 
         call cmo_delatt_def(att_name,ierror_return)
         if(ierror_return.eq.0) then
C
C           Verify the Default Mesh Object.
            call cmo_verify_def(ierror_return)
            if(ierror_return.ne.0) then
               write(logmess,'(a,a,a)')
     *         'ERROR cmo_delete ',
     *         'Mesh Object is not consistent: ',cmo_name(1:len)
               call writloga('default',0,logmess,0,ierr)
            endif
         endif
C
      else 

C....    Delete Attribute in the Current Mesh Object 
         if((cmo_name(1:len).eq.'-cmo-') .or.
     *      (cmo_name(1:len).eq.'-def-')) then
C
            call cmo_get_name(cmo_name,ierror_return)
            cmsgin(3)=cmo_name
            len = icharlnf(cmo_name)
         endif

C....    Delete Attribute from Named Mesh Object
C
         call cmo_delatt_cmo(cmo_name,att_name,ierror_return)
C
         if(ierror_return.eq.0) then
C
C....       Verify the Mesh Object.
C
            call cmo_verify_cmo(cmo_name,ierror_return)
C
               if(ierror_return.ne.0) then
                  write(logmess,'(a,a,a)')
     *           'ERROR cmo_delatt ',
     *           'Mesh Object is not consistent: ',cmo_name(1:len)
                  call writloga('default',0,logmess,0,ierr)
               endif
C
         endif
C
      endif
C
      return
      end

c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################

        subroutine cmo_delatt_all_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror)

C #####################################################################
C      PURPOSE -
C
C         This Routine Deletes an Attribute from an existing Mesh Object,
C         even if is it permanent, and if it is a scalar, also deletes
C         any attributes with this attribute as rank or length.
C
C         Compare cmo_delatt above which deletes the just the
C         attribute and only if it is a temporary attribute.
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
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
c ........................................................................

        implicit none

        integer nwds,ierror
        character*(*) cmsgin(*)
        integer imsgin(*),msgtype(*)
        real*8 xmsgin(*)

        character*32 cmo, att_name
        integer lcmo,latt, ierr_report

        character*32 cname
        character*32 cmsgout(6)
        real*8 xmsgout(6)
        integer imsgout(6),nwdsdel,nwdsmod,tmsgout(6)

        character*32 ctype,crank,clength,cpers,cinter,cio
     &              ,ctype_att,cpers_att
        integer index

        integer local_debug,len,len1,len2,ityp,ierr,i,i_att,n_att

        character*132 logmess

        integer icharlnf
        external icharlnf

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C begin cmo_delatt_all_lg used for DELATT option

        ierror=0
        ierr_report=0

        lcmo=icharlnf(cmsgin(3))
        if (lcmo.gt.32) lcmo=32
        cmo=cmsgin(3)(1:lcmo)

C       for default argument, get name of current cmo
        if((cmo(1:5).eq.'-def-') .or.
     *        (cmo(1:5).eq.'-cmo-')) then
           call cmo_get_name(cmo,ierr)
           lcmo=icharlnf(cmo)
           if (lcmo.gt.32) lcmo=32
        endif

        latt=icharlnf(cmsgin(4))
        if (latt.gt.32) latt=32
        att_name=cmsgin(4)(1:latt)

        call cmo_exist(cmo(1:lcmo),ierr)
        if (ierr.ne.0) then
          ierror=-1
          write(logmess,'(a,a)')
     *    'DELATT Mesh Object does not exist: ',cmo(1:lcmo)
          call writloga('default',0,logmess,0,ierr)
           ierr_report=1
          goto 9999
        endif

        call cmo_get_attparam(att_name,cmo,index,ctype,crank,
     &       clength,cinter,cpers,cio,ierror)

C       attribute already gone, no action
        if (ierror.ne.0) then
           write(logmess,'(a,a)')
     *     'DELATT no action, attribute does not exist: ',
     *     att_name(1:latt)
           call writloga('default',0,logmess,0,ierr)
           ierr_report=1
           goto 1000
        endif
        ctype_att=ctype
        cpers_att=cpers

        nwdsmod=6
        nwdsdel=4
        do i=1,6
           xmsgout(i)=0.d0
           imsgout(i)=0
           tmsgout(i)=3
           cmsgout(i)=' '
        enddo
        cmsgout(1)='cmo'
        cmsgout(3)=cmo
        cmsgout(5)='persistence'
        cmsgout(6)='temporary'

        len=icharlnf(ctype_att)
        if (ctype_att(1:len).ne.'INT') goto 500

c ......... (delete any attributes with input attribute as rank or length) ...........

        call cmo_get_info('number_of_attributes',cmo
     &                     ,n_att,len,ityp,ierr)
        i_att=1
100     call cmo_get_attribute_name(cmo,i_att,cname,ierror)
        call cmo_get_attparam(cname,cmo,index,ctype,crank,
     &        clength,cinter,cpers,cio,ierror)
        len1=icharlnf(clength)
        len2=icharlnf(crank)
        if (clength(1:len1).eq.att_name(1:latt)
     &         .or. crank(1:len1).eq.att_name(1:latt)) then
           cmsgout(4)=cname
           len=icharlnf(cpers)
           if (cpers(1:len).ne.'temporary') then
              cmsgout(2)='modatt'
              call cmo_modatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsmod
     &                     ,ierror)
           endif
           cmsgout(2)='delatt'
           call cmo_delatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsdel
     &                     ,ierror)
           if (ierror.eq.0) then
              n_att=n_att-1
              if (i_att.le.n_att) goto 100
           endif
        endif
        i_att=i_att+1
        if (i_att.le.n_att) goto 100

c ......... (now delete the attribute itself) ...........
500     continue

        cmsgout(4)=att_name
        len=icharlnf(cpers_att)
        if (cpers_att(1:len).ne.'temporary') then
           cmsgout(2)='modatt'
           call cmo_modatt(imsgout,xmsgout,cmsgout,tmsgout
     &                     ,nwdsmod,ierror)
        endif
        cmsgout(2)='delatt'
        call cmo_delatt(imsgout,xmsgout,cmsgout,tmsgout
     &                     ,nwdsdel,ierror)

c .................................................................

c........ (successful return) ..................

1000    ierror=0
        if (local_debug.gt.0) call mmverify()
        return

c........ (failure return) ..................
9999    ierror=1
        if (local_debug.gt.0) call mmverify()

C       check to see if error report has already been written
C       if not, write general error report now
        if (ierr_report .eq. 0) then
           write(logmess,'(a,a,a,a,a)')
     *      'ERROR cmo_DELATT ',
     *      'cmo: ', cmo(1:lcmo),' attribute: ',att_name(1:latt)
           call writloga('default',0,logmess,0,ierr)
        endif

        return
c .....................................................
        end

c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
