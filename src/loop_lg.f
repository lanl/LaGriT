      subroutine loop_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C   PURPOSE
C     Allow recursive looping on a standard LAGriT command
C
C     FORMAT:
C
C       loop / do      variable loop_start loop_stop loop_stride loop_end / lagrit_command
C
C       loop / foreach variable list_item1 list_item2 ... list_itemN loop_end / lagrit_command
C
C   INPUT/OUTPUT
C         ierr   - INTEGER ERROR FLAG (==0 ==> OK, <>0 ==> AN ERROR)
C
C         $Log:   /pvcs.config/t3d/src/loop_lg.f_a  $
CPVCS    
CPVCS       Rev 1.1   19 Apr 2002 15:38:12   gable
CPVCS    Correct error checking.
CPVCS    
CPVCS       Rev 1.0   11 Apr 2002 15:34:14   gable
CPVCS    Initial revision.
c
      implicit none
c
c INCLUDES
c
      include 'commands_lg.h'
C
C PARAMETERS
C     Hardwired parameters!!!!!!
C     Maximum depth of loop nesting is 10
C     Maximum number of list tokens is 250
C
      integer nstack_max, nlist_max, ntotal_max
      parameter (nstack_max = 10, nlist_max = 250, ntotal_max = 2500)
C
C SUBROUTINE ARGUMENTS
C
      integer nwds
      integer imsgin(nwds), msgtype(nwds), ierr
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C LOCAL VARIABLES
C
      integer i,j, istop, len_command, i_command_type, 
     1        iarg_command, iarg, i_last_loop_end,
     2        i_first_loop_end, list_start,
     3        num_loop_end, n_loop_end_found
      character*8  isubname
      character*72  errmsg
      character*132 logmess
      character*72  ch_work
C
C STACK VARIABLES
C      
      integer lp_lev
      data lp_lev / 0 /
      integer loop_start(nstack_max),
     1        loop_stop(nstack_max),
     2        loop_stride(nstack_max)
      data (loop_start (i), i=1,nstack_max)/nstack_max*0/
      data (loop_stop  (i), i=1,nstack_max)/nstack_max*0/
      data (loop_stride(i), i=1,nstack_max)/nstack_max*0/
      integer i_do(nstack_max)
      data (i_do (i), i=1,nstack_max)/nstack_max*0/
      integer iargument_list_total(nstack_max)
      data (iargument_list_total (i), i=1,nstack_max)/nstack_max*0/
      integer iargument_loop_end(nstack_max)
      data (iargument_loop_end   (i), i=1,nstack_max)/nstack_max*0/
      character*32 loop_variable(nstack_max),
     1             loop_value(nstack_max),
     2             loop_task(nstack_max)
      data (loop_variable(i),i=1,nstack_max)/nstack_max*'-nodef-'/
      data (loop_value   (i),i=1,nstack_max)/nstack_max*'-nodef-'/
      data (loop_task    (i),i=1,nstack_max)/nstack_max*'-nodef-'/
      character*8092 lagrit_command1(nstack_max),
     1               lagrit_command2(nstack_max)
      data (lagrit_command1(i),i=1,nstack_max)/nstack_max*'-nodef-'/
      data (lagrit_command2(i),i=1,nstack_max)/nstack_max*'-nodef-'/
      character*72 ch_argument_list(nstack_max,nlist_max)
      data ((ch_argument_list(i,j),i=1,nstack_max), j=1,nlist_max)
     1   /ntotal_max*'-nodef-'/
C
C FUNCTIONS
C
      integer icharlnf, icharlnb
C
C#######################################################################
c BEGIN
 
C-----INIT LOCAL VARIABLES
      isubname='loop_lg'
      errmsg= '-undefined error-'
      lp_lev = lp_lev + 1
      write(logmess,
     1 "('LOOP: loop nesting level = ',i5)")lp_lev
      call writloga('default',0,logmess,0,ierr)
C
C     Error Check: Be sure lp_lev does not get bigger than nstack_max
C
      if(lp_lev .gt. nstack_max)then
         write(logmess,
     1   "('ERROR: loop nesting .gt. nstack_max =',i5)")nstack_max
         call writloga('default',0,logmess,0,ierr)
         call x3d_error
     1   (isubname,'ERROR: Increase size of nstack_max parameter')
         ierr = -1
         goto 9999
      endif
C
C------PARSE argument 2, Is it a DO or FOREACH type loop ?
C
      iarg = 2
      if (msgtype(iarg) .eq. 3) then
           if((cmsgin(iarg)(1:icharlnf(cmsgin(iarg)))
     1                                     .eq. 'do') .or.
     2        (cmsgin(iarg)(1:icharlnf(cmsgin(iarg)))
     3                                     .eq. 'DO')) then
                 i_command_type = 1
C
           elseif((cmsgin(iarg)(1:icharlnf(cmsgin(iarg)))
     1                                     .eq. 'foreach') .or.
     1            (cmsgin(iarg)(1:icharlnf(cmsgin(iarg)))
     3                                     .eq. 'FOREACH')) then
                 i_command_type = 2
           else
              call x3d_error
     1       (isubname, 'ERROR: Argument 2')
              call x3d_error
     1       (isubname,
     2      'ERROR: loop type must be /do/ or /foreach/ ')
             ierr = -1
             goto 9999
           endif
C
      else
         call x3d_error
     1  (isubname, 'ERROR: Argument 2')
         call x3d_error
     1   (isubname,
     2      'ERROR: loop type must be /do/ or /foreach/ ')
         ierr = -1
         goto 9999
      endif
C
C------PARSE argument 3, loop_variable
      iarg = 3
      if (msgtype(iarg) .eq. 3) then
         loop_variable(lp_lev) = 
     1         cmsgin(iarg)(1:icharlnf(cmsgin(iarg)))
      else
         call x3d_error
     1  (isubname, 'ERROR: Argument 3')
         call x3d_error
     1  (isubname, 'ERROR: loop_variable must be character')
         ierr = -1
         goto 9999
      endif
C
      if (i_command_type .eq. 1) then
C
C------PARSE argument 4, loop_start
C
      iarg = 4
      if (msgtype(iarg) .eq. 1) then
         loop_start(lp_lev) = imsgin(iarg)
      else
         call x3d_error
     1   (isubname,'ERROR: Argument 4, /loop_start/ must be integer')
         ierr = -1
         goto 9999
      endif
C
C------PARSE argument 5, loop_stop
C
      iarg = 5
      if (msgtype(iarg) .eq. 1) then
         loop_stop(lp_lev) = imsgin(iarg)
      else
         call x3d_error
     1   (isubname,'ERROR: argument 5, /loop_stop/ must be integer')
         ierr = -1
         goto 9999
      endif
C
C------PARSE argument 6, loop_stride
C
      iarg = 6
      if (msgtype(iarg) .eq. 1) then
         loop_stride(lp_lev) = imsgin(iarg)
      else
         call x3d_error
     1   (isubname,'ERROR: argument 6, /loop_stride/ must be integer')
         ierr = -1
         goto 9999
      endif
      endif
C
      i_first_loop_end = 0
      i_last_loop_end  = 0
      num_loop_end     = 0
      do i = 4, nwds
C
C     Find the first and last /loop_end/ argument
C
        if (msgtype(i) .eq. 3) then
         if((cmsgin(i)(1:icharlnf(cmsgin(i))) .eq. 'loop_end') .or.
     1      (cmsgin(i)(1:icharlnf(cmsgin(i))) .eq. 'LOOP_END')) then
           if(i_last_loop_end .eq. 0)then
              i_first_loop_end = i
              iargument_loop_end(lp_lev) = i
              iargument_list_total(lp_lev) = i-4
           endif
           i_last_loop_end = i
           num_loop_end = num_loop_end + 1
         endif
        endif
      enddo
C
C     Error if a 'loop_end' was not found.
C
      if(num_loop_end .eq. 0)then
         call x3d_error
     1   (isubname,'ERROR: token /loop_end/ not found ')
         ierr = -1
         goto 9999
      endif
C------------------------------------------------------------------
C     /foreach/ case where a list is provided,
C      parse the list into a character array.
C
      if ( i_command_type .eq. 2) then
C
C     This is a foreach (list) type loop
C
C     Make sure the number of list tokens is not greater than nlist_max
C
      if(iargument_list_total(lp_lev) .gt. nlist_max)then
         write(logmess,
     1   "('ERROR: Number in list .gt. nlist_max =',i5)")nlist_max
         call writloga('default',0,logmess,0,ierr)
         call x3d_error
     1   (isubname,'ERROR: Increase size of nlist_max parameter')
         ierr = -1
         goto 9999
      endif
C
C    Put the list of arguments into a character variable stack.
C
      list_start = iargument_loop_end(lp_lev) -
     1             iargument_list_total(lp_lev)-1  
      do i = 1, iargument_list_total(lp_lev)
C
C     Character list variable
C
      if (msgtype(i) .eq. 3) then
       if(icharlnf(cmsgin(i+list_start)) .gt. 72)then
C
C        Error check: list tokens limited to 72 characters
C
         call x3d_error
     1   (isubname,'ERROR: list arguments can only have 72 characters')
         ierr = -1
         goto 9999
       endif
       ch_argument_list(lp_lev,i) = 
     1     cmsgin(i+list_start)(1:icharlnf(cmsgin(i+list_start)))
C
C     Real list variable
C
      elseif (msgtype(i) .eq. 2) then
         write(ch_work,100) cmsgin(i+list_start)
   80    format(e20.12)
         ch_argument_list(lp_lev,i) = ch_work(1:icharlnf(ch_work))
C
C     Integer list variable
C
      elseif (msgtype(i) .eq. 1) then
         write(ch_work,100) cmsgin(i+list_start)
   90    format(i25)
         ch_argument_list(lp_lev,i) = ch_work(1:icharlnf(ch_work))
      endif      
      enddo
      endif      
C
C------------------------------------------------------------------
C------PARSE  / lagrit_command ..... /
C      if /do/      iarg_command = argument 8
C      if /foreach/ iarg_command = argument iargument_loop_end+1
C
      if ( i_command_type .eq. 1) then
         iarg_command = 8
      else
         iarg_command = iargument_loop_end(lp_lev) + 1
      endif
      
      len_command=icharlnb(command)
      istop=0
C
C     loop_task is the LAGriT command first argument
C
      loop_task(lp_lev) = 
     1    cmsgin(iarg_command)(1:icharlnf(cmsgin(iarg_command)))
C
C     Look for the place in the command string where the lagrit_command begins
C
C     Get past the first occurence of 'loop_end' string
C
C     Find the 'loop_end'
C
C     Don't want to just look for the loop_task string because you
C     could have the same string occur earlier in the command string.
C     (i.e. loop foreach cmo_var cmo1 cmo2 loop_end cmo verify cmo_var)
C
      istop= 5
      n_loop_end_found = 0
      do while ((istop .lt. len_command) .and.
     1     ((command(istop-6:istop+1) .ne. 'loop_end') .and.
     2      (command(istop-6:istop+1) .ne. 'LOOP_END')))
            istop=istop+1
      enddo
C
C     Now get to the beginning of the 'lagrit_command' part of 'command'
C
      do while ((istop .lt. len_command) .and. 
     1  (command(istop:istop-1+icharlnf(cmsgin(iarg_command))) .ne. 
     2   loop_task(lp_lev)(1:icharlnf(cmsgin(iarg_command)))))
         istop=istop+1
      enddo
C
C    Put the command in the lagrit_command2 stack
C
      lagrit_command2(lp_lev) = 
     1       command(istop:len_command)//';finish'
      
      if(i_command_type .eq. 1)then
C
C     Error Check; Check for valid loop_start, loop_stop, loop_stride
C
      if(loop_stride(lp_lev) .eq. 0)then
         call x3d_error
     1   (isubname,'ERROR: loop_stride = 0 => infinite loop')
         ierr = -1
         goto 9999
      endif
      if(loop_stride(lp_lev) .lt. 0)then
        if(loop_start(lp_lev) .lt. loop_stop(lp_lev))then
         call x3d_error
     1   (isubname,'ERROR: No Action, check loop_start loop_start')
         ierr = -1
         goto 9999
        endif
      endif
      if(loop_stride(lp_lev) .gt. 0)then
        if(loop_start(lp_lev) .gt. loop_stop(lp_lev))then
         call x3d_error
     1   (isubname,'ERROR: No Action, check loop_start loop_start')
         ierr = -1
         goto 9999
        endif
      endif
      endif
C
C------------------------------------------------------------------
C      Construct commands the loop will execute.
C
      if ( i_command_type .eq. 1) then
C
C     Cannot use a standard:
C     do i = loop_start (lp_lev),loop_stop(lp_lev),loop_stride(lp_lev)
C     command because during recursive calls to loop_lg the i variable
C     gets hammered.
C
      i_do(lp_lev) = loop_start (lp_lev)
      dowhile(((i_do(lp_lev) .le. loop_stop(lp_lev)) .and. 
     1         (loop_stride(lp_lev) .gt. 0)) .or.
     2        ((i_do(lp_lev) .ge. loop_stop(lp_lev)) .and.
     3         (loop_stride(lp_lev) .lt. 0)))
C
C      Put the loop integer variable into a character string
C
         write(loop_value(lp_lev),100) i_do(lp_lev)
  100    format(i15)
C
C     Construct the command
C
         lagrit_command1(lp_lev) = 'define / '//
     1                     loop_variable(lp_lev)
     2         (1:icharlnf(loop_variable(lp_lev))) //
     3                     '/' //
     4                     loop_value(lp_lev)
     5         (1:icharlnf(loop_value(lp_lev))) //
     6                     ' ; finish '
C
         call dotask(lagrit_command1(lp_lev),ierr)
         call dotask(lagrit_command2(lp_lev),ierr)
C         
      i_do(lp_lev) = i_do(lp_lev) + loop_stride(lp_lev)

      enddo      
      elseif ( i_command_type .eq. 2) then
      
      i_do(lp_lev) = 1
      dowhile(i_do(lp_lev) .le. iargument_list_total(lp_lev))
C
C     Construct the command
C
        lagrit_command1(lp_lev) = 
     1    'define / '//
     2     loop_variable(lp_lev)
     3     (1:icharlnf(loop_variable(lp_lev))) //
     4    '/' //
     5     ch_argument_list(lp_lev,i_do(lp_lev))
     6     (1:icharlnf(ch_argument_list(lp_lev,i_do(lp_lev))))//
     7    ' ; finish '
C
         call dotask(lagrit_command1(lp_lev),ierr)
         call dotask(lagrit_command2(lp_lev),ierr)
C         
         i_do(lp_lev) = i_do(lp_lev) + 1
      enddo
      endif
c
c  remove define definition entry
c
         lagrit_command1(lp_lev) = 'define / '//
     1                     loop_variable(lp_lev)
     2         (1:icharlnf(loop_variable(lp_lev))) //
     3                     '/ remove' //
     6                     ' ; finish '
         call dotask(lagrit_command1(lp_lev),ierr)
      
 9999 continue
      lp_lev = lp_lev - 1
      return
      end
      