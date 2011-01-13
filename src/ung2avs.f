*DK ung2avs
      subroutine ung2avs(imsgin,xmsgin,cmsgin,msgtype,nwds,ics)
C
C#######################################################################
C
C      PURPOSE -
C
C      This routine is used to convert files in UNGenerate format
C      to AVS format. It allows the user to input a constant value for
C      the third coordinate. The default is 0.0. The xyz can be
C      reordered in any combination when written to the AVS file by
C      defining the two columns of coordinate date in the ung2avs
C      command to be xy, xz, yx, zx, yz, or zy. The default is xy.
C
C          FORMAT: UNG2AVS/avs_file_out/ung_file_in/[constant]/ &
C                                      [xy, xz, yx, zx, yz, zy]
C
C                 default is:
C                 UNG2AVS/avs_file_out/ung_file_in
C                     converts ung_file_in to avs_file_out
C                     with a constant default z value of 0.0
C                     in xy order
C                       or
C                 UNG2AVS/avs_file_out/ung_file_in/7.0
C                     converts ung_file_in to avs_file_out
C                     with a constant default z value of 7.0
C                     in xy order
C                       or
C                 UNG2AVS/avs_file_out/ung_file_in/15.0/xz
C                     converts ung_file_in to avs_file_out
C                     with a constant y value of 15.0
C                     in xz order
C
C          UNG FILE FORMAT:
C              The UNG format contains a file of xy values, in groups of
C              sequentially numbered sets, of connected line segments,
C              each ending with END. The format for connected lines that
C              form polygons or that do not form polygons is very similar.
C              To make sets of connected lines that are polygons the first
C              xy pair for each set of lines must be on the same line as
C              the set number. Closed polygons will be formed by connecting
C              the last point of a point set to the first point of the
C              point set. The set number is on a line by itself for
C              non-polygons. Spacing on a line does not matter.
C
C              FORMAT for connected lines that do not form polygons:
C
C                     1
C                       x   y
C                       x   y
C                       x   y
C                         ...
C                     END
C                     2
C                       x   Y
C                       x   y
C                       x   y
C                         ...
C                     END
C                         ...
C                     n
C                       x   y
C                       x   y
C                         ...
C                     END
C                     END
C
C              FORMAT for polygons:
C
C                     1 x   y
C                       x   y
C                       x   y
C                         ...
C                     END
C                     2 x   y
C                       x   y
C                       x   y
C                         ...
C                     END
C                         ...
C                     n x   y
C                       x   y
C                         ...
C                     END
C                     END
C
C              SAMPLE of connected, non-polygon, lines:
C
C                  UNG file                          AVS file
C                1                           10  7 0 0 0
C                  10.00  15.00               1   10.0000 15.0000  0.0000
C                  10.50  15.50               2   10.5000 15.5000  0.0000
C                  11.00  16.00               3   11.0000 16.0000  0.0000
C                END                          4   20.0000 25.0000  0.0000
C                2                            5   20.5000 25.5000  0.0000
C                  20.00  25.00               6   21.0000 26.0000  0.0000
C                  20.50  25.50               7   22.0000 27.0000  0.0000
C                  21.00  26.00               8   30.0000 35.0000  0.0000
C                  22.00  27.00               9   30.5000 35.5000  0.0000
C                END                         10   31.0000 36.0000  0.0000
C                3                           1  1 line    1    2
C                  30.00  35.00              2  1 line    2    3
C                  30.50  35.50              3  2 line    4    5
C                  31.00  36.00              4  2 line    5    6
C                END                         5  2 line    6    7
C                END                         6  3 line    8    9
C                                            7  3 line    9   10
C
C              SAMPLE of polygons:
C
C                  UNG file                          AVS file
C                1 10.00  15.00              10  10  0 0 0
C                  10.50  15.50               1   10.0000 15.0000  0.0000
C                  11.00  16.00               2   10.5000 15.5000  0.0000
C                END                          3   11.0000 16.0000  0.0000
C                2 20.00  25.00               4   20.0000 25.0000  0.0000
C                  20.50  25.50               5   20.5000 25.5000  0.0000
C                  21.00  26.00               6   21.0000 26.0000  0.0000
C                  22.00  27.00               7   22.0000 27.0000  0.0000
C                END                          8   30.0000 35.0000  0.0000
C                3 30.00  35.00               9   30.5000 35.5000  0.0000
C                  30.50  35.50              10   31.0000 36.0000  0.0000
C                  31.00  36.00              1  1 line    1    2
C                END                         2  1 line    2    3
C                END                         3  1 line    3    1
C                                            4  2 line    4    5
C                                            5  2 line    5    6
C                                            6  2 line    6    7
C                                            7  2 line    7    4
C                                            8  3 line    8    9
C                                            9  3 line    9   10
C                                           10  3 line   10    8
C
C      INPUT ARGUMENTS -
C
C         NONE
C
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C
C      CHANGE HISTORY -
C
C         $Log: ung2avs.f,v $
C         Revision 2.00  2007/11/09 20:04:05  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   18 May 2006 15:04:56   gable
CPVCS    Modified output format so code will work with more than 9999 elements.
CPVCS    
CPVCS       Rev 1.5   26 Feb 2004 10:58:04   dcg
CPVCS    use explicit size for temporary arrays - linux will
CPVCS    not compile with nwds as size
CPVCS
CPVCS       Rev 1.4   26 Nov 2003 11:41:34   gable
CPVCS    Changed coordinate arrays to double precision arrays.
CPVCS    Modified output format statement to more significant figures
CPVCS    and changed some integer output formats so they would work
CPVCS    with bigger integers.
CPVCS
CPVCS       Rev 1.3   Mon Dec 13 12:49:00 1999   llt
CPVCS    modified documentation
CPVCS
CPVCS       Rev 1.2   Wed Dec 08 13:39:40 1999   llt
CPVCS    added option to allow for all combinations of the xyz ordering
CPVCS
CPVCS       Rev 1.1   Fri Jan 22 14:43:56 1999   llt
CPVCS    modified header
CPVCS
CPVCS       Rev 1.0   Fri Jan 22 14:28:16 1999   llt
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
c
c      include "local_element.h"
C
C#######################################################################
C
c parser variables for subroutine call
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
 
      integer ics, ierror, icscode
      character*40 isubname
c
      integer iunit_in, iunit_out
      logical trueend, polytrue, firstmaterial
      integer nnodes, nmat, i, mat_num, j, i_save, length
      pointer (ipmat_nums, mat_nums(1000000))
      integer mat_nums
      real*8 z_value, x_save
      pointer (ipx_value, x_value(1000000))
      pointer (ipy_value, y_value(1000000))
      real*8 x_value, y_value
      pointer (ipline_end, line_end(10000000))
      logical line_end
      character*2 order
 
c local parser variables
      character*250 input_msg
      integer lenparse
      integer msg(10)
      real*8 xmsg(10)
      integer imsg(10)
      character*32 cmsg(10)
C
C
C#######################################################################
C
      character*132 logmess
C
C#######################################################################
C
      isubname='ung2avs'
C
      if(nwds .eq. 3 .or. nwds .eq. 4 .or. nwds .eq. 5) then
        call hassign (iunit_out, cmsgin(2), ierror)
        if (iunit_out.lt.0 .or. ierror.lt.0) then
          call x3d_error(isubname,'out hassign bad file unit')
          goto 9999
        endif

        call hassign (iunit_in, cmsgin(3), ierror)
        if (iunit_in.lt.0 .or. ierror.lt.0) then
          call x3d_error(isubname,'in  hassign bad file unit')
          goto 9999
        endif

        if (nwds .eq. 4 .or. nwds .eq. 5) then
          z_value = xmsgin(4)
        else
          z_value = 0.000
        end if
        order = 'xy'
        if (nwds .eq. 5) then
          if (cmsgin(5) .eq. 'xy') then
            order = 'xy'
          elseif (cmsgin(5) .eq. 'xz') then
            order = 'xz'
          elseif (cmsgin(5) .eq. 'yx') then
            order = 'yx'
          elseif (cmsgin(5) .eq. 'zx') then
            order = 'zx'
          elseif (cmsgin(5) .eq. 'yz') then
            order = 'yz'
          elseif (cmsgin(5) .eq. 'zy') then
            order = 'zy'
          else
            order = 'xy'
          end if
        end if
 
      else
         write(logmess,3000)
 3000    format(' UNG2AVS / file_out / file_in / [z_value] ')
         call writloga('default',0,logmess,0,ics)
      endif
C
c initialize values
c
      trueend = .false.
      nnodes = 0
      nmat = 0
      firstmaterial = .true.
      polytrue = .false.
      nwds = 0
 
c read file to calculate maximum number of nodes -- for memory management
      do length=1, 1000000
         read(iunit_in,'(a)', end=90) input_msg
      end do
 90   continue
      rewind (iunit_in)
 
c allocate memory
      call mmgetblk ('mat_nums',isubname,ipmat_nums,length,2,icscode)
      call mmgetblk ('x_value',isubname,ipx_value,length,2,icscode)
      call mmgetblk ('y_value',isubname,ipy_value,length,2,icscode)
      call mmgetblk ('line_end',isubname,ipline_end,length,2,icscode)
 
c read and parse string
      do i=1, 1000000
         read(iunit_in,'(a)', end=100) input_msg
         lenparse = len(input_msg)
 
         call parse_string2(lenparse, input_msg,
     .                     imsg,msg,xmsg,cmsg,nwds)
 
c check to see if at a new material
         if ((nwds .eq. 1 .or. nwds .eq. 3) .and.
     .        (cmsg(1)(1:3) .ne. 'END')) then
           trueend = .false.
           mat_num = imsg(1)
           nmat = nmat + 1
           if (nwds .eq. 3) then
             polytrue = .true.
             nnodes = nnodes + 1
             mat_nums(nnodes) = mat_num
             x_value(nnodes) = xmsg(2)
             y_value(nnodes) = xmsg(3)
             line_end(nnodes) = .false.
           endif
c check to see if at END
         else if ((nwds .eq. 1) .and.
     .            (cmsg(1)(1:3) .eq. 'END') .and.
     .            (.not. trueend)) then
           trueend = .true.
           line_end(nnodes) = .true.
c check to see if at END END
         else if ((nwds .eq. 1) .and. (cmsg(1)(1:3) .eq. 'END') .and.
     .            trueend) then
           goto 100
c calculate number of nodes and write x,y
         elseif (nwds .eq. 2) then
            if (polytrue) then
              if (firstmaterial) then
                x_save = xmsg(1)
                firstmaterial = .false.
              endif
            endif
            nnodes = nnodes + 1
            mat_nums(nnodes) = mat_num
            x_value(nnodes) = xmsg(1)
            y_value(nnodes) = xmsg(2)
            line_end(nnodes) = .false.
         end if
      end do
      print*, 'ERROR, loop size needs to be increased'
 
 100  continue
c
 
c WRITE AVS FILE
c
c   write header
      if (polytrue) then
        write (iunit_out, *) nnodes, nnodes, ' 0 0 0'
      else
        write (iunit_out, *) nnodes, ' ', nnodes-nmat, ' 0 0 0 '
      end if
 
c   write xyz values
 
        if (order .eq. 'xy') then
          do i=1, nnodes
            write(iunit_out, 110) i, x_value(i), y_value(i), z_value
          end do
        elseif (order .eq. 'xz') then
          do i=1, nnodes
            write(iunit_out, 110) i, x_value(i), z_value, y_value(i)
          end do
        elseif (order .eq. 'yx') then
          do i=1, nnodes
            write(iunit_out, 110) i, y_value(i), x_value(i), z_value
          end do
        elseif (order .eq. 'zx') then
          do i=1, nnodes
            write(iunit_out, 110) i, y_value(i), z_value, x_value(i)
          end do
        elseif (order .eq. 'yz') then
          do i=1, nnodes
            write(iunit_out, 110) i, z_value, x_value(i), y_value(i)
          end do
        elseif (order .eq. 'zy') then
          do i=1, nnodes
            write(iunit_out, 110) i, z_value, y_value(i), x_value(i)
          end do
        end if
 
 110  format(i10, 2x, 3(1pe20.12))
 
c   write connectivity
      j = 1
      i_save = 1
      do i=1,nnodes
        if (.not. line_end(i)) then
          write(iunit_out, 120) j, mat_nums(i),
     .                        i,  i+1
 120      format (i10, 2x, i6, ' line ', i10, 2x, i10)
           j = j + 1
        else
          if (polytrue) then
            write(iunit_out, 120) j, mat_nums(i),
     .                          i, i_save
            i_save = i+1
            j = j + 1
          end if
        end if
      end do
 
c deallocate memory

 9999 continue

      call mmrelblk ('mat_nums',isubname,ipmat_nums,icscode)
      call mmrelblk ('x_value',isubname,ipx_value,icscode)
      call mmrelblk ('y_value',isubname,ipy_value,icscode)
      call mmrelblk ('line_end',isubname,ipline_end, icscode)
 
      if (iunit_out.gt.0) close (iunit_out)
      if (iunit_in.gt.0) close (iunit_in)
 
      return
      end
