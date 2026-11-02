C #####################################################################
C
C     input_lines_lg.h
C
C     Limits on LaGriT input lines, and the line count of each input
C     level, for read_command_lg and read_line_lg. Include AFTER
C     commands_lg.h (it uses maxnlevels).
C
C     LINE_NOTE_LEN  a command line longer than this is used in full
C                    but a NOTE says so: LaGriT V3.3.4 and earlier read
C                    only the first 80 columns and ignored the rest
C                    without a word
C     LINE_MAX_LEN   a line longer than this is an ERROR and its
C                    command is skipped (never cut, never run in part).
C                    Longer commands are split over lines ending in &.
C                    The joined command is limited by maxlen_buff
C                    (commands_lg.h).
C     nline_lg(n)    lines read so far at input level n (1 is the
C                    terminal or the main input; infile adds a level);
C                    for messages
C
C #####################################################################
      integer LINE_NOTE_LEN, LINE_MAX_LEN
      parameter (LINE_NOTE_LEN = 80)
      parameter (LINE_MAX_LEN = 1024)
C
      integer nline_lg(maxnlevels)
      common /input_lines_lg/ nline_lg
      save /input_lines_lg/
