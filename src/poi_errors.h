C #####################################################################
C
C     poi_errors.h
C
C     State for the Poisson disk error accumulator.
C
C     USAGE - include AFTER poi_params.h, which defines POI_MSG_LEN:
C
C        implicit none
C        include 'poi_params.h'
C        include 'poi_errors.h'
C
C     Only the routines that detect or report input problems need
C     this. Routines that merely do work should include poi_params.h
C     alone, so it stays visible which parts of the file participate
C     in error handling and which do not.
C
C ---------------------------------------------------------------------
C
C     WHY AN ACCUMULATOR
C
C     The original code failed on the first bad token, so a command
C     with three mistakes in it took three edit-and-rerun cycles to
C     diagnose. The one exception was the 3D bounding box check, which
C     deliberately collected all six missing define variables and
C     reported them together. That is the better behavior, and this
C     generalizes it to the whole input phase.
C
C     The contract is:
C
C        poi_err_reset          once, before any checking
C        poi_err_add   (msg)    zero or more times, as faults are found
C        poi_err_report(ierror) once, at the end of the checking phase
C
C     poi_err_report emits every accumulated message inside ONE error
C     block and sets ierror to -1 if anything was accumulated, or 0 if
C     nothing was. Callers therefore branch on ierror exactly once per
C     phase instead of after every individual check.
C
C     Detection is thereby separated from reporting: a validation
C     routine states what is wrong and returns, and has no opinion
C     about how or when that reaches the log.
C
C ---------------------------------------------------------------------
C
C     NOT THREAD SAFE, AND NOT REENTRANT.
C
C     LaGriT drives one command at a time from a single thread, so a
C     COMMON block is the idiomatic choice here and matches how the
C     rest of the code carries global state. If poisson_disk is ever
C     called recursively, or from more than one thread, this must be
C     revisited.
C
C #####################################################################
C
C     poi_nerr        number of messages currently accumulated
C     poi_nerr_lost   messages dropped because the buffer was full
C     poi_errmsg      the accumulated message text
C
C     Numeric and character data are kept in separate COMMON blocks.
C     Mixing them in one block is not standard Fortran.
C ---------------------------------------------------------------------
      integer POI_ERR_MAX
      parameter (POI_ERR_MAX = 40)
C
      integer poi_nerr,poi_nerr_lost
      common /poi_err_num/ poi_nerr,poi_nerr_lost
C
      character*(POI_MSG_LEN) poi_errmsg(POI_ERR_MAX)
      common /poi_err_chr/ poi_errmsg
C
C     END poi_errors.h
