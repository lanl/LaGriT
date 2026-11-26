C #####################################################################
C
C     token_full_lg.h
C
C     The full text of the long word tokens of the command being run
C     at each input level, for token_full_lg.f. Include AFTER
C     commands_lg.h (it uses maxnlevels).
C
C     A token is kept in cmsgin with 32 characters. parse_string keeps
C     here every word token longer than that (up to MAXLTOK of them
C     per command, LENLTOK characters each), so a command that takes
C     a file name can get it in full with token_full_lg. After the
C     command, token_warn_lg writes a WARNING for each long token that
C     was not taken in full.
C
C     MAXLTOK           long tokens kept per command
C     LENLTOK           characters kept of a long token (a file name)
C     ltok_n(lev)        how many are kept for the command at level lev
C     ltok_iwd(k,lev)    the token's place in the command (cmsgin)
C     ltok_len(k,lev)    its length in the command
C     ltok_use(k,lev)    characters a command took (0: none, so the
C                        token was used cut to 32)
C     ltok_txt(k,lev)    its text (the first LENLTOK characters)
C
C #####################################################################
      integer MAXLTOK, LENLTOK
      parameter (MAXLTOK = 64)
      parameter (LENLTOK = 132)
C
      integer ltok_n(maxnlevels), ltok_iwd(MAXLTOK,maxnlevels),
     &   ltok_len(MAXLTOK,maxnlevels), ltok_use(MAXLTOK,maxnlevels)
      character*132 ltok_txt(MAXLTOK,maxnlevels)
      common /token_full_lgi/ ltok_n, ltok_iwd, ltok_len, ltok_use
      common /token_full_lgc/ ltok_txt
      save /token_full_lgi/, /token_full_lgc/
