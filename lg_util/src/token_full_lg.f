C     File contains token_new_lg, token_rec_lg, token_full_lg and
C     token_warn_lg: the full text of long word tokens (file names).
C
C     A word token is kept in cmsgin with 32 characters. parse_string
C     keeps the full text of every longer word token of the command
C     (token_new_lg, token_rec_lg), up to 132 characters. A command
C     that takes a file name asks for it with token_full_lg, so names
C     up to 132 characters work. After the command, token_warn_lg
C     writes a WARNING for each long token that was used cut: those
C     no command took in full (cut to 32), and file names longer
C     than 132 characters.
C
C     The store is kept per input level (nlevels), so the commands a
C     command runs with dotask do not change its tokens.
C
C SUBROUTINE BEGIN ####################################################
      subroutine token_new_lg()
C #####################################################################
C     PURPOSE - Forget the long tokens of the level's last command;
C     parse_string calls it before it parses a new command.
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'token_full_lg.h'
C
      if(nlevels.ge.1 .and. nlevels.le.maxnlevels) ltok_n(nlevels)=0
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine token_rec_lg(iwd,ctext,length)
C #####################################################################
C     PURPOSE - Keep a word token longer than 32 characters.
C
C     iwd     its place in the command (cmsgin(iwd))
C     ctext   its text; length its length (all of it is kept up to
C             LENLTOK characters)
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'token_full_lg.h'
      integer iwd,length
      character*(*) ctext
      integer k,lev
C
      lev=nlevels
      if(lev.lt.1 .or. lev.gt.maxnlevels) return
      if(ltok_n(lev).ge.MAXLTOK) return
      k=ltok_n(lev)+1
      ltok_n(lev)=k
      ltok_iwd(k,lev)=iwd
      ltok_len(k,lev)=length
      ltok_use(k,lev)=0
      ltok_txt(k,lev)=' '
      ltok_txt(k,lev)=ctext(1:min(length,LENLTOK,len(ctext)))
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine token_full_lg(iwd,ctok,cfull,lfull)
C #####################################################################
C     PURPOSE - The full text of a token, for a file name.
C
C     iwd     the token's place in the command (cmsgin(iwd))
C     ctok    the token as the command has it (cmsgin(iwd), 32
C             characters)
C     cfull   (out) the full text, up to len(cfull) and 132
C             characters; ctok itself when the token is not longer
C             than 32 characters, or when it does not come from the
C             command line as written (a define's value, or a token
C             the command made itself)
C     lfull   (out) the length of cfull's text
C
C     The token is found by its text: the one kept at place iwd whose
C     first 32 characters are ctok, else any kept token whose first 32
C     characters are ctok (a command may have moved its tokens), if
C     all such have the same text.
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'token_full_lg.h'
      integer iwd,lfull
      character*(*) ctok,cfull
      integer k,lev,ifound,lc,icharlnf
C
      cfull=ctok
      lfull=icharlnf(ctok)
      if(lfull.gt.len(cfull)) lfull=len(cfull)
      lev=nlevels
      if(lev.lt.1 .or. lev.gt.maxnlevels) return
      lc=len(ctok)
      if(lc.lt.32 .or. lfull.lt.32) return
      ifound=0
      do k=1,ltok_n(lev)
         if(ltok_iwd(k,lev).eq.iwd .and.
     &      ltok_txt(k,lev)(1:32).eq.ctok(1:32)) ifound=k
      enddo
      if(ifound.eq.0) then
         do k=1,ltok_n(lev)
            if(ltok_txt(k,lev)(1:32).eq.ctok(1:32)) then
               if(ifound.eq.0) then
                  ifound=k
               elseif(ltok_txt(k,lev).ne.ltok_txt(ifound,lev)) then
                  return
               endif
            endif
         enddo
      endif
      if(ifound.eq.0) return
      k=ifound
      lfull=min(ltok_len(k,lev),LENLTOK,len(cfull))
      cfull=ltok_txt(k,lev)(1:lfull)
      ltok_use(k,lev)=max(ltok_use(k,lev),lfull)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine token_warn_lg()
C #####################################################################
C     PURPOSE - After a command: a WARNING for each long word token
C     that was used cut (not taken in full by the command: cut to 32
C     characters; a file name over 132 characters: cut to 132). Then
C     forget the level's long tokens.
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'token_full_lg.h'
      integer k,lev,iuse,ierrw
      character*132 cmsg
C
      lev=nlevels
      if(lev.lt.1 .or. lev.gt.maxnlevels) return
      do k=1,ltok_n(lev)
         iuse=ltok_use(k,lev)
         if(iuse.lt.ltok_len(k,lev)) then
            if(iuse.eq.0) iuse=32
            write(cmsg,'(a,i0,a,i0,a,i0,a)') 'WARNING: token ',
     &        ltok_iwd(k,lev),' has ',ltok_len(k,lev),
     &        ' characters; LaGriT uses the first ',iuse,':'
            call writloga('default',0,cmsg,0,ierrw)
            if(iuse.le.70) then
               write(cmsg,'(a,a,a)') '  "',ltok_txt(k,lev)(1:iuse),'"'
            else
               write(cmsg,'(a,a,a)') '  "',ltok_txt(k,lev)(1:60),
     &           ' ..."'
            endif
            call writloga('default',0,cmsg,0,ierrw)
         endif
      enddo
      ltok_n(lev)=0
      return
      end
