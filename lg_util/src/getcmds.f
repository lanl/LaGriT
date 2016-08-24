*dk,getcmds
      subroutine getcmds(n,imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                   imsgprmp)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE GETS AND PROCESS COMMANDS EITHER IN AN
C            INTERACTIVE MODE OR FROM A COMMAND FILE.
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
C        $Log: getcmds.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.18   01 Jun 2001 16:29:00   dcg
CPVCS    get rid of upper case
CPVCS    make implicit none
CPVCS
CPVCS       Rev 1.17   Tue Apr 13 10:49:20 1999   dcg
CPVCS    changes for new parser
CPVCS
CPVCS       Rev 1.16   Sat Jan 10 07:08:24 1998   het
CPVCS    Add the getcmds_msg command to retrive the next message
CPVCS       from whatever source is available.
CPVCS
CPVCS       Rev 1.15   Thu Sep 18 13:29:04 1997   het
CPVCS    Correct an error with the length of cmsgout(1)
CPVCS
CPVCS       Rev 1.14   Mon Sep 15 13:18:42 1997   het
CPVCS    Add the title and comment card options.
CPVCS
CPVCS       Rev 1.12   Wed Mar 06 16:28:48 1996   dcg
CPVCS    include string length in calls to parse_string
CPVCS
CPVCS       Rev 1.11   12/08/95 14:05:32   dcg
CPVCS    fix blank before continuation
CPVCS
CPVCS       Rev 1.10   11/27/95 11:10:34   het
CPVCS    Make UNICOS changes
CPVCS
CPVCS       Rev 1.9   05/24/95 15:46:10   het
CPVCS    Change the write format for file names
CPVCS
CPVCS       Rev 1.8   03/21/95 12:31:14   dcg
CPVCS
CPVCS       Rev 1.7   02/23/95 13:43:02   ejl
CPVCS    Fisxed problem with blank line input from TTy.
CPVCS    Also fixed default option COMMAND/////input/.
CPVCS
CPVCS       Rev 1.6   02/23/95 13:12:40   ejl
CPVCS    Fixed problem with Blank Line input.
CPVCS
CPVCS       Rev 1.5   02/07/95 13:06:38   het
CPVCS    Correct an error with ibuff (-1)
CPVCS
CPVCS       Rev 1.4   01/23/95 17:30:46   dcg
CPVCS
CPVCS
CPVCS       Rev 1.3   01/04/95 22:00:06   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   01/02/95 10:54:32   het
CPVCS    Change the file name references to 32 characters instead
CPVCS    of 4096 characters (this gives the SGI trouble).
CPVCS
CPVCS
CPVCS       Rev 1.1   12/21/94 11:14:36   het
CPVCS    Corrected an error with adding char(0) as the terminating character.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:41:48   pvcs
CPVCS    Original version.
C
C#######################################################################
      implicit none
C
c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 junitcmd 
C
C     THE FOLLOWING ARRAYS ARE FOR JACK PETERSON'S PARSER.
C
      integer nd,nsd,nwrds,nchrwrd,lmsgt,n,imsgout(*),
     *  msgtype(*),nwds,lenend,nwds1,nwds2,ib2,jname,
     *  ierrass,lenstart,ib,li,loc,locp,locm,ist,iet1,
     *  ibt1,ierrlog,nummsg,lenparse,ipos,iflag,ie,i,
     *  icount,iflag2,ist1,ist2,length2,length1,idum,
     *  length,icode,icharlnb,ie1,ib1,icont,
     * icharlnf,lenbuff,i1,lmsg,nwdsold,
     * iqchrn,itthr,ipause,istack,ictcmd,
     * icommand,jcommand,lcommand,len1,iecho,ierr,isvpause,
     * nchrsav,imsgold
 
      PARAMETER ( nd=4 , nsd=1 )
      common /getcmdsd/ id(nd)
      character*1 id
C
      character*1 kchar
C
C     THESE ARRAYS CONTAIN THE COMPLETE MESSAGE AS READ FROM THE
C     TERMINAL
C
      PARAMETER ( nwrds = 128 , nchrwrd = 8 )
      PARAMETER ( lmsgt=nwrds*nchrwrd)
C
C     THESE ARRAYS CONTAIN THE LAST MESSAGE PROCESSED SO THAT IT CAN BE
C     REPEATED.
C
      COMMON /GETCMDS2/ lmsg
      COMMON /GETCMDSA/ msgb, msgc
      character*4096 msgb, msgc
      COMMON /GETCMDS3/ nwdsold, imsgold(nwrds)
      COMMON /GETCMDSB/ msgold
      character*4096 msgold
C
C     THESE ARRAYS WILL CONTAIN THE INDIVIDUAL COMMANDS THAT ARE PULLED
C     OFF STACKS.
C
      real*8 xmsgout(n)
      character*32 cmsgout(n), ccommand
C
C     THIS COMMON PROVIDES SPACE FOR A CFTLIB MESSAGE BUFFER.
C     NOTE: THIS BUFFER IS 64 WORDS LONG FOLLOWED BY 1 WORD THAT
C           INDICATES HOW MANY CHARACTERS ARE IN THE BUFFER.  IF THE
C           FIRST WORD EQUALS -1, THEN NO MESSAGE IS PRESENT.
C
 
      COMMON /QMBUFFC0/ iqchrn
      COMMON /QMBUFFC1/ ibuff
      character*4096 ibuff
C
      COMMON /MSGSAVE0/ nchrsav
      COMMON /MSGSAVE1/ msgsav
      character*4096 msgsav
C
      COMMON /GETCMDS4/ itthr, ipause, istack
C
      COMMON /GETCMDSE/ kcommand(10)
      character*32 kcommand, iname
      COMMON /GETCMDS5/ ictcmd, icommand, jcommand, lcommand
C
C     PROVIDE SPACE (IN LOW-CORE [<2,000,000]) FOR AN I/O BUFFER.
C
      COMMON /GETCOM1/ isvpause
C
C#######################################################################
C
      CHARACTER*132 interfil
      CHARACTER*80 imessage
      CHARACTER imsgprmp*(*)
      character*80 iline
      character*32 ifile
C
C#######################################################################
C
CCHT DECLARE THE FUNCTION ALLBLK (ALL BLANK) TO BE LOGICAL
C
C
C#######################################################################
C
C
C#######################################################################
C
C
C     ******************************************************************
C
C     INITIALIZE VARIABLES IN COMDECK getcmdsc.
C
      data iecho / 1 /
C
C#######################################################################
C
ccht
ccht
ccht
ccht set the prompt message
ccht
      imessage='nomessage'
      if( imsgprmp .ne. ' ' ) imessage = imsgprmp
ccht
      imessage='nomessage'
      if( imsgprmp .ne. ' ' ) imessage = imsgprmp
ccht
ccht
      if(itthr.eq.0) then
         itthr=1
         ictcmd=0
         ipause=0
         istack=0
         icommand=0
         jcommand=0
         lcommand=0
         id(1)=','
         id(2)='/'
         id(3)='='
         id(4)=':'
      endif
ccht
      goto 101
 100  goto 9998
 101  continue
ccht
ccht blank out the message array
ccht
      nwds=0
      do 200 i1=1,n
         msgtype(i1)=0
         imsgout(i1)=0
         xmsgout(i1)=0.0d+00
         cmsgout(i1)=' '
 200  continue
      msgc=' '
ccht
      if(istack.eq.-1) then
         istack=0
         goto 100
      elseif(istack.eq.0) then
         lenbuff=icharlnf(ibuff)
         if(jcommand.eq.0.or.ipause.ne.0) then
            if(imessage.eq.'nomessage') then
               imessage='enter an interactive command'
            endif
C
C
C    GET COMMANDS IN AN INTERACTIVE MODE.
C
            if(ibuff(1:lenbuff).eq.'-1') then
               if(imessage.eq.'*empty*') goto 100
                  write(interfil,9000) imessage
                  call writloga('default',1,interfil,0,ierr)
 9000             format(' ',a80)
               icont=0
 220           continue
C*****         call rcvtty
               read(*,'(a80)',end=100) ibuff
 221           continue
               icont=index(ibuff,'&')
               if(icont.gt.0) then
                  ibuff(icont:icont)=' '
                  dowhile(ibuff((icont-2):(icont-2)).eq.' ')
                    icont=icont-1
                  enddo
                  read(*,'(a80)') iline
                  ib1=1
                  ie1=icharlnb(iline)
                  dowhile(iline(ib1:ib1).eq.' ')
                     ib1=ib1+1
                  enddo
                  ibuff(icont:icont+ie1-ib1)=iline(ib1:ie1)
                  if(icont.gt.0) goto 221
               endif
               iqchrn=icharlnb(ibuff)
               if(ibuff(1:iqchrn).eq.'-1') goto 220
            endif
            icode=0
            lmsg=iqchrn
            msgb = ' '
            msgb(1:iqchrn)=ibuff(1:iqchrn)
            ibuff='-1'
            if(icode.eq.2) goto 101
         elseif(jcommand.gt.0 .and. ibuff(1:lenbuff).ne.'-1') then
            msgb = ' '
            msgb=ibuff(1:iqchrn)
            lmsg = iqchrn
            ibuff='-1'
         else
            ipause=0
            jcommand=2
            ifile=kcommand(lcommand)
            length=icharlnf(ifile)
            inquire(file=ifile(1:length),number=junitcmd)
C              *** GET THE LOGICAL UNIT NUMBER ASSOCAITED WITH THIS FILE
            icode=0
            msgb=' '
            read(junitcmd,'(a80)',end=202) msgb(1:80)
 222        continue
            icont=index(msgb,'&')
            if(icont.gt.0) then
               msgb(icont:icont)=' '
               dowhile(msgb((icont-2):(icont-2)).eq.' ')
                  icont=icont-1
               enddo
               read(junitcmd,'(a80)',end=202) iline
               ib1=1
               ie1=icharlnb(iline)
               dowhile(iline(ib1:ib1).eq.' ')
                  ib1=ib1+1
               enddo
               msgb(icont:icont+ie1-ib1)=iline(ib1:ie1)
               goto 222
            endif
            goto 201
 202        continue
            icode=2
 201        continue
            iqchrn=icharlnb(msgb)
            do idum=1,iqchrn
               if(msgb(iqchrn:iqchrn).eq.' ') then
                  iqchrn=iqchrn-1
                  goto 203
               endif
            enddo
 203        continue
            lmsg=iqchrn
            if(msgb(1:7).eq.'endfile') icode=2
            if(icode.eq.2) then
               close(junitcmd)
               write(interfil,9010) lcommand,
     *         kcommand(lcommand)(1:icharlnf(kcommand(lcommand)))
               call writloga('default',1,interfil,0,ierr)
 9010          format(' ','Closing command file:  ',i1,' - ',a)
               lcommand=lcommand-1
               if(lcommand.eq.0) then
                  icommand=0
                  jcommand=0
                  ipause=isvpause
                  if(ipause.eq.0) then
                     goto 100
                  else
                     goto 101
                  endif
               else
                  write(interfil,9011) lcommand,
     *               kcommand(lcommand)(1:icharlnf(kcommand(lcommand)))
                  call writloga('default',0,interfil,1,ierr)
 9011             format(' ','Continue reading from:  ',i1,' - ',a)
                  goto 101
               endif
            endif
         endif
      endif
      lenbuff=icharlnf(ibuff)
       if(ibuff(1:lenbuff).ne.'-1') then
          length1=iqchrn
          length2=length1+4
          msgb(length2:(length2+lmsg))=msgb(1:lmsg)
          msgb(1:length1)=ibuff(1:length1)
          msgb((length1+1):(length1+1+3))=' ; '
          lmsg = lmsg + length1 + 3
          ibuff='-1'
       endif
       if(lmsg.le.0) goto 101
       do 120 i1=1,nd
          ist2=1
 110      continue
          ist1=index(msgb(ist2:lmsg),id(i1))
          if(ist1.eq.0) then
             ist1=lmsg+1
          else
             ist1=ist1 + (ist2-1)
          endif
          if(ist1.ge.lmsg) goto 115
          ist2=index(msgb(ist1+1:lmsg),id(i1))
          if(ist2.eq.0) then
             ist2=lmsg+1
          else
             ist2=ist2 + ist1
          endif
          if(ist2.gt.lmsg) goto 115
          iflag2=0
          if(ist1+1.eq.ist2) iflag2=1
          icount=0
          do i=ist1+1,ist2-1
             if(msgb(i:i).eq.' ') icount=icount+1
          enddo
          if(icount.eq.(ist2-1-ist1)) iflag2=1
          if(iflag2.eq.1) then
             msgc=' -def-'
             ie=lmsg-ist1+6
             msgc(7:ie)=msgb((ist1+1):lmsg)
             lmsg=lmsg+6-1
             msgb(ist1:lmsg)=msgc(1:ie)
          endif
          goto 110
 115      continue
          iflag=0
          do 140 i=1,lmsg
             if(msgb(i:i).eq."'".or.msgb(i:i).eq."'") iflag=iflag+1
             if(mod(iflag,2).eq.0.and.msgb(i:i).eq.id(i1)) msgb(i:i)=' '
 140     continue
 120  continue
      ipos=index(msgb,';')
      if(ipos.eq.0) ipos=lmsg+1
      if(ipos.lt.lmsg) then
         istack=1
         length=ipos-1
         msgc=' '
         msgc(1:length)=msgb(1:length) // char(0)
      lenparse=length
         length=lmsg-ipos
         msgold=' '
         msgold(1:length)=msgb((ipos+1):lmsg) // ' '
         msgb=' '
         msgb(1:length)=msgold(1:length)
         lmsg=lmsg-ipos
      else
         if(jcommand.eq.0) then
            if(ipause.eq.0) then
               istack=-1
            else
               istack=0
            endif
         else
            istack=0
         endif
         msgc(1:lmsg)=msgb(1:lmsg) // char(0)
        lenparse=lmsg
      endif
      ictcmd=ictcmd+1
      nummsg=int((ipos-1)/nchrwrd)+1
      if(iecho.eq.1) then
         write(interfil,9040) ictcmd,msgc(1:8*min(9,nummsg))
         call writloga('default',0,interfil,0,ierr)
 9040    format(' ',i5,2x,a)
         if(nummsg.gt.9) then
            do 102 i=10,nummsg,9
               write(interfil,9041) msgc(8*(i-1)+1:8*min(i+8,nummsg))
               call writloga('default',0,interfil,0,ierr)
 9041          format(' ',7x,a)
 102        continue
         endif
      endif
      interfil=' '
      call writset('stat','log','on',ierrlog)
      if(nummsg.gt.9) then
         write(interfil,9042) msgc(1:8*min(9,nummsg))
         call writloga('log',0,interfil,0,ierr)
 9042    format(a," &")
         do 103 i=10,nummsg,9
            if(nummsg.gt.(i+8)) then
               write(interfil,9044) msgc(8*(i-1):8*min(i+8,nummsg))
               call writloga('log',0,interfil,0,ierr)
 9044          format(3x,a," &")
            else
               write(interfil,9045) msgc(8*(i-1):8*min(i+8,nummsg))
               call writloga('log',0,interfil,0,ierr)
 9045          format(3x,a)
            endif
 103     continue
      else
         write(interfil,'(a6)') msgc(1:6)
         if(interfil(1:6).eq.'infile') then
            write(interfil,9046) msgc(1:8*min(9,nummsg))
            call writloga('log',0,interfil,0,ierr)
 9046       format('* ',a)
         else
            write(interfil,9043) msgc(1:8*min(9,nummsg))
            call writloga('log',0,interfil,0,ierr)
 9043       format(a)
         endif
      endif
      call writfls('log',ierrlog)
      call writset('stat','log','off',ierrlog)
      call writfls('bat',ierrlog)
C
C
C     CHANGE LOWER CASE E IN NUMBERS TO UPPER CASE; PARSER
C     WON'T ACCEPT LOWER CASE E'S.
C
C
      ibt1=1
      iet1=ipos-1
      ist=ibt1
  150 continue
C
C
C     LOOK FOR AN 'e'
C
C
      locm=index(msgc(ist:iet1),'e-')
      locp=index(msgc(ist:iet1),'e+')
      if(locm.gt.0.or.locp.gt.0) then
         loc=max(locm,locp)
         loc = loc + (ist-1)
C
C
C
C     MARCH BACKWARDS UNTIL WE GET TO A COMMAND DELIMITER
C     OR TO SOMETHING THAT SAYS THIS ISN'T A NUMBER; I.E.,
C     NOT A DIGIT, PLUS, MINUS, OR PERIOD.
C
C
         do 151 li=loc-1,ist,-1
            kchar=msgc(li:li)
C
C
C           IF IT'S A COMMAND DELIMITER:
C
C
            if (kchar.eq.' ') goto 152
C
C
C           IF IT ISN'T A DIGIT OR A PERIOD OR A PLUS OR MINUS
C
C
            if((kchar.gt.'9').or.(kchar.lt.'+').or.
     *         (kchar.eq.',').or.(kchar.eq.'/')) goto 153
  151    continue
C
C
C     WE GOT TO A COMMAND DELIMITER WITHOUT FINDING
C     ANYTHING BUT DIGITS, +, -, OR ..  SO WE WILL
C     REPLACE THE 'e' WITN AN 'E'.
C
C
C     IF 'e' WAS FIRST CHAR, SKIP
C
  152    if (li.eq.loc-1) goto 153
C
C
         ib=loc-1
         ie=ib+1
         msgc(ie:ie)='E'
C
C
C     UPDATE STRING START POINTER; IF NOT AT END, GO BACK
C     AND SEARCH FOR MORE.
C
C
  153    ist=loc+1
         loc=index(msgc(ist:iet1),' ')
         if(loc.gt.0) then
            ist=loc + 1 + (ist-1)
            if (ist.lt.iet1) goto 150
         endif
      endif
C
C      SAVE THE UNPARSED MESSAGE JUST IN CASE WE NEED TO GET
C      AT IT FOR RE-PARSING.  THIS MIGHT OCCUR IF WE DON'T
C      LIKE THE FACT THAT THIS ROUTINE REMOVE SEVERAL SPECIAL
C      CHARACTERS AS DELIMITERS.
C
      nchrsav=iet1-ibt1+1
      length=nchrsav-ibt1+1
      msgsav(1:length)=msgc(ibt1:nchrsav)
C
C
C*****call gparse(pcb,imsgout,msgtype,nwds,ibt2,iet2,msgc,ibt1,iet1)
      call parse_string(lenparse,msgc,imsgout,msgtype,xmsgout,cmsgout,
     *              nwds)
C
      ccommand=cmsgout(1)
ccht
ccht
ccht repeat the last message
ccht
ccht
ccht process a 'run' command (this is different that 'go' or
ccht    'continue' since this command interactes with the driving
ccht    code and and the command processor).
ccht
      if(ccommand(1:3).eq.'run') then
         ipause=0
         if(jcommand.ne.0) then
            ifile=kcommand(lcommand)
            length=icharlnf(ifile)
            inquire(file=ifile(1:length),number=junitcmd)
C              *** GET THE LOGICAL UNIT NUMBER ASSOCAITED WITH THIS FILE
            close(junitcmd)
            write(interfil,9010) lcommand,kcommand(lcommand)
            call writloga('default',2,interfil,0,ierr)
            lcommand=lcommand-1
            if(lcommand.eq.0) then
               icommand=0
               jcommand=0
               ipause=isvpause
            endif
         endif
      endif
ccht
ccht
ccht process a 'go' command
ccht
      if(ccommand(1:8).eq.'continue') then
         ipause=0
         goto 101
      endif
ccht
ccht
ccht process a message enclosed in quotes
ccht
      if((ccommand(1:5).eq.'title' .or. ccommand(1:7).eq.'comment').and.
     *   lenparse.gt.7) then
         lenstart=0
         lenend=0
         do i=1,lenparse
            if(msgb(i:i).eq."'") then
               if(lenstart.eq.0) then
                  lenstart=i+1
               else
                  lenend=i-1
               endif
            endif
         enddo
         len1=len(cmsgout(1))
         nwds1=1+(lenend-lenstart)/len1
         nwds2=lenend-lenstart-len1*(nwds1-1)
         ib1=lenstart
         ib2=min(ib1+len1-1,lenend)
         nwds=nwds1
         do i=2,nwds1+1
            msgtype(i)=3
            cmsgout(i)=msgb(ib1:ib2)
            ib1=ib2+1
            ib2=min(ib2+len1,lenend)
         enddo
         cmsgout(nwds1+1)(ib1:len1*(nwds1+1)) =
     *      '                                '
         goto 9998
      endif
ccht
ccht
ccht see if this is the 'command file' command
ccht
      if(ccommand(1:6).eq.'infile'.or.ccommand(1:5).eq.'input') then
         if(nwds.lt.2) then
            write(interfil,9030)
            call writloga('default',1,interfil,0,ierr)
 9030       format(' ','The command file name must be entered:')
            goto 101
         endif
         iname=cmsgout(2)
C********call fexist(iname,ierr)
         ierr=1
         if(ierr.eq.0) then
            write(interfil,9050) iname
            call writloga('default',1,interfil,1,ierr)
 9050       format(' ','The file:  ',a8,
     *             ' does not exist as a local file')
         else
            jname=-1
            call hassign(jname,iname,ierrass)
            lcommand=lcommand+1
            kcommand(lcommand)=iname
            if(lcommand.eq.1) then
               icommand=1
               jcommand=1
               isvpause=ipause
               ipause=0
            endif
            write(interfil,9020) lcommand,
     *         kcommand(lcommand)(1:icharlnf(kcommand(lcommand)))
            call writloga('default',1,interfil,0,ierr)
 9020       format(' ','Assign command file:  ',i5,' - ',a)
         endif
         goto 101
      endif
ccht
      if(ccommand(1:3).eq.'end') goto 9998
ccht
      if(ccommand(1:5).eq.'pause') then
         ipause=1
         if(jcommand.eq.0) then
            nwds=0
            goto 100
         else
            goto 101
         endif
      endif
ccht
      goto 100
 9998 continue
ccht
      if(istack.eq.-1) istack=0
ccht
ccht
ccht zero out the prompt message array
ccht
      if(istack.eq.0.and.ipause.eq.0.and.jcommand.eq.0) then
         imsgprmp='*nomore*'
      else
         imsgprmp='*more*'
      endif
ccht
ccht
      goto 9999
 9999 continue
      return
      end
