C     File contains parse_string and parse_string2

      subroutine parse_string(imsgout,msgtype,
     *      xmsgout,cmsgout,nwds)
C 
C      Uses syntax defined for LAGRIT command lines
C      break up command, from commands_lg.h, into nwds tokens
C      put integers in imsgout, reals in xmsgout, strings in
C      cmsgout, msgtype is token type
C
C ######################################################################
C
C        $Log: parse_string.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      implicit none

      include 'commands_lg.h'

      integer lenparse,nwds
      integer imsgout(*),msgtype(*)
      real*8 xmsgout(*)
      character*32 cmsgout(*)

      integer i,j,istrt,length,inum,k,icharlnb
      logical isintgr,isreal
      real*8 xnum

      lenparse=icharlnb(command)
C
      nwds=0
      inum=0
      xnum=0.0d0
C  skip leading blanks and tabs
         istrt=1
         j=1
         do while (command(j:j).eq.' ' .or. 
     *      command(j:j).eq.achar(9))
            j=j+1
         enddo
         istrt=j

C  look for delimiter break off a token
 10      i=0
         if(istrt.gt.lenparse) go to 100
         nwds=nwds+1
         cmsgout(nwds)=' '
         j=istrt
         do while (command(j:j).ne.' '.and.command(j:j).ne.','.and.
     *     command(j:j).ne.'='.and. command(j:j).ne.achar(9) .and.
     *     command(j:j).ne.';'.and. command(j:j).ne.'/' .and.
     *     command(j:j).ne.char(0) .and. j.le.lenparse)
           i=i+1
           cmsgout(nwds)(i:i)=command(j:j)
           j=j+1
           if(j.gt.lenparse) go to 20
         enddo
C  figure out what kind of token
 20      length=i
         msgtype(nwds)=3
         imsgout(nwds)=0
         xmsgout(nwds)=0.
c  if length is zero fill in the default token
         if(length.eq.0.or.cmsgout(nwds)(1:length).eq.' ') then
            cmsgout(nwds)='-def-'
            go to 25
         endif
         inum=0
         xnum=0.0d0
         call isinteger_lg(cmsgout(nwds),length,inum,isintgr)
         if(isintgr) then
            imsgout(nwds)=inum
            xmsgout(nwds)=inum
            msgtype(nwds)=1
         else
            call isrl_lg(cmsgout(nwds),length,xnum,isreal)
            if(isreal) then
                xmsgout(nwds)=xnum
                msgtype(nwds)=2
            endif
         endif
C  look for next token
25       istrt=j+1
         j=istrt
         if(j.gt.lenparse) go to 100
         do while (command(j:j).eq.' ' .or. 
     *             command(j:j).eq.achar(9) )
            j=j+1
            if(j.gt.lenparse) go to 100
         enddo
         istrt=j
         go to 10
 100  continue
      return
      end

C    
C     evaluate word, return inum and isintgr 
      subroutine isinteger_lg(word,len,inum,isintgr)

      implicit none

      integer len,inum
      character*(*) word
      logical isintgr

      integer i,j,iflag,m,itop,ibot

      inum=0
      isintgr=.true.

C  integer is +- numbers
      iflag=1
      j=1
      if(word(1:1).eq.'-') then
         iflag=-1
         j=2
      elseif(word(1:1).eq.'+') then
         j=2
      endif
      do i=j,len
          m=ichar(word(i:i))
          itop=ichar('9')
          ibot=ichar('0')
          if(m.ge.ibot.and.m.le.itop) then
            inum=inum*10 + m-ibot
          else
            isintgr=.false.
            go to 9999
          endif
       enddo
       isintgr=.true.
       inum=iflag*inum

 9999  continue
       return
       end

C     evaluate tokens with scientific notation     
C     return xnum and isreal
      subroutine isrl_lg(word,len,xnum,isreal)

      implicit none

      integer len
      character*(*) word
      real*8 xnum
      logical isreal

      integer inum,j,iflag,ii,m,ibot,itop
      real*8 power,ten

      isreal=.false.
      ten=10.0d0
      xnum=0.0d0
      iflag=1
      j=1
      if(word(1:1).eq.'-') then
        iflag=-1
        j=j+1
      elseif (word(1:1).eq.'+') then
        j=j+1
      endif
C  look for base
      inum=0
      ii=j
      itop=ichar('9')
      ibot=ichar('0')
      m=ichar(word(ii:ii))
      if ((m.gt.itop.or.m.lt.ibot).and.word(ii:ii).ne.'.') then
         isreal=.false.
         return
      endif
      do while
     *  (m.ge.ibot.and.m.le.itop)
         inum=inum*10+m-ichar('0')
         ii=ii+1
         m=ichar(word(ii:ii))
      enddo
      if(word(ii:ii).eq.'.') then
C  get fraction part of base
         ii=ii+1
         if(ii.gt.len) then
            isreal=.true.
            xnum=inum*iflag
            go to 9999
         endif
         xnum=inum
         power=.1d0
         m=ichar(word(ii:ii))
         do while
     *      (m.ge.ibot.and.m.le.itop)
               xnum=xnum+(m-ibot)*power
               power=power*.1d0
               ii=ii+1
               m=ichar(word(ii:ii))
         enddo
         xnum=xnum*iflag
         if(ii.gt.len) then
            isreal=.true.
            go to 9999
         endif
      endif
      if(word(ii:ii).eq.'e'.or.word(ii:ii).eq.'E'.or.
     *   word(ii:ii).eq.'+'.or.word(ii:ii).eq.'-'.or.
     *   word(ii:ii).eq.'d'.or.word(ii:ii).eq.'D') then
      if(ii.eq.len) go to 9999
         if(xnum.eq.0.0d0.and.inum.eq.0.and.ii.eq.1) go to 9999
         if(xnum.eq.0d0) xnum=inum*iflag
         iflag=1
         if(word(ii:ii).eq.'-') iflag=-1
C  get exponent
         ii=ii+1
         inum=0
         if(word(ii:ii).eq.'+') ii=ii+1
         if(word(ii:ii).eq.'-') then
            ii=ii+1
            iflag=-1
         endif
         m=ichar(word(ii:ii))
         do while
     *     (m.ge.ibot.and.m.le.itop.and.
     *     ii.le.len)
             inum=inum*10+m-ichar('0')
             ii=ii+1
             m=ichar(word(ii:ii))
         enddo
         if(ii.le.len) go to 9999
         if(iflag.eq.1) then
            xnum=xnum*ten**inum
          else
            xnum=xnum/ten**inum
         endif
         isreal=.true.
      endif
 9999    continue
         return
         end

C     parse_string2
C     this version takes a single line msgc and parses into 
C     nwds number of tokens seperated by white space 
C     with the appropriate type assignments
C     put integers in imsgout, reals in xmsgout, strings in
C     cmsgout, msgtype is token type

      subroutine parse_string2(lenparse,msgc,imsgout,msgtype,
     *      xmsgout,cmsgout,nwds)

      implicit none

C     arguments
      integer lenparse,nwds
      character*4096 msgc
      integer imsgout(*),msgtype(*)
      real*8 xmsgout(*)
      character*32 cmsgout(*)

      integer i,j,istrt,length,inum
      logical isintgr,isreal
      real*8 xnum
C
      nwds=0
      length=0
      inum=0
      xnum=0.0d0

C  skip leading blanks
      istrt=1
      j=1
      do while (msgc(j:j).eq.' ' .or. msgc(j:j).eq.achar(9))
         j=j+1
      enddo

C  look for delimiter break off a token
      istrt=j
 10   i=0
      if(istrt.gt.lenparse) go to 100
      nwds=nwds+1
      cmsgout(nwds)=' '
      j=istrt
      do while ( msgc(j:j).ne.' '.and.msgc(j:j).ne.','.and.
     *    msgc(j:j).ne.';'.and.msgc(j:j).ne.'%'.and.
     *    msgc(j:j).ne.achar(9) .and.
     *    msgc(j:j).ne.char(0) .and. j.le.lenparse)
        i=i+1
        if (i.eq.32) then
           nwds=nwds+1
           cmsgout(nwds)=' '
           i=1
        endif
        cmsgout(nwds)(i:i)=msgc(j:j)
        j=j+1
        if(j.gt.lenparse) go to 20
      enddo
C  figure out what kind of token
 20   length=i
      msgtype(nwds)=3
      imsgout(nwds)=0
      xmsgout(nwds)=0.
      inum=0
      xnum=0.0d0
      call isinteger_lg(cmsgout(nwds),length,inum,isintgr)
      if(isintgr) then
         imsgout(nwds)=inum
         xmsgout(nwds)=inum
         msgtype(nwds)=1
      else
         call isrl_lg(cmsgout(nwds),length,xnum,isreal)
         if(isreal) then
             xmsgout(nwds)=xnum
             msgtype(nwds)=2
         endif
      endif
C  look for next token
      istrt=j+1
      j=istrt
      if(j.gt.lenparse) go to 100
      do while (msgc(j:j).eq.' ' .or. msgc(j:j).eq.achar(9))
         j=j+1
         if(j.gt.lenparse) go to 100
      enddo
      istrt=j

      go to 10

 100  continue
      return
      end

C     End file
