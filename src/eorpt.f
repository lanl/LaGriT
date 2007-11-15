      subroutine eorpt(string,nsw,polish,nswl,ierr)
      implicit real*8(a-h,o-z)
C
      character*132 logmess
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE TRANSLATES FROM INFIX NOTATION TO EARLY OPERAND
C     NON-REVERSED ORDER POLISH NOTATION.
C     RECEIVED FROM TOM GORMAN WHO FOUND IT IN KATZAN, HARRY, ADVANCED
C     PROGRAMMING, VAN NOSTRAND-REINHOLD, 1970 P. 54-60.
C
C
C     INPUT ARGUMENTS -
C
C        string - THE PARSED INFIX STRING IN SUPER STRINGS.
C        nsw - THE NUMBER OF SUPERWORDS IN THE STRING.
C
C
C     OUTPUT ARGUMENTS -
C
C        polish - THE POLISH NOTATION STRING.
C        nswl - THE NUMBER OF WORDS IN THE POLISH STACK.
C        ierr - ERROR FLAG.
C
C
C     CHANGE HISTORY -
C
C        $Log: eorpt.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:44:46 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   12/05/95 08:21:04   het
CPVCS    Make changes for UNICOS
CPVCS    
CPVCS       Rev 1.1   05/01/95 08:34:46   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:11:40   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      parameter (maxopr=128,nlvl=11)
      character*(*) string(nsw),polish(nsw)
      character*32 opstack(maxopr)
      character*32 synm(nlvl),opr(nlvl)
      character*32 iword1, iword2
      integer levl(nlvl),ntmp,tmlv(maxopr)
C
C#######################################################################
C
C     ************************************************************
C     SET OPERATOR, LEVEL AND SYNM DATA
C
      opr(1)='('
      opr(2)=')'
      opr(3)='or'
      opr(4)='and'
      opr(5)='not'
      opr(6)='eq'
      opr(7)='ne'
      opr(8)='lt'
      opr(9)='le'
      opr(10)='gt'
      opr(11)='ge'
      levl(1)=0
      levl(2)=1
      levl(3)=2
      levl(4)=3
      levl(5)=4
      levl(6)=5
      levl(7)=5
      levl(8)=5
      levl(9)=5
      levl(10)=5
      levl(11)=5
      do 5 i=1,nlvl
         synm(i)=opr(i)
    5 continue
C
      ierr=0
      ntmp=0
C
      ip=0
      last=-1
      i=0
C
C     ************************************************************
C     LOOP THROUGH THE INPUT STACK
C
   10 i=i+1
C
C     -------------------------------------------------------------
C     SKIP INBEDDED BLANKS
C
      if(string(i).eq.' ') go to 70
C
C     -------------------------------------------------------------
C     IS THIS AN OPERATOR?
C
      iword1=string(i)
      do 20 j=1,nlvl
         iword2=opr(j)
         len=icharlnf(iword2)
         if(iword1(1:len) .eq. iword2(1:len)) go to 30
   20 continue
C
C     -------------------------------------------------------------
C     NOT AN OPERATOR JUST PUT ON STACK
C
      ip=ip+1
      polish(ip)=string(i)
      go to 70
C
C     -------------------------------------------------------------
C     OPERATOR, CHECK THE HIERARCHY
C
   30 if(levl(j).le.last) if(levl(j)-1) 40,50,50
      if(j.eq.2) if(last) 40,60,40
C
C     -------------------------------------------------------------
C     PUT OPERATOR ONTO HOLD STACK
C
   40 ntmp=ntmp+1
      if(ntmp.gt.maxopr) go to 100
      last=levl(j)
C
C     -------------------------------------------------------------
C     SAVE OPERATOR IN STACK
C
      tmlv(ntmp)=levl(j)
      opstack(ntmp)=synm(j)
      go to 70
C
   50 if(tmlv(ntmp).ge.2) then
         ip=ip+1
         polish(ip)=opstack(ntmp)
      endif
C
      ntmp=ntmp-1
      last=-1
      if(ntmp.gt.0) last=tmlv(ntmp)
      go to 30
C
C     -------------------------------------------------------------
C     DROP MATCHING PARENTHESIS FROM STACK
C
   60 ntmp=ntmp-1
      last=-1
      if(ntmp.gt.0) last=tmlv(ntmp)
C
   70 if(i.lt.nsw) go to 10
C
C     -------------------------------------------------------------
C     UNLOAD THE OPERATOR STACK
C
      if(ntmp.lt.1) go to 110
      l=ntmp+1
      do 80 i=1,ntmp
         if(tmlv(l-i).lt.2) go to 90
         ip=ip+1
         polish(ip)=opstack(l-i)
   80 continue
      go to 110
C
C     -------------------------------------------------------------
C     SET MISMATCHED PARENTHESIS FLAG
C
   90 ierr=-1
      go to 9999
C
C     -------------------------------------------------------------
C     ARRAY OVERFLOW FLAG
C
  100 ierr=-2
      go to 9999
C
  110 nswl=ip
C
C
 9999 return
      end
