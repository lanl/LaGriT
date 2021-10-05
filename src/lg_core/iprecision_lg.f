      function iprecision()
C
C ######################################################################
C
C        $Log: iprecision_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
      implicit real*8 (a-h,o-z)
      real a
      real*8 b
      a=1.0/3.0
      b=1.0d+00/3.0d+00
      c=1.0d+00/3.0d+00
      x1=abs(a-c)
      x2=abs(b-c)
      if(x1.eq.0.0d+00) iprecision_test=1
      if(x2.eq.0.0d+00) iprecision_test=2
      iprecision=iprecision_test
      goto 9999
9999  continue
      return
      end
