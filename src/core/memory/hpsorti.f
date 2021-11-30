*dk,hpsorti
      SUBROUTINE hpsorti(n,ia)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS IS BASED ON THE NUMERICAL RECIPES ROUTINE FOR SORTING
C     AN ARRAY USING THE 'HEAP SORT'.
C
C
C     INPUT ARGUMENTS -
C
C        n - NO. OF ELEMENTS TO SORT INTO ASCENDING ORDER.
C        ia - INTEGER ARRAY TO BE SORTED.
C
C
C
C     OUTPUT ARGUMENTS -
C
C        ia - SORTED INTEGER ARRAY.
C
C
C     CHANGE HISTORY -
C
C        $Log: hpsorti.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   22 Aug 2000 10:21:48   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Aug 24 21:58:34 1999   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n
      INTEGER ia(n)
      INTEGER i,ir,j,l
      INTEGER iia
      if (n.lt.2) return
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          iia=ia(l)
        else
          iia=ia(ir)
          ia(ir)=ia(1)
          ir=ir-1
          if(ir.eq.1)then
            ia(1)=iia
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            if(ia(j).lt.ia(j+1))j=j+1
          endif
          if(iia.lt.ia(j))then
            ia(i)=ia(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        goto 20
        endif
        ia(i)=iia
      goto 10
      END
