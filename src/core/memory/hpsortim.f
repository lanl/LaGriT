*dk,hpsortim
      SUBROUTINE hpsortim(n,m,md,itemp,ia)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     HPSORTIM ("HeaP SORT using an Integer M-fold key")
C     takes an integer N, and an integer array IA, with
C     first dimension length MD and second dimension length N and
C     shuffles the columns IA(1:MD,*) so that they are in 
C     lexicographic order up to the M'th key.  That is, if I<J
C     and IA(1,I) > IA(1,J), we interchange the I'th and the J'th
C     columns.  If IA(1,I) = IA(1,J), we then check if 
C     IA(2,I) > IA(2,J), in which case we again interchange the
C     columns.  Continuing on in this fashion, we consult the
C     elements IA(K,I) and IA(K,J) for K up to M if necessary
C     to break ties.  (M<=MD.) 
C
C     INPUT ARGUMENTS -
C
C        N - no. of columns to sort into ascending order.
C        M - maximum number of keys (rows) to consult for comparisons
C        MD- column length of array IA (M<=MD)
C        IA - integer array of MD-tuples to be reordered
C             into lexicographic ascending order up to the M'th key.
C        ITEMP - temp array of length MD.
C
C     OUTPUT ARGUMENTS -
C
C        IA - SORTED REAL*8 ARRAY.
C
C
C     CHANGE HISTORY -
C
C        $Log: hpsortim.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   22 Aug 2000 10:21:50   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.1   Tue Aug 31 15:06:56 1999   kuprat
CPVCS    During reordering, we now shuffle whole columns 
CPVCS    (i.e. if M<MD, we also copy entries beyond the M'th row).
CPVCS    
CPVCS       Rev 1.0   Tue Aug 24 21:57:04 1999   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n,m,md
      INTEGER ia(md,*)
      INTEGER i,ir,j,l,k,k1
      INTEGER itemp(md)
      if (n.lt.2) return
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          do k=1,md
             itemp(k)=ia(k,l)
          enddo
        else
          do k=1,md
             itemp(k)=ia(k,ir)
             ia(k,ir)=ia(k,1)
          enddo
          ir=ir-1
          if(ir.eq.1)then
              do k=1,md
                 ia(k,1)=itemp(k)
              enddo
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
c$$$            if(ra(j).lt.ra(j+1))j=j+1
            do k=1,m-1
              if(ia(k,j).lt.ia(k,j+1)) then
                j=j+1
                goto 30
              elseif(ia(k,j).gt.ia(k,j+1)) then
                goto 30
              endif
            enddo
            if(ia(m,j).lt.ia(m,j+1))j=j+1
          endif
 30       continue
          do k=1,m-1
            if(itemp(k).lt.ia(k,j)) then
              do k1=1,md
                 ia(k1,i)=ia(k1,j)
              enddo
              i=j
              j=j+j
              goto 20
            elseif(itemp(k).gt.ia(k,j)) then
              j=ir+1
              goto 20
            endif
          enddo
          if(itemp(m).lt.ia(m,j)) then
            do k=1,md
               ia(k,i)=ia(k,j)
            enddo
            i=j
            j=j+j
          else
            j=ir+1
          endif
          goto 20
        endif
        do k=1,md
          ia(k,i)=itemp(k)
        enddo
      goto 10
      END
