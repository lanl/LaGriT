*dk insertll
      subroutine insertll(ifirst,iplist,iplink,lenlist,inc,i1,i2,next)
c #####################################################################
c
c     PURPOSE -
c
c        Insert item into linked list.  Given the pair (I1,I2), we
c        insert the 'range' item I2 into the linked list that corresponds
c        to 'domain' item I1.
c
c     INPUT ARGUMENTS -
c
c     IFIRST   - IFIRST(I1) gives the location of the beginning of the linked
c               list that corresponds to 'domain' item 'I1'.
c     IPLIST  - Pointer to LIST : the grand collection of linked lists.
c     IPLINK  - Pointer to LINK : the array containing information.
c               on how elements in LIST are linked together.
c     LENLIST - Current length of LIST (and LINK).
c     INC     - Appropriate increment for length of LIST when length
c               of LIST (and LINK) needs to be incremented.
c     I1      - domain item
c     I2      - range item
c     NEXT    - Next available storage location in LIST.
c
c     OUTPUT ARGUMENTS -
c
c     IFIRST,LIST,LINK,NEXT may be modified by this routine.
c
c     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/insertll.f_a  $
CPVCS    
CPVCS       Rev 1.4   Thu Apr 09 19:22:46 1998   kuprat
CPVCS    We now order inserts.
CPVCS    
CPVCS       Rev 1.3   Sun Jun 01 16:37:16 1997   kuprat
CPVCS    Put guard to prevent insertion of duplicate item into linked list.
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:51:52 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Fri Apr 11 11:03:34 1997   kuprat
CPVCS    Changed name of integer variable with misleading name.
CPVCS    
CPVCS       Rev 1.0   02/15/95 13:37:42   dcg
CPVCS    Original version
c
c ######################################################################
      implicit none

      integer lenptr
      parameter (lenptr=1000000)

      pointer (iplist,list), (iplink,link)
      integer ifirst(lenptr),list(lenptr),link(lenptr),
     &   lenlist,inc,i1,i2,next,ind,icscode,iprevind
      character*32 iblkname, isubname
 
c.... Increment linked list by INC if it's out of space.

      if (next.ge.lenlist) then
         lenlist=lenlist+inc
         call mmgetnam(iplist,iblkname,isubname,icscode)
         call mmincblk(iblkname,isubname,iplist,inc,icscode)
         call mmgetnam(iplink,iblkname,isubname,icscode)
         call mmincblk(iblkname,isubname,iplink,inc,icscode)
      endif

c.... IF ifirst(i1)=0, we must start a linked list corresponding to I1.
c.... If ifirst(i1) is not zero, we begin to follow links in the list.
c.... If we find that I2 is already in the list, we refrain from 
c.... adding it again.  Otherwise, we add the new datum in ascending
c.... order.

      if (ifirst(i1).eq.0) then
         ifirst(i1)=next
         link(next)=0
         list(next)=i2
         next=next+1
         goto 9999
      endif

      iprevind=0
      ind=ifirst(i1)
      
 10   continue
      if (i2.eq.list(ind)) goto 9999
      if (i2.lt.list(ind)) then
         if (iprevind.eq.0) then
            link(next)=ind
            ifirst(i1)=next
            list(next)=i2
            next=next+1
            goto 9999
         else
            link(next)=ind
            link(iprevind)=next
            list(next)=i2
            next=next+1
            goto 9999
         endif
      endif
      if (link(ind).eq.0) then
         link(ind)=next
         link(next)=0
         list(next)=i2
         next=next+1
         goto 9999
      endif
      iprevind=ind
      ind=link(ind)
      goto 10

 9999 continue

      return
      end
