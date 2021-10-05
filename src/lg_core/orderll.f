*dk orderll
      subroutine orderll(ifirst_,list_,link_,len_,ifirst,list)
c #####################################################################
c
c     PURPOSE -
c
c        Order linked lists.  Given the grand collection of linked lists
c        in LIST_ (which are indexed by some 'domain' set), we order these
c        lists by the domain set and list them in order in the grand
c        output list LIST.  After this there is no need for a 'link' array.
c        On output, the IFIRST array contains the starting positions for
c        the sublists in LIST.
c
c     INPUT ARGUMENTS -
c
c     IFIRST_  - IFIRST_(I) gives the location of the beginning of the linked
c               list that corresponds to 'domain' item I.
c     LIST_   - The grand collection of linked lists.
c     LINK_   - The array containing information
c               on how elements in LIST_ are linked together.
c     LEN_    - The number of domain items.
c
c     OUTPUT ARGUMENTS -
c
c     IFIRST   - IFIRST(I) gives the position in LIST of the sublist
c               for domain item I.
c     LIST    - The output list of ordered sublists.
c
c     CHANGE HISTORY -
C$Log: orderll.f,v $
CRevision 2.00  2007/11/05 19:46:03  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:56:16 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Fri Apr 11 11:04:24 1997   kuprat
CPVCS    Changed name of integer variable with misleading name.
CPVCS    
CPVCS       Rev 1.0   02/15/95 13:37:46   dcg
CPVCS    Original version
c
c ######################################################################
      implicit none

      integer lenptr
      parameter (lenptr=1000000)

      integer ifirst_(lenptr),link_(lenptr),list_(lenptr),
     &   ifirst(lenptr),list(lenptr),len_,next,i,ind
 
      next=1
      do 10 i=1,len_
         ifirst(i)=next
         if (ifirst_(i).eq.0) goto 10
         ind=ifirst_(i)
         list(next)=list_(ind)
         next=next+1
         do while(link_(ind).ne.0)
            ind=link_(ind)
            list(next)=list_(ind)
            next=next+1
         enddo
 10   continue
      ifirst(len_+1)=next
      return
      end
