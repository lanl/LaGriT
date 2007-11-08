 
 
 
	subroutine inter3 ( list1, nmax1, list2, nmax2, list3, nmax3,
     &                      ninter )
C                                                                       
C ##################################################################### 
C                                                                       
C     PURPOSE -                                                         
C                                                                       
C        None                                                           
C                                                                       
C     INPUT ARGUMENTS -                                                 
C                                                                       
C        None                                                           
C                                                                       
C     OUTPUT ARGUMENTS -                                                
C                                                                       
C        None                                                           
C                                                                       
C     CHANGE HISTORY -                                                  
C                                                                       
C        $Log:   /pvcs.config/t3d/src/inter3.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:51:58 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   01/04/95 22:03:12   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:15:30   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
      implicit real*8 (a-h, o-z)
c------------------------------------------------------------------------------
c	This subroutine finds the first two common entrees to the lists of
c	integers list1, list2 and list3; they are returned in the vector ninter.
c	If there is only ONE common entree, 0 is returned as the second one
c	(this correspond to an element having a face on a boundary). If there
c	is no common entree, an error message is produced. There cannot be
c	more than 2 common entrees (if the lists are correct though!).
c
c	Frederic Chalot - Stanford University - Fall 1990/1991
c------------------------------------------------------------------------------
c
	dimension list1(nmax1), list2(nmax2), list3(nmax3)
CMF$    layout list1(:serial)
CMF$    layout list2(:serial)
CMF$    layout list3(:serial)
	dimension ninter(2)
c
	ni = 0
	ninter(2) = 0
	do 300 n1 = 1, nmax1
	  l1 = list1(n1)
	  do 200 n2 = 1, nmax2
	    l2 = list2(n2)
	    do 100 n3 = 1, nmax3
	      l3 = list3(n3)
	      if ( (l1 .eq. l2) .and. (l1 .eq. l3) ) then
	        ni = ni+1
	        ninter(ni) = l1
	        if (ni.eq.2) return
	      end if
100	    continue
200	  continue
300	continue
c
	if (ni.eq.0) then
	  write(*,*) 0
	  write(*,'(a)')
     &	             '   ***** ERROR: empty intersection *****'
	  stop
	end if
c
c.... return
c
	return
	end
