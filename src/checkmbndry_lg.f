      subroutine checkmbndry_lg
C
C#######################################################################
C
C     PURPOSE -
C
C   This subroutine checks the value of mbndry in comparison to
C   the nnodesx30 (6 tets average per node*4 entries per tet in jtet
C   array+some extra)
C   and resets the value of mbndry to the max available if need be
C
C   INPUT ARGUMENT
C    none
C
C   OUTPUT ARGUMENT
C    none
C
C     CHANGE HISTORY -
C
C   $Log: checkmbndry_lg.f,v $
C   Revision 2.00  2007/11/05 19:45:47  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Tue Oct 12 16:45:38 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      character*132 logmess
      character*32 cmo
      integer ier,nnodes,leni,icmotype,mbndry
C
C#######################################################################
C
      call cmo_get_name(cmo,ier)
C
      call cmo_get_info('nnodes',cmo,nnodes,leni,icmotype,ier)
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
C
C  test value of mbndry
C
      if(mbndry-30*nnodes.ge.0) goto 9999
C
C  need to change value
C  set to 1,000,000,000
C
      mbndry=1000000000
C
C  test value of mbndry
C
      if(mbndry-30*nnodes.lt.0) then
C
C  if still not big enough should be running with 8 byte integers
C 
         write(logmess,100) 
 100     format('Number of nodes requires running with 8 byte integers')
         call writloga('default',0,logmess,0,ier)
         mbndry=mbndry*mbndry
      endif
      call cmo_set_info('mbndry',cmo,mbndry,1,1,ier)
      write(logmess,200) mbndry
 200  format('mbndry value set to ',i20)
      call writloga('default',0,logmess,0,ier)
 9999 continue
      return
      end
