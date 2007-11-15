      subroutine metis_lagrit_message()
C#######################################################################
C
C     PURPOSE - This just a place holder to prevent
C     unsatisfied externals during linking if the
C     METIS library is not present.
C
C  METIS:---------WARNING-------------------------------                           
C  METIS: The standard libraries, liblagrit.a, libutil.a                           
C  METIS: do not contain METIS functions. To use                                   
C  METIS: the METIS functions one must download the METIS                          
C  METIS: package, build METIS libraries on your local                             
C  METIS: system and link them with the LAGriT libraries.                          
C  METIS: See LaGriT documentation of METIS command.                          
C  METIS:                                                                          
C  METIS: See      www-users.cs.umn.edu/~karypis/metis                             
C  METIS:---------WARNING-------------------------------                           
C  
C   $Log: metis_lg.f,v $
C   Revision 2.00  2007/11/05 19:46:01  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   21 Feb 2003 11:23:38   gable
CPVCS    Made change in message sent to screen and log file.
CPVCS    
CPVCS       Rev 1.0   01 Nov 2002 13:05:50   gable
CPVCS    Initial revision.
C
C#######################################################################
      implicit none
      character*128 logmess
      integer ierr
C
C     Message when METIS routines are called without
C     linking in the actual METIS libraries.
C
      logmess = 'METIS:---------WARNING-------------------------------'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: The standard libraries, liblagrit.a, libutil.a'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: do not contain METIS functions. To use'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: the METIS functions one must download the METIS'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: package, build METIS libraries on your local'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: system and link them with the LAGriT libraries.'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: See LaGriT documentation of METIS command.'
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS:  '
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS: See      www-users.cs.umn.edu/~karypis/metis '
      call writloga('default',0,logmess,0,ierr)
      logmess = 'METIS:---------WARNING-------------------------------'
      call writloga('default',0,logmess,0,ierr)

      return
      end

      subroutine  metis_partgraphrecursive( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_wpartgraphrecursive( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_partgraphkway( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_wpartgraphkway( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_edgend( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_nodend( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_nodewnd( )
        call metis_lagrit_message()
         return
         end
      subroutine  metis_partmeshnodal( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_partmeshdual( )
        call metis_lagrit_message()
         return
         end
      subroutine  metis_meshtonodal( )
        call metis_lagrit_message()
         return
         end
      subroutine  metis_meshtodual( )
        call metis_lagrit_message()
         return
         end
      subroutine  metis_estimatememory( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_mcpartgraphrecursive( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_mcpartgraphkway( )
         call metis_lagrit_message()
         return
         end
      subroutine  metis_partgraphvkway( )
        call metis_lagrit_message()
         return
         end
      subroutine  metis_wpartgraphvkway( )
        call metis_lagrit_message()
         return
         end
