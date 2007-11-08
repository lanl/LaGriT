C##########################################################################
C
C  PURPOSE
C
C    Declarations for global variables
C    Also for pset and eltset name blocks
C
C
C###########################################################################
C
C  CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/global_lg.h_a  $
CPVCS    
CPVCS       Rev 1.4   Thu Mar 02 10:48:00 2000   dcg
CPVCS    correct length of global_character array
CPVCS
CPVCS       Rev 1.3   Mon Feb 07 13:07:00 2000   dcg
CPVCS
CPVCS       Rev 1.2   Mon Jan 31 17:22:06 2000   dcg
C
C###########################################################################
      pointer(ipglobal_name,global_name)
      character*32 global_name(*)
      pointer(ipglobal_type,global_type)
      integer global_type (*)
c 1=integer, 2=real, 3=character
      pointer(ipglobal_index,global_index)
      integer global_index(*)
      pointer(ipglobal_integer,global_integer)
      integer global_integer(2,*)
      pointer(ipglobal_real,global_real)
      real*8 global_real(2,*)
      pointer(ipglobal_character,global_character)
      character*32 global_character(2,*)
      common/global_variables_lg/ number_of_globals
      integer number_of_globals
 
 
 
