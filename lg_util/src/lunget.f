*dk,lunget
      function lunget(ifile)
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS FUNCTION FINDS THE LOGICAL UNIT NUMBER ASSOCIATED
C           WITH THE FILE.
C
C     INPUT ARGUMENTS -
C
C        ifile    - THE FILE NAME TO INQUIRE ABOUT.
C
C     OUTPUT ARGUMENTS -
C
C        lunget - THE LOGICAL UNIT NUMBER CONNECTED TO "ifile".
C
C     CHANGE HISTORY -
C
C        $Log: lunget.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:42:58   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      character ifile*(*)
      logical opend

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 kunit,lunget,jfile

C
C#######################################################################
C
      if(len(ifile).eq.0) then
         lenmax=8
C        *** IF THIS HAPPENS, THIS IS "PROABALY" AN INTEGER FIELD WITH A
C               WITH A CHARACTER CONTAINED WITHIN, MAKE AN ASSUMPTION.
C********jfile=int(ifile)
         read(ifile,'(i8)') jfile
         inquire(unit=jfile,
     *           number=kunit,opened=opend,err=9999)
         if(opend.eqv..true.) then
            lunget=kunit
         else
            lunget=-1
            inquire(file=ifile(1:lenmax),
     *              opened=opend,number=kunit,err=9999)
            if(opend.eqv..true.) then
               lunget=kunit
            endif
         endif
      else
         lenmax=icharlnf(ifile)
         lunget=-1
         inquire(file=ifile(1:lenmax),opened=opend,err=9999)
         if(opend.eqv..true.) then
            inquire(file=ifile(1:lenmax),number=kunit)
            lunget=kunit
         endif
      endif
      if(ifile(1:3).eq.'tty') lunget=6
      goto 9999
 9999 continue
      return
      end
