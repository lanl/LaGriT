      subroutine set_bit_type(ibits,id)

C
C #####################################################################
C
C     PURPOSE -
C
C        Set a bit pattern based on the integer input value.
C
C     INPUT ARGUMENTS - 
C
C        id
C
C     OUTPUT ARGUMENTS -
C
C        ibits
C
C     CHANGE HISTORY -
C
C        $Log: set_bit_type.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C   
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:01:04 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:41:50 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      integer ibits,id
C     INPUT ARGUMENTS -
C
C        id   - 
C
C       26 possible exterior node types based upon a single normal to a face
c
c   1   bottom             1, 4, 3, 2
c   2   top                5, 6, 7, 8
c   3   front (south)      1, 2, 6, 5
c   4   right (east)       2, 3, 7, 6
c   5   back  (north)      3, 4, 8, 7
c   6   left  (west)       1, 5, 8, 4
c   7   bottom front       1,2
c   8   bottom left        1,4
c   9   front  left        1,5
c  10   bottom right       2,3
c  11   front right        2,6
c  12   bottom back        3,4
c  13   right back         3,7
c  14   back left          4,8
c  15   top front          5,6
c  16   top left           5,8
c  17   top right          6,7
c  18   top back           7,8
c  19   bottom front left  1
c  20   bottom front right 2
c  21   bottom back  right 3
c  22   bottom back  left  4
c  23   top    front left  5
c  24   top    front right 6
c  25   top    back  right 7
c  26   top    back  left  8
c
c     The output will be assign a bit pattern depending upon the input ID value
c
c      data ipat( 1)  /  0 / ! 000000   ---    ---    ---    ---    ---    ---   
c      data ipat( 2)  /  1 / ! 000001  bottom  ---    ---    ---    ---    ---   
c      data ipat( 3)  /  2 / ! 000010   ---   top     ---    ---    ---    ---   
c      data ipat( 4)  /  3 / ! 000011  bottom top     ---    ---    ---    ---   
c      data ipat( 5)  /  4 / ! 000100   ---    ---   front   ---    ---    ---   
c      data ipat( 6)  /  5 / ! 000101  bottom  ---   front   ---    ---    ---   
c      data ipat( 7)  /  6 / ! 000110   ---   top    front   ---    ---    ---   
c      data ipat( 8)  /  7 / ! 000111  bottom top    front   ---    ---    ---   
c      data ipat( 9)  /  8 / ! 001000   ---    ---    ---   right   ---    ---   
c      data ipat(10)  /  9 / ! 001001  bottom  ---    ---   right   ---    ---   
c      data ipat(11)  / 10 / ! 001010   ---   top     ---   right   ---    ---   
c      data ipat(12)  / 11 / ! 001011  bottom top     ---   right   ---    ---   
c      data ipat(13)  / 12 / ! 001100   ---    ---   front  right   ---    ---   
c      data ipat(14)  / 13 / ! 001101  bottom  ---   front  right   ---    ---   
c      data ipat(15)  / 14 / ! 001110   ---   top    front  right   ---    ---   
c      data ipat(16)  / 15 / ! 001111  bottom top    front  right   ---    ---   
c      data ipat(17)  / 16 / ! 010000   ---    ---    ---    ---   back    ---   
c      data ipat(18)  / 17 / ! 010001  bottom  ---    ---    ---   back    ---   
c      data ipat(19)  / 18 / ! 010010   ---   top     ---    ---   back    ---   
c      data ipat(20)  / 19 / ! 010011  bottom top     ---    ---   back    ---   
c      data ipat(21)  / 20 / ! 010100   ---    ---   front   ---   back    ---   
c      data ipat(22)  / 21 / ! 010101  bottom  ---   front   ---   back    ---   
c      data ipat(23)  / 22 / ! 010110   ---   top    front   ---   back    ---   
c      data ipat(24)  / 23 / ! 010111  bottom top    front   ---   back    ---   
c      data ipat(25)  / 24 / ! 011000   ---    ---    ---   right  back    ---   
c      data ipat(26)  / 25 / ! 011001  bottom  ---    ---   right  back    ---   
c      data ipat(27)  / 26 / ! 011010   ---   top     ---   right  back    ---   
c      data ipat(28)  / 27 / ! 011011  bottom top     ---   right  back    ---   
c      data ipat(29)  / 28 / ! 011100   ---    ---   front  right  back    ---   
c      data ipat(30)  / 29 / ! 011101  bottom  ---   front  right  back    ---   
c      data ipat(31)  / 30 / ! 011110   ---   top    front  right  back    ---   
c      data ipat(32)  / 31 / ! 011111  bottom top    front  right  back    ---   
c      data ipat(33)  / 32 / ! 100000   ---    ---    ---    ---    ---   left   
c      data ipat(34)  / 33 / ! 100001  bottom  ---    ---    ---    ---   left   
c      data ipat(35)  / 34 / ! 100010   ---   top     ---    ---    ---   left   
c      data ipat(36)  / 35 / ! 100011  bottom top     ---    ---    ---   left   
c      data ipat(37)  / 36 / ! 100100   ---    ---   front   ---    ---   left   
c      data ipat(38)  / 37 / ! 100101  bottom  ---   front   ---    ---   left   
c      data ipat(39)  / 38 / ! 100110   ---   top    front   ---    ---   left   
c      data ipat(40)  / 39 / ! 100111  bottom top    front   ---    ---   left   
c      data ipat(41)  / 40 / ! 101000   ---    ---    ---   right   ---   left   
c      data ipat(42)  / 41 / ! 101001  bottom  ---    ---   right   ---   left   
c      data ipat(43)  / 42 / ! 101010   ---   top     ---   right   ---   left   
c      data ipat(44)  / 43 / ! 101011  bottom top     ---   right   ---   left   
c      data ipat(45)  / 44 / ! 101100   ---    ---   front  right   ---   left   
c      data ipat(46)  / 45 / ! 101101  bottom  ---   front  right   ---   left   
c      data ipat(47)  / 46 / ! 101110   ---   top    front  right   ---   left   
c      data ipat(48)  / 47 / ! 101111  bottom top    front  right   ---   left   
c      data ipat(49)  / 48 / ! 110000   ---    ---    ---    ---   back   left   
c      data ipat(50)  / 49 / ! 110001  bottom  ---    ---    ---   back   left   
c      data ipat(51)  / 50 / ! 110010   ---   top     ---    ---   back   left   
c      data ipat(52)  / 51 / ! 110011  bottom top     ---    ---   back   left   
c      data ipat(53)  / 52 / ! 110100   ---    ---   front   ---   back   left   
c      data ipat(54)  / 53 / ! 110101  bottom  ---   front   ---   back   left   
c      data ipat(55)  / 54 / ! 110110   ---   top    front   ---   back   left   
c      data ipat(56)  / 55 / ! 110111  bottom top    front   ---   back   left   
c      data ipat(57)  / 56 / ! 111000   ---    ---    ---   right  back   left   
c      data ipat(58)  / 57 / ! 111001  bottom  ---    ---   right  back   left   
c      data ipat(59)  / 58 / ! 111010   ---   top     ---   right  back   left   
c      data ipat(60)  / 59 / ! 111011  bottom top     ---   right  back   left   
c      data ipat(61)  / 60 / ! 111100   ---    ---   front  right  back   left   
c      data ipat(62)  / 61 / ! 111101  bottom  ---   front  right  back   left   
c      data ipat(63)  / 62 / ! 111110   ---   top    front  right  back   left   
c      data ipat(64)  / 63 / ! 111111  bottom top    front  right  back   left   
c
      if(id .eq. 1)then
         call setbit(32,0,ibits,1)
      elseif(id .eq. 2)then
         call setbit(32,1,ibits,1)
      elseif(id .eq. 3)then
         call setbit(32,2,ibits,1)
      elseif(id .eq. 4)then
         call setbit(32,3,ibits,1)
      elseif(id .eq. 5)then
         call setbit(32,4,ibits,1)
      elseif(id .eq. 6)then
         call setbit(32,5,ibits,1)
      elseif(id .eq. 7)then
         call setbit(32,0,ibits,1)      
         call setbit(32,2,ibits,1)      
      elseif(id .eq. 8)then
         call setbit(32,0,ibits,1)      
         call setbit(32,5,ibits,1)      
      elseif(id .eq. 9)then
         call setbit(32,2,ibits,1)      
         call setbit(32,5,ibits,1)      
      elseif(id .eq. 10)then
         call setbit(32,0,ibits,1)      
         call setbit(32,3,ibits,1)      
      elseif(id .eq. 11)then
         call setbit(32,2,ibits,1)      
         call setbit(32,3,ibits,1)      
      elseif(id .eq. 12)then
         call setbit(32,0,ibits,1)      
         call setbit(32,4,ibits,1)      
      elseif(id .eq. 13)then
         call setbit(32,3,ibits,1)      
         call setbit(32,4,ibits,1)      
      elseif(id .eq. 14)then
         call setbit(32,4,ibits,1)      
         call setbit(32,5,ibits,1)      
      elseif(id .eq. 15)then
         call setbit(32,1,ibits,1)      
         call setbit(32,2,ibits,1)      
      elseif(id .eq. 16)then
         call setbit(32,1,ibits,1)      
         call setbit(32,5,ibits,1)      
      elseif(id .eq. 17)then
         call setbit(32,1,ibits,1)      
         call setbit(32,3,ibits,1)      
      elseif(id .eq. 18)then
         call setbit(32,1,ibits,1)      
         call setbit(32,4,ibits,1)      
      elseif(id .eq. 19)then
         call setbit(32,0,ibits,1)      
         call setbit(32,2,ibits,1)      
         call setbit(32,5,ibits,1)      
      elseif(id .eq. 20)then
         call setbit(32,0,ibits,1)      
         call setbit(32,2,ibits,1)      
         call setbit(32,3,ibits,1)            
      elseif(id .eq. 21)then
         call setbit(32,0,ibits,1)      
         call setbit(32,4,ibits,1)      
         call setbit(32,3,ibits,1)            
      elseif(id .eq. 22)then
         call setbit(32,0,ibits,1)      
         call setbit(32,4,ibits,1)      
         call setbit(32,5,ibits,1)            
      elseif(id .eq. 23)then
         call setbit(32,1,ibits,1)      
         call setbit(32,2,ibits,1)      
         call setbit(32,5,ibits,1)            
      elseif(id .eq. 24)then
         call setbit(32,1,ibits,1)      
         call setbit(32,2,ibits,1)      
         call setbit(32,3,ibits,1)            
      elseif(id .eq. 25)then
         call setbit(32,1,ibits,1)      
         call setbit(32,4,ibits,1)      
         call setbit(32,3,ibits,1)            
      elseif(id .eq. 26)then
         call setbit(32,1,ibits,1)      
         call setbit(32,4,ibits,1)      
         call setbit(32,5,ibits,1)            
      endif
c
      return
      end
