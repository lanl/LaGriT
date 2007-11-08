*dk next_power_of_2
      integer function next_power_of_2(ii)
C
C  Calculate the power of 2 >= ii
C  i.e.  2**ceiling(log2(ii))
C
 
      real base_2_log_ii
      if (ii .le. 1) then
         next_power_of_2 = 1
      else
C       So ii > 1
        base_2_log_ii = log(float(ii)) / log(2.0)
        ii_log_2 = int(base_2_log_ii)
C          *** truncate base_2_log_ii
        if (2**ii_log_2 .ge. ii) then
           next_power_of_2 = 2**ii_log_2
        else
           next_power_of_2 = 2**(ii_log_2+1)
        endif
      endif
C
      goto 9999
 9999 continue
      return
      end
