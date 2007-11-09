      subroutine sortbins
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C#######################################################################
C    FORMAT - sort/cmoname/bins      /[ASCENDING|descending]/[ikey]/in_att/[epsilon_user]
C    FORMAT - sort/cmoname/index|rank/[ASCENDING|descending]/[ikey]/in_att1 in_att2 in_att3 ...
C
C    INPUT ARGUMENTS - imsgin,xmsgin,cmsgin,msgtyp,nwds
C    INPUTS
C            cmoname = name of MO to operate on ( / / and /-def/ supported)
C
C            index - multi-key sort such that in_att1(ikey(i)) i=1,...nnodes
C                    lists the attribute in_att1 in ascending or descending order.
C
C                    The ikey attribute will contain the permutation vector
C                    that could then be used in reordering the MO.
C                    (i.e. reorder/cmo/ikey)
C
C            rank  - multi-key sort such that ikey(i) lists the ranking of the node
C                    in the sorted list.
C            bins  - single-key sort which assigns each in_att1 value a bin.
C                    ikey(i) list the bin number of in_att1(i)
C                    If all array values are unique, then the maximum
C                    value of the index array will equal the number of
C                    entries in the sorted list. Otherwise, the
C                    maximum value of the index array will be less than
C                    the number of entries in the sorted list but will
C                    equal the number of unique entries in the list.
C
C                    When the bins option is used there is an optional
C                    argument, epsilon_user, that sets the epsilon value
C                    used to compare bins. Default is 1.e-10.
C
C            ascending - default, sort in ascending order
C
C            descending - sort in descending order
C
C            ikey - integer vector (VINT) which will hold the output
C                   sorted key values. If it exists it will be used,
C                   if it does not exist it will be created.
C                   If no value is given default key attribute name
C                   will be concatination of 'ikey_' and the first
C                   input attribute name. (i.e. /-def-/imt will produce
C                   a sort key named ikey_imt)
C
C            in_att - input attribute node based array upon which the
C                     sorting routine will sort
C
C                     Multi-key sorts can have an arbitrary number of input
C                     attributes. Attribute in_att1(n) has priority over
C                     in_att2(n) in breaking ties.
C
C                     Note: all attributes are put into a real*8 work array
C                     before being sent to the sort routine.
C
C    Old Syntax still supported
C
C    USAGE - sort/xyz/[INDEX|bins|rank]
C
C            sort/xyz/index - sorts the x,y,z coordinates integer arrays
C            i_index, J_index, k_index such that xic(i_index(i)) i=1,..nnodes
C            lists the coordinate in ascending order.
C            sort/xyz/bins - sorts the x,y,z coordinates and assigns each
C            i_index, j_index, k_index values in ascending order of
C            the bin number of the sorted list.
C            sort/xyz/rank - sorts the x,y,z coordinates and assigns each
C            i_index, j_index, k_index values the ranking of the
C            node in the sorted list.
C
C            If all array values are unique, then the maximum
C            value of the index array will equal the number of
C            entries in the sorted list. Otherwise, the
C            maximum value of the index array will be less than
C            the number of entries in the sorted list but will
C            equal the number of unique entries in the list.
C
C            For example given                 x = 0, 1, 2, 1, 0
c            sort/xyz/index would return i_index = 5, 1, 4, 2, 3
C            sort/xyz/bins  would return i_index = 1, 2, 3, 2, 1
C            sort/xyz/rank  would return i_index = 2, 4, 5, 3, 1
C
C
C
C     OUTPUT ARGUMENTS - creates 3 new node lenght vector integer attributes
C                        i_index, j_index, k_index.
C
C    END OF Old Syntax still supported
C
C     CHANGE HISTORY -
C        $Log: sortbins.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   29 Aug 2007 10:52:50   gable
CPVCS    Corrected spelling of keyword descending. Left in option of using misspelled
CPVCS    keyword decending.
CPVCS    
CPVCS       Rev 1.8   25 Jul 2007 10:01:54   gable
CPVCS    Added option for user control of epsilon value in bins option.
CPVCS    
CPVCS       Rev 1.7   08 Feb 2006 14:35:32   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.6   12 May 2003 10:46:18   dcg
CPVCS    allow attributes of arbitrary length not just nnodes or nelements
CPVCS
CPVCS       Rev 1.5   01 Nov 2002 13:06:52   gable
CPVCS    Added ability to sort on element attributes.
CPVCS
CPVCS       Rev 1.4   24 Apr 2001 10:16:14   jan
CPVCS    changed order of variable definition for nwds
CPVCS
CPVCS
CPVCS       Rev 1.3   06 Apr 2001 13:58:24   gable
CPVCS    Total change to syntax. Now supports multi-key sorting.
CPVCS
CPVCS       Rev 1.2   27 Apr 2000 12:07:14   dcg
CPVCS    remove call to cmo_get_info for iwork - iwork is not a mesh object attribute
CPVCS
CPVCS       Rev 1.1   Fri Jan 22 16:50:20 1999   dcg
CPVCS    move declaration of array length type before array declaration
CPVCS
CPVCS       Rev 1.0   Thu Sep 24 11:30:00 1998   gable
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit none
C
C     Variable/Data Dictionary
C
C     isort_type - string, which sort method to use.
C     catt_name - string, the name of array to sort.
C     itype_att - value, Type of array to sort, int-1 real-2
C     att_r - array, stores real values of array to sort.
C     att_i - array, stores int values of array to sort.
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      pointer (ipatt, att_r)
      pointer (ipatt, att_i)
      pointer (ipikey, ikey)
      pointer (ipiwork,iwork)
      pointer (iprwork,rwork)
      real*8 rwork(10000000)
      real*8 att_r(10000000)
      integer att_i(10000000)
      integer ikey(10000000)
      integer iwork(10000000)
      integer itype, ilen, ier, itype_att, irank
      integer nwds, nnodes, nkey, mkey, nsize, ioff_set, narg_input
      integer nelem, nsort
      integer isort_order
      integer i, index, nend
      real*8 rsort_order
      real*8 epsilon_user
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      integer icharlnf
 
      character*32 cmsgin(nwds)
      character*32 isubname, cmonam
      character*32 sort_key
      character*32 isort_type
      character*32 sort_order
      character*32 catt_name
      character*80 logmess
      character*90 cmdmessage
      character*32 ctype,crank,clen,cinter,cpers,cio
C
C#######################################################################
C
      isubname='sortbins'
c
c    Decide what to do based on command line strings
c
c    2 - Get the mesh object
c
      cmonam = cmsgin(2)
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmonam, ier)
         if(ier.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format(" SORT: CMO found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           goto 9999
         endif
      endif
C
C     Check that cmonam is a valid mesh object
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      if(ier .ne. 0)then
           write(logmess,9005) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9005   format(" SORT: ERROR looking for nnodes of mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           goto 9999
      endif
      call cmo_get_info('nelements',cmonam,nelem,ilen,itype,ier)
      if(ier .ne. 0)then
           write(logmess,9006) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9006   format(" SORT: ERROR looking for nelements of mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           goto 9999
      endif
C
C#######################################################################
C
C    3 - Get the sort method to be used (bins,index, or rank)
C
      if((cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'index') .or.
     1   (cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. '-def-') .or.
     2   (nwds .eq. 1).or. (nwds .eq. 2))then
            isort_type = 'index'
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'bins') then
            isort_type = 'bins'
C
C     Check for user specified epsilon
C
        epsilon_user = 1.e-10
        if(nwds .gt. 6)then
           if(msgtyp(7) .eq. 2)then
              epsilon_user = xmsgin(7)
           else
              write(logmess,9008) msgtyp(7)
 9008         format(" SORT: Arg(7) epsilon_user not float: ",i2)
              call writloga('default',0,logmess,0,ier)
              write(logmess,9009)
 9009         format(" SORT: Arg(7) Using default = 1.e-10 ")
              call writloga('default',0,logmess,0,ier)
           endif
         endif
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'rank') then
            isort_type = 'rank'
      else
        write(logmess,9010) cmsgin(3)(1:icharlnf(cmsgin(3)))
 9010   format(" SORT: Invalid option: ",a)
        call writloga('default',0,logmess,0,ier)
        write(logmess,9015)
 9015   format
     1  ("Usage: sort/cmo/[INDEX,bins,rank]/...")
        call writloga('default',0,logmess,0,ier)
        goto 9999
      endif
C
C
C#######################################################################
C
C    4 - Determine if sort is in ascending or descending order
C
      sort_order = cmsgin(4)
      if((cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. '-def-')) then
        isort_order = 1
        rsort_order = 1.0d0
      elseif((cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'ascending')) then
        isort_order = 1
        rsort_order = 1.0d0
      elseif((cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'descending')) then
        isort_order = -1
        rsort_order = -1.0d0
      elseif((cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'decending')) then
C
C       Spelling of descending corrected, but keep around the old incorrect
C       spelling just to support old control files.
C
        isort_order = -1
        rsort_order = -1.0d0
      else
        write(logmess,9018) cmsgin(4)(1:icharlnf(cmsgin(3)))
 9018   format(" SORT:Invalid option:",a," Use:ascending or descending")
        call writloga('default',0,logmess,0,ier)
        goto 9999
      endif
C
C######################################################################
C
C
C    6 - Get arrays that control the multi-key sort and put them
C        into a real*8 work array.
C
C
      narg_input = 6
C
C    Tokens narg_input through nwds will be the arrays that control the sort.
C
      if(isort_type .ne. 'bins')then
         nkey = nwds - narg_input + 1
         nend = nwds
      else
         nkey = 1
         nend = narg_input
      endif
C
C    Use the first sort array to decide on the lenght of the sort vector.
C    All other sort vectors must be the same length.
C
      catt_name = cmsgin(narg_input)(1:icharlnf(cmsgin(narg_input)))
      call cmo_get_info(catt_name,cmonam,ipatt,ilen,itype_att,ier)
      call cmo_get_length(catt_name,cmonam,ilen,irank,ier)
 
      nsize = ilen * nkey
      nsort = ilen
 
C
C    Allocate a 1D array that is nsort*nkey long. This
C    will be passed into the heap sort routine and treated
C    as a 2D array rwork(nkey,nsort)
C
      call mmgetblk('rwork',isubname,iprwork,nsize,2,ier)
C
C    Fill the real*8 work array that will be passed to the sort routine
C
      do mkey = narg_input, nend
 
      catt_name = cmsgin(mkey)(1:icharlnf(cmsgin(mkey)))
 
      call cmo_get_info(catt_name,cmonam,ipatt,ilen,itype_att,ier)
C
C   Check that attribute is nsort long
C
      call cmo_get_length(catt_name,cmonam,ilen,irank,ier)
 
      if(ilen .ne. nsort)then
        write(logmess,9026) ilen
 9026   format(" SORT:ERROR att_in lenght = ",i11)
        call writloga('default',0,logmess,0,ier)
        write(logmess,9027) nsort
 9027   format(" SORT:ERROR nsort = ",i11)
        call writloga('default',0,logmess,0,ier)
        goto 9999
      endif
C
      call cmo_get_attparam(
     1   catt_name,cmonam,index,
     2   ctype,
     3   crank,
     4   clen,
     5   cinter,
     6   cpers,
     7   cio,
     8   ier)
 
         ioff_set = (mkey - narg_input)*nnodes
C
C   Kind of a funky counter system to get index value
C   but that's the way it is....
C
         if(ctype(1:4) .eq. 'VINT')then
            itype_att = 1
            do i = 1, nsort
               index = (i-1)*nkey + (mkey - narg_input + 1)
               rwork(index) = att_i(i)
            enddo
         elseif(ctype(1:7) .eq. 'VDOUBLE')then
            itype_att = 2
            do i = 1, nsort
               index = (i-1)*nkey + (mkey - narg_input + 1)
               rwork(index) = att_r(i)
            enddo
         else
        write(logmess,9028) ctype(1:icharlnf(ctype))
 9028   format(" SORT:ERROR att_in type=",a, " Must be VINT or VDOUBLE")
        call writloga('default',0,logmess,0,ier)
        goto 9999
         endif
      enddo
c
C######################################################################
C    5 - Get or create the sort key array
C
C        Do this after the previous step so that the dotask command
C        does not interfer with the parsing of the sort key array names.
C
C
      sort_key = cmsgin(5)
c
c   What if sort_key exists and is named??? Make sure
c   it is integer, and we will overwrite it.
c
C   If blank or default then use the name of the first
C   attribute (command line argument 6) to name the sort key.
C
      if((cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. '-def-')) then
        sort_key = 'ikey'//'_'//(cmsgin(6)
     1  (1:icharlnf(cmsgin(6))))
      endif
C
C      Check to see if 'sort_key' exists as a variable, if not
C      then add it using a dotask call
C
      ier = 0
      call mmfindbk(sort_key,cmonam,ipikey,ilen,ier)
      if(ier .ne. 0)then
         ier = 0
         if(nsort .eq. nnodes)then
            cmdmessage = 'cmo/addatt/ /'//
     1                 sort_key(1:icharlnf(sort_key))//
     2                 '/vint/scalar/nnodes/ / /gax/0;finish'
         elseif(nsort .eq. nelem)then
            cmdmessage = 'cmo/addatt/ /'//
     1                 sort_key(1:icharlnf(sort_key))//
     2                 '/vint/scalar/nelements/ / /gax/0;finish'
         else
            cmdmessage = 'cmo/addatt/ /'//
     1                 sort_key(1:icharlnf(sort_key))//
     2                 '/vint/scalar/'//clen//'/ / /gax/0;finish'
 
         endif
         call dotaskx3d(cmdmessage, ier)
       endif
 
       call cmo_get_info(sort_key,cmonam,ipikey,ilen,itype,ier)
 
C######################################################################
C     Allocate work array for index pointers
C
      call mmgetblk('iwork',isubname,ipiwork,nsort,1,ier)
 
      do i = 1, nsort
         iwork(i) = i
         ikey (i) = i
      enddo
C
C
C     Old version had to distinguish between real or integer arrays
C     but now the data vector to sort is always a real*8 work array.
C     No longer distinguish between real or integer when using the
C     hpsort routine.
C
      if(isort_type(1:4) .eq. 'bins')then
        if(itype_att .eq. 2)then
         call hpsortrmp(nsort, nkey, nkey, rwork, rsort_order, iwork)
         call index_bins_r(nsort,iwork,att_r,ikey,epsilon_user)
        else
         call hpsortip(nsort,att_i,rsort_order,ikey)
         call index_bins_i(nsort,iwork,att_i,ikey)
        endif
      elseif(isort_type(1:5) .eq. 'index')then
         call hpsortrmp(nsort, nkey, nkey, rwork, rsort_order, ikey)
      elseif(isort_type(1:4) .eq. 'rank')then
         call hpsortrmp(nsort, nkey, nkey, rwork, rsort_order, iwork)
         call index_rank(nsort,iwork,ikey)
      endif
C
C
C######################################################################
C
 9999 call mmrelprt(isubname,ier)
 
      return
      end
C
      subroutine sort_old
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C
C     Macro routine to support old sort syntax
C
C    Old Syntax still supported
C
C    USAGE - sort/xyz/[INDEX|bins|rank]
C
C
      implicit none
      character*90 cbuff
      integer ier, nwds
      character*32 cmsgin(nwds)
      integer imsgin(nwds)
      real*8  xmsgin(nwds)
      integer msgtyp(nwds)
      character*32 isort_type
      character*80 logmess
      integer icharlnf
C
C    1 - Get the sort method to be used (bins,index, or rank)
C
      if((cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'index') .or.
     1   (cmsgin(2)(1:icharlnf(cmsgin(3))) .eq. '-def-') .or.
     2   (nwds .eq. 1).or. (nwds .eq. 2))then
            isort_type = 'index'
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'bins') then
            isort_type = 'bins'
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'rank') then
            isort_type = 'rank'
      else
        write(logmess,9010) cmsgin(3)(1:icharlnf(cmsgin(3)))
 9010   format(" SORT: Invalid option: ",a)
        call writloga('default',0,logmess,0,ier)
        write(logmess,9015)
 9015   format
     1  ("Usage: sort/xyz/[index,bins,rank] ")
        call writloga('default',0,logmess,0,ier)
        goto 9999
      endif
 
      if(isort_type(1:5) .eq. 'index')then
         ier = 0
         cbuff = 'sort/-def-/index/ascending/i_index/xic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/index/ascending/j_index/yic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/index/ascending/k_index/zic;finish'
         call dotaskx3d(cbuff, ier)
      elseif(isort_type(1:4) .eq. 'bins')then
         ier = 0
         cbuff = 'sort/-def-/bins/ascending/i_index/xic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/bins/ascending/j_index/yic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/bins/ascending/k_index/zic;finish'
         call dotaskx3d(cbuff, ier)
      elseif(isort_type(1:4) .eq. 'rank')then
         ier = 0
         cbuff = 'sort/-def-/rank/ascending/i_index/xic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/rank/ascending/j_index/yic;finish'
         call dotaskx3d(cbuff, ier)
         cbuff = 'sort/-def-/rank/ascending/k_index/zic;finish'
         call dotaskx3d(cbuff, ier)
      endif
 
 9999 continue
      return
      end
C
C###################################################################
C
      subroutine index_rank(n_value,indx,irank)
      implicit none
      integer n_value, i
      integer indx(n_value), irank(n_value)
      do i = 1, n_value
         irank(indx(i)) = i
      enddo
      return
      end
C###################################################################
C
      subroutine index_bins_r(n_value,indx,arrin,index_array,eps_tst)
      implicit none
      integer n_value
      real*8 arrin(n_value)
      real*8 eps, eps_tst, value_test
      integer indx(n_value), index_array(n_value)
      integer n, index_test
C
      index_test = 1
      value_test = arrin(indx(1))
      eps = abs(value_test*eps_tst)
      do n = 1, n_value
         if((arrin(indx(n)) .ge. value_test - eps) .and.
     1      (arrin(indx(n)) .le. value_test + eps)) then
               index_array(indx(n)) = index_test
         else
            index_test = index_test + 1
            index_array(indx(n)) = index_test
            value_test = arrin(indx(n))
            eps = abs(value_test*eps_tst)
         endif
      enddo
      return
      end
C##################################################################
C
      subroutine index_bins_i(n_value,indx,iarrin,index_array)
      implicit none
      integer n_value
      integer iarrin(n_value)
      integer ivalue_test
      integer indx(n_value), index_array(n_value)
      integer n, index_test
C
      index_test = 1
      ivalue_test = iarrin(indx(1))
      do n = 1, n_value
         if((iarrin(indx(n)) .ge. ivalue_test) .and.
     1      (iarrin(indx(n)) .le. ivalue_test)) then
               index_array(indx(n)) = index_test
         else
            index_test = index_test + 1
            index_array(indx(n)) = index_test
            ivalue_test = iarrin(indx(n))
         endif
      enddo
      return
      end
C#################################################################
C
      subroutine indexx_r(n,arrin,indx)
C
C     indexx routine taken from Numerical Recipes,
C     Press, Flannery, Teukolsky, Vetterling
C
      implicit none
      integer i, j, n, l, ir, indxt
      real*8 arrin
      integer indx
      real*8 q
      dimension arrin(n),indx(n)
c
      do 11 j=1,n
        indx(j)=j
11    continue
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          indxt=indx(l)
          q=arrin(indxt)
        else
          indxt=indx(ir)
          q=arrin(indxt)
          indx(ir)=indx(1)
          ir=ir-1
          if(ir.eq.1)then
            indx(1)=indxt
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
          endif
          if(q.lt.arrin(indx(j)))then
            indx(i)=indx(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        go to 20
        endif
        indx(i)=indxt
      go to 10
      end
c#################################################################
c
      subroutine indexx_i(n,iarrin,indx)
c
c     indexx routine taken from numerical recipes,
c     press, flannery, teukolsky, vetterling
c
      implicit none
      integer i, j, n, l, ir, indxt
      integer iarrin
      integer indx
      real*8 q
      dimension iarrin(n),indx(n)
c
      do 11 j=1,n
        indx(j)=j
11    continue
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          indxt=indx(l)
          q=iarrin(indxt)
        else
          indxt=indx(ir)
          q=iarrin(indxt)
          indx(ir)=indx(1)
          ir=ir-1
          if(ir.eq.1)then
            indx(1)=indxt
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            if(iarrin(indx(j)).lt.iarrin(indx(j+1)))j=j+1
          endif
          if(q.lt.iarrin(indx(j)))then
            indx(i)=indx(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        go to 20
        endif
        indx(i)=indxt
      go to 10
      end
 
