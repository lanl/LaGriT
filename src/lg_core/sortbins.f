      subroutine sortbins
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C#######################################################################
C    FORMAT - sort/cmoname/bins/
C                  [ASCENDING|descending]/[ikey]/in_att/[epsilon_user]
C    FORMAT - sort/cmoname/index|rank/
C                  [ASCENDING|descending]/[ikey]/in_att1 in_att2 in_att3 ...
C    FORMAT - sort/cmoname/line_graph/
C                  [ASCENDING|descending]/[ikey]/lg_sort_type
C
C    INPUT ARGUMENTS - imsgin,xmsgin,cmsgin,msgtyp,nwds
C    TOKENS
C     1      sort
C     2      cmoname = name of MO to operate on ( / / and /-def/ supported)
C
C     3      index - multi-key sort such that in_att1(ikey(i)) i=1,...nnodes
C                    lists the attribute in_att1 in ascending or descending order.
C
C                    The ikey attribute will contain the permutation vector
C                    that could then be used in reordering the MO.
C                    (i.e. reorder/cmo/ikey)
C
C     3      rank  - multi-key sort such that ikey(i) lists the ranking of the node
C                    in the sorted list.
C     3      bins  - single-key sort which assigns each in_att1 value a bin.
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
C     3      line_graph algorithm sorts elements or nodes
C                    default calls sort on elements
C
C     4      ascending - default, sort in ascending order
C
C     4      descending - sort in descending order
C
C     5      ikey - integer vector (VINT) which will hold the output
C                   sorted key values. If it exists it will be used,
C                   if it does not exist it will be created.
C                   If token is -def- the default key attribute name
C                   will be concatination of 'ikey_' and the first
C                   input attribute name. (i.e. /-def-/imt will produce
C                   a sort key named ikey_imt). If the line_graph option
C		    is used, and token is -def- the key_name name will
C		    be named 'lg_ikey_nodes' or 'lg_ikey_elements'  
C	     
C
C      6     in_att - input attribute node based array upon which the
C                     sorting routine will sort
C
C                     Multi-key sorts can have an arbitrary number of input
C                     attributes. Attribute in_att1(n) has priority over
C                     in_att2(n) in breaking ties.
C
C
C      6     lg_sort_type (for line_graph) - character string which controls 
C                           the key_name
C			    that will be created by the line_graph
C			    algorithm. Options are 'nodes' and 'elements'
C                           Default is elements
C		    
C             Note: all attributes are put into a real*8 work array
C             before being sent to the sort routine.
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
      include 'local_element.h'
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
      pointer (ipitet, itet)
      pointer (ipitettyp, itettyp)
C
C     Component id, component type, and loop id attributes, to be
C     created if the line_graph option is used.
C
      pointer (ipcid, cid)
C     Ordinarily we would call this attribute ctype. But let's call it
C     comptype because the variable ctype is already in use in this
C     source file.
      pointer (ipcomptype, comptype)
      pointer (iploopid, loopid)

      real*8 rwork(10000000)
      real*8 att_r(10000000)
      integer att_i(10000000)
      integer ikey(10000000)
      integer iwork(10000000)
      integer itet(10000000)
      integer itettyp(10000000)
      integer itype, ilen, ier, itype_att, irank
      integer ierror, ierr
      integer nwds, nnodes, nkey, mkey, nsize, ioff_set, narg_input
      integer cid, comptype, loopid
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
      character*32 key_name
C     nsort_clen will either be 'nnodes' or 'nelements'.
      character*32 nsort_clen, lg_sort_type
      character*32 isort_type
      character*32 sort_order
      character*32 catt_name
      character*32 ctype,crank,clen,cinter,cpers,cio
      character*132 cmdmessage
      character*132 logmess
C
C#######################################################################
C begin BEGIN SORT

C     set defaults for some important name strings and counters 
      isubname='sortbins'
      isort_type = 'notset'
      nsort_clen = 'notset'
      lg_sort_type = 'notset'
      key_name = 'notset'
      nnodes = 0
      nelem = 0
      nsort = 0
      ier = 0
c
C#######################################################################
C    SETUP and PARSE TOKENS
C#######################################################################
c
c    TOKEN 2 - Get the mesh object
c

c     Get default mesh object if -def-
      cmonam = cmsgin(2)
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmonam, ier)
         if(ier.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format(" ERROR SORT: found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           goto 9999
         endif
      endif

C     Check that cmonam is a valid mesh object

      call cmo_exist(cmonam,ier)
      if(ier.ne.0) then
        write(logmess,*)
     *  ' ERROR SORT: mesh object does not exist: '
     *   //cmonam(1:icharlnf(cmonam))
        call writloga('default',0,logmess,1,ierr)
        goto 9999
      endif

      call cmo_get_intinfo('nnodes',cmonam,nnodes,ilen,itype,ier)
      if(ier .ne. 0) then
        write(logmess,9005) cmonam(1:icharlnf(cmonam))
 9005  format(" ERROR SORT: looking for nnodes of mesh object: ",a)
        call writloga('default',0,logmess,0,ier)
        goto 9999
      endif

C     Check that mesh object has elements
C
C      call cmo_get_intinfo('nelements',cmonam,nelem,ilen,itype,ier)
C      if(ier .ne. 0)then
C        write(logmess,9006) cmonam(1:icharlnf(cmonam))
C 9006  format(" ERROR SORT: looking for nelements of mesh object: ",a)
C        call writloga('default',0,logmess,1,ier)
C        goto 9999
C      endif
C      if (nelem .eq. 0) then
C         write(logmess, '(a)')
C     *   ' ERROR SORT: mesh object has 0 elements.'
C         call writloga('default',0,logmess,1,ier)
C         goto 9999
C      endif

C
C#######################################################################
C
C    TOKEN 3 - Get the sort method to be used (bins,index,rank,etc.)
C              also check on specific tokens related to each
C

C     isort_type index
      if((cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'index') .or.
     1   (cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. '-def-') .or.
     2   (nwds .eq. 1).or. (nwds .eq. 2))then
         isort_type = 'index'

C     isort_type bins
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'bins') then
         isort_type = 'bins'
C
C        TOKEN 7 for bins - user specified epsilon

         epsilon_user = 1.e-10
         if(nwds .gt. 6)then
           if(msgtyp(7) .eq. 2)then
              epsilon_user = xmsgin(7)
           else
              write(logmess,9008) msgtyp(7)
 9008       format(" Warning SORT: Arg 7 epsilon_user not float: ",i2)
              call writloga('default',0,logmess,0,ier)
              write(logmess,'(a)')
     *        " SORT: Using default epsilon = 1.e-10 "
              call writloga('default',0,logmess,0,ier)
           endif
         endif

C     isort_type rank
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'rank') then
         isort_type = 'rank'

C     isort_type line_graph
      elseif(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq. 'line_graph') then

C        Check that mesh object has elements
         call cmo_get_intinfo('nelements',cmonam,nelem,ilen,itype,ier)
         if(ier .ne. 0)then
           write(logmess,9006) cmonam(1:icharlnf(cmonam))
 9006      format(" ERROR SORT: looking for nelements of 
     *            mesh object: ",a)
           call writloga('default',0,logmess,1,ier)
           goto 9999
         endif
         if (nelem .eq. 0) then
            write(logmess, '(a)')
     *      ' ERROR SORT: mesh object has 0 elements.'
            call writloga('default',0,logmess,1,ier)
            goto 9999
         endif

         isort_type = 'line_graph'

         if (nwds .gt. 6) then
           write(logmess,'(a)')
     *      ' ERROR SORT: Too many arguments for line_graph option'
           call writloga('default',0,logmess,1,ier)
           goto 9999
         endif

      else
C       TOKEN 3 has not been recognized

        write(logmess,9010) cmsgin(3)(1:icharlnf(cmsgin(3)))
 9010   format(" ERROR SORT: invalid option 3 : ",a)
        call writloga('default',0,logmess,0,ier)
        write(logmess,9015)
 9015   format
     1  ("Usage: sort/cmo/[INDEX,bins,rank,line_graph]/...")
        call writloga('default',0,logmess,1,ier)
        goto 9999

      endif
C
C
C#######################################################################
C
C    TOKEN 4 - Determine if sort is in ascending or descending order
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
        write(logmess,'(a,a)')
     *  " ERROR SORT: invalid option 4: ",
     *    cmsgin(4)(1:icharlnf(cmsgin(4)))
        call writloga('default',0,logmess,0,ier)
        write(logmess,'(a)') 
     *  " SORT: option should be ascending or descending"
        call writloga('default',0,logmess,1,ier)
        goto 9999
      endif

C######################################################################
C     TOKEN 5 - Get or create the sort key array name key_name
C
C     If default -def- then use the name of the first
C     attribute (command line argument 6) to name the sort key.
C
      if((cmsgin(5)(1:icharlnf(cmsgin(5))) .ne.'-def-') .and.
     *   nwds.ge.5 .and. msgtyp(5).eq.3  ) then
         key_name = cmsgin(5)

      elseif ((cmsgin(5)(1:icharlnf(cmsgin(5))).eq.'-def-') .and.  
     *    nwds.ge.6 .and. msgtyp(6).eq.3 ) then

        key_name = 'ikey'//'_'//(cmsgin(6)(1:icharlnf(cmsgin(6))))
        write(logmess,'(a,a)')
     *  " SORT: using created name for sort key: ",
     *  key_name(1:icharlnf(key_name))   
        call writloga('default',0,logmess,0,ier)

      elseif (nwds.lt.5 .and. key_name(1:6).eq.'notset') then

        key_name = 'lg_key_line_graph'
        write(logmess,'(a,a)')
     *  " SORT: using default name for sort key: ",
     *  key_name(1:icharlnf(key_name))   
        call writloga('default',0,logmess,0,ier)

      endif
 
C######################################################################
C    TOKEN 6 - depends on isort_type
C            - get key name and add attribute of appropriate length

C     line_graph 
C     TOKEN 6 is lg_sort_type 
C     defined as elements or nodes (elements is the default)
C     note, we use lg_sort_type, but may be able to just use nsort_clen
      if (isort_type .eq. 'line_graph') then

        if (nwds .gt. 6) then
           write(logmess,'(a)')
     *      ' SORT ERROR: Too many arguments for line_graph option'
           call writloga('default',0,logmess,1,ier)
           goto 9999

         elseif (nwds .lt. 6 ) then
              lg_sort_type = "elements"
              write(logmess, '(a)')
     *        ' Default line_graph option: using "elements"'
              call writloga('default',0,logmess,0,ier)
         else
C           if nwds equal 6 look for valid key word tokens

            lg_sort_type = cmsgin(6)

C           allow short version node or nodes, elem or elements
            if(lg_sort_type(1:4).ne.'node' .and.
     *        lg_sort_type(1:4) .ne.'elem' )
     *        then
              write(logmess, '(a)')
     *        'Invalid option: line_graph must be "nodes" or "elements"'
              call writloga('default',0,logmess,0,ier)
              goto 9999
            endif

         endif

C        check for valid cmo and set attribute name
         if (lg_sort_type(1:4) .eq. 'node') then
            if (nnodes .eq. 0) then
                write(logmess, '(a)')
     *      ' ERROR SORT: Quitting early because there are 0 nodes.'
                call writloga('default',0,logmess,0,ier)
                goto 9999
             endif
             nsort = nnodes
             if (key_name(1:8) .eq. '-notset-') 
     *           key_name = "lg_key_nodes"
             nsort_clen = "nnodes"
             lg_sort_type = "nodes"

         else
            if (nelem .eq. 0) then
               write(logmess, '(a)')
     *    ' ERROR SORT: Quitting early because there are 0 elements.'
               call writloga('default',0,logmess,0,ier)
               goto 9999
             endif
             nsort = nelem
             if (key_name(1:8) .eq. '-notset-') 
     *           key_name = "lg_key_elements"
             nsort_clen = "nelements"
             lg_sort_type = "elements"
         endif 

      else 
C     TOKEN 6 - for all except line_graph
C     Get arrays that control the multi-key sort and put them
C     into a real*8 work arrays. 

         narg_input = 6
C
C        TOKEN(S) narg_input through nwds 
C        will be the arrays that control the sort.
C
         if(isort_type .ne. 'bins')then
            nkey = nwds - narg_input + 1
            nend = nwds
         else
            nkey = 1
            nend = narg_input
         endif
C
C        Use the first sort array to decide on the length of the sort
C        vector. All other sort vectors must be the same length.
C
         catt_name = cmsgin(narg_input)(1:icharlnf(cmsgin(narg_input)))
         call cmo_get_attparam(
     1      catt_name,cmonam,index,
     2      ctype,
     3      crank,
     4      clen,
     5      cinter,
     6      cpers,
     7      cio,
     8      ier)
         call cmo_get_length(catt_name,cmonam,ilen,irank,ier)
         nsize = ilen * nkey
         nsort = ilen
         nsort_clen = clen

         if(nsort .le. 0)then
          write(logmess, '(a,a,a,a)') " ERROR SORT: attribute: "
     *     ,catt_name(1:icharlnf(catt_name))
     *     ," of length ", clen
           call writloga('default',0,logmess,0,ier)
           write(logmess,9027) nsort
 9027     format(" ERROR SORT: nsort = ",i11)
           call writloga('default',0,logmess,1,ier)
           goto 9999
         endif

C
C        Allocate a 1D array that is nsort*nkey long. This
C        will be passed into the heap sort routine and treated
C        as a 2D array rwork(nkey,nsort)
C
         call mmgetblk('rwork',isubname,iprwork,nsize,2,ier)
         if(ier .ne. 0)then
           write(logmess, '(a,a,a,i11)') 
     *     " ERROR SORT: allocating work array: rwork for option "
     *     ,isort_type(1:icharlnf(isort_type))
     *     ," of size ",nsize
           call writloga('default',0,logmess,1,ier)
           goto 9999
         endif

C
C        Fill the real*8 work array that will be passed to the sort
C        routine
C
         do mkey = narg_input, nend
 
         catt_name = cmsgin(mkey)(1:icharlnf(cmsgin(mkey)))
 
         call cmo_get_info(catt_name,cmonam,ipatt,ilen,itype_att,ier)
C
C        Check that attribute is nsort long
C
         call cmo_get_length(catt_name,cmonam,ilen,irank,ier)
 
         if(ilen .ne. nsort)then
           write(logmess,9026) ilen
 9026      format(" ERROR SORT: att_in length = ",i11)
           call writloga('default',0,logmess,0,ier)
           write(logmess,9027) nsort
           call writloga('default',0,logmess,1,ier)
           goto 9999
         endif
C
         call cmo_get_attparam(
     1      catt_name,cmonam,index,
     2      ctype,
     3      crank,
     4      clen,
     5      cinter,
     6      cpers,
     7      cio,
     8      ier)
 
            ioff_set = (mkey - narg_input)*nnodes
C
C           Kind of a funky counter system to get index value
C           but that's the way it is....
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
 9028          format(" ERROR SORT: attribute type=",a,
     *            " Must be VINT or VDOUBLE")
               call writloga('default',0,logmess,1,ier)
               goto 9999
            endif
         enddo

      endif
C     end TOKEN 6

C######################################################################
C     SETUP DONE - PARSED TOKENS DONE
C######################################################################
C     Allocate or get arrays for sort key pointer

C      get or create the sort key array with length type nsort_clen
C      Check to see if value in 'key_name' exists as a variable, 
C      if not, then create it using a dotask call
C
C      Note,this should correctly create the attribute length
C      as nnodes or nelements as defined in value of nsort_clen 
C
C      Note, there are some redundancies in the code with error
C      checking and extra variables that can be cleaned up.
C      For now they are left errors are caught if there are holes in logic 
C
      ier = 0
      call mmfindbk(key_name,cmonam,ipikey,ilen,ier)

C     extra checks to make sure attribute for sort key is correct 
C     some logic in above code will not have created the attribute yet
      if(ier .ne. 0) then

         ier = 0
         cmdmessage='cmo/addatt/'//cmonam(1:icharlnf(cmonam)) //
     1   '/'// key_name(1:icharlnf(key_name))//
     2   '/vint/scalar/'//nsort_clen//'/ / /gax/0 ; finish'
         call dotaskx3d(cmdmessage, ier)

      else
C     check that existing attribute has the correct length
C     we may want to delete these, but should work to reuse
C     as key array is intialized each time to 1 thru nsort

          if (nsort_clen(1:6) .eq. 'nnodes') then

             if (ilen .ne. nnodes) then
               cmdmessage='cmo/DELATT/'//cmonam(1:icharlnf(cmonam)) //
     *         '/'// key_name(1:icharlnf(key_name))//' ; finish'
               call dotaskx3d(cmdmessage, ier)

               cmdmessage='cmo/addatt/'//cmonam(1:icharlnf(cmonam)) //
     *         '/'// key_name(1:icharlnf(key_name))//
     *         '/vint/scalar/'//nsort_clen//'/ / /gax/0 ; finish'
               call dotaskx3d(cmdmessage, ier)

             endif

          elseif (nsort_clen(1:9) .eq. 'nelements') then

             if (ilen .ne. nelem ) then
               cmdmessage='cmo/DELATT/'//cmonam(1:icharlnf(cmonam)) //
     *         '/'// key_name(1:icharlnf(key_name))//' ; finish'
               call dotaskx3d(cmdmessage, ier)

               cmdmessage='cmo/addatt/'//cmonam(1:icharlnf(cmonam)) //
     *         '/'// key_name(1:icharlnf(key_name))//
     *         '/vint/scalar/'//nsort_clen//'/ / /gax/0 ; finish'
               call dotaskx3d(cmdmessage, ier)
             endif

          endif

      endif

C######################################################################
C     Allocate work array for index pointers and check ikey
C
      call cmo_get_info(key_name,cmonam,ipikey,ilen,itype,ier)

C     this is an extra check, nsort should be correctly set
C     to nnodes or nelements by this point
      if ( (nsort.ne.ilen) .or. (ier.ne.0) ) then
           write(logmess,'(a,a5,i11,a,a)')
     *      " ERROR SORT: invalid length  ",
     *      nsort_clen(1:4), nsort,
     *      " for attribute: ", key_name
           call writloga('default',0,logmess,1,ier)
           goto 9999
      endif

      call mmgetblk('iwork',isubname,ipiwork,nsort,1,ier)
      if (ier.ne.0 .or. nsort.eq.0) then 
          write(logmess,9030) nsort
 9030   format(" ERROR SORT: work array allocation length = ",i11)
          call writloga('default',0,logmess,1,ier)
          goto 9999
      else
        do i = 1, nsort
           iwork(i) = i
           ikey(i) = i
        enddo
      endif
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
      elseif(isort_type(1:10) .eq. 'line_graph') then
         call cmo_get_info('itet', cmonam, ipitet, ilen, itype, ier)
         if(ier.ne.0) call x3d_error(isubname,'get info itet')
         call cmo_get_info('itettyp', cmonam, ipitettyp, ilen, itype,
     *      ier)
         if(ier.ne.0) call x3d_error(isubname,'get info itettyp')
C
C        Loop through and make sure that itet consists solely of line
C        segments.
C
         do i = 1, ilen
            if (itettyp(i) .ne. ifelmlin) then
               write(logmess, *)
     *    ' ERROR SORT: line_graph elements must be of type line.'
               call writloga('default',0,logmess,0,ier)
               ier = 1
               goto 9999
            endif
         enddo


C    SORT BY ELEMENT
      if (lg_sort_type .eq. 'elements') then
C
C          Create new temporary attributes.
C          remove attributes to zero them out.
           ier = 0

           cmdmessage='cmo/DELATT/'//cmonam(1:icharlnf(cmonam))//
     *     '/cid; finish'
           call dotaskx3d(cmdmessage, ier)
           cmdmessage='cmo/addatt/'//cmonam(1:icharlnf(cmonam))//
     *     '/cid/VINT/scalar/nelements/linear/temporary; finish'
           call dotaskx3d(cmdmessage, ier)

           call cmo_get_info('cid', cmonam(1:icharlnf(cmonam)), ipcid,
     *        ilen, itype, ier)
           if(ier.ne.0) call x3d_error(isubname,'get info cid')

           cmdmessage = 'cmo/DELATT/' // cmonam(1:icharlnf(cmonam)) //
     *      '/ctype; finish'
           call dotaskx3d(cmdmessage, ier)
           cmdmessage='cmo/addatt/'//cmonam(1:icharlnf(cmonam)) //
     *      '/ctype/VINT/scalar/nelements/linear/temporary; finish'
           call dotaskx3d(cmdmessage, ier)
          
           call cmo_get_info('ctype', cmonam(1:icharlnf(cmonam)),
     *        ipcomptype, ilen, itype, ier)
           if(ier.ne.0) call x3d_error(isubname,'get info ctype')

           cmdmessage='cmo/DELATT/'//cmonam(1:icharlnf(cmonam)) //
     *      '/loopid; finish'
           call dotaskx3d(cmdmessage, ier)
           cmdmessage='cmo/addatt/'// cmonam(1:icharlnf(cmonam)) //
     *      '/loopid/VINT/scalar/nelements/linear/temporary; finish'
           call dotaskx3d(cmdmessage, ier)
          
           call cmo_get_info('loopid', cmonam(1:icharlnf(cmonam)),
     *        iploopid, ilen, itype, ier)
           if(ier.ne.0) call x3d_error(isubname,'get info loopid')

C
C   setup done, now let the C++ code take care of the rest!
C
           call line_graph_sort(itet, cid, comptype, loopid, ikey, 
     &        nelem)

C   SORT BY NODE
C   note the last argument is the number of elements
        elseif (lg_sort_type .eq. 'nodes') then

          call line_graph_nsort(itet, ikey, nelem)

        endif
      endif
C
C
C######################################################################
C

        write(logmess,'(a,a)')
     *  " SORT: order key written to attribute: ",
     *  key_name(1:icharlnf(key_name))
        call writloga('default',0,logmess,1,ier)

 9999 call mmrelprt(isubname,ierror)
 
      return
      end
C
C######################################################################
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

      integer ier, nwds
      character*32 cmsgin(nwds)
      integer imsgin(nwds)
      integer msgtyp(nwds)
      real*8  xmsgin(nwds)

      integer icharlnf

      character*32 isort_type
      character*132 logmess
      character*132 cbuff

C######################################################################
C begin BEGIN OLD SORT

C    NWDS = 3
C    TOKEN 3 - Get the sort method to be used (bins,index, or rank)
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
 9010   format(" SORT ERROR: Invalid option: ",a)
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
 
