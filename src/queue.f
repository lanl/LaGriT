C
C##############################################################################
C
C   PURPOSE - 
C
C       This file implements a QUEUE in fortran. This queue is
C       fixed-size, and implemented as an array, rather than as a linked
C       list, because fortran doesn't like linked lists, really. The
C       included methods are as follows:
C
C       *   queue_c - This is the constructor. It takes in a name for
C       the queue to be built and the desired maximum size of the queue.
C       It also takes in the type of the queue (integer, double, real).
C       It returns only an error code (0 = good). 
C       *   queue_push - This pushes a value onto the queue. It takes in
C       the name of the queue, a pointer to the data to be pushed on
C       (the data itself will be copied into the queue's memory space),
C       and the type of the data. It returns an error code (0 = good).
C       *   queue_pop - This pops the frontmost values from the queue.
C       It takes in the name of the queue, a pointer to an allocated
C       block of memory for the data to be copied into, and the type of
C       the data to be returned. It copies the data into the given
C       memory block, and returns and error code (0 = good).
C       *   queue_d - This is the deconstructor. It takes in the name of
C       the queue to be destroyed and releases all memory associated
C       with it. It returns an error code as well.
C
C   SYNTAX - 
C
C       SIZE is the number of elements in the array - not the number of
C       bytes to be allocated. Thus, the size of a 100 element
C       character*32 queue would the same as the size of a 100 element
C       integer queue.
C
C       IVALTYPE is an integer representing the type of the elements to
C       be placed in the queue. The possible types are:
C           1 - INTEG
C           2 - REAL*8
C           3 - CHARACTER*32
C
C   NOTE - 
C       The name of the queue is used to identify it to the
C       memory manager. Therefore the name of each queue MUST be unique.
C       Otherwise, it could easily become confused. Specifically, all
C       attributes of a single queue are allocated in a mm partition
C       that is given the name "queue_queuename" where queuename is the
C       name input to the constructor. Thus, if two queues are given the
C       same name, their data will end up in the same partition, and
C       push and/or pop could accidentally affect the wrong queue.
C
C##############################################################################
C

        subroutine queue_c
     &      (qname, ilen, ivaltype, ierror)

        implicit none

C###### Variables for input and output
        character*32        qname
        integer             ilen
        integer             ivaltype
        integer             ierror

C###### Variables for memory access
        pointer             (ipqueue1,   queue1)
        integer             queue1(ilen)
        pointer             (ipqueue2,   queue2)
        real*8              queue2(ilen)
        pointer             (ipqueue3,   queue3)
        character*32        queue3(ilen)
        pointer             (ipfront,   front)
        pointer             (ipback,    back)
        pointer             (ipsize,    qsize)
        pointer             (iptype,    qtype)
        pointer             (ipflag,    emptyflag)

C###### Variables for internal use
        character*32        isubname
        character*40        partname
        integer             front, back, emptyflag, qtype, qsize
        character*128       logmess

C###### Constants
        integer             INTEG, REAL, STRING
        data INTEG, REAL, STRING /1,2,3/

C###### Finally, some actual code.
        isubname = "queue"

        !print *, "Constructing queue"

C       Test to make sure the inputs make sense
        if (ivaltype .lt. 1 .or. ivaltype .gt. 3) then
            write(logmess, '(a)') 
     &          "ERROR - QUEUE: Invalid queue type specified."
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9990
        endif
        if (ilen .le. 0) then
            write(logmess, '(a)') 
     &          "ERROR - QUEUE: Invalid queue length specified."
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9990
        endif

C       Allocate memory for queue data object
        write(partname, '(a, a)') "queue_", qname
        !print *,partname
        call mmgetblk('front',partname,ipfront,1,1,ierror)
        !print *,ipfront,ierror
        call mmgetblk('back',partname,ipback,1,1,ierror)
        !print *,ipback,ierror
        call mmgetblk('qsize',partname,ipsize,1,1,ierror)
        !print *,ipsize,ierror
        call mmgetblk('qtype',partname,iptype,1,1,ierror)
        !print *,iptype,ierror
        call mmgetblk('emptyflag',partname,ipflag,1,1,ierror)
        !print *,ipflag,ierror
        if (ivaltype .eq. INTEG) then
            call mmgetblk('queue',partname,ipqueue1,ilen,INTEG,ierror)
        elseif (ivaltype .eq. REAL) then
            call mmgetblk('queue',partname,ipqueue2,ilen,REAL,ierror)
        elseif (ivaltype .eq. STRING) then
            call mmgetblk('queue',partname,ipqueue3,ilen,STRING,ierror)
        endif

C       Initialize memory for the queue data object
        qsize = ilen
        qtype = ivaltype
        emptyflag = 1
        front = 1
        back = 1
        
        ierror = 0

9990    continue
        return
        end

C##############################################################################

        subroutine queue_push
     &      (qname, ipvalue1, ipvalue2, ipvalue3, ivaltype, ierror)

        implicit none

C###### Variables for input and output
        character*32        qname
        pointer             (ipvalue1,   value1)
        pointer             (ipvalue2,   value2)
        pointer             (ipvalue3,   value3)
        integer             ivaltype
        integer             ierror

C###### Variables for memory access
        integer             ilen
        pointer             (ipqueue1,   queue1)
        integer             queue1(*)
        pointer             (ipqueue2,   queue2)
        real*8              queue2(*)
        pointer             (ipqueue3,   queue3)
        character*32        queue3(*)
        pointer             (ipfront,   front)
        pointer             (ipback,    back)
        pointer             (ipsize,    qsize)
        pointer             (iptype,    qtype)
        pointer             (ipflag,    emptyflag)

C###### Variables for internal use
        character*32        isubname
        character*40        partname
        integer             front, back, emptyflag, qtype, qsize
        integer             value1
        real*8              value2
        character*32        value3
        character*128       logmess

C###### Constants
        integer             INTEG, REAL, STRING
        data INTEG, REAL, STRING /1,2,3/

C###### Finally, some actual code.
        isubname = "queue_push"

        !print *,"Pushing item ", value1

C       Find values of queue object parameters 
        write(partname, '(a, a)') "queue_", qname
        !print *,partname
        call mmfindbk('front',partname,ipfront,ilen,ierror)
        !print *,ierror,ilen,ipfront
        call mmfindbk('back',partname,ipback,ilen,ierror)
        !print *,ierror,ilen,ipback
        call mmfindbk('qsize',partname,ipsize,ilen,ierror)
        !print *,ierror,ilen,ipsize
        call mmfindbk('qtype',partname,iptype,ilen,ierror)
        !print *,ierror,ilen,iptype
        call mmfindbk('emptyflag',partname,ipflag,ilen,ierror)
        !print *,ierror,ilen,ipflag
        if (ivaltype .eq. INTEG) then
            call mmfindbk('queue',partname,ipqueue1,ilen,ierror)
            !print *,ierror,ilen,ipqueue
        elseif (ivaltype .eq. REAL) then
            call mmfindbk('queue',partname,ipqueue2,ilen,ierror)
        elseif (ivaltype .eq. STRING) then
            call mmfindbk('queue',partname,ipqueue3,ilen,ierror)
        endif
        
        if (ierror .ne. 0) then
            !print *,2,1
            write(logmess, '(a)') 
     &          "ERROR - QUEUE_PUSH: Queue does not exist."
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9991
        endif
        
        !print *,emptyflag
C       Make sure the queue isn't full
        if (emptyflag .eq. -1) then
            write(logmess, '(a)') 
     &          "ERROR - QUEUE_PUSH: Queue is already full."
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9991
        endif
        
C       Push the new value onto the tail of the queue
        if (ivaltype .eq. INTEG) then
            queue1(back) = value1
        elseif (ivaltype .eq. REAL) then
            queue2(back) = value2
        elseif (ivaltype .eq. STRING) then
            queue3(back) = value3
        endif
        back = back + 1
        if (emptyflag .eq. 1) emptyflag = 0
        if (back .gt. qsize) back = 1
        if (back .eq. front) emptyflag = -1
                         
        ierror = 0

9991    continue
        return
        end

C##############################################################################

        subroutine queue_pop
     &      (qname, ipvalue1, ipvalue2, ipvalue3, ivaltype, ierror)

        implicit none

C###### Variables for input and output
        character*32        qname
        pointer             (ipvalue1,   value1)
        pointer             (ipvalue2,   value2)
        pointer             (ipvalue3,   value3)
        integer             ivaltype
        integer             ierror

C###### Variables for memory access
        integer             ilen
        pointer             (ipqueue1,   queue1)
        integer             queue1(*)
        pointer             (ipqueue2,   queue2)
        real*8              queue2(*)
        pointer             (ipqueue3,   queue3)
        character*32        queue3(*)
        pointer             (ipfront,   front)
        pointer             (ipback,    back)
        pointer             (ipsize,    qsize)
        pointer             (iptype,    qtype)
        pointer             (ipflag,    emptyflag)

C###### Variables for internal use
        character*32        isubname
        character*40        partname
        integer             front, back, emptyflag, qtype, qsize
        integer             value1
        real*8              value2
        character*32        value3
        character*128       logmess

C###### Constants
        integer             INTEG, REAL, STRING
        data INTEG, REAL, STRING /1,2,3/

C###### Finally, some actual code.
        isubname = "queue_pop"

        !print *,"Popping item ", value1, value2, value3

C       Find values of queue object parameters 
        write(partname, '(a, a)') "queue_", qname
        call mmfindbk('front',partname,ipfront,ilen,ierror)
        call mmfindbk('back',partname,ipback,ilen,ierror)
        call mmfindbk('qsize',partname,ipsize,ilen,ierror)
        call mmfindbk('qtype',partname,iptype,ilen,ierror)
        call mmfindbk('emptyflag',partname,ipflag,ilen,ierror)
        if (ivaltype .eq. INTEG) then
            call mmfindbk('queue',partname,ipqueue1,ilen,ierror)
        elseif (ivaltype .eq. REAL) then
            call mmfindbk('queue',partname,ipqueue2,ilen,ierror)
        elseif (ivaltype .eq. STRING) then
            call mmfindbk('queue',partname,ipqueue3,ilen,ierror)
        endif
        if (ierror .ne. 0) then
            write(logmess, '(a)') 
     &          "ERROR - QUEUE_POP: Queue does not exist."
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9992
        endif

C       Make sure the queue isn't empty
        if (emptyflag .eq. 1) then
C           write(logmess, '(a)') 
C    &          "ERROR - QUEUE_POP: Queue is already empty."
C           call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9992
        endif

C       Pop the value off the head of the queue
        if (ivaltype .eq. INTEG) then
            value1 = queue1(front)
        elseif (ivaltype .eq. REAL) then
            value2 = queue2(front)
        elseif (ivaltype .eq. STRING) then
            value3 = queue3(front)
        endif
        front = front + 1
        if (emptyflag .eq. -1) emptyflag = 0
        if (front .gt. qsize) front = 1
        if (front .eq. back) emptyflag = 1

        ierror = 0

9992    continue
        return
        end

C##############################################################################

        subroutine queue_d
     &      (qname, ierror)

        implicit none

        character*32        qname
        integer             ierror
        character*40        partname

        !print *,"Destructing queue"

        write(partname, '(a, a)') "queue_", qname

        call mmrelprt(partname, ierror)

9993    continue
        return
        end

C##############################################################################
