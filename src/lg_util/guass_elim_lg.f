*dk, guass_elim_lg
        subroutine guass_elim_lg(flag,n,LDA,epsilon,A,b,x
     &                            ,wrkmat,ierror)
 
C #####################################################################
C
C     PURPOSE -
C
C        solve general Ax=b using Gaussian elimination.
C        (A can be singular and non-symmetric, but must be square).
C
C     INPUT ARGUMENTS -
C
C        A - the array to invert
C        LDA - MAX first dimension of A to get storage correct
C        n - dimension of problem: A(1:n,1:n)*x(1:n)=b(1:n)
C        b - the (input) b vector in Ax=b
C        epsilon - relative accuracy
C             The average eigenvalue of b is computed as |Ab|/|b|,
C             and hence |x|~|b|/(|Ab|/|b|). Corrections dx to the estimate along
C             eigendirections of A with significantly smaller eigenvalues
C             give large changes to x for negligible improvement to |Ax-b|.
C             epsilon is used for the relative tests as appropriate.
C        flag - switch to indicate whether to calculate solution of
C               (1) Ax=b
C               (2) (A^2+eps^2*I)x=Ab where I(i,j)=(1 if i=j, 0 otherwise)
C                   [solve (A+i*eps)x=b in sense of Hadamard principle part]
C
C     OUTPUT ARGUMENTS -
C
C        x - the (output) solution vector x to Ax=b
C        ierror - set on return to
C                -1         if A=0 and b.ne.0
C                 0         if (|Ax-b|/|b|)^2 < epsilon,
C                 1+min((|Ax-b|/|b|)^2/(10*epsilon),1000000)
C                           otherwise
C                 |Ax-b|/|b| < epsilon should also hold
C                 but using this softer test for the error return.
C
C     WORKSPACE ARGUMENTS -
C
C        wrkmat - nxn matrix workspace (contents ignored)
C
C     SPELLING NOTES -
C
C        oops: Gauss is spelled "gauss" not "guass",
C        but since I checked in in that way I guess it'll stay ...
C
C     PROGRAMMING NOTES -
C
C        The basic code is a straightforward implementation of
C        Gaussian elimination. The modifications here are
C        giving the user the choice to solve (A^2+eps*I)x=Ab
C        (Hadamard principle part) for (nearly) singular matricies,
C        and putting in the epsilon tests (and calculating the scale
C        for epsilon). As it is a principle part, flag=2 approaches
C        epsilon=0 as a well defined limit, whereas flag=1 can be
C        "unpredictable" especially for the nearly singular case.
C
C        For nearly singular matricies where "wild" contributions (essentially
C        unrestricted small eigenvalue components) to x are undesireable,
C        using flag=2 and epsilon "relatively large" (1.e-2 to 1.e-4)
C        is recommended. This also works pretty well for the non-singular cases.
C
C        Using flag=1 (straightforward Ax=b solution)
C        has accuracy problems in certain cases, eg for the 2x2 problem
C           ( 0 1 ) (x1)   ( 1 )           (x1)   (0.999977878280)
C           ( 1 0 ) (x2) = ( 1 )  one gets (x2) = (0.999999999999) for epsilon=1.e-12;
C        flag=2 is more stable, although for large matricies it is slightly
C        more inefficient since the matrix has to be squared. (There is one
C        other "N^3" loop in this code, so it is not a huge overhead compare to
C        flag=1 -- the coding presumption is that the dimension of the matrix A
C        is typically n=3 and not n=1000...)
C        However, note that for flag=1 "often" |Ax-b|/|b| << epsilon,
C        whereas for flag=2 |Ax-b|/|b| ~ epsilon "almost always".
C
C        Although not coded here, a "best" epsilon could also be selected
C        by minimizing a functional showing a minimum and having the
C        appropriate symmetry properties, such as
C            chisq=|x|^2*(Tr A^2 + 3 eps^2)^2
C        or looking to see if/where |x|-|b|/(|Ab|/|b|) change sign.
C        (These are 2 empirical tests that worked fairly well for
C        the set of small matricies I looked at, which came from
C        typical cases projecting face velocity onto node velocity.)
C
C        Also compare solv_Mxb_schmidt_hilbert_lg (forms inverse with Schmidt-Hilbert).
C        For large matrices, using an iterative sparse solver such as GMRES is recommended
C
C     CHANGE HISTORY -
C
C        $Log: guass_elim_lg.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   17 Feb 2001 14:19:24   jtg
CPVCS    fixed log line; Rev 1.6=correctly did Rev 1.5
CPVCS    
CPVCS       Rev 1.5   Wed Mar 08 15:40:30 2000   dcg
CPVCS    change debug flag so that error messages are not written
CPVCS    return flag will have error status
CPVCS
CPVCS       Rev 1.2   Mon Dec 13 10:47:16 1999   jtg
CPVCS    Noted Gauss is spelled wrong
CPVCS
CPVCS       Rev 1.1   Wed Dec 01 15:43:52 1999   jtg
CPVCS    set local_debug to 0
CPVCS
CPVCS       Rev 1.0   Wed Dec 01 14:35:32 1999   jtg
CPVCS    Initial revision.
C
C ######################################################################
 
        implicit none
 
        integer n,LDA,flag,ierror
 
        real*8 x(n),b(n),A(LDA,n),wrkmat(n,n)
     &        ,epsilon
 
        integer i,j,k,local_debug
        real*8 eps,b2,Ab2,xxx
        character*132 cbuf
 
c ---------------------------------------------------------------
        local_debug=0
 
c ...............................
c fill wrkmat,x from A,b
 
        if (flag.eq.2) then
           ! set wrkmat=A^2, x=Ab
           ! obviously wrkmat and A, and x and b, need to be separate storage
           ! locations for this flag case.
           do i=1,n
              x(i)=0.d0
              do j=1,n
                 x(i)=x(i)+A(i,j)*b(j)
                 wrkmat(i,j)=0.d0
                 do k=1,n
                    wrkmat(i,j)=wrkmat(i,j)+A(i,k)*A(k,j)
                 enddo
              enddo
           enddo
        else
           ! set wrkmat=A, x=b
           ! except for the final test comparing |Ax-b|/|b|,
           ! wrkmat and A, and x and b, could have been indentical storage locations.
           do i=1,n
              x(i)=b(i)
              do j=1,n
                 wrkmat(i,j)=A(i,j)
              enddo
           enddo
        endif
 
c ...............................
c figure out eps from epsilon and |Ab|/|b|
c eps is used to screen out singular components
 
c calculate scale of A and b
        Ab2=0.d0
        b2=0.d0
        do i=1,n
           b2=b2+x(i)*x(i)
           xxx=0.d0
           do j=1,n
              xxx=xxx+wrkmat(i,j)*x(j)
           enddo
           Ab2=Ab2+xxx*xxx
        enddo
 
c abort if solution trivial (b=0), or no soln possible (A=0)
        if (b2.eq.0.d0 .or. Ab2.eq.0.d0) then
           do i=1,n
              x(i)=0.d0
           enddo
           goto 100
        endif
 
c set absolute eps for iteration, update diagonal for flag=2
        xxx=sqrt(Ab2/b2)
        eps=epsilon*xxx
        if (flag.eq.2) then
           ! vs using xxx=Tr(wrkmat)/dble(n) (average eigenvalue of A^2)
           ! if along small eigenvalue direction, I am presuming current
           ! xxx=sqrt(Ab2/b2) (average eigenvalue component in Ab) better ...
           xxx=eps*epsilon   ! wrkmat = ( A^2 + epsilon^2 * scale(A^2) )
           do i=1,n
              wrkmat(i,i)=wrkmat(i,i)+xxx
           enddo
           ! solve as for unsquared case, but with smaller epsilon?
           ! which version to use ... does it make a difference?
           ! eps=eps
           eps=1.d-4*eps
           ! eps=epsilon*eps
        endif
 
c ...............................
c solve using Gaussian elimination,
c testing the diagonals of A against eps to avoid dividing by zero
 
        do k=1,n-1
           do i=k+1,n
              xxx=wrkmat(k,k)
              if (abs(xxx).gt.eps) then
                 xxx=1.d0/xxx
              elseif (xxx.lt.0.d0.and.flag.ne.2) then
                 xxx=-1.d0/eps
              else
                 xxx=1.d0/eps
              endif
              xxx=xxx*wrkmat(i,k)
              x(i)=x(i)-x(k)*xxx
              do j=k+1,n
                 wrkmat(i,j)=wrkmat(i,j)-wrkmat(k,j)*xxx
              enddo
           enddo
        enddo
 
        do i=n,1,-1
           do j=i+1,n
              x(i)=x(i)-wrkmat(i,j)*x(j)
           enddo
           xxx=wrkmat(i,i)
           if (abs(xxx).gt.eps) then
              xxx=1.d0/xxx
           elseif (xxx.lt.0.d0.and.flag.ne.2) then
              xxx=-1.d0/eps
           else
              xxx=1.d0/eps
           endif
           x(i)=x(i)*xxx
        enddo
 
c ...............................
c test how good a solution and update ierror
c have to redo b2 since for flag=2 it is |Ab| rather than |b|
 
100     ierror=0
 
        b2=0.d0
        Ab2=0.d0
        do i=1,n
           xxx=-b(i)
           do j=1,n
              xxx=xxx+A(i,j)*x(j)
           enddo
           Ab2=Ab2+xxx*xxx
           b2=b2+b(i)*b(i)
        enddo
 
        if (local_debug.ne.0) then
           write(*,'(a,4g20.10)') 'gauss_elim_lg: |Ax-b|,|b|,epsilon='
     &            ,sqrt(Ab2),sqrt(b2),(sqrt(Ab2)/sqrt(b2))/epsilon
     &            ,epsilon
        endif
 
        if (b2.ne.0.d0) then
           ! actually, should test sqrt(Ab2/b2) against epsilon,
           ! but since this is a "hard error flag" (0 vs non-zero), use epsilon instead
           xxx=Ab2/b2
           if (epsilon.ne.0.d0) xxx=xxx/epsilon
           if (xxx.gt.1.d0) then
              if (xxx.gt.10000000.d0) then
                 ierror=1000000
              else
                 ierror=1+int(xxx*0.1d0)
              endif
           endif
        elseif (Ab2.ne.0.d0) then
           ierror=-1
        endif
 
        if (ierror.ne.0.and.local_debug.ne.0) then
           write(cbuf,'(a,3g15.5)')
     &          'gauss_elim_lg warning: (||Ax-b|/|b|)^2 > epsilon;'
     &          //' |Ax-b|,|b|,epsilon=',sqrt(Ab2),sqrt(b2),epsilon
           call writloga('default',0,cbuf,0,i)
        endif
 
c ...............................
        return
        end
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
