*dk, schmidt_hilbert

        subroutine inv_schmidt_hilbert_lg
     &          (epsilon,MXD1,MXD2,ndim1,ndim2
     &           ,array,ainv,iarb,arb,work)

C #####################################################################
C
C     PURPOSE -
C
C        Invert general array using Schmidt-Hilbert Orthonormalization
C        (can be singular, non-symmetric, non-square array).
C
C        subroutine to apply this inverse to a vector
C        also included below (solv_Mxb_schmidt_hilbert_lg)
C
C     INPUT ARGUMENTS -
C
C        epsilon - Directions which occur in array with
C                      norm / (typical norm) < epsilon
C                  are considered part of the "arbitrary" space
C                  (hence epsilon is a relative accuracy;
C                   see below for "typical norm")
C        MXD1,MXD2,ndim1,ndim2
C                - MAX/actual dimenions of array,ainv
C        array   - the array to invert
C
C     WORKSPACE ARGUMENTS -
C
C        work    - workspace (contents ignored)
C
C     OUTPUT ARGUMENTS -
C
C        ainv - the inverse of array
C        iarb, arb -
C             iarb(k)=0 if the matrix is not singular
C                    >0 if the matrix is singular
C                       and an arbitrary amount of arb(k,i)
C                       can be added to soln(i)
C                       [ arb(k,i) is normalized ]
C                    <0 procedure yield iarb>0,
C                       but test |array*arb| < eps failed 
C                       [ if |arb| non-zero, it is normalized after test ]
C
C     PROGRAMMING NOTES -
C
C        logical sizes for eps are
C           (epsilon)*[ typical sqrt(sum_j array(j,i)^2) ]
C        or (epsilon)*[ typical entry of array ]
C        where epsilon is the desired (input) relative accuracy.
C        The trade off is how "localized eigenvectors" are
C        weighted (already-diagonal entries will need to have
C        a comparable size to the "typical" entry).
C        For now, the first form is used.
C
C        For matricies with both "large" and "small" entries,
C        where the eignevectors for each are desired, a different
C        routine, such as LUD decomposition, is required.
C
C     CHANGE HISTORY -
C
C     $Log: inv_schmidt_hilbert_lg.f,v $
C     Revision 2.00  2007/11/03 00:49:11  spchu
C     Import to CVS
C  
CPVCS    
CPVCS       Rev 1.2   Mon Oct 25 16:30:14 1999   jtg
CPVCS    fixed missing scale in epsilon test in solv routine
CPVCS    
CPVCS       Rev 1.1   Tue Oct 19 14:57:20 1999   jtg
CPVCS    fixed local_debug flag (was non-zero)
CPVCS    
CPVCS       Rev 1.0   Tue Oct 19 13:05:52 1999   jtg
CPVCS    Initial revision.
C
C ######################################################################
        implicit none

        integer ndim1,ndim2,MXD1,MXD2
        real*8 eps,epsilon
        real*8 array(MXD1,ndim2),ainv(MXD2,ndim1)
     &          ,arb(MXD2,ndim2),work(MXD2,ndim1)
        integer iarb(ndim2)

        integer i,j,k,l,local_debug,iway
        real*8 xnorm,dotij,qq,qqq
c ........................................................................
        local_debug=0
10      format(1x,4g13.4)

        if (ndim2.le.0.or.ndim2.gt.MXD2) goto 9999
        if (ndim1.le.0.or.ndim1.gt.MXD1) goto 9999

c calculate eps with units to use in calculation
        eps=0.d0
        iway=1
        if (iway.eq.1) then
           ! compare to "typical" (max) norm
           do i=1,ndim1
             qq=0.d0
             do j=1,ndim2
               qqq=array(i,j)
               qq=qq+qqq*qqq
             enddo
             if (qq.gt.eps) eps=qq
           enddo
        else
           ! compare to "typical" (max) entry
           do i=1,ndim1
             do j=1,ndim2
               qqq=abs(array(i,j))
               if (qqq.gt.eps) eps=qqq
             enddo
           enddo
        endif
        eps=eps*epsilon

c initialize ainv
        do i=1,ndim2
          do j=1,ndim1
            ainv(i,j)=0.d0
          enddo
        enddo

        do i=1,ndim2

c initialize work vector
          do j=1,ndim1
            work(i,j)=array(j,i)
          enddo

c make work vector normal
          xnorm=0.d0
          do j=1,ndim1
            xnorm=xnorm+work(i,j)*work(i,j)
          enddo
          xnorm=sqrt(xnorm)

          if (xnorm.le.eps) then

c no vector: set iarb so don't use
            iarb(i)=1

          else

c initialize arb
            do j=1,ndim1
              work(i,j)=work(i,j)/xnorm
            enddo
            do j=1,ndim2
              arb(i,j)=0.d0
            enddo
            arb(i,i)=1.d0/xnorm

c make ith work vector normal to all below it
            do j=1,i-1
              if (iarb(j).eq.0) then
                dotij=0.d0
                do l=1,ndim1
                  dotij=dotij+work(i,l)*work(j,l)
                enddo
                do l=1,ndim1
                  work(i,l)=work(i,l)-dotij*work(j,l)
                enddo
                do l=1,ndim2
                  arb(i,l)=arb(i,l)-dotij*arb(j,l)
                enddo
              endif
            enddo

c re-normalize ith work vector
            xnorm=0.d0
            do l=1,ndim1
              xnorm=xnorm+work(i,l)*work(i,l)
            enddo
            xnorm=sqrt(xnorm)

            if (xnorm.le.eps) then

c if no new piece of vector: set iarb so don't use
              iarb(i)=2

            else

c update work, arb
              iarb(i)=0
              do l=1,ndim1
                work(i,l)=work(i,l)/xnorm
              enddo
              do l=1,ndim2
                arb(i,l)=arb(i,l)/xnorm
              enddo

c increment ainv
              do k=1,ndim2
                do j=1,ndim1
                  ainv(k,j)=ainv(k,j)+work(i,j)*arb(i,k)
                enddo
              enddo

            endif

          endif

        enddo

c should have ainv at this point, plus allowed arbitrary vectors
c test and normalize allowed arbitrary vectors
c set flag to negative if fail

        do i=1,ndim2

          if (iarb(i).eq.0) then

            ! zero arb in case used accidentally
            do j=1,ndim2
              arb(i,j)=0.d0
            enddo

          elseif (iarb(i).eq.1) then

            ! no vector in this direction: set arb to delta(i,j)
            do j=1,ndim2
              arb(i,j)=0.d0
            enddo
            arb(i,i)=1.d0

          else ! if (iarb(i).eq.2) then

            qq=0.d0
            do j=1,ndim2
              qq=qq+arb(i,j)*arb(i,j)
            enddo
            qq=sqrt(qq)
            if (qq.le.eps) then
              if (local_debug.gt.0)
     &          write(*,*) '*** iarb.ne.0, |arb|=0, not using i=',i 
              iarb(i)=-1
              ! normalize if possible
              if (qq.ne.0.d0) then
                 qq=1.d0/(qq)
                 do j=1,ndim2
                   arb(i,j)=arb(i,j)*qq
                 enddo
              endif
            else
              qq=1.d0/(qq)
              do j=1,ndim2
                arb(i,j)=arb(i,j)*qq
              enddo
              qqq=0.d0
              do k=1,ndim1
                qq=0.d0
                do j=1,ndim2
                  qq=qq+array(k,j)*arb(i,j)
                enddo
                qqq=qqq+qq*qq
              enddo
              qqq=sqrt(qqq)
              if (qqq.gt.eps) then
                if (local_debug.gt.0) write(*,*)
     &            '*** iarb.ne.0, |M(arb)|.ne.0, not using i=',i
                iarb(i)=-1
              endif
            endif
            if (local_debug.gt.0) then
              write(*,*) 'zero eigenvalue vector ',i*iarb(i)
              write(*,10) (arb(i,j),j=1,ndim2)
            endif

          endif

        enddo

c count indep vectors and print ainv
        if (local_debug.gt.0) then
          j=0
          do i=1,ndim2
            if (iarb(i).eq.0) j=j+1
          enddo
          write(*,*) 'a^T has',j,' independent vectors'
          write(*,*)'ainv'
          do i=1,ndim2
            write(*,10)(ainv(i,j),j=1,ndim1)
          enddo
        endif

        ! sucessful return
1000    return

        ! error return
9999    continue
        if (local_debug.gt.1) stop 'schmidt_hilbert_inv_lg error'
        call writloga('default',0,
     &  'Warning: schmidt_hilbert_inv_lg: array dimensions not allowed'
     &  ,0,i)
        return

        end

c ===========================================
        subroutine solv_Mxb_schmidt_hilbert_lg
     &          (epsilon,MXD1,MXD2,ndim1,ndim2
     &           ,array,vec,sol,ainv,iarb,arb,work)

C #####################################################################
C
C     PURPOSE -
C
C        solve general Mx=b using Schmidt-Hilbert Orthonormalization
C        (M can be singular, non-symmetric, non-square array).
C
C     ARGUMENTS same as for inv_schmidt_hilbert_lg -
C
C        epsilon, MXD1,MXD2,ndim1,ndim2
C        array, work, ainv, iarb, arb
C
C     ADDITIONAL ARGUMENTS -
C
C        vec - the (input) b vector in MX=b
C        sol - the (output) x solution vector to MX=b
C
C     PROGRAMMING NOTES -
C
C        after calling inv_schmidt_hilbert_lg, the solution
C        is optimized (minimal magnitude, best fit)
C        wrt the arbitrary vectors. This is probably not necessary.
C
C     CHANGE HISTORY -
C
C        $Log: inv_schmidt_hilbert_lg.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
C
C ######################################################################
        implicit none

        integer ndim1,ndim2,MXD1,MXD2
        real*8 epsilon
        real*8 array(MXD1,ndim2),ainv(MXD2,ndim1)
     &          ,arb(MXD2,ndim2),work(MXD2,ndim1)
     &          ,vec(ndim1),sol(ndim2)
        integer iarb(ndim2)

        integer i,j,local_debug
        real*8 qq,qqq,xerr,q1,q2,scale

c ........................................................................

        local_debug=0
10      format(1x,4g13.4)

        call inv_schmidt_hilbert_lg
     &       (epsilon,MXD1,MXD2,ndim1,ndim2
     &       ,array,ainv,iarb,arb,work)

30      do i=1,ndim2
          sol(i)=0.d0
          do j=1,ndim1
            sol(i)=sol(i)+ainv(i,j)*vec(j)
          enddo
        enddo

        if (local_debug.gt.0) then
           write(*,*) (sol(i),i=1,ndim2)
        endif

        ! minimize magnitude wrt (normalized) arbitrary components
        ! since arb normalized, don't need epsilon test?
        do i=1,ndim2
          if (iarb(i).gt.0) then
            qqq=0.d0
            do j=1,ndim2
              qqq=qqq+arb(i,j)*sol(j)
            enddo
            do j=1,ndim2
              sol(j)=sol(j)-arb(i,j)*qqq
            enddo
          endif
        enddo

        ! test that sol - if not, use best fit
        ! find scale for vec
        scale=0.d0
        do i=1,ndim1
           scale=scale+vec(i)*vec(i)
        enddo

        ! test that sol - if not, use best fit
        xerr=0.d0
        q1=0.d0
        q2=0.d0
        do i=1,ndim1
          work(1,i)=0.d0
          do j=1,ndim2
            work(1,i)=work(1,i)+array(i,j)*sol(j)
          enddo
          qq=work(1,i)-vec(i)
          xerr=xerr+qq*qq
          q1=q1+work(1,i)*vec(i)
          q2=q2+work(1,i)*work(1,i)
        enddo
        if (xerr.gt.epsilon*scale) then
           if (local_debug.gt.0)
     &       write(*,*) '*** NO sol WITH THIS vec ***'
           ! scale to best fit magnitude
           if (q2.gt.epsilon*scale) then
             q1=q1/q2
             do i=1,ndim2
               sol(i)=sol(i)*q1
             enddo
           else
             do i=1,ndim2
               sol(i)=0.d0
             enddo
           endif
           xerr=0.d0
           do i=1,ndim1
             qq=0.d0
             do j=1,ndim2
               qq=qq+array(i,j)*sol(j)
             enddo
             qq=qq-vec(i)
             xerr=xerr+qq*qq
           enddo
        endif

        if (local_debug.gt.0) then
           write(*,*)'(sol(i),i=1,',ndim2,')'
           write(*,10)(sol(i),i=1,ndim2)
           write(*,*)'xerr=',xerr
        endif

40      continue

        return

        end

c ===========================================
