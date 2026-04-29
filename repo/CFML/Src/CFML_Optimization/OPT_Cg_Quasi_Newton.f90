 Submodule (CFML_Optimization) OPT_Cg_Quasi_Newton
  implicit none
   contains

    !!----
    !!---- Module Subroutine Cg_Quasi_Newton(Model_Functn,Nparm,X,F,G,C,Ipr)
    !!----    integer,                          intent(in)     :: n      ! The number of variables in the function to be
    !!----                                                               ! minimized.
    !!----    real(kind=cp),dimension(n),       intent(in out) :: x      ! The vector containing the current estimate to
    !!----                                                               ! the minimizer.
    !!----                                                               ! In -> Must contain an initial estimate supplied by the user.
    !!----                                                               ! Out-> X will hold the best estimate to the minimizer obtained
    !!----    real(kind=cp),                    intent(   out) :: f      ! Out-> F will contain the lowest value of the object function obtained.
    !!----    real(kind=cp),dimension(n),       intent(   out) :: g      ! Out-> G =(g(1),...g(n)) will contain the elements of the gradient of
    !!----                                                               !       F evaluated at the point contained in X=(x(1),...x(N))
    !!----    type(Opt_conditions),             intent(in out) :: C      ! Conditions for the algorithm. C is of type(Opt_conditions)
    !!----    integer, optional,                intent(in)     :: ipr   ! Logical unit for printing if the parameter C%IOUT /= 0.
    !!----
    !!--<<    Interface
    !!----       Subroutine Model_Functn(n,x,f,g)
    !!----          use CFML_GlobalDeps, only:cp
    !!----          integer,                    intent(in)     :: n
    !!----          real(kind=cp),dimension(:), intent(in)     :: x
    !!----          real(kind=cp),              intent(out)    :: f
    !!----          real(kind=cp),dimension(:), intent(out)    :: g
    !!----       End Subroutine Model_Functn
    !!-->>    End Interface
    !!----
    !!----    Minimization Of Unconstrained Multivariate Functions
    !!----    Subroutine CG_QUASI_NEWTON minimizes an unconstrained nonlinear
    !!----    scalar valued function of a vector variable X either by the
    !!----    BFGS variable metric algorithm or by a beale restarted conjugate
    !!----    gradient algorithm.(BFGS: Broyden, Fletcher, Goldfarb and Shanno)
    !!----    (Authors: D.F. Shanno and K.H. Phua, The original name of the
    !!----    subroutine was CONMIN)
    !!----    ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE 6 (DECEMBER 1980), 618-622
    !!----
    !!--<<
    !!---- REMARKS:    In addition to the specified values in the above
    !!----             argument list, the user must supply a subroutine
    !!----             "Model_Functn" which calculates the function and gradient at
    !!----             X and places them in F and G(1),...,G(N) respectively.
    !!-->>
    !!--..             An example subroutine for the Rosenbrock function is:
    !!--..
    !!--..             Subroutine Model_Functn(n,x,f,g)
    !!--..                integer,parameter  :: cp=selected_real_kind(14, 300)
    !!--..                integer,                    intent(in) :: n
    !!--..                real(kind=cp),dimension(:), intent(in) :: x
    !!--..                real(kind=cp),              intent(out):: f
    !!--..                real(kind=cp),dimension(:), intent(out):: g
    !!--..                real(kind=cp) :: t1,t2
    !!--..
    !!--..                t1=x(2)-x(1)*x(1)
    !!--..                t2=1.0-x(1)
    !!--..                f=100.0*t1*t1+t2*t2
    !!--..                g(1)=-400.0*t1*x(1)-2.0*t2
    !!--..                g(2)=200.0*t1
    !!--..
    !!--..                return
    !!--..             End Subroutine Model_Functn
    !!--..
    !!--..    Code converted using TO_F90 by Alan Miller
    !!--..    Modified and adapted to F-language by Juan Rodriguez-Carvajal
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Cg_Quasi_Newton(Model_Functn, N, X, F, G, C, Ipr)
       !---- Arguments ----!
       integer,                    intent(in)       :: n
       real(kind=cp),dimension(n), intent(in out)   :: x
       real(kind=cp),              intent(   out)   :: f
       real(kind=cp),dimension(n), intent(   out)   :: g
       type(Opt_conditions_Type),  intent(in out)   :: C
       integer, optional,          intent(in)       :: Ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local variables ----!
       logical       :: rsw
       integer       :: ioutk, nx
       integer       :: ng, nry, nrd, ncons, ncons1, ncons2, nrst, i, ncalls
       integer       :: nxpi, ngpi, nrdpi, nrypi, ij, j, ii, ngpj
       real(kind=cp) :: fp,fmin,alpha,at,ap,gsq,dg,dg1
       real(kind=cp) :: dp,step,dal,u1,u2,u3,u4
       real(kind=cp) :: xsq,rtst

       !---- W is a vector of working storage.
       !---- If C%NMETH=0, W must be dimensioned 5*N+2.
       !---- If C%NMETH=1, W must be dimensioned N*(N+7)/2.
       !---- In both cases, W must be real.
       real(kind=cp),dimension(:),allocatable :: w

       !---- Initialize ITER,IFUN,NFLAG,and IOUTK,which counts output iterations.
       c%iter=0
       c%ifun=0
       ioutk=0
       c%nflag=0

       !---- Set parameters to extract vectors from W.
       !---- W(I) holds the search vector,W(NX+I) holds the best current
       !---- estimate to the minimizer,and W(NG+I) holds the gradient
       !---- at the best current estimate.
       nx=n
       ng=nx+n

       !---- Test which method is being used.
       !---- If C%NMETH=0, W(NRY+I) holds the restart Y vector and
       !---- W(NRD+I) holds the restart SEARCH vector.
       !---- If C%NMETH=1,W(NCONS+I) holds the approximate inverse Hessian.
       if (c%nmeth == 1) then
          ncons=3*n
          if (allocated(w)) deallocate(w)
          allocate(w(n*(n+7)/2))     !W has been removed from the argument
       else
          nry=ng+n
          nrd=nry+n
          ncons=5*n
          ncons1=ncons+1
          ncons2=ncons+2
          if (allocated(w)) deallocate(w)
          allocate(w(5*n+2))
       end if

       !---- Calculate the function and gradient at the initial
       !---- point and initialize NRST,which is used to determine
       !---- whether a beale restart is being done. NRST=N means that this
       !---- iteration is a restart iteration. initialize RSW,which indicates
       !---- that the current search direction is a gradient direction.
       do_20: do
          call Model_Functn(n,x,f,g)
          c%ifun=c%ifun+1
          nrst=n
          rsw=.true.

          !---- Calculate the initial search direction , the norm of X squared,
          !---- and the norm of G squared. DG1 is the current directional
          !---- derivative,while XSQ and GSQ are the squared norms.
          dg1=0.0
          xsq=0.0
          do i=1,n
             w(i)=-g(i)
             xsq=xsq+x(i)*x(i)
             dg1=dg1-g(i)*g(i)
          end do
          gsq=-dg1

          !---- Test if the initial point is the minimizer.
          if (gsq <= c%eps*c%eps*max(1.0_cp,xsq)) return

          !---- Begin the major iteration loop. NCALLS is used to guarantee that
          !---- at least two points have been tried when C%NMETH=0. FMIN is the
          !---- current function value.
          do_40: do
             fmin=f
             ncalls=c%ifun

             !---- If output is desired,test if this is the correct iteration
             !---- and if so, write output.
             if (present(ipr)) then
                if (c%iout  /= 0) then
                   if (ioutk == 0)then
                      write(unit=ipr,fmt="(a,i5,a,i6,2(a,f20.8))")"  Iteration #",c%iter, &
                            "    Num F-eval:",c%ifun,"    F-value = ",fmin,"    G-squared = ",gsq
                   end if
                   ioutk=ioutk+1
                   if (ioutk == c%iout) ioutk=0
                end if
             end if

             !---- Begin linear search. ALPHA is the steplength.
             !---- Set ALPHA to the nonrestart conjugate gradient ALPHA.
             alpha=alpha*dg/dg1

             !---- If C%NMETH=1 or a restart has been performed, set ALPHA=1.0.
             if (nrst == 1 .OR. c%nmeth == 1) alpha=1.0

             !---- If a gradient direction is used, set ALPHA=1.0/DSQRT(GSQ),
             !---- which scales the initial search vector to unity.
             if (rsw) alpha=1.0/sqrt(gsq)

             !---- The linear search fits a cubic to F and DAL, the function
             !---- and its derivative at ALPHA, and to FP and DP,the function
             !---- and derivative at the previous trial point AP.
             !---- Initialize AP ,FP,and DP.
             ap=0.0
             fp=fmin
             dp=dg1

             !---- Save the current derivative to scale the next search vector.
             dg=dg1

             !---- Update the iteration.
             c%iter=c%iter+1

             !---- Calculate the current steplength  and store the current X and G.
             step=0.0
             do i=1,n
                step=step+w(i)*w(i)
                nxpi=nx+i
                ngpi=ng+i
                w(nxpi)=x(i)
                w(ngpi)=g(i)
             end do
             step=SQRT(step)

             !---- Begin the linear search iteration.
             !---- Test for failure of the linear search.
             do_80: do
                if (alpha*step <= c%acc) then
                   !---- Test if direction is a gradient direction.
                   if (.not. rsw) cycle do_20
                   c%nflag=2
                   return
                end if

                !---- Calculate the trial point.
                do i=1,n
                   nxpi=nx+i
                   x(i)=w(nxpi)+alpha*w(i)
                end do

                !---- Evaluate the function at the trial point.
                call Model_Functn(n,x,f,g)

                !---- Test if the maximum number of function calls have been used.
                c%ifun=c%ifun+1
                if (c%ifun > c%mxfun) then
                   c%nflag=1
                   return
                end if

                !---- Compute the derivative of f at alpha.
                dal=0.0
                do i=1,n
                   dal=dal+g(i)*w(i)
                end do

                !---- Test whether the new point has a negative slope but a higher
                !---- function value than ALPHA=0. If this is the case,the search
                !---- has passed through a local maximun and is heading for a distant local
                !---- minimum.
                if (f > fmin .and. dal < 0.0) then
                   !---- A relative maximum has been passed. Reduce ALPHA and restart the search.
                   alpha=alpha/3.0
                   ap=0.0
                   fp=fmin
                   dp=dg
                   cycle do_80
                end if

                !---- If not, test whether the steplength criteria have been met.
                if (.not. (f > fmin+0.0001*alpha*dg .or. abs(dal/dg)  > 0.9)) then
                   !---- If they have been met, test if two points have been tried
                   !---- If NMETH=0 and if the true line minimum has not been found.
                   if(.not.((c%ifun-ncalls) <= 1 .and. abs(dal/dg) > c%eps .and. c%nmeth == 0)) exit do_80
                end if

                !---- A new point must be tried. Use cubic interpolation to find
                !---- the trial point AT.
                u1=dp+dal-3.0*(fp-f)/(ap-alpha)
                u2=u1*u1-dp*dal
                if (u2 < 0.0) u2=0.0
                u2=sqrt(u2)
                at=alpha-(alpha-ap)*(dal+u2-u1)/(dal-dp+2.0*u2)

                !---- Test whether the line minimum has been bracketed.
                if ( dal/dp > 0.0) then
                   !---- The minimum has not been bracketed. Test if both points are
                   !---- greater than the minimum and the trial point is sufficiently
                   !---- smaller than either.
                   if (.not.(dal > 0.0 .and. 0.0 < at .and. at < (0.99*min(ap,alpha)))) then
                      !---- Test if both points are less than the minimum and the trial point
                      !---- is sufficiently large.
                      if (.not.(dal <= 0.0 .and. at > (1.01*max(ap,alpha)))) then
                         !---- If the trial point is too small,double the largest prior point.
                         if (dal <= 0.0) at=2.0*max(ap,alpha)
                         !---- If the trial point is too large, halve the smallest prior point.
                         if (dal > 0.0) at=min(ap,alpha)/2.0
                      end if
                   end if
                else
                   !---- The minimum has been bracketed. Test whether the trial point lies
                   !---- sufficiently within the bracketed interval.
                   !---- If it does not, choose at as the midpoint of the interval.
                   if (at < (1.01*min(alpha,ap)) .or. at > (0.99*max(alpha,ap))) at=(alpha+ap)/2.0
                end if

                !---- Set AP=ALPHA, ALPHA=AT,and continue search.
                ap=alpha
                fp=f
                dp=dal
                alpha=at

             end do do_80

             !---- The line search has converged. Test for convergence of the algorithm.
             gsq=0.0
             xsq=0.0
             do i=1,n
                gsq=gsq+g(i)*g(i)
                xsq=xsq+x(i)*x(i)
             end do
             if (gsq <= c%eps*c%eps*max(1.0_cp,xsq)) return

             !---- Search continues. Set W(I)=ALPHA*W(I),the full step vector.
             w(1:n)=alpha*w(1:n)

             !---- Compute the new search vector. First test whether a
             !---- conjugate gradient or a variable metric vector is used.
             if (c%nmeth /= 1) then
                !---- Conjugate gradient update section.
                !---- Test if a Powell restart is indicated.
                rtst=0.0
                do i=1,n
                   ngpi=ng+i
                   rtst=rtst+g(i)*w(ngpi)
                end do
                if ( abs(rtst/gsq) > 0.2) nrst=n

                !---- If a restart is indicated, save the current D and Y
                !---- as the beale restart vectors and save D'Y and Y'Y
                !----- in W(NCONS+1) and W(NCONS+2).
                if (nrst == n) then
                   w(ncons+1)=0.0
                   w(ncons+2)=0.0
                   do i=1,n
                      nrdpi=nrd+i
                      nrypi=nry+i
                      ngpi=ng+i
                      w(nrypi)=g(i)-w(ngpi)
                      w(nrdpi)=w(i)
                      w(ncons1)=w(ncons1)+w(nrypi)*w(nrypi)
                      w(ncons2)=w(ncons2)+w(i)*w(nrypi)
                   end do
                end if

                !---- Calculate  the restart Hessian times the current gradient.
                u1=0.0
                u2=0.0
                do  i=1,n
                  nrdpi=nrd+i
                  nrypi=nry+i
                  u1=u1-w(nrdpi)*g(i)/w(ncons1)
                  u2=u2+w(nrdpi)*g(i)*2.0/w(ncons2)-w(nrypi)*g(i)/w(ncons1)
                end do
                u3=w(ncons2)/w(ncons1)
                do  i=1,n
                  nxpi=nx+i
                  nrdpi=nrd+i
                  nrypi=nry+i
                  w(nxpi)=-u3*g(i)-u1*w(nrypi)-u2*w(nrdpi)
                end do

                !---- If this is a restart iteration, W(NX+I) contains the new search
                !---- vector.
                if (nrst /= n) then
                   !---- Not a restart iteration. Calculate the restart Hessian
                   !---- times the current Y.
                   u1=0.0
                   u2=0.0
                   u3=0.0
                   u4=0.0
                   do i=1,n
                      ngpi=ng+i
                      nrdpi=nrd+i
                      nrypi=nry+i
                      u1=u1-(g(i)-w(ngpi))*w(nrdpi)/w(ncons1)
                      u2=u2-(g(i)-w(ngpi))*w(nrypi)/w(ncons1)+2.0*w(nrdpi)*(g(i)-w(ngpi))/w(ncons2)
                      u3=u3+w(i)*(g(i)-w(ngpi))
                   end do
                   step=0.0
                   do i=1,n
                      ngpi=ng+i
                      nrdpi=nrd+i
                      nrypi=nry+i
                      step=(w(ncons2)/w(ncons1))*(g(i)-w(ngpi)) +u1*w(nrypi)+u2*w(nrdpi)
                      u4=u4+step*(g(i)-w(ngpi))
                      w(ngpi)=step
                   end do

                   !---- Calculate the doubly updated Hessian times the current
                   !---- gradient to obtain the search vector.
                   u1=0.0
                   u2=0.0
                   do i=1,n
                      u1=u1-w(i)*g(i)/u3
                      ngpi=ng+i
                      u2=u2+(1.0+u4/u3)*w(i)*g(i)/u3-w(ngpi)*g(i)/u3
                   end do
                   do i=1,n
                      ngpi=ng+i
                      nxpi=nx+i
                      w(nxpi)=w(nxpi)-u1*w(ngpi)-u2*w(i)
                   end do
                end if

                !---- Calculate the derivative along the new search vector.
                dg1=0.0
                do i=1,n
                   nxpi=nx+i
                   w(i)=w(nxpi)
                   dg1=dg1+w(i)*g(i)
                end do

                !---- If the new direction is not a descent direction,stop.
                if (dg1 > 0.0) then
                   ! roundoff has produced a bad direction.
                   c%nflag=3
                   return
                end if

                !---- Update nrst to assure at least one restart every n iterations.
                if (nrst == n)nrst=0
                nrst=nrst+1
                rsw=.false.
                cycle do_40
             end if

             !---- A variable metric algoritm is being used. Calculate Y and D'Y.
             u1=0.0
             do i=1,n
                ngpi=ng+i
                w(ngpi)=g(i)-w(ngpi)
                u1=u1+w(i)*w(ngpi)
             end do

             !---- If RSW=.TRUE.,set up the initial scaled approximate Hessian.
             if (rsw) then
                !---- calculate y'y.
                u2=0.0
                do i=1,n
                   ngpi=ng+i
                   u2=u2+w(ngpi)*w(ngpi)
                end do

                !---- Calculate the initial Hessian as H=(P'Y/Y'Y)*I
                !---- and the initial U2=Y'HY and W(NX+I)=HY.
                ij=1
                u3=u1/u2
                do i=1,n
                   do j=i,n
                      ncons1=ncons+ij
                      w(ncons1)=0.0
                      if (i == j) w(ncons1)=u3
                      ij=ij+1
                   end do
                   nxpi=nx+i
                   ngpi=ng+i
                   w(nxpi)=u3*w(ngpi)
                end do
                u2=u3*u2

             else  !RSW
                !---- Calculate W(NX+I)=HY and U2=Y'HY.
                u2=0.0
                do i=1,n
                   u3=0.0
                   ij=i
                   if (i /= 1) then
                      ii=i-1
                      do j=1,ii
                         ngpj=ng+j
                         ncons1=ncons+ij
                         u3=u3+w(ncons1)*w(ngpj)
                         ij=ij+n-j
                      end do
                   end if
                   do j=i,n
                      ncons1=ncons+ij
                      ngpj=ng+j
                      u3=u3+w(ncons1)*w(ngpj)
                      ij=ij+1
                   end do
                   ngpi=ng+i
                   u2=u2+u3*w(ngpi)
                   nxpi=nx+i
                   w(nxpi)=u3
                end do
             end if !RSW

             !---- Calculate the updated approximate Hessian.
             u4=1.0+u2/u1
             do i=1,n
                nxpi=nx+i
                ngpi=ng+i
                w(ngpi)=u4*w(i)-w(nxpi)
             end do
             ij=1
             do i=1,n
                nxpi=nx+i
                u3=w(i)/u1
                u4=w(nxpi)/u1
                do j=i,n
                   ncons1=ncons+ij
                   ngpj=ng+j
                   w(ncons1)=w(ncons1)+u3*w(ngpj)-u4*w(j)
                   ij=ij+1
                end do
             end do

             !---- Calculate the new search direction W(I)=-HG and its derivative.
             dg1=0.0
             do i=1,n
                u3=0.0
                ij=i
                if (i /= 1) then
                   ii=i-1
                   do j=1,ii
                      ncons1=ncons+ij
                      u3=u3-w(ncons1)*g(j)
                      ij=ij+n-j
                   end do
                end if
                do j=i,n
                   ncons1=ncons+ij
                   u3=u3-w(ncons1)*g(j)
                   ij=ij+1
                end do
                dg1=dg1+u3*g(i)
                w(i)=u3
             end do

             !---- Test for a downhill direction.
             if (dg1 > 0.0) then
                !---- Roundoff has produced a bad direction.
                c%nflag=3
                return
             end if
             rsw=.false.
          end do do_40
          exit
       end do do_20

    End Subroutine Cg_Quasi_Newton

 End submodule OPT_Cg_Quasi_Newton