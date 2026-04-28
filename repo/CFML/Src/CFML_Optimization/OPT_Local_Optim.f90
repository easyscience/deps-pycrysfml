Submodule (CFML_Optimization) OPT_Local_Optim
  implicit none
   contains
    !!----
    !!---- Module Subroutine Local_Min_DFP(Model_Functn, N, X, F, C, mini, maxi, Ipr)
    !!----  integer,              intent(in)      :: N       ! The Number Of Parameters
    !!----  real, dimension(:),   intent(in out)  :: X       ! Vector Of Length N Containing Parameter Values
    !!----                                                   ! In -> Must Contain The Initial Parameter Estimates
    !!----                                                   ! Out-> The Final Parameter Estimates As Determined By Local
    !!----  real,                 intent(out   )  :: F       ! The Value Of The Function At The Final Parameter Estimates
    !!----  real, dimension(:),   intent(in)      :: Mmin    ! Lower bounds of the parameters
    !!----  real, dimension(:),   intent(in)      :: Mmax    ! Upper bounds of the parameters
    !!----  type(Opt_conditions), intent(in out)  :: C       ! Conditions for the algorithm. C is of type(Opt_conditions)
    !!----  integer, optional,    intent(in)      :: ipr     ! Logical unit for printing if the parameter C%IOUT /= 0.
    !!----  Interface
    !!----     Subroutine Model_Functn(nparm,x, f)
    !!----        Use CFML_Math_General, only : cp
    !!----        real(kind=cp),dimension(:), intent(in)  :: x
    !!----        real(kind=cp),              intent(out) :: f
    !!----        integer,                    intent(in)  :: nparm
    !!----     End Subroutine Model_Functn
    !!----  End Interface
    !!----
    !!----
    !!----  MINIMUM OF A FUNCTION OF N VARIABLES USING A QUASI-NEWTON METHOD
    !!----  (Davidon-Fletcher-Powel method)
    !!----  This routine is a wrapper for Local_DFP, in order to be used as a
    !!----  standalone routine (independently of Csendes_Global) for minimizing
    !!----  locally a user supplied function, with the calling interface given above,
    !!----  with respect to the free parameters stored in array X and limited to the
    !!----  region [Mini,Maxi]. If there is no stable minimum in the given region the
    !!----  algorithm may fail.
    !!----  The important parameters for the algorithm are stored in the C-variable
    !!----  On input the components C%eps and C%mxfun are needed (a call to
    !!----  Init_Opt_conditions is enough to provide sensible values). On output
    !!----  the component C%ifun is updated
    !!----
    !!---- Update: August - 2007
    !!

    Module Subroutine Local_Min_DFP(Model_Functn, N, X, F, C, mini, maxi, Ipr)
       !---- Arguments ----!
       integer,                     intent(in)      :: n
       real(kind=cp), dimension(:), intent(in out)  :: x
       real(kind=cp),               intent(out)     :: f
       type(Opt_conditions_Type),   intent(in out)  :: C
       real(kind=cp), dimension(:), intent(in)      :: mini
       real(kind=cp), dimension(:), intent(in)      :: maxi
       integer, optional,           intent(in)      :: Ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       real(kind=cp), dimension(N)     :: mmin
       real(kind=cp), dimension(N)     :: mmax
       real(kind=cp), dimension(N)     :: Xn
       integer                         :: nfev, i

       do i = 1, n
          if (mini(i) == maxi(i)) then
             Err_CFML%Ierr=1
             write(unit=Err_CFML%Msg,fmt="(a,i3)")"  ERROR: Minimum=Maximum, for parameter #",i
             if(present(ipr)) write(unit=ipr,fmt="(a)")   Err_CFML%Msg
             return
          end if
          mmax(i) = (maxi(i)-mini(i)) * 0.5
          mmin(i) = mini(i) + maxi(i)
          Xn(i) = (X(i) - mmin(i)) / mmax(i)
       end do
       call Local_DFP(N,C%eps,C%mxfun,Xn,F,Nfev,Mmin,Mmax, Model_Functn)
       c%ifun=Nfev
       X(1:n)=Xn(1:n)*Mmax(1:n) + Mmin(1:n)

    End Subroutine Local_Min_DFP

    !!--++
    !!--++ Module Subroutine Local_DFP (N,Eps,Maxfn,X,F,Nfev,Mmin,Mmax, Model_Functn)
    !!--++  integer,            intent(in)      :: N       ! The Number Of Parameters
    !!--++  real,               intent(in)      :: Eps     ! Convergence Criterion
    !!--++  integer,            intent(in)      :: Maxfn   ! Maximum Number Of Function Evaluations
    !!--++  real, dimension(:), intent(in out)  :: X       ! Vector Of Length N Containing Parameter Values
    !!--++                                                 ! In -> Must Contain The Initial Parameter Estimates
    !!--++                                                 ! Out-> The Final Parameter Estimates As Determined By Local
    !!--++  real,               intent(out   )  :: F       ! The Value Of The Function At The Final Parameter Estimates
    !!--++  integer,            intent(out)     :: Nfev    ! The Effective Number Of Function Evaluations
    !!--++  real, dimension(:), intent(in)      :: Mmin    ! Lower bounds of the parameters
    !!--++  real, dimension(:), intent(in)      :: Mmax    ! Upper bounds of the parameters
    !!--++
    !!--++  Interface
    !!--++     Subroutine Model_Functn(nparm,x, f)
    !!--++        Use CFML_Math_General, only : cp
    !!--++        real(kind=cp),dimension(:), intent(in)  :: x
    !!--++        real(kind=cp),              intent(out) :: f
    !!--++        integer,                    intent(in)  :: nparm
    !!--++     End Subroutine Model_Functn
    !!--++  End Interface
    !!--++
    !!--++
    !!--++  MINIMUM OF A FUNCTION OF N VARIABLES USING A QUASI-NEWTON METHOD
    !!--++  (Davidon-Fletcher-Powel method)
    !!--++
    !!--..  Information
    !!--..
    !!--..  Convergence criterion: this convergence condition is satisfied if
    !!--..  on two successive iterations, the parameter estimates (i.e.,
    !!--..  x(i), i=1,...,n) differs, component by component, by at most eps.
    !!--..
    !!--..  Searching Limits: x(i) is searched in the interval (mmin(i),mmax(i))
    !!--..
    !!--..  The procedure calls the Subroutine FUN to which the argument name of
    !!--..  the user supplied subroutine Model_Functn is passed. This function
    !!--..  calculates the function F for given parameter values x(1),x(2),...,x(n).
    !!--..  The calling sequence has the following form
    !!--..
    !!--..      CALL FUN(X, F, N, MMIN, MMAX,Model_Functn)
    !!--..
    !!--..  Where x is a vector of length n. Model_Functn must have the explicit
    !!--..  interface given above in the calling program. Model_Functn must not
    !!--..  alter the values of x(i),i=1,...,n or n.
    !!--..
    !!--++ Update: August - 2007
    !!

    Module Subroutine Local_DFP(N, Eps, Maxfn, X, F, Nfev, Mmin, Mmax, Model_Functn)
       !---- Arguments ----!
       integer,                     intent(in)      :: n
       real(kind=cp),               intent(in)      :: eps
       integer,                     intent(in)      :: maxfn
       real(kind=cp), dimension(:), intent(in out)  :: x
       real(kind=cp),               intent(out)     :: f
       integer,                     intent(out)     :: nfev
       real(kind=cp), dimension(:), intent(in)      :: mmin
       real(kind=cp), dimension(:), intent(in)      :: mmax

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local Variables ----!
       logical              :: from290=.false.
       integer              :: ig, igg, is, idiff, ir, ij, i, iopt, j, nm1, jj, jp1, l, kj, k,  &
                               link, itn, ii, im1, jnt, np1, jb, nj, ier
       real(kind=cp)        :: hh, hjj, v, df, relx, gs0, diff, aeps, alpha, ff, tot, f1, f2,  &
                               z, gys, dgs, sig, zz, hhh, ghh
       real(kind=cp), dimension(N)       :: g !working arrays
       real(kind=cp), dimension(N*(N+1)) :: h
       real(kind=cp), dimension(3*N)     :: w

       real(kind=cp), parameter      :: reps = 1.1921e-07_cp, zero = 0.0_cp, one = 1.0_cp, half = 0.5_cp, &
                                        seven = 7.0_cp, five = 5.0_cp, twelve = 12.0_cp, p1 = 0.1_cp
       !---- Initialization ----!

       !---- IOPT: OPTIONS SELECTOR.
       !---- = 0 Causes LOCAL_DFP to initialize the hessian matrix H to the identity matrix.
       !---- = 1 Indicates that H has been initialized by the user to a positive definite matrix.
       !---- = 2 Causes LOCAL_DFP to compute the diagonal values of the hessian matrix and set H to
       !----     a diagonal matrix containing these values.
       !---- = 3 Causes LOCAL_DFP to compute an estimate of the hessian in H.
       iopt = 0
       ier = 0
       hh = sqrt(reps)
       ig = n
       igg = n + n
       is = igg
       idiff = 1
       ir = n
       w(1) = -one
       w(2) = zero
       w(3) = zero
       h=zero

       !----  Evaluate function at starting point
       g(1:n) = x(1:n)
       call fun(g, f, n, mmin, mmax,Model_Functn)
       nfev = 1
       do
          if (iopt /= 1) then
             !---- set off-diagonal elements of h to 0.0
             if (n /= 1) then
                ij = 2
                do i = 2, n
                   do j = 2, i
                      h(ij) = zero
                      ij = ij + 1
                   end do
                   ij = ij + 1
                end do

                if (iopt == 0) then
                   !---- set diagonal elements of h to one
                   ij = 0
                   do i = 1, n
                      ij = ij + i
                      h(ij) = one
                   end do
                   exit     !from infinite loop
                end if
             end if

             !---- get diagonal elements of hessian
             im1 = 1
             nm1 = 1
             np1 = n + 1
             do i = 2, np1
                hhh = hh * abs(x(im1))
                g(im1) = x(im1) + hhh
                call fun(g, f2, n, mmin, mmax,Model_Functn)
                g(im1) = g(im1) + hhh
                call fun(g, ff, n, mmin, mmax,Model_Functn)
                h(nm1) = (ff-f2+f-f2) / (hhh*hhh)
                g(im1) = x(im1)
                im1 = i
                nm1 = i + nm1
             end do
             nfev = nfev + n + n

             if (iopt == 3 .and. n /= 1) then
                !---- get the rest of the hessian
                jj = 1
                ii = 2
                do i = 2, n
                   ghh = hh * abs(x(i))
                   g(i) = x(i) + ghh
                   call fun(g, f2, n, mmin, mmax,Model_Functn)
                   do j = 1, jj
                      hhh = hh * abs(x(j))
                      g(j) = x(j) + hhh
                      call fun(g, ff, n, mmin, mmax,Model_Functn)
                      g(i) = x(i)
                      call fun(g, f1, n, mmin, mmax,Model_Functn)
                      h(ii) = (ff-f1-f2+f) / (hhh*ghh)
                      ii = ii + 1
                      g(j) = x(j)
                   end do
                   jj = jj + 1
                   ii = ii + 1
                end do
                nfev = nfev + ((n*n-n)/2)
             end if
          end if

          !---- Factor H to L*D*L-TRANSPOSE
          ir = n
          if (n <= 1) then
             if (h(1) > zero) exit  !exit from infinite loop
             h(1) = zero
             ir = 0
          else
             nm1 = n - 1
             jj = 0
             do j = 1, n
                jp1 = j + 1
                jj = jj + j
                hjj = h(jj)
                if (hjj <= zero) then
                   h(jj) = zero
                   ir = ir - 1
                else
                   if (j /= n) then
                      ij = jj
                      l = 0
                      do i = jp1, n
                         l = l + 1
                         ij = ij + i - 1
                         v = h(ij) / hjj
                         kj = ij
                         do k = i, n
                            h(kj+l) = h(kj+l) - h(kj) * v
                            kj = kj + k
                         end do
                         h(ij) = v
                      end do
                   end if
                end if
             end do
          end if

          if (ir /= n) then
             ier = 129
             return
          end if
          exit
       end do

       itn = 0
       df = -one
       do_120: DO
          !---- Evaluate gradienT W(IG+I),I=1,...,N
          link = 1

          !---- Evaluate gradient
          do_410: DO
             If (idiff /= 2) then
                !---- forward differences
                !---- gradient =    w(ig+i), i=1,...,n
                do i = 1, n
                   z = hh * abs(x(i))
                   zz = x(i)
                   x(i) = zz + z
                   call fun(x, f1, n, mmin, mmax,Model_Functn)
                   w(ig+i) = (f1-f) / z
                   x(i) = zz
                end do
                nfev = nfev + n

             else ! now idiff == 2
                !---- central differences
                !---- gradient =    w(ig+i), i=1,...,n
                do i = 1, n
                   z = hh * abs(x(i))
                   zz = x(i)
                   x(i) = zz + z
                   call fun(x, f1, n, mmin, mmax,Model_Functn)
                   x(i) = zz - z
                   call fun(x, f2, n, mmin, mmax,Model_Functn)
                   w(ig+i) = (f1-f2) / (z+z)
                   x(i) = zz
                end do
                nfev = nfev + n + n
             end if

             if ( link == 2 ) then
                do  ! 1-iteration loop
                    if (nfev < maxfn) then
                       gys = zero
                       do i = 1, n
                          gys = gys + w(ig+i) * w(is+i)
                          w(igg+i) = w(i)
                       end do
                       df = ff - f
                       dgs = gys - gs0
                       if (dgs <= zero) exit ! 1-iteration loop
                       if (dgs+alpha*gs0 <= zero) then
                          !---- update hessian h using complementary dfp formula
                          sig = one / gs0
                          ir = -ir
                          call update(h, n, w, sig, g, ir, 0, zero)
                          do i = 1, n
                             g(i) = w(ig+i) - w(igg+i)
                          end do
                          sig = one / (alpha*dgs)
                          ir = -ir
                          call update(h, n, g, sig, w, ir, 0, zero)
                          exit ! 1-iteration loop
                       end if

                       !---- update hessian using dfp formula
                       zz = alpha / (dgs-alpha*gs0)
                       sig = -zz
                       call update(h, n, w, sig, g, ir, 0, reps)
                       z = dgs * zz - one
                       do i = 1, n
                          g(i) = w(ig+i) + z * w(igg+i)
                       end do
                       sig = one / (zz*dgs*dgs)
                       call update(h, n, g, sig, w, ir, 0, zero)
                    end if
                    exit
                end do !1-iteration loop
             end if

             !---- Begin iteration loop
             if (nfev >= maxfn) exit do_410
             itn = itn + 1
             do i = 1, n
                w(i) = -w(ig+i)
             end do

             !---- Determine search direction W by solving    H*W = -G
             !---- Where H = L*D*L-TRANSPOSE
             if (ir >= n) then
                !---- n .eq. 1
                g(1) = w(1)
                if (n <= 1) then
                   w(1) = w(1) / h(1)
                else
                   !---- n .gt. 1
                   ii = 1
                   !---- solve l*w = -g
                   do i = 2, n
                      ij = ii
                      ii = ii + i
                      v = w(i)
                      im1 = i - 1
                      do j = 1, im1
                         ij = ij + 1
                         v = v - h(ij) * w(j)
                      end do
                      g(i) = v
                      w(i) = v
                   end do
                   !---- solve (d*lt)*z = w where lt = l-transpose
                   w(n) = w(n) / h(ii)
                   jj = ii
                   nm1 = n - 1
                   do nj = 1, nm1
                      !---- j = n-1,n-2,...,1
                      j = n - nj
                      jp1 = j + 1
                      jj = jj - jp1
                      v = w(j) / h(jj)
                      ij = jj
                      do i = jp1, n
                         ij = ij + i - 1
                         v = v - h(ij) * w(i)
                      end do
                      w(j) = v
                   end do
                end if
             end if

             !---- Determine step length ALPHA
             relx = zero
             gs0 = zero
             do i = 1, n
                w(is+i) = w(i)
                diff = abs(w(i)) / abs(x(i))
                relx = max(relx, diff)
                gs0 = gs0 + w(ig+i) * w(i)
             end do
             if (relx == zero) then
                if (idiff /= 2) then
                   !---- change to central differences
                   idiff = 2
                   cycle do_120
                else
                   exit do_410
                end if
             end if
             aeps = eps / relx
             ier = 130

             if (gs0 >= zero) then
                if (idiff /= 2) then
                   !---- change to central differences
                   idiff = 2
                   cycle do_120
                else
                   exit do_410
                end if
             end if
             if (df == zero) then
                if (idiff /= 2) then
                   !---- change to central differences
                   idiff = 2
                   cycle do_120
                else
                   exit do_410
                end if
             end if

             ier = 0
             alpha = (-df-df) / gs0
             if (alpha <= zero) alpha = one
             alpha = min(alpha, one)
             if (idiff == 2) alpha = max(p1,alpha)
             ff = f
             tot = zero
             jnt = 0

             !---- Search along    X + ALPHA*W
             do_200: DO
                if (nfev >= maxfn) exit do_410
                do i = 1, n
                   w(i) = x(i) + alpha * w(is+i)
                end do
                call fun(w, f1, n, mmin, mmax,Model_Functn)
                nfev = nfev + 1
                if (f1 < f) then
                   f2 = f
                   tot = tot + alpha
                   do
                      ier = 0
                      f = f1
                      do i = 1, n
                         x(i) = w(i)
                      end do
                      if (jnt-1 < 0) then
                         if (nfev >= maxfn) exit do_410
                         do i = 1, n
                            w(i) = x(i) + alpha * w(is+i)
                         end do
                         call fun(w, f1, n, mmin, mmax,Model_Functn)
                         nfev = nfev + 1
                         if (f1 >= f) then
                            from290=.true.
                            exit do_200
                         end if
                         if (f1+f2 >= f+f .and. seven*f1+five*f2 > twelve*f) jnt = 2
                         tot = tot + alpha
                         alpha = alpha + alpha
                      else if (jnt-1 == 0) then
                         exit do_200
                      else
                         from290=.true.
                         exit do_200
                      end if
                   end do
                end if

                if (f == ff .and. idiff == 2 .and. relx > eps) ier = 130
                if (alpha < aeps) then
                   if (idiff /= 2) then
                      !---- change to central differences
                      idiff = 2
                      cycle do_120
                   else
                      exit do_410
                   end if
                end if
                if (nfev >= maxfn) exit do_410
                alpha = half * alpha
                do i = 1, n
                   w(i) = x(i) + alpha * w(is+i)
                end do
                call fun(w, f2, n, mmin, mmax,Model_Functn)
                nfev = nfev + 1
                if (f2 < f) then
                   tot = tot + alpha
                   ier = 0
                   f = f2
                   do i = 1, n
                      x(i) = w(i)
                   end do
                else
                   z = p1
                   if (f1+f > f2+f2) z = one + half * (f-f1) / (f+f1-f2-f2)
                   z = max(p1,z)
                   alpha = z * alpha
                   jnt = 1
                   cycle do_200
                end if
                exit
             end do do_200

             if (.not. from290) then
                if (tot < aeps) then
                   if (idiff /= 2) then
                      !---- change to central differences
                      idiff = 2
                      cycle do_120
                   else
                      exit do_410
                   end if
                end if
             end if
             from290=.false.
             alpha = tot

             !---- Save old gradient
             do i = 1, n
                w(i) = w(ig+i)
             end do

             !---- Evaluate gradient W(IG+I), I=1,...,N
             link = 2
          end do do_410

          !---- MAXFN function evaluations
          if (relx > eps .and. ier == 0) cycle do_120
          exit
       end do do_120

       !---- Compute H = L*D*L-TRANSPOSE and output
       if (n == 1) return
       np1 = n + 1
       nm1 = n - 1
       jj = (n*(np1)) / 2
       do jb = 1, nm1
          jp1 = np1 - jb
          jj = jj - jp1
          hjj = h(jj)
          ij = jj
          l = 0
          do i = jp1, n
             l = l + 1
             ij = ij + i - 1
             v = h(ij) * hjj
             kj = ij
             do k = i, n
                h(kj+l) = h(kj+l) + h(kj) * v
                kj = kj + k
             end do
             h(ij) = v
          end do
          hjj = h(jj)
       end do
    End Subroutine Local_DFP


    !!--++
    !!--++ Subroutine Update(A, N, Z, Sig, W, Ir, Mk, Eps)
    !!--++    real,dimension(:), intent(out)      :: A       !
    !!--++    integer,           intent(in)       :: N       !
    !!--++    real,dimension(n), intent(in out)   :: Z       !
    !!--++    real,              intent(in)       :: Sig
    !!--++    real,dimension(n), intent(in out)   :: W
    !!--++    integer,           intent(   out)   :: Ir
    !!--++    integer,           intent(   out)   :: mk
    !!--++    real,              intent(in)       :: eps
    !!--++
    !!--++    Routine called by LOCAL Subroutine
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Update(a, n, z, sig, w, ir, mk, eps)
       !---- Arguments ----!
       real(kind=cp), dimension(:),intent(in out)   :: A       !
       integer,                    intent(in)       :: N       !
       real(kind=cp),dimension(:), intent(in out)   :: Z       !
       real(kind=cp),              intent(in)       :: Sig
       real(kind=cp),dimension(:), intent(in out)   :: W
       integer,                    intent(in out)   :: Ir
       integer,                    intent(in)       :: mk
       real(kind=cp),              intent(in)       :: eps

       !---- Local Variables ----!
       integer                   :: j, jj, ij, jp1, i, ii, mm
       real(kind=cp)             :: ti, v, tim, al, r, b, gm, y
       real(kind=cp), parameter  :: zero = 0.0_cp, one = 1.0_cp, four = 4.0_cp

       !---- UPDATE FACTORS GIVEN IN A  SIG*Z*Z-TRANSPOSE IS ADDED
       !---- FIRST EXECUTABLE STATEMENT
       if (n <= 1) then
          a(1) = a(1) + sig * z(1) * z(1)
          ir = 1
          if (a(1) > zero) return
          a(1) = zero
          ir = 0
       else
          do    !one-iteration loop
             if (sig <= zero) then
                if (sig == zero .or. ir == 0) return
                ti = one / sig
                jj = 0
                if (mk /= 0) then  !   l*w = z on input
                   do j = 1, n
                      jj = jj + j
                      if (a(jj) /= zero) ti = ti + (w(j)*w(j)) / a(jj)
                   end do
                else
                   !---- solve l*w = z
                   w(1:n) = z(1:n)
                   do j = 1, n
                      jj = jj + j
                      v = w(j)
                      if (a(jj) <= zero) then
                         w(j) = zero
                      else
                         ti = ti + (v*v) / a(jj)
                         if (j /= n) then
                            ij = jj
                            jp1 = j + 1
                            do i = jp1, n
                               ij = ij + i - 1
                               w(i) = w(i) - v * a(ij)
                            end do
                         end if
                      end if
                   end do
                end if

                !---- SET    TI, TIM    AND W
                if (ir > 0) then
                   if (ti > zero) then
                      ti = eps / sig
                      if (eps == zero) ir = ir - 1
                   else
                      if (mk-1 <= 0) then
                         mm = 0
                         tim = one / sig
                         exit   !one-iteration loop
                      end if
                   end if
                else
                   ti = zero
                   ir = -ir - 1
                end if

                tim = ti
                ii = jj
                i = n
                do j = 1, n
                   if (a(ii) /= zero) tim = ti - (w(i)*w(i)) / a(ii)
                   w(i) = ti
                   ti = tim
                   ii = ii - i
                   i = i - 1
                end do
                mm = 1
                jj=0
                exit   !one-iteration loop
             else
                mm = 0
                tim = one / sig
                jj = 0
             end if
             exit !one-iteration loop
          end do   !one-iteration loop

          !---- UPDATE A
          do j = 1, n
             jj = jj + j
             ij = jj
             jp1 = j + 1
             !---- update a(j,j)
             v = z(j)
             if (a(jj) <= zero) then
                !----  a(j,j) .eq. zero
                if (ir <= 0 .and. sig >= zero .and. v /= zero) then
                   ir = 1 - ir
                   a(jj) = (v*v) / tim
                   if (j == n) return
                   do i = jp1, n
                      ij = ij + i - 1
                      a(ij) = z(i) / v
                   end do
                   return
                end if
                ti = tim
             else
                !---- a(j,j) .gt. zero
                al = v / a(jj)
                ti = w(j)
                if (mm == 0) ti = tim + v * al
                r = ti / tim
                a(jj) = r * a(jj)
                if (r == zero) exit
                if (j == n) exit

                !---- update remainder of column j
                b = al / ti
                if (r <= four) then
                   do i = jp1, n
                      ij = ij + i - 1
                      z(i) = z(i) - v * a(ij)
                      a(ij) = a(ij) + b * z(i)
                   end do
                else
                   gm = tim / ti
                   do i = jp1, n
                      ij = ij + i - 1
                      y = a(ij)
                      a(ij) = b * z(i) + y * gm
                      z(i) = z(i) - v * y
                   end do
                end if
                tim = ti
             end if
          end do
          if (ir < 0) ir = -ir
       end if
    End Subroutine Update

End Submodule OPT_Local_Optim
