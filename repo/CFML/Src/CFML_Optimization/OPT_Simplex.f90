 Submodule (CFML_Optimization) OPT_Simplex
  implicit none
  contains
    !!----
    !!---- Module Subroutine Nelder_Mead_Simplex(Model_Functn, Npar, P, Step, Var, Func, C, Ipr)
    !!----    integer,                      intent(in)      :: nop   ! In -> no. of parameters, incl. any to be held fixed
    !!----    real (kind=cp), dimension(:), intent(in out)  :: p     ! In -> starting values of parameters
    !!----                                                           ! Out-> final values of parameters
    !!----    real (kind=cp), dimension(:), intent(in out)  :: step  ! In -> initial step sizes
    !!----    real (kind=cp), dimension(:), intent(out)     :: var   ! Out-> contains the diagonal elements of the inverse of
    !!----                                                           !       the information matrix.
    !!----    real (kind=cp),               intent(out)     :: func  ! Out-> the function value corresponding to the final
    !!----                                                           !       parameter values.
    !!----    type(opt_conditions),         intent(in out)  :: c     ! Optimization Conditions
    !!----    integer, optional,            intent(in)      :: Ipr   ! Logical unit for printing if the parameter C%IOUT /= 0.
    !!----
    !!--<<    Interface
    !!----       Subroutine Model_Functn(n,p, func)                  ! name of the user's subroutine - arguments (P,FUNC)
    !!----          use CFML_Math_General, only: cp                  ! which returns the function value for a given set of
    !!----          integer,                      intent(in)  :: n   ! Number of parameters
    !!----          real (kind=cp), dimension(:), intent(in)  :: p   ! parameter values in array P.
    !!----          real (kind=cp),               intent(out) :: func
    !!----          real (kind=cp), dimension(:),optional, intent(out):: g
    !!----       End Subroutine Model_Functn
    !!-->>    End Interface
    !!----
    !!----    A subroutine for function minimization using the SIMPLEX method.
    !!----
    !!--<<
    !!----    Optimization Conditions type with the following components:
    !!----       C%MXFUN = Input, the maximum no. of function evaluations allowed.
    !!----                   Say, 20 times the number of parameters, NPAR.
    !!----       C%IOUT  = Input, print control parameter
    !!----                   < 0 No printing
    !!----                   = 0 Printing of parameter values and the function
    !!----                       value after initial evidence of convergence.
    !!----                   > 0 As for C%IOUT = 0 plus progress reports after every
    !!----                       C%IOUT evaluations, plus printing for the initial simplex.
    !!----       C%EPS   = Input, stopping criterion.
    !!----                 The criterion is applied to the standard deviation of
    !!----                 the values of FUNC at the points of the simplex.
    !!----       C%Loops = Input, the stopping rule is applied after every NLOOP
    !!----                 function evaluations.   Normally NLOOP should be slightly
    !!----                 greater than NPAR, say NLOOP = 2*NPAR.
    !!----       C%Iquad = Input, = 1 If fitting of a quadratic surface is required
    !!----                        = 0 If not
    !!----                 N.B. The fitting of a quadratic surface is strongly
    !!----                 recommended, provided that the fitted function is
    !!----                 continuous in the vicinity of the minimum.   It is often
    !!----                 a good indicator of whether a premature termination of
    !!----                 the search has occurred.
    !!----       C%ACC   = Input, criterion for expanding the simplex to overcome
    !!----                 rounding errors before fitting the quadratic surface.
    !!----                 The simplex is expanded so that the function values at
    !!----                 the points of the simplex exceed those at the supposed
    !!----                 minimum by at least an amount SIMP.
    !!----
    !!----       C%NFLAG = Output, = 0 for successful termination
    !!----                   = 1 If maximum no. of function evaluations exceeded
    !!----                   = 2 If information matrix is not +ve semi-definite
    !!----                   = 3 If NPAR < 1
    !!----                   = 4 If C%Loops < 1
    !!----
    !!----       N.B. P, STEP and VAR (If C%Iquad = 1) must have dimension at least NPAR
    !!----            in the calling program.
    !!-->>
    !!--..    For details, see Nelder & Mead, The Computer JournaL, January 1965
    !!--..    Programmed by D.E.Shaw,
    !!--..    CSIRO, Division of Mathematics & Statistics
    !!--..    P.O. BOX 218, Lindfield, N.S.W. 2070
    !!--..
    !!--..    With amendments by R.W.M.WEDDERBURN
    !!--..    Rothamsted Experimental Station
    !!--..    Harpenden, Hertfordshire, ENGLAND
    !!--..
    !!--..    Further amended by Alan Miller
    !!--..    CSIRO Division of Mathematical & Information Sciences
    !!--..    Private Bag 10, CLAYTON, VIC. 3169
    !!--..
    !!--..    Fortran 90 conversion by Alan Miller, June 1995
    !!--..    Alan.Miller @ vic.cmis.csiro.au
    !!--..    Latest revision - 5 December 1999
    !!--..
    !!--..    Conversion to F-language and more modifications
    !!--..    by Juan Rodriguez-Carvajal (LLB-CEA)- 7 April 2004
    !!----
    !!---- Update: February - 2005, June 2020
    !!
    Module Subroutine Nelder_Mead_Simplex(Model_Functn, Nop, P, Step, Var, Func, C, Ipr)
       !---- Arguments ----!
       integer,                      intent(in)      :: nop
       real(kind=cp), dimension(:),  intent(in out)  :: p, step
       real(kind=cp), dimension(:),  intent(out)     :: var
       real(kind=cp),                intent(out)     :: func
       type(opt_conditions_Type),    intent(in out)  :: c
       integer, optional,            intent(in)      :: Ipr

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
       real(kind=cp), dimension(nop+1,nop)     :: g
       real(kind=cp), dimension(nop+1)         :: h
       real(kind=cp), dimension(nop)           :: pbar, pstar, pstst,aval, pmin
       real(kind=cp), dimension(nop*(nop+1)/2) :: bmat, vc

       real(kind=cp)                           :: ymin, rmax, hstst, a0, hmin, test, hmean, &
                                                  hstd, hstar, hmax, savemn

       real(kind=cp), parameter                :: zero = 0.0_cp, half = 0.5_cp, one = 1.0_cp, two = 2.0_cp

       integer                                 :: i, i1, i2, iflag, ii, ij, imax, imin, irank, irow, j, j1, jj, &
                                                  k, l, loop, nap, neval, nmore, np1, nullty

       !---- A = Reflection coefficient, B = Contraction coefficient, and
       !---- C = Expansion coefficient.
       real(kind=cp), parameter :: a = 1.0_cp, b = 0.5_cp, cc = 2.0_cp

       !---- If progress reports have been requested, print heading
       if (c%iout > 0) then
          if (present(ipr)) then
             write(unit=ipr,fmt="(a,i4,a,/,a)") " Progress Report every",c%iout, &
                  " function evaluations"," EVAL.   FUNC.VALUE.          PARAMETER VALUES"
          end if
       end if

       !---- Check input arguments
       c%nflag = 0
       if (nop <= 0) c%nflag = 3
       if (c%loops <= 0) c%nflag = 4
       if (c%nflag /= 0) return

       !---- SET NAP = NO. OF PARAMETERS TO BE VARIED, I.E. WITH STEP /= 0
       nap = count(step /= zero)
       neval = 0
       loop  = 0
       iflag = 0

       !---- IF NAP = 0 EVALUATE FUNCTION AT THE STARTING POINT AND RETURN
       if (nap <= 0) then
          call Model_Functn(Nop,p,func)
          return
       end if

       !---- SET UP THE INITIAL SIMPLEX
       do          ! main infinite loop setting the initial simplex
          g(1,:) = p
          irow = 2
          do i = 1, nop
             if (step(i) /= zero) then
                g(irow,:) = p
                g(irow,i) = p(i) + step(i)
                irow = irow + 1
             end if
          end do

          np1 = nap + 1
          do i = 1, np1
             p = g(i,:)
             call Model_Functn(Nop,p,h(i))
             neval = neval + 1
             if (c%iout > 0 .and. present(ipr) ) then
                write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") " ",neval, "  ", h(i),"  ", p
             end if
          end do

          !---- START OF MAIN CYCLE.
          !---- FIND MAX. & MIN. VALUES FOR CURRENT SIMPLEX (HMAX & HMIN).
          Main_loop: do
             loop = loop + 1
             imax = 1
             imin = 1
             hmax = h(1)
             hmin = h(1)
             do i = 2, np1
                if (h(i) > hmax) then
                   imax = i
                   hmax = h(i)
                else
                   if (h(i) < hmin) then
                      imin = i
                      hmin = h(i)
                   end if
                end if
             end do

             !---- FIND THE CENTROID OF THE VERTICES OTHER THAN P(IMAX)
             pbar = zero
             do i = 1, np1
                if (i /= imax) then
                   pbar = pbar + g(i,:)
                end if
             end do
             pbar = pbar / nap

             !---- REFLECT MAXIMUM THROUGH PBAR TO PSTAR,
             !---- HSTAR = FUNCTION VALUE AT PSTAR.
             pstar = a * (pbar - g(imax,:)) + pbar
             call Model_Functn(Nop,pstar,hstar)

             neval = neval + 1
             if (c%iout > 0 .and. present(ipr)) then
                if (modulo(neval,c%iout) == 0) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                   " ",neval, "  ", hstar,"  ", pstar
             end if

             !---- IF HSTAR < HMIN, REFLECT PBAR THROUGH PSTAR,
             !---- HSTST = FUNCTION VALUE AT PSTST.
             if (hstar < hmin) then
                pstst = cc * (pstar - pbar) + pbar
                call Model_Functn(Nop,pstst,hstst)
                neval = neval + 1
                if (c%iout > 0 .and. present(ipr)) then
                   if (modulo(neval,c%iout) == 0) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                      " ",neval, "  ", hstst, "  ", pstst
                end if

                !---- IF HSTST < HMIN REPLACE CURRENT MAXIMUM POINT BY PSTST AND
                !---- HMAX BY HSTST, THEN TEST FOR CONVERGENCE.
                if (hstst >= hmin) then   ! replace maximum point by pstar & h(imax) by hstar.
                   g(imax,:) = pstar
                   h(imax) = hstar
                else
                   g(imax,:) = pstst
                   h(imax) = hstst
                end if

             else   ! (hstar < hmin)
                !---- HSTAR is not < HMIN.
                !---- Test whether it is < function value at some point other than
                !---- P(IMAX).   If it is replace P(IMAX) by PSTAR & HMAX by HSTAR.
                do_250: do
                   do i = 1, np1
                      if (i /= imax) then
                         if (hstar < h(i)) then  ! replace maximum point by pstar & h(imax) by hstar.
                            g(imax,:) = pstar
                            h(imax) = hstar
                            exit do_250
                         end if
                      end if
                   end do

                   !---- HSTAR > all function values except possibly HMAX.
                   !---- If HSTAR <= HMAX, replace P(IMAX) by PSTAR & HMAX by HSTAR.
                   if (hstar <= hmax) then
                      g(imax,:) = pstar
                      hmax = hstar
                      h(imax) = hstar
                   end if

                   !---- Contracted STEP to the point PSTST,
                   !---- HSTST = Function value at PSTST.
                   pstst = b * g(imax,:) + (one-b) * pbar
                   call Model_Functn(Nop,pstst,hstst)
                   neval = neval + 1
                   if (c%iout > 0 .and. present(ipr)) then
                      if (modulo(neval,c%iout) == 0) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                         " ",neval, "  ", hstst, "  ", pstst
                   end if

                   !---- IF HSTST < HMAX replace P(IMAX) by PSTST & HMAX by HSTST.
                   if (hstst <= hmax) then
                      g(imax,:) = pstst
                      h(imax) = hstst
                   else     !(hstst <= hmax)
                      !---- HSTST > HMAX.
                      !---- Shrink the simplex by replacing each point, other than the current
                      !---- minimum, by a point mid-way between its current position and the
                      !---- minimum.
                      do i = 1, np1
                         if (i /= imin) then
                            do j = 1, nop
                               if (step(j) /= zero) g(i,j) = (g(i,j) + g(imin,j)) * half
                               p(j) = g(i,j)
                            end do
                            call Model_Functn(Nop,p,h(i))
                            neval = neval + 1
                            if (c%iout > 0 .and. present(ipr)) then
                               if (modulo(neval,c%iout) == 0) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                                  " ",neval, "  ", h(i), "  ", p
                            end if
                         end if
                      end do
                   end if   !(hstst <= hmax)
                   exit
                end do do_250
             end if  ! (hstar < hmin)

             !---- If LOOP = NLOOP test for convergence, otherwise repeat main cycle.
             if (loop < c%loops) cycle main_loop

             !---- Calculate mean & standard deviation of function values for the
             !---- current simplex.
             hmean = SUM( h(1:np1) ) / np1
             hstd  = SUM( (h(1:np1) - hmean) ** 2 )
             hstd  = SQRT(hstd / np1)

             !---- If the RMS > STOPCR, set IFLAG & LOOP to zero and go to the
             !---- start of the main cycle again.
             if (hstd > c%eps .and. neval <= c%mxfun) then
                iflag = 0
                loop = 0
                cycle main_loop
             end if

             !---- Find the centroid of the current simplex and the function value there.
             do i = 1, nop
                if (step(i) /= zero) then
                   p(i) = sum( g(1:np1,i) ) / np1
                end if
             end do
             call Model_Functn(Nop,p,func)
             neval = neval + 1
             if (c%iout > 0 .and. present(ipr)) then
                if (modulo(neval,c%iout) == 0) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                   " ",neval, "  ", func, "  ", p
             end if

             !---- Test whether the no. of function values allowed, c%mxfun, has been
             !---- overrun; if so, exit with C%NFLAG = 1.
             if (neval > c%mxfun) then
                c%nflag = 1
                if (c%iout < 0) return
                if (present(ipr)) then
                   Write(unit=ipr,fmt="(a,i5)") "  No. of function evaluations > ",c%mxfun
                   Write(unit=ipr,fmt="(a, f14.6)") "  RMS of function values of last simplex =",hstd
                   Write(unit=ipr,fmt="(a,4(/,6f13.5))") "  Centroid of last simplex =",p
                   Write(unit=ipr,fmt="(a, f14.6)") "  Function value at centroid =",func
                end if
                return
             end if

             !---- Convergence criterion satisfied.
             !---- If IFLAG = 0, set IFLAG & save HMEAN.
             !---- If IFLAG = 1 & change in HMEAN <= STOPCR then search is complete.
             if (c%iout >= 0 .and. present(ipr)) then
                Write(unit=ipr,fmt="(/,a)") "  EVIDENCE OF CONVERGENCE"
                Write(unit=ipr,fmt="(a,4(/,6f13.5))") "  Centroid of last simplex =",p
                Write(unit=ipr,fmt="(a, f14.6)") "  Function value at centroid =",func
             end if

             if (iflag == 0 .or. abs(savemn-hmean) >= c%eps) then
                iflag = 1
                savemn = hmean
                loop = 0
             else
                exit main_loop
             end if
          end do main_loop

          if (c%iout >= 0 .and. present(ipr)) then
             Write(unit=ipr,fmt="(/,a,i5,a)") "  Minimum found after ",neval," function evaluations"
             Write(unit=ipr,fmt="(a,4(/,6f13.6))") "  Minimum at",p
             Write(unit=ipr,fmt="(a, f14.6)") "  Function value at minimum =",func
          end if
          if (c%iquad <= 0) return

          !----
          !---- Quadratic surface fitting
          !----
          if (c%iout >= 0 .and. present(ipr)) &
             Write(unit=ipr,fmt="(/,a,/)")  "  Fitting quadratic surface about supposed minimum"

          !---- Expand the final simplex, if necessary, to overcome rounding errors.
          hmin = func
          nmore = 0
          do i = 1, np1
             do
                test = abs(h(i)-func)
                if (test < c%acc) then
                   do j = 1, nop
                      if (step(j) /= zero) g(i,j) = (g(i,j)-p(j)) + g(i,j)
                      pstst(j) = g(i,j)
                   end do
                   call Model_Functn(Nop,pstst,h(i))
                   nmore = nmore + 1
                   neval = neval + 1
                   if (h(i) >= hmin) cycle
                   hmin = h(i)
                   if (c%iout >= 0 .and. present(ipr)) write(unit=ipr,fmt="(a,i4,a,f12.5,a, 5f11.4, 3(/,t22, 5f11.4))") &
                                                                            " ",neval, "  ", hmin, "  ", pstst
                else
                   exit
                end if
             end do
          end do

          !---- Function values are calculated at an additional NAP points.
          do i = 1, nap
             i1 = i + 1
             pstar = (g(1,:) + g(i1,:)) * half
             call Model_Functn(Nop,pstar,aval(i))
             nmore = nmore + 1
             neval = neval + 1
          end do

          !---- The matrix of estimated second derivatives is calculated and its
          !---- lower triangle stored in BMAT.
          a0 = h(1)
          do i = 1, nap
             i1 = i - 1
             i2 = i + 1
             do j = 1, i1
                j1 = j + 1
                pstst = (g(i2,:) + g(j1,:)) * half
                call Model_Functn(Nop,pstst,hstst)
                nmore = nmore + 1
                neval = neval + 1
                l = i * (i-1) / 2 + j
                bmat(l) = two * (hstst + a0 - aval(i) - aval(j))
             end do
          end do

          l = 0
          do i = 1, nap
             i1 = i + 1
             l = l + i
             bmat(l) = two * (h(i1) + a0 - two*aval(i))
          end do

          !---- The vector of estimated first derivatives is calculated and
          !---- stored in aval.
          do i = 1, nap
             i1 = i + 1
             aval(i) = two * aval(i) - (h(i1) + 3.0_cp*a0) * half
          end do

          !---- The matrix Q of Nelder & Mead is calculated and stored in G.
          pmin = g(1,:)
          do i = 1, nap
             i1 = i + 1
             g(i1,:) = g(i1,:) - g(1,:)
          end do

          do i = 1, nap
             i1 = i + 1
             g(i,:) = g(i1,:)
          end do

          !---- invert bmat
          call syminv(bmat, nap, bmat, nullty, c%nflag, rmax)
          if (c%nflag == 0) then
             irank = nap - nullty
             exit !quit the infinite loop
          else                                 ! BMAT not +ve definite
                                               ! Resume search for the minimum
             if (c%iout >= 0 .and. present(ipr)) write(unit=ipr,fmt="(/,a,/,a,/)")    &
                "  Matrix of estimated second derivatives not +VE definite!", &
                "  Minimum probably not found"
             c%nflag = 2
             if (neval > c%mxfun) return
             if (present(ipr)) write(unit=ipr,fmt="(/,t11,a,/)")   "Search restarting"
             step = half * step
             cycle    !
          end if
       end do ! Main infinite loop setting the initial simplex

       !---- BMAT*A/2 IS CALCULATED AND STORED IN H.
       do i = 1, nap
          h(i) = zero
          do j = 1, nap
             if (j <= i) then
                l = i * (i-1) / 2 + j
             else
                l = j * (j-1) / 2 + i
             end if
             h(i) = h(i) + bmat(l) * aval(j)
          end do
       end do

       !---- Find the position, PMIN, & value, YMIN, of the minimum of the
       !---- quadratic.
       ymin = dot_product( h(1:nap), aval(1:nap) )
       ymin = a0 - ymin
       do i = 1, nop
          pstst(i) = dot_product( h(1:nap), g(1:nap,i) )
       end do
       pmin = pmin - pstst
       if (c%iout >= 0 .and. present(ipr)) then
          write(unit=ipr,fmt="(a,f14.6,a,4(/,6f13.5))") "  Minimum of quadratic surface =",ymin," at", pmin
          write(unit=ipr,fmt="(a,/,a,/)")                                        &
               "  If this differs by much from the minimum estimated from the minimization,",          &
               "  The minimum may be false &/or the information matrix may be inaccurate"
       end if

       !---- Q*BMAT*Q'/2 is calculated & its lower triangle stored in VC
       do i = 1, nop
          do j = 1, nap
             h(j) = zero
             do k = 1, nap
                if (k <= j) then
                   l = j * (j-1) / 2 + k
                else
                   l = k * (k-1) / 2 + j
                end if
                h(j) = h(j) + bmat(l) * g(k,i) * half
             end do
          end do

          do j = i, nop
             l = j * (j-1) / 2 + i
             vc(l) = dot_product( h(1:nap), g(1:nap,j) )
          end do
       end do

       !---- The diagonal elements of VC are copied into VAR.
       j = 0
       do i = 1, nop
          j = j + i
          var(i) = vc(j)
       end do
       if (c%iout < 0) return
       if (present(ipr)) then
          write(unit=ipr,fmt="(a,i3,/,a)") "  Rank of information matrix =",irank, &
                                            "  Inverse of information matrix:-"
          call print_tri_matrix(vc, nop, ipr)

          write(unit=ipr,fmt="(5(/,a),/)")                           &
               "  If the function minimized was -LOG(LIKELIHOOD),"   ,&
               "  this is the covariance matrix of the parameters."  ,&
               "  If the function was a sum of squares of residuals,",&
               "  this matrix must be multiplied by twice the estimated residual variance", &
               "  to obtain the covariance matrix."
       end if
       call syminv(vc, nap, bmat, nullty, c%nflag, rmax)

       !---- BMAT NOW CONTAINS THE INFORMATION MATRIX
       if (present(ipr)) then
          write(unit=ipr,fmt="(a,/)")    " INFORMATION MATRIX:-"
          call print_tri_matrix(bmat, nop, ipr)
       end if
       ii = 0
       ij = 0
       do i = 1, nop
          ii = ii + i
          if (vc(ii) > zero) then
             vc(ii) = one / sqrt(vc(ii))
          else
             vc(ii) = zero
          end if
          jj = 0
          do j = 1, i - 1
             jj = jj + j
             ij = ij + 1
             vc(ij) = vc(ij) * vc(ii) * vc(jj)
          end do
          ij = ij + 1
       end do

       if (present(ipr)) then
          write(unit=ipr,fmt="(/,a)")   " CORRELATION MATRIX:-"
          ii = 0
          do i = 1, nop
             ii = ii + i
             if (vc(ii) /= zero) vc(ii) = one
          end do
          call print_tri_matrix(vc, nop, ipr)

          !---- Exit, on successful termination.
          write(unit=ipr,fmt="(/,a,i4,a,/)") " A further", nmore, &
                                               " function evaluations have been used"
       end if

    End Subroutine Nelder_Mead_Simplex

    !!--++
    !!--++ Subroutine Print_Tri_Matrix(A, N, Iunit)
    !!--++    real (kind=cp),dimension(:), intent(in)   :: A       ! Matrix
    !!--++    integer,                     intent(in)   :: N       ! The order of A
    !!--++    integer,                     intent(in)   :: Iunit   ! Output unit
    !!--++
    !!--++    (Private)
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Print_Tri_Matrix(A, N, Iunit)
       !---- Arguments ----!
       real(kind=cp),dimension(:),  intent(in)  :: a
       integer,                     intent(in)  :: n
       integer,                     intent(in)  :: iunit

       !---- Local variables ----!
       integer  :: i, ii, i1, i2, l

       l = 1
       do l = 1, n, 6
          ii = l * (l-1) / 2
          do i = l, n
             i1 = ii + l
             ii = ii + i
             i2 = min(ii,i1+5)
             write(unit=iunit,fmt="(tr1,6f13.5)") a(i1:i2)
          end do
       end do

    End Subroutine Print_Tri_Matrix

    !!--++
    !!--++ Subroutine Syminv(A, N, C, Nullty, Ifault, Rmax)
    !!--++   real(kind=cp),dimension(:), intent(in)    :: A       ! the symmetric matrix to be inverted, stored in lower triangular form
    !!--++   integer,                    intent(in)    :: N       ! The order of A
    !!--++   real(kind=cp),dimension(:), intent(out)   :: C       ! the inverse of A (A generalized inverse if C is singular), also stored in lower triangular form.
    !!--++   integer,                    intent(out)   :: Nullty  ! the rank deficiency of A.
    !!--++   integer,                    intent(out)   :: Ifault  ! Error Code: 1 if N < 1, 2 If A Is not +ve semi-definite
    !!--++                                                             !             0 Otherwise
    !!--++   real (kind=cp),             intent(out)   :: Rmax        ! an estimate of the relative accuracy of the diagonal elements of C.
    !!--++
    !!--++    ALGORITHM AS7, Applied statistics, VOL.17, 1968, with  modifications
    !!--++    by A.J.MILLER
    !!--<<
    !!--++    Note: If RMAX = 1.E-04 then the diagonal elements of C will be
    !!--++          accurate to about 4 dec. digits.
    !!-->>
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Syminv(A, N, C, Nullty, Ifault, Rmax)
       !---- Arguments ----!
       real(kind=cp), dimension(:),  intent(in)     :: A
       integer,                      intent(in)     :: N
       real(kind=cp), dimension(:),  intent(out)    :: C
       integer,                      intent(out)    :: Nullty
       integer,                      intent(out)    :: Ifault
       real(kind=cp),                intent(out)    :: Rmax

       !---- Local variables ----!
       integer                          :: i, icol, irow, j, jcol, k, l, mdiag, ndiag, nn, nrow
       real(kind=cp), parameter         :: zero = 0.0_cp, one = 1.0_cp
       real(kind=cp)                    :: x
       real(kind=cp), dimension(size(a)):: w  !workspace array that was in the argument

       w=zero
       nrow = n
       ifault = 1
       if (nrow > 0) then
          ifault = 0
          !---- Cholesky factorization of A, result in C
          call chola(a, nrow, c, nullty, ifault, rmax, w)
          if (ifault == 0) then
             !---- Invert C & form the product (CINV)'*CINV, where CINV is the inverse
             !---- of C, row by row starting with the last row.
             !---- IROW = The row number, NDIAG = Location of last element in the row.
             nn = nrow * (nrow+1) / 2
             irow = nrow
             ndiag = nn
             do
                if (c(ndiag) /= zero) then
                   l = ndiag
                   do i = irow, nrow
                      w(i) = c(l)
                      l = l + i
                   end do
                   icol = nrow
                   jcol = nn
                   mdiag = nn
                   do
                      l = jcol
                      x = zero
                      if (icol == irow) x = one / w(irow)
                      k = nrow
                      do
                         if (k /= irow) then
                            x = x - w(k) * c(l)
                            k = k - 1
                            l = l - 1
                            if (l > mdiag) l = l - k + 1
                            cycle
                         else
                            exit
                         end if
                      end do

                      c(l) = x / w(irow)
                      if (icol == irow) exit
                      mdiag = mdiag - icol
                      icol = icol - 1
                      jcol = jcol - 1
                   end do

                else
                   l = ndiag
                   do j = irow, nrow
                      c(l) = zero
                      l = l + j
                   end do
                end if ! (c(ndiag) /= zero)

                ndiag = ndiag - irow
                irow = irow - 1
                if (irow /= 0) cycle
                exit
             end do
          end if
       end if

    End Subroutine Syminv

    !!--++
    !!--++ Subroutine Chola(A, N, U, Nullty, Ifault, Rmax, R)
    !!--++    real (kind=cp),dimension(:), intent(in)   :: A        ! a +ve definite matrix stored in lower-triangular form
    !!--++    integer,                     intent(in)   :: N        ! The order of A
    !!--++    real (kind=cp),dimension(:), intent(out)  :: U        ! a lower triangular matrix such that U*U' = A.
    !!--++    integer,                     intent(out)  :: Nullty   ! the rank deficiency of A.
    !!--++    integer,                     intent(out)  :: Ifault   ! Error Code: 1 if N < 1, 2 If A Is not +ve semi-definite
    !!--++                                                          !             0 Otherwise
    !!--++    real (kind=cp),              intent(out)  :: Rmax     ! an estimate of the relative accuracy of the diagonal elements of U.
    !!--++    real (kind=cp),dimension(:), intent(out)  :: R        ! array containing bounds on the relative accuracy of each diagonal element of U.
    !!--++
    !!--++    Algorithm AS6, Applied Statistics, vol.17, (1968) with  modifications
    !!--++    by A.J.MILLER
    !!--<<
    !!--++    Note: Eta should be set equal to the smallest +ve value such that
    !!--++          1.0_cp + eta is calculated as being greater than 1.0_cp in
    !!--++          the accuracy
    !!--++
    !!--++      Given a symmetric matrix order n as lower triangle in a( ),
    !!--++      calculates an upper triangle, u( ), such that uprime * u = a.
    !!--++      a must be positive semi-definite.  eta is set to multiplying
    !!--++      factor determining effective zero for pivot.
    !!--++
    !!--++      arguments:-
    !!--++      a()     = input, a +ve definite matrix stored in lower-triangular form.
    !!--++      n       = input, the order of a
    !!--++      u()     = output, a lower triangular matrix such that u*u' = a.
    !!--++                a & u may occupy the same locations.
    !!--++      nullty  = output, the rank deficiency of a.
    !!--++      ifault  = output, error indicator
    !!--++                      = 1 if n < 1
    !!--++                      = 2 if a is not +ve semi-definite
    !!--++                      = 3 if nn < n*(n+1)/2
    !!--++                      = 0 otherwise
    !!-->>
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Chola(A, N, U, Nullty, Ifault, Rmax, R)
       !---- Arguments ----!
       real(kind=cp),dimension(:), intent(in)   :: a
       integer,                    intent(in)   :: n
       real(kind=cp),dimension(:), intent(out)  :: u
       integer,                    intent(out)  :: nullty
       integer,                    intent(out)  :: ifault
       real(kind=cp),              intent(out)  :: rmax
       real(kind=cp),dimension(:), intent(out)  :: r

       !---- Local variables ----!
       integer                   :: i, icol, irow, j, k, l, m
       real (kind=cp), parameter :: eta = epsilon(1.0_cp), zero = 0.0_cp
       real (kind=cp)            :: rsq, w

       ifault = 1
       u=zero  !included here to avoid its use before initialization
       if (n > 0) then
          ifault = 2
          nullty = 0
          rmax = eta
          r(1) = eta
          j = 1
          k = 0
          do icol = 1, n         !     Factorize column by column, ICOL = Column no.
             l = 0
             do irow = 1, icol    !     IROW = Row number within column ICOL
                k = k + 1
                w = a(k)
                if (irow == icol) rsq = (w*eta) ** 2
                m = j
                do i = 1, irow
                   l = l + 1
                   if (i == irow) exit
                   w = w - u(l) * u(m)
                   if (irow == icol) rsq = rsq + (u(l)**2*r(i)) ** 2
                   m = m + 1
                end do
                if (irow == icol) exit
                if (u(l) /= zero) then
                   u(k) = w / u(l)
                else
                   u(k) = zero
                   if (abs(w) > abs(rmax*a(k))) return
                end if
             end do

             !---- End of row, estimate relative accuracy of diagonal element.
             rsq = sqrt(rsq)
             if (abs(w) > 5.0*rsq) then
                if (w < zero) return
                u(k) = sqrt(w)
                r(i) = rsq / w
                if (r(i) > rmax) rmax = r(i)
             else
                u(k) = zero
                nullty = nullty + 1
             end if
             j = j + icol
          end do
          ifault = zero
       end if

    End Subroutine Chola

 End Submodule OPT_Simplex