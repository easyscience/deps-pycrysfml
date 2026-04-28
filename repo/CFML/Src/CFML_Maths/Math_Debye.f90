!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_Debye
 implicit none

 Contains
    !!----
    !!---- DEBYE
    !!----
    !!---- 02/04/2019
    !!
    Module Function Debye(N,X) Result(Fval)
       !---- Arguments ----!
       integer,       intent(in) :: N ! Order of the Debye function
       real(kind=cp), intent(in) :: X ! Value
       real(kind=cp)             :: fval

       !---- Local Variables ----!
       real(kind=dp) :: xx,ff

       !> Init
       fval=0.0_cp
       call clear_error()

       !> Check
       if (n <= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="MATHS@DEBYE: The order for Debye function was ZERO!"
          return
       end if

       !> precision
       if (CP == SP) then
          xx=dble(x)
       else
          xx=x
       end if

       !> Calculation
       ff=Debye_DP(n,xx)
       if (Err_CFML%Ierr /= 0) return

       !> Precision
       if (CP == SP) then
          fval=real(ff)
       else
          fval=ff
       end if

    End Function Debye

    !!----
    !!---- DEBYE_PR_CHEBYSHEVSERIES
    !!----
    !!----    PRIVATE (USED FOR DEBYE FUNCTIONS)
    !!----    This function evaluates a Chebyshev series, using the Clenshaw method
    !!----    with Reinsch modification, as analysed in the paper by Oliver in
    !!----    J.I.M.A., vol. 20, 1977, pp379-391
    !!----
    !!----  28/03/2019
    !!
    Module Function Debye_PR_ChebyshevSeries(n, a, t) Result(fval)
       !---- Arguments ----!
       integer,                       intent(in) :: N    ! The no. of terms in the sequence
       real(kind=dp), dimension(0:N), intent(in) :: A    ! The coefficients of the Chebyshev series
       real(kind=dp),                 intent(in) :: T    ! The value at which the series is to be evaluated
       real(kind=dp)                             :: fval ! Return value
       !---- Local Variables ----!
       real(kind=dp), parameter  :: HALF = 0.5_dp
       real(kind=dp), parameter  :: TWO  = 2.0_dp
       real(kind=dp), parameter  :: TEST = 0.6_dp

       integer       :: i
       real(kind=dp) :: d1, d2, tt, u0, u1, u2


       !> Init
       u1 = 0.0_dp

       !>  If ABS ( T )  < 0.6 use the standard Clenshaw method
       if (abs(t) < test) then
          u0 = 0.0_dp
          tt = t + t
          do i = n, 0, -1
             u2 = u1
             u1 = u0
             u0 = tt * u1 + a(i) - u2
          end do
          fval = (u0-u2) / two

       else
          !> If ABS ( T )  > =  0.6 use the Reinsch modification
          d1 = 0.0_dp

          !>  T > =  0.6 code
          if (t > 0.0_dp) then
             tt = (t-half) - half
             tt = tt + tt
             do i = n, 0, -1
                d2 = d1
                u2 = u1
                d1 = tt * u2 + a(i) + d2
                u1 = d1 + u2
             end do
             fval = (d1+d2) / two

          else
             !> T < =  -0.6 code
             tt = (t+half) + half
             tt = tt + tt
             do i = n, 0, -1
                d2 = d1
                u2 = u1
                d1 = tt * u2 + a(i) - d2
                u1 = d1 - u2
             end do
             fval = (d1-d2) / two
          end if
       end if

    End Function Debye_PR_ChebyshevSeries

    !!----
    !!---- FUNCTION DEBYE_DP
    !!----    Calculates the Debye function of order N
    !!----
    !!----    If X < 0.0 then limited to |x| < 2*Pi
    !!----
    !!---- 28/03/2019
    !!
    Module Function Debye_DP(N,X) Result(Fval)
       !---- Arguments ----!
       integer,       intent(in) :: N ! Order of the Debye function
       real(kind=dp), intent(in) :: X ! Value
       real(kind=dp)             :: fval

       !> Init
       fval=0.0_dp

       !> Check
       if (n <= 0) then
          err_CFML%IErr=1
          err_CFML%Msg="MATHS@DEBYE_DP: The order for Debye function was ZERO!"
          return
       end if

       if (x < 0.0_dp) then
          if (abs(x) > TPI) then
             err_CFML%IErr=1
             err_CFML%Msg="MATHS@DEBYE_DP: The argument is negative and less than 2Pi"
             return
          end if
          fval =DebyeN(n,x)
       else
          select case (n)
             case (1)
                fval=Debye1(x)
             case (2)
                fval=Debye2(x)
             case (3)
                fval=Debye3(x)
             case (4)
                fval=Debye4(x)
             case (5:)
                if (x > TPI) then
                   err_CFML%IErr=1
                   ERR_CFML%Msg="MATHS@DEBYE_DP: The argument was greater then 2Pi and the order >= 5!"
                  return
                end if
                fval=DebyeN(n,x)

          end select
       end if

    End Function Debye_DP



    !!----
    !!---- FUNCTION DEBYE1
    !!----    Calculates the Debye function of order 1, defined as
    !!----    DEBYE1(x) = [Integral {0 to x} t/(exp(t)-1) dt] / x
    !!----
    !!----    The code uses Chebyshev series whose coefficients are given to 20
    !!----    decimal places.
    !!----
    !!---- EXTRA INFORMATION
    !!----
    !!---- If X < 0.0 an error message is defined and the function returns the value 0.0
    !!----
    !!---- NTERMS: The no. of elements of the array ADEB1. The recommended value is such that
    !!----         ABS(ADEB1(NTERMS)) < EPS/100 , with 1 <= NTERMS <= 18
    !!----
    !!----   XLOW: The value below which DEBYE1 = 1 - x/4 + x*x/36 to machine precision.
    !!----         The recommended value is SQRT(8*EPSNEG)
    !!----
    !!---- XUPPER: The value above which DEBYE1 = (pi*pi/(6*x)) - exp(-x)(x+1)/x.
    !!----         The recommended value is -LOG(2*EPS)
    !!----
    !!----   XLIM: The value above which DEBYE1 = pi*pi/(6*x)
    !!----         The recommended value is -LOG(XMIN)
    !!----
    !!---- 28/03/2019
    !!
    Module Function Debye1(X) Result(Fval)
       !---- Arguments ----!
       real(kind=dp), intent(in) :: X
       real(kind=dp)             :: fval

       !---- Local Variables ----!
       integer       :: i, nexp, nterms
       real(kind=dp) :: expmx, rk, suma, t, xx, xk, xlim, xlow, xupper

       real(kind=dp), parameter :: QUART = 0.25_dp
       real(kind=dp), parameter :: HALF  = 0.50_dp
       real(kind=dp), parameter :: ONE   = 1.00_dp
       real(kind=dp), parameter :: FOUR  = 4.00_dp
       real(kind=dp), parameter :: EIGHT = 8.00_dp
       real(kind=dp), parameter :: NINE  = 9.00_dp
       real(kind=dp), parameter :: THIRT6= 36.0_dp
       real(kind=dp), parameter :: ONEHUN=100.0_dp
       real(kind=dp), parameter :: DEBINF= 0.60792710185402662866_dp

       real(kind=dp), parameter :: ADEB1(0:18) = (/  &
                                                 2.40065971903814101941_dp, 0.19372130421893600885_dp,  &
                                                -0.623291245548957703D-2,   0.35111747702064800D-3,     &
                                                -0.2282224667012310D-4,     0.158054678750300D-5,       &
                                                -0.11353781970719D-6,       0.835833611875D-8,          &
                                                -0.62644247872D-9,          0.4760334890D-10,           &
                                                -0.365741540D-11,           0.28354310D-12,             &
                                                -0.2214729D-13,             0.174092D-14,               &
                                                -0.13759D-15,               0.1093D-16,                 &
                                                -0.87D-18,                  0.7D-19,                    &
                                                -0.1D-19 /)

       !> Start computation
       xx = x

       !> Check x >= 0.0
       if (xx < 0.0_dp) then
          !> Error activated
          err_CFML%IErr=1
          ERR_CFML%Msg="MATHS@DEBYE1: doesn't work with negative Argument"
          fval = 0.0_dp
          return
       end if

       !> Constants
       t = EPSILON(0.0_dp)
       xlow = SQRT(t*eight)
       xupper = -LOG(t+t)
       xlim = -LOG(TINY(0.0_dp))
       t = t / onehun
       do nterms = 18, 0, -1
          if (abs(adeb1(nterms)) > t) exit
       end do

       !> Code for xx <= 4.0
       if (xx <= four) then
          if (xx < xlow) then
             fval = ((xx-nine)*xx+thirt6) / thirt6
          else
             t = ((xx*xx/eight)-half) - half
             fval = Debye_PR_ChebyshevSeries(nterms,adeb1,t) - quart * xx
          end if

       else
          !> Code for xx > 4.0
          fval = one / (xx*debinf)
          if (xx < xlim) then
             expmx = EXP(-xx)
             if (xx > xupper) then
                fval = fval - expmx * (one+one/xx)
             else
                suma = 0.0_dp
                rk = AINT(xlim/xx)
                nexp = INT(rk)
                xk = rk * xx
                do i = nexp, 1, -1
                   t = (one+one/xk) / rk
                   suma = suma * expmx + t
                   rk = rk - one
                   xk = xk - xx
                end do
                fval = fval - suma * expmx
             end if
          end if
       end if

    End Function Debye1

    !!----
    !!---- FUNCTION DEBYE2
    !!----    Calculates the Debye function of order 2, defined as
    !!----    DEBYE2(x) = 2*[Integral {0 to x} t*t/(exp(t)-1) dt] / (x*x)
    !!----
    !!----    The code uses Chebyshev series whose coefficients are given to 20
    !!----    decimal places.
    !!----
    !!---- EXTRA INFORMATION
    !!----
    !!---- If X < 0.0 an error message is defined and the function returns the value 0.0
    !!----
    !!---- NTERMS: The no. of elements of the array ADEB2. The recommended value is such that
    !!----         ABS(ADEB2(NTERMS)) < EPS/100 , with 1 <= NTERMS <= 18
    !!----
    !!----   XLOW: The value below which DEBYE2 = 1 - x/3 + x*x/24 to machine precision.
    !!----         The recommended value is SQRT(8*EPSNEG)
    !!----
    !!---- XUPPER: The value above which DEBYE2 = (4*zeta(3)/x^2) - 2*exp(-x)(x^2+2x+1)/x^2.
    !!----         The recommended value is -LOG(2*EPS)
    !!----
    !!----  XLIM1: The value above which DEBYE2 = 4*zeta(3)/x^2
    !!----         The recommended value is -LOG(XMIN)
    !!----
    !!----  XLIM2: The value above which DEBYE2 = 0.0 to machine precision.
    !!----         The recommended value is SQRT(4.8/XMIN)
    !!----
    !!---- Update: January 2017
    !!
    Module Function Debye2(X) Result(Fval)
       !---- Argument ----!
       real(kind=dp), intent(in) :: X
       real(kind=dp)             :: fval

       !---- Local Variables ----!
       integer       :: i, nexp, nterms
       real(kind=dp) :: expmx, rk, suma, t, xx, xk, xlim1, xlim2, xlow, xupper

       real(kind=dp), parameter :: QUART = 0.25_dp
       real(kind=dp), parameter :: HALF  = 0.50_dp
       real(kind=dp), parameter :: ONE   = 1.00_dp
       real(kind=dp), parameter :: TWO   = 2.00_dp
       real(kind=dp), parameter :: THREE = 3.00_dp
       real(kind=dp), parameter :: FOUR  = 4.00_dp
       real(kind=dp), parameter :: EIGHT = 8.00_dp
       real(kind=dp), parameter :: TWENT4= 24.0_dp
       real(kind=dp), parameter :: ONEHUN=100.0_dp
       real(kind=dp), parameter :: DEBINF= 4.80822761263837714160_dp

       real(kind=dp), parameter :: ADEB2(0:18) = (/  &
                                                 2.59438102325707702826_dp, 0.28633572045307198337_dp,  &
                                                -0.1020626561580467129D-1,  0.60491097753468435D-3,     &
                                                -0.4052576589502104D-4,     0.286338263288107D-5,       &
                                                -0.20863943030651D-6,       0.1552378758264D-7,         &
                                                -0.117312800866D-8,         0.8973585888D-10,           &
                                                -0.693176137D-11,           0.53980568D-12,             &
                                                -0.4232405D-13,             0.333778D-14,               &
                                                -0.26455D-15,               0.2106D-16,                 &
                                                -0.168D-17,                 0.13D-18,                   &
                                                -0.1D-19 /)

       !> Start computation
       xx = x

       !> Check xx >= 0.0
       if (xx < 0.0_dp) then
          ! Error activated
          err_CFML%IErr=1
          ERR_CFML%Msg="MATHS@DEBYE2: doesn't work with negative Argument"
          fval = 0.0_dp
          return
       end if

       !> Constants.
       t = TINY(0.0_dp)
       xlim1 = -LOG(t)
       xlim2 = SQRT(debinf) / SQRT(t)
       t = EPSILON(0.0_dp)
       xlow = SQRT(t*eight)
       xupper = -LOG(t+t)
       t = t / onehun
       do nterms = 18, 0, -1
          if (ABS(adeb2(nterms)) > t) exit
       end do

       !> Code for xx <= 4.0
       if (xx <= four) then
          if (xx < xlow) then
             fval = ((xx-eight)*xx+twent4) / twent4
          else
             t = ((xx*xx/eight)-half) - half
             fval = Debye_PR_ChebyshevSeries(nterms,adeb2,t) - xx / three
          end if

       else
          !> Code for xx > 4.0
          if (xx > xlim2) then
             fval = 0.0_dp
          else
             fval = debinf / (xx*xx)
             if (xx < xlim1) then
                expmx = EXP(-xx)
                if (xx > xupper) then
                   suma = ((xx+two)*xx+two) / (xx*xx)
                else
                   suma = 0.0_dp
                   rk = AINT(xlim1/xx)
                   nexp = INT(rk)
                   xk = rk * xx
                   do i = nexp, 1, -1
                      t = (one+two/xk+two/(xk*xk)) / rk
                      suma = suma * expmx + t
                      rk = rk - one
                      xk = xk - xx
                   end do
                end if
                fval = fval - two * suma * expmx
             end if
          end if
       end if

    End Function Debye2

    !!----
    !!---- DEBYE3
    !!----    Calculates the Debye function of order 3, defined as
    !!----    DEBYE3(x) = 3*[Integral {0 to x} t^3/(exp(t)-1) dt] / (x^3)
    !!----
    !!----    The code uses Chebyshev series whose coefficients are given to 20
    !!----    decimal places.
    !!----
    !!---- EXTRA INFORMATION
    !!----
    !!---- If X < 0.0 an error message is defined and the function returns the value 0.0
    !!----
    !!---- NTERMS: The no. of elements of the array ADEB3. The recommended value is such that
    !!----         ABS(ADEB3(NTERMS)) < EPS/100 , with 1 <= NTERMS <= 18
    !!----
    !!----   XLOW: The value below which DEBYE3 = 1 - 3x/8 + x*x/20 to machine precision.
    !!----         The recommended value is SQRT(8*EPSNEG)
    !!----
    !!---- XUPPER: The value above which DEBYE3 = (18*zeta(4)/x^3) - 3*exp(-x)(x^3+3x^2+6x+6)/x^3.
    !!----         The recommended value is -LOG(2*EPS)
    !!----
    !!----  XLIM1: The value above which DEBYE3 = 18*zeta(4)/x^3
    !!----         The recommended value is -LOG(XMIN)
    !!----
    !!----  XLIM2: The value above which DEBYE3 = 0.0 to machine precision.
    !!----         The recommended value is CUBE ROOT(19/XMIN)
    !!----
    !!---- 28/03/2019
    !!
    Module Function Debye3(X) Result(Fval)
       !---- Argument ----!
       real(kind=dp), intent(in) :: X
       real(kind=dp)             :: fval

       !---- Local Variables ----!
       integer       :: i, nexp, nterms
       real(kind=dp) :: expmx, rk, suma, t, xx, xk, xki, xlim1, xlim2, xlow, xupper

       real(kind=dp), parameter :: PT375 = 0.375_dp
       real(kind=dp), parameter :: HALF  = 0.50_dp
       real(kind=dp), parameter :: ONE   = 1.00_dp
       real(kind=dp), parameter :: THREE = 3.00_dp
       real(kind=dp), parameter :: FOUR  = 4.00_dp
       real(kind=dp), parameter :: SIX   = 6.00_dp
       real(kind=dp), parameter :: SEVP5 = 7.50_dp
       real(kind=dp), parameter :: EIGHT = 8.00_dp
       real(kind=dp), parameter :: TWENTY= 20.0_dp
       real(kind=dp), parameter :: ONEHUN=100.0_dp
       real(kind=dp), parameter :: DEBINF= 0.51329911273421675946D-1

       real(kind=dp), parameter :: ADEB3(0:18) = (/  &
                                                 2.70773706832744094526_dp, 0.34006813521109175100_dp,  &
                                                -0.1294515018444086863D-1,  0.79637553801738164D-3,     &
                                                -0.5463600095908238D-4,     0.392430195988049D-5,       &
                                                -0.28940328235386D-6,       0.2173176139625D-7,         &
                                                -0.165420999498D-8,         0.12727961892D-9,           &
                                                -0.987963459D-11,           0.77250740D-12,             &
                                                -0.6077972D-13,             0.480759D-14,               &
                                                -0.38204D-15,               0.3048D-16,                 &
                                                -0.244D-17,                 0.20D-18,                   &
                                                -0.2D-19 /)

       !> Start computation
       xx = x

       !> Error test
       if (xx < 0.0_dp) then
          ! Error activated
          err_CFML%IErr=1
          ERR_CFML%Msg="MATHS@DEBYE3: doesn't work with negative Argument"
          fval = 0.0_dp
          return
       end if

       !> Constants.
       t = TINY(0.0_dp)
       xlim1 = -LOG(t)
       xk = one / three
       xki = (one/debinf) ** xk
       rk = t ** xk
       xlim2 = xki / rk
       t = EPSILON(0.0_dp)
       xlow = SQRT(t*eight)
       xupper = -LOG(t+t)
       t = t / onehun
       do nterms = 18, 0, -1
          if (ABS(adeb3(nterms)) > t) exit
       end do

       !> Code for xx <= 4.0
       if (xx <= four) then
          if (xx < xlow) then
             fval = ((xx-sevp5)*xx+twenty) / twenty
          else
             t = ((xx*xx/eight)-half) - half
             fval = Debye_PR_ChebyshevSeries(nterms,adeb3,t) - pt375 * xx
          end if

       else
          !> Code for xx > 4.0
          if (xx > xlim2) then
             fval = 0.0_dp
          else
             fval = one / (debinf*xx*xx*xx)
             if (xx < xlim1) then
                expmx = EXP(-xx)
                if (xx > xupper) then
                   suma = (((xx+three)*xx+six)*xx+six) / (xx*xx*xx)
                else
                   suma = 0.0_dp
                   rk = AINT(xlim1/xx)
                   nexp = INT(rk)
                   xk = rk * xx
                   do i = nexp, 1, -1
                      xki = one / xk
                      t = (((six*xki+six)*xki+three)*xki+one) / rk
                      suma = suma * expmx + t
                      rk = rk - one
                      xk = xk - xx
                   end do
                end if
                fval = fval - three * suma * expmx
             end if
          end if
       end if

    End Function Debye3

    !!----
    !!---- DEBYE4
    !!----    Calculates the Debye function of order 4, defined as
    !!----    DEBYE4(x) = 4*[Integral {0 to x} t^4/(exp(t)-1) dt] / (x^4)
    !!----
    !!----    The code uses Chebyshev series whose coefficients are given to 20
    !!----    decimal places.
    !!----
    !!---- EXTRA INFORMATION
    !!----
    !!---- If X < 0.0 an error message is defined and the function returns the value 0.0
    !!----
    !!---- NTERMS: The no. of elements of the array ADEB4. The recommended value is such that
    !!----         ABS(ADEB4(NTERMS)) < EPS/100 , with 1 <= NTERMS <= 18
    !!----
    !!----   XLOW: The value below which  DEBYE4 = 1 - 4x/10 + x*x/18 to machine precision.
    !!----         The recommended value is SQRT(8*EPSNEG)
    !!----
    !!---- XUPPER: The value above which DEBYE4=(96*zeta(5)/x^4)-4*exp(-x)(x^4+4x^2+12x^2+24x+24)/x^4
    !!----         The recommended value is -LOG(2*EPS)
    !!----
    !!----  XLIM1: The value above which DEBYE4 = 96*zeta(5)/x^4
    !!----         The recommended value is -LOG(XMIN)
    !!----
    !!----  XLIM2: The value above which DEBYE4 = 0.0 to machine precision.
    !!----         The recommended value is FOURTH ROOT(99/XMIN)
    !!----
    !!---- 28/03/2019
    !!
    Module Function Debye4(X) Result(FVal)
       !---- Argument ----!
       real(kind=dp), intent(in) :: X
       real(kind=dp)             :: fval

       !---- Local Variables ----!
       integer       :: i, nexp, nterms
       real(kind=dp) :: expmx, rk, suma, t, xx, xk, xki, xlim1, xlim2, xlow, xupper

       real(kind=dp), parameter :: HALF  = 0.50_dp
       real(kind=dp), parameter :: ONE   = 1.00_dp
       real(kind=dp), parameter :: TWOPT5= 2.50_dp
       real(kind=dp), parameter :: FOUR  = 4.00_dp
       real(kind=dp), parameter :: FIVE  = 5.00_dp
       real(kind=dp), parameter :: EIGHT = 8.00_dp
       real(kind=dp), parameter :: TWELVE= 12.0_dp
       real(kind=dp), parameter :: EIGHTN= 18.0_dp
       real(kind=dp), parameter :: TWENT4= 24.0_dp
       real(kind=dp), parameter :: FORTY5= 45.0_dp
       real(kind=dp), parameter :: ONEHUN=100.0_dp
       real(kind=dp), parameter :: DEBINF= 99.54506449376351292781_dp


       real(kind=dp), parameter :: ADEB4(0:18) = (/  &
                                                 2.78186941502052346008_dp, 0.37497678352689286364_dp,  &
                                                -0.1494090739903158326D-1,  0.94567981143704274D-3,     &
                                                -0.6613291613893255D-4,     0.481563298214449D-5,       &
                                                -0.35880839587593D-6,       0.2716011874160D-7,         &
                                                -0.208070991223D-8,         0.16093838692D-9,           &
                                                -0.1254709791D-10,          0.98472647D-12,             &
                                                -0.7772369D-13,             0.616483D-14,               &
                                                -0.49107D-15,               0.3927D-16,                 &
                                                -0.315D-17,                 0.25D-18,                   &
                                                -0.2D-19 /)

       !> Start computation
       xx = x

       !> Error test
       if (xx < 0.0_dp) then
          ! Error activated
          err_CFML%IErr=1
          err_cfml%msg="MATHS@DEBYE4: doesn't work with negative Argument"
          fval = 0.0_dp
          return
       end if

       !> Constants.
       t = TINY(0.0_dp)
       xlim1 = -LOG(t)
       rk = one / four
       xk = debinf ** rk
       xki = t ** rk
       xlim2 = xk / xki
       t = EPSILON(0.0_dp)
       xlow = SQRT(t*eight)
       xupper = -LOG(t+t)
       t = t / onehun
       do nterms = 18, 0, -1
          if (ABS(adeb4(nterms)) > t) exit
       end do

       !> Code for xx <= 4.0
       if (xx <= four) then
          if (xx < xlow) then
             fval = ((twopt5*xx-eightn)*xx+forty5) / forty5
          else
             t = ((xx*xx/eight)-half) - half
             fval = Debye_PR_ChebyshevSeries(nterms,adeb4,t) - (xx+xx) / five
          end if

       else
          !> Code for xx > 4.0
          if (xx > xlim2) then
             fval = 0.0_dp
          else
             t = xx * xx
             fval = (debinf/t) / t
             if (xx < xlim1) then
                expmx = EXP(-xx)
                if (xx > xupper) then
                   suma = ((((xx+four)*xx+twelve)*xx+twent4)*xx+twent4) / (xx*xx*xx*xx )
                else
                   suma = 0.0_dp
                   rk = INT(xlim1/xx)
                   nexp = INT(rk)
                   xk = rk * xx
                   do i = nexp, 1, -1
                      xki = one / xk
                      t = ((((twent4*xki+twent4)*xki+twelve)*xki+four)*xki+one ) / rk
                      suma = suma * expmx + t
                      rk = rk - one
                      xk = xk - xx
                   end do
                end if
                fval = fval - four * suma * expmx
             end if
          end if
       end if

    End Function Debye4

    !!----
    !!---- DEBYEN
    !!----    Calculates the Debye function of order N of X.
    !!----
    !!----    Limitation: |x| < 2*Pi
    !!----
    !!---- 28/03/2019
    !!
    Module Function DebyeN(n,x) Result(Fval)
       !---- Arguments ----!
       integer,       intent(in) :: N ! Order of Debye function
       real(kind=dp), intent(in) :: X
       real(kind=dp)             :: Fval

       !---- Local Variables ----!
       integer, parameter                        :: KMAX=12
       integer                                   :: k,i
       real(kind=dp)                             :: den,t1,t2
       real(kind=dp), dimension(KMAX), parameter :: B2K=(/                                       &
                                                      1.0_dp/  6.0_dp,        -1.0_dp/  30.0_dp, &
                                                      1.0_dp/ 42.0_dp,        -1.0_dp/  30.0_dp, &
                                                      5.0_dp/ 66.0_dp,      -691.0_dp/2730.0_dp, &
                                                      7.0_dp/  6.0_dp,     -3617.0_dp/ 510.0_dp, &
                                                  43867.0_dp/798.0_dp,   -174611.0_dp/ 330.0_dp, &
                                                 854513.0_dp/138.0_dp,-236364091.0_dp/2730.0_dp/)

       !> Init
       fval=0.0_dp

       !> Check
       if (abs(x) > 2.0*pi) then
          ! Error Flag
          err_CFML%IErr=1
          ERR_CFML%Msg="MATHS@DEBYEN: The absolute value of argument for DEBYEN was greater than 2PI "
          return
       end if

       !> Calculation
       t1=dble(n)/dble(2*(n+1))

       t2=0.0_dp
       do k=1,kmax
          i=2*k
          den=dble(2*k+n)*factorial_R(i)
          t2 = t2 + (B2K(k)/den)*(x**(2*k))
       end do
       fval = 1.0_dp - (t1*x) + (n*t2)

    End Function DebyeN

End Submodule Math_Debye
