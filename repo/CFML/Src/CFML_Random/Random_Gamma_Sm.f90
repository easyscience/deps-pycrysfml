!!----
!!----
!!----
SubModule (CFML_Random) Random_Gamma_Sm
  implicit none
   Contains
   !!----
   !!---- SUBROUTINE RANDOM_CHISQ
   !!----
   !!----    Generates a random variate from the chi-squared
   !!----    distribution with ndf degrees of freedom
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_ChiSQ(Ndf, First) Result(Fn_Val)
      !---- Arguments ----!
      integer, intent(in) :: ndf
      logical, intent(in) :: first
      real(kind=cp)       :: fn_val


      fn_val=random_gamma(half*ndf, first)
      fn_val = two * fn_val

   End Function Random_ChiSQ

   !!----
   !!---- RANDOM_EXPONENTIAL
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate in [0,infinity) from a negative exponential
   !!----    distribution wlth density proportional to exp(-random_exponential), using inversion.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Exponential() Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp)  :: fn_val

      !---- Local variable ----!
      real(kind=cp) :: r

      do
         call random_number(r)
         if (r > zero) exit
      end do

      fn_val = -log(r)

   End Function Random_Exponential

   !!----
   !!---- RANDOM_GAMMA
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random gamma variate.
   !!--..
   !!--..    calls either random_gamma1 (S > 1.0)
   !!--..    or random_exponential (S = 1.0)
   !!--..    or random_gamma2 (S < 1.0).
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Gamma(S, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)  :: s        !shape parameter of distribution (0.0 < real)
      logical,       intent(in)  :: first
      real(kind=cp)              :: fn_val

      !> Init
      Fn_val=0.0_cp
      if (s <= zero) then
         err_cfml%Ierr=1
         err_cfml%msg="RANDOM_GAMMA@RANDOM: Shape Parameter Value Must Be Positive"
         return
      end if

      if (s > one) then
         fn_val=random_gamma1(s, first)
      else if (s < one) then
         fn_val=random_gamma2(s, first)
      else
         fn_val= random_exponential()
      end if

   End Function Random_Gamma

   !!----
   !!---- SUBROUTINE RANDOM_GAMMA1
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate in [0,infinity) from a gamma distribution
   !!----    with density proportional to gamma**(s-1)*exp(-gamma), based upon best"s t
   !!----    distribution method
   !!----
   !!---- Update: 11/07/2015
   !!
   Module Function Random_Gamma1(S, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)   :: s         ! shape parameter of distribution (1.0 < real)
      logical,       intent(in)   :: first
      real(kind=cp)               :: fn_val

      !---- Local variables ----!
      real(kind=cp)            :: d, r, g, f, x
      real(kind=cp), save      :: b, h
      real(kind=cp), parameter :: sixty4 = 64.0_cp, three = 3.0_cp, pt75 = 0.75_cp

      !> Init
      Fn_Val=0.0_cp
      if (s <= one) then
         err_cfml%Ierr=1
         err_cfml%msg="RANDOM_GAMMA1@RANDOM: Impermissible Shape Parameter Value"
         return
      end if

      if (first) then                        ! initialization, if necessary
         b = s - one
         h = sqrt(three*s - pt75)
      end if

      do
         call random_number(r)
         g = r - r*r
         if (g <= zero) cycle
         f = (r - half)*h/sqrt(g)
         x = b + f
         if (x <= zero) cycle
         call random_number(r)
         d = sixty4*g*(r*g)**2
         if (d <= zero) exit
         if (d*x < x - two*f*f) exit
         if (log(d) < two*(b*log(x/b) - f)) exit
      end do
      fn_val = x

   End Function Random_Gamma1

   !!----
   !!---- RANDOM_GAMMA2
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate in [0,infinity) from
   !!----    a gamma distribution with density proportional to
   !!----    gamma2**(s-1) * exp(-gamma2), using a switching method.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Gamma2(S, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)  :: s          ! shape parameter of distribution (1.0 < real)
      logical,       intent(in)  :: first
      real(kind=cp)              :: fn_val

      !---- Local variables ----!
      real(kind=cp)       :: r, x, w
      real(kind=cp), save :: a, p, c, uf, vr, d

      !> Init
      Fn_val=0.0_cp
      if (s <= zero .or. s >= one) then
         err_cfml%IErr=1
         err_cfml%msg="RANDOM_GAMMA2@RANDOM: Shape Parameter Value Outside Permitted Range"
         return
      end if

      if (first) then                        ! initialization, if necessary
         a = one - s
         p = a/(a + s*exp(-a))
         if (s < vsmall) then
            err_cfml%IErr=1
            err_cfml%msg="RANDOM_GAMMA2@RANDOM: Shape Parameter Value Too Small"
            return
         end if
         c = one/s
         uf = p*(vsmall/a)**s
         vr = one - vsmall
         d = a*log(a)
      end if

      do
         call random_number(r)
         if (r >= vr) then
            cycle
         else if (r > p) then
            x = a - log((one - r)/(one - p))
            w = a*log(x)-d
         else if (r > uf) then
            x = a*(r/p)**c
            w = x
         else
            fn_val = zero
            return
         end if

         call random_number(r)
         if (one-r <= w .and. r > zero) then
            if (r*(w + one) >= one) cycle
            if (-log(r) <= w) cycle
         end if
         exit
      end do

      fn_val = x

   End Function Random_Gamma2

   !!----
   !!---- RANDOM_WEIBULL
   !!----
   !!----                                a
   !!----                         a-1  -x
   !!----               f(x) = a.x    e
   !!----
   !!----    Generates a random variate from the Weibull distribution with
   !!----    probability density as shown before.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Weibull(a) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)  :: a
      real(kind=cp)              :: fn_val

      !---- For speed, there is no checking that a is
      !---- not zero or very small.
      fn_val=random_exponential()
      fn_val = fn_val** (one/a)

   End Function Random_Weibull

End SubModule Random_Gamma_Sm