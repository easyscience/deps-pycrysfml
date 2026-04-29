!!----
!!----
!!----
SubModule (CFML_Random) Random_Binomial_Sm

  implicit none
   Contains
   !!----
   !!---- RANDOM_BINOMIAL1
   !!----
   !!----    Generates A Random Binomial Variate Using C.D.Kemp"s method.
   !!----    This algorithm is suitable when many random variates are
   !!----    required with the SAME parameter values for n & p.
   !!----
   !!--..    Reference: Kemp, C.D. (1986). `A modal method for generating
   !!--..    binomial variables", Commun. Statist. - Theor. Meth. 15(3),
   !!--..    805-813.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Binomial1(N, P, First) Result(Ival)
      !---- Arguments ----!
      integer,       intent(in)  :: n       ! Number Of Bernoulli Trials       (1 <= Integer)
      real(kind=cp), intent(in)  :: p       ! Bernoulli Success Probability    (0 <= Real <= 1)
      logical,       intent(in)  :: first   ! .TRUE. for the first call using the current parameter values
                                            ! .FALSE. if the values of (n,p) are unchanged from last call
      integer                    :: ival

      !---- Local variables ----!
      integer                  :: ru, rd
      integer, save            :: r0
      real(kind=cp)            :: u, pd, pu
      real(kind=cp), save      :: odds_ratio, p_r

      !> Init
      Ival=0

      if (first) then
         r0 = (n+1)*p
         p_r=bin_prob(n, p, r0)
         odds_ratio = p / (one - p)
      end if

      call random_number(u)
      u = u - p_r
      if (u < zero) then
         ival = r0
         return
      end if

      pu = p_r
      ru = r0
      pd = p_r
      rd = r0
      do
         rd = rd - 1
         if (rd >= 0) then
            pd = pd * (rd+1) / (odds_ratio * (n-rd))
            u = u - pd
            if (u < zero) then
               ival = rd
               return
            end if
         end if

         ru = ru + 1
         if (ru <= n) then
            pu = pu * (n-ru+1) * odds_ratio / ru
            u = u - pu
            if (u < zero) then
               ival = ru
               return
            end if
         end if
      end do

      !> This point should not be reached, but just in case:
      ival = r0

   End Function Random_Binomial1

   !!----
   !!---- RANDOM_BINOMIAL2
   !!----
   !!----    Generates a single random deviate from a binomial distribution whose number
   !!----    of trials is N and whose probability of an event in each trial is P.
   !!----    Random_binomial2 <-- A random deviate yielding the number
   !!----    of events from N independent trials, each of which has a
   !!----    probability of event P.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Binomial2(N, Pp, First) Result(Ival)
      !---- Arguments ----!
      integer,       intent(in)    :: n        ! The number of trials in the binomial distribution from which a random deviate is to be generated
      real(kind=cp), intent(in)    :: pp       ! The probability of an event in each trial of the binomial distribution from which a random deviate
      logical,       intent(in)    :: first    ! .TRUE. for the first call to perform initialization. Set FIRST = .FALSE. for further calls using the
                                               ! same pair of parameter values (N, P)
      integer                      :: ival

      !---- local variables ----!
      real(kind=cp)            :: alv, amaxp, f, f1, f2, u, v, w, w2, x, x1, x2, ynorm, z, z2
      integer                  :: i, ix, ix1, k, mp
      integer, save            :: m
      real(kind=cp), save      :: p, q, xnp, ffm, fm, xnpq, p1, xm, xl, xr, c, al, xll,  &
                                  xlr, p2, p3, p4, qn, r, g

      !---- setup, perform only when parameters change
      if (first) then
         p = min(pp, one-pp)
         q = one - p
         xnp = n * p
      end if

      if (xnp > 30.0_cp) then
         if (first) then
            ffm = xnp + p
            m = ffm
            fm = m
            xnpq = xnp * q
            p1 = int(2.195_cp*sqrt(xnpq) - 4.6_cp*q) + half
            xm = fm + half
            xl = xm - p1
            xr = xm + p1
            c = 0.134_cp + 20.5_cp / (15.3_cp + fm)
            al = (ffm-xl) / (ffm - xl*p)
            xll = al * (one + half*al)
            al = (xr - ffm) / (xr*q)
            xlr = al * (one + half*al)
            p2 = p1 * (one + c + c)
            p3 = p2 + c / xll
            p4 = p3 + c / xlr
         end if

         !---- generate variate, binomial mean at least 30.
         do

            call random_number(u)
            u = u * p4
            call random_number(v)

            !---- triangular region
            if (u <= p1) then
               ix = xm - p1 * v + u
               if (pp > half) ix = n - ix
               ival = ix
               return
            end if

            !---- parallelogram region
            if (u <= p2) then
               x = xl + (u-p1) / c
               v = v * c + one - abs(xm-x) / p1
               if (v > one .or. v <= zero) cycle
               ix = x
            else
               !---- left tail
               if (u <= p3) then
                  ix = xl + log(v) / xll
                  if (ix < 0) cycle
                  v = v * (u-p2) * xll
               else
                  !---- right tail
                  ix = xr - log(v) / xlr
                  if (ix > n) cycle
                  v = v * (u-p3) * xlr
               end if
            end if

            !---- determine appropriate way to perform accept/reject test
            k = abs(ix-m)
            if (k <= 20 .or. k >= xnpq/2-1) then
               !---- explicit evaluation
               f = one
               r = p / q
               g = (n+1) * r
               if (m < ix) then
                  mp = m + 1
                  do i = mp, ix
                     f = f * (g/i-r)
                  end do
               else if (m > ix) then
                  ix1 = ix + 1
                  do i = ix1, m
                     f = f / (g/i-r)
                  end do
               end if

               if (v > f) then
                  cycle
               else
                  if (pp > half) ix = n - ix
                  ival = ix
                  return
               end if
            end if

            !---- squeezing using upper and lower bounds on log(f(x))
            amaxp = (k/xnpq) * ((k*(k/3.0 + 0.625_cp) + 0.1666666666666_cp)/xnpq + half)
            ynorm = -k * k / (2.0_cp*xnpq)
            alv = log(v)
            if (alv<ynorm - amaxp) then
               if (pp > half) ix = n - ix
               ival = ix
               return
            end if
            if (alv>ynorm + amaxp) cycle

            !---- stirling"s (actually de moivre"s) formula to machine accuracy
            !---- for the final acceptance/rejection test
            x1 = ix + 1
            f1 = fm + one
            z = n + 1 - fm
            w = n - ix + one
            z2 = z * z
            x2 = x1 * x1
            f2 = f1 * f1
            w2 = w * w
            if (alv - (xm*log(f1/x1) + (n-m+half)*log(z/w) + (ix-m)*log(w*p/(x1*q)) +    &
               (13860.0-(462.0-(132.0-(99.0-140.0/f2)/f2)/f2)/f2)/f1/166320.0 +               &
               (13860.0-(462.0-(132.0-(99.0-140.0/z2)/z2)/z2)/z2)/z/166320.0 +                &
               (13860.0-(462.0-(132.0-(99.0-140.0/x2)/x2)/x2)/x2)/x1/166320.0 +               &
               (13860.0-(462.0-(132.0-(99.0-140.0/w2)/w2)/w2)/w2)/w/166320.0) > zero) then
               cycle
            else
               if (pp > half) ix = n - ix
               ival = ix
               return
            end if
            exit
         end do

      else
         !---- inverse cdf logic for mean less than 30
         if (first) then
            qn = q ** n
            r = p / q
            g = r * (n+1)
         end if

         do
            ix = 0
            f = qn
            call random_number(u)
            do
               if (u >= f) then
                  if (ix > 110) exit
                  u = u - f
                  ix = ix + 1
                  f = f * (g/ix - r)
                  cycle
               end if
               exit
            end do
            if (ix > 110) cycle
            exit
         end do
      end if

      if (pp > half) ix = n - ix
      ival = ix

   End Function Random_Binomial2

   !!--++
   !!--++ SUBROUTINE BIN_PROB
   !!--++
   !!--++    (PRIVATE)
   !!--++    Calculate a binomial probability
   !!--++
   !!--++ Update: 11/07/2015
   !!
   Module Function Bin_Prob(N, P, R) Result(Fn_Val)
      !---- Arguments ----!
      integer, intent(in)         :: n, r
      real(kind=cp), intent(in)   :: p
      real(kind=cp)               :: fn_val

      !---- Local variable ----!
      real(kind=dp) :: n1,r1,nr1,rn1,rr1,rnr1

      n1=real(n+1)
      r1=real(r+1)
      nr1=real(n-r+1)
      rn1=lngamma(n1)
      rr1=lngamma(r1)
      rnr1=lngamma(nr1)
      fn_val = exp( rn1 - rr1 - rnr1 + r*log(p) + (n-r)*log(one - p) )

   End Function Bin_Prob

   !!--++
   !!--++ SUBROUTINE LNGAMMA
   !!--++
   !!--++    (PRIVATE)
   !!--++    Logarithm to base e of the gamma subroutine.
   !!--++     Accurate to about 1.e-14. (Alan Miller)
   !!--++
   !!--++ Update: 11/07/2015
   !!
   Module Function Lngamma(X) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=dp), intent(in) :: x
      real(kind=dp)             :: fn_val

      !---- Local variables ----!
      real(kind=dp) :: a1 = -4.166666666554424e-02,     &
                       a2 = 2.430554511376954e-03,      &
                       a3 = -7.685928044064347e-04,     &
                       a4 = 5.660478426014386e-04,      &
                       pi = 3.141592653589793,          &
                       lnrt2pi = 9.189385332046727e-1,  &
                       eps=tiny(1.0_dp),                   &
                       temp, arg, productt
      logical       :: reflect

      !---- lngamma is not defined if x = 0 or a negative integer.
      if (.not.(x > 0.0_dp .or. abs(x-real(int(x),dp)) > eps) ) then
         fn_val = 0.0_dp
         return
      end if

      !---- If x < 0, use the reflection formula:
      !----  gamma(x) * gamma(1-x) = pi * cosec(pi.x)
      reflect = (x < 0.0_dp)
      if (reflect) then
         arg = 1.0_dp - x
      else
         arg = x
      end if

      !---- Increase the argument, if necessary, to make it > 10.
      productt = 1.0_dp
      do
         if (arg <= 10.0_dp) then
            productt = productt * arg
            arg = arg + 1.0_dp
            cycle
         end if
         exit
      end do

      !---- Use a polynomial approximation to Stirling"s formula.
      !---- N.B. The real(kind=cp) Stirling"s formula is used here, not
      !---- the simpler, but less accurate formula given by De Moivre
      !---- in a letter to Stirling, which is the one usually quoted.
      arg = arg - 0.5_dp
      temp = 1.0_dp/arg**2
      fn_val = lnrt2pi + arg * (log(arg) - 1.0_dp  + &
               (((a4*temp + a3)*temp + a2)*temp + a1)*temp) - log(productt)
      if (reflect) then
         temp = sin(pi * x)
         fn_val = log(pi/temp) - fn_val
      end if

   End Function Lngamma

   !!----
   !!---- RANDOM_NEG_BINOMIAL
   !!----
   !!----    Generates a random negative binomial variate using unstored
   !!----    inversion and/or the reproductive property.
   !!----
   !!--..    the parameter h is set so that unstored inversion only is
   !!--..    used when p <= h, otherwise a combination of unstored
   !!--..    inversion and the reproductive property is used.
   !!----
   !!--..    adapted from fortran 77 code from the book:
   !!--..    dagpunar, j. "principles of random variate generation"
   !!--..    clarendon press, oxford, 1988.   isbn 0-19-852202-9
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Neg_Binomial(Sk, P) Result(Ival)
      !---- Arguments ----!
      real(kind=cp), intent(in)   :: sk   ! Number of failures required (dagpunar's words!). the "power" parameter of the negative binomial  (0 < real)
      real(kind=cp), intent(in)   :: p    ! bernoulli success probability  (0 < real(kind=cp) < 1)
      integer                     :: ival

      !---- Local variables ----!
      !---- the parameter uln = -log(machine"s smallest real(kind=cp) number).
      real(kind=cp), parameter    :: h = 0.7_cp
      real(kind=cp)               :: q, x, st, uln, v, r, s, y, g
      integer                     :: k, i, n

      !> Init
      Ival=0
      if (sk <= zero .or. p <= zero .or. p >= one) then
         err_cfml%ierr=2
         err_cfml%msg="RANDOM_NEG_BINOMIAL@RANDOM: Impermissible distribution parameter values"
         return
      end if
      q = one - p
      x = zero
      st = sk
      if (p > h) then
         v = one/log(p)
         k = st
         do i = 1,k
            do
               call random_number(r)
               if (r > zero) exit
            end do
            n = v*log(r)
            x = x + n
         end do
         st = st - k
      end if
      s = zero
      uln = -LOG(vsmall)
      if (st > -uln/log(q)) then
         err_cfml%ierr=1
         err_cfml%msg="RANDOM_NEG_BINOMIAL@RANDOM: P Is Too Large For This Value Of Sk"
         return
      end if
      y = q**st
      g = st
      call random_number(r)
      do
         if (y > r) exit
         r = r - y
         s = s + one
         y = y*p*g/s
         g = g + one
      end do
      ival = x + s + half

   End Function Random_Neg_Binomial

End Submodule Random_Binomial_Sm