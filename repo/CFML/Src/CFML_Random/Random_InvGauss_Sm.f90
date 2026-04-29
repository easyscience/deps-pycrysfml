!!----
!!----
!!----
SubModule (CFML_Random) Random_InvGauss_Sm
  implicit none
   Contains
   !!----
   !!---- RANDOM_INV_GAUSS
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate in [0,infinity] from
   !!----    a reparameterised generalised inverse gaussian (gig) distribution
   !!----    with density proportional to  gig**(h-1) * exp(-0.5*b*(gig+1/gig))
   !!----    using a ratio method.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Inv_Gauss(H, B, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)  :: h, b      ! parameter of distribution (0 <= real)
      logical,       intent(in)  :: first
      real(kind=cp)              :: fn_val

      !---- Local variables ----!
      real(kind=cp)            :: ym, xm, r, w, r1, r2, x
      real(kind=cp), save      :: a, c, d, e
      real(kind=cp), parameter :: quart = 0.25_cp

      !> Init
      Fn_val=0.0_cp
      if (h < zero .or. b <= zero) then
         err_cfml%Ierr=1
         err_cfml%msg="RANDOM_INV_GAUSS@RANDOM: Impermissible Distribution Parameter Values"
         return
      end if

      if (first) then                        ! initialization, if necessary
         if (h > quart*b*sqrt(vlarge)) then
            err_cfml%Ierr=1
            err_cfml%msg="RANDOM_INV_GAUSS@RANDOM: The Ratio H:B Is Too Small"
            return
         end if
         e = b*b
         d = h + one
         ym = (-d + sqrt(d*d + e))/b
         if (ym < vsmall) then
            err_cfml%Ierr=1
            err_cfml%msg="RANDOM_INV_GAUSS@RANDOM: The Value Of B Is Too Small"
            return
         end if

         d = h - one
         xm = (d + sqrt(d*d + e))/b
         d = half*d
         e = -quart*b
         r = xm + one/xm
         w = xm*ym
         a = w**(-half*h) * sqrt(xm/ym) * exp(-e*(r - ym - one/ym))
         if (a < vsmall) then
            err_cfml%ierr=1
            err_cfml%msg="RANDOM_INV_GAUSS@RANDOM: The Value Of H Is Too Large"
            return
         end if
         c = -d*log(xm) - e*r
      end if

      do
         call random_number(r1)
         if (r1 <= zero) cycle
         call random_number(r2)
         x = a*r2/r1
         if (x <= zero) cycle
         if (log(r1) < d*log(x) + e*(x + one/x) + c) exit
      end do

      fn_val = x

   End Function Random_Inv_Gauss

End SubModule Random_InvGauss_Sm