!!----
!!----
!!----
SubModule (CFML_Random) Random_Beta_Sm
  implicit none
   Contains
   !!----
   !!---- RANDOM_BETA
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate in [0,1] from a beta distribution with
   !!----    density proportional to beta**(aa-1) * (1-beta)**(bb-1) using cheng"s log
   !!----    logistic method.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Beta(Aa, Bb, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)    :: aa, bb   ! shape parameter from distribution (0 < real)
      logical,       intent(in)    :: first
      real(kind=cp)                :: fn_val

      !---- Local variables ----!
      real(kind=cp), parameter  :: aln4 = 1.3862944_cp
      real(kind=cp)             :: a, b, g, r, s, x, y, z
      real(kind=cp), save       :: d, f, h, t, c
      logical,       save       :: swap

      !> Init
      fn_val=0.0_cp
      if (aa <= zero .or. bb <= zero) then
         err_cfml%Ierr=1
         err_cfml%msg="RANDOM_BETA@RANDOM: Impermissible Shape Parameter Value(s)"
         return
      end if

      if (first) then                        ! initialization, if necessary
         a = aa
         b = bb
         swap = b > a
         if (swap) then
            g = b
            b = a
            a = g
         end if
         d = a/b
         f = a+b
         if (b > one) then
            h = sqrt((two*a*b - f)/(f - two))
            t = one
         else
            h = b
            t = one/(one + (a/(vlarge*b))**b)
         end if
         c = a+h
      end if

      do
         call random_number(r)
         call random_number(x)
         s = r*r*x
         if (r < vsmall .or. s <= zero) cycle
         if (r < t) then
            x = log(r/(one - r))/h
            y = d*exp(x)
            z = c*x + f*log((one + d)/(one + y)) - aln4
            if (s - one > z) then
               if (s - s*z > one) cycle
               if (log(s) > z) cycle
            end if
            fn_val = y/(one + y)
         else
            if (4.0*s > (one + one/d)**f) cycle
            fn_val = one
         end if
         exit
      end do

      if (swap) fn_val = one - fn_val

   End Function Random_Beta

End SubModule Random_Beta_Sm