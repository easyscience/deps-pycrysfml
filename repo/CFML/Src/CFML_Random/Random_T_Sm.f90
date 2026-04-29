!!----
!!----
!!----
SubModule (CFML_Random) Random_T_Sm
  implicit none
   Contains
   !!----
   !!---- RANDOM_T
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!----
   !!----    Subroutine generates a random variate from a
   !!----    t distribution using kinderman and monahan"s ratio method.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_T(M) Result(Fn_Val)
      !---- Arguments ----!
      integer,      intent(in)  :: m        ! degrees of freedom of distribution (1 <= Integer)
      real(kind=cp)             :: fn_val

      !---- Local variables ----!
      real(kind=cp), save      :: s, c, a, f, g
      real(kind=cp)            :: r, x, v
      real(kind=cp), parameter :: three = 3.0_cp, four = 4.0_cp, quart = 0.25_cp,   &
                                  five = 5.0_cp, sixteen = 16.0_cp
      integer                  :: mm = 0

      !> Init
      Fn_val=0.0_cp
      if (m < 1) then
         err_cfml%ierr=1
         err_cfml%msg="RANDOM_T@RANDOM: Impermissible Degrees Of Freedom"
         return
      end if

      if (m /= mm) then                    ! initialization, if necessary
         s = m
         c = -quart*(s + one)
         a = four/(one + one/s)**c
         f = sixteen/a
         if (m > 1) then
            g = s - one
            g = ((s + one)/g)**c*sqrt((s+s)/g)
         else
            g = one
         end if
         mm = m
      end if

      do
         call random_number(r)
         if (r <= zero) cycle
         call random_number(v)
         x = (two*v - one)*g/r
         v = x*x
         if (v > five - a*r) then
            if (m >= 1 .and. r*(v + three) > f) cycle
            if (r > (one + v/s)**c) cycle
         end if
         exit
      end do

      fn_val = x

   End Function Random_T

End SubModule Random_T_Sm