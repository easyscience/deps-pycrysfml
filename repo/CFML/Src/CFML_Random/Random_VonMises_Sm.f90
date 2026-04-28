!!----
!!----
!!----
SubModule (CFML_Random) Random_VonMises_Sm
  implicit none
  Contains
   !!----
   !!---- RANDOM_VON_MISES
   !!----
   !!----    Von Mises Distribution
   !!----
   !!---- Update: 11/07/2015
   !!
   Module Function Random_Von_Mises(K, First) Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp), intent(in)  :: k         ! Parameter of the von Mises distribution
      logical,       intent(in)  :: first     ! set to .TRUE. the first time that the subroutine is called
      real(kind=cp)              :: fn_val

      !---- Local variables ----!
      integer                             :: j, n
      integer, save                       :: nk
      real(kind=cp), parameter            :: pi = 3.14159265_cp
      real(kind=cp), save,dimension(20)   :: p
      real(kind=cp), save,dimension(0:20) :: theta
      real(kind=cp)                       :: sump, r, th, lambda, rlast
      real(kind=dp)                       :: dk

      !> Init
      Fn_val=.0_cp
      if (first) then                        ! initialization, if necessary
         if (k < zero) then
            err_cfml%ierr=1
            err_cfml%msg="RANDOM_VON_MISES@RANDOM: Error in argument k for random_von_Mises"
            return
         end if
         nk = k + k + one
         if (nk > 20) then
            err_cfml%ierr=1
            err_cfml%msg="RANDOM_VON_MISES@RANDOM: Error in argument k for random_von_Mises"
            return
         end if
         dk = k
         theta(0) = zero
         if (k > half) then
            !---- set up array p of probabilities.
            sump = zero
            do j = 1, nk
               if (j < nk) then
                  theta(j) = acos(one - j/k)
               else
                  theta(nk) = pi
               end if

               !---- numerical integration of e^[k.cos(x)] from theta(j-1)
               !---- to theta(j)
               p(j)=integral(theta(j-1), theta(j), dk)
               sump = sump + p(j)
            end do ! j = 1, nk
            p(1:nk) = p(1:nk) / sump
         else
            p(1) = one
            theta(1) = pi
         end if                         ! if k > 0.5
      end if                           ! if first
      call random_number(r)
      do j = 1, nk
         r = r - p(j)
         if (r < zero) exit
      end do
      r = -r/p(j)

      do
         th = theta(j-1) + r*(theta(j) - theta(j-1))
         lambda = k - j + one - k*cos(th)
         n = 1
         rlast = lambda

         do
            call random_number(r)
            if (r > rlast) exit
            n = n + 1
            rlast = r
         end do
         if (n /= 2*(n/2)) exit         ! is n even?
         call random_number(r)
      end do

      fn_val = sign(th, (r - rlast)/(one - rlast) - half)

   End Function Random_Von_Mises

   !!--++
   !!--++ INTEGRAL
   !!--++
   !!--++    Gaussian integration of exp(k.cosx) from a to b.
   !!--++
   !!--++ 14/04/2019
   !!
   Module Function Integral(A, B, Dk) Result(Resultt)
      !---- Arguments ----!
      real(kind=cp), intent(in)      :: a, b
      real(kind=dp), intent(in)      :: dk
      real(kind=cp)                  :: resultt

      !---- Local variables ----!
      real(kind=dp)             :: xmid, rangee, x1, x2
      real(kind=dp),dimension(3):: x = (/0.238619186083197_dp, 0.661209386466265_dp, 0.932469514203152_dp/)
      real(kind=dp),dimension(3):: w = (/0.467913934572691_dp, 0.360761573048139_dp, 0.171324492379170_dp/)
      integer                   :: i

      xmid = (a + b)/2.0_dp
      rangee = (b - a)/2.0_dp
      resultt = 0.0_dp
      do i = 1, 3
         x1 = xmid + x(i)*rangee
         x2 = xmid - x(i)*rangee
         resultt = resultt + w(i)*(exp(dk*cos(x1)) + exp(dk*cos(x2)))
      end do

      resultt = resultt * rangee

   End Function Integral

End SubModule Random_VonMises_Sm