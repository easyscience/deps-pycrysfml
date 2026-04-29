!!----
!!----
!!----
SubModule (CFML_Random) Random_Normal_Sm
  implicit none
   Contains
   !!----
   !!---- RANDOM_NORMAL
   !!----
   !!--..    Adapted from the following Fortran 77 code ALGORITHM 712,
   !!--..    Collected Algorithms From Acm.
   !!--..    This Work Published In Transactions On Mathematical Software,
   !!--..    Vol. 18, No. 4, December, 1992, Pp. 434-435.
   !!----
   !!----    The subroutine random_normal() returns a normally distributed
   !!----    pseudo-random number with zero mean and unit variance.
   !!----    The algorithm uses the ratio of uniforms method of A.J. Kinderman
   !!----    and J.F. Monahan augmented with quadratic bounding curves.
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Normal() Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp) :: fn_val

      !---- Local variables ----!
      real(kind=cp) :: s = 0.449871_cp, t = -0.386595_cp, &
                       a = 0.19600_cp,  b = 0.25472_cp,   &
                      r1 = 0.27597_cp, r2 = 0.27846_cp,   &
                      u, v, x, y, q

      !> Init
      Fn_val=0.0_cp

      !---- Generate P = (u,v) uniform in rectangle enclosing ----!
      !---- acceptance region                                 ----!
      do
         call random_number(u)
         call random_number(v)
         v = 1.7156_cp * (v - half)

         !---- Evaluate the quadratic form ----!
         x = u - s
         y = abs(v) - t
         q = x**2 + y*(a*y - b*x)

         !---- Accept P if inside inner ellipse ----!
         if (q < r1) exit

         !---- Reject P if outside outer ellipse ----!
         if (q > r2) cycle

         !---- Reject P if outside acceptance region ----!
         if (v**2 < -4.0*log(u)*u**2) exit
      end do

      !---- Return ratio of P"s coordinates as the normal deviate ----!
      fn_val = v/u

   End Function Random_Normal

   !!----
   !!---- RANDOM_MVNORM
   !!----
   !!----    Generates an n variate random normal vector using
   !!----    a cholesky decomposition.
   !!----
   !!--..    Adapted from Fortran 77 code from the book:
   !!--..    Dagpunar, J. "Principles of random variate generation"
   !!--..    Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
   !!--..
   !!---- 14/04/2019
   !!
   Module Subroutine Random_Mvnorm(N, H, D, First, F, X)
      !---- Arguments ----!
      integer,                            intent(in) :: n       ! number of variates in vector (input,integer >= 1)
      real(kind=cp), dimension(n),        intent(in) :: h       ! h(n) vector of means
      real(kind=cp), dimension(n*(n+1)/2),intent(in) :: d       ! d(n*(n+1)/2) variance matrix (j> = i)
      real(kind=cp), dimension(n*(n+1)/2),intent(out):: f       ! f(n*(n+1)/2) lower triangular decomposition of variance matrix (j <= i)
      real(kind=cp), dimension(n),        intent(out):: x       ! x(n) delivered vector
      logical,                            intent(in) :: first   ! .true. if this is the first call of the routine
                                                                ! or if the distribution has changed since the last
                                                                ! call of the routine. otherwise set to .false.

      !---- Local variables ----!
      integer       :: j, i, m
      real(kind=cp) :: y, v
      integer, save :: n2

      !> Init
      f=0.0_cp
      x=0.0_cp
      if (n < 1) then
         err_cfml%Ierr=1
         err_cfml%msg="RANDOM_MVNORM@RANDOM: Wrong size of Vector"
         return
      end if

      if (first) then                        ! initialization, if necessary
         n2 = 2*n
         if (d(1) < zero) then
            err_cfml%Ierr=1
            err_cfml%msg="RANDOM_MVNORM@RANDOM: Variance is Non Positive"
            return
         end if
         f(1) = sqrt(d(1))
         y = one/f(1)
         do j = 2,n
            f(j) = d(1+j*(j-1)/2) * y
         end do

         do i = 2,n
            v = d(i*(i-1)/2+i)
            do m = 1,i-1
               v = v - f((m-1)*(n2-m)/2+i)**2
            end do
            if (v < zero) then
               err_cfml%Ierr=1
               err_cfml%msg="RANDOM_MVNORM@RANDOM: Variance is Non Positive"
               return
            end if
            v = sqrt(v)
            y = one/v
            f((i-1)*(n2-i)/2+i) = v
            do j = i+1,n
               v = d(j*(j-1)/2+i)
               do m = 1,i-1
                  v = v - f((m-1)*(n2-m)/2+i)*f((m-1)*(n2-m)/2 + j)
               end do ! m = 1,i-1
               f((i-1)*(n2-i)/2 + j) = v*y
            end do ! j = i+1,n
         end do ! i = 2,n
      end if

      x(1:n) = h(1:n)
      do j = 1,n
         y=random_normal()
         do i = j,n
            x(i) = x(i) + f((j-1)*(n2-j)/2 + i) * y
         end do ! i = j,n
      end do ! j = 1,n

   End Subroutine Random_Mvnorm

End Submodule Random_Normal_Sm