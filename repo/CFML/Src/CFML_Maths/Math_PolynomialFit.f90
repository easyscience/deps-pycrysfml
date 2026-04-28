!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_PolynomialFit
  implicit none

Contains
   !!----
   !!---- Function Polynomial_Fit
   !!----
   !!---- Determine the coefficients of a polynomial for a set of Data
   !!----
   !!---- 21/01/2021
   !!
   Pure Module Function Polynomial_Fit(X, Y, NPoints, Order) Result(Coeff)
      !---- Arguments ----!
      real(kind=cp), dimension(:), intent(in) :: X        ! X-Values
      real(kind=cp), dimension(:), intent(in) :: Y        ! Y-Values
      integer,                     intent(in) :: NPoints  ! Number of Points
      integer,                     intent(in) :: Order    ! Order of polynomial
      real(kind=cp), dimension(Order+1)       :: Coeff    ! Coefficients of the Polynomial

      !---- Local Variables ----!
      integer                                    :: n1, m, m1, m2, k, i, j, ij
      real(kind=cp), dimension(:), allocatable   :: Xc, Yx, B
      real(kind=cp), dimension(:,:), allocatable :: C
      real(kind=cp)                              :: Yc, s

      !> Init
      Coeff=0.0_cp

      !> Check
      if (NPoints < Order) return

      !> Start definitions
      n1=NPoints
      m=Order; m1=m+1; m2=2*m

      !> Allocating variables
      allocate(Xc(m2), Yx(m), B(m1), C(m1,m1))

      !> Load Xc
      Xc=0.0_cp
      do k=1, m2
         Xc(k)=sum(X(1:n1)**k)
      end do

      !> Load Yc
      Yc=0.0_cp
      Yc=sum(Y(1:n1))

      !> Yx
      Yx=0.0
      do k=1, m
         Yx(k)=sum(Y(1:n1)*X(1:n1)**k)
      end do

      !> Load C
      C=0.0_cp
      do i=1, m1
        do j=1, m1
            ij=i+j-2
            if (i==1 .and. j==1)  then
              c(1,1) = n1
            else
              c(i,j)=Xc(ij)
            end if
         end do
      end do

      !> Load B
      B=0.0_cp
      B(1)=yc
      do i=2, m1
         B(i)=Yx(i-1)
      end do

      do k=1, m
         do i=k+1, m1
            B(i) = B(i) - C(i,k)/C(k,k)*B(k)
            do j=k+1, m1
               C(i,j) = C(i,j) - C(i,k)/C(k,k)*C(k,j)
            end do
         end do
      end do

      Coeff(m1)=B(m1)/C(m1,m1)
      do i=m, 1, -1
         s=0.0_cp
         do k=i+1, m1
           s = s + C(i,k)*Coeff(k)
         end do
         Coeff(i) = (B(i)-s)/C(i,i)
      end do

   End Function Polynomial_Fit

End Submodule Math_PolynomialFit