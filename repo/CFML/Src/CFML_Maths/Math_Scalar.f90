!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Scalar
 implicit none
 Contains

    !!----
    !!---- SCALAR_I
    !!----     Scalar Product including metrics
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Scalar_I(X,Y,G) Result(R)
       !---- Arguments ----!
       integer, dimension(:),         intent(in) :: x,y     ! Input vectors
       real(kind=cp), dimension(:,:), intent(in) :: g       ! Metrics
       real(kind=cp)                             :: r       ! Scalar

       if (size(x)/= size(y) .or. size(x)*size(x) /= size(g)) then
          r=tiny(0.0_cp)
       else
          r=dot_product(real(x), matmul(g,real(y)))
       end if

    End Function Scalar_I

    !!----
    !!---- SCALAR_R
    !!----    Scalar Product including metrics
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Scalar_R(X,Y,G) Result(R)
       !---- Arguments ----!
       real(kind=cp), dimension(:),   intent(in) :: x,y    ! Input vectors
       real(kind=cp), dimension(:,:), intent(in) :: g      ! Metrics
       real(kind=cp)                             :: r      ! Scalar

       if (size(x)/= size(y) .or. size(x)*size(x) /= size(g)) then
          r=tiny(0.0_cp)
       else
          r=dot_product(x, matmul(g,y))
       end if

    End Function Scalar_R


End Submodule Maths_Scalar
