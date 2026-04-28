!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Upper_Triangular
 implicit none
 Contains

    !!----
    !!---- UPPER_TRIANGULAR_I
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Upper_Triangular_I(A,n) Result (T)
       !---- Argument ----!
       integer, dimension(:,:), intent(in) :: A     ! Input array
       integer,                 intent(in) :: n     ! Dimension
       integer, dimension(n,n)             :: T

       !---- Local Variables ----!
       integer :: i,j,p,q,m

       m=n
       p=size(A(:,1)); q=size(A(1,:))
       if(n > p .or. n > q) m=min(p,q)

       T=0
       do j=1,m
          do i=1,j
             T(i,j)=A(i,j)
          end do
       end do

    End Function Upper_Triangular_I

    !!----
    !!---- UPPER_TRIANGULAR_R
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Upper_Triangular_R(A,n) Result (T)
       !---- Argument ----!
       real(kind=cp), dimension(:,:), intent(in) :: A   ! Input array
       integer,                       intent(in) :: n   ! Dimension
       real(kind=cp), dimension(n,n)             :: T

       !---- Local Variables ----!
       integer :: i,j,p,q,m

       m=n
       p=size(A(:,1)); q=size(A(1,:))
       if(n > p .or. n > q) m=min(p,q)

       T=0.0_cp
       do j=1,m
          do i=1,j
             T(i,j)=A(i,j)
          end do
       end do

    End Function  Upper_Triangular_R


End Submodule Maths_Upper_Triangular
