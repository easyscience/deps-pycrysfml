!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Lower_Triangular
 implicit none
 Contains
    !!----
    !!---- LOWER_TRIANGULAR_I
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Lower_Triangular_I(A,n) Result (T)
       !---- Argument ----!
       integer, dimension(:,:), intent(in) :: A    ! Input array
       integer,                 intent(in) :: n    ! Dimension of array
       integer, dimension(n,n)             :: T

       !---- Local arguments ----!
       integer :: i,j,p,q,m

       m=n
       p=size(A(:,1)); q=size(A(1,:))
       if (n > p .or. n > q) m=min(p,q)

       T=0
       do j=1,m
          do i=j,m
             T(i,j)=A(i,j)
          end do
       end do

    End Function Lower_Triangular_I

    !!----
    !!---- LOWER_TRIANGULAR_R
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Lower_Triangular_R(A,n) Result (T)
       !---- Argument ----!
       real(kind=cp), dimension(:,:), intent(in) :: A    ! Input Array
       integer,                       intent(in) :: n    ! Dimension of A
       real(kind=cp), dimension(n,n)             :: T

       !---- Local Variables ----!
       integer :: i,j,p,q,m


       m=n
       p=size(A(:,1)); q=size(A(1,:))
       if(n > p .or. n > q) m=min(p,q)

       T=0.0_cp
       do j=1,m
          do i=j,m
             T(i,j)=A(i,j)
          end do
       end do

    End Function Lower_Triangular_R


End Submodule Maths_Lower_Triangular
