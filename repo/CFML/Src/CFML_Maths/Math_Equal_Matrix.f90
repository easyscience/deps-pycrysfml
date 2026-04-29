!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Equal_Matrix
 implicit none
 Contains
    !!----
    !!---- EQUAL_MATRIX_C
    !!----    Determines if two complex arrays are equal in NxN
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Matrix_C(a,b,n) result(info)
       !---- Argument ----!
       complex(kind=cp), dimension(:,:),          intent(in) :: a,b      ! Input arrays NxN
       integer,                         optional, intent(in) :: n        ! Dimensions N
       logical                                               :: info

       !---- Local variables ----!
       integer       :: i,j,ndim,ndim2
       real(kind=cp) :: x,y

       !> init
       info=.false.

       !> Check on a
       ndim=size(a,dim=1)
       ndim2=size(a,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_C@MATHS: The A Matrix is not a square matrix NxN"
          return
       end if

       !> Check on b
       ndim=size(b,dim=1)
       ndim2=size(b,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_C@MATHS: The B Matrix is not a square matrix NxN"
          return
       end if

       !> Same shape
       ndim=size(a,dim=1)
       ndim2=size(b,dim=1)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_C@MATHS: The shape of A and B Matrix are different"
          return
       end if

       if (present(n)) then
          if (n > ndim) return
          ndim=n
       end if

       do i=1,ndim
          do j=1,ndim
             x= abs(real(a(i,j))-real(b(i,j)))
             y= abs(aimag(a(i,j))-aimag(b(i,j)))
             if (x > epss .or. y > epss) return
          end do
       end do
       info=.true.

    End Function Equal_Matrix_C

    !!----
    !!---- EQUAL_MATRIX_I
    !!----    Determines if two integer arrays are equal in NxN
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Matrix_I(a,b,n) result(info)
       !---- Argument ----!
       integer, dimension(:,:),          intent(in) :: a,b     ! Input arrays (NxN)
       integer,                optional, intent(in) :: n       ! Dimension of Arrays
       logical                                      :: info

       !---- Local variables ----!
       integer :: i,j,ndim,ndim2

       !> Init
       info=.false.

       !> Check on a
       ndim=size(a,dim=1)
       ndim2=size(a,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_I@MATHS: The A Matrix is not a square matrix NxN"
          return
       end if

       !> Check on b
       ndim=size(b,dim=1)
       ndim2=size(b,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_I@MATHS: The B Matrix is not a square matrix NxN"
          return
       end if

       !> Same shape
       ndim=size(a,dim=1)
       ndim2=size(b,dim=1)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_I@MATHS: The shape of A and B Matrix are different"
          return
       end if

       if (present(n)) then
          if (n > ndim) return
          ndim=n
       end if

       do i=1,ndim
          do j=1,ndim
             if (a(i,j) /= b(i,j)) return
          end do
       end do
       info=.true.

    End Function Equal_Matrix_I

    !!----
    !!---- EQUAL_MATRIX_R
    !!----    Determines if two real arrays are equal in NxN
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Matrix_R(a,b,n) result(info)
       !---- Argument ----!
       real(kind=cp), dimension(:,:),          intent(in) :: a,b      ! Input arrays NxN
       integer,                      optional, intent(in) :: n        ! Dimensions N
       logical                                            :: info

       !---- Local variables ----!
       integer :: i,j,ndim,ndim2

       !> init
       info=.false.

       !> Check on a
       ndim=size(a,dim=1)
       ndim2=size(a,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_R@MATHS: The A Matrix is not a square matrix NxN"
          return
       end if

       !> Check on b
       ndim=size(b,dim=1)
       ndim2=size(b,dim=2)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_R@MATHS: The B Matrix is not a square matrix NxN"
          return
       end if

       !> Same shape
       ndim=size(a,dim=1)
       ndim2=size(b,dim=1)
       if (ndim /= ndim2) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="EQUAL_MATRIX_R@MATHS: The shape of A and B Matrix are different"
          return
       end if

       if (present(n)) then
          if (n > ndim) return
          ndim=n
       end if

       do i=1,ndim
          do j=1,ndim
             if (abs(a(i,j) - b(i,j)) > epss ) return
          end do
       end do
       info=.true.

    End Function Equal_Matrix_R

End Submodule Maths_Equal_Matrix
