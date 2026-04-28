!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Equal_Vector
 implicit none
 Contains

    !!----
    !!---- EQUAL_VECTOR_C
    !!----    Determines if two real(kind=sp) vectors are equal in N
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Vector_C(a,b,n) result(info)
       !---- Argument ----!
       complex(kind=cp), dimension(:), intent(in) :: a,b      ! Input vectors
       integer,              optional, intent(in) :: n        ! Dimension of the vector
       logical                                    :: info

       !---- Local variables ----!
       integer       :: i,ndim,ndim2
       real(kind=cp) :: x,y

       !> init
       info=.false.

       !> Check
       ndim=size(a)
       ndim2=size(b)
       if (ndim /= ndim2) then
          Err_CFML%IErr=1
          Err_CFML%Msg="MATHS@EQUAL_VECTOR_C: The size of A and B vector are different!"
          return
       end if

       if (present(n)) then
          ndim=min(ndim,n)
       end if
       ndim=max(ndim,1)

       do i=1,ndim
          x=abs(real(a(i))-real(b(i)))
          y=abs(aimag(a(i))-aimag(b(i)))
          if (x > epss .or. y > epss) return
       end do

       info=.true.

    End Function Equal_Vector_C

    !!----
    !!---- EQUAL_VECTOR_I
    !!----    Determines if two integer vectors are equal in N
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Vector_I(a,b,n) result(info)
       !---- Argument ----!
       integer, dimension(:),           intent(in) :: a,b    ! Input vectors
       integer,               optional, intent(in) :: n      ! Dimension of the vectors
       logical                                     :: info

       !---- Local variables ----!
       integer :: i,ndim,ndim2

       !> Init
       info=.false.

       !> Check
       ndim=size(a)
       ndim2=size(b)
       if (ndim /= ndim2) then
          Err_CFML%IErr=1
          Err_CFML%Msg="MATHS@EQUAL_VECTOR_I: The size of A and B vector are different!"
          return
       end if

       if (present(n)) then
          ndim=min(ndim,n)
       end if
       ndim=max(ndim,1)

       do i=1,ndim
          if (a(i) /= b(i)) return
       end do
       info=.true.

    End Function Equal_Vector_I

    !!----
    !!---- EQUAL_VECTOR_R
    !!----    Determines if two real(kind=sp) vectors are equal in N
    !!----
    !!---- 28/03/2019
    !!
    Module Function Equal_Vector_R(a,b,n) result(info)
       !---- Argument ----!
       real(kind=cp), dimension(:),           intent(in) :: a,b      ! Input vectors
       integer,                     optional, intent(in) :: n        ! Dimension of the vector
       logical                                           :: info

       !---- Local variables ----!
       integer :: i,ndim,ndim2

       !> init
       info=.false.

       !> Check
       ndim=size(a)
       ndim2=size(b)
       if (ndim /= ndim2) then
          Err_CFML%IErr=1
          Err_CFML%Msg="MATHS@EQUAL_VECTOR_R: The size of A and B vector are different!"
          return
       end if

       if (present(n)) then
          ndim=min(ndim,n)
       end if
       ndim=max(ndim,1)

       do i=1,ndim
          if (abs(a(i) - b(i)) > epss ) return
       end do

       info=.true.

    End Function Equal_Vector_R

End Submodule Maths_Equal_Vector
