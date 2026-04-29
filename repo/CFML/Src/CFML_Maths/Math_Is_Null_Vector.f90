!!----
!!---- SUBMODULE CFML_Maths
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Is_Null_Vector
 implicit none
 Contains
    !!----
    !!---- IS_NULL_VECTOR_I
    !!----    Determine if a vector is null
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Is_Null_Vector_I(V) Result(info)
       !---- Arguments ----!
       integer,  dimension(:), intent(in)  :: V
       logical                             :: Info

       !---- Local Variables ----!
       integer :: i

       info= .true.
       do i=1, size(v)
          if (v(i) /= 0) then
             info= .false.
             exit
          end if
       end do

    End Function Is_Null_Vector_I

    !!----
    !!---- IS_NULL_VECTOR_R
    !!----    Determine if a vector is null
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Is_Null_Vector_R(V) result(info)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in)  :: v
       logical                                  :: info

       !---- Local Variables ----!
       integer :: i

       info= .true.
       do i=1, size(v)
          if (abs(v(i)) > epsilon(1.0_cp)) then
             info= .false.
             exit
          end if
       end do

    End Function Is_Null_Vector_R

End Submodule Maths_Is_Null_Vector
