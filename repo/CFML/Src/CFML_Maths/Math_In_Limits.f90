!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_In_Limits
 implicit none
 Contains

    !!----
    !!---- IN_LIMITS_I
    !!----   Logical function that is true if all the components of the vector vect
    !!----   are within the limits:
    !!----          limits(1,i)  <= vect(i) <=  limits(2,i), for all i.
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function In_Limits_I(v,limits,n) result(ok)
       !---- Arguments ----!
       integer, dimension(:),             intent(in) :: v        ! Input Vector
       integer, dimension(:,:),           intent(in) :: limits   ! Normally (2,n)
       integer,                 optional, intent(in) :: n        ! Dimension of vect
       logical                                       :: ok

       !---- Local arguments ----!
       integer :: i,ndim,ndim2

       !> Init
       ndim=size(v)
       ndim2=size(limits,2)
       if (ndim /= ndim2) then
          ok=.false.
          return
       end if

       if (present(n)) then
          ndim=min(n,ndim)
       end if
       ndim=max(ndim,1)

       ok=.true.
       do i=1,ndim
          if (v(i) >= limits(1,i) .and. v(i) <= limits(2,i)) cycle
          ok=.false.
          exit
       end do

    End Function in_limits_i

    !!----
    !!---- IN_LIMITS_R
    !!----   Logical function that is true if all the components of the vector vect
    !!----   are within the limits:
    !!----          limits(1,i)  <= vect(i) <=  limits(2,i), for all i.
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function In_Limits_R(v,limits,n) result(ok)
       !---- Arguments ----!
       real(kind=cp), dimension(:),             intent(in) :: v        ! Input vector
       real(kind=cp), dimension(:,:),           intent(in) :: limits   ! Normally (2,n)
       integer,                       optional, intent(in) :: n        ! Dimension of Vect
       logical                                             :: ok

       !---- Local Variables ----!
       integer :: i,ndim,ndim2

       !> Init
       ndim=size(v)
       ndim2=size(limits,2)
       if (ndim /= ndim2) then
          ok=.false.
          return
       end if

       if (present(n)) then
          ndim=min(n,ndim)
       end if
       ndim=max(ndim,1)

       ok=.true.
       do i=1,ndim
          if (v(i) >= limits(1,i) .and. v(i) <= limits(2,i)) cycle
          ok=.false.
          exit
       end do

    End Function in_limits_R

End Submodule Maths_In_Limits
