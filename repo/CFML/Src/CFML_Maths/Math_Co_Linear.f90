!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_Co_Linear
 Contains

    !!----
    !!---- CO_LINEAR_C
    !!----    Determines if two complex vectors are co-linear
    !!----
    !!---- 28/03/2019
    !!
    Module Function Co_Linear_C(a,b,n) Result(co_linear)
       !---- Argument ----!
       complex(kind=cp), dimension(:), intent(in) :: a,b    ! Complex vectors
       integer,              optional, intent(in) :: n      ! Dimension of the vectors
       logical                                    :: co_linear

       !---- Local variables ----!
       integer          :: i,ia,ib,ndim
       complex(kind=cp) :: c

       !> Init
       ndim=size(a)
       if (present(n)) ndim=n

       co_linear=.true.

       do i=1,ndim
          if (abs(a(i)) > epss) then
             ia=i
             exit
          end if
       end do

       do i=1,ndim
          if (abs(b(i)) > epss) then
             ib=i
             exit
          end if
       end do

       if (ia /= ib) then
          co_linear=.false.
          return
       else
          c=a(ia)/b(ib)
          do i=1,ndim
             if (abs(a(i)-c*b(i)) > epss) then
                co_linear=.false.
                return
             end if
          end do
       end if

    End Function Co_Linear_C

    !!----
    !!---- CO_LINEAR_I
    !!----    Determines if two integer vectors are co-linear
    !!----
    !!---- 28/03/2019
    !!
    Module Function Co_Linear_I(a,b,n) Result(co_linear)
       !---- Argument ----!
       integer, dimension(:),           intent(in) :: a,b        ! Input vectors
       integer,               optional, intent(in) :: n          ! Dimension of the vector
       logical                                     :: co_linear

       !---- Local variables ----!
       integer       :: i,ia,ib,ndim
       real(kind=cp) :: c

       !> init
       ndim=size(a)
       if (present(n)) ndim=n

       co_linear=.true.

       do i=1,ndim
          if (abs(a(i)) > 0) then
             ia=i
             exit
          end if
       end do

       do i=1,ndim
          if (abs(b(i)) > 0) then
             ib=i
             exit
          end if
       end do

       if (ia /= ib) then
          co_linear=.false.

       else
          c=real(a(ia))/real(b(ib))
          do i=1,ndim
             if (abs( real(a(i))-c*real(b(i)) ) > epss) then
                co_linear=.false.
                return
             end if
          end do
       end if

    End Function Co_Linear_I

    !!----
    !!---- CO_LINEAR_R
    !!----    Determines if two real vectors are co-linear
    !!----
    !!---- 28/03/2019
    !!
    Module Function Co_Linear_R(a,b,n) Result(co_linear)
       !---- Argument ----!
       real(kind=cp), dimension(:),           intent(in) :: a,b        ! Input real vectors
       integer,                     optional, intent(in) :: n          ! Dimension of the vectors
       logical                                           :: co_linear

       !---- Local variables ----!
       integer       :: i,ia,ib,ndim
       real(kind=cp) :: c

       !> init
       ndim=size(a)
       if (present(n)) ndim=n

       co_linear=.true.

       do i=1,ndim
          if (abs(a(i)) > epss) then
             ia=i
             exit
          end if
       end do

       do i=1,ndim
          if (abs(b(i)) > epss) then
             ib=i
             exit
          end if
       end do

       if (ia /= ib) then
          co_linear=.false.
          return

       else
          c=a(ia)/b(ib)
          do i=1,ndim
             if (abs(a(i)-c*b(i)) > epss) then
                co_linear=.false.
                return
             end if
          end do
       end if

    End Function Co_Linear_R

End Submodule Math_Co_Linear
