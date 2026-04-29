!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_Trace
 implicit none
 Contains

    !!----
    !!---- TRACE_C
    !!----    Calculates the trace of a complex nxn array
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Trace_C(a) Result(b)
       !---- Argument ----!
       complex(kind=cp), dimension(:,:), intent(in) :: a
       complex(kind=cp)                             :: b

       !---- Local variables ----!
       integer :: i,imax

       b=(0.0,0.0)
       imax=min(size(a,1),size(a,2))
       do i=1,imax
          b=b+a(i,i)
       end do

    End Function Trace_C

    !!----
    !!---- TRACE_I
    !!----    Calculates the trace of an integer 3x3 array
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Trace_I(a) Result(b)
       !---- Argument ----!
       integer, dimension(:,:), intent(in) :: a
       integer                             :: b

       !---- Local variables ----!
       integer :: i,imax

       b=0
       imax=min(size(a,1),size(a,2))
       do i=1,imax
          b=b+a(i,i)
       end do

    End Function Trace_I

    !!----
    !!---- TRACE_R
    !!----    Calculates the trace of a real 3x3 array
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Trace_R(a) Result(b)
       !---- Argument ----!
       real(kind=cp), dimension(:,:), intent(in) :: a
       real(kind=cp)                             :: b

       !---- Local variables ----!
       integer :: i,imax

       b=0.0
       imax=min(size(a,1),size(a,2))
       do i=1,imax
          b=b+a(i,i)
       end do

    End Function Trace_R


End Submodule Math_Trace
