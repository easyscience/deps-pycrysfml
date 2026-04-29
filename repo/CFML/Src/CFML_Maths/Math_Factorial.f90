!!----
!!---- SUBMODULE CFML_Maths
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Factorial
 implicit none
 Contains
    !!----
    !!---- FACTORIAL_I
    !!----    Factorial of N.  The value returned is an integer
    !!----
    !!---- 27/03/2019
    !!
    Elemental Module Function Factorial_I(N) Result(fact)
       !---- Argument ----!
       integer, intent(in) :: N        ! Factorial of N
       integer             :: Fact

       !---- Local variables ----!
       integer, parameter :: N_LIM = 12
       integer            :: i

       !> Init
       fact=0

       !> Check the current limits
       if ( n > N_LIM) return

       if (n == 0) then
          fact=1
       else
          fact=1
          do i=1,n
             fact=fact*i
          end do
       end if

    End Function Factorial_I

    !!----
    !!---- FACTORIAL_R
    !!----    Factorial of N.
    !!----
    !!---- 27/03/2019
    !!
    Elemental Module Function Factorial_R(N) Result(Fact)
       !---- Arguments ----!
       integer,        intent(in) :: N      ! Factorial of N
       real(kind=cp)              :: Fact

       !---- Local Variables ----!
       integer :: i

       !> Init
       Fact=0.0_cp

       if (n == 0) then
          Fact = 1.0_cp
       else
          Fact=1.0_cp
          do i=1,n
             Fact = Fact * real(i,kind=cp)
          end do
       end if

    End Function Factorial_R

End Submodule Maths_Factorial
