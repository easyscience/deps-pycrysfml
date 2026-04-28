SubModule (CFML_gSpaceGroups) gS_Smallest_IntegralVec
   implicit none
   Contains

    !!----
    !!---- SMALLEST_INTEGRAL_VECTOR
    !!----
    !!---- Finds the smallest set of integers for the direction given by v.
    !!---- It assumes denominators in v have no prime numbers greater than 97.
    !!----
    !!---- 22/04/2019
    !!
    Module Subroutine Smallest_Integral_Vector(v)
       !---- Arguments ----!
       type(rational), dimension(:), intent(inout) :: v

       !---- Local variables ----!
       integer                :: i
       integer, dimension(25)             :: primos = [ 2, 3, 5, 7,11,13,17,19,23,29,&
                                                       31,37,41,43,47,53,59,61,67,71,&
                                                       73,79,83,89,97 ]
       type(rational), dimension(size(v)) :: vAux

       !> Init
       do i = 1 , size(v)
          v = v(i)%Denominator * v
       end do

       do i = 1 , 25
          vAux = v / (primos(i)//1)
          do
             if (.not. Rational_Is_Integer(vAux)) exit
             v    = vAux
             vAux = v / (primos(i)//1)
          end do
       end do
    End Subroutine Smallest_Integral_Vector

End Submodule gS_Smallest_IntegralVec