!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Operator_LT
 implicit none
 Contains
    !!----
    !!---- RATIONAL_LT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_LT(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       logical                     :: res

       !---- Local Variables ----!
       type(rational) :: r_simple, s_simple

       r_simple = rational_simplify(r)
       s_simple = rational_simplify(s)

       res = r_simple%numerator * s_simple%denominator < &
           & s_simple%numerator * r_simple%denominator

    End Function Rational_Lt

    !!----
    !!---- RATIONAL_INTEGER_LT
    !!----
    !!----
    !!
    Elemental Module Function Rational_Integer_LT(R, I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: r
       integer(kind=LI),intent (in) :: i
       logical                      :: res

       !---- Local Variables ----!
       type(rational) :: r_simple

       r_simple = rational_simplify(r)
       res = r_simple%numerator < i * r_simple%denominator

    End Function Rational_Integer_LT

    !!----
    !!---- INTEGER_RATIONAL_LT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_LT(I,R) Result (Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: r
       logical                      :: res

       !---- Local Variables ----!
       type(rational) :: r_simple

       r_simple = rational_simplify(r)
       res = i * r_simple%denominator < r_simple%numerator

    End Function Integer_Rational_Lt

End Submodule RAT_Operator_LT