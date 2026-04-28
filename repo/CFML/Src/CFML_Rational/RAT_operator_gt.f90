!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Operator_GT
 implicit none
 Contains
    !!----
    !!---- RATIONAL_GT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_GT(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       logical                     :: res

       !---- Local Variables ----!
       type(rational) :: r_simple, s_simple

       r_simple = rational_simplify(r)
       s_simple = rational_simplify(s)

       res = r_simple%numerator * s_simple%denominator > &
           & s_simple%numerator * r_simple%denominator

    End Function Rational_GT

    !!----
    !!---- RATIONAL_INTEGER_GT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_GT(R, I) Result(Res)
       !---- Arguments ----!
       type(rational),   intent (in) :: r
       integer(kind=LI), intent (in) :: i
       logical                       :: res

       !---- Local Variables ----!
       type(rational) :: r_simple

       r_simple = rational_simplify(r)

       res = r_simple%numerator > i * r_simple%denominator

    End Function Rational_Integer_GT

    !!----
    !!---- INTEGER_RATIONAL_GT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_GT(I, R) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: r
       logical                      :: res

       !---- Local Variables ----!
       type(rational) :: r_simple

       r_simple = rational_simplify(r)
       res = i * r_simple%denominator > r_simple%numerator

    End Function Integer_Rational_GT

End Submodule RAT_Operator_GT

