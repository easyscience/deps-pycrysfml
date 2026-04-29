!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_operator_multiply
 implicit none
 Contains
    !!----
    !!---- RATIONAL_MULTIPLY
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Multiply(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       type(rational)              :: res

       res = r%numerator * s%numerator // (r%denominator * s%denominator)
       res=rational_simplify(res)

    End Function Rational_Multiply

    !!----
    !!---- INTEGER_RATIONAL_MULTIPLY
    !!----
    !!---- 08/04/2019s
    !!
    Elemental Module Function Integer_Rational_Multiply(I, S) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: s
       type(rational)               :: res

       res = i * s%numerator // s%denominator
       res=rational_simplify(res)

    End Function Integer_Rational_Multiply

    !!----
    !!---- RATIONAL_INTEGER_MULTIPLY
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_Multiply(S,I) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: s
       type(rational)               :: res

       res = i * s%numerator // s%denominator
       res=rational_simplify(res)

    End Function Rational_Integer_Multiply

End Submodule RAT_operator_multiply