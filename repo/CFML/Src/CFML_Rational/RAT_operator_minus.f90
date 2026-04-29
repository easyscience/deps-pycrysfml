!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Operator_Minus
 implicit none
 Contains
    !!----
    !!---- RATIONAL_MINUS
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Minus(R) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational)              :: res

       res = - r%numerator // r%denominator
       res=rational_simplify(res)

    End Function Rational_Minus

    !!----
    !!---- RATIONAL_SUBTRACT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Subtract(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       type(rational)              :: res

       res = r%numerator * s%denominator - r%denominator * s%numerator // &
           & r%denominator * s%denominator
       res=rational_simplify(res)

    End Function Rational_Subtract

    !!----
    !!---- INTEGER_RATIONAL_SUBTRACT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_Subtract(I, S) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: s
       type(rational)               :: res

       res = (i * s%denominator -  s%numerator) //  s%denominator
       res=rational_simplify(res)

    End Function Integer_Rational_Subtract

    !!----
    !!---- RATIONAL_INTEGER_SUBTRACT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_Subtract(S,I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: s
       integer(kind=LI),intent (in) :: i
       type(rational)               :: res

       res = (s%numerator - i * s%denominator) //  s%denominator
       res=rational_simplify(res)

    End Function Rational_Integer_Subtract

End Submodule RAT_Operator_Minus