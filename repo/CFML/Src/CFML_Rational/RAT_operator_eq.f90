!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Operator_EQ
 implicit none
 Contains
    !!----
    !!---- RATIONAL_EQ
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_EQ(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       logical                     :: res

       res = r%numerator * s%denominator == s%numerator * r%denominator

    End Function Rational_EQ

    !!----
    !!---- RATIONAL_INTEGER_EQ
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_EQ(R, I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: r
       integer(kind=LI),intent (in) :: i
       logical                      :: res

       res = r%denominator == 1_LI .and. r%numerator == i

    End Function Rational_Integer_EQ

    !!----
    !!---- INTEGER_RATIONAL_EQ
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_EQ(I, R) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: r
       logical                      :: res

       res = r%denominator == 1_LI .and. r%numerator == i

    End Function Integer_Rational_EQ

End Submodule RAT_Operator_EQ
