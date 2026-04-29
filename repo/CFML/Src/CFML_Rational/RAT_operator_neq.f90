!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_operator_neq
 implicit none
 Contains
    !!----
    !!---- RATIONAL_NE
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_NE(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       logical                     :: res

       res = r%numerator * s%denominator /= s%numerator * r%denominator

    End Function Rational_NE


    !!----
    !!---- RATIONAL_INTEGER_NE_INTEGER
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_NE(R, I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: r
       integer(kind=LI),intent (in) :: i
       logical                      :: res

       res = r%numerator /= i * r%denominator

    End Function Rational_Integer_NE

    !!----
    !!---- FUNCTION INTEGER_RATIONAL_NE
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_NE(I, R) Result(Res)
       !---- Arguments ----!
       integer(kind=LI),intent (in) :: i
       type(rational),  intent (in) :: r
       logical                      :: res

       res = r%numerator /= i * r%denominator

    End Function Integer_Rational_NE

End Submodule RAT_operator_neq
