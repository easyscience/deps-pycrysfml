!!----
!!---- Submodule
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Constructor
 implicit none
 Contains
    !!----
    !!---- FUNCTION MAKE_RATIONAL
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Make_Rational(Numerator, Denominator) Result(Res)
       !---- Arguments ----!
       integer(kind=li), intent (in) :: numerator
       integer(kind=li), intent (in) :: denominator
       type(rational)                :: res

       res = rational(numerator, denominator)

    End Function Make_Rational

    !!----
    !!---- FUNCTION MAKE_RATIONAL_INT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Make_Rational_Int(Numerator, Denominator) Result(Res)
       !---- Arguments ----!
       integer, intent (in) :: numerator
       integer, intent (in) :: denominator
       type(rational)       :: res

       res = rational(int(numerator,kind=li), int(denominator,kind=li))

    End Function Make_Rational_Int

End Submodule RAT_Constructor