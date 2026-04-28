!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Operator_Divisor
 implicit none
 Contains
    !!----
    !!---- RATIONAL_DIVIDE
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Divide(R, S) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational), intent (in) :: s
       type(rational)              :: res

       !---- Local Variables ----!
       integer(kind=LI) :: denom

       denom = r%denominator * s%numerator
       if (denom /= 0_LI) then
          res = r%numerator * s%denominator // denom
          res=rational_simplify(res)
       else
          res=0_LI//1_LI
       end if

    End Function Rational_Divide

    !!----
    !!---- RATIONAL_INTEGER_DIVIDE
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_Divide(R, I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: r
       integer(kind=LI),intent (in) :: i
       type(rational)               :: res

       if (i /= 0_LI) then
          res = r%numerator // (r%numerator*i)
          res=rational_simplify(res)
       else
          res=0_LI//1_LI
       end if

    End Function Rational_Integer_Divide

    !!----
    !!---- INTEGER_RATIONAL_DIVIDE
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Integer_Rational_Divide(I, R) Result(Res)
       !---- Arguments ----!
       integer(kind=LI), intent(in) :: I
       type (rational),  intent(in) :: r
       type (rational)              :: res

       if (r /= 0_LI/1_LI) then
          res= r%denominator // (r%numerator * i)
          res=rational_simplify(res)
       else
          res=0_LI//1_LI
       end if

    End Function Integer_Rational_Divide

End Submodule RAT_Operator_Divisor