!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Assignment
 implicit none
 Contains
    !!----
    !!---- ASSIGN_RATIONAL_INT_LI
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Subroutine Assign_Rational_Int_LI(Res, I)
       !---- Arguments ----!
       type(rational),   intent (out) :: res  ! volatile
       integer(kind=LI), intent (in)  :: i

       res = i // 1_LI

    End Subroutine Assign_Rational_Int_LI

    !!----
    !!---- ASSIGN_RATIONAL_INT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Subroutine Assign_Rational_Int(Res, I)
       !---- Arguments ----!
       type(rational),  intent (out) :: res  ! volatile
       integer,         intent (in)  :: i

       res = i // 1

    End Subroutine Assign_Rational_Int

    !!----
    !!---- SUBROUTINE ASSIGN_RATIONAL_REAL_CP
    !!----
    !!----
    !!
    Elemental Module Subroutine Assign_Rational_Real_Cp(Res, Xr)
       !---- Arguments ----!
       type(rational), intent(out) :: res  ! volatile
       real(kind=cp),  intent (in) :: xr

       !---- Local variables ----!
       integer(kind=LI)                     :: maxden,ai,t,si
       integer(kind=LI), dimension(0:1,0:1) :: m
       real(kind=cp)                        :: x,rai,eps !,startx,er1,er2

       maxden=MAXIMUM_DENOMINATOR; eps=1.0e-6_cp
       m = 0; m(0,0)=1_LI; m(1,1)=1_LI
       si=sign(1.0,xr)
       x=abs(xr)

       do
          ai=int(x)
          if ( m(1,0)*ai+m(1,1) > maxden) exit
          t = m(0,0) * ai + m(0,1)
          m(0,1) = m(0,0); m(0,0) = t
          t = m(1,0) * ai + m(1,1)
          m(1,1) = m(1,0)
          m(1,0) =  t
          rai=real(ai,kind=cp)
          if ( abs(x - rai) < eps) exit !division by zero
          x = 1.0_cp/(x - rai)
       end do

       res= si*m(0,0)// m(1,0)

    End Subroutine Assign_Rational_Real_CP

    !!----
    !!---- ASSIGN_INT_RATIONAL
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Subroutine Assign_Int_Rational(I, Res)
       !---- Arguments ----!
       type(rational), intent (in)   :: res  !, volatile
       integer,        intent (out)  :: i

       i= nint(real(res%numerator,kind=cp)/real(res%denominator,kind=cp))

    End Subroutine Assign_Int_Rational

    !!----
    !!---- ASSIGN_INT_LI_RATIONAL
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Subroutine Assign_Int_LI_Rational(I, Res)
       !---- Arguments ----!
       type(rational),  intent (in)   :: res  !, volatile
       integer(kind=LI),intent (out)  :: i

       i= nint(real(res%numerator,kind=cp)/real(res%denominator,kind=cp))

    End Subroutine Assign_Int_LI_Rational

    !!----
    !!---- SUBROUTINE ASSIGN_REAL_RATIONAL_CP
    !!----
    !!----
    !!
    Elemental Module Subroutine Assign_Real_Rational_CP(X, Res)
       !---- Arguments ----!
       type(rational), intent(in)   :: res
       real(kind=cp),  intent (out) :: x

       x=real(res%numerator,kind=cp)/real(res%denominator,kind=cp)

    End Subroutine Assign_Real_Rational_CP

End Submodule RAT_Assignment