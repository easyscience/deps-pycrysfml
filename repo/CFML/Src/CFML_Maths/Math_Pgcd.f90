!!----
!!---- SUBMODULE CFML_Maths
!!----
!!----
!!
Submodule (CFML_Maths) Math_Pgcd
 implicit none
 Contains
    !!----
    !!---- Function GCD(i,j) Result(mcd)
    !!----    integer, intent(in) :: i
    !!----    integer, intent(in) :: j
    !!----    integer             :: mcd
    !!----
    !!----    Function calculating the maximum common divisor of two integers
    !!----
    !!---- Update: February - 2005
    !!
    Elemental Module Function Gcd(a,b) Result(mcd)
       !---- Arguments ----!
       integer, intent(in) :: a,b
       integer             :: mcd

       !---- Local variables ----!
       integer  :: u,v,m

       u=max(a,b)
       v=min(a,b)
       do
          m=mod(u,v)
          if (m == 0) exit
          u=v
          v=m
       end do

       mcd=v

    End Function Gcd

    !!----
    !!---- Function LCM(i,j) result(mcm)
    !!----    integer, intent(in) :: i
    !!----    integer, intent(in) :: j
    !!----    integer             :: mcm
    !!----
    !!----    Function calculating the minimum common multiple of two integers
    !!----
    !!---- Update: February - 2005
    !!
    Elemental Module Function Lcm(a,b) result(mcm)
       !---- Arguments ----!
       integer, intent(in) :: a,b
       integer             :: mcm

       !---- Local variables ----!
       integer :: t

       !> Init
       mcm=0

       t=gcd(a,b)
       if (t /=0) mcm=(a*b)/t

    End Function Lcm

End Submodule Math_Pgcd

