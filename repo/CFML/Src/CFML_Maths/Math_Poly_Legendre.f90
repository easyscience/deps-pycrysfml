!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_Poly_Legendre
 implicit none
 Contains

    !!----
    !!---- POLY_LEGENDRE
    !!----    Compute the Associated Legendre Polynomial Pml(x).
    !!----
    !!----    Here m (order) and l (degree) are integers satisfying
    !!----         0 <= m <= l
    !!----        -1 <= x <= 1.
    !!----
    !!---- 27/03/2019
    !!
    Elemental Module Function Poly_Legendre(L,M,X) Result(Plmx)
       !---- Arguments ----!
       integer,      intent (in) :: l
       integer,      intent (in) :: m
       real(kind=cp),intent (in) :: x
       real(kind=cp)             :: plmx

       !---- Local variables ----!
       integer       :: i, ll
       real(kind=cp) :: fact, pll, pmm, pmmp1, somx2


       !> Check limits
       if (m < 0 .or. m > l .or. abs(x) > 1.0) then
          plmx=0.0
          return
       end if

       !> Calculation
       pmm=1.0
       if (m > 0) then                  !Compute P(m,m)
          somx2=sqrt((1.0_cp-x)*(1.0_cp+x))
          fact=1.0_cp
          do i=1,m
             pmm=-pmm*fact*somx2
             fact=fact+2.0_cp
          end do
       end if

       if (l == m) then
          plmx=pmm
       else
          pmmp1=x*real(2*m+1,kind=cp)*pmm           !Compute P(m,m+1)
          if (l == m+1) then
             plmx=pmmp1                 !Compute P(m,L), L > m+1
          else
             do ll=m+2,l
                pll=(x*real(2*ll-1,kind=cp)*pmmp1-real(ll+m-1,kind=cp)*pmm)/real(ll-m,kind=cp)
                pmm=pmmp1
                pmmp1=pll
             end do
             plmx=pll
          end if
       end if

    End Function Poly_Legendre

End Submodule Math_Poly_Legendre
