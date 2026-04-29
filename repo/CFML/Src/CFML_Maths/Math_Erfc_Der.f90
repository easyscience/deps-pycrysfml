!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Erfc_Der
 implicit none
 Contains

    !!----
    !!---- Erfc_Deriv(X)
    !!----    Derivative of the complementary error function
    !!----
    !!---- 09/04/2019
    !!
    Elemental Module Function Erfc_Deriv(X) Result(Der)
       !---- Argument ----!
       real(kind=cp), intent(in)    :: x
       real(kind=cp)                :: der

       der = - 1.128379167096_cp * exp(-x*x)   !from Abramovitz

    End Function Erfc_Deriv

End Submodule Maths_Erfc_Der
