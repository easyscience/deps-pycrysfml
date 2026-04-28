!!----
!!---- SUBMODULE CFML_Maths
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Negligible
 implicit none
 Contains
    !!----
    !!---- NEGLIGIBLE_C
    !!----    Calculate if a complex number is negligible (abs < EPSS)
    !!----
    !!---- 27/03/2019
    !!
    Elemental Module Function Negligible_C(C) Result(Neglig)
       !---- Argument ----!
       complex(kind=cp), intent( in) :: C         ! Complex number
       logical                       :: Neglig

       !> Init
       Neglig=.false.
       if (abs(c) > epss) return

       Neglig=.true.

    End Function Negligible_C

    !!----
    !!---- NEGLIGIBLE_R
    !!----    Determines if a real number is negligible (abs < EPSS)
    !!----
    !!---- 27/03/2019
    !!
    Elemental Module Function Negligible_R(R) Result(neglig)
       !---- Argument ----!
       real(kind=cp), intent( in) :: R          ! Real number
       logical                    :: Neglig

       !> Init
       Neglig=.false.
       if (abs(R) > epss) return

       Neglig=.true.

    End Function Negligible_R

End Submodule Maths_Negligible
