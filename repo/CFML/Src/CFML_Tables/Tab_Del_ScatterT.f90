!!----
!!----
!!----
SubModule (CFML_Scattering_Tables) TAB_Del_ScatterT
   Implicit none
  Contains

   !!----
   !!---- REMOVE_CHEM_INFO
   !!----    Deallocate Chem_Info Table
   !!----
   !!---- 15/04/2019
   !!
   Module Subroutine Remove_Chem_Info()

      if (allocated(chem_info)) deallocate(chem_info)

   End Subroutine Remove_Chem_Info

   !!----
   !!---- REMOVE_DELTA_FP_FPP
   !!----    Deallocate Anomalous_ScFac Table
   !!----
   !!---- 15/04/2019
   !!
   Module Subroutine Remove_Delta_Fp_Fpp()

      if (allocated(Anomalous_ScFac)) deallocate(Anomalous_ScFac)

   End Subroutine Remove_Delta_Fp_Fpp

   !!----
   !!---- REMOVE_MAGNETIC_FORM
   !!----    Deallocate Magnetic_Form Table
   !!----
   !!---- 15/04/2019
   !!
   Module Subroutine Remove_Magnetic_Form()

      if (allocated(Magnetic_Form)) deallocate(Magnetic_Form)
      if (allocated(Magnetic_j2))   deallocate(Magnetic_j2)
      if (allocated(Magnetic_j4))   deallocate(Magnetic_j4)
      if (allocated(Magnetic_j6))   deallocate(Magnetic_j6)

   End Subroutine Remove_Magnetic_form

   !!----
   !!---- REMOVE_XRAY_FORM
   !!----    Deallocate Xray_Form Table
   !!----
   !!---- 15/04/2019
   !!
   Module Subroutine Remove_Xray_Form()

      if (allocated(Xray_Form)) deallocate(Xray_Form)

   End Subroutine Remove_Xray_form

End SubModule TAB_Del_ScatterT