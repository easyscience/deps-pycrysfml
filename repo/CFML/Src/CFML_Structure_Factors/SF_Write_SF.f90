Submodule (CFML_Structure_Factors) SF_Write_SF
   !---- Variables ----!
   implicit none

 Contains
   !!--++
   !!--++ SUBROUTINE WRITE_STRUCTURE_FACTORS
   !!--++
   !!--++    Writes in logical unit=lun the list of structure factors
   !!--++    contained in the array hkl
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Write_Structure_Factors_Crys(Reflex, Lun, Mode,iph)
      !---- Argument ----!
      class(RefList_Type),        intent(in) :: Reflex
      integer,                    intent(in) :: lun
      character(len=*), optional, intent(in) :: Mode
      integer,          optional, intent(in) :: iph

      !---- Local Variables ----!
      integer :: i,ir
      character(len=:), allocatable :: ph_info

      ir=1
      ph_info="                 "
      if(present(iph)) then
        ph_info=" FOR PHASE #     "
        write(ph_info(14:17),"(i3)") iph
      end if
      if (present(mode)) then
         select case (l_case(mode(1:3)))
            case ("nuc")
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NEUTRONS)"//ph_info
               write(unit=lun,fmt="(a)")     "    ==================================================="
               ir=2
            case ("ele")
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(ELECTRONS)"//ph_info
               write(unit=lun,fmt="(a)")     "    ===================================================="
               ir=3
            case default
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"//ph_info
               write(unit=lun,fmt="(a)")     "    ================================================="
         end Select

      else
         write(unit=lun,fmt="(a)")   "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"//ph_info
         write(unit=lun,fmt="(a)")   "    ================================================="
      end if

      write(unit=lun,fmt="(/,a,/)") &
           "   H   K   L   Mult    SinTh/Lda       dspc          |Fc|         Phase          F-Real        F-Imag       |Fc|^2      Num"

      select type (ref => reflex%ref)
         class is (SRefl_Type)
            do i=1,reflex%Nref
               write(unit=lun, fmt="(3i4,i5,6f14.5,f14.3,i8)") &
                               ref(i)%h, ref(i)%mult, ref(i)%S, 0.5/ref(i)%S, &
                               ref(i)%Fc(ir), ref(i)%Phase(ir), ref(i)%a(ir), ref(i)%b(ir), ref(i)%Fc(ir)*ref(i)%Fc(ir), i
            end do
      end select

   End Subroutine Write_Structure_Factors_Crys

   !!--++
   !!--++ SUBROUTINE WRITE_STRUCTURE_FACTORS_MAG
   !!--++
   !!--++    Writes in logical unit=lun the list of structure factors
   !!--++    calculated for a structure described using Shubnikov groups.
   !!--++
   !!--++    If "full" is present, full information contained in stf is output
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Write_Structure_Factors_Mag(Reflex, Stf, Lun, Full,iph)
      !---- Argument ----!
      class(RefList_Type),     intent(in) :: Reflex
      type(StrfList_Type),     intent(in) :: stf
      integer,                 intent(in) :: lun
      logical, optional,       intent(in) :: full
      integer, optional,       intent(in) :: iph

      !---- Local Variables ----!
      character(len=7),dimension(0:2), parameter :: RTYP=["  Nuc  ","  Mag  ","Nuc+Mag"]
      integer :: i
      character(len=:), allocatable :: ph_info

      ph_info="                 "
      if(present(iph)) then
        ph_info=" FOR PHASE #     "
        write(ph_info(14:17),"(i3)") iph
      end if

      write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NUCLEAR and MAGNETIC)"//ph_info
      write(unit=lun,fmt="(a)")     "    ==============================================================="

      write(unit=lun,fmt="(/,a,/)") "   H   K   L Mult SinTh/Lda d-spacing  ref-type       sqNuc       sqMiV  NumRef"
      select type (ref => reflex%ref)
         class is (Refl_Type)
            do i=1,reflex%Nref
               write(unit=lun,fmt="(3i4,i5,2f10.5,tr3,a,2f12.5,i8)") &
              ref(i)%h, ref(i)%mult, ref(i)%S, 0.5/ref(i)%S, RTYP(ref(i)%imag), &
              stf%strf(i)%sqNuc, stf%strf(i)%sqMiV,i
            end do
      end select

      if (present(full)) then
         write(unit=lun,fmt="(/,a,/)") &
                "   H   K   L    Nuc-Real    Nuc-Imag  (MsFx-Real  MsFx-Imag)  (MsFy-Real  MsFy-Imag)  (MsFz-Real  MsFz-Imag)  (MiVx-Real  MiVx-Imag)  (MiVy-Real  MiVy-Imag)  (MiVz-Real  MiVz-Imag)  NumRef"
         do i=1,reflex%Nref
            write(unit=lun,fmt="(3i4,14f12.5,i8)") &
                 reflex%ref(i)%h, stf%strf(i)%NsF, stf%strf(i)%MsF, &
                 stf%strf(i)%MiV, i
         end do
      else
      end if

   End Subroutine Write_Structure_Factors_Mag

End Submodule SF_Write_SF