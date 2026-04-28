!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Linear_Allowed
   implicit none
   Contains

   !!--++
   !!--++ LINEAR_EOS_ALLOWED_EOS
   !!--++
   !!--++ Date: 03/02/2021
   !!--++ Revision: OK
   !!
   Module Function Linear_EoS_Allowed_Eos(Eos) Result(Allowed)
      !---- Arguments ----!
      type(Eos_Type), intent(in) :: EoS     ! Eos Parameter
      logical                    :: allowed

      !> init
      allowed=.true.

      !> Model
      if (eos%imodel == 6) then
         allowed=.false.    !APL
         return
      end if

      !> Thermal model
      select case (eos%itherm)
         case (6:9)
            allowed=.false.
      end select

   End Function Linear_EoS_Allowed_Eos

   !!--++
   !!--++ LINEAR_EOS_ALLOWED_I
   !!--++
   !!--++ Date: 03/02/2021
   !!--++ Revision: OK
   !!
   Module Function Linear_EoS_Allowed_I(Imodel, Itherm) Result(Allowed)
      !---- Arguments ----!
      integer, optional, intent(in) :: imodel      !number of eos PV model
      integer, optional, intent(in) :: itherm      !number of thermal model
      logical                       :: allowed

      !---- Local Variables ----!

      !> init
      allowed=.true.

      !> Model
      if (present(imodel)) then
         if (imodel == 6) allowed=.false.    !APL
      end if

      !> Thermal model
      if (present(itherm)) then
         if (itherm == 6 .or. itherm == 7 .or. itherm == 8 .or. itherm == 9) allowed=.false.  !MGD & Einstein
      end if

   End Function Linear_EoS_Allowed_I

End SubModule EoS_Linear_Allowed