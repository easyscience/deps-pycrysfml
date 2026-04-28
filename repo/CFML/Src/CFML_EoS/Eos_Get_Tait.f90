!!----
!!----
!!----
SubModule (CFML_EoS) Eos_Get_Tait
   implicit none
   Contains

   !!--++
   !!--++ SUBROUTINE GET_TAIT
   !!--++
   !!--++ Returns a,b,c Tait parameters in a vector.
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Function Get_Tait(T, Eos) Result(Vec)
      !---- Arguments ----!
      real(kind=cp),             intent(in)  :: T    ! Temperature
      type(Eos_Type),            intent(in)  :: Eos  ! Eos Parameters
      real(kind=cp), dimension(3)            :: Vec  ! Vector (a,b,c) of Tait parameters

      !---- Local Variables ----!
      real(kind=cp) :: k0,kp,kpp

      !> Init
      Vec=0.0_cp
      if (eos%imodel /= 5) return

      select case(eos%itherm)
         case (1:5)                  ! normal thermal expansion models with dK/dT
            k0 =Get_K0_T(T,eos)
            kp =Get_Kp0_T(T,eos)

         case default               ! includes no thermal model, also pthermal which requires params at Tref
            k0 =eos%params(2)
            kp =eos%params(3)
      end select

      if (eos%iorder < 4) then
         kpp= -1.0_cp*kp/k0        ! implied value for Kpp except for 4th order
      else
         kpp=eos%params(4)
      end if

      if (eos%linear) then
         k0  =k0/3.0_cp
         kp  =kp/3.0_cp
         kpp =kpp/3.0_cp
      end if

      Vec(1)=(1.0_cp + kp)/(1.0_cp+kp+k0*kpp)
      Vec(2)= kp/k0
      if (abs(1_cp+kp) > tiny(0.0)) Vec(2)=Vec(2)-kpp/(1.0_cp+kp)

      Vec(3)= (1.0_cp+kp+k0*kpp)/(kp*kp+kp-k0*kpp)

   End Function Get_Tait

End SubModule Eos_Get_Tait
