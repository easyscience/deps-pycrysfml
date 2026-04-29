!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Bulk
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_K
   !!----
   !!---- Returns the value of K (or M if linear) at P and T
   !!---- Works by using get_volume(P,T) and then using the V in k_cal
   !!----
   !!---- Date: 18/07/2016
   !!
   Module Function Get_K(P, T, Eos) Result(K)
      !---- Arguments ----!
      real(kind=cp), intent(in)    :: p    ! Pressure
      real(kind=cp), intent(in)    :: t    ! Tenperature
      type(Eos_Type),intent(in)    :: Eos  ! Eos Parameters
      real(kind=cp)                :: k

      !---- Local Variables ----!
      real(kind=cp) :: v

      v=get_volume(p,t,eos)
      k=k_cal(v,t,eos,p=p)

   End Function Get_K

   !!----
   !!---- FUNCTION GET_Kp
   !!----
   !!---- Returns the value of K (or M if linear) at P and T
   !!---- Works by using get_volume(P,T) and then using the V in k_cal
   !!----
   !!---- Date: 1/03/2018
   !!
   Module Function Get_Kp(P, T, Eos) Result(Kp)
      !---- Arguments ----!
      real(kind=cp),intent(in)        :: p    ! Pressure
      real(kind=cp),intent(in)        :: t    ! Tenperature
      type(Eos_Type),intent(in)       :: Eos  ! Eos Parameters
      real(kind=cp)                   :: kp

      !---- Local Variables ----!
      real(kind=cp) :: v

      v=get_volume(p,t,eos)
      kp=kp_cal(v,t,eos,p=p)

   End Function Get_Kp

   !!--++
   !!--++ FUNCTION GET_K0_T
   !!--++
   !!--++ Returns k0 needed for Eos calculations at T this means for Pthermal,
   !!--++ k at Tref is returned.
   !!--++ In the linear case then  M(T, P=0) from params is returned
   !!--++
   !!--++ If k is calculated as being negative, an error message is set
   !!--++ and k0 is returned as the value at Tref for safety.
   !!--++
   !!--++ Date:17/07/2015
   !!
   Module Function Get_K0_T(T, Eos) Result(k0)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: k0

      !---- Local Variables ----!
      real(kind=cp) :: vr

      !> Init
      k0=eos%params(2) !default (and correct if no thermal model)

      if (.not. eos%Pthermaleos)then
         select case(eos%icross)
            case(1)
               k0=eos%params(2)+eos%params(8)*(t-eos%tref)  !Old linear variation of K with T

            case(2)
               vr=eos%params(1)/Get_V0_T(T,Eos)          ! Get_V0_T returns a0 for linear
               if (eos%linear) vr=vr**3.0_cp
               if (vr > 0.001 .and. vr < huge(0.0_cp)) k0=eos%params(2)*vr**eos%params(8)   !Anderson Gruneisen approach using params(8) as delta
         end select
      end if

   End Function Get_K0_T

   !!--++
   !!--++ FUNCTION GET_KP0_T
   !!--++
   !!--++ Returns kp0 needed for Eos calculations at T this means for Pthermal,
   !!--++ kp at Tref is returned.
   !!--++
   !!--++ In the linear case then  Mp(T, P=0) from params is returned
   !!--++
   !!--++ Date:17/11/2016
   !!
   Module Function Get_Kp0_T(T, Eos) Result(kp0)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: kp0

      !---- Local Variables ----!
      real(kind=cp) :: vr

      !> Init
      kp0=eos%params(3) !default (and correct if no thermal model)

      if (.not. eos%Pthermaleos)then
         select case(eos%icross)
            case(1)  !Old linear variation of K with T, no Kp variation

            case(2)
               vr=Get_V0_T(T,Eos)/eos%params(1)            !normally Vr > 0, but if negative thermal expansion, not
               if (eos%linear) vr=vr**3.0_cp
               if (vr > 0.001 .and. vr < huge(0._cp) ) kp0=eos%params(3)*vr**eos%params(9)   ! params(9) is delta-prime
         end select
      end if

   End Function Get_Kp0_T

   !!--++
   !!--++ FUNCTION GET_KPP0_T
   !!--++
   !!--++ Returns kpp0 needed for Eos calculations at T this means for pthermal,
   !!--++ kpp at Tref is returned.
   !!--++
   !!--++ In the linear case then  Mp(T, P=0) from params is returned
   !!--++
   !!--++ Date:17/11/2016
   !!
   Module Function Get_Kpp0_T(T, Eos) Result(kpp0)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: kpp0

      !---- Local Variables ----!
      type(Eos_Type) :: eost     ! workspace

      !> Init
      kpp0=eos%params(4) !default (and correct if no thermal model, or if Pthermal model)

      select case(eos%itherm)
         case(1:5)  ! Normal isothermal eos at T
            eost=eos           ! Modify eost to hold K0 and Kp at the T of interest

            eost%params(2)=Get_K0_T(T,Eos)
            eost%params(3)=Get_Kp0_T(T,Eos)
            call set_eos_implied_values(Eost)

            kpp0=eost%params(4)
      end select

   End Function Get_Kpp0_T

End SubModule EoS_Get_Bulk