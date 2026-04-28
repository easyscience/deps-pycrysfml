!!----
!!----
!!----
SubModule (CFML_EoS) EoS_GPT_Crun
   implicit none

   Contains

   !!----
   !!---- FUNCTION GET_GPT
   !!----
   !!---- Obtain the value of G (or Glinear) at P and T
   !!----
   !!---- Date: 11/07/2016
   !!
   Module Function Get_GPT(P, T, EoS) Result(gpt)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P   ! Pressure
      real(kind=cp),  intent(in) :: T   ! Temperature
      type(Eos_Type), intent(in) :: Eos ! EoS Variable
      real(kind=cp)              :: gpt

      !---- Local Variables ----!
      integer         :: i
      real(kind=cp)   :: delp

      !> default
      gpt=eos%params(30)          ! default is g(Pref,Tref)

      !> T variation
      select case(eos%ishear)       ! choice of model
         case(1)                    ! model 1 is polynomial in P and T
            gpt=gpt+(t-eos%tref)*eos%params(34)
            delp=p-eos%pref
            do i=1,3
               if (eos%params(i+30) < tiny(0.0)) exit
               gpt=gpt+eos%params(i+30)*delp**i      ! eg linear term is (P-Pref)*dG/dP with params(31)
            end do

      end select

   End Function Get_GPT

   !!----
   !!---- FUNCTION  GET_GRUN_PT
   !!----
   !!---- Returns Gruneisen parameter at this P,T as gamma0*(V/V0)
   !!----
   !!---- Date: 18/07/2016
   !!
   Module Function Get_Grun_PT(P, T, Eos, i) Result(G)
      !---- Arguments ----!
      real(kind=cp),     intent(in) :: P    ! Pressure
      real(kind=cp),     intent(in) :: T    ! Temperarture
      type(Eos_Type),    intent(in) :: EoS  ! Eos Parameter
      integer, optional, intent(in) :: i   ! which oscillator
      real(kind=cp)                 :: g

      !---- Local Variables ----!
      real(kind=cp) :: v

      v=get_volume(P,T,eos)
      G=Get_Grun_V(v,Eos,i)

   End Function Get_Grun_PT

   !!----
   !!---- FUNCTION  GET_GRUN_Th
   !!----
   !!---- Returns thermal Gruneisen parameter at this volume
   !!----
   !!---- Date: 26/05/2020 Restructured from previous version of 13/05/2020 for special cases
   !!
   Module Function Get_Grun_Th(P, T, Eos)  Result(G)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P,T
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: g

      !---- Local Variables ----!
      integer              :: i
      integer,dimension(1) :: ii
      real(kind=cp)        :: v,sumc,sumb,sumt,cvf,cv
      real(kind=cp),dimension(0:2) :: Tc,Cvi

      !> init
      G=0.0_cp

      if (eos%imodel == -1) then !ptv table does not support Grueneisen
         call set_error(-1,'EoS from PTV table does not support Grueneisen parameters')
         return
      end if

      v=get_volume(P,T,eos)

      !> No extra oscillators
      if (sum(eos%IOsc) == 0 .or. eos%itran > 0)then
         G=Get_Grun_V(v,Eos)
         return
      end if

      !> get Cv
      Cvi=0._cp
      do i=0,2
         Cvi(i)=Get_Cv(P, T, Eos,i)
      end do
      Cv=Cvi(0)+Cvi(1)+Cvi(2)

      if (Cv > tiny(0._cp))then
         ! normal case at finite T
         sumc=0._cp
         do i=0,2
            sumc=sumc+Get_Grun_V(v,Eos,i)*Get_Cv(P, T, Eos,i)
         end do
         G=sumc/cv

      else
         ! here for Cv = 0 : Either all einstein at low T or T=0
         if (eos%itherm == 7 .or. eos%iosc(1) == 1 .or. eos%iosc(2) == 1)then
            !At least one Debye: only Debye contribute to gamma_th
            sumb=0._cp
            sumt=0._cp

            !main oscillator
            if (eos%itherm == 7)then
               cvf=(1._cp-eos%params(40)-eos%params(45))/(Get_DebyeT(V, Eos,0)**3._cp)
               sumb=cvf
               sumt=cvf*Get_Grun_V(v,Eos,0)
            end if

            !extra osc1
            if (eos%iosc(1) == 1)then
               cvf=eos%params(40)/(Get_DebyeT(V, Eos,1)**3._cp)
               sumb=sumb+cvf
               sumt=sumt+cvf*Get_Grun_V(v,Eos,1)
            end if

            !extra osc2
            if (eos%iosc(2) == 1)then
               cvf=eos%params(45)/(Get_DebyeT(V, Eos,2)**3._cp)
               sumb=sumb+cvf
               sumt=sumt+cvf*Get_Grun_V(v,Eos,2)
            end if
            G=sumt/sumb
         else
            !All Einstein at very low or zero T
            tc=huge(0._cp)
            tc(0)=Get_DebyeT(V, Eos,0)
            if (eos%iosc(1) == 2)tc(1)=Get_DebyeT(V, Eos,1)
            if (eos%iosc(2) == 2)tc(2)=Get_DebyeT(V, Eos,2)
            ii=minloc(tc)
            i=ii(1)-1   !minloc returns absolute location, not its label
            g=Get_Grun_V(v,Eos,i)
         end if
      end if

   End Function Get_Grun_Th

   !!----
   !!---- FUNCTION  GET_GRUN_V
   !!----
   !!---- Returns Gruneisen parameter at this volume as gamma0*(V/V0) for one mode only
   !!---- For the thermal Grueneisen parameter use Get_Grun_Th
   !!---- If linear it calculates gamma0*(a/a0)^3
   !!----
   !!---- Date: 18/07/2016
   !!
   Module Function Get_Grun_V(V, Eos, i) Result(Grun)
      !---- Arguments ----!
      real(kind=cp),     intent(in) :: V    ! Volume or length if linear
      type(Eos_Type),    intent(in) :: EoS  ! Eos Parameter
      integer, optional, intent(in) :: i    ! which oscillator
      real(kind=cp) :: Grun                 !The resulting gruneisen gamma

      !---- Local Variables ----!
      integer       :: io
      real(kind=cp) :: VV0,q

      !>local copy of pointer to oscillator
      io=0
      if (present(i))then
         if (i > 0 .and. i <= N_OSC_MODELS)io=i
      end if

      !Init
      grun=0.0_cp

      if (eos%imodel == -1)then !ptv table does not support Grueneisen
         call set_error(-1,'EoS from PTV table does not support Grueneisen parameters')
         return
      end if

      !> Must be careful with transitions because eospar%params(1) is the high phase V0
      !> V0=get_volume(eospar%pref,eospar%tref,eospar) (Nov 2016)
      !> no I don't think so. Grun is a property of the high phase

      VV0=V/eos%params(1)
      if (eos%linear) VV0=VV0**3.0_cp

      if (io == 0)then                    !main thermal model

         if (eos%itherm >= 6 .and. eos%itherm <= 8  .and. eos%params(14) > 0.5_cp )then
            !q-compromise: gamma/V is constant: MGD and Einstein are allowed q-comp, Holland-Powell is always q-comp
            Grun=eos%params(18)*VV0
         else
            !Normal
            if (abs(eos%params(19)) > 0.00001_cp) then
               VV0=VV0**eos%params(19)
            else
               VV0=1.0_cp
            end if
            Grun=eos%params(18)*VV0
         end if

      else if(eos%iosc(io) > 0) then          !additonal Debye or Einstein oscillator
         if (eos%params(39+5*io) > 0.5_cp)then
            Grun=eos%params(37+5*io)*VV0

         else
            !Normal
            q =eos%params(38+5*io)      ! 43 or 48
            if (abs(q) > tiny(0._cp))then
               VV0=VV0**q
            else
               VV0=1.0_cp
            end if
            Grun=eos%params(37+5*io)*VV0
         end if
      end if

   End Function Get_Grun_V


End SubModule EoS_GPT_Crun