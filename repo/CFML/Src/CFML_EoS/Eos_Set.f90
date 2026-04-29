!!----
!!----
!!----
SubModule (CFML_Eos) EoS_Set
   implicit none
   Contains

   !!--++
   !!--++ SUBROUTINE SET_CROSS_NAMES
   !!--++
   !!--++ Set the character variables in eos_type data structures for Cross-terms
   !!--++
   !!--++ Date: 11/11/2016
   !!
   Module Subroutine Set_Cross_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS  ! EoS object

      !---- Local Variables ----!
      character(len=50) :: ptext

      !> Check for valid model number. If not valid, set zero
      if (EoS%icross < 0 .or. EoS%icross > N_CROSS_MODELS) EoS%icross=0

      EoS%cmodel=crossmodel_names(EoS%icross)

      select case(EoS%icross)
         case (0)
            EoS%parname(8:9) = ' '

         case (1)
            if (len_trim(EoS%vscale_name) > 0)then
               ptext='units are '//trim(EoS%pscale_name)//'/K'
            else
               ptext='units are P units/K'
            end if
            if (EoS%linear)then
               EoS%parname(8) = 'dM/dT'
               EoS%comment(8) = 'dM/dT '//trim(ptext)
            else
               EoS%parname(8) = 'dK/dT'//trim(ptext)
               EoS%comment(8) = 'dK/dT '//trim(ptext)
            end if

         case (2)
            EoS%parname(8) = 'delta'
            EoS%comment(8) = 'Anderson delta_T, without units'
            EoS%parname(9) = 'delPr'
            EoS%comment(9) = 'delta_prime for Kprime power law, without units'
      end select

   End Subroutine Set_Cross_Names

   !!--++
   !!--++ SUBROUTINE SET_EOS_FACTORS
   !!--++
   !!--++ Initialize the EoS Factors without change in the parameters values
   !!--++
   !!--++ Date: 17/02/2015
   !!
   Module Subroutine Set_EoS_Factors(Eos)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: Eos

      !> Init
      EoS%factor=1.0
      EoS%alphafactor=1.0E5_cp

      select case(EoS%itherm)
         case (-1)       ! pvt table
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply alpha values on printing

         case (0)
            EoS%factor(10:19) = 1.0_cp

         case (1)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E8_cp

         case (2)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E8_cp
            EoS%factor(12)  = 1.0_cp

         case (3)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E4_cp

         case (4)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp

         case (5)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp

         case (6)
            EoS%factor(10)  = 1.0E5_cp                ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp

      end select

      select case(EoS%itran)
         case (1:3)
            EoS%factor(20:n_eospar) = 1.0_cp
            EoS%factor(24)          = 1.0E3_cp         ! 1000 for aL
            EoS%factor(26)          = 1.0E3_cp         ! 1000 for aH
      end select

      return
   End Subroutine Set_Eos_Factors

   !!----
   !!---- SUBROUTINE SET_EOS_NAMES
   !!----
   !!---- Set the character variables in eos_type data structures
   !!---- to match the flags already set
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Set_Eos_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS   ! EoS object

      !---- Local Variables ----!
      character(len=50),dimension(5)  :: ptext    ! local variable to hold name of pressure scale

      !> Check for valid model number. If not valid, set zero
      if (EoS%imodel < -1 .or. EoS%imodel > N_PRESS_MODELS) EoS%imodel=0

      !> Set the Eos name
      EoS%model=Pmodel_names(EoS%imodel)
      if (EoS%imodel <= 0) return

      !> set the comments for parameters for volume or linear eos
      !> set the pressure scale text first
      ptext=' '
      if (len_trim(EoS%pscale_name) > 0) then
         ptext(2)='units are '//trim(EoS%pscale_name)
         ptext(4)='units are inverse '//trim(EoS%pscale_name)

      else
         ptext(2)='same units as pressure data'
         ptext(4)='inverse pressure units'
      end if

      !> Set the volume/linear scale name
      if (len_trim(EoS%vscale_name) > 0) then
         ptext(1)='units are '//trim(EoS%vscale_name)
      else
         if (.not. EoS%linear) then
            ptext(1)='units as volume data'
         else
            ptext(1)='units as length data'
         end if
      end if

      if (.not. EoS%linear) then
         EoS%ParName(1:4) =(/'V0   ','K0   ','Kp   ','Kpp  '/)

         EoS%comment(1) = 'Reference pressure volume: '//trim(ptext(1))
         EoS%comment(2) = 'Bulk modulus: '//trim(ptext(2))
         EoS%comment(3) = 'dK/dP: dimensionless'
         EoS%comment(4) = 'd2K/dP2: '//trim(ptext(4))
         EoS%LinearDir  = ' '

         select case(EoS%imodel)
            case (6)                !APL
               EoS%ParName(5) = 'Z   '
               EoS%comment(5) = 'N(electrons) in V0'
         end select

      else
         EoS%ParName(1:4) =(/'L0   ','M0   ','Mp   ','Mpp  '/)

         EoS%comment(1) = 'Reference pressure length: '//trim(ptext(1))
         EoS%comment(2) = 'Linear modulus: '//trim(ptext(2))
         EoS%comment(3) = 'dM/dP: dimensionless'
         EoS%comment(4) = 'd2M/dP2: '//trim(ptext(4))

         select case(EoS%imodel)
            case (6)
               EoS%ParName(5) = 'Z   '
               EoS%comment(5) = 'N(electrons) in V0'

         end select

      end if

      !> Thermal models are only set in init_EoS_thermal

      !>Adjustl all scale names so that they can be compared in index
      EoS%pscale_name=trim(adjustl(EoS%pscale_name))
      EoS%vscale_name=trim(adjustl(EoS%vscale_name))

   End Subroutine Set_Eos_Names

      !!----
   !!---- SUBROUTINE SET_EOS_USE
   !!----
   !!---- sets the 'use' flags for Eos type based on all current settings
   !!----
   !!----    Iuse     Comments
   !!----   ----------------------------------------------------------------------------
   !!----      0      parameter not used
   !!----      1      parameter is used, settable, refineable
   !!----      2      parameter is used and/or should be reported, settable, but cannot be refined
   !!----      3      parameter is used and/or should be reported, not settable, cannot be refined
   !!----             (includes implied values)
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Set_EoS_Use(Eos)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: Eos  ! EoS object

      !---- Local Variables ----!
      integer                     :: i
      integer,dimension(N_EOSPAR) :: useflags       ! local copy: useful for avoiding some resets

      !> Init
      useflags=EoS%iuse
      EoS%iuse=0
      EoS%allowed_orders=.true.

      !> EoS Model
      select case(EoS%imodel)
         case (0)
            EoS%iuse(1)=1                                    ! None eg thermal only
            EoS%allowed_orders=.false.

         case (1,7)
            EoS%iuse(1:3)=1                                  ! Murnaghan, Kumar
            EoS%allowed_orders(2)=.false.
            EoS%allowed_orders(4)=.false.

         case (2,4,5)                                        ! other isothermal EoS
            EoS%iuse(1:EoS%iorder)=1
            if (EoS%iorder < 4) EoS%iuse(EoS%iorder+1:4)=3   ! implied values

         case (3)                                            ! Vinet
            EoS%iuse(1:EoS%iorder)=1
            if (EoS%iorder < 4) EoS%iuse(EoS%iorder+1:4)=3   ! implied values
            EoS%allowed_orders(4)=.false.

         case (6)                                            ! APL only
            EoS%iuse(1:EoS%iorder)=1
            if (EoS%iorder < 4) EoS%iuse(EoS%iorder+1:4)=3   ! implied values
            EoS%iuse(5)=2

      end select

      !> Thermal Model
      select case(EoS%itherm)
         case (1)             ! Berman
            EoS%iuse(10:11)=1 ! alpha terms
            EoS%iuse(18)=2    ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=2    ! Grunesien q power law parameter
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (2)             ! Fei
            EoS%iuse(10:12)=1 ! alpha terms
            EoS%iuse(18)=2    ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=2    ! Grunesien q power law parameter
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (3)             ! HP 1998
            EoS%iuse(10:11)=1 ! alpha terms
            EoS%iuse(18)=2
            EoS%iuse(19)=2    ! Grunesien q power law parameter
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (4)             ! Holland-Powell thermal expansion, in Kroll form
            if (EoS%imodel ==0) EoS%iuse(3)=2  ! require Kprime_zero but not stable in refinement if no P data (added 27/01/2014 RJA)
            EoS%iuse(10)=1    ! alpha at Tref
            EoS%iuse(11)=1    ! Einstein T set refineable 2 Sept 2020
            EoS%iuse(18)=2    ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=2    ! Grunesien q power law parameter
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (5)             ! Salje
            EoS%iuse(10:11)=1
            EoS%iuse(18)=2    ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=2    ! Grunesien q power law parameter
            EoS%TRef_fixed   = .true.
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (6)             ! Thermal pressure in H&P form (no dK/dT): requires a eos model as well
            if (EoS%imodel==0) EoS%iuse(2:4)=2   ! K, Kp refinement is not stable without a pressure model
            EoS%iuse(8:9)=0   ! No dK/dT parameter:
            EoS%iuse(10)=1    ! alpha at Tref
            EoS%iuse(11)=1    ! Einstein T
            EoS%iuse(13)=2    ! Natoms per formula unit
            EoS%iuse(14)=0    ! Flag for q-compromise: does not appear to user
            if (PscaleMGD(eos) .and. VscaleMGD(eos)) then
               EoS%iuse(18)=3    ! Grunesien parameter at Pref,Tref. This is implied by alpha0

            else
               EoS%iuse(18)=2    ! If the pscale and vscale are not as required, make free parameter for Ks Kt conversion
            end if
            EoS%iuse(19)=0    ! Grunesien q power law parameter: this is a q-comp model
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  =.true.
            EoS%Osc_allowed  =.false.

         case (7,8)           ! Thermal pressure in MGD or Einstein form,
            if (EoS%imodel==0) EoS%iuse(2:4)=2   ! K, Kp refinement is not stable without a pressure model
            EoS%iuse(8:9)=0   ! No dK/dT parameter:

            EoS%iuse(11)=1    ! Debye T

            EoS%iuse(13)=2    ! Natoms per formula unit
            EoS%iuse(14)=0    ! Flag for q-compromise: does not appear to user
            EoS%iuse(18)=1    ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=1    ! Grunesien q power law parameter
            if (EoS%params(14) > 0.5_cp) then
               EoS%params(19)=0._cp
               EoS%iuse(19)=0
            end if

            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  = .true.
            EoS%osc_allowed  = .true.

         case(9)
            if (EoS%imodel==0) EoS%iuse(2:4)=2   ! K, Kp refinement is not stable without a pressure model
            EoS%iuse(8:9)=0     ! No dK/dT parameter
            EoS%iuse(10:17)=0   ! No other paramters except gamma0 and q
            EoS%iuse(18)=1      ! Grunesien parameter at Pref,Tref
            EoS%iuse(19)=1      ! Grunesien q power law parameter
            EoS%TRef_fixed   = .false.
            EoS%pthermaleos  = .true.
            EoS%osc_allowed  = .false.

      end select

      !> Phase transition model
      select case(EoS%itran)
         case (1,2)     ! Landau PV or TV
            EoS%iuse(20)=2           !settable, no refine: sense of transition,
            EoS%iuse(21)=1           !settable, allow refine: Ptr or Ttr
            EoS%iuse(24)=1           !settable, allow refine: aL
            EoS%iuse(25)=1           !settable, allow refine: betaL
            EoS%iuse(26)=1           !settable, allow refine: aH
            EoS%iuse(27)=1           !settable, allow refine: betaH

         case (3)     ! Landau PVT
            EoS%iuse(20:22)=2        !settable, no refine: sense of transition, T(tr), dT(Tr)/dP
            EoS%iuse(24)=1           !settable, allow refine: aL,
            EoS%iuse(23)=2           !settable, fixed d2Tr/dP2
            EoS%iuse(25:27)=1        !settable, allow refine:  betaL,aH,betaH
      end select

      !> Shear model
      select case(EoS%ishear)
         case (0)
            EoS%iuse(30)=0            ! No model, G0 set very large

         case (1)
            EoS%iuse(30:34)=2         ! Polynomial model: settable, not refineable
      end select

      !> Cross terms model
      if (EoS%pthermaleos) then
         EoS%icross=0                ! Kill the cross-terms
         EoS%iuse(8:9)=0
         EoS%params(8:9)=0.0

      else
         select case(EoS%icross)
            case (0)
               EoS%iuse(8)=0

            case(1)
               EoS%iuse(8)=1

            case(2)
               EoS%iuse(8)=1
               EoS%iuse(9)=2  !settable, no refine: del-prime refinement always unstable
         end select
      end if

      !> Additional oscillators
      EoS%iuse(40:49)=0
      if (EoS%iosc(1) > 0) then
         EoS%iuse(40:43)=1
         if (EoS%params(44) > 0.5_cp)EoS%iuse(43)=0
      end if

      if (EoS%iosc(2) > 0) then
         EoS%iuse(45:48)=1
         if (EoS%params(49) > 0.5_cp)EoS%iuse(48)=0
      end if

      !> Use flags for data scales: leave them unchanged, because we do not have the dataset
      EoS%iuse(50:59)=useflags(50:59)

      !> Set the refine flags to be consistent with the use flags
      do i=1,N_EOSPAR
         if (EoS%iuse(i) /=1) EoS%iref(i)=0
      end do

   End Subroutine Set_Eos_Use

   !!--++
   !!--++ SUBROUTINE SET_OSC_NAMES
   !!--++
   !!--++ Set the character variables in eos_type data structures for 2nd oscillators
   !!--++
   !!--++ Date: 09/03/2020
   !!
   Module Subroutine Set_Osc_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS  ! EoS object

      !---- Local Variables ----!
      integer :: i,n0,n1

      !> Check for valid model number. If not valid, set zero
      if (EoS%iosc(1) < 0 .or. EoS%iosc(1) > N_OSC_MODELS) EoS%iosc(1)=0
      if (EoS%iosc(2) < 0 .or. EoS%iosc(2) > N_OSC_MODELS) EoS%iosc(2)=0

      !> Set the name
      EoS%oscmodel(1)=oscmodel_names(EoS%iosc(1))
      EoS%oscmodel(2)=oscmodel_names(EoS%iosc(2))

      !> Set all the parameter names again, no harm in doing so

      do i=1,2
          n0=35+5*i
          n1=39+5*i

          select case(EoS%iosc(i))
             case (0)
                EoS%parname(n0:n1) = ' '
                EoS%comment(n0:n1) = ' '

             case (1)
                EoS%parname(n0)  ='mfrac'
                EoS%parname(n0+1)='ThD'
                EoS%parname(n0+2)='gamma'
                EoS%parname(n0+3)='q'
                EoS%parname(n0+4) ='qcomp'
                EoS%comment(n0)='Fraction of modes with this oscillator'
                EoS%comment(n0+1)='Debye Temperature in K'
                EoS%comment(n0+2)='Gruenesien mode gamma for this oscillator'
                EoS%comment(n0+3)='Gruneisen power law in V/V0  for this oscillator'
                EoS%comment(n0+4)='Switch for q-compromise model, +1 for compromise'

             case (2)
                EoS%parname(n0)  ='mfrac'
                EoS%parname(n0+1)='Th_E '
                EoS%parname(n0+2)='gamma'
                EoS%parname(n0+3)='q    '
                EoS%parname(n0+4) ='qcomp'
                EoS%comment(n0)='Fraction of modes with this oscillator'
                EoS%comment(n0+1)='Einstein Temperature in K'
                EoS%comment(n0+2)='Gruenesien mode gamma for this oscillator'
                EoS%comment(n0+3)='Gruneisen power law in V/V0  for this oscillator'
                EoS%comment(n0+4)='Switch for q-compromise model, +1 for compromise'

          end select
      end do

   End Subroutine Set_Osc_Names

   !!----
   !!---- SUBROUTINE SET_EOS_IMPLIED_VALUES
   !!----
   !!---- Fix Kp and Kpp values from Model and Order of EoSpar
   !!---- And other implied values
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Set_Eos_Implied_Values(Eos)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: Eos  ! EoS object

      !---- Local Variables ----!
      real(kind=cp),dimension(N_EOSPAR):: ev           ! local copies of room pressure parameter values
      real(kind=cp)                    :: pfg0,c0,c2   ! variables for APL
      real(kind=cp)                    :: Cvref, y      ! variables used for HP thermal p

      !> Local copy
      ev= EoS_to_Vec(eos) !  ev contains volume-like parameters

      select case (EoS%imodel)
         case (1) ! Murnaghan
            ev(4)=0._cp     !clear Kpp if set
            ev(5)=0._cp     !clear Z if set

         case (2) ! Birch-Murnaghan
            if (EoS%iorder == 2) ev(3)=4.0_cp
            if (EoS%iorder == 2 .or. EoS%iorder == 3) then
               if (abs(ev(2)) > 0.0) ev(4)=-1.0_cp*((ev(3)-4.0_cp)*(ev(3)-3.0_cp)+35.0_cp/9.0_cp)/ev(2)  !for order 2 and 3
            end if
            ev(5)=0._cp     !clear Z if set

         case (3) ! Vinet
            if (EoS%iorder == 2) ev(3)=1.0_cp
            if (EoS%iorder == 2 .or. EoS%iorder == 3) then
               if (abs(ev(2)) > 0.0) ev(4)=-1.0_cp*((0.5_cp*ev(3))**2+0.5*ev(3)-19.0_cp/36.0_cp)/ev(2) !for order 2 and 3
            end if
            ev(5)=0._cp     !clear Z if set

         case (4) ! Natural
            if (EoS%iorder == 2) ev(3)=2.0_cp
            if (EoS%iorder == 2 .or. EoS%iorder == 3) then
               if (abs(ev(2)) > 0.0) ev(4)=-1.0_cp*(1.0_cp + (ev(3)-2.0_cp)+(ev(3)-2.0_cp)**2.0_cp)/ev(2) !for order 2 and 3
            end if
            ev(5)=0._cp     !clear Z if set

         case (5) ! Tait with definitions of order derived from Holland and Powell (2011)
            if (EoS%iorder == 2) ev(3)=4.0_cp
            if (EoS%iorder == 2 .or. EoS%iorder == 3)then
               if (abs(ev(2)) > 0.0)ev(4)=-1.0_cp*ev(3)/ev(2)
            end if
            ev(5)=0._cp     !clear Z if set

         case (6) ! APL
            pFG0=AFERMIGAS*(ev(5)/ev(1))**1.66666667_cp
            c0=-1.0_cp*log(3.0_cp*ev(2)/pFG0)           ! assumes V in A^3
            if (EoS%iorder == 2)ev(3)=3.0_cp+2.0_cp*c0/3.0_cp

            if (EoS%iorder < 4)then
               c2=1.5_cp*(ev(3)-3.0_cp)-c0
               ev(4)=(20._cp + 12._cp*c0 + c0*c0 + 2.0_cp*c2*(9.0_cp+c0) + 4.0_cp*c2*c2)
               ev(4)=-1.0_cp*ev(4)/9._cp/ev(2)
            end if

         case(7) ! Kumar
            if (EoS%iorder == 2)ev(3)=4.0_cp
            ev(5)=0._cp     !clear Z if set

      end select

      !> Thermal models
      select case(eos%itherm)
         case(6)        ! Holland-Powell thermal pressure. Set gamma if pscales and vscales appropriate
             if (pscaleMGD(eos) .and. vscaleMGD(eos)) then
                !We cannot use get_cv to calculate the CV at this point, because that uses gamma0
                y=eos%params(11)/eos%tref  !ThetaE/Tref
                Cvref=3.0_cp*eos%params(13)*8.314_cp * y**2._cp * exp(y)/(exp(y)-1)**2._cp
                eos%params(18)=eos%params(1)*eos%params(10)*eos%params(2)/Cvref
                eos%params(18)=eos%params(18)/EPthermal_factor(Eos)
             end if
      end select

      !> Handle linear or volume
      if (.not. EoS%linear) then
         if (EoS%iorder == 2) EoS%params(3)=ev(3)
         if (EoS%iorder == 2 .or. EoS%iorder == 3) EoS%params(4)=ev(4)

      else
         if (EoS%iorder == 2) EoS%params(3)=ev(3)*3.0_cp
         if (EoS%iorder == 2 .or. EoS%iorder == 3) EoS%params(4)=ev(4)*3.0_cp
      end if

   End Subroutine Set_Eos_Implied_Values

   !!--++
   !!--++ SUBROUTINE SET_SHEAR_NAMES
   !!--++
   !!--++ PRIVATE
   !!--++ Set the character variables in eos_type data structures for SHEAR EoS
   !!--++
   !!--++
   !!--++ Date: 11/07/2016
   !!
   Module Subroutine Set_Shear_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS  ! EoS object

      !---- Local Variables ----!
      integer :: n

      !> Check for valid model number. If not valid, set zero
      if (EoS%ishear < 0 .or. EoS%ishear > N_SHEAR_MODELS) EoS%ishear=0

      !> Set the Eos name
      EoS%smodel=shearmodel_names(EoS%ishear)

      !> Set upper limit to thermal parameter numbers
      n=34
      if (n > N_EOSPAR) n=N_EOSPAR

      select case(EoS%ishear)
         case (0)
            EoS%parname(30:n) = ' '
            EoS%comment(30:n) = ' '

         case (1)
            EoS%parname(30)='G0'
            EoS%parname(31)='dG/dP'
            EoS%parname(32)='d2G/'
            EoS%parname(33)='d3G/'
            EoS%parname(34)='dG/dT'
            EoS%comment(30)='Shear modulus at Pref, Tref, in pressure units'
            EoS%comment(31)='Pressure derivative of shear modulus: no units'
            EoS%comment(32)='2nd Pressure derivative of shear modulus: P^-1'
            EoS%comment(33)='3rd Pressure derivative of shear modulus: P^-2'
            EoS%comment(34)='Temperature derivative of shear modulus'
      end select

   End Subroutine Set_Shear_Names

   !!--++
   !!--++ SUBROUTINE SET_THERMAL_NAMES
   !!--++
   !!--++ Set the character variables in eos_type data structures for thermal EoS
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Subroutine Set_Thermal_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS  ! EoS object

      !---- Local Variables ----!
      integer :: n

      !> Check for valid model number. If not valid, set zero
      if (EoS%itherm < -1 .or. EoS%itherm > N_THERM_MODELS) EoS%itherm=0

      !> Set the Eos name
      EoS%tmodel=Tmodel_names(EoS%itherm)

      !> Set upper limit to thermal parameter numbers
      n=19
      if (n > n_eospar) n=n_eospar

      !> Set the V0 name and comment here, in case eos is thermal only
      if (.not. EoS%linear) then
         EoS%ParName(1) ='V0   '
         EoS%comment(1) = 'Reference pressure volume:'
         if (len_trim(EoS%vscale_name) > 0) then
            EoS%comment(1) = trim(EoS%comment(1))//' units are '//trim(EoS%vscale_name)
         else
            EoS%comment(1) = trim(EoS%comment(1))//' units as volume data'
         end if
      else          !Linear
         EoS%ParName(1) ='L0   '
         EoS%comment(1) ='Reference pressure length:'
         if (len_trim(EoS%vscale_name) > 0) then
            EoS%comment(1) = trim(EoS%comment(1))//' units are '//trim(EoS%vscale_name)
         else
            EoS%comment(1) = trim(EoS%comment(1))//' units as length data'
         end if
      end if

      select case(EoS%itherm)
         case (0)
            EoS%parname(10:n) = ' '
            EoS%comment(10:n) = ' '

         case (1)
            EoS%parname(10:11) = (/'alph0','alph1'/)
            EoS%comment(10) = 'Constant of thermal expansion x10^5 K^-1'
            EoS%comment(11) = 'Linear term thermal expansion x10^8 K^-2'

         case (2)
            EoS%parname(10:12) = (/'alph0','alph1','alph2'/)
            EoS%comment(10) = 'Constant of thermal expansion x10^5 K^-1'
            EoS%comment(11) = 'Linear term thermal expansion x10^8 K^-2'
            EoS%comment(12) = '1/T^2 term thermal expansion, K'

         case (3)
            EoS%parname(10:11) = (/'alph0','alph1'/)
            EoS%comment(10) = 'Constant of thermal expansion x10^5 K^-1'
            EoS%comment(11) = 'Sqrt term of thermal expansion x10^4 K^-1/2'

         case (4)    ! Kroll needs Kp as well (in case no pressure eos)
            EoS%parname(3) = 'Kp   '
            EoS%comment(3) = 'dK/dP: dimensionless'
            if (EoS%linear)then
               EoS%parname(3) = 'Mp   '
               EoS%comment(3) = 'dM/dP: dimensionless'
            end if
            EoS%parname(10:11) = (/'alph0','Th_E '/)
            EoS%comment(10) = 'Constant of thermal expansion at Tref x10^5 K^-1'
            EoS%comment(11) = 'Einstein temperature in K'

         case (5)
            EoS%parname(10:11) = (/'p1   ','T_sat'/)
            EoS%comment(10) = 'Approx 3x highT thermal expansion x10^5 K^-1'
            EoS%comment(11) = 'Saturation temperature in K'

         case (6)
            EoS%parname(10:11) = (/'alph0','Th_E '/)
            EoS%comment(10) = 'Constant of thermal expansion at Tref x10^5 K^-1'
            EoS%comment(11) = 'Einstein temperature in K'
            EoS%parname(13) = 'Natom'
            EoS%comment(13) = 'Number of atoms per formula unit'
            EoS%parname(14) = 'qcomp'
            EoS%comment(14) = 'Switch for q-compromise model, +1 for compromise'

         case (7)
            EoS%parname(11) = 'ThMGD'
            EoS%comment(11) = 'Debye temperature in K'
            EoS%parname(13) = 'Natom'
            EoS%comment(13) = 'Number of atoms per formula unit'
            EoS%parname(14) = 'qcomp'
            EoS%comment(14) = 'Switch for q-compromise model, +1 for compromise'

         case (8)
            EoS%parname(11) = 'Th_E'
            EoS%comment(11) = 'Einstein temperature in K'
            EoS%parname(13) = 'Natom'
            EoS%comment(13) = 'Number of atoms per formula unit'
            EoS%parname(14) = 'qcomp'
            EoS%comment(14) = 'Switch for q-compromise model, +1 for compromise'

         case (9)
            EoS%parname(10:17) = ''
            EoS%comment(10:17) = ''
      end select

      !> Common terms for all thermal
      EoS%parname(18) = 'Gamm0'
      EoS%comment(18) = 'Gruneisen parameter at Tref,Pref'
      EoS%parname(19) = 'q    '
      EoS%comment(19) = 'Gruneisen power law in V/V0'

   End Subroutine Set_Thermal_Names

   !!--++
   !!--++ SUBROUTINE SET_TRANSITION_NAMES
   !!--++
   !!--++ Set the character variables in eos_type data structures for Transition EoS
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Subroutine Set_Transition_Names(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS  ! EoS object

      !---- Local Variables ----!
      integer :: n

      !> Check for valid model number. If not valid, set zero
      if (EoS%itran < 0 .or. EoS%itran > N_TRANS_MODELS) EoS%itran=0

      !> Set the model name
      EoS%tranmodel=Tranmodel_names(EoS%itran)

      !> Set upper limit to parameter numbers
      n=29
      if (n > n_eospar) n=n_eospar

      select case(EoS%itran)
         case (0)
            EoS%parname(20:n) = ' '
            EoS%comment(20:n) = ' '

         case (1)       ! Landau power law P-V
            EoS%parname(20:27) = (/'High ','Ptr  ','     ','     ','aL   ','betaL','aH   ','betaH'/)
            EoS%comment(20) = 'Indicator = +1 if high P phase is high sym phase'
            EoS%comment(21) = 'Transition pressure'
            EoS%comment(22) = ''
            EoS%comment(23) = ''
            EoS%comment(24) = 'Scaling parameter, low phase x10^3'
            EoS%comment(25) = 'Power law term, low phase'
            EoS%comment(26) = 'Scaling parameter, high phase x10^3'
            EoS%comment(27) = 'Power law term, high phase'

         case (2)       ! Landau power law V-T
            EoS%parname(20:27) = (/'High ','Ttr  ','     ','     ','aL   ','betaL','aH   ','betaH'/)
            EoS%comment(20) = 'Indicator = +1 if high T phase is high sym phase'
            EoS%comment(21) = 'Transition temperature'
            EoS%comment(22) = ''
            EoS%comment(23) = ''
            EoS%comment(24) = 'Scaling parameter, low phase x10^3'
            EoS%comment(25) = 'Power law term, low phase'
            EoS%comment(26) = 'Scaling parameter, high phase x10^3'
            EoS%comment(27) = 'Power law term, high phase'

         case (3)       ! Landau power law PVT
            EoS%parname(20:27) = (/'High ','Ttr  ','Ttr-P','TtrP2','aL   ','betaL','aH   ','betaH'/)
            EoS%comment(20) = 'Indicator = +1 if high T phase is high sym phase'
            EoS%comment(21) = 'Transition temperature at P=0'
            EoS%comment(22) = 'Ttr=Ttr0 + uP + vP^2: P coeff'
            EoS%comment(23) = 'Ttr=Ttr0 + uP + vP^2: P^2 coeff'
            EoS%comment(24) = 'Scaling parameter, low phase x10^3'
            EoS%comment(25) = 'Power law term, low phase'
            EoS%comment(26) = 'Scaling parameter, high phase x10^3'
            EoS%comment(27) = 'Power law term, high phase'
      end select

   End Subroutine Set_Transition_Names

End SubModule EoS_Set