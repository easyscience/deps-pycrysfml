!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Init
   implicit none
   Contains

   !!----
   !!---- SUBROUTINE INIT_EOS_Angles
   !!----
   !!---- Initialize the EoS Type for Angles polynomial
   !!----
   !!---- Date: 07/10/2020
   !!
   Module Subroutine Init_EoS_Angles(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !> Check for valid model number. If not valid, set zero
      if (eos%iangle < 0 .or. eos%iangle > N_ANGLE_MODELS) eos%iangle=0

      Eos%AngPoly  = 0.0_cp
      Eos%angpoly(1:3,0,1)=90._cp

   End Subroutine Init_EoS_Angles

   !!----
   !!---- SUBROUTINE INIT_EOS_CELL_TYPE
   !!----
   !!---- Subroutine to initialise eos_cell_type and set to default orthorhombic
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Subroutine Init_Eos_Cell_Type(Cell_Eos)
      !---- Arguments ----!
      type(eos_cell_type),intent(in out) :: cell_eos

      !---- Local Variables ----!
      integer i

      !> clear eos
      do i=0,6
         call Init_EoS_Type(cell_eos%eos(i))
      end do

      !>reset to default orthorhombic
      cell_eos%n=3
      cell_eos%system='TRICLINIC'
      cell_eos%obtuse = .true.
      cell_eos%unique_label=' '
      cell_eos%unique=0
      call Init_EoS_Type(cell_eos%eosang)
      call set_cell_types(cell_eos)      ! sets the cout array PV VT etc

   End Subroutine Init_Eos_Cell_Type

   !!----
   !!---- SUBROUTINE INIT_EOS_CROSS
   !!----
   !!---- Initialize the EoS Type for P-T cross-terms
   !!----
   !!---- Date: 11/11/2016
   !!
   Module Subroutine Init_EoS_Cross(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !> Check for valid model number. If not valid, set zero
      if (EoS%icross < 0 .or. EoS%icross > N_CROSS_MODELS) EoS%icross=0
      if (EoS%pthermaleos) EoS%icross=0

      EoS%params(8)           = 0.0_cp
      EoS%params(9)           = 0.0_cp
      EoS%vcv(8:9,1:N_EOSPAR) = 0.0_cp
      EoS%vcv(1:N_EOSPAR,5:6) = 0.0_cp
      EoS%factor(8)           = 1.0_cp
      EoS%factor(9)           = 1.0_cp

      call Set_Cross_Names(EoS)    ! Set the variable names
      call Set_Eos_Use(EoS)        ! update the use flags

   End Subroutine Init_EoS_Cross

   !!----
   !!---- INIT_EOS_DATA_TYPE
   !!----    Initialize EoS_Data_Type
   !!----
   !!---- 17/07/2015
   !!
   Module Subroutine Init_EoS_Data_Type(E)
      !---- Arguments ----!
      type (EoS_Data_Type), intent(in out)   :: E

      !> Init
      E%IUse = 0
      E%IGrp = 0
      E%T    = 298.0
      E%P    = 0.0_cp
      E%V    = 0.0_cp
      E%cell = 0.0_cp
      E%ang  = 0.0_cp

      E%SigT = 0.0_cp
      E%SigP = 0.0_cp
      E%SigV = 0.0_cp
      E%sigc = 0.0_cp
      E%siga = 0.0_cp

   End Subroutine Init_EoS_Data_Type

   !!----
   !!---- SUBROUTINE INIT_EOS_GROUPSCALES
   !!----
   !!---- Initialize the EoS Type for extra oscillators
   !!----
   !!---- Date: 23/03/2020
   !!
   Module Subroutine Init_EoS_GroupScales(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !---- Variables ----!
      integer  :: i

      !>Default values
      EoS%params(50:59) = 1.0_cp
      EoS%factor(50:59) = 1.0_cp
      EoS%vcv(50:59,1:N_EOSPAR)= 0.0_cp
      EoS%vcv(1:N_EOSPAR,50:59)= 0.0_cp

      !>Names
      EoS%ParName(1) ='     '
      EoS%comment(1) ='Not used'
      do i=1,9
        write(EoS%ParName(50+i),'(''Sca '',i1)')i
        write(EoS%comment(50+i),'(''Scale factor for data group '',i1)')i
      end do

      !>Use flags default to 0. The values depend on the groups of data present
      ! and therefore cannot be set in cfml_eos_mod but must be reset by main programs
      EoS%iuse(50:59)=0

   End Subroutine Init_EoS_GroupScales

   !!----
   !!---- SUBROUTINE INIT_EOS_OSC
   !!----
   !!---- Initialize the EoS Type for extra oscillators
   !!----
   !!---- Date: 09/03/2020
   !!
   Module Subroutine Init_EoS_Osc(EoS, i)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS
      integer,         intent(in)     :: i     !=1 for 1st oscillator, =2 for second, = 3 for both


      if (i == 1 .or. i == 3) then
         if (eos%iosc(1) < 0 .or. eos%iosc(1) > N_OSC_MODELS) eos%iosc(1)=0

         !> initial values same for all models
         eos%params(40)        = 0.0_cp     !fraction
         eos%params(41)        = eos%tref  ! Characteristic T
         eos%params(42)        = 1.0_cp     ! gamma
         eos%params(43)        = 0.0_cp     ! q
         eos%params(44)        = 0.0_cp     ! not used

         eos%vcv(40:44,1:N_EOSPAR)= 0.0_cp
         eos%vcv(1:N_EOSPAR,40:44)= 0.0_cp

         eos%factor(40:44)        = 1.0_cp
      end if

      if (i == 2 .or. i == 3)then
         if (eos%iosc(2) < 0 .or. eos%iosc(2) > N_OSC_MODELS) eos%iosc(2)=0

         !> initial values same for all models
         eos%params(45)        = 0.0_cp     !fraction
         eos%params(46)        = eos%tref  ! Characteristic T
         eos%params(47)        = 1.0_cp     ! gamma
         eos%params(48)        = 0.0_cp     ! q
         eos%params(49)        = 0.0_cp     ! not used

         eos%vcv(45:49,1:N_EOSPAR)= 0.0_cp
         eos%vcv(1:N_EOSPAR,45:49)= 0.0_cp

         eos%factor(45:49)        = 1.0_cp
      end if

      call Set_Osc_Names(Eos)    ! Set the variable names
      call Set_Eos_Use(Eos)      ! update the use flags

   End Subroutine Init_EoS_Osc

   !!----
   !!---- SUBROUTINE INIT_EOS_Shear
   !!----
   !!---- Initialize the EoS Type for Shear case
   !!----
   !!---- Date: 17/02/2015
   !!
   Module Subroutine Init_EoS_Shear(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !---- Variables ----!
      integer  :: n

      !> Check for valid model number. If not valid, set zero
      if (EoS%ishear < 0 .or. EoS%ishear > N_SHEAR_MODELS) EoS%ishear=0

      !> Set upper limit to parameter numbers
      n=34
      if (n > N_EOSPAR) n=N_EOSPAR

      select case(EoS%ishear)
         case (0)
            EoS%params(30)          = huge(0.0_cp)   ! default is infinitely stiff
            EoS%vcv(30,1:n)         = 0.0_cp
            EoS%vcv(30:N_EOSPAR,1:n)= 0.0_cp
            EoS%vcv(1:N_EOSPAR,30:n)= 0.0_cp
            EoS%factor(30:n)        = 1.0_cp

         case (1)        ! polynomial
            EoS%params(30)    = 100.0_cp      ! G0 at Pref Tref
            EoS%params(31:34) =   0.0_cp      ! Polynomial coefficients
            EoS%factor(30:n)  =   1.0_cp
      end select

      call Set_Shear_Names(EoS)    ! Set the variable names
      call Set_Eos_Use(EoS)        ! update the use flags

   End Subroutine Init_EoS_Shear

   !!----
   !!---- SUBROUTINE INIT_EOS_THERMAL
   !!----
   !!---- Initialize the EoS Type for Thermal case
   !!----
   !!---- Date: 10/09/2013
   !!
   Module Subroutine Init_EoS_Thermal(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !---- Variables ----!
      integer    :: n

      !> Check for valid model number. If not valid, set zero
      if(EoS%itherm < -1 .or. EoS%itherm > N_THERM_MODELS) EoS%itherm=0

      !> Set upper limit to thermal parameter numbers
      n=19
      if (n > N_EOSPAR)n=N_EOSPAR

      EoS%alphafactor=1.0E5_cp                      ! Normal scale factor for printing values of alpha

      select case(EoS%itherm)
         case (-1)           ! PTV table
            EoS%factor(10)   = 1.0E5_cp            ! factor to multiply alpha values on printing
            EoS%params(10:n) = 0.0_cp              ! all thermal parameters set zero, not used
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (0)
            EoS%params(8)    = 0.0_cp
            EoS%params(10:n) = 0.0_cp
            EoS%vcv(8,1:n)   = 0.0_cp
            EoS%vcv(10:n,1:n)= 0.0_cp
            EoS%vcv(1:n,10:n)= 0.0_cp
            EoS%factor(10:n) = 1.0_cp
            EoS%TRef         = 298.0_cp
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (1)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E8_cp
            EoS%TRef        = 298.0_cp             ! Simple thermal expansion,
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (2)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E8_cp
            EoS%factor(12)  = 1.0_cp
            EoS%TRef        = 298.0_cp             ! Simple thermal expansion,
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (3)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0E4_cp
            EoS%TRef        = 298.0_cp             ! Simple thermal expansion,
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (4)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp
            EoS%TRef        = 298.0_cp             ! Holland and Powell thermal expansion without P
            EoS%TRef_fixed  = .false.
            EoS%params(11)  = 298.0_cp             ! Einstein temperature
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (5)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp
            EoS%TRef        = 0.0_cp               ! Salje thermal expansion
            EoS%TRef_fixed  = .true.
            EoS%params(11)  = 298.0_cp             ! Saturation temperature
            EoS%pthermaleos  =.false.
            EoS%Osc_allowed  =.false.

         case (6)
            EoS%factor(10)  = 1.0E5_cp             ! factor to multiply values on printing
            EoS%factor(11)  = 1.0_cp
            EoS%TRef        = 298.0_cp
            EoS%TRef_fixed  = .false.
            EoS%params(11)  = 298.0_cp             ! Einstein temperature default
            EoS%params(13)  = 1.0                   ! Natom
            EoS%params(14)  = 1.0                   ! q-compromise
            EoS%pthermaleos  =.true.
            EoS%Osc_allowed  =.false.

         case(7,8)                                 ! MGD,  Einstein Osc:
            EoS%factor(10:14)  = 1.0_cp            ! plus gamma0 and q as (18),(19)
            EoS%TRef           = 298.0_cp
            EoS%TRef_fixed     = .false.

            EoS%params(11)     = 298.0_cp          ! Debye/Einstein temperature default

            EoS%params(13)     = 1.0               ! Natoms/molecule for MGD
            EoS%params(14)     = 0.0               ! flag to use full q
            EoS%pthermaleos    =.true.
            EoS%Osc_allowed    =.true.

         case(9)                                   ! molar Cv for Pthermal from table
            EoS%TRef           = 298.0_cp
            EoS%TRef_fixed     = .false.
            EoS%params(10:17)  = 0.0_cp            ! includes param(14) set to use full q
            EoS%pthermaleos    =.true.
            EoS%Osc_allowed    =.false.

      end select

      !> Set the common terms for Ks to Kt conversion: also used in  thermal pressure with oscillator
      if (eos%itherm /= -1) then
         EoS%params(18)=1.0_cp      ! gamma0
         EoS%params(19)=0.0_cp      ! q
      end if

      call Init_EoS_Cross(EoS)                             ! init the cross-terms
      if (.not. EoS%osc_allowed) call Init_EoS_Osc(EoS,3)  ! clear extra oscillators
      call Set_Thermal_Names(EoS)                          ! Set the variable names
      call Set_Eos_Use(EoS)                                ! update the use flags and other pointers

   End Subroutine Init_EoS_Thermal

   !!----
   !!---- SUBROUTINE INIT_EOS_TRANSITION
   !!----
   !!---- Initialize the EoS Type for Transition case
   !!----
   !!---- Date: 17/02/2015
   !!
   Module Subroutine Init_EoS_Transition(EoS)
      !---- Arguments ----!
      type (EoS_Type), intent(in out) :: EoS

      !---- Variables ----!
      integer  :: n

      !> Check for valid model number. If not valid, set zero
      if (EoS%itran < 0 .or. EoS%itran > N_TRANS_MODELS) EoS%itran=0

      !> Set upper limit to parameter numbers
      n=29
      if (n > N_EOSPAR) n=N_EOSPAR

      select case(EoS%itran)
         case (0)
            EoS%params(20:n)        = 0.0_cp
            EoS%vcv(20,1:n)         = 0.0_cp
            EoS%vcv(20:n_eospar,1:n)= 0.0_cp
            EoS%vcv(1:n_eospar,20:n)= 0.0_cp
            EoS%factor(20:n)        = 1.0_cp

         case (1)        ! Landau PV
            EoS%params(20) = 1.0_cp        ! high P is high sym
            EoS%params(21) = 5.0_cp        ! Safe default Ptr
            EoS%params(22) = 0.0_cp        ! dTr/dP: not used
            EoS%params(23) = 0.0_cp        ! d2Tr/dP2 not used
            EoS%params(24) = 0.0_cp        ! no excess V
            EoS%params(25) = 0.5_cp        ! power law
            EoS%params(26) = 0.0_cp        ! excess V high phase
            EoS%params(27) = 0.5_cp        ! power law high phase

            EoS%factor(20:n) = 1.0_cp
            EoS%factor(24)   = 1.0E3_cp    ! 1000 for aL
            EoS%factor(26)   = 1.0E3_cp    ! 1000 for aH

         case (2)        ! Landau TV
            EoS%params(20) =   1.0_cp        ! high T is high sym
            EoS%params(21) = 800.0_cp        ! Safe default Ttr
            EoS%params(22) = 100.0_cp        ! dTr/dP: dummy will be used in get_pressure
            EoS%params(23) =   0.0_cp        ! d2Tr/dP2 not used
            EoS%params(24) =   0.0_cp        ! no excess V
            EoS%params(25) =   0.5_cp        ! power law
            EoS%params(26) =   0.0_cp        ! excess V high phase
            EoS%params(27) =   0.5_cp        ! power law high phase

            EoS%factor(20:n) = 1.0_cp
            EoS%factor(24)   = 1.0E3_cp      ! 1000 for aL
            EoS%factor(26)   = 1.0E3_cp      ! 1000 for aH

         case (3)        ! Landau PVT
            EoS%params(20) =   1.0_cp        ! high T is high sym
            EoS%params(21) = 800.0_cp        ! Safe defaulat Tr
            EoS%params(22) =   1.0_cp
            EoS%params(23) =   0.0_cp
            EoS%params(24) =   0.0_cp        ! no excess V
            EoS%params(25) =   0.5_cp
            EoS%params(26) =   0.0_cp        ! excess V high phase
            EoS%params(27) =   0.5_cp        ! power law high phase

            EoS%factor(20:n) = 1.0_cp
            EoS%factor(24)   = 1.0E3_cp      ! 1000 for aL
            EoS%factor(23)   = 1.0E5_cp      ! for da/dP
            EoS%factor(26)   = 1.0E3_cp      ! 1000 for aH

      end select

      call Set_Transition_Names(EoS)    ! Set the variable names
      call Set_Eos_Use(EoS)             ! update the use flags

   End Subroutine Init_EoS_Transition

   !!----
   !!---- SUBROUTINE INIT_EOS_TYPE
   !!----
   !!---- Initialize EoS_Type setting all parameters to sensible values
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Init_EoS_Type(Eos, CLin, IThermal, ITransition, Ishear, Icross)
      !---- Arguments ----!
      type (EoS_Type),            intent(out)    :: Eos          ! EoS Type
      character(len=*), optional, intent(in)     :: CLin         ! Character variable to indicate linear EoS or not
      integer,          optional, intent(in)     :: IThermal     ! integer to indicate ithermal type
      integer,          optional, intent(in)     :: ITransition  ! integer to indicate transition type
      integer,          optional, intent(in)     :: IShear       ! integer to indicate shear type
      integer,          optional, intent(in)     :: ICross       ! integer to indicate cross-terms type

      !> test for optional argument for linear
      EoS%Linear  =.false.
      if (present(clin)) then
         if (index(U_case(clin(1:3)),'LIN') > 0) EoS%linear=.true.
      end if

      EoS%Title   =' '
      EoS%IModel  =0
      EoS%IOrder  =3
      EoS%IAngle  =0

      EoS%ParName=' '
      EoS%comment=' '
      EoS%doc=' '
      EoS%savedate=' '
      EoS%lineardir=' '
      EoS%pscale_name=' '
      EoS%vscale_name=' '
      call Set_Eos_Names(EoS)         ! also sets the print factors for the pressure part

      EoS%PRef     = 0.0_cp
      EoS%Density0 = 0.0_cp

      EoS%Iuse     =0
      EoS%Iuse(1:4)=1                 ! Vo, Ko, Kpp

      EoS%params   = 0.0_cp
      EoS%esd      = 0.0_cp

      EoS%params(1)= 1.0_cp
      EoS%params(2)=10.0_cp
      EoS%params(3)= 4.0_cp
      EoS%params(5)= 1.0_cp          ! Z for APL, set non-zero for safety. params(5) not used by any other EoS

      EoS%X        = 0.0_cp
      EoS%stoich   = 1.0_cp

      EoS%WChi2    = 0.0_cp
      EoS%DelPMax  = 0.0_cp
      EoS%IWt      = 0

      EoS%IRef     = 0
      EoS%factor   = 1.0_cp
      EoS%LastShift= 0.0_cp
      EoS%VCV      = 0.0_cp

      !> Test for optional argument for thermal
      EoS%ITherm  = 0
      EoS%Tref    = 298.0_cp         ! Sensible default

      Eos%Cv_table= 0.0_cp
      Eos%cv_external=.false.

      if (present(ithermal) .and. ithermal > -2 )then
         EoS%Itherm = ithermal
         call Init_Eos_Thermal(EoS)              ! set up default values, names for specific thermal eqn
      end if

      !> Test for optional argument for transition
      EoS%ITran  =0
      if (present(itransition) .and. itransition  > -1 )then
         EoS%ITran = itransition
         call Init_Eos_Transition(EoS)              ! set up default values, names for specific transition
      end if

      !> Test for optional argument for cross terms
      EoS%ICross  =0
      if (present(icross) .and. icross  > -1 )then
         EoS%Icross = icross
         call Init_Eos_Cross(EoS)              ! set up default values, names for specific crosstrem model
      end if

      !> Test for optional argument for shear model
      EoS%Ishear  =0
      if (present(ishear) .and. ishear  > -1 )then
         EoS%Ishear = ishear
      end if

      call Init_Eos_Shear(EoS)              ! set up default values, names for specific shear model

      call Init_EoS_Osc(EoS,3)

      call Init_EoS_Groupscales(EoS)

      call Init_EoS_Angles(Eos)

   End Subroutine Init_EoS_Type

End SubModule EoS_Init