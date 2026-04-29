!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Pthermal
   implicit none

   Contains

   !!----
   !!---- FUNCTION THERMAL_PRESSURE_EOS
   !!----
   !!----
   !!---- Date: 03/02/2021
   !!
   Module Function Thermal_Pressure_Eos(I) Result(Pth)
      !---- Arguments ----!
      integer, intent(in) :: i        !a thermal model number
      logical             :: pth

      select case(i)
         case(1:5)
            pth=.false.

         case(6:10)
            pth=.true.

         case default
            pth=.false.
      end select

   End Function Thermal_Pressure_Eos

   !!----
   !!---- FUNCTION PSCALEMGD
   !!----
   !!---- Date: 31/03/2021
   !!
   Module Function PscaleMGD(EoS) Result(MGD)
      !---- Arguments ----!
      type(Eos_Type), intent(in)  :: EoS        ! EoS
      logical                     :: MGD        ! .true. if e%pscale_name iskbar or Gpa

      !---- Local Variables ----!
      character(len=len(eos%vscale_name)) :: vname

      !> Init
      MGD=.false.

      vname=adjustl(U_case(eos%pscale_name))
      if (len_trim(vname) == 0)return

      if (index(vname,'GPA') > 0 ) MGD=.true.
      if (index(vname,'KBAR') > 0 ) MGD=.true.

   End Function PscaleMGD

   !!----
   !!---- FUNCTION PTHERMAL
   !!----
   !!----  Calculate Pthermal from eosparameters at temperature T
   !!----
   !!---- Date: 10/09/2013
   !!
   Module Function Pthermal(V, T, Eos, J) Result(Pth)
      !---- Arguments ----!
      real(kind=cp),   intent(in) :: V       ! Volume: not needed for HP2011 of linear-thermalpressure, needed for MGD, q-comp and Einstein
                                             ! assumed this is  'a' if linear
      real(kind=cp),   intent(in) :: T       ! Temperature
      type(Eos_Type),  intent(in) :: EoS     ! Eos Parameter
      integer,optional,intent(in) :: j       ! which oscillator: then Pthermal only calculates for this one
      real(kind=cp)               :: pth

      !---- Local Variables ----!
      integer                           :: i,jo
      real(kind=cp)                     :: thtref,exp0,eta0,eth,eth0
      real(kind=cp)                     :: gammaV, thetaD,thetaE
      real(kind=cp),dimension(0:2)      :: pthp  !contributions to pth
      real(kind=cp),dimension(N_EOSPAR) :: ev


      !> Local copies
      ev= eos_to_vec(EoS)    !handle linear case
      jo=-1                  !calculate all
      if (present(j))then
         if (j > -1 .and. j < 3) jo=j
      end if
      pthp=0._cp
      pth=0._cp

      select case (EoS%itherm)
         case (6) !> Thermal pressure from Holland and Powell 2011
            thtref=ev(11)/EoS%tref                          ! T_einstein/Tref
            exp0=exp(thtref)                                ! exp(T_Ein/Tref)
            eta0= thtref*thtref*exp0/(exp0-1.0_cp)**2.0_cp  ! eta at Tref

            pthp(0) = ev(10)*ev(2)*ev(11)/eta0*( 1.0_cp/(exp(ev(11)/t)-1.0_cp) -1.0_cp/(exp0 -1.0_cp))
            !> no scaling required because pth is scaled by Ko

         case (7) !> MGD in the form of Kroll et al (2012)
            thetaD=get_DebyeT(V,EoS)                         ! Both get_Debye and get_grun expect 'a' value if linear
            gammaV=get_grun_V(V,EoS)
            pthp(0)=gammaV/v*(EthDebye(T,thetaD,EoS%params(13))-EthDebye(EoS%tref,thetaD,EoS%params(13)))

         case(8) !> Einstein oscillator
            gammaV=get_grun_V(V,EoS)
            thetaE=get_DebyeT(V,EoS)
            eth=EthEinstein(T,thetaE,EoS%params(13))
            eth0=EthEinstein(EoS%tref,thetaE,EoS%params(13))
            pthp(0)=gammaV/v*(eth-eth0)

         case(9)   !Cv table
            gammaV=get_grun_V(V,EoS)
            eth=EthTable(T,EoS)
            eth0=EthTable(EoS%tref,EoS)
            pthp(0)=gammaV/v*(eth-eth0)

         case default
            pthp(0)=0.0_cp
      end select

      !> Extra oscillators: only allowed in combination with models 6,7,8
      if (EoS%osc_allowed .and. sum(EoS%iosc) > 0)then
         pthp(0)=(1._cp-EoS%params(40)-EoS%params(45))*pthp(0)     ! partial contribution main oscillator

         do i=1,2
            select case(EoS%iosc(i))
               case(0)
                  cycle

               case(1) !> DEBYE
                  thetaD=get_DebyeT(V,EoS,i)
                  gammaV=get_grun_V(V,EoS,i)
                  pthp(i)=gammaV/v*EoS%params(35+5*i)*  &
                      (EthDebye(T,thetaD,EoS%params(13))-EthDebye(EoS%tref,thetaD,EoS%params(13)))

               case(2) !> Einstein
                  gammaV=get_grun_V(V,EoS,i)
                  thetaE=get_DebyeT(V,EoS,i)
                  pthp(i)=gammaV/v*EoS%params(35+5*i)*  &
                      (EthEinstein(T,thetaE,EoS%params(13))-EthEinstein(EoS%Tref,thetaE,EoS%params(13)))
            end select
         end do
      end if

      !> if the thermal energy was from EthDebye or EthEinstein, it is in J/mol pth
      !> Then if V in m3/mol  Eth/V is in J/m3=Pa
      select case(EoS%itherm)
         case(7,8,9)
            pthp=pthp*EPthermal_factor(EoS)
      end select

      !> Now return requested part of pth:
      if (jo == -1)then
         pth=sum(pthp)
      else
         pth=pthp(jo)
      end if

   End Function Pthermal

   !!--++
   !!--++ FUNCTION EPTHERMAL_FACTOR
   !!--++
   !!--++  Calculate Scale factor for E(thermal) to P(thermal) on basis of units for P and V
   !!--++  Scale should be used to multiply the Pthermal calculated from eos parameters
   !!--++
   !!--++ Date: 03/09/2020
   !!
   Module Function EPthermal_factor(Eos) Result(scale)
      !---- Arguments ----!
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameters
      real(kind=cp)              :: scale

      !---- Local Variables ----!
      character(len=len(eos%vscale_name)) :: vname

      !>init
      scale=1.0_cp

      !>if the thermal energy was from EthDebye or EthEinstein, it is in J/mol pth
      !>Then if V in m3/mol  Pth=Eth/V is in J/m3=Pa

      !> Pressure scales
      if (index(U_case(eos%pscale_name),'GPA') > 0)  scale=1.0E-9
      if (index(U_case(eos%pscale_name),'KBAR') > 0) scale=1.0E-8

      !> Volume
      vname=adjustl(U_case(eos%vscale_name))
      if (len_trim(vname) == 0)return

      !> test for cm3/mol or equivalent
      if (index(vname,'CM') > 0 .and. index(vname,'3') > 0 .and. index(vname,'MOL') > 0)scale=scale*1.0E+6

      return
   End Function EPthermal_factor

   !!--++
   !!--++ FUNCTION ETHDEBYE
   !!--++
   !!--++  Calculates the Debye thermal Energy in Jmol(-1)
   !!--++  because R is given in Jmol(-1)K(-1)
   !!--++
   !!--++ Date: 18/11/2015
   !!
   Module Function EthDebye(T, Theta, Natom) Result(Eth)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T       ! Temperature
      real(kind=cp),  intent(in) :: Theta   ! Debye T
      real(kind=cp),  intent(in) :: Natom   ! Number of atoms in formula unit
      real(kind=cp)              :: Eth

      !---- Local Variables ----!
      !real(kind=8)  :: x
      real(kind=cp)  :: x

      if (T < 0.1) then
         Eth=0.0_cp

      else
         x=theta/t
         Eth=debye(3,x)
         Eth=3.0_cp*Natom*8.314_cp*T*Eth
      end if

   End Function EthDebye

   !!--++
   !!--++ FUNCTION ETHEINSTEIN
   !!--++
   !!--++  Calculates the thermal Energy in Jmol(-1) of N Einstein oscillators
   !!--++  because R is given in Jmol(-1)K(-1)
   !!--++
   !!--++ Date: 3/2020
   !!
   Module Function EthEinstein(T, Theta, Natom) Result(Eth)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T       ! Temperature
      real(kind=cp),  intent(in) :: Theta   ! Einstein T
      real(kind=cp),  intent(in) :: Natom   ! Number of atoms in formula unit
      real(kind=cp)              :: Eth

      !---- Local Variables ----!

      if (T < 0.1) then
         Eth=0.0_cp
      else
         Eth=3.0_cp*Natom*8.314_cp*Theta/(exp(Theta/T)-1.0_cp)
      end if

      return
   End Function EthEinstein

   !!--++
   !!--++ FUNCTION ETHTABLE
   !!--++
   !!--++  Extracts the thermal Energy in Jmol(-1) from cv_table(:,3)
   !!--++  values in table are Jmol(-1)K(-1)
   !!--++  Cv for this model is independent of P
   !!--++
   !!--++ Date: 9/2021
   !!
   Module Function EthTable(T, EoS) Result(Eth)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameters
      real(kind=cp)              :: Eth

      !---- Local Variables ----!
      integer  :: i

      if (T < 1.0) then
          Eth=0.0_cp

      else
         i=locate(eos%cv_table(1:,1),T,size(eos%cv_table,dim=1))     ! i is the index to the entry with biggest T < Trequest
         if (i > Ubound(eos%cv_table,dim=1)-1)then
            call set_error(-1,'Request to Ethtable with T > Tmax')
            i=Ubound(eos%cv_table,dim=1)
            Eth=(nint(T)-eos%cv_table(i,1))*eos%cv_table(i,2) +eos%cv_table(i,3)

         elseif (i < 1) then
            call set_error(-1, 'Request to Ethtable with T < Tmin')
            Eth=eos%cv_table(1,3)

         else
            Eth=eos%cv_table(i,3) + (eos%cv_table(i+1,3) - eos%cv_table(i,3))/(eos%cv_table(i+1,1) - eos%cv_table(i,1))*(T - eos%cv_table(i,1))

         end if
      end if

   End Function EthTable

   !!----
   !!---- VSCALEMGD
   !!----
   !!---- 31/05/2019
   !!
   Module Function VscaleMGD(EoS) Result(MGD)
      !---- Arguments ----!
      type(Eos_Type), intent(in)  :: EoS        ! EoS
      logical                     :: MGD        ! .true. if e%vscale_name is cm3/mol

      !---- Local Variables ----!
      character(len=len(eos%vscale_name)) :: vname

      !> Init
      MGD=.false.

      vname=adjustl(U_case(eos%vscale_name))
      if(len_trim(vname) == 0) return

      if (index(vname,'CM') > 0 .and. index(vname,'3') > 0 .and. index(vname,'MOL') > 0) MGD=.true.

   End Function VscaleMGD

   !!----
   !!---- FUNCTION GET_DEBYET
   !!----
   !!---- Calculate the Debye Temperature at V
   !!----
   !!---- Date: 16/03/2017
   !!
   Module Function Get_DebyeT(V, Eos, i) result(DebyeT)
      !---- Arguments ----!
      real(kind=cp),    intent(in) :: V       ! Volume or length
      type(Eos_Type),   intent(in) :: EoS     ! Eos Parameters
      integer,optional, intent(in) :: i       ! which oscillator
      real(kind=cp)                :: DebyeT

      !---- Local Variables ----!
      integer       :: io
      real(kind=cp) :: gammaV,V0V

      !> Default
      DebyeT=eos%tref

      if (.not. eos%Osc_allowed) return

      V0V=eos%params(1)/V
      if (eos%linear) V0V=V0V**3.0_cp

      !> local copy of pointer to oscillator
      io=0
      if (present(i)) then
         if (i > 0 .and. i <= N_OSC_MODELS)io=i
      end if

      if (io == 0)then                    !main thermal model
                                          !> For linear uses the same parameter values, no factor of 3
         if (eos%params(14) > 0.5_cp)then
            !> q-compromise
            DebyeT=eos%params(11)
         else
            !> normal
            if (abs(eos%params(19)) < tiny(0._cp)) then
               DebyeT=eos%params(11)*V0V**eos%params(18)                  ! when q=0, gamma=gamma0
            else
               gammaV=get_Grun_v(v,eos)               ! Get_grun knows about linear/volume
               DebyeT=eos%params(11)*exp((eos%params(18)-gammaV)/eos%params(19))  ! if q=0 then this gives DebyeT=nan
            end if
         end if

      else                              !extra oscillator: Debye and einstein
         if (eos%iosc(io) == 0)return

         if (eos%params(39+5*io) > 0.5_cp)then
            !> q-compromise
            DebyeT=eos%params(36+5*io)
         else
            !> normal
            if (abs(eos%params(38+5*io)) < tiny(0._cp)) then
               DebyeT=eos%params(36+5*io)*V0V**eos%params(37+5*io)                 ! when q=0, gamma=gamma0
            else
               gammaV=get_Grun_v(v,eos,io)               ! Get_grun knows about linear/volume
               DebyeT=eos%params(36+5*io)*exp((eos%params(37+5*io)-gammaV)/eos%params(38+5*io))  ! if q=0 then this gives DebyeT=nan
            end if
         end if
      end if

   End Function Get_DebyeT

End SubModule EoS_Pthermal