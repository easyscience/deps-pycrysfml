!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Checks
   implicit none
   Contains

   !!--++
   !!--++ SUBROUTINE CHECK_AXIS
   !!--++
   !!--++ Returns .true. if length of axis can be calculated from eos in cell
   !!--++
   !!--++ Date: 15/12/2020  (does not appear to be used???)
   !!
   Module Subroutine Check_Axis(Cell, Axis, OK)
      !---- Arguments ----!
      type(eos_cell_type), intent(in out) :: cell
      type(axis_type),     intent(in)     :: axis
      logical                             :: ok

      !---- Local Variables ----!

      !> init
      ok=.true.

      !> set flags
      call Set_Cell_Types(cell)

      !> Test
      select case(axis%ieos)
         case (0:6)
            if (cell%loaded(axis%ieos) > 0) return

         case(-2)
            call Eos_Cell_Loaded_Check(cell)
            if ( err_cfml%Ierr <=  1) return
      end select

      ok=.false.

   End Subroutine Check_Axis

   !!----
   !!---- SUBROUTINE CHECK_SCALES
   !!----
   !!----  Check Scales definitions
   !!----
   !!---- Date: 03/02/2021
   !!
   Module Subroutine Check_Scales(EoS, Dat)
      !---- Arguments ----!
      type(Eos_Type),                     intent(in)  :: EoS     ! EoS
      type (eos_data_list_type),optional, intent(in)  :: Dat   ! data structure

      !---- Local Variables ----!
      character(len=40) :: name

      !> Init
      call clear_error()

      !> APL
      if (eos%imodel == 6)then
         if (len_trim(eos%pscale_name) == 0)then
            if (eos%linear)then
               call set_error(-1,'APL EoS must have a Pscale (and M0) in GPa' ) ! Warning
            else
               call set_error(-1,'APL EoS must have a Pscale (and K0) in GPa')
            end if
         end if

         if (len_trim(eos%vscale_name) == 0 .or. index(U_case(eos%Vscale_name),'A') == 0)then
            if (len_trim(err_CFML%Msg) == 0)then
               if (eos%linear)then
                  call set_error(-1,'APL EoS must have a Vscale and L0 in A')
               else
                  call set_error(-1,'APL EoS must have a Vscale and V0 in A^3')
               end if
            else
               if (eos%linear)then
                  call set_error(-1,trim(err_CFML%Msg)//' and a Vscale and L0 in A')
               else
                  call set_error(-1,trim(err_CFML%Msg)//' and a Vscale and V0 in A^3')
               end if
            end if
         end if
      end if

      !> If MGD or q-compromise type thermal EoS, must have eos%pscale_name and eos%_Vscale_name
      if (eos%itherm == 7 .or. eos%itherm == 8 .or. eos%itherm == 9)then
         if ( .not. pscaleMGD(Eos))then
            call set_error(-1,'EoS must have a Pscale in kbar or GPa')
         end if

         if ( .not. VscaleMGD(EoS))then
            if (len_trim(err_CFML%Msg) == 0)then
               call set_error(-1,'EoS must have a Vscale in cm3/mol')
            else
               call set_error(-1,trim(err_CFML%Msg)//' and a Vscale in cm3/mol')
            end if
         end if
         if (len_trim(err_CFML%Msg) /= 0) err_CFML%Msg=trim(err_CFML%Msg)//' set to get correct results. '
      end if

      !> End checks here if only eos present
      if (.not. present(dat)) return

      !>For all EoS compare data and eos scales
      if (len_trim(eos%pscale_name) /= 0 .and. len_trim(dat%Pscale_name) /=0)then
         if (trim(u_case(adjustl(eos%pscale_name))) /= trim(u_case(adjustl(dat%Pscale_name))))then
            if (len_trim(err_CFML%Msg) > 0) err_CFML%Msg=trim(err_CFML%Msg)//' And'
            call set_error(-1,trim(err_CFML%Msg)//' Pscales of data and EoS are different.')
         end if
      end if

      if (len_trim(eos%vscale_name) /= 0 )then
         if (eos%linear)then
            name=trim(u_case(adjustl(dat%Lscale_name)))
         else
            name=trim(u_case(adjustl(dat%Vscale_name)))
         end if

         if (len_trim(name) /= 0)then
            if (trim(u_case(adjustl(eos%vscale_name))) /= trim(name))then
               if (len_trim(err_CFML%Msg) > 0) err_CFML%Msg=trim(err_CFML%Msg)//' And'
               call set_error(-1,trim(err_CFML%Msg)//' Vscales of data and EoS are different')
            end if
         end if
      end if

   End Subroutine Check_Scales

   !!----
   !!---- SUBROUTINE PHYSICAL_CHECK
   !!----
   !!---- Check if the parameters have physical sense
   !!---- New routine with new logic
   !!---- Returns on first error
   !!----
   !!---- Date: 19/07/2018
   !!
   Module Subroutine Physical_Check(EoS, Pin, Tin, Vin)
      !---- Arguments ----!
      type(Eos_Type),        intent(in) :: EoS  ! EoS object
      real(kind=cp),optional,intent(in) :: pin  ! Pressure
      real(kind=cp),optional,intent(in) :: vin  ! volume
      real(kind=cp),optional,intent(in) :: tin  ! Temperature

      !---- Local variables ----!
      integer             :: n
      character(len=100)  :: car
      real(kind=cp)       :: tlimit,pinf,p,v,t,vmin !,pmin
      type(eos_type)      :: e,eiso
      logical             :: vpresent

      !>local copies
      E=EoS
      T=e%tref

      !> check PVT present
      n=0
      if (present(Tin))then
         T=Tin
         n=n+1
      end if
      P=0._cp
      if (present(Pin))then
         P=Pin
         n=n+1
      end if

      !> Volume : This is needed for most tests of most EoS
      V=0._cp
      Vpresent=.false.
      if (present(Vin))then
         if (Vin < 0._cp) then
            call set_error(1,'Volume is negative')
            return
         end if
         V=Vin
         n=n+1
         Vpresent=.true.
      end if

      if (n == 0)return      !no arguments
      if (e%imodel > 0 .and. e%itherm > 0 .and. n < 2)return   ! not enough arguments for PT eos

      !> Positive T
      if (t < 0.0_cp) then
         call set_error(1,'T is less than zero K')
         return
      end if

      !> Now check for valid parameters at reference
      call EoSParams_Check(E)
      if (err_CFML%Flag) return

      !> Now check pthermal and isothermal seperately: Pthermal is first
      if (e%pthermaleos)then
          if (e%params(3) > 0._cp)then  ! K limit does not occur if Kp or Mp negative
             ! FIRST find the V at which K=K0/2 at Tref, WITHOUT using pressure
             eiso=e
             eiso%pthermaleos=.false.
             eiso%itherm=0
             vmin=get_volume_K(eiso%params(2)/2.0_cp,eiso%tref,eiso)
             if (vpresent)then
                if (v > vmin)then
                   err_CFML%Msg='Thermal pressure EoS not valid at this V and T: the V is too big so '
                   call set_error(1,trim(err_CFML%Msg)//'the compressional part of the EoS at Tref is not valid')
                   return
                end if
                if (k_cal(v,t,e) < tiny(0._cp))then
                   call set_error(1,'Thermal pressure EoS not valid at this V and T: the K is negative (maybe because of q large?)')
                   return
                end if

             else
                ! No volume input. So calculate the isochor Pressure of Vmin at the input T, and compare to input P
                if (get_k(p,t,e) < tiny(0._cp))then
                   call set_error(1,'Thermal pressure EoS not valid at this V and T: the K is negative (maybe because of q large?)')
                   return
                end if
                if (get_volume(p,t,e) > Vmin)then
                   err_CFML%Msg='Thermal pressure EoS not valid at this V and T: the V is too big so '
                   call set_error(1,trim(err_CFML%Msg)//'the compressional part of the EoS at Tref is not valid')
                   return
                end if
             end if
          end if

      else  !isothermal or no thermal: check thermal part first for T being valid
         !> Check validity of normal-type thermal model: only needs T
         select case(e%itherm)
            case (2)                ! Fei:
               if (e%params(12) > tiny(0.0_cp)) then  ! non-physical V and divergent alpha at low T when alpha2 .ne. 0
                  tlimit=(2.0_cp*e%params(12)/e%params(11))**(1.0_cp/3.0_cp)
                  if (t < tlimit) then
                     write(unit=car,fmt='(f5.1)')tlimit
                     car=adjustl(car)
                     call set_error(1,'Fei equation yields non-physical behaviour below T = '//trim(car)//'K')
                     return
                  end if

               else if(e%params(12) < tiny(0.0_cp)) then  ! alpha2 < 0
                  tlimit=sqrt(-1.0_cp*e%params(12)/e%params(10))
                  if (t < tlimit) then
                     write(unit=car,fmt='(f5.1)')tlimit
                     car=adjustl(car)
                     call set_error(1,'Fei equation yields non-physical behaviour below T = '//trim(car)//'K')
                     return
                  end if
               end if

            case(3)               ! HP 1998: trap non-physical behaviour at low T
               tlimit=((10.0_cp*e%params(10)+e%params(11))/e%params(10))**2.0_cp
               if (t < tlimit) then
                  write(unit=car,fmt='(f5.1)')tlimit
                  car=adjustl(car)
                  call set_error(1,'HP1998 equation yields non-physical behaviour below T = '//trim(car)//'K')
                  return
               end if
         end select

         !> Now check the validity of Eos params at T
         call pveos_check(e,P,V,T,vpresent)
         if (err_CFML%Flag) then
            err_CFML%Msg='Compressional EoS not valid at this PV: '//trim(err_CFML%Msg)
            return
         end if
      end if

      ! If got to here, now check that properties at P,T,V valid of Full EoS
      ! because  checks  above are for the PV part and the TV part, without transitions.
      ! all must be valid for the Eos to be valid

      if (e%itherm /=7 .and. e%itherm /=8 .and. e%itherm /=9 .and. .not. vpresent )then        !only done if V not provided at start
         v=get_volume(p,t,e)
         if (err_CFML%Flag) then         ! added 22/05/2017
            write(unit=car, fmt='(2f10.1)') p, t
            car=adjustl(car)
            err_CFML%Msg='Volume cannot be calculated at P,T = '//trim(car)
            return
         end if

         if (v < tiny(0.0) ) then
            write(unit=car, fmt='(2f10.1)') p, t
            car=adjustl(car)
            call set_error(1,'Volume calculated as zero or negative at P,T = '//trim(car))
            return
         end if
      end if

      if (.not. e%linear .and.  V > tiny(0._cp) .and. e%imodel /= 0)then
         if (K_cal(V,T,E,P) < tiny(0._cp))then
            write(unit=car, fmt='(2f10.1)') p, t
            car=adjustl(car)
            call set_error(1,'Bulk modulus calculated as zero or negative at P,T = '//trim(car))
            return
         end if
      end if

      !> Produce warning for curved phase boundaries: Pinflection = a/-2b when Ttr=Tr0+aP+bP^2
      if (e%itran>0 .and. abs(e%params(23)) > tiny(0.0) )then
         pinf=abs(e%params(22)/2.0/e%params(23))
         if (abs(p/pinf -1.0) < 0.1) then
            call set_error(1,'P in region of boundary inflection P: PVT calculations may be inaccurate or wrong')
         end if
      end if

   End Subroutine Physical_Check

   !!--++
   !!--++ SUBROUTINE PVEOS_CHECK
   !!--++
   !!--++ Checks compressional part of Eos at P,T for validity (normally that K > 0, or K > K0/2)
   !!--++ for volume
   !!--++ Does not do transition part
   !!--++
   !!--++ Update: 17/12/2018
   !!
   Module Subroutine PVEoS_Check(EoS, Pin, Vin, Tin, Vpresent)
      !---- Arguments ----!
      type(Eos_Type),          intent(in) :: Eos  ! EoS object
      real(kind=cp), optional, intent(in) :: pin  ! Pressure
      real(kind=cp), optional, intent(in) :: vin  ! volume
      real(kind=cp), optional, intent(in) :: tin  ! Temperature
      logical,                 intent(in) :: vpresent ! .true. when the Vin is meaningful

      !---- Local variables ----!
      real(kind=cp)       :: p,v,t
      !real(kind=cp),dimension(3)       :: abc
      real(kind=cp)       :: plim,klim,logterm,vv0,k0,kp !kc,bp,step,kprev,Vnew,Vprev
      type(eos_type)      :: e


      if (EoS%linear)return

      !>local copies
      e=EoS
      p=pin
      t=tin
      v=vin

      !set no transitions
      e%itran=0

      !This routine is private and only called from physical_check
      ! therefore if pthermaleos then T will always be Tref. But set it to be safe, and suppress all thermal part
      if (e%pthermaleos)then
         t=e%tref
         e%pthermaleos=.false.
         e%itherm=0
      end if

      k0=Get_K0_T(T,e)              ! Handles thermal pressure case, returns K0 or M0
      if (k0 < 0._cp) then
         call set_error(1,'K is negative at P=0 and this T')
         return
      end if
      kp=Get_Kp0_T(T,e)

      ! now do further tests dependening on Vpresent
      ! When V is present, calculate K from V,T
      ! And error state when K <  K(P=0,T)/2, except for Murnaghan which is stable to K=0
      if (vpresent)then
         if (v > Get_V0_T(T,E))then
            select case(e%imodel)
               case(1) ! Murngahan: limit is when K=0
                  if (p < -1.0_cp*k0/kp) call set_error(1,' ')

               case(2,3,4,5,6)   ! BM, Vinet, NS, Tait, APL
                  if (K_cal(V,T,E) < get_K0_T(T,E)/2.0) call set_error(1,' ')

               case(7)       !Kumar
                  vv0=v/Get_V0_T(T,E)
                  if (vv0*exp((kp+1)*(1-vv0)) < 0.5_cp) call set_error(1,' ')
            end select
         end if

      else      ! V was not given, but p was
         if (p < 0._cp)then
            select case(e%imodel)
               case(1) ! Murngahan
                  if (p + 1.0_cp*k0/kp < tiny(0.)) call set_error(1,' ')

               case(2,3,4,5,6) ! find V that gives K = K(P=0,T)/2, by iteration
                  Klim=get_K0_T(T,E)/2.0_cp
                  V=get_volume_K(Klim,e%tref,e)
                  plim=get_pressure(V,T,e)
                  if (p < plim)then
                     call set_error(1,' ')
                  end if

               case(7)       !Kumar
                  logterm=(kp+1)*p/k0 +1
                  if (logterm < tiny(0._cp)) then
                     call set_error(1,' ')
                     return
                  else
                     if ((1-log(logterm)/(kp+1))*logterm < 0.5_cp) call set_error(1,' ')
                  end if
            end select

         end if

      end if

      if (err_CFML%Flag) call set_error(1,'K < K0/2')

   End Subroutine PVEoS_Check

   !!----
   !!---- SUBROUTINE EOSPARAMS_CHECK
   !!----
   !!---- Check for Params that are invalid for all cases.
   !!----
   !!--.. NOTE:
   !!--..      Written 7-April-2014 to fix invalid eos parameters and report as error message
   !!--..      This is different from physical_check, because that checks properties at specific
   !!--..      p and T
   !!----      Later added warning state for non-fatal problems
   !!---- Date: 11/07/2016
   !!
   Module Subroutine EoSParams_Check(EoS)
      !---- Argument ----!
      type (EoS_Type), intent(in out) :: EoS

      !---- Local Variables ----!
      integer           :: i
      real(kind=cp)     :: pinf
      character(len=80) :: text

      !> Init
      call clear_error()

      !> Check for valid model numbers
      if (EoS%imodel < -1 .and. EoS%imodel > N_PRESS_MODELS) then
         call set_error(1,' Invalid number for type of compressional eos')
         return
      end if

      if (EoS%itherm < -1 .and. EoS%itherm > N_THERM_MODELS) then
         call set_error(1,' Invalid number for type of thermal model')
         return
      end if

      if (EoS%itran < -1 .and. EoS%itran > N_TRANS_MODELS) then
         call set_error(1,' Invalid number for type of phase transition model')
         return
      end if

      if (EoS%ishear < 0 .and. EoS%ishear > N_SHEAR_MODELS) then
         call set_error(1,' Invalid number for type of shear modulus model')
         return
      end if

      if (EoS%icross < 0 .and. EoS%icross > N_CROSS_MODELS) then
         call set_error(1,' Invalid number for type of PT cross-terms model')
         return
      end if

      if (EoS%iangle < 0 .and. EoS%iangle > N_ANGLE_MODELS) then
         call set_error(1,' Invalid number for type of angle polynomial')
         return
      end if

      call check_scales(eos)

      !> Check that v0 is positive
      if (EoS%params(1) < tiny(0.0) .and. EoS%iangle == 0) then
         EoS%params(1)=1.0_cp

         if (EoS%linear) then
            call set_error(1,' a0 was < 0. Not allowed! Reset to 1.00')
         else
            call set_error(1,' V0 was < 0. Not allowed! Reset to 1.00')
         end if
      end if

      !> Check K0 is positive for V-P (-ve K ok for linear)
      if (.not. EoS%linear .and. EoS%imodel > 0) then
         if (EoS%params(2) < tiny(0.0_cp) ) then
            EoS%params(2)=10.0_cp

            if (len_trim(err_CFML%Msg) == 0) then
               call set_error(1,' K0 was < 0. Not allowed! Reset to 10.0')
            else
               call set_error(1,trim(err_CFML%Msg)//' And K0 was < 0. Not allowed! Reset to 10.0')
            end if
         end if
      end if

      !> Check that Z> 0 for APL EoS
      if (EoS%imodel ==6) then
         if (EoS%params(5) < tiny(0.0_cp) ) then
            EoS%params(5)=1.0_cp

            if (len_trim(err_CFML%Msg) == 0) then
               call set_error(1,' Z was < 0. Not allowed! Reset to 1.0')
            else
               call set_error(1,trim(err_CFML%Msg)//' Z was < 0. Not allowed! Reset to 1.0')
            end if
         end if
      end if

      !> Check Tref is positive
      if (EoS%tref < -1.0_cp*tiny(0.0_cp)) then
         EoS%tref=0.0_cp

         if (len_trim(err_CFML%Msg) == 0) then
            call set_error(1,' Tref was < 0. Not allowed! Reset to 0 K')
         else
            call set_error(1,trim(err_CFML%Msg)//' And Tref was < 0. Not allowed! Reset to 0 K')
         end if
      end if

      !> Thermal cases
      select case(EoS%itherm)  ! for specific thermal parameters
         case (4,6,7,8)    !>Kroll or Pthermal must have characteristic T > 0.
            if (EoS%params(11) < 0.1) then
               EoS%params(11)=EoS%Tref
               if (EoS%Tref < 0.1) EoS%params=0.1

               if (len_trim(err_CFML%Msg) == 0) then
                  call set_error(1,trim(EoS%comment(11))//' was =< 0. Not allowed! Reset to Tref')
               else
                  call set_error(1,trim(err_CFML%Msg)//' And '//trim(EoS%comment(11))//' was =< 0. Not allowed! Reset to Tref')
               end if
            end if

            if(eos%itherm == 7 .or. eos%itherm ==8)then !thermal P require Natom, only Cv and alpha need this in HP2011
               if (eos%params(13) < 1.0) then

                  if (len_trim(err_CFML%Msg) == 0) then
                     call set_error(1,'Natom < 1.0 not valid: reset it!')
                  else
                     call set_error(1,trim(err_CFML%Msg)//' And Natom < 1.0 not valid: reset it!')
                  end if
               end if

            else if(eos%itherm == 6) then !only Cv and alpha need this in HP2011
               if (eos%params(13) < 1.0) then

                  if (len_trim(err_CFML%Msg) == 0) then
                     call set_error(-1,'Natom =0 so PVT  correct, but not heat capacities or calculated alpha')
                  else
                     call set_error(-1,trim(err_CFML%Msg)//' And Natom =0 so PVT  correct, but not heat capacities or calculated alpha')
                  end if
               end if
            endif

         end select

      !> Check q-comp switch
      if (EoS%itherm == 7 .or. EoS%itherm == 8) then
         if (EoS%params(14) < 0._cp) then
            EoS%params(14)=0.0_cp

         else if(EoS%params(14) > 1._cp) then
            EoS%params(14)=1.0_cp
         end if
      end if

      !> Extra oscillator models
      if (EoS%iosc(1) >0 .and. EoS%params(41) < 0.1_cp) then
         EoS%params(41)=EoS%Tref

         if (len_trim(err_CFML%Msg) == 0) then
            call set_error(1,trim(EoS%parname(41))//'for 2nd oscillator was =< 0. Not allowed! Reset to Tref')
         else
            call set_error(1,trim(err_CFML%Msg)//' And '//trim(EoS%parname(41))//'for 2nd oscillator was =< 0. Not allowed! Reset to Tref')
         end if
      end if

      if (EoS%iosc(2) >0 .and. EoS%params(46) < 0.1_cp) then
         EoS%params(46)=EoS%Tref

         if (len_trim(err_CFML%Msg) == 0) then
            call set_error(1,trim(EoS%parname(46))//'for 3rd oscillator was =< 0. Not allowed! Reset to Tref')
         else
            call set_error(1,trim(err_CFML%Msg)//' And '//trim(EoS%parname(46))//'for 3rd oscillator was =< 0. Not allowed! Reset to Tref')
         end if
      end if

      if (EoS%iosc(1) > 0 .and. EoS%params(40)+EoS%params(45) > 1.0) then

         if (len_trim(err_CFML%Msg) == 0) then
            call set_error(1,'Mode fractions of extra oscillators reset to sum to 1')
         else
            call set_error(1,trim(err_CFML%Msg)//' And mode fractions of extra oscillators reset to sum to 1')
         end if

         if (EoS%params(40) > 1.0_cp)then
            EoS%params(40)=1.0_cp
            EoS%params(45)=0.0_cp
         else
            EoS%params(45)=1.0_cp-EoS%params(40)
         end if
      end if

      !> Scale factors for data must be positive
      text=' '
      do i=51,59
         if (EoS%iuse(i) > 0 .and. EoS%params(i) < tiny(0._cp)) then
            text='Data scale factor =< 0., reset to 1.0'
            EoS%params(i)=1.0_cp
         end if
      end do

      if (len_trim(text) /= 0)then
         if (len_trim(err_CFML%Msg) == 0)then
            call set_error(1,trim(text))
         else
            call set_error(1,trim(err_CFML%Msg)//' and '//trim(text))
         end if
      end if

      !> Produce warning for curved phase boundaries: Pinflection = a/-2b when Ttr=Tr0+aP+bP^2
      if (EoS%itran>1 .and. abs(EoS%params(24)) > tiny(0.0) ) then
         pinf=-1.0*EoS%params(22)/2.0/EoS%params(24)
         if (abs(pinf) < 10.0) then

            write(text,'(a,f4.1,1x,a)')'Phase boundary inflects at P ~ ',pinf,trim(EoS%pscale_name)
            if (len_trim(err_CFML%Msg) == 0) then
               call set_error(1,trim(text))
            else
               call set_error(1,trim(err_CFML%Msg)//' and '//trim(text))
            end if
         end if
      end if

      !> If MGD and linear warn that this is not generally valid
      if ((EoS%itherm == 6 .or. EoS%itherm == 7 .or. EoS%itherm == 8) .and. EoS%linear) then

         text='Linear EoS only has valid parameters if the material is cubic'
         if (len_trim(err_CFML%Msg) == 0) then
            call set_error(-1,' *****WARNING: '//trim(text))
        else
           call set_error(-1,trim(err_CFML%Msg)//' And '//trim(text) )
        end if
      end if

   End Subroutine EoSParams_Check

End SubModule EoS_Checks