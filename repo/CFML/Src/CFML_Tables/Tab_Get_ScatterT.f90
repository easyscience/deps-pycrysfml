!----
!!----
!!----
SubModule (CFML_Scattering_Tables) TAB_Get_ScatterT
   Implicit none
  Contains

  !!----
  !!---- GET_ABS_XS
  !!----    Provides the absorption cross-section ( barns, for v= 2200m/s, l(A)=3.95/v (km/s) )
  !!----    for given chemical symbol of the element.
  !!----    In case of problems the returned value is 0.0.
  !!----
  !!---- 15/04/2019
  !!
  Module Function Get_Abs_Xs(Symb) Result(u)
     !---- Arguments ----!
     character(len=*), intent(in) :: Symb
     real(kind=cp)                :: u

     !---- Local variables ----!
     character(len=2) :: atm_car
     integer          :: i

     !> Init
     u=0.0_cp
     if (.not. allocated(chem_info) ) call set_chem_info()

     atm_car=u_case(symb(1:2))
     if (atm_car(2:2) > "Z" .or. atm_car(2:2) < "A") atm_car(2:2)=" "
     do i=1,NUM_CHEM_INFO
        if (index(atm_car,chem_info(i)%Symb) /=0) then
           u=chem_info(i)%Sea
           exit
        end if
     end do

  End Function Get_Abs_Xs

   !!----
   !!---- GET_ANOMALOUS_SCFAC
   !!----    Function which returns a row from the anomalous_scfac table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Anomalous_ScFac(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(anomalous_sc_type)         :: info   ! Anomalous scattering factors

      !> Init
      if (r > 0 .and. r <= NUM_DELTA_FP) then
         if (.not. allocated(anomalous_scfac)) call set_delta_fp_fpp()
         info = anomalous_scfac(r)
      end if

   End Function Get_Anomalous_ScFac

   !!----
   !!---- GET_ATOMIC_MASS
   !!----    Provides the atomic mass given the chemical symbol of the element
   !!----    In case of problems the returned mass is ZERO.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Atomic_Mass(Symb) Result(mass)
      !---- Arguments ----!
      character(len=*), intent (in) :: Symb     ! Symbol of the atopic species
      real(kind=cp)                 :: Mass

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i

      !> Init
      mass=0.0_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(Symb)
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            mass=chem_info(i)%AtWe
            exit
         end if
      end do

   End Function Get_Atomic_Mass

   !!----
   !!---- GET_ATOMIC_VOL
   !!----    Provides the atomic volume given the chemical symbol of the element
   !!----    In case of problems the returned Volume is ZERO.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Atomic_Vol(Symb) Result(vol)
      !---- Arguments ----!
      character(len=*), intent (in) :: Symb
      real(kind=cp)                 :: Vol

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i

      !> Init
      vol=0.0_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(Symb)
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            vol=chem_info(i)%VAtm
            exit
         end if
      end do

   End Function Get_Atomic_Vol

   !!----
   !!---- GET_CHEM_INFO
   !!----    Function which returns a row from the chem_info table
   !!----
   !!---- 11/09/2024
   !!
   Module Function Get_Chem_Info(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(chem_info_type)            :: info   ! Chemical info

      !> Init
      if (r > 0 .and. r <= NUM_CHEM_INFO) then
         if (.not. allocated(chem_info)) call set_chem_info()
         info = chem_info(r)
      end if

   End Function Get_Chem_Info

   !!----
   !!---- GET_CHEMSYMB
   !!----    Subroutine to get the chemical symbol from label
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Chem_Symb(Label) Result(Symb)
      !---- Argument ----!
      character(len=*),  intent(in) :: Label    ! Label
      character(len=2)              :: Symb     ! Chemical Symbol

      !---- Local variables ----!
      character(len=*),  parameter :: parcar="1234567890+-."
      character(len=2)             :: car
      integer                      :: npos

      !> Init
      Symb="**"
      car=adjustl(label)
      npos=index(parcar,car(2:2))
      if (npos /=0) car(2:2)=" "
      car=u_case(car)
      car(2:2)=l_case(car(2:2))

      Symb=car

   End Function Get_Chem_Symb

   !!----
   !!---- GET_COVALENT_RADIUS
   !!----    Provides the covalent radius given the chemical symbol of the element
   !!----    In case of problems the returned radius is 1.4 angstroms.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Covalent_Radius(Symb) Result(rad)
      !---- Arguments ----!
      character(len=*), intent (in) :: Symb
      real(kind=cp)                 :: rad

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i

      !> Init
      rad=1.4_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(symb(1:2))
      if (atm_car(2:2) > "Z" .or. atm_car(2:2) < "A") atm_car(2:2)=" "
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            rad=chem_info(i)%RCov
            exit
         end if
      end do

   End Function Get_Covalent_Radius

   !!----
   !!---- GET_FERMI_LENGTH
   !!----    Provides the Fermi length (in 10-12 cm) given the chemical
   !!----    symbol of the element. In case of problems the returned Fermi
   !!----    length is 0.0 10-12 cm.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Fermi_Length(Symb) Result(b)
      !---- Arguments ----!
      character(len=*), intent(in) :: Symb
      real(kind=cp)                :: b

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i

      !> Init
      b=0.0_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(symb(1:2))
      if (atm_car(2:2) > "Z" .or. atm_car(2:2) < "A") atm_car(2:2)=" "
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            b=chem_info(i)%SctF
            exit
         end if
      end do

   End Function Get_Fermi_Length

   !!----
   !!---- GET_INC_XS
   !!----    Provides incoherent scattering neutron cross-section (barns -> [10**(-24) cm**2] )
   !!----    for given chemical symbol of the element.
   !!----    In case of problems the returned value is 0.0.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Inc_Xs(Symb) Result(u)
      !---- Arguments ----!
      character(len=*), intent(in) :: Symb
      real(kind=cp)                :: u

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i

      !> Init
      u=0.0_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(symb(1:2))
      if (atm_car(2:2) > "Z" .or. atm_car(2:2) < "A") atm_car(2:2)=" "
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            u=chem_info(i)%SedInc
            exit
         end if
      end do

   End Function Get_Inc_Xs

   !!----
   !!---- GET_IONIC_RADIUS
   !!----    Provides the ionic radius given the chemical symbol of the element
   !!----    and the valence as an integer.
   !!----    In case of problems the returned radius is 0.0 angstroms.
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Ionic_Radius(Symb,Valence) Result(rad)
      !---- Arguments ----!
      character(len=*), intent (in) :: Symb
      integer,          intent (in) :: valence
      real(kind=cp)                 :: rad

      !---- Local variables ----!
      character(len=2) :: atm_car
      integer          :: i,j

      !> Init
      rad=0.0_cp
      if (.not. allocated(chem_info) ) call set_chem_info()

      atm_car=u_case(symb(1:2))
      if (atm_car(2:2) > "Z" .or. atm_car(2:2) < "A") atm_car(2:2)=" "
      do i=1,NUM_CHEM_INFO
         if (index(atm_car,chem_info(i)%Symb) /=0) then
            do j=1,5
               if (valence == chem_info(i)%oxid(j)) then
                  rad=chem_info(i)%Rion(j)
                  exit
               end if
            end do
         end if
      end do

   End Function Get_Ionic_Radius

   !!----
   !!---- GET_MAGNETIC_FORM
   !!----    Function which returns the row r the magnetic_form table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Magnetic_Form(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(magnetic_form_type)        :: info   ! Magnetic form factors

      !> Init
      if (r > 0 .and. r <= NUM_MAG_FORM) then
         if (.not. allocated(magnetic_form)) call set_magnetic_form()
         info = magnetic_form(r)
      end if

   End Function Get_Magnetic_Form

   !!----
   !!---- GET_MAGNETIC_J2
   !!----    Function which returns the row r from the magnetic_j2 table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Magnetic_J2(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(magnetic_form_type)        :: info   ! Magnetic form factors J2

      !> Init
      if (r > 0 .and. r <= NUM_MAG_J2) then
         if (.not. allocated(magnetic_j2)) call set_magnetic_form()
         info = magnetic_j2(r)
      end if

   End Function Get_Magnetic_J2

   !!----
   !!---- GET_MAGNETIC_J4
   !!----    Function which returns the row r from the magnetic_j4 table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Magnetic_J4(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(magnetic_form_type)        :: info   ! Magnetic form factors J4

      !> Init
      if (r > 0 .and. r <= NUM_MAG_J4) then
         if (.not. allocated(magnetic_j4)) call set_magnetic_form()
         info = magnetic_j4(r)
      end if

   End Function Get_Magnetic_J4

   !!----
   !!---- GET_MAGNETIC_J6
   !!----    Function which returns the row r from the magnetic_j6 table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Magnetic_J6(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(magnetic_form_type)        :: info   ! Magnetic form factors J6

      !> Init
      if (r > 0 .and. r <= NUM_MAG_J6) then
         if (.not. allocated(magnetic_j6)) call set_magnetic_form()
         info = magnetic_j6(r)
      end if

   End Function Get_Magnetic_J6

   !!----
   !!---- GET_XRAY_FORM
   !!----    Function which returns the row r from the xray_form table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Xray_Form(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(xray_form_type)            :: info   ! Xray form factors

      !> Init
      if (r > 0 .and. r <= NUM_XRAY_FORM) then
         if (.not. allocated(xray_form)) call set_xray_form()
         info = xray_form(r)
      end if

   End Function Get_Xray_Form

   !!----
   !!---- GET_XRAY_WAVELENGTHS
   !!----    Function which returns the row r from the xray_wavelengths table
   !!----
   !!---- 12/09/2024
   !!
   Module Function Get_Xray_Wavelengths(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(xray_wavelength_type)      :: info   ! Xray form factors

      !> Init
      if (r > 0 .and. r <= 7) then
         info = xray_wavelengths(r)
      end if

   End Function Get_Xray_Wavelengths

   !!----
   !!---- GET_ZSYMB
   !!----    Procedure to get the atomic number of chemical symbol from label
   !!----
   !!---- 15/04/2019
   !!
   Module Function Get_Z_Symb(Symb) Result(Z)
      !---- Argument ----!
      character(len=*),  intent(in):: Symb     ! Chemical Symbol
      integer                      :: Z        ! Atomic number

      !---- Local variables ----!
      character(len=2)             :: car
      integer                      :: npos

      !> Init
      z=0
      if (.not. allocated(chem_info) ) call set_chem_info()

      car=u_case(symb)
      if (car(2:2) > "Z" .or. car(2:2) < "A") car(2:2)=" "
      do npos=1,NUM_CHEM_INFO
         if (car == Chem_Info(npos)%Symb) then
            Z=Chem_Info(npos)%Z
            exit
         end if
      end do

   End Function Get_Z_Symb

End SubModule TAB_Get_ScatterT