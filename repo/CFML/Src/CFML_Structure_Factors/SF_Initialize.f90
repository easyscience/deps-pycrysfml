Submodule (CFML_Structure_Factors) SF_Initialize
   !---- Variables ----!
   implicit none

 Contains

   !!----
   !!---- SUBROUTINE INIT_STRUCTURE_FACTORS
   !!----
   !!----    Allocates and initializes arrays for Structure Factors calculations.
   !!----    A calculation of fixed tables is also performed.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_Structure_Factors(Reflex, Atm, Grp, Mode, Lambda, Lun)
      !---Arguments ---!
      class(RefList_Type),         intent(in) :: Reflex
      type(AtList_type),           intent(in) :: Atm
      type(SpG_type),              intent(in) :: Grp
      character(len=*),  optional, intent(in) :: Mode
      real(kind=cp),     optional, intent(in) :: lambda
      integer,           optional, intent(in) :: lun

      !--- Local variables ---!
      integer :: Natm, Multr
      integer :: ierr

      !> Init
      call clear_error()

      Natm = Atm%natoms
      Multr= Grp%Numops
      if(.not. init_symOP) call SF_init_opMatTr(Grp)

      !> Scattering factor tables
      if (allocated(AF0)) deallocate(AF0)
      allocate(AF0(Natm,Reflex%Nref),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for AF0 allocating!"
         return
      end if
      AF0=0.0_cp

      !> Anomalous Scattering factor tables
      if (allocated(AFP)) deallocate(AFP)
      allocate(AFP(Natm),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for AFP allocating!"
         return
      end if
      AFP=0.0_cp

      if (allocated(AFPP)) deallocate(AFPP)
      allocate(AFPP(Natm),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for AFPP allocating!"
         return
      end if
      AFPP=0.0_cp

      !> HR Table
      if (allocated(HR)) deallocate(HR)
      allocate(HR(Multr,Reflex%Nref),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for HR allocating!"
         return
      end if
      HR=HR_Type(0.0_cp)

      !---- HT Table ----!
      if (allocated(HT)) deallocate(HT)
      allocate(HT(Multr,Reflex%Nref),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for HTR allocating!"
         return
      end if
      HT=0.0_cp

      if (allocated(TH)) deallocate(TH)
      allocate(TH(Natm,Reflex%Nref),stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         err_CFML%Msg="Error on memory for TH allocating!"
         return
      end if
      TH=0.0_cp

      if (allocated(Ajh)) deallocate(Ajh)
      allocate(Ajh(Natm,Reflex%Nref), stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for Aj(h) allocating!"
         return
      end if
      Ajh=0.0_cp

      if (allocated(Bjh)) deallocate(Bjh)
      allocate(Bjh(Natm,Reflex%Nref), stat=ierr)
      if (ierr /=0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for Bj(h) allocating!"
         return
      end if
      Bjh=0.0_cp

      if (present(mode)) then
         if (present(lambda)) then
            if (present(lun)) then
               call Set_Fixed_Tables(Reflex,Atm,Grp,Mode,lambda,lun)
            else
               call Set_Fixed_Tables(Reflex,Atm,Grp,Mode,lambda)
            end if
         else
            if (present(lun)) then
               call Set_Fixed_Tables(Reflex,Atm,Grp,Mode,lun=lun)
            else
               call Set_Fixed_Tables(Reflex,Atm,Grp,Mode)
            end if
         end if
      else
         if (present(lambda)) then
            if (present(lun)) then
               call Set_Fixed_Tables(Reflex,Atm,Grp,lambda=lambda,lun=lun)
            else
               call Set_Fixed_Tables(Reflex,Atm,Grp,lambda=lambda)
            end if
         else
            if (present(lun)) then
               call Set_Fixed_Tables(Reflex,Atm,Grp,lun=lun)
            else
               call Set_Fixed_Tables(Reflex,Atm,Grp)
            end if
         end if
      end if

      if (err_CFML%IErr ==0) SF_Initialized=.true.

   End Subroutine Init_Structure_Factors

   !!----
   !!---- subroutine init_calc_strfactors
   !!----
   !!----    Allocates and initializes arrays for Calc_StrFactors calculations.
   !!----    Calculations of fixed tables are performed. Should be called before using
   !!----    the subroutine Calc_StrFactor
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_Calc_StrFactors(Reflex, Atm, Grp, Mode, Lambda, Lun)
      !---Arguments ---!
      class(RefList_Type),        intent(in) :: Reflex
      type(AtList_type),          intent(in) :: Atm
      type(SpG_type),             intent(in) :: Grp
      character(len=*), optional, intent(in) :: Mode
      real(kind=cp),    optional, intent(in) :: lambda
      integer,          optional, intent(in) :: lun


      call Init_Structure_Factors(Reflex,Atm,Grp,Mode,lambda,lun)
      if (err_CFML%Ierr /=0) return

      call Calc_Table_TH(Reflex,Atm)

   End Subroutine Init_Calc_StrFactors

   !!----
   !!---- SUBROUTINE INIT_CALC_HKL_STRFACTORS
   !!----
   !!----    Allocates and initializes arrays for hkl - Structure Factors calculations.
   !!----    No calculation of fixed tables is performed. Should be called before using
   !!----    the subroutine Calc_hkl_StrFactor
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_Calc_hkl_StrFactors(Atm, Grp, Mode, Lambda, Lun)
      !---Arguments ---!
      type(AtList_type),           intent(in) :: Atm
      type(SpG_type),              intent(in) :: Grp
      character(len=*),  optional, intent(in) :: Mode
      real(kind=cp),     optional, intent(in) :: lambda
      integer,           optional, intent(in) :: lun

      !--- Local variables ---!
      integer :: Natm
      integer :: ierr
      character(len=3) :: tipo

      !> Init
      call clear_error()

      tipo="XRA"
      if (present(mode)) tipo=adjustl(mode)

      tipo=U_Case(tipo)
      Natm = Atm%natoms
      if(.not. init_symOP) call SF_init_opMatTr(Grp)

      !> Anomalous Scattering factor tables
      if (allocated(AFP)) deallocate(AFP)
      allocate(AFP(Natm),stat=ierr)
      if (ierr /=0) then
         err_CFML%Ierr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for AFP allocating!"
         return
      end if
      AFP=0.0_cp

      if (allocated(AFPP)) deallocate(AFPP)
      allocate(AFPP(Natm),stat=ierr)
      if (ierr /=0) then
         err_CFML%Ierr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Error on memory for AFPP allocating!"
         return
      end if
      AFPP=0.0_cp

      !> Table Fabc
      select case (tipo)
         case ("XRA")
            if (present(lambda)) then
               if (present(lun)) then
                  call Create_Table_Fabc_Xray(Atm,lambda,lun)
               else
                  call Create_Table_Fabc_Xray(Atm,lambda)
               end if
            else
               if (present(lun)) then
                  call Create_Table_Fabc_Xray(Atm,lun=lun)
               else
                  call Create_Table_Fabc_Xray(Atm)
               end if
            end if

         case ("ELE")
            if (present(lun)) then
               call Create_Table_Fabc_Xray(Atm,lun=lun)
            else
               call Create_Table_Fabc_Xray(Atm)
            end if


         case ("NUC")
            if (present(lun)) then
               call Create_Table_AFP_NeutNuc(Atm,lun=lun)
            else
               call Create_Table_AFP_NeutNuc(Atm)
            end if

      end select

      if (err_CFML%IErr ==0) SF_Initialized=.true.

   End Subroutine Init_Calc_hkl_StrFactors

   !!--++
   !!--++ SUBROUTINE SET_FIXED_TABLES
   !!--++    Calculates arrays that are fixed during all further
   !!--++    calculations
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Set_Fixed_Tables(Reflex, Atm, Grp, Mode, Lambda, Lun)
      !---- Arguments ----!
      class(RefList_Type),        intent(in) :: Reflex
      type(AtList_type),          intent(in) :: Atm
      type(SpG_type),             intent(in) :: Grp
      character(len=*), optional, intent(in) :: Mode
      real(kind=cp),    optional, intent(in) :: lambda
      integer,          optional, intent(in) :: lun

      !---- Local variables ----!
      character(len=3) :: tipo

      tipo="XRA"
      if (present(mode)) tipo=adjustl(mode)
      tipo=U_Case(tipo)

      !> Table HR - HT
      call Create_Table_HR_HT(Reflex,Grp)

      !>Table AF0
      select case (tipo)
         case ("XRA")
            if (present(lambda)) then
               if (present(lun)) then
                  call Create_Table_AF0_Xray(Reflex,Atm,lambda,lun)
               else
                  call Create_Table_AF0_Xray(Reflex,Atm,lambda)
               end if
            else
               if (present(lun)) then
                  call Create_Table_AF0_Xray(Reflex,Atm,lun=lun)
               else
                  call Create_Table_AF0_Xray(Reflex,Atm)
               end if
            end if
            if (err_CFML%Ierr /=0) return

            !> Modify the scattering factor tables to include the
            !> multipliers factors concerning centre of symmetry and
            !> centred translations
            if (Grp%Centred == 2) then
               af0=2.0_cp*af0
               afpp=2.0_cp*afpp
            end if

            if (Grp%Num_Lat  > 1) then
               af0=Grp%Num_Lat*af0
               afpp=Grp%Num_Lat*afpp
            end if

         case ("ELE")
            if (present(lun)) then
               call Create_Table_AF0_Electrons(Reflex,Atm,lun=lun)
            else
               call Create_Table_AF0_Electrons(Reflex,Atm)
            end if
            if (err_CFML%Ierr /=0) return

            !> Modify the scattering factor tables to include the
            !> multipliers factors concerning centre of symmetry and
            !> centred translations
            if (Grp%Centred == 2) then
               af0=2.0_cp*af0
               afpp=0.0_cp
            end if

            if (Grp%Num_Lat  > 1) then
               af0=Grp%Num_Lat*af0
               afpp=0.0_cp
            end if

            diff_electrons=.true.

         case ("NUC","NEU")
            if (present(lun)) then
               call Create_Table_AFP_NeutNuc(Atm,lun=lun)
            else
               call Create_Table_AFP_NeutNuc(Atm)
            end if
            if (err_CFML%Ierr /=0) return

            if (Grp%Centred == 2) afp=2.0_cp*afp
            if (Grp%Num_Lat  > 1) afp=Grp%Num_Lat*afp

      end select

   End Subroutine Set_Fixed_Tables

End SubModule SF_Initialize