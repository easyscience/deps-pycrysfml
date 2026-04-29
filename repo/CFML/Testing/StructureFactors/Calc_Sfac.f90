!!----
!!---- Program: CALC_SFAC
!!----          Example of simple program using CFML2008
!!----
!!---- Author: Juan Rodriguez-Carvajal
!!---- Revision: April 2022
!!
Program Calc_Structure_Factors
   !---- Use Modules ----!
   use CFML_GlobalDeps,                only: cp,Err_CFML, clear_error
   use CFML_Strings,                   only: U_Case, File_Type
   use CFML_gSpaceGroups,              only: SPG_Type, Write_SpaceGroup_Info, get_moment_ctr
   use CFML_Atoms,                     only: AtList_Type, Write_Atom_List
   use CFML_Metrics,                   only: Cell_G_Type, Write_Crystal_Cell
   use CFML_Reflections,               only: Refl_Type,SRefl_Type, RefList_Type, Initialize_RefList, Gener_Reflections, &
                                             Get_MaxNumRef, H_Uni,Gener_Reflections_Shub
   use CFML_IOForm,                    only: Read_Xtal_Structure
   use CFML_Structure_Factors,         only: StrfList_Type,Init_Structure_Factors, Structure_Factors, &
                                             Calc_StrFactor, Magnetic_Structure_Factors,Write_Structure_Factors, Write_Structure_Factors_Mag

   !---- Variables ----!
   implicit none

   type (File_Type)                 :: fich_cfl
   type (AtList_Type)               :: A
   class (Cell_G_Type), allocatable :: Cell
   type (RefList_Type)              :: hkl
   class (SPG_Type),    allocatable :: SpG

   character(len=132)          :: line
   character(len=256)          :: filcod     ! Name of the input file
   character(len=15)           :: sinthlamb  ! String with stlmax (2nd cmdline argument)
   real(kind=cp)               :: stlmax     ! Maximum Sin(Theta)/Lambda
   real(kind=cp)               :: sn, sf2, Lambda, tini,tend
   integer                     :: lun, ier, i !MaxNumRef,
   complex                     :: fc
   integer                     :: narg
   logical                     :: arggiven=.false.,sthlgiven=.false.
   logical                     :: lfil1,lfil2,lfil3

   !> Parte Magnetica
   integer                    :: codini=0, icodes(3)
   real(kind=cp), dimension(3):: multip
   type (StrfList_Type)       :: Stf

   !> Arguments on the command line
   narg=command_argument_count()

   if (narg > 0) then
      call get_command_argument(1,filcod)
      i=index(filcod,".")
      if (i/= 0) filcod=filcod(1:i-1)
      arggiven=.true.
   end if

   if (narg > 1) then
      call get_command_argument(2,sinthlamb)
      read(unit=sinthlamb,fmt=*,iostat=ier) stlmax
      if (ier == 0) sthlgiven=.true.
   end if

   !> Header of the Program
   write(unit=*,fmt="(/,/,6(a,/))")                                                  &
        "            ------ PROGRAM STRUCTURE FACTORS ------"                      , &
        "                ---- Version 1.0 April-2022 ----"                         , &
        "    *******************************************************************"  , &
        "    * Calculates structure factors reading a CFL, CIF or MCIF file    *"  , &
        "    *******************************************************************"  , &
        "                      (JRC- April 2022 )"
   write(unit=*,fmt=*) " "

   if (.not. arggiven) then
      write(unit=*,fmt="(a)", advance='no') " => Code of the file xx.(cif|mcif|cfl) (give xx): "
      read(unit=*,fmt="(a)") filcod
      if (len_trim(filcod) == 0) stop
   end if

   if (.not. sthlgiven) then
      write(unit=*,fmt="(a)", advance='no') " => Maximum sinTheta/Lambda: "
      read(unit=*,fmt=*) stlmax
   end if

   call cpu_time(tini)
   open(newunit=lun,file=trim(filcod)//".sfa", status="replace",action="write")
   write(unit=lun,fmt="(/,/,6(a,/))")                                                  &
          "            ------ PROGRAM STRUCTURE FACTORS ------"                      , &
          "                ---- Version 1.0 April-2022----"                          , &
          "    *******************************************************************"  , &
          "    * Calculates structure factors reading a CFL, CIF or MCIF file    *"  , &
          "    *******************************************************************"  , &
          "                      (JRC- April 2022 )"

   !> Asking for files...
   inquire(file=trim(filcod)//".cif", exist=lfil1)
   inquire(file=trim(filcod)//".mcif",exist=lfil2)
   inquire(file=trim(filcod)//".cfl", exist=lfil3)

   if ( (.not. lfil1) .and. (.not. lfil2) .and. (.not. lfil3)) then
      write(unit=*,fmt="(a)") " File: "//trim(filcod)//".(cif|mcif|cfl)  does'nt exist!"
      close(unit=lun)
      stop
   end if

   if (lfil1) then
      call Read_Xtal_Structure(trim(filcod)//".cif", Cell, SpG, A)

   else if (lfil2) then
      call Read_Xtal_Structure(trim(filcod)//".mcif",Cell,SpG, A)

   else if (lfil3) then
      call Read_Xtal_Structure(trim(filcod)//".cfl",Cell, SpG, A, FType=fich_cfl)
   end if

   if (err_CFML%Ierr /= 0) then
      write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      close(unit=lun)
      stop
   end if

   !> Cell parameters
   call Write_Crystal_Cell(Cell,lun)

   !> Spacegroup
   call Write_SpaceGroup_Info(SpG,lun)

   !> Atoms
   if (Spg%Magnetic) then
      !> Get information on moment constraints and modify the list of atoms accordingly
      do i=1,A%natoms
         icodes=[1,2,3]
         if (A%Atom(i)%mom < 0.001) cycle  ! Skip non-magnetic atoms
         call Get_Moment_CTR(A%Atom(i)%x,A%Atom(i)%moment,SpG,codini,icodes,Multip)
      end do
   end if

   call Write_Atom_List(A,1,lun)

   !> Wavelength in a CFL file
   lambda=0.70926 !Mo kalpha (used only for x-rays)
   if (lfil3) then
      do i=1,fich_cfl%nlines
         line=adjustl(fich_cfl%line(i)%str)
         if (u_case(line(1:6)) == "LAMBDA") then
            read(unit=line(7:),fmt=*,iostat=ier) lambda
            if (ier /= 0) lambda=0.70926
         end if
      end do
   end if

   !> Creating a list of reflections

   !call Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,IPhase,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ,kout)
   call Gener_Reflections(Cell,0.0,stlmax,hkl,SpG,1,.false.,Unique=.true.,Friedel=.true.,Ref_typ='SRefl')

   if (hkl%Nref <=0 ) then
      print*, " Problems generating a list of reflections!"
      stop
   end if

   !> Calculation for neutron scattering
   call clear_error()
   call Init_Structure_Factors(hkl, A, Spg, 'NUC', lun=lun)
   if (err_CFML%Ierr /= 0) then
      write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      close(unit=lun)
      stop
   end if
   call Structure_Factors(hkl, A, SpG, 'NUC')    ! Calling Structure_Factors
   call Write_Structure_Factors(hkl, lun, 'NUC') !Writing Structure_Factors
   if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)

   !> Test of another structure factor subroutine
   write(unit=lun,fmt="(/,a,/)") " => Calculation using subroutine: Calc_StrFactor"
   write(unit=lun,fmt="(/,a,/)") "   H   K   L   Mult  SinTh/Lda    |Fc|       Phase        F-Real      F-Imag      Num"
   select type (rr => hkl%ref)
      type is (srefl_type)
         do i=1, hkl%nref
            sn=rr(i)%s * rr(i)%s
            call Calc_StrFactor(i,sn,A,Spg,'P','N',sf2,fc=fc)
            write(unit=lun,fmt="(3i4,i5,5f12.5,i8,f12.5)") &
                  rr(i)%h, rr(i)%mult, rr(i)%S, rr(i)%Fc, &
                  rr(i)%Phase, real(fc), aimag(fc), i, sqrt(sf2)
         end do
   end select

   !> Calculation for X-rays assume Mo-kalpha if Lambda not given
   call Init_Structure_Factors(hkl, A, Spg, 'XRay', lambda, lun)
   if (err_CFML%Ierr /= 0) then
      write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      close(unit=lun)
      stop
   end if
   call Structure_Factors(hkl, A, SpG)
   call Write_Structure_Factors(hkl,lun)
   if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)


   !> Calculation for Electron Diffraction
   call clear_error()
   call Init_Structure_Factors(hkl, A, Spg, 'ELE',lun=lun)
   if (err_CFML%Ierr /= 0) then
      write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      close(unit=lun)
      stop
   end if
   call Structure_Factors(hkl, A, SpG,'ELE')
   call Write_Structure_Factors(hkl,lun, 'ELE')
   if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)

   !> Magnetic structure factors
   if (Spg%Magnetic) then
      call clear_error()
      call Magnetic_Structure_Factors(hkl, Cell, A, SpG, stlmax, Stf, lun=lun)
      if (err_CFML%Ierr /= 0) then
         write(unit=*,fmt="(a)") trim(err_CFML%Msg)
         close(unit=lun)
         stop
      end if
      call Write_Structure_Factors_Mag(hkl, Stf, Lun, .true.)
   end if
   call cpu_time(tend)


   write(unit=*,fmt="(a)") " Normal End of: PROGRAM STRUCTURE FACTORS "
   write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".sfa"
   write(unit=*,fmt="(a,f10.5,a)") " CPU-time: ",tend-tini," seconds"

   close(unit=lun)
End Program Calc_Structure_Factors

