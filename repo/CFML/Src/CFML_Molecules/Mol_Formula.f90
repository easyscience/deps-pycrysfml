Submodule (CFML_Molecules) Mol_Formula

   implicit none

 Contains
   !!--++
   !!--++ Subroutine Empiric_Formula_FAtom
   !!--++
   !!--++    Obtain the Empiric Formula from Atm variable
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Empiric_Formula_AtList(Atm, Formula, Form_Weight)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: Atm
      character(len=*),        intent(out) :: Formula
      real(kind=cp), optional, intent(out) :: Form_Weight

      !---- Local variables ----!
      character(len=2)                  :: car
      character(len=5)                  :: numcar
      integer                           :: i,j
      integer, dimension(NUM_CHEM_INFO) :: N_PT
      real(kind=cp)                     :: weight

      !> Init
      N_PT=0
      weight=0.0_cp

      Formula=" "
      if (Atm%natoms <= 0) then
         if (present(Form_weight)) Form_weight=0.0_cp
         return
      end if

      !> Set Information Table
      call Set_Chem_Info()

      do i=1,atm%natoms
         car=atm%atom(i)%chemsymb
         car=u_case(car)
         do j=1,NUM_CHEM_INFO
            if (car == Chem_Info(j)%Symb) then
               n_pt(j)=n_pt(j)+1
               exit
            end if
         end do
      end do

      if (all (n_pt ==0)) then
         if (present(Form_weight)) Form_weight=0.0_cp
         call Remove_Chem_Info()
         return
      end if

      do i=1,NUM_CHEM_INFO
         if (n_pt(i) == 0) cycle

         car=Chem_Info(i)%Symb
         car(2:2)=l_case(car(2:2))
         write(unit=numcar,fmt="(i5)") n_pt(i)
         Formula=trim(Formula)//trim(car)//adjustl(numcar)
         weight=weight+n_pt(i)*Chem_Info(i)%atwe
      end do

      call Remove_Chem_Info()

      if (present(Form_weight)) Form_weight=weight

   End Subroutine Empiric_Formula_AtList

   !!--++
   !!--++ SUBROUTINE EMPIRIC_FORMULA_MOLEC
   !!--++
   !!--++    Obtain the Empiric Formula from Molecule variable and
   !!--++    the Weight is the variable is present.
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Empiric_Formula_Molec(Mol, Formula, Form_Weight)
      !---- Arguments ----!
      type(molecule_type),      intent(in)  :: Mol
      character(len=*),         intent(out) :: Formula
      real(kind=cp), optional,  intent(out) :: Form_Weight

      !---- Local variables ----!
      character(len=2)                  :: car
      character(len=5)                  :: numcar
      integer                           :: i,j
      integer, dimension(NUM_CHEM_INFO) :: N_PT
      real(kind=cp)                     :: weight

      !> Init
      N_PT=0
      weight=0.0_cp

      Formula=" "
      if (Mol%natoms <= 0) then
         if (present(Form_weight)) Form_weight=0.0_cp
         return
      end if

      !> Set Information Table
      call Set_Chem_Info()

      do i=1,Mol%natoms
         car= Get_Chem_Symb(Mol%atsymb(i))
         car=u_case(car)
         do j=1,NUM_CHEM_INFO
            if (car == Chem_Info(j)%Symb) then
               n_pt(j)=n_pt(j)+1
               exit
            end if
         end do
      end do

      if (all (n_pt ==0)) then
         if (present(Form_weight)) Form_weight=0.0_cp
         call Remove_Chem_Info()
         return
      end if

      do i=1,NUM_CHEM_INFO
         if (n_pt(i) == 0) cycle
         car=Chem_Info(i)%Symb
         car(2:2)=l_case(car(2:2))
         write(unit=numcar,fmt="(i5)") n_pt(i)
         Formula=trim(Formula)//trim(car)//adjustl(numcar)
         weight=weight+n_pt(i)*Chem_Info(i)%atwe
      end do

      call Remove_Chem_Info()

      if (present(Form_weight)) Form_weight=weight

   End Subroutine Empiric_Formula_Molec

   !!--++
   !!--++ Subroutine Empiric_Formula_Molcrys
   !!--++
   !!--++    Obtain the Empiric Formula from Molecule variable and
   !!--++    the Weight is the variable is present.
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Empiric_Formula_Molcrys(Molcrys, Formula, Form_Weight)
      !---- Arguments ----!
      type(MolCrystal_type),   intent(in)  :: Molcrys
      character(len=*),        intent(out) :: Formula
      real(kind=cp), optional, intent(out) :: Form_Weight

      !---- Local variables ----!
      character(len=2)                  :: car
      character(len=5)                  :: numcar
      integer                           :: i,j,k
      integer, dimension(Num_Chem_Info) :: N_PT
      real(kind=cp)                     :: weight


      !> Init
      N_PT=0
      weight=0.0_cp

      Formula=" "

      if (molcrys%n_free <= 0 .and. molcrys%n_mol <=0) then
         if (present(Form_weight)) Form_weight=0.0_cp
         return
      end if

      !> Set Information Table
      call Set_Chem_Info()

      do i=1,molcrys%n_free
         car=molcrys%atm(i)%chemsymb
         car=u_case(car)
         do j=1,NUM_CHEM_INFO
            if (car == Chem_Info(j)%Symb) then
               n_pt(j)=n_pt(j)+1
               exit
            end if
         end do
      end do

      do k=1,molcrys%n_mol
         do i=1,molcrys%mol(k)%natoms
            car=molcrys%mol(k)%atsymb(i)
            car=u_case(car)
            do j=1,NUM_CHEM_INFO
               if (car == Chem_Info(j)%Symb) then
                  n_pt(j)=n_pt(j)+1
                  exit
               end if
            end do
         end do
      end do

      if (all (n_pt ==0)) then
         if (present(Form_weight)) Form_weight=0.0_cp
         call Remove_Chem_Info()
         return
      end if

      do i=1,NUM_CHEM_INFO
         if (n_pt(i) == 0) cycle
         car=Chem_Info(i)%Symb
         car(2:2)=l_case(car(2:2))
         write(unit=numcar,fmt="(i5)") n_pt(i)
         Formula=trim(Formula)//trim(car)//adjustl(numcar)
         weight=weight+n_pt(i)*Chem_Info(i)%atwe
      end do

      call Remove_Chem_Info()

      if (present(Form_weight)) Form_weight=weight

   End Subroutine Empiric_Formula_Molcrys


End SubModule Mol_Formula