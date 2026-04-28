!!
SubModule (CFML_Atoms) Atm_ChangeList

   implicit none

   Contains
   !!----
   !!---- SUBROUTINE CHANGE_ATOMLIST_TYPE
   !!----
   !!---- Procedure to pass the component Atom to the desired
   !!---- atom_type
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Change_AtomList_Type(AtList, TypeAtm, Nv)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      character(len=*),  intent(in)     :: TypeAtm
      integer, optional, intent(in)     :: Nv        ! Number of K-vectors

      !---- Local Variables ----!
      integer           :: Iv,i
      type(AtList_Type) :: At

      !> Init
      Iv=0
      if (present(Nv)) Iv=nv

      call clear_error()
      if (AtList%natoms <=0) return

      select case (trim(u_case(TypeAtm)))
         case ('ATM_TYPE')
            call Allocate_Atom_List(AtList%Natoms, At,'Atm_Type', 0)
            if (err_CFML%IErr /=0) return

            select type (A => AtList%Atom)
               type is (Atm_Type)
                  !> Atm_Type -> Atm_Type
                  !> Do nothing
                  call Allocate_Atom_List(0, At,' ', 0)
                  return

               type is (Atm_std_Type)
                  !> Atm_Std_Type -> Atm_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (Atm_Ref_Type)
                  !> Atm_Ref_Type -> Atm_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (ModAtm_Std_Type)
                  !> ModAtm_Std_Type -> Atm_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (ModAtm_Ref_Type)
                  !> ModAtm_Ref_Type -> Atm_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

            end select

         case ('ATM_STD_TYPE')
            call Allocate_Atom_List(AtList%Natoms, At,'Atm_Std_Type', 0)
            if (err_CFML%IErr /=0) return
            select type (A => AtList%Atom)
               type is (Atm_Type)
                  !> Atm_type -> Atm_Std_type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (Atm_std_Type)
                  !> Atm_std_type -> Atm_Std_Type
                  !> Do nothing!
                  call Allocate_Atom_List(0, At,' ', 0)
                  return

               type is (Atm_Ref_Type)
                  !> Atm_Ref_Type -> Atm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Std_Type)
                  !> ModAtm_Std_Type -> Atm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Ref_Type)
                  !> ModAtm_Ref_Type -> Atm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

            end select

         case ('ModAtm_STD_TYPE')
            call Allocate_Atom_List(AtList%Natoms, At,'ModAtm_Std_Type', Iv)
            if (err_CFML%IErr /=0) return

            select type (A => AtList%Atom)
               type is (Atm_Type)
                  !> Atm_Type -> ModAtm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (Atm_std_Type)
                  !> Atm_Std_Type -> ModAtm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (Atm_Ref_Type)
                  !> Atm_Ref_Type -> ModAtm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Std_Type)
                  !> ModAtm_STD_Type -> ModAtm_Std_Type
                  !> Do nothing
                  call Allocate_Atom_List(0, At,' ', 0)
                  return

               type is (ModAtm_Ref_Type)
                  !> ModAtm_Ref_Type -> ModAtm_Std_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (ModAtm_Std_Type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                           call copyinfo_mstd_type(B(i),A(i))
                        end do
                  end select

            end select

         case ('ATM_REF_TYPE')
            call Allocate_Atom_List(AtList%Natoms, At,'Atm_Ref_Type', 0)
            if (err_CFML%IErr /=0) return

            select type (A => AtList%Atom)
               type is (Atm_Type)
                  !> Atm_type -> Atm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (Atm_std_Type)
                  !> Atm_Std_Type -> Atm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (Atm_Ref_Type)
                  !> Atm_Ref_Type -> Atm_Ref_Type
                  !> Do nothing
                  call Allocate_Atom_List(0, At,' ', 0) ! Nothing to do
                  return

               type is (ModAtm_Std_Type)
                  !> ModAtm_Std_Type -> Atm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Ref_Type)
                  !> ModAtm_Ref_Type -> Atm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

            end select

         case ('ModAtm_REF_TYPE')
            call Allocate_Atom_List(AtList%Natoms, At,'ModAtm_Ref_Type', Iv)
            if (err_CFML%IErr /=0) return

            select type (A => AtList%Atom)
               type is (Atm_Type)
                  !> Atm_Type -> ModAtm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  do i=1,AtList%Natoms
                     call copyinfo_atm_type(at%atom(i),A(i))
                  end do

               type is (Atm_std_Type)
                  !> Atm_Std_Type -> ModAtm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (Atm_Ref_Type)
                  !> Atm_Ref_Type -> ModAtm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (atm_std_type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Std_Type)
                  !> ModAtm_Std_Type -> ModAtm_Ref_Type
                  At%mcomp         = atList%mcomp
                  At%symm_checked  = atlist%symm_checked
                  At%active        = atlist%active
                  At%IPh           = atlist%IPh

                  select type (B=> At%atom)
                     class is (ModAtm_Std_Type)
                        do i=1,AtList%Natoms
                           call copyinfo_atm_type(B(i),A(i))
                           call copyinfo_std_type(B(i),A(i))
                           call copyinfo_mstd_type(B(i),A(i))
                        end do
                  end select

               type is (ModAtm_Ref_Type)
                  !> ModAtm_Ref_Type -> ModAtm_Ref_Type
                  !> Do nothing
                  call Allocate_Atom_List(0, At,' ', 0)
                  return
            end select

         case default
            call set_error(1," Type_Atom unknown for conversion!" )
            return
      end select

      !> Deallocating AtList
      call Allocate_Atom_List(0, AtList,' ', 0)

      !> Copy
      select case (trim(u_case(TypeAtm)))
         case ('ATM_TYPE')
            call Allocate_Atom_List(At%Natoms, AtList,'Atm_Type', 0)

         case ('ATM_STD_TYPE')
            call Allocate_Atom_List(At%Natoms, AtList,'Atm_Std_Type', 0)

         case ('ModAtm_STD_TYPE')
            call Allocate_Atom_List(At%Natoms, AtList,'ModAtm_Std_Type', Iv)

         case ('ATM_REF_TYPE')
            call Allocate_Atom_List(At%Natoms, AtList,'Atm_Ref_Type', 0)

         case ('ModAtm_REF_TYPE')
            call Allocate_Atom_List(At%Natoms, AtList,'ModAtm_Ref_Type', Iv)
      end select
      if (err_CFML%IErr /=0) return

      !> Is possible to do this?
      AtList = At

      !> Deallocating At
      call Allocate_Atom_List(0, At,' ', 0)

   End Subroutine Change_AtomList_Type

   !!--++
   !!--++ SUBROUTINE COPYINFO_ATM_TYPE
   !!--++
   !!--++     Pass the share information between
   !!--++     Atom_Types
   !!--++
   !!--++ Updated: April - 2022
   !!
   Module Subroutine CopyInfo_Atm_Type(At1, At2)
      !---- Arguments ----T
      class(Atm_Type), intent(in out):: At1
      class(Atm_Type), intent(in)    :: At2

      !---- Local Variables ----!
      integer :: i

      At1%Lab      =At2%Lab
      At1%ChemSymb =At2%ChemSymb
      At1%SfacSymb =At2%SfacSymb
      At1%Z        =At2%Z
      At1%Mult     =At2%Mult
      At1%Charge   =At2%Charge
      At1%U_iso    =At2%U_iso
      At1%Occ      =At2%Occ
      At1%UType    =At2%UType
      At1%ThType   =At2%ThType
      At1%Magnetic =At2%Magnetic
      At1%Mom      =At2%Mom
      At1%AtmInfo  =At2%AtmInfo
      At1%Wyck     =At2%Wyck
      At1%Active   =At2%Active

      do i=1,3
         At1%x(i)      =At2%x(i)
         At1%Moment(i) =At2%Moment(i)
         At1%Ind_ff(i) =At2%Ind_ff(i)
      end do

      do i=1,6
         At1%u(i)      =At2%u(i)
      end do

      do i=1,5
         At1%VarF(i)   =At2%VarF(i)
      end do

   End Subroutine CopyInfo_Atm_Type

   !!--++
   !!--++ SUBROUTINE COPYINFO_STD_TYPE
   !!--++
   !!--++     Pass the share information between
   !!--++     Atom_Std_Types
   !!--++
   !!--++ Updated: April - 2022
   !!
   Module Subroutine CopyInfo_Std_Type(At1, At2)
      !---- Arguments ----T
      class(Atm_Std_Type), intent(in out):: At1
      class(Atm_Std_Type), intent(in)    :: At2

      !---- Local Variables ----!
      integer :: i

      At1%Occ_Std    = At2%Occ_std
      At1%U_iso_Std  = At2%U_iso_std

      do i=1,3
         At1%X_Std(i)       = At2%X_std(i)
         At1%Moment_Std(i)  = At2%Moment_std(i)
      end do

      do i=1,6
         At1%U_Std(i)       = At2%U_std(i)
      end do

   End Subroutine CopyInfo_Std_Type

   !!--++
   !!--++ SUBROUTINE COPYINFO_MSTD_TYPE
   !!--++
   !!--++     Pass the share information between
   !!--++     MAtom_Std_Types
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine CopyInfo_MStd_Type(At1, At2)
      !---- Arguments ----T
      class(ModAtm_Std_Type), intent(in out):: At1
      class(ModAtm_Std_Type), intent(in)    :: At2

      !---- Local Variables ----!
      integer :: i,j

      At1%n_oc  = At2%n_oc
      At1%n_bc  = At2%n_bc
      At1%n_mc  = At2%n_mc
      At1%n_dc  = At2%n_dc
      At1%n_uc  = At2%n_uc

      do i=1,MAX_MOD
         At1%poc_q(i)   = At2%poc_q(i)
         At1%pbc_q(i)   = At2%pbc_q(i)
         At1%pmc_q(i)   = At2%pmc_q(i)
         At1%pdc_q(i)   = At2%pdc_q(i)
         At1%puc_q(i)   = At2%puc_q(i)

         do j=1,2
            At1%Ocs(j,i)       = At2%Ocs(j,i)
            At1%Ocs_std(j,i)   = At2%Ocs_std(j,i)
            At1%Bcs(j,i)       = At2%Bcs(j,i)
            At1%Bcs_std(j,i)   = At2%Bcs_std(j,i)
         end do

         do j=1,6
            At1%Mcs(j,i)       = At2%Mcs(j,i)
            At1%Mcs_std(j,i)   = At2%Mcs_std(j,i)
            At1%Dcs(j,i)       = At2%Dcs(j,i)
            At1%Dcs_std(j,i)   = At2%Dcs_std(j,i)
         end do

         do j=1,12
            At1%Ucs(j,i)       = At2%Ucs(j,i)
            At1%Ucs_std(j,i)   = At2%Ucs_std(j,i)
         end do
      end do

      !> Have to be assigned
      !Xs
      !Moms
      !Us

   End Subroutine CopyInfo_MStd_Type

End SubModule Atm_ChangeList