Module nFP_Simulation
   use CFML_GlobalDeps
   use CFML_DiffPatt
   use CFML_Reflections
   use CFML_Atoms
   use CFML_Molecules
   use CFML_Structure_Factors
   use nFP_Globals
   use nFP_Reflections
   use CFML_Random, only: random_poisson

   implicit none
   private
   public :: nFP_Simulate, Merge_Atom_Lists

   contains

    Subroutine nFP_Simulate(lun)
      integer, intent(in) :: lun
      !--- Local variables ---!
      ! i,j,n:counters, n_pat:number of pattern, N_atms:total numbet of atoms,
      ! N_mol: number of molecules, d:number of k-vectors, npts=maximum number of points of all the patterns
      integer :: i, j, k, n, n_pat, N_atms, N_mol, d, npts, irad, ipr,L
      character(len=:),  allocatable :: Type_Atm
      character(len=512) :: filename
      Type(AtList_Type) :: AtL,At

      do n=1,N_patterns
       Select Type (pt => Pat(n)%Pdat)
         Class is (DiffPat_E_Type)
           if(allocated(pt%ycalc)) deallocate(pt%ycalc)
           npts=pt%npts
           allocate(pt%ycalc(npts))
           pt%ycalc=pt%bgr
       End Select
       write(*,"(2(a,i5),a)") " => Ycalc for pattern ",n," allocated with ",npts," points"
      end do

      Do i=1,N_phases
        ! Compute structure factors
        call sf_clear_init_symop()
        !First, convert all molecules to individual atoms before calculation of structure factors
        !=> Posponed
        N_mol=Ph(i)%Nmol
        if(Ph(i)%Nmol > 0) then
           N_atms=Ph(i)%atm_list%natoms+sum(Ph(i)%mol(1:N_mol)%natoms)
           n=0
           select type (Atm => Ph(i)%atm_list%Atom)
              type is (Atm_Std_Type)
                 d=0
                 call Allocate_Atom_List(N_atms, AtL, 'Atm_Std_Type', d)
                 Type_Atm='Atm_Std_Type'
              type is (Atm_Ref_Type)
                 d=0
                 call Allocate_Atom_List(N_atms, AtL, 'Atm_Ref_Type', d)
                 Type_Atm='Atm_Ref_Type'
              type is (ModAtm_Std_Type)
                 d=size(atm(1)%Xs)-3
                 call Allocate_Atom_List(N_atms, AtL, 'ModAtm_Std_Type', d)
                 Type_Atm='ModAtm_Std_Type'
              type is (ModAtm_Ref_Type)
                 d=size(atm(1)%Xs)-3
                 call Allocate_Atom_List(N_atms, AtL, 'ModAtm_Ref_Type', d)
                 Type_Atm='ModAtm_Ref_Type'
           end select

           n=0
           Do j=1,Ph(i)%Nmol

             call Molec_to_AtList(Ph(i)%Mol(j), Type_Atm, At, 'F', Ph(i)%Cell)
             select type (Atm => AtL%Atom)
                type is (Atm_Std_Type)
                   Select Type (a_atom => At%Atom)
                      Type is(Atm_Std_Type)
                      Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
                   End Select
                type is (Atm_Ref_Type)
                   Select Type (a_atom => At%Atom)
                      Type is(Atm_Ref_Type)
                      Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
                   End Select
                type is (ModAtm_Std_Type)
                   Select Type (a_atom => At%Atom)
                      Type is(ModAtm_Std_Type)
                      Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
                   End Select
                type is (ModAtm_Ref_Type)
                   Select Type (a_atom => At%Atom)
                      Type is(ModAtm_Ref_Type)
                      Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
                   End Select
             end select
             n=n+At%natoms
             call allocate_atom_list(0,At,' ',0)
           End do
        else
          AtL=Ph(i)%atm_list
        end if


        Write(*,"(a,i3)") " Initializing structure factors for phase #",i
        ! Neutrons
        call init_structure_factors(RL(i),AtL,Ph(i)%spg,mode="NUC",Lun=lun)
        if (err_cfml%ierr /= 0) return
        Write(*,"(a,i3)") " Calculating structure factors for phase #",i
        call structure_factors(RL(i),AtL,Ph(i)%spg,mode="NUC")
        if (err_cfml%ierr /= 0) return
        Write(*,"(a,i3)") " Writing structure factors for phase #",i
        call Write_Structure_Factors(RL(i),lun,mode="NUC",iph=i)


        ! Xrays
        !    call init_structure_factors(Ref(i),Ph(i)%atm_list,Ph(i)%spg,mode="XRA",lambda=ppc%lambda)
        !    if (err_cfml%ierr /= 0) return
        !    call structure_factors(Ref(i),Ph(i)%atm_list,Ph(i)%spg,mode="XRA",lambda=ppc%lambda)
        !    if (err_cfml%ierr /= 0) return
        !    call Write_Structure_Factors(Ref(i),lun,mode="XRA")

        do j=1, Ph(i)%Ncontr
           n_pat= Ph(i)%patterns(j) !number of the pattern to which the phase contributes
           if(Pat(n_pat)%sample /= "P") cycle
           if(Pat(n_pat)%mode == "CW") then
              Select Case(Pat(n_pat)%radiation)
                case("X") ; irad=1
                case("N") ; irad=2
                case("E") ; irad=3
                Case Default ; irad=2
              End Select

             call Profile_Contribution_CW(i,j,RL(i),prph(i)%prc(j)%yp)

             write(Filename,"(2(a,i3.3),a)") "Pattern_",n_pat,"_phase_",i,".xys"
             open(newunit=ipr,file=filename,status="replace",action="write")
                write(ipr,"(a)") "XYDATA"
                write(ipr,"(2(a,i3))") " Contribution of phase ",i, " to pattern ",n_pat
                write(ipr,"(a)") " Testing the program nFP "
                write(ipr,"(a)") " Testing the program nFP "
                write(ipr,"(a)") " Testing the program nFP "
                write(ipr,"(a)") " Testing the program nFP "
                do L=1,Pat(n_pat)%PDat%npts
                  write(ipr,"(2f14.5)") Pat(n_pat)%PDat%x(L),prph(i)%prc(j)%yp(L)
                end do
             close(unit=ipr)
             Select Type (pt =>Pat(n_pat)%Pdat)
               Class is (DiffPat_E_Type)
                do L=1,pt%npts
                  pt%ycalc(L)=pt%ycalc(L)+prph(i)%prc(j)%yp(L)
                end do
             End Select
           else if(Pat(n_pat)%mode == "TF") then
           end if
        end do !j=1, Ph(i)%Ncontr

      End do !i=1,N_phases

      do j=1,N_patterns
        write(Filename,"(a,i3.3,a)") "Pattern_",j,".xys"
        Select Type (pt => Pat(j)%Pdat)
          Class is (DiffPat_E_Type)
            pt%ycalc=pt%bgr
        End Select
        do i=1,N_phases
          if(is_in(j,Ph(i)%patterns)) then
            Select Type (pt => Pat(j)%Pdat)
              Class is (DiffPat_E_Type)
                do k=1,pt%npts
                   pt%ycalc(k)=pt%ycalc(k)+prph(i)%prc(j)%yp(k)
                end do
            End Select
          end if
        end do

        Select Type (pt => Pat(j)%Pdat)
          Class is (DiffPat_E_Type)
            if(gen_pat(j)) then
               do i=1,pt%npts
                 pt%y(i)=Random_Poisson(pt%ycalc(i))
                 pt%sigma(i)=sqrt(pt%ycalc(i))
               end do
               call Write_Pattern_XYSig(Filename,pt)
            else
               call Write_Pattern_XYSig(Filename,pt,calc=.true.)
            end if
        End Select

      end do

    End Subroutine nFP_Simulate

    Function is_in(val,array) result(itis)
      integer,               intent(in) :: val
      integer, dimension(:), intent(in) :: array
      logical :: itis
      !
      integer :: i
      itis=.false.
      do i=1,size(array)
        if(val == array(i)) then
          itis=.true.
          Return
        end if
      end do
    End Function is_in

    !> Function Merge_Atom_Lists(At1,At2) result(At)
    !>   type(AtList_Type), intent(in) :: At1, At2
    !>   type(AtList_Type)             :: At
    !>
    !>  Function to merge two atom lists (they have to be completely defined)
    !>  The components of the second argument At2
    !>  are appended to At1 conserving the attributes of At1
    !>
    Function Merge_Atom_Lists(At1,At2,iph) result(A)
      type(AtList_Type), intent(in) :: At1, At2
      integer, optional, intent(in) :: iph
      type(AtList_Type)             :: A
      !--- Local variables ---!
      integer :: d,ip,n_atoms,n1,n2
      if(same_type_as(At1%Atom,At2%Atom) ) then
        ip=1
        d=0
        if(present(iph)) ip=iph
        n1=At1%natoms; n2=At2%natoms
        n_atoms=n1+n2
        !Allocate the atoms of the new atom list

        Select Type (at => At1%Atom)
          Type is(Atm_Std_Type)
            call Allocate_Atom_List(n_atoms, A,"Atm_Std_Type",d,ip)
          Type is(Atm_Ref_Type)
            call Allocate_Atom_List(n_atoms, A,"Atm_Ref_Type",d,ip)
          Type is(modAtm_Std_Type)
            d=size(at(1)%Xs)-3
            call Allocate_Atom_List(n_atoms, A,"modAtm_Std_Type",d,ip)
          Type is(modAtm_Ref_Type)
            d=size(at(1)%Xs)-3
            call Allocate_Atom_List(n_atoms, A,"modAtm_Ref_Type",d,ip)
          class Default
            call Allocate_Atom_List(n_atoms, A,"Atm_Std_Type",d,ip)
        End Select
        A%mcomp=At1%mcomp
        if(At1%symm_checked .and. At1%symm_checked) then
          A%symm_checked=.true.
        else
          A%symm_checked=.false.
        end if
        A%active(1:n1) = At1%active(1:n1);  A%active(n1+1:n_atoms) = At2%active(1:n2)

        !For polymorphic part Select Type is compulsory
        Select Type (a_atom => A%atom)

          Type is(Atm_Std_Type)
              Select Type (at1_atom => At1%atom)
                Type is(Atm_Std_Type)
                    a_atom(1:n1) = at1_atom(1:n1)
              End Select
              Select Type (at2_atom => At2%atom)
                Type is(Atm_Std_Type)
                    a_atom(n1+1:n_atoms) = at2_atom(1:n2)
              End Select

          Type is(Atm_Ref_Type)
              Select Type (at1_atom => At1%atom)
                Type is(Atm_Ref_Type)
                    a_atom(1:n1) = at1_atom(1:n1)
              End Select
              Select Type (at2_atom => At2%atom)
                Type is(Atm_Ref_Type)
                    a_atom(n1+1:n_atoms) = at2_atom(1:n2)
              End Select

          Type is(modAtm_Std_Type)
              Select Type (at1_atom => At1%atom)
                Type is(modAtm_Std_Type)
                    a_atom(1:n1) = at1_atom(1:n1)
              End Select
              Select Type (at2_atom => At2%atom)
                Type is(modAtm_Std_Type)
                    a_atom(n1+1:n_atoms) = at2_atom(1:n2)
              End Select

          Type is(modAtm_Ref_Type)
              Select Type (at1_atom => At1%atom)
                 Type is(modAtm_Ref_Type)
                     a_atom(1:n1) = at1_atom(1:n1)
              End Select
              Select Type (at2_atom => At2%atom)
                 Type is(modAtm_Ref_Type)
                    a_atom(n1+1:n_atoms) = at2_atom(1:n2)
              End Select
        End Select

      else
        Call Set_Error(1,"The atom types of both lists do not mach!")
      end if
    End Function Merge_Atom_Lists

    Function type_of_atoms(Atm) result(type_Atm)
      Class(Atm_Type), intent(in) :: Atm
      character(len=:), allocatable:: type_Atm
      Select Type(Atm)
        Type is(Atm_Type)
           type_Atm="Atm_Type"
        Type is(Atm_Std_Type)
           type_Atm="Atm_Std_Type"
        Type is(ModAtm_Std_Type)
           type_Atm="ModAtm_Std_Type"
        Type is(Atm_Ref_Type)
           type_Atm="Atm_Ref_Type"
        Type is(ModAtm_Ref_Type)
           type_Atm="ModAtm_Ref_Type"
        Class Default
           type_Atm="Unknown_Type"
      End Select
    End Function type_of_atoms

End Module nFP_Simulation