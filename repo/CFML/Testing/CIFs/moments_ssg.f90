 Program Moments_SSG
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Maths,        only: Set_EPS_Math
    use CFML_Strings,      only: File_type, u_case, Get_extension
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell, Change_Setting_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info, &
                                 Get_moment_ctr, Get_TFourier_Ctr, Get_moment_ctr_Wigner,&
                                 Get_Orbit, point_orbit,Get_Inv_OP
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List, ModAtm_Std_Type, &
                                 Get_moment_and_displacement_SSG, Get_Transf_Tn
    use CFML_IOForm
    use CFML_Rational
    use Atoms_in_BOX

    !---- Local Variables ----!
    implicit none

    character(len=:), allocatable       :: line
    character(len=512)                  :: fname,cmdline
    character(len=1)                    :: ans
    integer                             :: nlong,narg
    real(kind=cp)                       :: start, fin
    type(AtList_Type)                   :: Atm

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    character(len=15)                   :: Atm_lab
    character(len=256),dimension(26)    :: tctr_code
    class(Cell_G_Type),allocatable      :: Cell
    class(Spg_Type),   allocatable      :: Grp
    type(File_type)                     :: flist
    integer                             :: i, j, k, d, s, nt, ier, num_op
    real(kind=cp), dimension(3)         :: disp, moment, R_Latt, pos
    complex(kind=cp), dimension(3,24)   :: Tns
    !> Init
    narg=COMMAND_ARGUMENT_COUNT()
    cmdline=" "; nlong=0
    if (narg ==0) then
       write(unit=*,fmt='(/,a)',advance='no') " => Introduce the name of the file: "
       read(unit=*,fmt='(a)') fname
       if (len_trim(fname) <=0 ) call CloseProgram()
       cmdline=trim(fname)
    else
       call GET_COMMAND_ARGUMENT(1, cmdline)
    end if
    nlong=len_trim(cmdline)
    fname=cmdline
    !> Start
    call CPU_TIME(start)

    !> Type of Files

    call Set_CFML_debug(.true.)

    call Read_Xtal_Structure(fname,Cell,Grp,Atm,Ftype=flist)

    if(Err_CFML%Ierr == 0) then
       call Write_Crystal_Cell(Cell)
       call Set_Eps_Math(0.0002_cp)

       Select Type(Grp)
         Type is (SpG_Type)
           write(*,"(/a)")" ----------------------------------------------"
           write(*,"(a)") " This program works only for SuperSpace Groups!"
           write(*,"(a)") " ----------------------------------------------"
           call CloseProgram()
       End Select

       if(Atm%natoms > 0) then
          write(*,"(//a,i5)") "  Number of atoms:",Atm%natoms
          !do i=1,Atm%natoms
          !   codini=1; codes=11.0
          !   call Get_moment_ctr_Wigner(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,Icodes,Multip,ctr_code=ctr_code)!,Ipr=6)
          !   write(*,"(a,3f10.5,a)") " => Moment of atom "//trim(Atm%Atom(i)%Lab)//": ",Atm%Atom(i)%moment,"    CtrCode: "//trim(ctr_code)
          !   write(*,"(a,3i3,a,3f10.5,a)") " => Numerical codes: Icodes->[",Icodes,"]   Multipliers ->[",multip,"]"
          !end do

          Grp%inv = Get_Inv_OP(Grp%Op) !Assure the inverse operator pointer is set

          do
            write(unit=*,fmt="(/a)")   "  Complete list of symmetry operators and pointer to its inverse"
            write(unit=*,fmt="(a)")    "  =============================================================="
            do i=1,Grp%Multip
               write(unit=*,fmt="(a,i4,a,t80,i5)") "  SymmOp",i,": "//trim(Grp%Symb_Op(i)), Grp%inv(i)
            end do
             Select Type (Grp)
               Type is (SuperSpaceGroup_Type)
                  call Write_Atom_List(Atm,SpG=Grp)
             End Select

             write(*,"(a)", advance="no") " => Enter the label of the atom : "
             read(*,"(a)") atm_lab
             if(len_trim(atm_lab) == 0) exit
             write(*,"(a)", advance="no") " => Enter the number of the symmetry operator : "
             read(*,*) num_op
             write(*,"(a)", advance="no") " => Enter a lattice translation (3 reals) : "
             read(*,*) R_latt
             Select Type (Grp)

               Type is (SuperSpaceGroup_Type)

                call Get_moment_and_displacement_SSG(Atm,Grp,atm_lab,num_op,R_latt,pos,Disp,Moment)

                do i=1,Atm%natoms
                 Select Type (At => Atm%Atom(i))
                    Type is (ModAtm_Std_Type)
                      if(At%n_mc > 0) then
                        Tns(:,1:Grp%nq)=Get_Transf_Tn(At,Grp,num_op,"M")
                        write(*,"(/a)") "  Original and Transformed Modulation Amplitudes for atom: "//Trim(At%Lab)
                        do j=1,At%n_mc   !Construction of all possible Tmu
                          k=At%pmc_q(j)
                          Write(*,"(a,6f12.5)") "  Mcos & Msin original  :",At%Mcs(1:3,j),At%Mcs(1:3,j)
                          Write(*,"(a,6f12.5)") "  Mcos & Msin tranformed:",2.0*real(Tns(:,k)),2.0*aimag(Tns(:,k))
                        end do
                      end if
                  end Select
                end do
             End Select
             write(*,"(a,3f12.5,a)") "     Atom at position :",pos," -> "//trim(atm_lab)
             write(*,"(a,3f12.5)")   "  Atomic displacement :",disp
             write(*,"(a,3f12.5)")   "      Magnetic moment :",moment
             write(unit=*,fmt="(a)") " => Press <cr> to continue ..."
             read(unit=*,fmt="(a)") ans
          end do

       end if
    else
      write(*,"(a)") " => Error found!"
      write(*,"(a)") " => "//trim(Err_CFML%Msg)
    end if
    call CPU_TIME(fin)
    write(unit=*,fmt="(/,a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"

 contains
    !!----
    !!---- CLOSEPROGRAM
    !!----
    !!---- 09/05/2020
    Subroutine CloseProgram()
       write(unit=*,fmt="(a)")   " "
       write(unit=*,fmt="(a)")   " => Press <cr> to finish ..."
       read(unit=*,fmt="(a)") ans
       stop
    End Subroutine CloseProgram


End Program Moments_SSG