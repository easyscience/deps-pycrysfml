 Program Test_CIF_CFL_SHX
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Maths,        only: Set_EPS_Math
    use CFML_Strings,      only: File_type, u_case, Get_extension, pack_string
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell, Change_Setting_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info, &
                                 Get_moment_ctr, Get_TFourier_Ctr, Get_moment_ctr_Wigner,&
                                 Get_Orbit, point_orbit,Get_Inv_OP, Orbit_List, Orbit_Type
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List, ModAtm_Std_Type, &
                                 Get_Moment_and_Displacement_SSG
    use Atoms_in_BOX,      only: Get_ModAtoms_inBOX
    use CFML_IOForm

    !---- Local Variables ----!
    implicit none

    character(len=:), allocatable       :: line
    character(len=512)                  :: fname,cmdline
    integer                             :: nlong,narg
    real(kind=cp)                       :: start, fin
    type(AtList_Type)                   :: Atm
    type(Orbit_List)                    :: OL
    integer, dimension(3)               :: TBOX=[1,1,1],Icodes
    integer, dimension(3)               :: mcell

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    character(len=256)                  :: ctr_code,forma,formb !setting,
    character(len=256),dimension(26)    :: tctr_code
    class(Cell_G_Type),allocatable      :: Cell
    class(Spg_Type),   allocatable      :: Grp
    type(File_type)                     :: flist
    type(Orbit_Type)                    :: orb
    type(Orbit_type)                    :: orb3D
    integer :: i, j, d, codini, s, nt, ier
    real(kind=cp), dimension(3)               :: codes=1.0, v, disp, moment, Multip, posi
    real(kind=cp), dimension(:,:),allocatable :: codeT
    logical :: box_given, skip_orb

    !> Init
    call Set_CFML_debug(.true.)
    skip_orb=.false.
    narg=COMMAND_ARGUMENT_COUNT()
    cmdline=" "; nlong=0; box_given=.false.
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

    call Read_Xtal_Structure(fname,Cell,Grp,Atm,Ftype=flist)
    if(Err_CFML%Ierr == 0) then
       mcell=0
       do i=1,flist%nlines
         line=adjustl(flist%line(i)%Str)
         if(len_trim(line) < 6) cycle
         if(line(1:5) =="MCELL") then
           read(line(6:),*) mcell
           tbox(:)=mcell
           box_given=.true.
           exit
         end if
       end do
       if(.not. box_given) then
          write(*,"(a)",advance="no") " => Please enter the box for calculations (3 integers): "
          read(*,*,iostat=ier) tbox
          if(ier /= 0) tbox=[1,1,1]
          if(any(tbox == 0)) skip_orb=.true.
       end if
       call Write_Crystal_Cell(Cell)

       !if(len_trim(Grp%setting) /= 0) then  !This has been suppressed because the change of setting is done in Read_Xtal_structure
       !  write(*,"(/,a)") " => Transformed Cell"
       !  if(Grp%D > 4) then
       !    i=index(Grp%setting,"d")
       !    setting=Grp%setting(1:d-2)//";0,0,0"
       !  else
       !    setting=Grp%setting
       !  end if
       !  call Change_Setting_Cell(Cell,setting,Celln)
       !  call Write_Crystal_Cell(Celln)
       !end if

       !Determine the inverse operators
       Grp%Inv= Get_Inv_OP(Grp%Op)

       call Write_SpaceGroup_Info(Grp)


       call Set_Eps_Math(0.0002_cp)
       if(Atm%natoms > 0) then
          !First Check symmetry constraints in magnetic moments and Fourier coefficients
          !call Check_Symmetry_Constraints(Grp,Atm)
          write(*,"(//a,i5)") "  Number of atoms:",Atm%natoms
          Select Type (Grp)
            Type is (Spg_Type)
               call Write_Atom_List(Atm)
            Type is (SuperSpaceGroup_Type)
               call Write_Atom_List(Atm,SpG=Grp)
               formb="(a, i3,a,6f10.5,a)"
               write(unit=formb(4:4),fmt="(i1)") Grp%nk
          End Select
          !Calculate all atoms in the unit cell
                !1234567890123456789012345678901234567890
          forma="(i5, f10.5,tr8, f10.5,i8,tr5, i4,3f12.5)"
          d=Grp%d-1
          write(forma(5:5),"(i1)") d
          write(forma(16:16),"(i1)") d
          write(forma(30:30),"(i1)") d
          if(.not. skip_orb) then
            write(*,"(//a)") "  Orbits of atoms after applying constraints on moments:"
            write(*,"(  a)") "  ======================================================"
            Ol%num_orbs=Atm%natoms
            allocate(Ol%orbit(Atm%natoms))
          end if

          do i=1,Atm%natoms
            codini=1; codes=11.0
            !call Get_moment_ctr(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,Icodes,Multip,ctr_code=ctr_code)!,Ipr=6)
            call Get_moment_ctr_Wigner(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,Icodes,Multip,ctr_code=ctr_code)!,Ipr=6)
            write(*,"(/a,3f10.5,a)") " => Moment of atom "//trim(Atm%Atom(i)%Lab)//": ",Atm%Atom(i)%moment,"    CtrCode: "//trim(ctr_code)
            write(*,"(a,3i3,a,3f10.5,a)") " => Numerical codes: Icodes->[",Icodes,"]   Multipliers ->[",multip,"]"

            if(.not. skip_orb) then

              call Get_Orbit(Atm%Atom(i)%x,Grp,orb,Atm%Atom(i)%Lab, &
                     Atm%Atom(i)%ChemSymb,Atm%Atom(i)%moment,orb3D, &
                     convl=.true.,tbox=tbox)

              write(*,"(a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)

              Select Case(Grp%d-1)
                Case(3)
                  write(*,"(a)") "    N      X         Y         Z                 Mx        My       Mz      PointoOP"
                Case(4)
                  write(*,"(a)") "    N     X1        X2        X3        X4                 M1        M2         M3        M4      PointoOP"
                Case(5)
                  write(*,"(a)") "    N     X1        X2        X3        X4        X5                 M1        M2        M3        M4        M5      PointoOP"
                Case(6)
                  write(*,"(a)") "    N     X1        X2        X3        X4        X5        X6                 M1        M2        M3        M4        M5        M6      PointoOP"
              End Select
            end if

            Select Type (Grp)
              Type is (SuperSpaceGroup_Type)

                 Select Type(at => Atm%Atom(i))

                   type is (ModAtm_Std_Type)
                     if(.not. skip_orb) then
                      do j=1,orb%Mult
                          !Calculate modulation functions to write properly the superspace coordinates
                          s=orb%pts(j)
                          call Get_moment_and_displacement_SSG(At,Grp,s,real(orb%Lat(1:3,j)),disp,moment) !,tshift)
                          orb%mom(1:3,j)=orb%mom(1:3,j)+moment
                          orb%pos(1:3,j)=orb%pos(1:3,j)+disp
                          write(*,forma) j,orb%pos(1:d,j),orb%mom(1:d,j),s,orb%Lat(1:d,j),orb%mom(1:3,j)
                      end do

                      nt=orb3D%Mult
                      allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt),Ol%orbit(i)%Ls(nt),Ol%orbit(i)%Latt(3,nt))
                      allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%pts(nt))
                      Ol%orbit(i)%ChemSymb=At%ChemSymb
                      Ol%orbit(i)%mult=nt
                      do j=1,nt
                        Ol%orbit(i)%pos(:,j) = orb%pos(1:3,j)
                        Ol%orbit(i)%mom(:,j) = orb%mom(1:3,j)
                        Ol%orbit(i)%Ls(j)    = j
                        Ol%orbit(i)%pts(j)   = orb%pts(j)
                        Ol%orbit(i)%Latt(:,j)= orb%Lat(1:3,j)
                        write(unit=Ol%orbit(i)%lab(j),fmt="(a,i5)")  trim(At%Lab)//"_",j
                        Ol%orbit(i)%lab(j)=pack_string(Ol%orbit(i)%lab(j))
                      end do
                     end if
                 End Select

              Type is(Spg_Type)
                if(.not. skip_orb) then
                 do j=1,orb%Mult
                     s=orb%pts(j)
                     write(*,forma) j,orb%pos(1:d,j),orb%mom(1:d,j),s,orb%Lat(1:d,j),orb%mom(1:3,j)
                 end do
                 !write(*,"(/,a)") " => Orbit of atom (restricted to 3D): "//trim(Atm%Atom(i)%Lab)
                end if
            End Select
            !write(*,"(/,a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)//"  restricted to 3D but within TBOX"
            !call Get_Orbit3D(Atm%Atom(i)%x,Atm%Atom(i)%Lab,Atm%Atom(i)%ChemSymb,Grp,orb3D,tbox)
            !
            do j=1,orb3D%Mult
              posi=orb3D%pos(1:3,j)
              if(any(posi >= 1.00000) ) cycle
              write(*,"(i5,3f12.5,i8,tr5,a,3i4,a)") j,posi,orb3D%pts(j),"[",orb3D%Lat(1:3,j),"]"
            end do
            !
           Select Type(at => Atm%Atom(i))

             class is (ModAtm_Std_Type)
               write(*,"(a)") " => Modulation amplitudes of atom: "//trim(Atm%Atom(i)%Lab)
               if(allocated(CodeT)) deallocate(CodeT)
               allocate(CodeT(6,at%n_mc))
               CodeT=1.0
               Select Type (Grp)
                 Type is (SuperSpaceGroup_Type)
                    do j=1,At%n_mc
                      write(*,formb) "     Mcs: [",Grp%Q_coeff(:,j),"]",At%Mcs(:,j),"    CtrCode: "
                    end do
                    call Get_TFourier_Ctr(At%x,At%Mcs(:,1:at%n_mc),codeT,Grp,codini,"M",Ipr=6,ctr_code=tctr_code)
                    do j=1,At%n_mc
                      write(*,formb) "     Mcs: [",Grp%Q_coeff(:,j),"]",At%Mcs(:,j),"    CtrCode: "//trim(tctr_code(j))
                      !Atm%Atom(i)%Mcs(:,j) = At%Mcs(:,j)  !!!! ERROR
                    end do
                    if(allocated(CodeT)) deallocate(CodeT)
                    if(at%n_dc > 0) then
                      allocate(CodeT(6,at%n_dc))
                      CodeT=1.0
                      call Get_TFourier_Ctr(At%x,At%Dcs(:,1:at%n_dc),codeT,Grp,codini,"D",Ipr=6,ctr_code=tctr_code)
                      do j=1,At%n_dc
                        write(*,formb) "     Dcs: [",Grp%Q_coeff(:,j),"]",At%Dcs(:,j),"    CtrCode: "//trim(tctr_code(j))
                      end do
                    end if
               end select
           end select
          end do

          if(.not. skip_orb) then
             Select Type (Grp)

                 Type is (SuperSpaceGroup_Type)
                   ! Testing atoms in BOX
                   call Get_ModAtoms_inBOX(Atm,Grp,TBOX,Ol)
                   formb="(a15,tr5,a, 3f14.6,2i4,a,3i4,a)"
                   do i=1,Ol%num_orbs
                     write(*,"(/,a,i4)") " Orbit of atom: ",i
                     do j=1,Ol%orbit(i)%mult
                        write(*,formb) Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb,Ol%orbit(i)%pos(1:3,j),Ol%orbit(i)%pts(j),Ol%orbit(i)%Ls(j),"  [",Ol%orbit(i)%Latt(:,j)," ]"
                        write(*,"(tr22,3f14.6)") Ol%orbit(i)%mom(1:3,j)
                     end do
                   end do
                   call Write_CIF_P1()
                   i=index(fname,".")
                   call Write_MCIF_Template(fname(1:i-1)//"_mod.mcif",Cell,Grp,Atm,"Testing Write_ssg_MCIF")

                 Type is (SPG_Type)
                   i=index(fname,".")
                   call Write_Cif_Template(fname(1:i)//"cif", Cell, Grp, Atm, 2, "Testing WriteCIF")
                   call Write_MCIF_Template(fname(1:i-1)//"_mod.mcif",Cell,Grp,Atm,"Testing Write_MCIF")

             End Select
          end if
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
       !---- Local Variables ----!
       character(len=1) :: ans

       write(unit=*,fmt="(a)")   " "
       write(unit=*,fmt="(a)")   " => Press <cr> to finish ..."
       read(unit=*,fmt="(a)") ans

       stop
    End Subroutine CloseProgram

    Subroutine Write_CIF_P1()
      integer :: Ipr=1
      real    :: pos(3)
       !---- Local Variables ----!
      i=index(fname,".",back=.true.)
        open(newunit=ipr,file=fname(1:i-1)//"_P1.mcif", status="replace",action="write")
        write(unit=Ipr,fmt="(a)") "#  -----------------------------------------------"
        write(unit=Ipr,fmt="(a)") "#  Magnetic CIF file generated by CrysFML08 in P1"
        write(unit=Ipr,fmt="(a)") "#  -----------------------------------------------"
        write(unit=Ipr,fmt="(a)") "# This is a simple mCIF in P1 in a BOX (multiple CELL)"
        write(unit=Ipr,fmt="(a)") "# MagCIF file for: "//trim(fname)
        write(unit=Ipr,fmt="(a)") " "
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "_magnetic_space_group_standard_setting  'no'"
        write(unit=Ipr,fmt="(a)") '_parent_space_group.name_H-M  "'//"P 1"//'"'
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        mcell(:)  = tbox(:)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_a    ",cell%cell(1)*mcell(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_b    ",cell%cell(2)*mcell(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_c    ",cell%cell(3)*mcell(3)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_alpha ",cell%ang(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_beta  ",cell%ang(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_gamma ",cell%ang(3)
        write(unit=Ipr,fmt="(a,f10.5)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")  "loop_"
        write(unit=Ipr,fmt="(a)")  "_space_group_symop_magn_operation.id"
        write(unit=Ipr,fmt="(a)")  "_space_group_symop_magn_operation.xyz"
        write(unit=Ipr,fmt="(a)")  "1   x,y,z,+1"
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "_atom_site_label"
        write(unit=Ipr,fmt="(a)") "_atom_site_type_symbol"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_x"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_y"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_z"
        do i=1,Ol%num_orbs
          do j=1,Ol%orbit(i)%mult
             pos=Ol%orbit(i)%pos(1:3,j)/mcell
             if(any(pos < 0.0 ) .or. any(pos >= 1.0 )) cycle
             write(Ipr,"(a15,tr5,a,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb,pos
          end do
        end do
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.label"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_x"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_y"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_z"
        do i=1,Ol%num_orbs
          do j=1,Ol%orbit(i)%mult
             pos=Ol%orbit(i)%pos(1:3,j)/mcell
             if(any(pos < 0.0 ) .or. any(pos >= 1.0 )) cycle
             write(Ipr,"(a15,tr5,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%mom(1:3,j)
          end do
        end do
        close(unit=Ipr)
    End Subroutine Write_CIF_P1


End Program Test_CIF_CFL_SHX