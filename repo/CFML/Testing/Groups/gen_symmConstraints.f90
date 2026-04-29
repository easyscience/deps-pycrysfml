  Module Test_Constraints

   Type, public :: Point_Group_Type,

   End Type Point_Group_Type

   contains

   Subroutine Set_Point_Group(generators,PG)

   End Subroutine Set_Point_Group

   !!
   !!----  Module Subroutine Get_moment_ctr(xnr,moment,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr      ! Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(3),            intent(in out) :: moment   ! Magnetic moment at position xnr
   !!----     class(SuperSpaceGroup_Type),            intent(in)     :: Spg      ! Super Space Group
   !!----     Integer,                                intent(in out) :: codini   ! Number of the Last attributed parameter
   !!----     real(kind=cp), dimension(3),            intent(in out) :: codes    ! codewords for magnetic moment
   !!----     integer,                       optional,intent(in)     :: ord      ! Order of stabilizer
   !!----     integer, dimension(:),         optional,intent(in)     :: ss       ! Pointer to operators
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att      ! Translations of stabilizer operators
   !!----     integer,                       optional,intent(in)     :: Ipr      ! Logical unit for writing
   !!----     character(len=*),              optional,intent(out)    :: ctr_code ! Symmetry code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: March 2020
   !!----
   !!
   Module Subroutine Get_Tensor2ndOrder_ctr(Tensor,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(:,:),          intent(in out) :: Tensor
      class(SpG_type),                        intent(in)     :: Spg
      Integer,                                intent(in out) :: codini
      real(kind=cp), dimension(:),            intent(in out) :: codes
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),              optional,intent(out)    :: ctr_code

      ! Local variables
      character(len=1),  dimension(3)   :: codd
      character(len=15), dimension(3)   :: St_Cod
      character(len=:), allocatable     :: mag
      integer                           :: i,j,order,n,ig,iss
      real(kind=cp)                     :: suma
      integer,           dimension(48)  :: ss_ptr
      real(kind=cp),     dimension(3,48):: atr
      real(kind=cp),     dimension(3)   :: cod,multi
      real(kind=cp),     dimension(3)   :: x
      real(kind=cp),     dimension(3,3) :: magm  !g, magm= delta * det(g) * g
      real(kind=dp),     dimension(3,3) :: sCtr
      real(kind=cp),     dimension(3)   :: momentL,TotMom


      !Test if all codes are given ... in such a case the user constraints
      !are prevalent

      suma=0.0_cp
      if(present(ctr_code)) ctr_code="(0,0,0)"
      n=3 !Real moments -> three components
      do j=1,3
         suma=suma+abs(codes(j))
      end do


      momentL=moment
      sCtr=0.0_cp
      if(order > 1) then
        do ig=1,order
          j=ss_ptr(ig)
          magm(:,:) = real(Spg%Op(j)%Mat(1:3,1:3))*Spg%Op(j)%dt*Spg%Op(j)%time_inv
          mag=Set_Symb_From_Mat(magm,["u","v","w"])
          sCtr=sCtr+magm !Adding constraint matrices for each operator of stabilizer
          if(present(ipr)) then
            write(unit=ipr,fmt='(a,i2,a,t20,a,t55,a,t75,9f8.4)') '     Operator ',ig,": ",trim(Spg%Symb_Op(j)), &
             trim(mag), sCtr
          end if
        end do  !ig operators
        sCtr=sCtr/order
        suma=sum(abs(sCtr))
        !write(*,"(a,f10.4,a,i3)") " suma:",suma, "Mag_Type:", spg%mag_type
        if(suma < epss .or. spg%mag_type == 2) then !This corresponds to a grey point group
           moment=0.0_cp
           codes=0.0_cp
           if(present(Ipr)) then
             write(Ipr,"(a)")         " Grey point group or symmetry paramagnetic site: the attached moment is zero "
             write(Ipr,"(a,24f14.6)") " Final codes: ",codes(1:n)
             write(Ipr,"(a,24f14.6)") " Constrained moment: ",moment
           end if
           return
        end if
        TotMom=matmul(sCtr,momentL)
        call Get_Refinement_Codes(n,TotMom,sCtr,iss,multi,codd,momentL)
        cod=0.0
        do j=1,n
          if(codd(j) /= "0") then
            do i=1,iss
              if(codd(j) == cdd(i)) then
                cod(j)=codini+i
                exit
              end if
            end do
          end if
        end do
        moment=momentL
        codes=0.0
        do j=1,n
          if(abs(multi(j)) > epss)  codes(j) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
        end do
        codini=codini+iss
        if(present(Ipr)) then
          Write(unit=Ipr,fmt="(a,i4)")       " Number of free parameters: ",iss
          write(unit=Ipr,fmt="(a,3f14.6)")   " Multipliers: ",(multi(j), j=1,n)
          write(unit=Ipr,fmt="(28a)")        " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,3i6)")      " Resulting integer codes: ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,3f14.6)")   " Final codes: ",codes(1:n)
          write(unit=Ipr,fmt="(a,3f14.6)")   " Constrained Moment: ",moment
        end if

      else !No restrictions

        codd(1:n)=cdd(1:n)
        multi(1:n)=1.0_cp
        do j=1,n
          cod(j)=codini+j
          codes(j) = (abs(cod(j))*10.0_cp + abs(multi(j)))
        end do
        codini=codini+n
        if(present(Ipr)) then
          write(unit=Ipr,fmt="(a)")         " General position, no constraints in moment "
          write(unit=Ipr,fmt="(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,24f14.6)") " Final codes: ",codes(1:n)
          write(unit=Ipr,fmt="(a,24f14.6)") " Constrained moment: ",moment
        end if

      end if

      if(present(ctr_code)) then
          do j=1,3
            St_cod(j)=" "
            if(abs(multi(j)) < 0.00001) then
              St_cod(j) = "0"
            else
              if(multi(j) == 1.0_cp) then
                 St_cod(j)=codd(j)
              else if(multi(j) == -1.0_cp) then
                 St_cod(j)="-"//codd(j)
              else
                 write(unit=St_cod(j),fmt="(f10.5,a)")  multi(j),"*"//codd(j)
                 St_cod(j)=adjustl(St_cod(j))
              end if
            end if
          end do
         write(unit=ctr_code,fmt="(28a)") " ( ",(St_cod(j)//", ",j=1,n-1),St_cod(j)//" )"
         ctr_code=pack_string(ctr_code)
      end if
   End Subroutine Get_Tensor2ndOrder_ctr



  End Module Test_Constraints


!!----
!!----
!!----
!!----
 Program Gen_
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Strings,      only: File_type, u_case
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List
    use CFML_IOForm

    !---- Local Variables ----!
    implicit none

    character(len=512)                  :: fname,cmdline
    integer                             :: nlong,narg
    real(kind=cp)                       :: start, fin

    type(Cell_G_Type)                   :: Cell
    class(Spg_Type), allocatable        :: SpG
    type(AtList_Type)                   :: Atm


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

    !> Start
    call CPU_TIME(start)

    !> Type of Files
    call Read_Xtal_Structure(trim(cmdline), Cell, Spg, Atm)

    !> Print Information
    if (Err_CFML%Ierr == 0) then
       !> Cell Info
       call Write_Crystal_Cell(Cell)
       write(unit=*,fmt='(a)') " "

       !> SpaceGroup
       call Write_SpaceGroup_Info(SpG)
       write(unit=*,fmt='(a)') " "

       !> Atoms
       call Write_Atom_List(Atm)
       write(unit=*,fmt='(a)') " "
    else
       write(unit=*,fmt='(/,a)') " => ERROR: "//trim(Err_CFML%Msg)
    end if
    call CPU_TIME(fin)
    write(unit=*,fmt="(/,a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"


    !!!! TEST JRC
    !call Set_Eps_Math(0.0002_cp)
    !
    !call Readn_Set_Xtal_Structure(fname,Cell,Grp,Atm,"MAtm_std","CFL")!,file_list=flist) !,Iphase,Job_Info,file_list,CFrame)
    !if(Err_CFML%Ierr == 0) then
    !   !write(*,"(/,a,/)")  " => Content of the CFL-file: "//flist%Fname
    !   !do i=1,flist%nlines
    !   !   write(*,"(i6,a)") i,"    "//flist%line(i)%Str
    !   !end do
    !   call Write_Crystal_Cell(Cell)
    !   if(len_trim(Grp%setting) /= 0) then
    !     write(*,"(/,a)") " => Transformed Cell"
    !     if(Grp%D > 4) then
    !       i=index(Grp%setting,"d")
    !       setting=Grp%setting(1:d-2)//";0,0,0"
    !     else
    !       setting=Grp%setting
    !     end if
    !     call Change_Setting_Cell(Cell,setting,Celln)
    !     call Write_Crystal_Cell(Celln)
    !   end if
    !   call Write_SpaceGroup_Info(Grp)
    !
    !   i=index(fname,".")
    !   filename=fname(1:i)//"cif"
    !   call Write_Cif_Template(filename, Cell, Grp, Atm, 2, "Testing WriteCIF")
    !
    !   if(Atm%natoms > 0) then
    !      !First Check symmetry constraints in magnetic moments and Fourier coefficients
    !      !call Check_Symmetry_Constraints(Grp,Atm)
    !      write(*,"(//a,i5)") "  Number of atoms:",Atm%natoms
    !      call Write_Atom_List(Atm,SpG=Grp)
    !      !Calculate all atoms in the unit cell
    !      forma="(i5, f10.5,tr8, f10.5,i8)"
    !      formb="(a, i3,a,6f10.5,a)"
    !      write(unit=formb(4:4),fmt="(i1)") Grp%nk
    !      write(forma(5:5),"(i1)") Grp%d-1
    !      write(forma(16:16),"(i1)") Grp%d-1
    !      write(*,"(//a)") "  Orbits of atoms after applying constraints on moments:"
    !      write(*,"(  a)") "  ======================================================"
    !
    !
    !      do i=1,Atm%natoms
    !        !codini=1; codes=1.0
    !        call Get_moment_ctr(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,codes,ctr_code=ctr_code)!,Ipr=6)
    !        write(*,"(a,3f10.5,a)") " => Moment of atom "//trim(Atm%Atom(i)%Lab)//": ",Atm%Atom(i)%moment,"    CtrCode: "//trim(ctr_code)
    !        call Get_Orbit(Atm%Atom(i)%x,Grp,Mult,orb,Atm%Atom(i)%moment,morb,ptr)
    !        write(*,"(a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)
    !
    !        Select Case(Grp%d-1)
    !          Case(3)
    !            write(*,"(a)") "    N      X         Y         Z                 Mx        My       Mz      PointoOP"
    !          Case(4)
    !            write(*,"(a)") "    N     X1        X2        X3        X4                 M1        M2         M3        M4      PointoOP"
    !          Case(5)
    !            write(*,"(a)") "    N     X1        X2        X3        X4        X5                 M1        M2        M3        M4        M5      PointoOP"
    !          Case(6)
    !            write(*,"(a)") "    N     X1        X2        X3        X4        X5        X6                 M1        M2        M3        M4        M5        M6      PointoOP"
    !        End Select
    !
    !        do j=1,Mult
    !            write(*,forma) j,orb(:,j),morb(:,j),ptr(j)
    !        end do
    !       Select Type(at => Atm%Atom(i))
    !         class is (MAtm_Std_Type)
    !           write(*,"(a)") " => Modulation amplitudes of atom: "//trim(Atm%Atom(i)%Lab)
    !           if(allocated(CodeT)) deallocate(CodeT)
    !           allocate(CodeT(6,at%n_mc))
    !           CodeT=1.0
    !           call Get_TFourier_Ctr(At%x,At%Mcs(:,1:at%n_mc),codeT,Grp,codini,"M",ctr_code=tctr_code)
    !           do j=1,At%n_mc
    !             write(*,formb) "     Mcs: [",Grp%Q_coeff(:,j),"]",At%Mcs(:,j),"    CtrCode: "//trim(tctr_code(j))
    !           end do
    !           if(allocated(CodeT)) deallocate(CodeT)
    !           allocate(CodeT(6,at%n_dc))
    !           CodeT=1.0
    !           call Get_TFourier_Ctr(At%x,At%Dcs(:,1:at%n_dc),codeT,Grp,codini,"D",ctr_code=tctr_code)
    !           do j=1,At%n_dc
    !             write(*,formb) "     Dcs: [",Grp%Q_coeff(:,j),"]",At%Dcs(:,j),"    CtrCode: "//trim(tctr_code(j))
    !           end do
    !       end select
    !      end do
    !   end if
    !end if

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

End Program Test_SHX_CIF_CFL