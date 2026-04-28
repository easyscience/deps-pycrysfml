!!----
!!----
!!----
!!----
 Module Test_Change_sett
    Use CFML_GlobalDeps,        only: CP,DP, LI, EPS, err_cfml, clear_error, CFML_Debug,TPI
    Use CFML_Rational
    Use CFML_Symmetry_Tables
    Use CFML_Magnetic_Database
    Use CFML_SuperSpace_Database
    Use CFML_Maths,             only: Set_eps_math, modulo_lat, determ3D, Get_eps_math, Zbelong,EPSS,Diagonalize_RGEN, &
                                      equal_vector,resolv_sist_3x3,trace,Equal_Matrix, Inverse_Matrix,Lat_modulo
    Use CFML_Strings,           only: u_case, l_case, pack_string, get_separator_pos, get_num, &
                                      get_words, String_Fraction_2Dig,Set_Symb_From_Mat, Get_Vec_from_FracStr
    Use CFML_gSpaceGroups
    Implicit none
    private
    public :: Change_Setting_SpG, change_new
    !---- Private Variables ----!
    integer,          dimension(0:2), parameter :: CENT=[2,1,2]       ! Multiplier for calculating the total multiplicity
    character(len=1), dimension(10),  parameter :: XYZ=["x","y","z","t","u","v","w","p","q","r"]
    character(len=1), dimension(10),  parameter :: ABC=["a","b","c","d","e","f","g","h","i","j"]
    character(len=3), dimension(10),  parameter :: X1X2X3=["x1 ","x2 ","x3 ","x4 ","x5 ","x6 ","x7 ","x8 ","x9 ","x10"]
    character(len=3), dimension(10),  parameter :: A1A2A3=["a1 ","a2 ","a3 ","a4 ","a5 ","a6 ","a7 ","a8 ","a9 ","a10"]


 contains

   Subroutine Change_Setting_SpG(setting, SpaceG,xyz_type)
      !---- Arguments ----!
      character(len=*),           intent(in )    :: setting
      class(spg_type),            intent(in out) :: SpaceG
      character(len=*), optional, intent(in )    :: xyz_type

      !---- Local variables ----!
      Type(rational), dimension(SpaceG%D,SpaceG%D)     :: Pmat,invPmat
      Type(rational), dimension(SpaceG%D-1,SpaceG%D-1) :: rot,roti,identd
      Type(rational), dimension(SpaceG%D-1)            :: v
      Type(rational), dimension(:,:),allocatable       :: newLat
      Type(rational) :: det
      integer :: i,j,k,l,n,m,Npos,d,Dd,ng
      character(len=6) :: Strcode
      real             :: latn
      character(len=80), dimension(:),allocatable :: gen_lat, gen_new
      type(spg_type)   :: SpG, P1_t !Auxiliary space groups
      logical          :: centring

      Dd=SpaceG%D
      d=Dd-1
      call Get_Mat_From_Symb_Int(setting, Pmat)
      if(err_CFML%Ierr /= 0) return
      rot=Pmat(1:d,1:d)
      det=Rational_Determ(rot)
      if(det < 0_LI ) then
         err_CFML%Ierr=1
         err_CFML%Msg ="The determinant of the transformation matrix should be positive"
         return
      end if
      !L=(SpaceG%Num_Lat+1+d)*nint(det)
      latn=SpaceG%Num_Lat+1+d
      L=nint(latn*real(det))
      write(*,"(a,i3,a)") " Allocating for: ",L, " centring vectors"
      allocate(newLat(SpaceG%D-1,L))
      newLat=0//1
      invPmat=Rational_Inverse_Matrix(Pmat)
      SpaceG%setting=trim(setting)
      SpaceG%mat2std=Get_Symb_From_Mat(invPmat,StrCode="abc" )
      centring=.false.
      Strcode="xyz"
      if(present(xyz_type)) Strcode=trim(xyz_type)
      roti=Rational_Inverse_Matrix(rot)
      call rational_identity_matrix(identd)

      L=0
      if (SpaceG%Num_Lat > 0) then  !Original lattice is centered
          write(*,"(a,i3)")  " -> Centring vectors",SpaceG%Num_Lat
          do_i:do i=1,SpaceG%Num_Lat      !Transform the centring vectors to the new lattice
            v=Rational_Modulo_Lat(matmul(roti,SpaceG%Lat_tr(:,i)))
            write(*,"(i8,a,10a)") i," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
            if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i
            end do
            L=L+1
            newlat(:,L)=v
            write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
         end do do_i
      end if

      write(*,"(a)")  " -> Basis vectors"
      do_i2:do i=1,d  !Test also the basis vectors of the original setting
        v=Rational_Modulo_Lat(roti(1:d,i))
        write(*,"(i8,a,10a)") i," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
        if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i2
            end do
        L=L+1
        newlat(:,L)=v
        write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
      end do do_i2

      if(L > 0) then !Generate the group P1 with the primitive set of lattice centring in order to get its total number
        allocate(gen_lat(L))
        do i=1,L
          gen_lat(i)=" "
          do j=1,d
            gen_lat(i)=trim(gen_lat(i))//XYZ(j)//"+"//trim(rational_string(newlat(j,i)))//","
          end do
          n=len_trim(gen_lat(i))
          gen_lat(i)(n+1:n+1)="1"
        end do
        call group_constructor(gen_lat,P1_t)
        L=P1_t%Num_Lat
        write(*,"(a)")  " -> Lattice vectors"
        do i=1,L
          newlat(:,i)=P1_t%Lat_tr(:,i)
          write(*,"(i8,a,10a)") i," -> ",(trim(rational_string(newlat(j,i)))//"  ",j=1,d)
       end do
      end if

      SpG%Num_Lat=L
      if(SpG%Num_Lat > 0) centring=.true.

      allocate(gen_new(SpaceG%Numops+1+SpG%Num_Lat))

      Npos=SpaceG%Numops*cent(SpaceG%centred)
      SpG%Multip=Npos*(1+SpG%Num_Lat)
      Spg%NumOps=SpaceG%NumOps

      call Allocate_SpaceGroup(Dd, SpG%Multip, SpG)

      if(SpG%Num_Lat > 0) then
        if(allocated(SpG%Lat_tr)) deallocate(SpG%Lat_tr)
        allocate(SpG%Lat_tr(d,SpG%Num_Lat))
        SpG%Lat_tr(:,:)=newlat(:,1:L)
      end if

      k=0
      do i=1,Npos          !Transform the first Npos operators
        SpG%Op(i)%Mat=matmul(matmul(invPmat,SpaceG%Op(i)%Mat),Pmat)

        SpG%Op(i)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(i)%Mat(1:d,Dd))
        SpG%Op(i)%time_inv=SpaceG%Op(i)%time_inv
        SpG%Op(i)%dt=SpaceG%Op(i)%dt
        SpG%Symb_Op(i)=Get_Symb_from_Mat(SpG%Op(i)%Mat, Strcode,SpG%Op(i)%time_inv)
        if(i > 1 .and. i <= SpaceG%Numops) then
          k=k+1
          gen_new(k)=SpG%Symb_Op(i)
        end if
      end do
      if(SpaceG%centred /= 1) then
        k=k+1
        gen_new(k)=SpG%Symb_Op(SpaceG%Numops+1)
      end if
      ng=k


      if(centring) then
        n=Npos
        do L=1,SpG%Num_Lat
           do k=1,Npos
             SpG%Op(k+n)%Mat=SpG%Op(k)%Mat             !Same matrix as the first NumOps operators
             SpG%Op(k+n)%time_inv=SpG%Op(k)%time_inv   !Same time inversion
             SpG%Op(k+n)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(k)%Mat(1:d,Dd)+SpG%Lat_tr(:,L)) !Different translation
             SpG%Op(k+n)%dt=SpG%Op(k)%dt
             SpG%Symb_Op(k+n)=Get_Symb_from_Mat(SpG%Op(k+n)%Mat, Strcode,SpG%Op(k+n)%time_inv)
             if(k == 1) then
                ng=ng+1
                gen_new(ng) = SpG%Symb_Op(k+n)
             end if
           end do
           n=n+Npos
        end do
      end if

      !Reallocate the array components of the initial space group
      call Allocate_SpaceGroup(Dd, SpG%Multip, SpaceG)
      SpaceG%Multip=SpG%Multip
      SpaceG%Op=SpG%Op             !Copy the array components keeping the names,labels, etc.
      SpaceG%Symb_Op=SpG%Symb_Op
      SpaceG%Num_Lat=Spg%Num_lat
      SpaceG%Num_aLat=SpaceG%Num_aLat*(1+SpG%Num_Lat)

      SpaceG%centred = 1
      do i=1,SpaceG%Multip !Looking for -1
        if(rational_equal(SpaceG%Op(i)%Mat(1:d,1:d),-identd) .and. SpaceG%Op(i)%time_inv ==  1) then
          SpaceG%Centre_coord=rational(1_LI,2_LI) * SpaceG%Op(i)%Mat(1:d,Dd)
          if(sum(abs(SpaceG%Centre_coord)) > 0_LI) then
            SpaceG%centred = 0
            SpaceG%Centre="                                                                                                                           "
            write(unit=SpaceG%Centre,fmt="(a,10a)") "Centrosymmetric with centre at: [ ",(trim(Rational_String(SpaceG%Centre_coord(j)))//" ",j=1,d),"]"
          else
            SpaceG%centred = 2
            SpaceG%Centre_coord=0_LI//1_LI
            SpaceG%Centre="Centrosymmetric with centre at origin"
          end if
          exit
        end if
      end do

      SpaceG%anticentred=1
      do i=1,Npos !Looking for anticentring
        if(rational_equal(SpaceG%Op(i)%Mat(1:d,1:d),-identd) .and. SpaceG%Op(i)%time_inv == -1) then
          SpaceG%AntiCentre_coord=rational(1_LI,2_LI) * SpaceG%Op(i)%Mat(1:d,Dd)
          if(sum(abs(SpaceG%AntiCentre_coord)) > 0_LI) then
            SpaceG%anticentred = 0
            SpaceG%Centre="                                                                                                   "
            write(unit=SpaceG%Centre,fmt="(a,10a)") "Non-centrosymmetric, Anti-centric with -1' @ : [ ",(trim(Rational_String(SpaceG%AntiCentre_coord(j)))//" ",j=1,d),"]"
          else
            SpaceG%anticentred = 2
            SpaceG%AntiCentre_coord=0_LI//1_LI
            SpaceG%Centre="Non-centrosymmetric, Anti-centric with -1' @ origin"
          end if
          exit
        end if
      end do


      if(SpaceG%Num_Lat > 0) then
         if(allocated(SpaceG%Lat_tr)) deallocate(SpaceG%Lat_tr)
         allocate(SpaceG%Lat_tr(d,SpaceG%Num_Lat))     ! Centring vectors
         SpaceG%Lat_tr=SpG%Lat_tr                      ! Direct assignment of vectors
      end if

      if(SpaceG%Num_aLat > 0) then
         if(allocated(SpaceG%aLat_tr)) deallocate(SpaceG%aLat_tr)
         allocate(SpaceG%aLat_tr(d,SpaceG%Num_aLat))     ! Anti-translation vectors
         m=0
         if(SpaceG%Mag_Type /= 2) then
           do k=1,SpaceG%multip
             if(rational_equal(SpaceG%Op(k)%Mat(1:d,1:d),identd) .and. SpaceG%Op(k)%time_inv == -1) then
               m=m+1
               SpaceG%aLat_tr(:,m) = SpaceG%Op(k)%Mat(1:d,Dd)
             end if
           end do
         end if
         SpaceG%Num_aLat=m
      end if
      !Transform the symmetry operators of the list of generators
      !The generators have been constructed above
      SpaceG%generators_list= " "
      do i=1,ng
        SpaceG%generators_list=trim(SpaceG%generators_list)//trim(gen_new(i))//";"
      end do
      i=index(SpaceG%generators_list,";",back=.true.)
      if( i /= 0) then
        SpaceG%generators_list=SpaceG%generators_list(1:i-1)
      end if
   End Subroutine Change_Setting_SpG

   Subroutine Change_new(setting,SpaceG,SpGn,xyz_type)
      !---- Arguments ----!
      character(len=*),             intent(in )    :: setting
      class(spg_type),              intent(in )    :: SpaceG
      class(spg_type), allocatable, intent(out)    :: SpGn
      character(len=*),   optional, intent(in )    :: xyz_type

      !---- Local variables ----!
      Type(rational), dimension(SpaceG%D,SpaceG%D)     :: Pmat,invPmat,Mat
      Type(rational), dimension(SpaceG%D-1,SpaceG%D-1) :: rot,roti,identd
      Type(rational), dimension(SpaceG%D-1)            :: v
      Type(rational), dimension(:,:),allocatable       :: newLat
      Type(rational) :: det
      integer :: i,j,k,l,n,m,Npos,d,Dd,ng,time_inv
      character(len=6) :: Strcode
      real             :: latn
      character(len=80), dimension(:),allocatable :: gen
      character(len=:), allocatable :: generators

      Dd=SpaceG%D
      d=Dd-1
      call Get_Mat_From_Symb_Int(setting, Pmat)
      if(err_CFML%Ierr /= 0) return
      rot=Pmat(1:d,1:d)
      det=Rational_Determ(rot)
      if(det < 0_LI ) then
         err_CFML%Ierr=1
         err_CFML%Msg ="The determinant of the transformation matrix should be positive"
         return
      end if
      generators="GENERATORS "
      !L=(SpaceG%Num_Lat+1+d)*nint(det)
      latn=SpaceG%Num_Lat+1+d
      L=nint(latn*real(det))
      write(*,"(a,i3,a)") " Allocating for: ",L, " centring vectors"
      allocate(newLat(SpaceG%D-1,L))
      newLat=0//1
      invPmat=Rational_Inverse_Matrix(Pmat)
      Strcode="xyz"
      if(present(xyz_type)) Strcode=trim(xyz_type)
      roti=Rational_Inverse_Matrix(rot)
      call rational_identity_matrix(identd)

      L=0
      if (SpaceG%Num_Lat > 0) then  !Original lattice is centered
          write(*,"(a,i3)")  " -> Centring vectors",SpaceG%Num_Lat
          do_i:do i=1,SpaceG%Num_Lat      !Transform the centring vectors to the new lattice
            v=Rational_Modulo_Lat(matmul(roti,SpaceG%Lat_tr(:,i)))
            write(*,"(i8,a,10a)") i," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
            if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i
            end do
            L=L+1
            newlat(:,L)=v
            write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
         end do do_i
      end if

      write(*,"(a)")  " -> Basis vectors"
      do_i2:do i=1,d  !Test also the basis vectors of the original setting
        v=Rational_Modulo_Lat(roti(1:d,i))
        write(*,"(i8,a,10a)") i," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
        if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i2
            end do
        L=L+1
        newlat(:,L)=v
        write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
      end do do_i2

      allocate(gen(L+SpaceG%Multip)) !allocating the maximum number of generators

      if(L > 0) then !Construct the lattice centrering generators
        do i=1,L
          gen(i)=" "
          do j=1,d
            gen(i)=trim(gen(i))//XYZ(j)//"+"//trim(rational_string(newlat(j,i)))//","
          end do
          n=len_trim(gen(i))
          gen(i)(n+1:n+1)="1"
        end do
      end if
      Npos=SpaceG%Numops*cent(SpaceG%centred)
      do i=2,Npos
        Mat=matmul(matmul(invPmat,SpaceG%Op(i)%Mat),Pmat)
        Mat(1:d,Dd)=Rational_Modulo_Lat(Mat(1:d,Dd))
        time_inv=SpaceG%Op(i)%time_inv
        L=L+1
        gen(L)=Get_Symb_from_Mat(Mat,Strcode,time_inv)
      end do

      do i=1,L
        generators= generators//trim(gen(i))//";"
      end do
      i=index(generators,";",back=.true.)
      generators=generators(1:i-1)

      call Set_gSpG_from_string(generators,SpGn,.true.)

      !SpaceG%setting=trim(setting)
      !SpaceG%mat2std=Get_Symb_From_Mat(invPmat,StrCode="abc" )
      !Transform the symmetry operators of the list of generators
      !The generators have been constructed above
      SpGn%generators_list= " "
      do i=1,ng
        SpGn%generators_list=trim(SpGn%generators_list)//trim(gen(i))//";"
      end do
      i=index(SpGn%generators_list,";",back=.true.)
      if( i /= 0) then
        SpGn%generators_list=SpGn%generators_list(1:i-1)
      end if
   End Subroutine Change_new

 End Module Test_Change_sett

 Program Test_SHX_CIF_CFL
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Strings,      only: File_type, u_case
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell, Change_Setting_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info,inverse_setting
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List
    use CFML_IOForm
    use Test_Change_sett

    !---- Local Variables ----!
    implicit none

    character(len=512)                  :: fname,cmdline,title, sett, setting
    integer                             :: i,nlong,narg,lun
    real(kind=cp)                       :: start, fin
    logical                             :: trn_to

    class(Cell_G_Type),allocatable      :: Cell, Celln
    class(Spg_Type),   allocatable      :: SpG, SpGn
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
    i=index(trim(cmdline),".", back=.true.)
    fname=cmdline(1:i)//"cfl"

    !> Start
    call CPU_TIME(start)

    !> Type of Files
    call Read_Xtal_Structure(trim(cmdline), Cell, Spg, Atm)

    if (Err_CFML%Ierr == 0) then
       !> Cell Info
       call Write_Crystal_Cell(Cell)
       write(unit=*,fmt='(a)') " "

       !> SpaceGroup
       call Write_SpaceGroup_Info(SpG)
       write(unit=*,fmt='(a)') " "
    end if
    !write(*,"(a)",advance="no") " => Enter the change of setting: "
    !read(*,"(a)") setting
    !
    !
    !if(len_trim(setting) /= 0)  then
    !  i=index(setting,"trn_to")
    !  if(i /= 0) then
    !      SpG%Mat2std_Shu=setting
    !      sett=inverse_setting(setting(1:i-1),SpG%D)
    !  else
    !      sett=setting
    !  end if
    !  !allocate(SpGn, source=SpG)
    !  allocate(Cell_G_Type :: Celln)
    !  write(*,"(a)") "  Apply change of setting to the actual space group: "//trim(sett)
    !  call Change_new(sett, SpG, SpGn)
    !  call Change_Setting_Cell(Cell,sett,Celln)
    !end if


    !> Print Information
    if (Err_CFML%Ierr == 0) then
       !call Write_Crystal_Cell(Celln)
       !write(unit=*,fmt='(a)') " "
       !
       !!> SpaceGroup
       !call Write_SpaceGroup_Info(SpGn)
       !write(unit=*,fmt='(a)') " "

       !> Atoms
       call Write_Atom_List(Atm)
       write(unit=*,fmt='(a)') " "
       !Write a new CFL file with all information
       open(newunit=lun,file=trim(fname),status="replace", action="write")
       title="CFL file generated from: "//trim(cmdline)
       call Write_CFL_File(Lun,Cell, SpG, Atm, Title)
       close(unit=lun)
    else
       write(unit=*,fmt='(/,a)') " => ERROR: "//trim(Err_CFML%Msg)
    end if
    call CPU_TIME(fin)
    write(unit=*,fmt="(/,a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"


    !!!! TEST JRC
    !call Set_Eps_Math(0.0002_cp)
    !
    !call Readn_Set_Xtal_Structure(fname,Cell,Grp,Atm,"ModAtm_std","CFL")!,file_list=flist) !,Iphase,Job_Info,file_list,CFrame)
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
    !         class is (ModAtm_Std_Type)
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