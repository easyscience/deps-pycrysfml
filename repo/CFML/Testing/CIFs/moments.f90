Program moments_calc
    use CFML_GlobalDeps
    use CFML_Strings,           only: file_List_type,cut_string,u_case,pack_string
    use CFML_gSpaceGroups,      only: SPG_type, Write_SpaceGroup_info, Set_SpaceGroup, Kvect_Info_Type
    use CFML_Atoms,             only: AtList_Type, Write_Atom_List,MAtom_list_Type
    use CFML_metrics,           only: Cell_G_Type, Write_Crystal_Cell
    use CFML_IOForm,            only: Read_Xtal_Structure
    use CFML_kvec_Symmetry

    implicit none

    type (file_List_type)          :: fich_cfl
    class(SPG_type),    allocatable:: SpG
    class(SPG_type),    allocatable:: SpGT
    type (Atlist_Type)             :: A, Amod
    class(Cell_G_Type), allocatable:: Cell
    type (MagSymm_k_Type)          :: MGp
    type (MAtom_list_Type)         :: Am
    type (Point_Orbit_kv), dimension(:,:), allocatable :: orbit_atm
    type (Mod_Orbit)               :: m_orbit
    type (Kvect_Info_Type)         :: kinf

    character(len=256)                  :: filcod,filout,cmdline,cfl_file     !Name of the input file and command line
    character(len=1)                    :: keyv,asig
    integer, dimension(3,2)             :: hlim
    real, dimension(:,:),   allocatable :: maxm
    real, dimension(:),     allocatable :: mmx
    real                                :: start, tend,sig
    real,    dimension(3)               :: R_latt, moment,vp
    integer                             :: lun=1,ier,i,j,k,L, ia, n,nt,nlong,n_ini,n_end,num_op
    integer                             :: narg, mul
    logical                             :: esta, arggiven=.false., ok=.false.,lim=.true., mag_structure, indiv=.false.,modl=.false.
    integer                             :: mult,n_latts
    character(len=10)                   :: lab_at



    !---- Arguments on the command line ----!



    narg=COMMAND_ARGUMENT_COUNT()
    call Get_Command(Command=Cmdline,Length=nlong)
    call cut_string(cmdline,nlong) !Eliminate the name of the program
    cmdline=trim(adjustl(cmdline))
    call cut_string(cmdline,nlong) !Eliminate the name of the file
    cmdline=trim(adjustl(cmdline)) !It should get hlim

    if(narg > 0) then
        call GET_COMMAND_ARGUMENT(1,filcod)
        arggiven=.true.
        cfl_file=filcod
        i=index(filcod,'.cfl',back=.true.)
        j=index(filcod,'.fst',back=.true.)
        if( i /= 0) filcod=filcod(1:i-1)
        if( j /= 0) filcod=filcod(1:j-1)
    end if

    sig=-1.0; asig="m"; modl=.false.
    if(len_trim(cmdline) == 0) then
      lim=.false.
    else
      read(cmdline,*,iostat=ier) (hlim(i,:),i=1,3),sig,modl
      if(ier /= 0) then
        write(*,"(a)") " Error reading the arguments: "//trim(cmdline)
        lim=.false.
        sig=-1.0; modl=.false.; asig="m"
      else
        if(sig > 0.0) asig="p"
      end if
    end if


    write(unit=*,fmt="(/,/,7(a,/))")                                                   &
          "                      ------ PROGRAM MOMENTS ------"                      , &
          "                      ---- Version  0.1 April-2025  ----"                 , &
          "    *******************************************************************"  , &
          "    * Calculates magnetic moment in whatever position of the crystal  *"  , &
          "    *******************************************************************"  , &
          "                            (JRC- April-2025)"
    write(unit=*,fmt=*) " "

    if( .not. arggiven) then
        write(unit=*,fmt="(a)") "  This program needs an input CFL/FST-file with the description of a magnetic structure"
        write(unit=*,fmt="(a)") "  using the formalism of propagation vectors and SYMM/MSYM operators"
        write(unit=*,fmt="(a)") "  The faster way to use it is by providing arguments in invoking the program"
        write(unit=*,fmt="(a)") "  Example:"
        write(unit=*,fmt="(a)") "   > Moments CFL-File.cfl  -3 3 0 1 0 12  -1.0  F"
        write(unit=*,fmt="(a)") "  The program reads the file CFL-File.cfl and calculates all atom "
        write(unit=*,fmt="(a)") "  positions and magnetic moments in the box: a:[-3, 3],b:[0,1],c:[0,12]"
        write(unit=*,fmt="(a)") "  The svalue -1.0 indicates a minus sign in Fourier series,e.g. exp(- 2Pi i k.R_L),"
        write(unit=*,fmt="(a)") "  it may be 1.0 so that exp( 2Pi i k.R_L)"
        write(unit=*,fmt="(a)") "  F is for False (T is for True), it applies modulo lattice translations if T"
        write(unit=*,fmt="(a)") "  If only the name of the file is provided, the box limits are asked by the"
        write(unit=*,fmt="(a)") "  program if no individual atom calculations are selected."
        write(unit=*,fmt="(a)") "  The program generates an mCIF file in P1-derived SSGs."
        write(unit=*,fmt=*) " "
        write(unit=*,fmt=*) " "
        write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
        read(unit=*,fmt="(a)") filcod
        if(len_trim(filcod) == 0) call finish()
    end if

    if( .not. lim) then
      write(unit=*,fmt="(a)",advance="no") " => Do you want to calculate individual atoms? (<cr>=Y) "
      read(unit=*,fmt="(a)") keyv
      if(len_trim(keyv) == 0 .or. keyv == "Y" .or. keyv == "y") then
        indiv=.true.
        lim=.false.
        write(unit=*,fmt="(a)",advance="no") " => Use sig=-1.0 for the exponential ? (<cr>=Y) "
        read(unit=*,fmt="(a)") keyv
        if(len_trim(keyv) == 0 .or. keyv == "Y" .or. keyv == "y") then
          sig=-1.0; asig="m"
        else
          sig=1.0;  asig="p"
        end if
      end if
    end if


    if(.not. indiv) then
       if(.not. lim) then
           do
             write(unit=*,fmt="(a)",advance="no") " => Provide the limits for calculations (e.g. -1 3 0 12 -1 4): "
             read(unit=*,fmt=*,iostat=ier) (hlim(i,:),i=1,3)
             if(ier == 0) exit
           end do
           lim=.true.
       end if
    end if

    write(filout,"(a,L1)") trim(filcod)//"_"//asig,modl
    open(unit=lun,file=trim(filout)//".mom", status="replace",action="write")

    write(unit=lun,fmt="(/,/,7(a,/))")                                                   &
          "                      ------ PROGRAM MOMENTS ------"                      , &
          "                      ---- Version  0.1 April-2025  ----"                 , &
          "    *******************************************************************"  , &
          "    * Calculates magnetic moment in whatever position of the crystal  *"  , &
          "    *******************************************************************"  , &
          "                            (JRC- April-2025)"

    inquire(file=trim(cfl_file),exist=esta)

    if( .not. esta) then
        write(unit=*,fmt="(a)") " File: "//trim(cfl_file)//" doesn't exist!"
        call finish()
    end if

    call cpu_time(start)

    call Read_Xtal_Structure(trim(cfl_file),Cell,SpG,A,FileList=fich_cfl)


    If(err_CFML%Ierr /= 0) then

        write(unit=*,fmt="(a)") "  "//trim(err_CFML%Msg)
        call finish()

    else

        call Write_Crystal_Cell(Cell,lun)
        call Write_SpaceGroup_Info(SpG,lun)
        if(A%natoms > 0) call Write_Atom_List(A,Iunit=lun)


        n_ini=1
        n_end=fich_cfl%nlines

        call Readn_Set_Magnetic_Kv_Structure(fich_cfl,n_ini,n_end,MGp,Am)

        if(err_CFML%Ierr /= 0 .or. err_CFML%flag) then
            write(unit=*,fmt="(a)") " =>"//err_CFML%Msg
            write(unit=lun,fmt="(/a/)") " =>"//err_CFML%Msg
            mag_structure=.false.
        else
            mag_structure=.true.
            call Write_Magnetic_Structure(lun,MGp,Am)
            if(.not. indiv) then
                nt= (hlim(1,2)-hlim(1,1)+1)*(hlim(2,2)-hlim(2,1)+1)*(hlim(3,2)-hlim(3,1)+1)*MGp%Num_Lat
                allocate(orbit_atm(nt,Am%natoms))
                nt=0
                do i=hlim(1,1),hlim(1,2)
                   do j=hlim(2,1),hlim(2,2)
                      do k=hlim(3,1),hlim(3,2)
                         do L=1,MGp%Num_Lat
                            R_latt=real([i,j,k],kind=cp)+MGp%Ltr(:,L)
                            nt=nt+1
                            do ia=1,Am%natoms
                               call Get_kv_Orbit(Am%atom(ia),MGp,R_latt,orbit_atm(nt,ia),sig=sig,modl=modl)
                            end do
                         end do
                      end do
                   end do
                end do

            else
                ! With this routine Lat_Modulo is never applied
                do
                  write(unit=*,fmt="(a)",advance="no") " => Enter the lattice translation, the symmetry operator number and the label of the atom: "
                  read(unit=*,fmt=*,iostat=ier) R_latt, num_op, lab_at
                  if(ier /= 0) exit
                  !Here L
                  call Calc_Magnetic_Moment_kvec(Am,MGp,lab_at,num_op,R_latt,vp,moment,sig=sig)
                  write(unit=*,fmt="(a,i2.2,a,3f8.3,a,2(3f10.4,a))") " => "//trim(lab_at)//"_",num_op," [",R_latt,"]  pos: (",vp,")  m=[",moment,"]"
                end do

            end if
        end if
    end if

    if(lim) then
      allocate(maxm(3,Am%natoms),mmx(Am%natoms))
      maxm=0.0
      mmx=0.0
      do i=1,nt
         do ia=1,Am%natoms
           R_latt=orbit_atm(i,ia)%Latr
           write(unit=lun,fmt="(/,a,3F10.3,a)") " Equivalent Atoms and Magnetic moments in the Cell R_Latt: [",R_latt," ]  Atom : "//trim(Am%Atom(ia)%lab)
           write(unit=lun,fmt="(a)") " ========================================================================================================"
           write(unit=lun,fmt="(a)") " Label                  x         y         z            mx        my        mz"
           do j=1,orbit_atm(i,ia)%mult
             vp=orbit_atm(i,ia)%mom(:,j)
             mmx(ia)=sqrt(dot_product(vp,vp))
             if(mmx(ia) > maxm(1,ia)) then
               maxm(1,ia) = mmx(ia) !Value of the moment
               maxm(2,ia) = j       !Order in the orbit
               maxm(3,ia) = i       !
             end if
             Write(unit=lun,fmt="(a,3f10.5,tr4,3f10.5)") " "//orbit_atm(i,ia)%Labs(j)//" :",orbit_atm(i,ia)%pos(:,j),orbit_atm(i,ia)%mom(:,j)
           end do
         end do
      end do
      write(unit=lun,fmt="(//a)")  " ---------------------------------------------------------------- "
      write(unit=lun,fmt="(a)")    " Maximum values of moments for the different atoms in the crystal "
      write(unit=lun,fmt="(a/)")   " ---------------------------------------------------------------- "
      write(unit=lun,fmt="(a)") " Num                  Label                  x         y         z            mx        my        mz"
      do ia=1,Am%natoms
         i=nint(maxm(3,ia))
         j=nint(maxm(2,ia))
         write(unit=lun,fmt="(a,3F10.3)")                  " Lattice vector: ", orbit_atm(i,ia)%Latr
         Write(unit=lun,fmt="(i4,a,3f10.5,tr4,3f10.5,a,f10.5)") ia,    "       Atom: "//orbit_atm(i,ia)%Labs(j)//" :",orbit_atm(i,ia)%pos(:,j),orbit_atm(i,ia)%mom(:,j),"  Moment: ",maxm(1,ia)
      end do
    end if
    !Writing a superspace magnetic CIF file in P1 (testing)

    !call Construct_Modulated_Structure_P1(A,Am,SpG,MGp,Amod)

    call Get_kv_Orbit_ZeroCell(Am,MGp,m_orbit,sig,modl)
    kinf=get_kvec_info(MGp)

    if(kinf%nk > 0) then
      call Write_CIF_P1_ssg(trim(filcod),Cell,SpG,A,m_orbit,kinf)
    end if

    close(unit=lun)
    call cpu_time(tend)
    write(unit=*,fmt="(/,a)")        " => Normal End of: PROGRAM MOMENTS "
    write(unit=*,fmt="(  a)")        " => Results in file: "//trim(filout)//".mom"
    write(unit=*,fmt="(a,f10.2,a)")  " => CPU-Time: ", tend-start," seconds"

    call finish()

  contains

    Subroutine finish()
      !if(wait_end) then
      !  write(unit=*,fmt="(a)",advance="no") " => Please, press <cr> to finish the program"
      !  read(unit=*,fmt="(a)") keyv
      !  stop
      !else
        stop
      !end if
    End Subroutine finish


End Program moments_calc
