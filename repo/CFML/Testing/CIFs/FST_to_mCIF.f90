Program FST_to_mCIF
    use CFML_GlobalDeps
    use CFML_Strings,     only: file_List_type,u_case
    use CFML_gSpaceGroups,only: SPG_type, Kvect_Info_Type
    use CFML_Atoms,       only: AtList_Type, MAtom_list_Type
    use CFML_metrics,     only: Cell_G_Type
    use CFML_IOForm,      only: Read_Xtal_Structure
    use CFML_kvec_Symmetry

    implicit none

    type (file_List_type)          :: fich_cfl
    class(SPG_type),    allocatable:: SpG
    type (Atlist_Type)             :: A
    class(Cell_G_Type), allocatable:: Cell
    type (MagSymm_k_Type)          :: MGp
    type (MAtom_list_Type)         :: Am
    type (Mod_Orbit)               :: m_orbit
    type (Kvect_Info_Type)         :: kinf

    character(len=256)             :: filcod,cfl_file,cmdline,line     !Name of the input file and command line
    real                           :: start, tend,sig
    real,    dimension(3)          :: moment,vp
    integer                        :: lun=1,ier,i,j,k,L, ia, n,nt,nlong,n_ini,n_end,num_op
    integer                        :: narg, mul
    logical                        :: esta, arggiven=.false., ok=.false., mag_structure, modl=.false.,wait_end
    integer                        :: mult,n_latts
    character(len=10)              :: lab_at

    wait_end=.true.
    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
        call Get_Command(Command=cmdline,Length=nlong)
        call GET_COMMAND_ARGUMENT(1,filcod)
        arggiven=.true.
        cfl_file=filcod
        i=index(filcod,'.cfl',back=.true.)
        j=index(filcod,'.fst',back=.true.)
        k=index(Cmdline,"-f_stop")
        if( i /= 0) filcod=filcod(1:i-1)
        if( j /= 0) filcod=filcod(1:j-1)
        if( k /= 0) wait_end=.false.
    end if

    sig=-1.0; modl=.true.

    write(unit=*,fmt="(/,/,7(a,/))")                                                   &
          "                        ----- PROGRAM FST_to_mCIF -----"                  , &
          "                      ---- Version  0.1 October-2025 ----"                , &
          "    *******************************************************************"  , &
          "    * Generates an mCIF file from the output FST-file of FullProf     *"  , &
          "    * This program works only for incommensurate magnetic structures  *"  , &
          "    *******************************************************************"  , &
          "                            (JRC- October-2025)"
    write(unit=*,fmt=*) " "

    if( .not. arggiven) then
        write(unit=*,fmt="(a)") "  This program needs an input FST/CFL-file with the description of a magnetic structure"
        write(unit=*,fmt="(a)") "  using the formalism of propagation vectors and SYMM/MSYM operators"
        write(unit=*,fmt="(a)") "  The faster way to use it is by providing arguments in invoking the program"
        write(unit=*,fmt="(a)") "  Example:"
        write(unit=*,fmt="(a)") "   > FST_to_mCIF FST-File.fst"
        write(unit=*,fmt="(a)") "  The program reads the file FST-File.fst and calculates all atom positions "
        write(unit=*,fmt="(a)") "  and magnetic modulations using the convention of SSG formalism. The program converts"
        write(unit=*,fmt="(a)") "  the calculations to an mCIF file described in one of the following P1-like SSGs:"
        write(unit=*,fmt="(a)") "     P1(abg)0, P1(a1,b1,g1)0(a2,b2,g2)0 or P1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0"
        write(unit=*,fmt="(a)") "  depending on the number of independent propagation vectors"
        write(unit=*,fmt="(a)") "  The program generates the file mCIF file in one of the P1-derived SSGs."
        write(unit=*,fmt=*) " "
        write(unit=*,fmt=*) " "
        write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl/fst (give xx): "
        read(unit=*,fmt="(a)") filcod
        if(len_trim(filcod) == 0) call finish()
    end if

    inquire(file=trim(cfl_file),exist=esta)

    if( .not. esta) then
        write(unit=*,fmt="(a)") " File: "//trim(cfl_file)//" doesn't exist!"
        call finish()
    end if

    call set_CFML_debug(.true.)
    call cpu_time(start)

    call Read_Xtal_Structure(trim(cfl_file),Cell,SpG,A,FileList=fich_cfl)


    If(err_CFML%Ierr /= 0) then

        write(unit=*,fmt="(a)") "  "//trim(err_CFML%Msg)
        call finish()

    else
        n_ini=1
        n_end=fich_cfl%nlines

        call Readn_Set_Magnetic_Kv_Structure(fich_cfl,n_ini,n_end,MGp,Am)

        if(err_CFML%Ierr /= 0 .or. err_CFML%flag) then
            write(unit=*,fmt="(a)") " =>"//err_CFML%Msg
            write(unit=lun,fmt="(/a/)") " =>"//err_CFML%Msg
            call finish()
        else
            mag_structure=.true.
        end if
    end if
    !Writing a superspace magnetic CIF file in P1 (testing)
    call Get_kv_Orbit_ZeroCell(Am,MGp,m_orbit,sig,modl)
    kinf=get_kvec_info(MGp)
    do i=1,m_orbit%mult
      write(*,"(a,3f10.5,a,3i3,a)") m_orbit%Labs(i),m_orbit%pos(:,i),"  [",m_orbit%Latt(:,i)," ]"
      do k=1,kinf%nq
        vp=0.0_cp
        do j=1,kinf%nk
           vp=vp + kinf%q_coeff(j,k)*kinf%kv(:,j)
        end do
        line=' '
        write(unit=line, fmt='(6i4)') kinf%q_coeff(:,k)
        L=len_trim(line)
        write(unit=line(L+1:), fmt='(2(a,3f10.5))') " Mcos: ",real(m_orbit%Tfourier(:,k,i))," Msin: ",aimag(m_orbit%Tfourier(:,k,i))
        write(unit=*,fmt='(2x,i4,3f10.5,3x,a)') k, vp, trim(line)
      end do
    end do

    if(kinf%nk > 0) then
      call Write_CIF_P1_ssg(trim(filcod),Cell,SpG,A,m_orbit,kinf)
    else
      write(unit=*,fmt="(a)") " => The structure is commensurate (or the file is not complete!), no mCIF is generated"
      write(unit=*,fmt="(a)") " => Please check the file "//trim(cfl_file)//", and merge non-completed items"
    end if
    call cpu_time(tend)
    write(unit=*,fmt="(/,a)")        " => Normal End of: PROGRAM FST_to_mCIF "
    write(unit=*,fmt="(  a)")        " => Results in file: "//trim(filcod)//"_P1_ssg.mcif"
    write(unit=*,fmt="(a,f10.2,a)")  " => CPU-Time: ", tend-start," seconds"

    !call finish()

  contains

  Subroutine finish()
    character(len=1) :: keyv
    if (wait_end) then
      write(unit=*,fmt="(/,a)") " => Press <enter> to finish "
      read(unit=*,fmt="(a)") keyv
      stop
    else
      stop
    end if
  End Subroutine finish

End Program FST_to_mCIF
