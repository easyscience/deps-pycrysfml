program powpat

    use CFML_Atoms,      only: AtList_Type,Write_Atom_List
    Use CFML_DiffPatt,   only: DiffPat_Type,PowPatt_CW_Conditions_Type,PowPatt_TOF_Conditions_Type,Write_Pattern
    use CFML_GlobalDeps, only: Clear_Error,Cp,Err_CFML
    use CFML_IOForm,     only: BlockInfo_Type,Get_CFL_Block_Info,Pattern_Type,Phase_Type,Read_CFL_Pattern,&
                               Read_CFL_Phase,Write_CFL_Pattern,Write_CFL_Phase
    use CFML_Metrics,    only: Cell_G_Type
    use CFML_Strings,    only: File_Type,Reading_File
    use CFML_Utilities,  only: CW_Powder_Pattern,TOF_Powder_Pattern

    integer                                         :: i,j,k,narg,nph,npatt
    character(len=1024)                             :: cfl_name,filcod
    real(kind=cp),        dimension(:), allocatable :: xc,yc
    type(BlockInfo_Type), dimension(:), allocatable :: block_ph,block_patt
    type(File_Type)                                 :: cfl_file
    type(Phase_Type),     dimension(:), allocatable :: phase
    type(Pattern_Type),   dimension(:), allocatable :: pattern

    ! Get CFL name
    narg = command_argument_count()
    if(narg > 0) then
        call GET_COMMAND_ARGUMENT(1,cfl_name)
    else
        write(*,'(a)',advance = 'no') 'CFL file: '
        read*, cfl_name
    end if
    i = index(cfl_name,'.cfl',back=.true.)
    if (i < 1) cfl_name = cfl_name//".cfl"
    filcod = cfl_name(1:i-1)

    ! Read CFL file
    cfl_file = Reading_file(cfl_name)
    call check_error()

    ! Find phase and patterns blocks
    block_ph = Get_CFL_Block_Info(cfl_file,'phase_')
    call check_error()
    block_patt = Get_CFL_Block_Info(cfl_file,'pattern_')
    call check_error()

    ! Assign memory to arrays
    nph = size(block_ph)
    npatt = size(block_patt)
    allocate(phase(nph))
    allocate(pattern(npatt))

    ! Read phases
    do i = 1 , nph
        call read_cfl_phase(cfl_file,phase(i),'atm_type',0,i_ini=block_ph(i)%nl(1),i_end=block_ph(i)%nl(2))
        call check_error()
        call write_cfl_phase(6,phase(i),i)
        call check_error()
    end do

    ! Read patterns
    do i = 1 , npatt
        call read_cfl_pattern(cfl_file,pattern(i),i_ini=block_patt(i)%nl(1),i_end=block_patt(i)%nl(2))
        call check_error()
        call write_cfl_pattern(6,pattern(i),i)
    end do

    ! Compute patterns
    do i = 1 , npatt
        do j = 1 , nph
            do k = 1 , size(phase(j)%patterns)
                if (phase(j)%patterns(k) == i) then
                    select type (c => pattern(i)%cond)
                        class is (PowPatt_CW_Conditions_Type)
                            call cw_powder_pattern(phase(j)%cell,phase(j)%spg,phase(j)%atm_list,c,xc,yc)
                        class is (PowPatt_TOF_Conditions_Type)
                            call tof_powder_pattern(phase(j)%cell,phase(j)%spg,phase(j)%atm_list,c,xc,yc)
                    end select
                    if (.not. allocated(pattern(i)%data%x)) then
                        allocate(pattern(i)%data%x(size(xc)))
                        allocate(pattern(i)%data%y(size(yc)))
                        allocate(pattern(i)%data%sigma(size(yc)))
                        pattern(i)%data%x = xc
                        pattern(i)%data%y = 0.0
                        pattern(i)%data%sigma = 0.0
                        pattern(i)%data%npts = size(xc)
                    end if
                    pattern(i)%data%y = pattern(i)%data%y + yc * phase(j)%scale_factors(k)
                end if
            end do
        end do
        call write_pattern(pattern(i)%name//'.xys',pattern(i)%data,'XYS')
    end do

    contains

    subroutine check_error()
        if (err_cfml%ierr /= 0) then
            write(*,'(a)') trim(err_cfml%msg)
            stop
        end if
    end subroutine

end program