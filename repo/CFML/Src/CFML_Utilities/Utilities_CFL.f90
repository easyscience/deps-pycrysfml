submodule (CFML_Utilities) Utilities_CFL

implicit none

contains

    module subroutine check_patterns_allocation()
        !> Checks if the array of patterns is allocated. If not, sets an error in err_cfml.
        if (.not. allocated(Pat)) then
            err_cfml%ierr = -1
            err_cfml%msg = 'check_patterns_allocation: array of patterns is not allocated.'
        end if
    end subroutine check_patterns_allocation

    module subroutine check_phases_allocation()
        !> Checks if the array of phases is allocated. If not, sets an error in err_cfml.
        if (.not. allocated(Ph)) then
            err_cfml%ierr = -1
            err_cfml%msg = 'check_phases_allocation: array of phases is not allocated.'
        end if
    end subroutine check_phases_allocation

    module subroutine read_cfl(cfl)
        !> Computes patterns from instructions given in a cfl file

        ! Arguments
        type(file_type), intent(in) :: cfl !> cfl file type

        ! Local parameters
        integer, parameter :: NB_MAX = 100 ! Maximum number of blocks

        ! Local variables
        integer :: n
        integer :: NB_Patt  ! Number of Pattern blocks
        integer :: NB_Phas  ! Number of Phase blocks
        logical, dimension(:), allocatable :: gen_patt
        type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Patt ! Pattern blocks
        type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Phas ! Phase blocks

        call Clear_Error()

        ! Set number of phases and patterns blocks
        call Get_Blocks_Filetype(cfl,NPatt=NB_Patt,Bl_Patt=Bl_Patt,NPhas=NB_Phas,Bl_Phas=Bl_Phas)
        if (err_cfml%ierr /= 0) return
        if (NB_Phas == 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'patterns_simulation: no phase block found in cfl file.'
            return
        end if
        if (NB_Patt == 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'patterns_simulation: no pattern block found in cfl file.'
            return
        end if
        ! Assign memory to arrays
        if (allocated(Ph)) deallocate(Ph)
        if (allocated(Pat)) deallocate(Pat)
        allocate(Ph(NB_Phas))
        allocate(Pat(NB_Patt))
        allocate(gen_patt(NB_Patt))
        ! Read patterns
        do n = 1 , NB_Patt
            Pat(n)%irf = 0
            call read_cfl_pattern(cfl,Pat(n),Bl_Patt(n)%Nl(1),Bl_Patt(n)%Nl(2),gen_patt(n))
            if (err_cfml%ierr /= 0) return
        end do
        ! Read phases
        do n = 1 , NB_Phas
            call Read_XTal_CFL(cfl,Ph(n)%Cell,Ph(n)%Spg,Ph(n)%Atm_list,NPhase=n)
            if (err_cfml%ierr /= 0) return
            Ph(n)%atm_list%iph = n
            Ph(n)%name = Bl_Phas(n)%strname
            call Read_Phase_PattContr(cfl,Bl_Phas(n)%Nl(1),Bl_Phas(n)%Nl(2),Ph(n))
            if (err_cfml%ierr /= 0) return
            !nc=Ph(i)%Ncontr
            !if(allocated(prph(i)%prc)) deallocate(prph(i)%prc) !Profiles of each phase and each pattern
            !allocate(prph(i)%prc(nc))  !For Phase i, Ph(i)%Ncontr profiles are stored
            !do j=1,Ph(i)%Ncontr
            !k=Ph(i)%patterns(j)
            !if(Pat(k)%sample == "P") then
            !    np=Pat(k)%Pdat%npts
            !    if(allocated(prph(i)%prc(j)%yp)) deallocate(prph(i)%prc(j)%yp)
            !    allocate(prph(i)%prc(j)%yp(np))
            !    prph(i)%prc(j)%n_pts=np
            !end if
            !end do
        end do

    end subroutine read_cfl

    subroutine Read_Phase_PattContr(cfl,N_ini,N_end,Phase)

        ! Arguments
        Type(file_type),         intent(in)    :: cfl
        integer,                 intent(in)    :: n_ini
        integer,                 intent(in)    :: n_end
        Type(Phase_Type),        intent(inout) :: Phase

        ! Local Variables
        integer                       :: i, j, iv
        character(len=:),allocatable  :: line
        logical                       :: contr_pat, scale_pat
        integer,        dimension(15) :: ivet
        real(kind=cp),  dimension(15) :: vet

        call clear_error()

        Phase%Ncontr = 0
        contr_pat    = .false.
        scale_pat    = .false.
        vet          = 0.0_cp
        ivet         = 0
        if(allocated(Phase%patterns)) deallocate(Phase%patterns)
        if(allocated(Phase%pat_mode)) deallocate(Phase%pat_mode)
        if(allocated(Phase%pat_sample)) deallocate(Phase%pat_sample)
        if(allocated(Phase%scale_factors)) deallocate(Phase%scale_factors)

        do i = N_ini , N_end
            if(contr_pat .and. scale_pat) exit
            line=l_case(adjustl(cfl%line(i)%str))
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            j=index(line,'!')
            if (j > 0) line=line(:j-1)
            j=index(line,'#')
            if (j > 0) line=line(:j-1)
            j=index(line,'patterns')
            if(j /= 0) then
                line=line(j+8:)
                call Get_Num(line, vet, ivet, iv)
                if(iv == 0) then
                    call Set_Error(1,"At least 1 pattern should be specified after PATTERNS keyword")
                    return
                end if
                allocate(Phase%patterns(iv),Phase%pat_mode(iv),Phase%pat_sample(iv),Phase%pow(iv),Phase%sxt(iv))
                Phase%patterns=vet(1:iv)
                Phase%pat_mode=" "
                Phase%pat_sample=" "
                Phase%Ncontr=iv
                contr_pat=.true.
                cycle
            end if
            j=index(line,"scale_factors")
            if(j /= 0) then
                line=line(j+13:)
                call Get_Num(line, vet, ivet, iv)
                if(iv == 0) exit
                allocate(Phase%scale_factors(iv))
                Phase%scale_factors=vet(1:iv)
                scale_pat=.true.
                cycle
            end if
        end do
        if (.not. scale_pat .and. contr_pat) then
            allocate(Phase%scale_factors(Phase%Ncontr))
            Phase%scale_factors=1.0
        end if

    end subroutine Read_Phase_PattContr

end submodule Utilities_CFL