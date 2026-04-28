submodule (CFML_Py_Utilities) Utilities_Structure_Factors

implicit none

contains

    module function magnetic_structure_factors_from_mcif(mcif_file,sfacsymb,experiment,sintlmin,sintlmax,unique,mag_only,mag_ext,friedel) Result(hkl)
        !> Computes nuclear and magnetic structure factors from a mcif file

        ! Arguments
        character(len=*),               intent(in) :: mcif_file   !> mcif file
        character(len=4), dimension(:), intent(in) :: sfacsymb    !> scattering factor symbols for atoms in the mcif
        character(len=*), optional,     intent(in) :: experiment  !> powder: "P" | single-crystal: "S"
        real(kind=cp),    optional,     intent(in) :: sintlmin    !> minimum value of sin(theta) / lambda
        real(kind=cp),    optional,     intent(in) :: sintlmax    !> maximum value of sin(theta) / lambda
        logical,          optional,     intent(in) :: unique      !> generate only unique reflections
        logical,          optional,     intent(in) :: mag_only    !> if True, generate only magnetic reflections
        logical,          optional,     intent(in) :: mag_ext     !> if True, consider magnetic extinctions
        logical,          optional,     intent(in) :: friedel     !> if True, apply Friedel law
        type(reflist_type)                         :: hkl         !> reflection list

        ! Local variables
        integer :: i,mult,maxnref
        real(kind=cp) :: smin,smax,wave
        character(len=1) :: mode
        character(len=:), allocatable :: experiment_
        logical :: existe,friedel_law,magonly,magext,ordering
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: a
        type(scattering_species_type) :: scf
        type(strflist_type) :: stf

        ! Reset variable err_cfml
        call clear_error()

        ! Default values for optional parameters
        mode = 'P'
        wave = 1.54
        smin = 0.0
        smax = 1.0
        friedel_law = .true.
        magonly = .false.
        magext = .true.
        ordering = .true.

        ! Check if the cif exists
        inquire(file=mcif_file,exist=existe)
        if (.not. existe) then
            err_cfml%ierr = 1
            err_cfml%msg  = 'File '//trim(mcif_file)//' does not exist.'
            return
        end if

        ! Optional arguments
        if (present(experiment)) then
            experiment_ = u_case(adjustl(trim(experiment)))
            if (experiment_ == 'S') mode = 'S'
        end if
        if (present(sintlmin)) smin        = sintlmin
        if (present(sintlmax)) smax        = sintlmax
        if (present(mag_only)) magonly     = mag_only
        if (present(mag_ext))  magext      = mag_ext
        if (present(unique))   ordering    = unique
        if (present(friedel))  friedel_law = friedel

        ! Read crystal structure from file
        call read_xtal_structure(mcif_file,cell,spg,a)
        if (err_cfml%ierr /= 0) return

        ! Set scattering factor symbols for magnetic atoms
        if (size(sfacsymb) /= a%natoms) then
            err_cfml%ierr = 1
            err_cfml%msg = 'magnetic_structure_factors_from_mcif: Number of atoms and scattering factor symbols is different'
            return
        end if
        do i = 1 , a%natoms
            if (a%atom(i)%magnetic) a%atom(i)%SfacSymb = sfacsymb(i)
        end do

        ! Generate reflections
        call gener_reflections(cell,smin,smax,hkl,spg,magext=magext,order=ordering,mag_only=magonly,friedel=friedel_law,ref_typ='mrefl')
        if (err_cfml%ierr /= 0) return

        ! Compute magnetic structure factors
        call sf_clear_init_symop()
        call sf_init_opmattr(spg)
        call set_form_factors(a,scf,mag=.true.)
        if (err_cfml%ierr /= 0) return
        stf%nref = hkl%nref
        allocate(stf%strf(stf%nref))
        select type(ref => hkl%ref)
            type is (mrefl_type)
                do i = 1 , hkl%nref
                    call calc_mag_structure_factor(ref(i),cell,spg,a,scf,mode,stf%strf(i))
                    ref(i)%a     = real(stf%strf(i)%nsf)
                    ref(i)%b     = aimag(stf%strf(i)%nsf)
                    ref(i)%fc    = sqrt(stf%strf(i)%sqnuc)
                    ref(i)%phase = atan2(ref(i)%b,ref(i)%a) * to_deg
                    ref(i)%msf   = stf%strf(i)%msf
                    ref(i)%miv   = stf%strf(i)%miv
                    ref(i)%mivc  = stf%strf(i)%mivc
                    ref(i)%sqmiv = stf%strf(i)%sqmiv
                end do
        end select

    end function magnetic_structure_factors_from_mcif

    module function structure_factors_from_cif(cif_file,radiation,lambda,sintlmin,sintlmax,unique,friedel) Result(hkl)
        !> Computes structure factors from a cif file

        ! Arguments
        character(len=*),           intent(in) :: cif_file  !> cif file
        character(len=*), optional, intent(in) :: radiation !> x-rays: "XRA" | neutrons: "NEU" | electrons: "ELE"
        real(kind=cp),    optional, intent(in) :: lambda    !> wavelength
        real(kind=cp),    optional, intent(in) :: sintlmin  !> minimum value of sin(theta) / lambda
        real(kind=cp),    optional, intent(in) :: sintlmax  !> maximum value of sin(theta) / lambda
        logical,          optional, intent(in) :: unique    !> generate only unique reflections
        logical,          optional, intent(in) :: friedel   !> if True, apply Friedel law
        type(reflist_type)                     :: hkl       !> reflection list

        ! Local variables
        integer :: mult,maxnref
        real(kind=cp) :: smin,smax,wave
        character(len=3) :: mode
        character(len=:), allocatable :: mode_
        logical :: existe,friedel_law,ordering
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: a

        call clear_error()

        ! Default values for optional parameters
        mode = 'XRA'
        wave = 1.54
        smin = 0.0
        smax = 1.0
        friedel_law = .true.
        ordering = .false.

        ! Check if the cif exists
        inquire(file=cif_file,exist=existe)
        if (.not. existe) then
            err_cfml%ierr = 1
            err_cfml%msg  = 'File '//trim(cif_file)//' does not exist.'
            return
        end if

        ! Optional arguments
        if (present(radiation)) then
            mode_ = u_case(adjustl(trim(radiation)))
            select case (mode_)
                case ('NEU','NUC')
                    mode = 'NUC'
                case ('ELE')
                    mode = 'ELE'
            end select
        end if
        if (present(lambda))   wave        = lambda
        if (present(sintlmin)) smin        = sintlmin
        if (present(sintlmax)) smax        = sintlmax
        if (present(unique))   ordering    = unique
        if (present(friedel))  friedel_law = friedel

        ! Read crystal structure from file
        call read_xtal_structure(cif_file,cell,spg,a)
        if (err_cfml%ierr /= 0) return

        ! Generate reflections
        call gener_reflections(cell,smin,smax,hkl,spg,unique=ordering,friedel=friedel_law,ref_typ='srefl')
        if (err_cfml%ierr /= 0) return

        ! Compute structure factors
        call sf_clear_init_symop()
        call init_structure_factors(hkl,A,spg,mode=mode,lambda=wave)
        if (err_cfml%ierr /= 0) return
        call structure_factors(hkl,A,spg,mode=mode,lambda=wave)
        if (err_cfml%ierr /= 0) return

    end function structure_factors_from_cif

end submodule Utilities_Structure_Factors