submodule (CFML_Utilities) Utilities_Patterns

implicit none

contains

    module subroutine allocate_profile_contributions_for_phases(prph)
        ! Before calling this subroutine, public arrays Ph and Pat defined at
        ! CFML_Powder must be allocated.

        ! Arguments
        type(profile_contrib_phase), dimension(:), allocatable, intent(inout) :: prph

        ! Local variables
        integer :: i,j,k,nc,np

        call clear_error()
        if (.not. allocated(Ph)) then
            err_cfml%ierr = -1
            err_cfml%msg = 'allocate_profile_contributions_for_phases: array of phases is not allocated.'
            return
        end if
        if (.not. allocated(Pat)) then
            err_cfml%ierr = -1
            err_cfml%msg = 'allocate_profile_contributions_for_phases: array of patterns is not allocated.'
            return
        end if
        if(allocated(prph)) deallocate(prph)
        allocate(prph(size(Ph)))
        do i = 1 , size(Ph)
            nc = Ph(i)%Ncontr
            if (allocated(prph(i)%prc)) deallocate(prph(i)%prc) !Profiles of each phase and each pattern
            allocate(prph(i)%prc(nc))
            do j = 1 , Ph(i)%Ncontr
                k = Ph(i)%patterns(j)
                if (Pat(k)%sample == "P") then
                    np = Pat(k)%Pdat%npts
                    if (allocated(prph(i)%prc(j)%yp)) deallocate(prph(i)%prc(j)%yp)
                    allocate(prph(i)%prc(j)%yp(np))
                    prph(i)%prc(j)%n_pts = np
                end if
            end do
        end do

    end subroutine allocate_profile_contributions_for_phases

    module subroutine compute_patterns()
        !> Computes pattern Pat(n) from phases stored in Ph

        ! Local variables
        integer :: i,j,k,l,n,npts
        logical :: is,sf_neutron,sf_xray
        type(profile_contrib_phase), dimension(:), allocatable :: prph

        call clear_error()
        call allocate_profile_contributions_for_phases(prph)
        if (err_cfml%ierr /= 0) return
        
        ! Allocate patterns
        do i = 1 , size(Pat)
            select type (pt => Pat(i)%Pdat)
                class is (DiffPat_E_type)
                    if (allocated(pt%ycalc)) deallocate(pt%ycalc)
                    allocate(pt%ycalc(pt%npts))
                    pt%ycalc = pt%bgr
            end select
        end do

        ! Compute structure factors
        do i = 1 , size(Ph)
            ! Compute structure factors
            call sf_clear_init_symop()
!            !First, convert all molecules to individual atoms before calculation of structure factors
!            !=> Posponed
!            N_mol=Ph(i)%Nmol
!            if (Ph(i)%Nmol > 0) then
!                N_atms = Ph(i)%atm_list%natoms + sum(Ph(i)%mol(1:N_mol)%natoms)
!                n = 0
!                select type (Atm => Ph(i)%atm_list%Atom)
!                type is (Atm_Std_type)
!                    d=0
!                    call Allocate_Atom_List(N_atms, AtL, 'Atm_Std_type', d)
!                    type_Atm='Atm_Std_type'
!                type is (Atm_Ref_type)
!                    d=0
!                    call Allocate_Atom_List(N_atms, AtL, 'Atm_Ref_type', d)
!                    type_Atm='Atm_Ref_type'
!                type is (ModAtm_Std_type)
!                    d=size(atm(1)%Xs)-3
!                    call Allocate_Atom_List(N_atms, AtL, 'ModAtm_Std_type', d)
!                    type_Atm='ModAtm_Std_type'
!                type is (ModAtm_Ref_type)
!                    d=size(atm(1)%Xs)-3
!                    call Allocate_Atom_List(N_atms, AtL, 'ModAtm_Ref_type', d)
!                    type_Atm='ModAtm_Ref_type'
!                end select
!
!                n=0
!                do j = 1 , Ph(i)%Nmol
!                    call Molec_to_AtList(Ph(i)%Mol(j), type_Atm, At, 'F', Ph(i)%Cell)
!                    select type (Atm => AtL%Atom)
!                        type is (Atm_Std_type)
!                            select type (a_atom => At%Atom)
!                                type is(Atm_Std_type)
!                                    Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
!                            end select
!                        type is (Atm_Ref_type)
!                            select type (a_atom => At%Atom)
!                                type is(Atm_Ref_type)
!                                    Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
!                            end select
!                        type is (ModAtm_Std_type)
!                            select type (a_atom => At%Atom)
!                                type is(ModAtm_Std_type)
!                                    Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
!                            end select
!                        type is (ModAtm_Ref_type)
!                            select type (a_atom => At%Atom)
!                                type is(ModAtm_Ref_type)
!                                    Atm(n+1:n+At%natoms)=a_atom(1:At%natoms)
!                            end select
!                    end select
!                    n = n + At%natoms
!                    call allocate_atom_list(0,At,' ',0)
!                end do
!            else
!                AtL=Ph(i)%atm_list
!            end if
            ! Initialization of structure factors
            sf_neutron = .false.
            sf_xray = .false.
            do j = 1 , Ph(i)%Ncontr
                n = Ph(i)%patterns(j)
                if (Pat(n)%cond%job == 1 .and. .not. sf_neutron) then
                    ! Neutrons
                    call init_structure_factors(RL(i),Ph(i)%atm_list,Ph(i)%spg,mode="NUC")
                    if (err_cfml%ierr /= 0) return
                    call structure_factors(RL(i),Ph(i)%atm_list,Ph(i)%spg,mode="NUC")
                    if (err_cfml%ierr /= 0) return
                    sf_neutron = .true.
                else if(Pat(n)%cond%job == 0 .and. .not. sf_xray) then
                    ! Xrays
                    select type (cond => Pat(n)%cond)
                        type is (Powpatt_cw_conditions_type)
                            call init_structure_factors(RL(i),Ph(i)%atm_list,Ph(i)%spg,mode="XRA",lambda=cond%lambda(1))
                            if (err_cfml%ierr /= 0) return
                            call structure_factors(RL(i),Ph(i)%atm_list,Ph(i)%spg,mode="XRA",lambda=cond%lambda(1))
                            if (err_cfml%ierr /= 0) return
                            sf_xray = .true.
                    end select
                end if
            end do
            ! Compute profiles contributions of phases to patterns
            do j = 1 , Ph(i)%Ncontr
                k = Ph(i)%patterns(j) !number of the pattern to which the phase contributes
                if (Pat(k)%sample /= "P") cycle
                if (Pat(k)%mode == "CW") then
                    call Profile_Contribution_CW(i,j,RL(i),prph(i)%prc(j)%yp)
                else if(Pat(k)%mode == "TF") then
                end if
            end do ! Ph(i)%Ncontr
        end do ! N_phases
        ! Compute patterns
        do j = 1 , size(Pat)
            select type (pt => Pat(j)%Pdat)
                class is (DiffPat_E_type)
                pt%ycalc = pt%bgr
            end select
            do i = 1 , size(Ph)
                is = .false.
                do k = 1 , size(Ph(i)%patterns)
                    if (Ph(i)%patterns(k) == j) then
                        is = .true.
                        exit
                    end if
                end do
                if (is) then
                    select type (pt => Pat(j)%Pdat)
                        class is (DiffPat_E_type)
                            do k = 1 , pt%npts
                                pt%ycalc(k) = pt%ycalc(k) + prph(i)%prc(j)%yp(k)
                            end do
                    end select
                end if
            end do
            select type (pt => Pat(j)%Pdat)
                class is (DiffPat_E_type)
                    do i = 1 , pt%npts
                        !pt%y(i)=Random_Poisson(pt%ycalc(i))
                        pt%sigma(i)=sqrt(pt%ycalc(i))
                    end do
            end select
        end do

    end subroutine compute_patterns

    subroutine Profile_Contribution_CW(iph,ipat,RL,yp)

        ! Arguments
        integer,                     intent(in)     :: iph,ipat
        type(RefP_type),             intent(in)     :: RL !It has been totally filled before
        real(kind=cp), dimension(:), intent(out)    :: yp

        ! Local variables
        integer       :: j,i1,i2,k,kn,nref,n_pat,irad
        real(kind=cp) :: pos,Bragg,fwhm,eta,asym1,asym2,scalef,corr,intens,ratio
        real(kind=cp), dimension(2) :: Lambda
        real(kind=cp), dimension(:), allocatable :: y
        logical :: twowaves

        nref  = RL%Nref
        n_pat = Ph(iph)%patterns(ipat)
        select type(cond => Pat(n_pat)%cond)
            type is(PowPatt_CW_Conditions_type)
                asym1 = cond%asym1
                asym2 = cond%asym2
                twowaves = cond%twowaves
                lambda = cond%Lambda
                ratio = cond%ratio
        end select

        scalef = Ph(iph)%scale_factors(ipat)
        yp(:) = 0.0_cp

        select case(Pat(n_pat)%radiation)
            case("X") 
                irad = 1
            case("N") 
                irad = 2
            case("E") 
                irad = 3
            case default 
                irad = 2
        end select
        do k = 1 , Nref  !Calculate the position of the reflection for each pattern and the contributing points
            i1 = RL%ptr(1,k,ipat)
            if (i1 == 0) cycle !The reflection is not contributing to pattern n_pat
            i2 = RL%ptr(2,k,ipat)
            Bragg = RL%pos(k,ipat)
            fwhm = RL%fwhm(k,ipat)
            eta = RL%eta(k,ipat)
            corr = RL%corr(k,ipat)
            !Calculate the profile of the reflection
            select type (rf => RL%Ref)
                class is (Srefl_type)
                    intens = scalef * corr * rf(k)%mult * rf(k)%fc(irad)**2
                    write(77,'(4i8,3f12.4)') rf(k)%H(1:3),rf(k)%mult,corr,rf(k)%fc(irad)
            end select
            if (asym1 > 0.000001_cp) then
                if (allocated(y)) deallocate(y)
                allocate(y(i1:i2)); y=0.0_cp
                call calc_pseudo_voigt(Pat(n_pat)%Pdat%x(i1:i2),y(i1:i2),bragg,eta,fwhm,asym1,asym2)
                yp(i1:i2) = yp(i1:i2) + y(i1:i2) * intens
            else
                do j = i1 , i2
                    yp(j) = yp(j) + pseudovoigt(Pat(n_pat)%Pdat%x(j) - bragg, [fwhm,eta]) * intens
                end do
            end if
            if(twowaves) then
                kn = k + Nref
                Bragg = RL%pos(kn,ipat)
                fwhm = RL%fwhm(kn,ipat)
                eta = RL%eta(kn,ipat)
                i1 = RL%ptr(1,kn,ipat)
                i2 = RL%ptr(2,kn,ipat)
                corr = RL%corr(kn,ipat)
                !Calculate the profile of the reflection
                select type (rf => RL%Ref)
                    class is (Srefl_type)
                        intens = scalef * corr * rf(kn)%mult * rf(kn)%fc(irad)**2 * ratio
                end select
                if (asym1 > 0.000001_cp) then
                    if (allocated(y)) deallocate(y)
                    allocate(y(i1:i2))
                    y = 0.0_cp
                    call calc_pseudo_voigt(Pat(n_pat)%Pdat%x(i1:i2),y(i1:i2),bragg,eta,fwhm,asym1,asym2)
                    yp(i1:i2) = yp(i1:i2) + y(i1:i2) * intens
                else
                    do j = i1 , i2
                        yp(j) = yp(j) + pseudovoigt(Pat(n_pat)%Pdat%x(j) - bragg, [fwhm,eta]) * intens
                    end do
                end if
            end if
        end do

        ! Exclude regions
!      do j=1,Pat(n_pat)%PDat%npts
!        pos=Pat(n_pat)%PDat%x(j)
!        do k=1,Excl(n_pat)%num_excl
!           if(pos >= Excl(n_pat)%Exc(k)%mina .and. pos <= Excl(n_pat)%Exc(k)%maxb) then
!             yp(j)=0.0
!             Exit
!           end if
!        end do
!      end do

    end subroutine Profile_Contribution_CW

    ! ------------------------------------------------
    ! Procedures below should be removed in the future.

    module subroutine cw_powder_pattern(cell,spg,a,ppc,xc,yc,tth)
        !> Computes a powder pattern from cell, space group,
        !> atom list and experimental conditions

        ! Arguments
        class(cell_g_type),                       intent(in)    :: cell !> unit cell
        class(spg_type),                          intent(in)    :: spg  !> space group
        type(atlist_type),                        intent(in)    :: a    !> list of atoms
        type(powpatt_cw_conditions_type),         intent(inout) :: ppc  !> experimental conditions
        real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
        real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
        real(kind=cp), dimension(:), optional,    intent(in)    :: tth  !> two theta axis provided by the user

        ! Local variables
        integer :: maxnref,mult
        real(kind=cp) :: stlmax
        type(reflist_type) :: hkl

        ! Generate reflections
        stlmax = sin(0.5*ppc%tthmax*to_rad) / ppc%lambda(1)
        mult = 2 * spg%numops
        maxnref = get_maxnumref(stlmax,cell%vol) !Removing mult
        call h_uni(cell,spg,.true.,0.0,stlmax,"s",maxnref,hkl,Ref_typ="srefl")
        ! Compute structure factors
        call sf_clear_init_symop()
        if (ppc%job == 1) then     ! Neutrons
            call init_structure_factors(hkl,A,spg,mode="NUC")
            if (err_cfml%ierr /= 0) return
            call structure_factors(hkl,A,spg,mode="NUC")
            if (err_cfml%ierr /= 0) return
        else if(ppc%job == 0) then ! Xrays
            call init_structure_factors(hkl,A,spg,mode="XRA",lambda=ppc%lambda(1))
            if (err_cfml%ierr /= 0) return
            call structure_factors(hkl,A,spg,mode="XRA",lambda=ppc%lambda(1))
            if (err_cfml%ierr /= 0) return
        end if

        ! Compute the powder pattern
        if (present(tth)) then
            call cw_powder_pattern_profile(ppc,hkl,xc,yc,tth)
        else
            call cw_powder_pattern_profile(ppc,hkl,xc,yc)
        end if

    end subroutine cw_powder_pattern

    subroutine cw_powder_pattern_profile(ppc,hkl,xc,yc,tth)

        ! Arguments
        type(powpatt_cw_conditions_type),         intent(in)  :: ppc
        type(reflist_type),                       intent(in)  :: hkl
        real(kind=cp), dimension(:), allocatable, intent(out) :: xc   !> two theta angle
        real(kind=cp), dimension(:), allocatable, intent(out) :: yc   !> calculated intensity
        real(kind=cp), dimension(:), optional,    intent(in)  :: tth  !> two theta axis provided by the user

        ! Local variables
        integer :: i,j,i1,i2,npts
        real(kind=cp) :: bragg,chw,cs,eta,fwhm,hg,hl,intens,lorentzf,ss,th1,th2,tt
        real(kind=cp), dimension(:), allocatable :: y

        ! Allocate arrays
        if (present(tth)) then
            npts = size(tth)
        else
            npts = int((ppc%tthmax - ppc%tthmin) / ppc%step) + 1
        end if
        if (allocated(xc)) deallocate(xc)
        if (allocated(yc)) deallocate(yc)
        allocate(xc(npts),yc(npts))

        ! 2thetas
        if (present(tth)) then
            xc(:) = tth(:)
        else
            do i = 1 , npts
                xc(i) = ppc%tthmin + (i-1) * ppc%step
            end do
        end if
        yc(:) = 0.0

        ! Intensities
        chw = 15.0
        call Init_Prof_Val()
        select type(ref => hkl%ref)
            type is (srefl_type)
                do i = 1 , hkl%nref
                    ss = ppc%Lambda(1) * ref(i)%S ! sin(theta)
                    cs = sqrt(abs(1.0-ss*ss))  ! cos(theta)
                    tt = ss / cs               ! tan(theta)
                    lorentzf = 0.5/(ss*ss*cs)
                    bragg = 2.0*asind(ss) + ppc%zero
                    hg = sqrt(tt*(ppc%U*tt+ppc%V)+ppc%W)
                    hl = ppc%X*tt + ppc%Y/cs
                    call get_fwhm_eta(hg,hl,fwhm,eta)
                    select case(nint(eta*10.0))
                        case(:2)
                            chw = 25.0
                        case(3:5)
                            chw = 45.0
                        case(6:7)
                            chw = 60.0
                        case(8:)
                            chw = 90.0
                    end select
                    th1 = Bragg-chw*fwhm
                    th2 = Bragg+chw*fwhm
                    i1 = locate(xc,th1,npts)
                    i2 = locate(xc,th2,npts)
                    i1 = max(i1,1)
                    i2 = min(i2,npts)
                    intens = lorentzf * ref(i)%mult * ref(i)%fc(2)**2 * ppc%scale_factor
                    write(88,'(4i8,3f12.4)') ref(i)%H(1:3),ref(i)%mult,lorentzf,ref(i)%fc(2)
                    if (ppc%is_asym) then
                        if (allocated(y)) deallocate(y)
                        allocate(y(i1:i2))
                        call calc_pseudo_voigt(xc(i1:i2),y(i1:i2),bragg,eta,fwhm,ppc%asym1,ppc%asym2)
                        do j = i1 , i2
                            yc(j) = yc(j) + y(j) * intens
                        end do
                    else
                        do j = i1 , i2
                            yc(j) = yc(j) + pseudovoigt(xc(j) - bragg, [fwhm,eta]) * intens
                        end do
                    end if
                end do
        end select

    end subroutine cw_powder_pattern_profile

    subroutine cw_read_cfl(cfl_file,cell,spg,a,ppc)
        !> Read content of a cfl file.

        ! Arguments
        type(file_type),                  intent(inout) :: cfl_file !> cfl file content
        class(cell_g_type), allocatable,  intent(out)   :: cell     !> unit cell
        class(spg_type),    allocatable,  intent(out)   :: spg      !> space group
        type(atlist_type),                intent(out)   :: a        !> list of atoms
        type(powpatt_cw_conditions_type), intent(out)   :: ppc      !> experimental conditions

        ! Local variables
        integer :: nph,npatt
        type(blockinfo_type), dimension(:), allocatable :: phase_block
        type(blockinfo_type), dimension(:), allocatable :: pattern_block

        phase_block = get_cfl_block_info(cfl_file,"phase")
        pattern_block = get_cfl_block_info(cfl_file,"pattern")

        !!ierror = json%getitem(item,'cif')
        !!if (ierror == 0) then
        !!    ! Structural data in a CIF file
        !!    if (ierror == 0) ierror = cast(cif,item)
        !!    if (ierror == 0) call read_xtal_structure(cif,cell,spg,a)
        !!    if (ierror /= 0) then
        !!        err_cfml%ierr = -1
        !!        err_cfml%msg = 'cw_read_json: unable to cast cif'
        !!    end if
        !!    if (err_cfml%ierr /= 0) return
        !!else
        !!    ! Structural data in the JSON file
        !!    call xtal_structure_from_json(json,0,cell,spg,a)
        !!    if (err_cfml%ierr /= 0) return
        !!end if
        !!!call write_spacegroup_info(spg,21)
        !!call cw_experimental_conditions_from_json(json,ppc)
        !!if (err_cfml%ierr /= 0) return

    end subroutine cw_read_cfl

    subroutine set_fullprof_occupancies(a,spg)

        ! Arguments
        type(atlist_type), intent(inout) :: a    !> list of atoms
        class(spg_type),   intent(in)    :: spg  !> space group

        ! Local variables
        integer :: i,mpos

        do i = 1 , a%natoms
            mpos = get_multip_pos(a%atom(i)%x,spg)
            a%atom(i)%occ = a%atom(i)%occ * mpos / (1.0*spg%multip)
        end do

    end subroutine set_fullprof_occupancies

    module subroutine tof_powder_pattern(cell,spg,a,ppc,xc,yc,tof)
        !> Computes a powder pattern from cell, space group,
        !> atom list and experimental conditions

        ! Arguments
        class(cell_g_type),                       intent(in)    :: cell !> unit cell
        class(spg_type),                          intent(in)    :: spg  !> space group
        type(atlist_type),                        intent(in)    :: a    !> list of atoms
        type(powpatt_tof_conditions_type),        intent(inout) :: ppc  !> experimental conditions
        real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
        real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
        real(kind=cp), dimension(:), optional,    intent(in)    :: tof  !> tof axis provided by the user

        ! Local variables
        integer :: maxnref,mult
        real(kind=cp) :: d_min,stlmax
        type(reflist_type) :: hkl

        ! Generate reflections
        d_min = (ppc%tof_min - ppc%zero) / ppc%dtt1
        stlmax = 1. / d_min / 2.
        mult = 2 * spg%numops
        maxnref = get_maxnumref(stlmax,cell%vol,multip=mult)
        call h_uni(cell,spg,.true.,0.0,stlmax,"s",maxnref,hkl,Ref_typ="srefl")
        ! Compute structure factors
        call sf_clear_init_symop()
        call init_structure_factors(hkl,A,spg,mode="NUC")
        if (err_cfml%ierr /= 0) return
        call structure_factors(hkl,A,spg,mode="NUC")
        if (err_cfml%ierr /= 0) return

        ! Compute the powder pattern
        if (present(tof)) then
            call tof_powder_pattern_profile(ppc,hkl,xc,yc,tof)
        else
            call tof_powder_pattern_profile(ppc,hkl,xc,yc)
        end if

    end subroutine tof_powder_pattern

    subroutine tof_powder_pattern_profile(ppc,hkl,xc,yc,tof)

        ! Arguments
        type(powpatt_tof_conditions_type),        intent(in)  :: ppc
        type(reflist_type),                       intent(in)  :: hkl
        real(kind=cp), dimension(:), allocatable, intent(out) :: xc   !> tof
        real(kind=cp), dimension(:), allocatable, intent(out) :: yc   !> calculated intensity
        real(kind=cp), dimension(:), optional,    intent(in)  :: tof  !> tof axis provided by the user

        ! Local variables
        integer :: i,n,npts
        real(kind=cp) :: dsp,dsp2,dsp4,dt,H_g,H_l,omega,r,sina,w_neg,w_pos
        real(kind=cp), dimension(:), allocatable :: alpha,beta,eta,H,lorentz,tofs
        real(kind=cp), dimension(:,:), allocatable :: limits

        ! Local parameters
        real(kind=cp), parameter :: o1=2.69269, o2=2.42843, o3=4.47163, o4=0.07842
        real(kind=cp), parameter :: e1=1.36603, e2=0.47719, e3=0.11116

        sina = sin(ppc%bank_angle * TO_RAD)

        ! Allocate arrays
        if (present(tof)) then
            npts = size(tof)
        else
            npts = int((ppc%tof_max - ppc%tof_min) / ppc%step) + 1
        end if
        if (allocated(xc)) deallocate(xc)
        if (allocated(yc)) deallocate(yc)
        allocate(xc(npts),yc(npts))

        ! x-axis
        if (present(tof)) then
            xc(:) = tof(:)
        else
            do i = 1 , npts
                xc(i) = ppc%tof_min + (i-1) * ppc%step
            end do
        end if
        yc(:) = 0.0

        ! Reflection parameters
        if (allocated(alpha)) deallocate(alpha)
        if (allocated(beta)) deallocate(beta)
        if (allocated(eta)) deallocate(eta)
        if (allocated(H)) deallocate(H)
        if (allocated(limits)) deallocate(limits)
        if (allocated(lorentz)) deallocate(lorentz)
        if (allocated(tofs)) deallocate(tofs)
        allocate(alpha(hkl%nref))
        allocate(beta(hkl%nref))
        allocate(eta(hkl%nref))
        allocate(H(hkl%nref))
        allocate(limits(2,hkl%nref))
        allocate(lorentz(hkl%nref))
        allocate(tofs(hkl%nref))
        do i = 1 , hkl%nref
            dsp = 0.5 / hkl%ref(i)%s
            dsp2 = dsp * dsp
            dsp4 = dsp2 * dsp2
            tofs(i) = ppc%zero + ppc%dtt1 * dsp + ppc%dtt2 * dsp2
            alpha(i) = ppc%alpha0 + ppc%alpha1 / dsp
            beta(i) = ppc%beta0 + ppc%beta1 / dsp4
            lorentz(i) = dsp4 * sina
            H_g = (ppc%sigma0 + ppc%sigma1 * dsp2 + ppc%sigma2 * dsp4) / INV_8LN2
            H_g = sqrt(h_g)
            H_l = ppc%gamma0 + ppc%gamma1 * dsp + ppc%gamma2 * dsp2
            H(i) = H_g**5+o1*H_g**4*H_l+o2*H_g**3*H_l**2+o3*H_g**2*H_l**3+o4*H_g*H_l**4+H_l**5
            H(i) = abs(H(i))**0.2_cp
            r = H_l / H(i)
            eta(i) = max(1.0e-06_cp, r*(e1 -(e2 + e3*r)*r))
            w_neg = 1.38629436112/alpha(i)
            w_pos = 1.38629436112/beta(i)
            limits(1,i) = tofs(i) - 15 * (H_g+w_neg)
            limits(2,i) = tofs(i) + 15 * (H_g+w_pos)
        end do

        ! Intensities
        select type(ref => hkl%ref)
            type is (srefl_type)
                do n = 1 , npts
                    do i = 1 , hkl%nref
                        if (xc(n) < limits(1,i) .or. xc(n) > limits(2,i)) cycle
                        dt = xc(n) - tofs(i)
                        call tof_Jorgensen_VonDreele(dt,alpha(i),beta(i),H(i),eta(i),omega)
                        yc(n) = yc(n) + lorentz(i) * ref(i)%mult * ref(i)%fc(2)**2 * ppc%scale_factor * omega
                    end do
                end do
            end select

    end subroutine tof_powder_pattern_profile

end submodule Utilities_Patterns