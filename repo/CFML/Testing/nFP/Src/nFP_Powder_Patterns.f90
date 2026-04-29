Module nFP_Powder_Patterns

    Use CFML_GlobalDeps,        only: clear_error,cp,dp,err_cfml,to_deg,to_rad
    Use CFML_Maths,             only: locate
    Use CFML_Reflections,       only: RefList_Type, Refl_Type
    Use CFML_Structure_Factors, only: StrfList_Type
    Use CFML_DiffPatt,          only: DiffPat_Type, DiffPat_E_Type, allocate_pattern, powpatt_cw_conditions_type, &
                                      powpatt_tof_conditions_type
    use CFML_Profiles,          only: PseudoVoigt,Calc_Pseudo_Voigt,Get_Fwhm_Eta,Pseudovoigt, &
                                      Tof_Jorgensen_Vondreele,Prof_Val,Init_Prof_Val
    Use CFML_gSpaceGroups,      only: SpG_Type, get_multip_pos, set_spacegroup,write_spacegroup_info
    Use CFML_Strings,           only: Set_Symb_From_Mat,pack_string,l_case,Reading_File,File_Type
    Use CFML_Rational
    Use CFML_Atoms,             only: AtList_Type, Allocate_Atom_List,Write_Atom_List
    Use CFML_Metrics,           only: Cell_G_type, set_Crystal_Cell,write_crystal_cell
    Use CFML_Reflections,       only: RefList_Type,H_uni,get_maxnumref,Initialize_RefList,Srefl_Type
    Use CFML_gSpaceGroups,      only: SpG_Type, Set_SpaceGroup, Get_moment_ctr_Wigner,&
                                      Get_moment_ctr,Write_SpaceGroup_Info, get_multip_pos
    Use CFML_Structure_Factors, only: Write_Structure_Factors, Structure_Factors,StrfList_Type,&
                                      Init_Structure_Factors, Magnetic_Structure_Factors, &
                                      Sf_Clear_Init_Symop


    implicit none
    public

contains

    subroutine cw_powder_pattern(cell,spg,a,ppc,xc,yc,tth)
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
        stlmax = sin(0.5*ppc%tthmax*to_rad) / ppc%lambda
        mult = 2 * spg%numops
        maxnref = get_maxnumref(stlmax,cell%vol,mult=mult)
        call initialize_reflist(maxnref,hkl,"srefl")
        call h_uni(cell,spg,.true.,0.0,stlmax,"s",maxnref,hkl)
        ! Compute structure factors
        call sf_clear_init_symop()
        if (ppc%job == 1) then     ! Neutrons
            call init_structure_factors(hkl,A,spg,mode="NUC")
            if (err_cfml%ierr /= 0) return
            call structure_factors(hkl,A,spg,mode="NUC")
            if (err_cfml%ierr /= 0) return
        else if(ppc%job == 0) then ! Xrays
            call init_structure_factors(hkl,A,spg,mode="XRA",lambda=ppc%lambda)
            if (err_cfml%ierr /= 0) return
            call structure_factors(hkl,A,spg,mode="XRA",lambda=ppc%lambda)
            if (err_cfml%ierr /= 0) return
        end if

        ! Compute the powder pattern
        if (present(tth)) then
            call cw_powder_pattern_profile(ppc,hkl,xc,yc,tth)
        else
            call cw_powder_pattern_profile(ppc,hkl,xc,yc)
        end if

    end subroutine cw_powder_pattern

    subroutine cw_powder_pattern_from_cfl(cfl_name,xc,yc)
        !> Computes a powder pattern from information provided by a
        !> CFL file.
        character(len=*),                         intent(in)    :: cfl_name !> name of the CFL fi;e
        real(kind=cp), dimension(:), allocatable, intent(out)   :: xc       !> two theta angle
        real(kind=cp), dimension(:), allocatable, intent(out)   :: yc       !> calculated intensity

        ! Local variables
        type(file_type) :: cfl_file
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: a
        type(powpatt_cw_conditions_type) :: ppc

        call clear_error()
        cfl_file = reading_file(cfl_name)
        if (err_cfml%ierr /= 0) return
        call cw_powder_pattern(cell,spg,a,ppc,xc,yc)

    end subroutine cw_powder_pattern_from_cfl

    subroutine cw_powder_pattern_profile(ppc,hkl,xc,yc,tth)

        ! Arguments
        type(powpatt_cw_conditions_type),         intent(in)  :: ppc
        type(reflist_type),                       intent(in)  :: hkl
        real(kind=cp), dimension(:), allocatable, intent(out) :: xc   !> two theta angle
        real(kind=cp), dimension(:), allocatable, intent(out) :: yc   !> calculated intensity
        real(kind=cp), dimension(:), optional,    intent(in)  :: tth  !> two theta axis provided by the user

        ! Local variables
        integer :: i,i1,i2,j,npts
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
                    ss = ppc%Lambda * ref(i)%S ! sin(theta)
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

    subroutine tof_powder_pattern(cell,spg,a,ppc,xc,yc,tof)
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
        maxnref = get_maxnumref(stlmax,cell%vol,mult=mult)
        call initialize_reflist(maxnref,hkl,"srefl")
        call h_uni(cell,spg,.true.,0.0,stlmax,"s",maxnref,hkl)
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

End Module nFP_Powder_Patterns