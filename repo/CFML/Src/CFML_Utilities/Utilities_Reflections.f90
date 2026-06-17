submodule (CFML_Utilities) Utilities_Reflections

implicit none

contains

    subroutine allocate_reflection_list()
        !> Allocates the list of reflections for each phase in Ph.
        !> The number of reflections is determined from the maximum sin(theta)/lambda value of all the patterns.

        if(allocated(RL)) deallocate(RL)
        call check_phases_allocation()
        if (err_cfml%ierr /= 0) return
        allocate(RL(size(Ph)))

    end subroutine allocate_reflection_list

    module subroutine generate_reflections_for_patterns()
        !> Generates the list of reflections for each phase in Ph.

        ! Local variables
        integer :: i,j,k,n,n_patt,n_ref
        real(kind=cp) :: sintlmax,dsp
        real(kind=cp), dimension(:), allocatable :: wdt
        logical :: twowaves

        ! Allocate array RL defined in CFML_Powder
        call allocate_reflection_list()
        if (err_cfml%ierr /= 0) return

        ! Determine the maximum sin(theta)/lambda value for all the patterns
        sintlmax = sintl_max()
        if (err_cfml%ierr /= 0) return

        ! Generate the list of reflections for each phase in Ph
        call generate_reflections_for_phases(sintlmax)
        if (err_cfml%ierr /= 0) return

        !Determine now the contributions to the patterns
        allocate(wdt(size(Pat)))
        wdt = 5.0_cp
        do i = 1 , size(Pat)
            select type (tw => Pat(i)%cond)
                class is (PowPatt_CW_Conditions_type)
                    wdt(i) = max(5.0,tw%wdt)
                class is (PowPatt_TOF_Conditions_type)
                    wdt(i) = max(5.0,tw%wdt)
            end select
        end do

        do i = 1 , size(Ph)
            n_patt = Ph(i)%Ncontr
            n_ref = RL(i)%Nref
            RL(i)%n_patt = n_patt
            RL(i)%i_ph = i
            do j=1,Ph(i)%Ncontr  !Check if there is a pattern with 2 wavelengths (in such a case the number of reflection is duplicated)
                n = Ph(i)%patterns(j) !number of the pattern to which the phase contributes
                select type (tw => Pat(n)%cond)
                    class is (PowPatt_CW_Conditions_type)
                        if(tw%twowaves) then
                        n_ref = n_ref * 2 !duplicate the number of reflections
                        twowaves = .true.
                        exit
                        end if
                end select
            end do

            if(allocated(RL(i)%ptr))     deallocate(RL(i)%ptr)
            if(allocated(RL(i)%pos))     deallocate(RL(i)%pos)
            if(allocated(RL(i)%HG))      deallocate(RL(i)%HG)
            if(allocated(RL(i)%HL))      deallocate(RL(i)%HL)
            if(allocated(RL(i)%FWHM))    deallocate(RL(i)%FWHM)
            if(allocated(RL(i)%ETA))     deallocate(RL(i)%ETA)
            if(allocated(RL(i)%patts))   deallocate(RL(i)%patts)
            if(allocated(RL(i)%corr))    deallocate(RL(i)%corr)
            allocate(RL(i)%ptr(2,n_ref,n_patt),RL(i)%pos(n_ref,n_patt),RL(i)%HG(n_ref,n_patt),&
                     RL(i)%HL(n_ref,n_patt),RL(i)%corr(n_ref,n_patt))
            allocate(RL(i)%FWHM(n_ref,n_patt),RL(i)%ETA(n_ref,n_patt),RL(i)%patts(n_patt))
            RL(i)%ptr   = 0
            RL(i)%pos   = 0.0_cp
            RL(i)%HG    = 0.0_cp
            RL(i)%HL    = 0.0_cp
            RL(i)%FWHM  = 0.0_cp
            RL(i)%ETA   = 0.0_cp
            RL(i)%patts = Ph(i)%patterns

            call init_prof_val()  !This initializes the table for Gauss-Legendre integration

            do j = 1, Ph(i)%Ncontr
                n = Ph(i)%patterns(j) !number of the pattern to which the phase contributes
                if (Pat(n)%sample /= "P") cycle
                if(Pat(n)%mode == "CW") then
                    call reflections_contribution_cw_ini(i,j,RL(i),wdt(n))
                else if(Pat(n)%mode == "TF") then
                    do k = 1 , RL(i)%Nref  !Calculate the position of the reflection for each pattern and the contributing points
                        dsp = 2.0_cp / RL(i)%Ref(k)%s
                        RL(i)%pos(k,j) = dsp * Pat(n)%Pdat%wave(1)
                    end do
                end if
            end do
        end do

    end subroutine generate_reflections_for_patterns

    subroutine generate_reflections_for_phases(sintlmax)
        !> Generates the list of reflections for each phase in Ph.
        !> The number of reflections is determined from the maximum sin(theta)/lambda value of all the patterns.

        ! Local variables
        integer :: i, maxnumref
        real(kind=cp) :: sintlmax

        do i = 1 , size(Ph)
            select type(spg => Ph(i)%SpG)
                type is (SPG_type)
                    if(Ph(i)%mag) then
                        if(Ph(i)%mag_only) then
                            call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,&
                                Unique=.true.,mag_only=.true.,Friedel=.true.,Ref_typ='MRefl')
                        else
                            call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,&
                                Unique=.true.,Friedel=.true.,Ref_typ='MRefl')
                        end if
                    else
                        if(spg%NumSpg < 230 .and. spg%NumSpg > 0) then
                            maxnumref = get_maxnumref(sintlmax, Ph(i)%cell%Vol, 0.0_cp, multip=spg%Multip)
                            call H_Uni(Ph(i)%cell, spg, .true., 0.0, sintlmax, "s", maxnumref, RL(i),Ref_typ='SRefl')
                        else
                            call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,&
                                Unique=.true.,Friedel=.true.,Ref_typ='SRefl')
                        end if
                   end if
                type is (SuperSpaceGroup_type)
                    if(Ph(i)%mag) then
                        if(Ph(i)%mag_only) then
                            call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,&
                                Unique=.true.,mag_only=.true.,Friedel=.true.,Ref_typ='MRefl',kout=Ph(i)%kvec)
                        else
                            call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,&
                                Unique=.true.,Friedel=.true.,Ref_typ='MRefl',kout=Ph(i)%kvec)
                        end if
                    else
                        call gener_reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,&
                            Unique=.true.,Friedel=.true.,Ref_typ='SRefl',kout=Ph(i)%kvec)
                    end if
            end select
        end do

    end subroutine generate_reflections_for_phases

    subroutine get_hg_hl_cw(nph,npat,nrf,iwav,HG,HL)
        ! Arguments
        integer,       intent(in) :: npat,nph,nrf,iwav
        real(kind=cp), intent(out):: HG,HL

        ! Local variables
        integer       :: j
        real(kind=cp) :: sinth,costh,tanth,costh2,lambda,frac_gsz, frac_lstr,isosz,isostr, &
                         u,v,w,x,y,z,dst2,dsiz,HG2,isosize_broad, isostr_broad,sq
        real(kind=cp), dimension(3)  :: hkl
        real(kind=cp), dimension(15) :: par

        j             = Pat(npat)%irf
        u             = 0.0_cp
        v             = 0.0_cp
        w             = 0.0_cp
        x             = 0.0_cp
        y             = 0.0_cp
        z             = 0.0_cp
        lambda        = Pat(npat)%Pdat%wave(iwav)
        sinth         = RL(nph)%Ref(nrf)%s*lambda
        sq            = 4.0_cp*(RL(nph)%Ref(nrf)%s)**2 ! 1/d^2
        hkl           = RL(nph)%Ref(nrf)%hr
        costh         = sqrt(1.0_cp-sinth*sinth)
        costh2        = costh*costh
        tanth         = sinth/costh
        isosz         = 0.0
        dsiz          = 0.0
        isosize_broad = 0.0
        par           = 0.0
        isostr        = 0.0
        dst2          = 0.0
        frac_gsz      = 0.0
        frac_lstr     = 0.0
        isosz         = Ph(nph)%pow(npat)%iso_size
        isostr_broad  = Ph(nph)%pow(npat)%iso_strain
        if(isosz > 1.0) then
            frac_gsz      = Ph(nph)%pow(npat)%Gauss_iso_size_frac
            isosize_broad = 180.0_cp*lambda/isosz/PI !division by costh below
        end if
        if(isostr > 0.0) then ! isostr is always zero in this subroutine. Ask Juan
            frac_lstr = Ph(nph)%pow(npat)%Lorentz_iso_strain_frac
        end if
        !call Calc_Anisotropic_Strain("QUARTIC_FORM","P6/mmm","CW",par,hkl,sq,dst2)
        select type( cd => Pat(npat)%cond)
            class is (PowPatt_CW_Conditions_type)
                u = cd%u
                v = cd%v
                w = cd%w
                x = cd%x
                y = cd%y
        end select
        !if(j > 0 ) Then
        !    u = u + cw_irf(j)%u_i(iwav)
        !    v = v + cw_irf(j)%v_i(iwav)
        !    w = w + cw_irf(j)%w_i(iwav)
        !    x = x + cw_irf(j)%x_i(iwav)
        !    y = y + cw_irf(j)%y_i(iwav)
        !    z = z + cw_irf(j)%z_i(iwav)
        !end if
        HG2 = ((u + (dst2 + isostr_broad) * (1.0_cp - frac_lstr)**2)  * tanth + v) * tanth + &
              w + (frac_gsz * (isosize_broad + dsiz) / costh)**2
        HG  = sqrt(HG2)
        HL  = (x + sqrt(dst2) * frac_lstr) * tanth + (y + (dsiz + isosize_broad) * (1.0_cp - frac_gsz)) / costh
    end subroutine get_hg_hl_cw

    subroutine reflections_contribution_cw_ini(iph,ipat,RL,wdt)

        ! Arguments
        integer,                     intent(in)     :: iph,ipat
        type(RefP_type),             intent(in out) :: RL !It has been partially filled before
        real(kind=cp),               intent(in)     :: wdt

        ! Local variables
        integer :: k,kn,NRef,npts,n_pat
        real(kind=cp)               :: pt, sint, cost, HG, HL, fwhm, eta, plor1, &
                                        pos_left, pos_right, plor, pref_corr, ratio
        real(kind=cp), dimension(2) :: lambda
        logical :: twowaves

        Nref  = RL%Nref
        n_pat = Ph(iph)%patterns(ipat)
        npts  = Pat(n_pat)%Pdat%npts
        select type(cond => Pat(n_pat)%cond)
            type is(PowPatt_CW_Conditions_type)
                twowaves = cond%twowaves
                lambda   = cond%Lambda
                ratio    = cond%ratio
        end select

        associate(xp => Pat(n_pat)%Pdat%x)

            do k = 1 , Nref  !Calculate the position of the reflection for each pattern and the contributing points
                sint = RL%Ref(k)%s * Pat(n_pat)%PDat%wave(1)
                if(sint > 1.0_cp) cycle
                cost = sqrt(abs(1.0-sint*sint))
                pt = 2.0_cp * asind( sint )
                RL%pos(k,ipat) = pt
                call get_hg_hl_cw(iph,n_pat,k,1,HG,HL)
                ! Calculate the FWHM for each reflection and complete the type
                call get_fwhm_eta(HG,HL,fwhm,eta)
                pos_left = pt-fwhm*wdt
                pos_right = pt+fwhm*wdt
                RL%ptr(1,k,ipat) = max(1,locate(xp,pos_left,npts))
                RL%ptr(2,k,ipat) = min(npts,locate(xp,pos_right,npts))
                RL%HG(k,ipat) = HG
                RL%HL(k,ipat) = HL
                RL%FWHM(k,ipat) = fwhm
                RL%ETA(k,ipat) = eta
                !Calculation of CORR for the current reflection
                plor1 = Lorentz_abs_CW(sint,cost,pt,0.0,"NC","DBS","HEWAT")
                pref_corr = 1.0_cp
                !if(Ph(iph)%Pow(n_pat)%n_pref > 0) then
                !    call Preferred_orientation("MAX_MD",Ph(iph)%Pow(n_pat)%n_pref, &
                !            Ph(iph)%Pow(n_pat)%axes_pref, &
                !            Ph(iph)%Cell%gr,Rot_Mats(iph), &
                !            Ph(iph)%Pow(n_pat)%pref,RL%Ref(k),pref_corr)
                !end if
                RL%corr(k,ipat)=plor1*pref_corr
                if(twowaves) then !Store the second wavelength in the second half of the array
                    sint = RL%Ref(k)%s * lambda(2)
                    if(sint > 1.0_cp) cycle
                    cost = sqrt(abs(1.0-sint*sint))
                    kn = k+Nref
                    pt = 2.0_cp * asind( sint )
                    RL%pos(kn,ipat) = pt
                    call get_hg_hl_cw(iph,n_pat,kn,2,HG,HL)
                    pos_left = pt-fwhm*wdt
                    pos_right = pt+fwhm*wdt
                    RL%ptr(1,kn,ipat) = max(1,locate(xp,pos_left,npts))
                    RL%ptr(2,kn,ipat) = min(npts,locate(xp,pos_right,npts))
                    RL%HG(kn,ipat) = HG
                    RL%HL(kn,ipat) = HL
                    RL%FWHM(kn,ipat) = fwhm
                    RL%ETA(kn,ipat) = eta
                    !Calculation of CORR for the current reflection
                    plor = Lorentz_abs_CW(sint,cost,pt,0.0,"N","DBS","HEWAT")
                    pref_corr = 1.0_cp
                    !if(Ph(iph)%Pow(n_pat)%n_pref > 0) then
                    !    call Preferred_orientation("MAX_MD",Ph(iph)%Pow(n_pat)%n_pref, &
                    !                Ph(iph)%Pow(n_pat)%axes_pref, &
                    !                Ph(iph)%Cell%gr,Rot_Mats(iph), &
                    !                Ph(iph)%Pow(n_pat)%pref,RL%Ref(kn),pref_corr)
                    !end if
                    RL%corr(kn,ipat)=plor*pref_corr*ratio ! Apply ratio
                end if
            end do
        end associate

    end subroutine reflections_contribution_cw_ini

    function sintl_max() result(sintlmax)
        !> Determines sin(theta) / lambda min and max for all the patterns
        !> Array Pat defined in CFML_Powder must be allocated

        ! Return value
        real(kind=cp) :: sintlmax

        ! Local variables
        integer       :: i
        real(kind=cp) :: xm,sintl

        call check_patterns_allocation()
        if (err_cfml%ierr /= 0) return
        sintlmax=0.0
        do i = 1 , size(Pat)
            xm = Pat(i)%PDat%xmax*1.05
            select case(Pat(i)%mode)
                case("CW") ! Constant wavelength
                    sintl = sind(xm*0.5_cp)/Pat(i)%PDat%wave(1)
                case("TF") ! Time of flight
                    sintl = 1.0 / (2.0_cp*xm*Pat(i)%PDat%wave(1))
            end select
            if(sintl > sintlmax) sintlmax = sintl
        end do

    end function sintl_max

end submodule Utilities_Reflections