!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Ref_Calc

    implicit none

    Contains

    Module Subroutine Calc_Laue_XZ_Position_Ref(LaueDiff,R,shift)
        !> This subroutine calculates the position of the spot in the detector system (R%x,R%z)
        !> according to the information given in other components of R and in LaueDiff
        !> The optional shift vector is useful for calculating diffracted peaks by finite size
        !> samples: diffraction by a part of the sample (out of centre) give displaced (x,z)
        !> coordinates.
        !>
        !>    Created: October 2009 (JRC)
        !>    Updated:  August 2010 (JRC), July 2012 (JRC, shift and origin displacement added)

        ! Arguments
        type(Laue_Instrument_Type),          intent(in)     :: LaueDiff
        type(Laue_Ref_Type),                 intent(in out) :: R  !Laue reflection
        real(kind=cp), dimension(3),optional,intent(in)     :: shift
        
        ! Local variables
        real(kind=cp), dimension(3) :: xl,xd,u,v,ro,pos_det
        real(kind=cp), dimension(2) :: vd,rod
        real(kind=cp)               :: p,a,b,c,p1,p2,xc,zc
        logical                     :: ok

        R%x=0.0_cp
        R%z=0.0_cp
        if(R%Stats /= 0) return
        pos_det = LaueDiff%rOD
        if(present(shift)) pos_det = pos_det + shift

        ! First calculate the position (x,z) w.r.t. ideal centre of detector
        Select Case(LaueDiff%dtype)
            Case("Rec")
                u  = (/sind(R%gamma)*cosd(R%nu), cosd(R%gamma)*cosd(R%nu), sind(R%nu)/)
                p  = dot_product(LaueDiff%RD(:,2),pos_det/dot_product(LaueDiff%RD(:,2),u))
                xl = p*u
                xd = matmul(transpose(LaueDiff%RD),xl-pos_det)
                xc = xd(1)
                zc = xd(3)
            Case("Cyl")
                if(LaueDiff%tilted .or. LaueDiff%displaced .or. present(shift) ) then   !Tilted/Displaced cylindrical detector
                    u   = (/sind(R%gamma)*cosd(R%nu), cosd(R%gamma)*cosd(R%nu), sind(R%nu)/)
                    v   = matmul(LaueDiff%RD,u)
                    vd  = (/v(1),v(2)/)
                    ro  = matmul(LaueDiff%RD,pos_det)
                    rod =(/ro(1),ro(2)/)
                    a   = dot_product(vd,vd)
                    b   = -2.0_cp*dot_product(vd,rod)
                    c   = dot_product(rod,rod)-LaueDiff%D*LaueDiff%D
                    call Quadratic_Eqn_Solver(a,b,c,p1,p2,ok)
                    if (ok) then !Select positive p
                        p  = max(p1,p2)
                        zc = p*v(3)-ro(3)
                        xc = LaueDiff%D*atan2(p*v(1)-ro(1),p*v(2)-ro(2))
                    end if
                else                 !Ideal cylindrical detector
                    zc = LaueDiff%D * tand(R%nu)
                    xc = R%gamma*to_rad *  LaueDiff%D
                end if
        End Select
        !Apply the shift in mm corresponding to the difference of the origin in pixels w.r.t. (Np_h/2,Np_v/2)
        R%x=xc-LaueDiff%dism(1)
        R%z=zc-LaueDiff%dism(2)

    End Subroutine Calc_Laue_XZ_Position_Ref

    Module Subroutine Calc_Laue_Spot_XZ_Angles(LaueDiff,R,RL,opt,dk_min)
        !> This subroutine calculates angular components of the object R of type Laue_Ref_Type
        !> according to the information given in other components of R, the Laue limits contained
        !> in LaueDiff and the matrix RL transforming hkl-indices (contained as components of R) to
        !> the laboratory system. If the optional argument "opt" is provided the subroutine
        !> calculates only the spherical angles of the reflection without taking into account
        !> the status of the reflection. The coordinates x-z in the detector system are also calculated.
        !> The subroutine calculates also the status of the reflection contained in the
        !> component R%stats
        !>
        !>    Created: October 2009 (JRC)
        !>    Updated: January 2010 (optional argument opt, JRC)
        !>    Updated: July 2010    (taking into account negative and positive gammas, JRC)

        ! Arguments
        type(Laue_Instrument_Type),   intent(in)    :: LaueDiff
        type(Laue_Ref_Type),          intent(inout) :: R        ! Laue reflection
        real(kind=cp), dimension(3,3),intent(in)    :: RL       ! Matrix trasforming hkl to laboratory cartesian system
                                                                ! Normally RL = Rx Ry Rz UB
        integer,       optional,      intent(in)    :: opt
        real(kind=cp), optional,      intent(in)    :: dk_min   ! Resolution for satellite reflections

        ! Local variables
        real :: sin_nu ! Local variable introduced by Nebil on 8th April 2026. See below.

        R%lambda = 0.0_cp
        R%gamma  = 0.0_cp
        R%nu     = 0.0_cp
        R%stats  = 0
        R%mult   = 0
        R%nodal  = 0

        R%zv=matmul(RL,R%h) !Cartesian coordinates of h in the Lab system
        !if(present(opt)) then
        !    R%ds=1.0_cp/sqrt(dot_product(R%zv,R%zv))
        !    call Calc_Spherical_angles_from_zv(R)
        !    return
        !end if
        if( R%zv(2) >= 0.0_cp) then
            R%stats = -3 !Oposite side of Ewald spheres
            return
        else
            !  Determine the wavelength of the reflection  Lambda=-2y d^2, sinTheta=-yd
            !  sin Nu= Lambda*z, tanGamma= Lamda*x/(1+lambda*y)
            R%ds=1.0_cp/sqrt(dot_product(R%zv,R%zv))
            if(R%ds < LaueDiff%d_min) then
                R%stats=-2 !outside given resolution
                return
            end if
            if(present(dk_min)) then
                if(R%ds < dk_min) then
                    R%stats=-2 !outside given resolution for k-vector satellites
                    return
                end if
            end if
            R%lambda = -2.0_cp*R%zv(2)*R%ds*R%ds
            if (R%lambda < LaueDiff%L_min .or. R%lambda > LaueDiff%L_max) then
                R%stats=-1 !outside Ewald spheres
                return
            else !Calculate theta, gamma and nu
                ! -----------------------------------------------
                ! This block has been introduced because gfortran
                ! fails if absolute value of sin_nu is slightly 
                ! greater than one. Nebil, 08-April_2026
                sin_nu = R%lambda*R%zv(3)
                if (sin_nu < -1.0) then
                    sin_nu = -1.0
                else if (sin_nu > 1.0) then
                    sin_nu = 1.0
                end if
                R%nu = asind(sin_nu)
                ! ------------------------------------------------
                if(R%nu < LaueDiff%nu_min .or. R%nu > LaueDiff%nu_max) then
                    R%stats = 1   ! Beyond Nu-limit
                    return
                end if
                R%gamma=atan2d( R%lambda*R%zv(1), 1+R%lambda*R%zv(2))
                if(R%gamma >= 0.0_cp) then
                    if(R%gamma < LaueDiff%gap_min .or. R%gamma > LaueDiff%gap_max) then
                        R%stats = 2  ! Beyond Gamma-limit
                        return
                    end if
                else
                    if (R%gamma < LaueDiff%gan_min .or. R%gamma > LaueDiff%gan_max) then
                        R%stats = 2  ! Beyond Gamma-limit
                        return
                    end if
                end if
                call Calc_Laue_XZ_Position(LaueDiff,R)
                if(R%x < LaueDiff%x_min .or. R%x > LaueDiff%x_max .or. R%z < LaueDiff%z_min .or. R%z > LaueDiff%z_max) then
                    R%stats = 3  ! Outside the detector
                    return
                end if
                R%ttheta = 2.0_cp * asind(-R%zv(2)*R%ds) !the multiplication by 2 was lacking before July 2011!!
            end if
        end if

    End Subroutine Calc_Laue_Spot_XZ_Angles

    Module Subroutine Calc_Visible_Reflections_List(LDiff,M,hkl,Ref_List)

        ! Arguments
        type(Laue_Instrument_Type),    intent(in)    :: LDiff
        real(kind=cp), dimension(3,3), intent(in)    :: M        ! Matrix transforming a reflection hkl
                                                                 ! to the Cartesian in L-system to be visible
        type(RefList_Type),            intent(in)    :: hkl      ! List of all accesible reflections computed
                                                                 ! by function Generate_Laue_Reflections
        type(Laue_Ref_List_Type),      intent(inout) :: Ref_List ! It must be allocated before calling this subroutine

        ! Local variables
        integer :: n, i
        type(Laue_Ref_Type) :: Ref

        n=0
        call Init_Laue_Ref(Ref)
        do i = 1 , hkl%nref
            Ref%h = hkl%Ref(i)%h
            call Calc_Laue_Spot_XZ_Angles(LDiff,Ref,M)
            if(Ref%stats == 0) then
                n = n + 1
                !if(Cheb_Ref .or. Apply_Offsets) then  !Correct for distortions
                !    pos=[Ref%x , Ref%z]
                !    call Corr_MOSFLM(LDiff%D,Off_set%distort,pos)
                !    !Only the dimensions of the detector are used in Corr_xz_Chebychev
                !    call Corr_xz_Chebychev(pos,LDiff,Ncoeff,Cheb,shift)
                !    Ref%x=pos(1)+shift(1)
                !    Ref%z=pos(2)+shift(2)
                !end if
                if (n > size(Ref_List%LR)) then
                    err_cfml%ierr = 1
                    err_cfml%flag = .true.
                    err_cfml%msg = 'Calc_Visible_Reflections_List: number of calculated reflections > size of reflection list'
                    return
                end if
                Ref_List%LR(n) = Ref
            end if
        end do
        Ref_List%nref=n

    End Subroutine Calc_Visible_Reflections_List

    Module Subroutine Quadratic_Eqn_Solver(a,b,c,x1,x2,ok)

        ! Arguments
        real(kind=cp), intent(in)  :: a,b,c
        real(kind=cp), intent(out) :: x1,x2
        logical,       intent(out) :: ok

        ! Local variables
        real(kind=cp)            :: delta, sqd
        real(kind=cp), parameter :: eps=1.0e-35_cp

        x1=0.0_cp; x2=0.0_cp; ok=.false.
        if(abs(a) <= eps) then       ! Linear equation in practice
            if(abs(b) <= eps) return ! no solution
            x1=-c/b
            x2=x1
            ok=.true.
            return
        end if
        delta= b*b-4.0_cp*a*c   !discriminant
        if(delta < 0.0_cp) then ! no real solution
            return
        else if( abs(delta) <= eps) then ! a single solution
            x1= -0.5_cp*b/a
            x2=x1
        else !positive delta
            sqd = sqrt(delta)
            x1=0.5_cp*(-b+sqd)/a
            x2= 0.5_cp*(-b-sqd)/a
        end if
        ok = .true.

    End Subroutine Quadratic_Eqn_Solver

End Submodule Laue_Ref_Calc