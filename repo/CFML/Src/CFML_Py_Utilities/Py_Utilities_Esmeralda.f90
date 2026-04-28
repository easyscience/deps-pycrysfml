submodule (CFML_Py_Utilities) Py_Utilities_Esmeralda

implicit none

contains

    module function calculate_laue_image() result(laue_img)
        !> Calculate visible reflections

        ! Result
        type(esmeralda_laue_image_type) :: laue_img

        ! Local variables
        integer :: n
        type(laue_ref_list_type) :: laue_hkl !> List of visible reflections

        call clear_error()
        laue_img%nref = 0
        ! Check instrument is configured
        if (.not. is_instrument_es_configured) then
            err_cfml%ierr = 1
            err_cfml%msg = 'Py_Utilities_Esmeralda.calculate_laue_image: '&
                //'Laue instrument must be configured before calculating a Laue image.'
            return
        end if
        ! Check an UB-matrix was given
        if (.not. is_ub_es_configured) then
            err_cfml%ierr = 1
            err_cfml%msg = 'Py_Utilities_Esmeralda.calculate_laue_image: '&
                //'UB-matrix must be set before calculating a Laue image.'
            return
        end if
        ! If no space group was given, assume P1
        if (.not. is_spg_es_configured) then
            call set_spg_esmeralda('SPGR P1')
            if (err_cfml%ierr /= 0) return
        end if
        ! Generate accessible reflections
        call generate_laue_reflections(instrument_es,cell_es,spg_es,hkl_es,n)
        call allocate_laue_ref_list(laue_hkl,n)
        ! Compute image
        call calc_visible_reflections_list(instrument_es,ub_es,hkl_es,laue_hkl)
        if (err_cfml%ierr /= 0) call clear_error() ! Number of computed reflections > size of laue_hkl
        if (laue_hkl%nref > 0) call laue_image_from_reflection_list(laue_hkl,instrument_es,laue_img)

    end function calculate_laue_image

    module function calculate_laue_zone(x1,z1,x2,z2) result(zone)
        !> Calculate the zone that contains two points on the detector

        ! Arguments
        real, intent(in) :: x1  !> x coordinate of the first  point in pixels
        real, intent(in) :: z1  !> z coordinate of the first  point in pixels
        real, intent(in) :: x2  !> x coordinate of the second point in pixels
        real, intent(in) :: z2  !> z coordinate of the second point in pixels

        ! Results
        real, dimension(:,:), allocatable :: zone !> x,z points sampling of the zone

        ! Local variables
        real :: ga_1,ga_2,nu_1,nu_2,x_mm,z_mm
        character(len=:), allocatable :: geom

        call clear_error()
        ! Check instrument is configured
        if (.not. is_instrument_es_configured) then
            err_cfml%ierr = 1
            err_cfml%msg = 'Py_Utilities_Esmeralda.calculate_laue_zone: '&
                //'Laue instrument must be configured before calculating a Laue zone.'
            return
        end if
        ! Compute gamma and nu angles for each point
        geom = l_case(instrument_es%dtype)
        select case (geom)
            case ("cyl")
                x_mm = (x1 - instrument_es%xo) * instrument_es%h / instrument_es%np_h
                z_mm = (z1 - instrument_es%zo) * instrument_es%v / instrument_es%np_v
                ga_1 = x_mm / instrument_es%d
                nu_1 = atan(z_mm / instrument_es%d)
                x_mm = (x2 - instrument_es%xo) * instrument_es%h / instrument_es%np_h
                z_mm = (z2 - instrument_es%zo) * instrument_es%v / instrument_es%np_v
                ga_2 = x_mm / instrument_es%d
                nu_2 = atan(z_mm / instrument_es%d)
            case ("rec")
            case default
                err_cfml%ierr = 1
                err_cfml%msg = "Py_Utilities_Esmeralda.laue_image_from_reflection_list: detector geometry unknown."
                return
        end select

    end function calculate_laue_zone

    subroutine laue_image_from_reflection_list(hkl,inst,img)

        ! Arguments
        type(laue_ref_list_type),        intent(in)  :: hkl
        type(laue_instrument_type),      intent(in)  :: inst
        type(esmeralda_laue_image_type), intent(out) :: img

        ! Local variables
        integer :: i
        character(len=:), allocatable :: geom

        if (allocated(img%xz)) deallocate(img%xz)
        if (allocated(img%xz_stereo)) deallocate(img%xz_stereo)
        img%nref = hkl%nref
        if (img%nref == 0) return
        allocate(img%xz(2,hkl%nref),img%xz_stereo(2,hkl%nref),img%hkl(3,hkl%nref))
        geom = l_case(inst%dtype)
        select case (geom)
            case ("cyl")
                do i = 1 , img%nref
                    img%xz(1,i)  = (inst%xo + inst%np_h * hkl%lr(i)%x / inst%h)
                    img%xz(2,i)  = (inst%zo + inst%np_v * hkl%lr(i)%z / inst%v)
                    img%hkl(:,i) = hkl%lr(i)%h(:)
                    call get_stereographic_projection_from_zv(hkl%lr(i),&
                        img%xz_stereo(1,i),img%xz_stereo(2,i))
                end do
            case ("rec")
            case default
                err_cfml%ierr = 1
                err_cfml%msg = "Py_Utilities_Esmeralda.laue_image_from_reflection_list: detector geometry unknown."
                return
        end select

    end subroutine laue_image_from_reflection_list

    module subroutine set_instrument_esmeralda(name,dtype,d,h,v,np_h,np_v,xo,zo,ga_d,nu_d,l_min,l_max,&
        x_min,x_max,z_min,z_max,gan_min,gan_max,gap_min,gap_max,nu_min,nu_max)
        !> Set the Laue instrument for Esmeralda

        ! Arguments
        character(len=*), intent(in) :: name    !> Instrument name
        character(len=*), intent(in) :: dtype   !> Detector type: 'Rec' | 'Cyl'
        real(kind=4),     intent(in) :: d       !> Sample-detector distance (mm)
        real(kind=4),     intent(in) :: h       !> Horizontal detector size (mm)
        real(kind=4),     intent(in) :: v       !> Vertical   detector size (mm)
        integer,          intent(in) :: np_h    !> Number of horizontal pixels
        integer,          intent(in) :: np_v    !> Number of vertical   pixels
        real(kind=4),     intent(in) :: xo      !> Coordinate x in pixels for detector origin
        real(kind=4),     intent(in) :: zo      !> Coordinate z in pixels for detector origin
        real(kind=4),     intent(in) :: ga_d    !> Angle gamma in degrees for detector origin
        real(kind=4),     intent(in) :: nu_d    !> Angle nu    in degrees for detector origin
        real(kind=4),     intent(in) :: l_min   !> Lambda min
        real(kind=4),     intent(in) :: l_max   !> Lambda max
        real(kind=4),     intent(in) :: x_min   !> x min (mm)
        real(kind=4),     intent(in) :: x_max   !> x max (mm)
        real(kind=4),     intent(in) :: z_min   !> z min (mm)
        real(kind=4),     intent(in) :: z_max   !> z max (mm)
        real(kind=4),     intent(in) :: gan_min !> Negative gamma min (degrees)
        real(kind=4),     intent(in) :: gan_max !> Negative gamma max (degrees)
        real(kind=4),     intent(in) :: gap_min !> Positive gamma min (degrees)
        real(kind=4),     intent(in) :: gap_max !> Positive gamma max (degrees)
        real(kind=4),     intent(in) :: nu_min  !> Nu min (degrees)
        real(kind=4),     intent(in) :: nu_max  !> Nu max (degrees)

        instrument_es%name    = trim(name)
        instrument_es%dtype   = trim(dtype)
        instrument_es%d       = d
        instrument_es%h       = h
        instrument_es%v       = v
        instrument_es%np_h    = np_h
        instrument_es%np_v    = np_v
        instrument_es%xo      = xo
        instrument_es%zo      = zo
        instrument_es%ga_d    = ga_d
        instrument_es%nu_d    = nu_d
        instrument_es%l_min   = l_min
        instrument_es%l_max   = l_max
        instrument_es%d_min   = l_min / 2.
        instrument_es%x_min   = x_min
        instrument_es%x_max   = x_max
        instrument_es%z_min   = z_min
        instrument_es%z_max   = z_max
        instrument_es%gan_min = gan_min
        instrument_es%gan_max = gan_max
        instrument_es%gap_min = gap_min
        instrument_es%gap_max = gap_max
        instrument_es%nu_min  = nu_min
        instrument_es%nu_max  = nu_max
        is_instrument_es_configured = .true.

    end subroutine set_instrument_esmeralda

    module subroutine set_spg_esmeralda(spg_id)
        !> Set the space group for Esmeralda.

        ! Arguments
        character(len=*), intent(in) :: spg_id !> string specifying the space group to be built

        ! Local variables
        type(File_Type) :: cfl

        ! Put spg_id in cfl
        cfl%nlines = 1
        allocate(cfl%line(1))
        cfl%line(1)%str = trim(spg_id)

        ! Build spg_esmeralda
        if (allocated(spg_es)) deallocate(spg_es)
        call clear_error()
        call read_cfl_spg(cfl,spg_es)
        if (err_cfml%ierr /= 0) then 
            is_spg_es_configured = .true.
        else
            is_spg_es_configured = .false.
        end if

    end subroutine set_spg_esmeralda

    module subroutine set_ub_esmeralda(ub)
        !> Set the UB matrix for Esmeralda

        ! Arguments
        real(kind=4), dimension(3,3), intent(in) :: ub

        ! Local variables
        real(kind=cp), dimension(6) :: dcel
        real(kind=cp), dimension(3,3) :: ub_cfml

        call clear_error()
        ub_cfml = ub
        call cell_fr_ub(ub_cfml,dcel=dcel)
        if (err_cfml%ierr /= 0) return
        call set_crystal_cell(dcel(1:3),dcel(4:6),cell_es)
        if (err_cfml%ierr /= 0) return
        ub_es = ub
        is_ub_es_configured = .true.

    end subroutine set_ub_esmeralda

end submodule Py_Utilities_Esmeralda