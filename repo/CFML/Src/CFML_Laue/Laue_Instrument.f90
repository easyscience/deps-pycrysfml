!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Instrument

    implicit none

    Contains

    Module Subroutine Read_Laue_Instrm(filenam,LaueDiff,ExclR)

        ! Arguments
        character(len=*),                    intent(in)  :: filenam
        type(Laue_Instrument_Type),          intent(out) :: LaueDiff
        type(Excluded_Regions_Type),optional,intent(out) :: ExclR

        ! Local variables
        character(len=120)          :: line
        character(len=12)           :: keyt
        integer                     :: i,j,k,n,n_,ier
        logical                     :: read_orig
        real(kind=cp), dimension(2) :: dispt
        real(kind=cp)               :: xm,zm
        logical                     :: maxij
        type(file_type)             :: inst_file

        call clear_error()
        inst_file = Reading_File(filenam)
        if (err_cfml%ierr /= 0) then
            err_cfml%msg = "Read_Laue_Instrm -> Error opening the file: "//trim(filenam)
            return
        end if
        if(present(ExclR)) then
            maxij=.false.
            ExclR%imax=0; ExclR%jmax=0
            ExclR%inv_circ=0; ExclR%inv_rect=0
            ExclR%Nexc_Rect=0
            ExclR%Nexc_Circ=0
        end if

        call Init_Laue_Instrm(LaueDiff)   !Set the characteristics of the instrument similar to those of VIVALDI
        read_orig=.false.
        do n = 1 , inst_file%nLines
            line = adjustl(inst_file%line(n)%str)
            if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
            i=index(line," ")
            keyt=u_case(line(1:i-1))
            select case(keyt)
                case("END")
                    exit
                case("INFO")
                    LaueDiff%info= adjustl(line(i+1:))
                case("NAME")
                    LaueDiff%name= adjustl(line(i+1:))
                case("DET_TYPE")
                    LaueDiff%dtype = adjustl(line(i+1:))
                case("DIST_DET")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%D
                    if(ier /= 0) then
                    err_cfml%ierr = 1
                    err_cfml%flag = .true.
                    err_cfml%msg="Error in file: "//trim(filenam)//", reading the distance sample-detector"
                    return
                    end if
                case("DIM_XZP")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%H,    LaueDiff%V, &
                        LaueDiff%np_h, LaueDiff%np_v
                    if(ier /= 0) then
                    err_cfml%ierr = 1
                    err_cfml%flag = .true.
                    err_cfml%msg="Error in file: "//trim(filenam)//", reading the dimension and pixels of the detector"
                    return
                    end if
                    if(Present(ExclR)) then
                        ExclR%imax = LaueDiff%np_v
                        ExclR%jmax = LaueDiff%np_h
                        maxij=.true.
                    end if
                case("XZ_ORIGIN")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%xo, LaueDiff%zo
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading Xo and Zo of detector centre"
                        return
                    end if
                    read_orig=.true.
                case("GN_CENTRE")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%ga_d, LaueDiff%nu_d
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading Gamma and Nu of detector centre"
                        return
                    end if
                    if (abs(LaueDiff%ga_d) > 0.0001 .or. abs(LaueDiff%nu_d) > 0.0001) LaueDiff%displaced=.true.
                case("DISP_CENTRE")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%rOD
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading displacement of detector centre"
                        return
                    end if
                    if (abs(LaueDiff%rOD(1)) > 0.0001 .or. abs(LaueDiff%rOD(2)) > 0.0001  &
                        .or. abs(LaueDiff%rOD(3)) > 0.0001 ) LaueDiff%displaced=.true.
                case("ROT_ORDER")
                    LaueDiff%r_ord=adjustl(line(i+1:))
                case("TILT_ANGLES")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%Tiltx_d, LaueDiff%Tilty_d, LaueDiff%Tiltz_d
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the Tilt Angles"
                        return
                    end if
                    if (abs(LaueDiff%Tiltx_d) > 0.0001 .or. abs(LaueDiff%Tilty_d) > 0.0001  &
                        .or. abs(LaueDiff%Tiltz_d) > 0.0001 ) LaueDiff%Tilted=.true.
                    ! Limits
                case("RESOL")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%d_min
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the resolution d_min value"
                        return
                    end if
                case("REVERT_PIX")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%invert
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading if the intensities should be inverted or not"
                        return
                    end if
                case("WAV_LIMITS") !First attempt to read central lambda
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%L_min, LaueDiff%L_max, LaueDiff%L_Central
                    if(ier /= 0) then
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%L_min, LaueDiff%L_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the wavelength limits"
                        return
                    else
                        LaueDiff%L_Central=0.5*(LaueDiff%L_min+LaueDiff%L_max)
                    end if
                    end if
                case("GAM+_LIMITS")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%gap_min, LaueDiff%gap_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the Gamma(positive) limits"
                        return
                    end if
                case("GAM-_LIMITS")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%gan_min, LaueDiff%gan_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the Gamma(negative) limits"
                        return
                    end if
                case("NU_LIMITS")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%nu_min, LaueDiff%nu_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the Nu limits"
                        return
                    end if
                case("X_LIMITS")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%x_min, LaueDiff%x_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the X limits"
                        return
                    end if
                case("Z_LIMITS")
                    read(unit=line(i+1:),fmt=*,iostat=ier) LaueDiff%z_min, LaueDiff%z_max
                    if(ier /= 0) then
                        err_cfml%ierr = 1
                        err_cfml%flag = .true.
                        err_cfml%msg="Error in file: "//trim(filenam)//", reading the Z limits"
                        return
                    end if
                case("FLIP")
                    if(index(line,"H") /= 0)  LaueDiff%flip_hor=.true.
                    if(index(line,"V") /= 0)  LaueDiff%flip_ver=.true.
                case("EXCL_CIRC")
                    if(present(ExclR)) then
                    j=index(line," ")
                    read(unit=line(j:),fmt=*,iostat=ier)   ExclR%Nexc_Circ, ExclR%inv_circ
                    if(ier /= 0) then
                        read(unit=line(j:),fmt=*,iostat=ier)   ExclR%Nexc_Circ
                        if(ier /= 0) then
                            write(unit=*,fmt="(a)") &
                                " => Warning! Error reading number of circular excluded regions in file: "//trim(filenam)
                            cycle
                        end if
                    end if
                    if(allocated(ExclR%Exc_Circ)) deallocate(ExclR%Exc_Circ)
                    allocate(ExclR%Exc_Circ(3,ExclR%Nexc_Circ))
                    do k=1, ExclR%Nexc_Circ
                        n_ = n + k
                        line = adjustl(inst_file%line(n_)%str)
                        read(unit=line,fmt=*,iostat=ier) ExclR%Exc_Circ(:,k)
                        if(ier /= 0) then
                            write(unit=*,fmt="(a,i3)") " => Error reading circular excluded region: ",k
                        end if
                    end do
                    end if
                case("EXCL_RECT")
                    if(present(ExclR)) then
                        j=index(line," ")
                        read(unit=line(j:),fmt=*,iostat=ier)   ExclR%Nexc_Rect, ExclR%inv_rect
                        if(ier /= 0) then
                            read(unit=line(j:),fmt=*,iostat=ier)   ExclR%Nexc_Rect
                            if(ier /= 0) then
                                write(unit=*,fmt="(a)") &
                                    " => Warning! Error reading number of rectangular excluded regions in file: "//trim(filenam)
                                cycle
                            end if
                        end if
                        if(allocated(ExclR%Exc_Rect)) deallocate(ExclR%Exc_Rect)
                        allocate(ExclR%Exc_Rect(2,2,ExclR%Nexc_Rect))
                        do k=1, ExclR%Nexc_Rect
                            n_ = n + k
                            line = adjustl(inst_file%line(n_)%str)
                            read(unit=line,fmt=*,iostat=ier) ExclR%Exc_Rect(:,1,k), ExclR%Exc_Rect(:,2,k)
                            if(ier /= 0) then
                                write(unit=*,fmt="(a,i3)") " => Error reading rectangular excluded region: ",k
                            end if
                        end do
                        if(maxij) then   !Checking the high
                            if(ExclR%Exc_Rect(1,2,k) > ExclR%imax) ExclR%Exc_Rect(1,2,k)=ExclR%imax
                            if(ExclR%Exc_Rect(2,2,k) > ExclR%jmax) ExclR%Exc_Rect(2,2,k)=ExclR%jmax
                        else
                            if(ExclR%Exc_Rect(1,2,k) > ExclR%imax) ExclR%imax=ExclR%Exc_Rect(1,2,k)
                            if(ExclR%Exc_Rect(2,2,k) > ExclR%jmax) ExclR%jmax=ExclR%Exc_Rect(2,2,k)
                        end if
                    end if
            end select
        end do

        if(.not. read_orig) then
            LaueDiff%xo=0.5_cp*real(LaueDiff%Np_h,kind=cp)
            LaueDiff%zo=0.5_cp*real(LaueDiff%Np_v,kind=cp)
            LaueDiff%dism=(/0.0,0.0/)
        else
            if(LaueDiff%dtype=="Cyl")then
                if(abs(LaueDiff%xo) < 1.0 .and. abs(LaueDiff%zo) < 1.0 ) then
                dispt=(/ 0.5_cp*real(LaueDiff%Np_h,kind=cp), 0.5_cp*real(LaueDiff%Np_v,kind=cp) /) !Coordinates of the detector centre in pixels
                else
                dispt=(/LaueDiff%xo,LaueDiff%zo/)
                end if
                call Pix_To_Mil(LaueDiff,dispt(1),dispt(2),xm,zm)             !Coordinates in mm
                LaueDiff%dism=(/-xm,-zm/)  !displacement w.r.t. centre of the (xo,zo) origin in mm
            else
                LaueDiff%dism=(/0.0_cp,0.0_cp/)
            end if
        end if

        if(LaueDiff%dtype == "Oct") then  !Cyclops type ... Calculate D interms of H
            LaueDiff%D = LaueDiff%H/16.0_cp/tan(pi/8.0_dp)
        End if

        call Calc_RD_rOD(LaueDiff)

    End Subroutine Read_Laue_Instrm

End Submodule Laue_Instrument