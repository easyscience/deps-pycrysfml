!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Init

    implicit none

    Contains

    Module Subroutine Init_Header(Head)

        ! Arguments
        type(Header_Type), intent(in out) :: Head

        Head%Header_Length=2048
        Head%Title=" "
        head%User_LC_Date_Time=" "
        Head%Nframes=1
        Head%Nbits=16
        Head%Nrow=0; Head%Ncol=0
        Head%Instrument_name=" "
        Head%Detector_type="FLAT"
        Head%colrow_order="Column_Order"
        Head%flags=0 !dimensioned to 11
        Head%Pix_Size_h=0.0; Head%Pix_Size_v=0.0
        Head%Sample_Detector_dist=0.0
        Head%Lambda_min=0.0; Head%Lambda_max=0.0
        Head%Scan_Type="ACQ"
        Head%Sigma_Factor=1.0
        Head%normalization_time=0.0
        Head%normalization_monitor=0.0
        Head%Init_Val=0.0; Head%Step_Val=0.0; Head%Final_Val=0.0
        Head%N_comment_Items=0
        Head%N_scalar_Items=0
        Head%N_vector_Items=0
        Head%N_matrix_Items=0
        Head%N_environment=0
        Head%N_motors=0

   End Subroutine Init_Header

    Module Subroutine Init_Laue_Instrm(LaueDiff)

        ! Arguments
        Type(Laue_Instrument_Type),  intent(in out) :: LaueDiff

        LaueDiff%Info="Default Laue Diffractometer as VIVALDI"
        LaueDiff%Name="LAUE_DIFF"
        LaueDiff%dtype="Cyl"
        LaueDiff%r_ord='xzy'
        LaueDiff%D=159.155_cp
        LaueDiff%ga_d=0.0_cp
        LaueDiff%nu_d=0.0_cp
        LaueDiff%tiltx_d=0.0_cp
        LaueDiff%tilty_d=0.0_cp
        LaueDiff%tiltz_d=0.0_cp
        LaueDiff%H=800.0_cp
        LaueDiff%V=400.0_cp
        LaueDiff%np_h=4000
        LaueDiff%np_v=2000
        LaueDiff%rOD=(/0.0_cp,0.0_cp,0.0_cp/)
        LaueDiff%RD=reshape((/1.0_cp,0.0_cp,0.0_cp,  &
                              0.0_cp,1.0_cp,0.0_cp,  &
                              0.0_cp,0.0_cp,1.0_cp/),(/3,3/))
        LaueDiff%L_min     = 0.7_cp
        LaueDiff%L_max     = 4.0_cp
        LaueDiff%d_min     = 1.0_cp
        LaueDiff%gap_max   = (0.5_cp*LaueDiff%H/LaueDiff%D)*to_deg
        LaueDiff%gap_min   =  0.0_cp
        LaueDiff%gan_max   =  0.0_cp
        LaueDiff%gan_min   = -LaueDiff%gap_max
        LaueDiff%x_max     =   0.5_cp*LaueDiff%H
        LaueDiff%x_min     =  -LaueDiff%x_max
        LaueDiff%nu_max    =   atan2d(0.5_cp*LaueDiff%V,LaueDiff%D)
        LaueDiff%nu_min    =  -LaueDiff%nu_max
        LaueDiff%z_max     =   0.5_cp*LaueDiff%V
        LaueDiff%z_min     =  -LaueDiff%z_max
        LaueDiff%xo        =   0.5_cp*real(LaueDiff%np_h,kind=cp)
        LaueDiff%zo        =   0.5_cp*real(LaueDiff%np_v,kind=cp)
        LaueDiff%dism      =   0.0_cp
        LaueDiff%tilted    = .false.     ! True if RD /= I
        LaueDiff%displaced = .false.  ! True if rOD /= (0,0,0)
        LaueDiff%flip_hor  = .false.   ! True -> flip the image horizontally as soon as read
        LaueDiff%flip_ver  = .false.   ! True -> flip the image vertically as soon as read

    End Subroutine Init_Laue_Instrm

    Elemental Module Subroutine Init_Laue_Ref(Ref,d)

        ! Arguments
        type(Laue_Ref_Type), intent (in out) :: Ref
        integer, optional,   intent (in)     :: d

        Ref%stats   = 0
        Ref%mult    = 0
        Ref%nodal   = 0
        Ref%ds      = 0.0
        Ref%gamma   = 0.0
        Ref%nu      = 0.0
        Ref%Ttheta  = 0.0
        Ref%Lambda  = 0.0
        Ref%h       = 0.0
        Ref%zv      = 0.0
        Ref%Obs_int = 0.0
        Ref%Cal_int = 0.0
        Ref%Sigma   = 0.0
        Ref%x       = 0.0
        Ref%z       = 0.0
        Ref%stheta  = 0.0
        Ref%sphi    = 0.0
        Ref%kindex  = 0
        if(present(d)) then
            if(allocated (Ref%hs)) deallocate(Ref%hs)
            allocate(Ref%hs(3+d))
            Ref%hs=0
        end if

    End subroutine Init_Laue_Ref

End Submodule Laue_Init