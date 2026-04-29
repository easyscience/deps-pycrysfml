!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Geom

    implicit none

    Contains

    Module Subroutine Calc_RD_rOD(LaueDiff)
        !>  Subroutine calculating the rotation matrix LaueDiff%RD
        !>  and the translation vector LaueDiff%rOD (in Cartesian coordinates)
        !>  to position a flat rectangular detector in an arbitrary position in
        !>  the L-system.
        !>  It is supposed that the angular components of the tilt angles of
        !>  the detector, the distance from the sample to detector and the
        !>  polar angles of the detector centre are already contained in
        !>  LaueDiff components.
        !>
        !>  Created: June 2010 (JRC)
        !>  Updated: June 2010

        ! Argument
        type(Laue_Instrument_Type), intent(in out)  :: LaueDiff
      
        ! Local variables
        real(kind=cp), dimension(3,3) :: Rx,Ry,Rz

        Rx=Matrix_Rx(LaueDiff%tiltx_d,"D")
        Ry=Matrix_Ry(LaueDiff%tilty_d,"D")
        Rz=Matrix_Rz(LaueDiff%tiltz_d,"D")
        select case(LaueDiff%r_ord)
            case('xyz','XYZ')
            LaueDiff%RD=Matmul(Rx,matmul(Ry,Rz))
            case('xzy','XZY')
            LaueDiff%RD=Matmul(Rx,matmul(Rz,Ry))
            case('zxy','ZXY')
            LaueDiff%RD=Matmul(Rz,matmul(Rx,Ry))
            case('zyx','ZYX')
            LaueDiff%RD=Matmul(Rz,matmul(Ry,Rx))
            case('yxz','YXZ')
            LaueDiff%RD=Matmul(Ry,matmul(Rx,Rz))
            case('yzx','YZX')
            LaueDiff%RD=Matmul(Ry,matmul(Rz,Rx))
        end select
        if(LaueDiff%dtype == "Rec") then
            ! For cylindrical detectors LaueDiff%rOD is directly read in the instrument file
            LaueDiff%rOD=(/sind(LaueDiff%ga_d)*cosd(LaueDiff%nu_d),cosd(LaueDiff%ga_d)*cosd(LaueDiff%nu_d),sind(LaueDiff%nu_d)/)
            LaueDiff%rOD=LaueDiff%D*LaueDiff%rOD
        end if

    End Subroutine Calc_RD_rOD

    Module Subroutine Pix_To_Mil(LaueDiff,xpc,ypc,xmm,ymm)
        !> Conversion from pixels to mm for the instrument LaueDiff
        !> The origin for pixels is on the left bottom corner starting at (1,1)
        !> The origin for mm is in the position LaueDiff%(xo,zo) in pixels of
        !> the detecting surface.
        !>
        !>   Created: April 2010 (JRC & LFM)
        !>   Updated:  June 2010 (LFM)
        !>   Updated:  July 2010 (JRC)
        !>
        
        ! Arguments
        type(Laue_Instrument_Type), intent(in)  :: LaueDiff
        real(kind=cp),              intent(in)  :: xpc,ypc
        real(kind=cp),              intent(out) :: xmm,ymm

        xmm = LaueDiff%H * (xpc-LaueDiff%xo)/real(LaueDiff%np_h-1,kind=cp)
        ymm = LaueDiff%V * (ypc-LaueDiff%zo)/real(LaueDiff%np_v-1,kind=cp)

    End Subroutine Pix_To_Mil

End Submodule Laue_Geom