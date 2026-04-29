!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Stereo

    implicit none

    Contains

    Module Subroutine Get_Stereographic_Projection_From_ZV(r,x,z)
        !> Compute the stereographic projection from the
        !> scattering vector

        ! Arguments
        type(Laue_Ref_Type), intent(in)  :: r !> Reflection to be projected
        real(kind=4),        intent(out) :: x !> x coordinate of the projection
        real(kind=4),        intent(out) :: z !> z coordinate of the projection

        ! Local variables
        real :: d
        real, dimension(3) :: nv

        x = 0.0
        z = 0.0
        if (r%zv(2) > -0.0001) return
        nv = r%zv * r%ds ! Normalized vector
        d  = 1 - nv(2)
        x = nv(1) / d
        z = nv(3) / d

    End Subroutine Get_Stereographic_Projection_From_ZV

End Submodule Laue_Stereo