!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Ref_Gen

    implicit none

    Contains

    Module Subroutine Generate_Laue_Reflections(ldiff,cell,spg,hkl,n)
        !> Compute all acccesible reflections for the given Laue limits,
        !> cell and spacegroup

        ! Arguments
        type(Laue_Instrument_Type), intent(in)    :: ldiff  ! Laue instrument
        class(Cell_G_Type),         intent(in)    :: cell   ! Crystal cell
        class(Spg_Type),            intent(in)    :: spg    ! Space group
        type(RefList_Type),         intent(inout) :: hkl    ! List of reflections
        integer,                    intent(out)   :: n      ! Expected maximum number of Laue reflections

        ! Local Variables
        integer :: MaxNumRef,i
        integer, dimension(3) :: ord
        real(kind=CP) :: stlmax

        ord       = (/3,2,1/)
        stlmax    = 1.0_CP / (2.0_CP * ldiff%d_min)
        MaxNumRef = Get_MaxNumRef(stlmax,cell%Vol) !Removing Mult!
        !MaxNumRef = MaxNumRef * spg%NumOps * max(spg%Centred,1) !Not needed anymore

        call Gener_Reflections(cell,0.0,stlmax,hkl)

        ! Calculate the expected maximum number of Laue Reflections that can be simultaneously
        ! stimulated according to the limits
        if (ldiff%L_max < 2.0_CP * ldiff%d_min) then  !ITC vol C, p. 28
            n = 0.25 * PI * (ldiff%L_max-ldiff%L_min) * cell%Vol / real(spg%num_lat) / ldiff%d_min**4
        else
            ! ------------------------------------------------------------------------------------------
            ! Adapting to CrysFML2008
            ! In Esmeralda, we divide by spg%num_lat. Translation 000 was included in spg%num_lat in old
            ! CrysFML. In new CrysFML, it's not. This is why we have to add 1 now.
            ! Nebil, 8th April 2026.
            n = (4.0 / 3.0) * PI * ((1.0/ldiff%L_min)**3 - (1.0_CP/ldiff%L_max)**3) * cell%Vol / (real(spg%num_lat)+1)
            ! ------------------------------------------------------------------------------------------
            n = min(hkl%nref,n)
        end if

    End Subroutine Generate_Laue_Reflections

End Submodule Laue_Ref_Gen