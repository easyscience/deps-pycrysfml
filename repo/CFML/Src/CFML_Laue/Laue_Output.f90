!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Output

    implicit none

    Contains

    Module Subroutine Output_Excluded_Regions(lun,ExclR)

        ! Arguments
        integer,                     intent(in) :: lun
        type(Excluded_Regions_Type), intent(in) :: ExclR

        ! Local variables
        integer :: k
        character(len=14) :: inverse_str

        ! Output the result of reading excluded regions
        !if(maxij) then
        write(unit=lun,fmt="(8x,a,2i5)") "Maximum ij-indices for excluded regions ", ExclR%imax, ExclR%jmax
        !else
        !    write(unit=lun,fmt="(8x,a)")     "Maximum ij-indices for excluded regions not given in the file"
        !    write(unit=lun,fmt="(8x,a,2i5)") "Provisional values: ", ExclR%imax, ExclR%jmax
        !end if
        if( ExclR%Nexc_Rect > 0) then
            write(unit=lun,fmt="(8x,a,i4)") "Number of rectangular excluded regions ", ExclR%Nexc_Rect
            write(unit=lun,fmt="(8x,a)")    "i-min   j-min       i-max   j-max"
            do k=1, ExclR%Nexc_Rect
            inverse_str=" "
            if(k == ExclR%Inv_Rect) inverse_str=" INVERSE MASK "
            write(unit=lun,fmt="(8x,tr1,2(2i8,tr4),a)") ExclR%Exc_Rect(:,1,k), ExclR%Exc_Rect(:,2,k),inverse_str
            end do
        else
            write(unit=lun,fmt="(8x,a)") "NO rectangular excluded regions "
        end if
        if( ExclR%Nexc_Circ > 0) then
            write(unit=lun,fmt="(8x,a,i4)") "Number of circular excluded regions ", ExclR%Nexc_Circ
            write(unit=lun,fmt="(8x,a)")    "i-cent  j-cent  radius  "
            do k=1, ExclR%Nexc_Circ
            inverse_str=" "
            if(k == ExclR%Inv_Circ) inverse_str=" INVERSE MASK "
            write(unit=lun,fmt="(8x,tr2,2i8,tr4,i4,a)") ExclR%Exc_Circ(:,k),inverse_str
            end do
        else
            write(unit=lun,fmt="(8x,a)") "NO circular excluded regions "
        end if

    End Subroutine Output_Excluded_Regions

End Submodule Laue_Output