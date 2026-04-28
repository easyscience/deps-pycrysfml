submodule (CFML_VTK) VTK_Scan_Utils

    implicit none

    contains

    !!----
    !!---- SCAN_ARRAYS
    !!----    Subroutine to build vtk arrays for a 3D representation of the scan.
    !!----    Subroutine scan_limits must be called before to set a suitable
    !!----    threshold value in order to avoid memory exceptions.
    !!----
    !!---- 27/03/2023
    !!
    module subroutine scan_arrays(counts,th,vtk_points,vtk_cells,vtk_counts)

        ! Arguments
        integer(kind=4), dimension(:,:,:), intent(in)    :: counts     ! Scan counts
        integer,                           intent(in)    :: th         ! Threshold
        integer(kind=4), dimension(:,:),   intent(inout) :: vtk_points
        integer(kind=8), dimension(:,:),   intent(inout) :: vtk_cells
        integer(kind=4), dimension(:),     intent(inout) :: vtk_counts

        ! Local variables
        integer :: i,j,k,n

        n = 0
        do k = 1 , size(counts,3)
            do j = 1 , size(counts,2)
                do i = 1 , size(counts,1)
                    if (counts(i,j,k) >= th) then
                        n = n + 1
                        if (n > size(vtk_counts)) then
                            err_cfml%ierr = -1
                            err_cfml%flag = .true.
                            err_cfml%msg = 'scan_arrays: threshold inconsistent with array dimension'
                            return
                        end if
                        vtk_points(1,n) = i-1
                        vtk_points(2,n) = j-1
                        vtk_points(3,n) = k-1
                        vtk_cells(1,n) = 1
                        vtk_cells(2,n) = n
                        vtk_counts(n) = counts(i,j,k)
                    end if
                end do
            end do
        end do

    end subroutine scan_arrays

    !!----
    !!---- SCAN_LIMITS
    !!----    Subroutine to determine the threshold and maximum counts of a scan.
    !!----    Maximum number of points is set to 3125000 (100 MB). VTK raises
    !!----    exceptions for sizes > 2GB.
    !!----
    !!---- 27/03/2023
    !!
    module subroutine scan_limits(counts,th,max_cnt,npoints)

        ! Arguments
        integer(kind=4), dimension(:,:,:), intent(in)  :: counts   ! Scan counts
        integer,                           intent(out) :: th       ! Threshold
        integer,                           intent(out) :: max_cnt  ! Maximum counts / pixel
        integer,                           intent(out) :: npoints  ! Number of points in the range [th,max_cnt]

        ! Local variables
        integer, parameter :: MAX_NPOINTS = 3125000
        integer, parameter :: MAX_HISTO = 1000       ! Maximum number of counts / pixel for histogram calculation
        integer :: i,j,k,n
        integer, dimension(0:MAX_HISTO) :: histo

        histo(:) = 0
        max_cnt  = 0
        do k = 1 , size(counts,3)
            do j = 1 , size(counts,2)
                do i = 1 , size(counts,1)
                    n = min(counts(i,j,k),MAX_HISTO)
                    if (counts(i,j,k) > max_cnt) max_cnt = counts(i,j,k)
                    histo(n) = histo(n) + 1
                end do
            end do
        end do
        npoints = size(counts,1) * size(counts,2) * size(counts,3)
        th = 0
        do
            if (npoints <= MAX_NPOINTS) exit
            if (th == MAX_HISTO) exit
            npoints = npoints - histo(th)
            th = th + 1
        end do
        if (th == MAX_HISTO) then
            err_cfml%ierr = -1
            err_cfml%flag = .true.
            err_cfml%msg = 'scan_limits: Threshold cannot be computed'
        end if

    end subroutine scan_limits

end submodule VTK_Scan_Utils