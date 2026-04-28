!!----
!!----
!!----
Submodule (CFML_FFT) FFT_Gen
  implicit none
 Contains
    !!--++
    !!--++ FFT1D
    !!--++    FFT one dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft1D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:), intent(in)           :: array     ! In -> Complex array
       integer,          dimension(:), intent(in),  optional:: dim       ! In -> array containing the dimensions to be transformed
       logical,                        intent(in),  optional:: inv       ! In -> If .true., inverse transformation will be performed.
                                                                         !       Default is .false., i.e. forward transformation.
       complex(fftkind), dimension(size(array, 1))          :: ft

       !--- function result
       integer :: ierr

       ierr=0
       ft = array
       call fftn(ft, shape(array), dim, inv = inv, stat = ierr)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT1D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft1D

    !!--++
    !!--++ FFT2D
    !!--++    FFT two dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft2D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:), intent(in)           :: array         ! In -> Complex array
       integer,          dimension(:),   intent(in),  optional:: dim           ! In -> array containing the dimensions to be transformed
       logical,                          intent(in),  optional:: inv           ! In -> If .true., inverse transformation will be performed.
                                                                               !       Default is .false., i.e. forward transformation.
       complex(fftkind), dimension(size(array, 1), size(array, 2)):: ft

       !--- function result
       integer                                                    :: ierr

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])

       ierr=0
       call fftn(work, shape(array), dim, inv, stat = ierr)
       ft = reshape(work, [ size(array, 1), size(array, 2) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT2D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft2D

    !!--++
    !!--++ FFT3D
    !!--++    FFT three dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft3D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:,:), intent(in)           :: array        ! In -> Complex array
       integer,          dimension(:),     intent(in),  optional:: dim          ! In -> array containing the dimensions to be transformed
       logical,                            intent(in),  optional:: inv          ! In -> If .true., inverse transformation will be performed.
                                                                                !       Default is .false., i.e. forward transformation.

       !--- function result
       integer :: ierr
       complex(fftkind), &
            dimension(size(array, 1), size(array, 2), size(array, 3)):: ft

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])

       ierr=0
       call fftn(work, shape(array), dim, inv, stat = ierr)
       ft = reshape(work, [ size(array, 1), size(array, 2), size(array, 3) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT3D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft3D

    !!--++
    !!--++ FFT4D
    !!--++    FFT four dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft4D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:,:,:), intent(in)           :: array         ! In -> Complex array
       integer,          dimension(:),       intent(in),  optional:: dim           ! In -> array containing the dimensions to be transformed
       logical,                              intent(in),  optional:: inv           ! In -> If .true., inverse transformation will be performed.
                                                                                   !       Default is .false., i.e. forward transformation.

       !--- function result
       integer :: ierr
       complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4)):: ft

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])
       ierr=0
       call fftn(work, shape(array), dim, inv, stat = ierr)
       ft = reshape(work, [ size(array, 1), size(array, 2), size(array, 3), &
                             size(array, 4) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT4D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft4D

    !!--++
    !!--++ FFT5D
    !!--++    FFT five dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft5D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:,:,:,:), intent(in)           :: array           ! In -> Complex array
       integer,          dimension(:),         intent(in),  optional:: dim             ! In -> array containing the dimensions to be transformed
       logical,                                intent(in),  optional:: inv             ! In -> If .true., inverse transformation will be performed.
                                                                                       !       Default is .false., i.e. forward transformation.

       !--- function result
       integer :: ierr

       complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5)):: ft

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])

       ierr=0
       call fftn(work, shape(array), dim, inv, stat = IErr)
       ft = reshape(work, [ size(array, 1), size(array, 2), size(array, 3), &
                             size(array, 4), size(array, 5) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT5D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft5D

    !!--++
    !!--++ FFT6D
    !!--++    FFT six dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft6D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:,:,:,:,:), intent(in)           :: array         ! In -> Complex array
       integer,          dimension(:),           intent(in),  optional:: dim           ! In -> array containing the dimensions to be transformed
       logical,                                  intent(in),  optional:: inv           ! In -> If .true., inverse transformation will be performed.
                                                                                       !       Default is .false., i.e. forward transformation.

       !--- function result
       integer :: ierr
       complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5), size(array, 6)):: ft

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])

       ierr=0
       call fftn(work, shape(array), dim, inv, stat = ierr)
       ft = reshape(work, [ size(array, 1), size(array, 2), size(array, 3), &
                             size(array, 4), size(array, 5), size(array, 6) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT6D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft6D

    !!--++
    !!--++ FFT7D
    !!--++    FFT seven dimension
    !!--++
    !!--++ 14/04/2019
    !!
    Module Function Fft7D(Array, Dim, Inv) Result(Ft)
       !--- formal parameters
       complex(fftkind), dimension(:,:,:,:,:,:,:), intent(in)           :: array      ! In -> Complex array
       integer,          dimension(:),             intent(in),  optional:: dim        ! In -> array containing the dimensions to be transformed
       logical,                                    intent(in),  optional:: inv        ! In -> If .true., inverse transformation will be performed.
                                                                                      !       Default is .false., i.e. forward transformation.

       !--- function result
       integer :: ierr
       complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5), size(array, 6), size(array, 7)):: ft

       !--- allocate 1-d array to be used by routine fftn
       complex(fftkind), dimension(:), allocatable :: work

       allocate( work(size(array)) )
       work = reshape(array, [ size(array) ])
       ierr=0
       call fftn(work, shape(array), dim, inv, stat = IErr)
       ft = reshape(work, [ size(array, 1), size(array, 2), size(array, 3), &
                             size(array, 4), size(array, 5), size(array, 6), &
                             size(array, 7) ])

       if (allocated(work)) deallocate(work)
       if (ierr /=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="FFT7D@CFML_FFT: Error in FFT!"
       end if

    End Function Fft7D

    !!--++
    !!--++ FFTN
    !!--++    General routine for FFT calculations
    !!--++
    !!--++ 14/04/2019
    !!
    Module Subroutine Fftn(Array, Shape, Dim, Inv, Stat)
       !--- formal parameters
       complex(fftkind), dimension(:), intent(in out)       :: array
       integer,          dimension(:), intent(in)           :: shape
       integer,          dimension(:), intent(in),  optional:: dim
       logical,                        intent(in),  optional:: inv
       integer,                        intent(out), optional:: stat

       !--- local arrays
       integer, dimension(size(shape)):: d

       !--- local scalars
       logical      :: inverse
       integer      :: i, ndim, ntotal
       real(fftkind):: scal

       !--- optional parameter settings
       if (present(inv)) then
          inverse = inv
       else
          inverse = .false.
       end if
       if (present(dim)) then
          ndim = min(size(dim), size(d))
          d(1:ndim) = dim(1:ndim)
       else
          ndim = size(d)
          d = [(i, i = 1, size(d))]
       end if
       ntotal = product(shape)
       scal = sqrt(1.0_fftkind / product(shape(d(1:ndim))))
       array(1:ntotal) = array(1:ntotal) * scal
       do i = 1, ndim
          call fftradix(array, ntotal, shape(d(i)), product(shape(1:d(i))), &
               inverse, stat)
          if (present(stat)) then
             if (stat /=0) return
          end if
       end do

    End Subroutine Fftn

End SubModule FFT_Gen
