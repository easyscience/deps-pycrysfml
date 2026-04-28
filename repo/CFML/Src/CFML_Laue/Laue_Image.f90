!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Image

    implicit none

    Contains

    Module Subroutine Read_Binary_2D_Image(bin_lun,header,Image,nf)
        !> This subroutine reads the value of the environment and motor variables
        !> as well as the intensities of the image corresponding to the frame nf.
        !> It is assumed that the header has already been read and fully constructed.
        !>
        !> Created: December 2011   (JRC)
        !> Updated: December 2011   (JRC)

        ! Arguments
        integer,                                 intent(in)    :: bin_lun  !Logical unit of binary file
        type(header_type),                       intent(inout) :: header
        integer(kind=datakind),  dimension(:,:), intent(out)   :: Image
        integer ,                                intent(in)    :: nf  !number of the frame to be added

        ! Local variables
        integer(kind=2), dimension(size(image,1),size(image,2)) :: short_Image
        integer :: i,j,n

        ! Reading Time, Monitor and Counts
        if(allocated(header%Time))    read(unit=bin_lun) header%Time(nf)
        if(allocated(header%Monitor)) read(unit=bin_lun) header%Monitor(nf)
        if(allocated(header%counts))  read(unit=bin_lun) header%counts(nf)
        ! Reading the Environment and motor variables according to instructions of the header
        ! corresponding to the frame number nf
        n=Header%N_Environment
        if( n /= 0) then
        read(unit=bin_lun) header%Envnt_Value(1:n,nf)
        end if
        n=Header%N_Motors
        if( n /= 0) then
        read(unit=bin_lun) header%Motor_Value(1:n,nf)
        end if
        !Reading the image according to instructions of header 16 bits or 32 bits
        if(header%Nbits == 16) then
            if(header%colrow_order == "Column_Order") then  !order by columns (Fortran-like)
            read(unit=bin_lun) short_Image
            else                            !order by rows (C-like)
            read(unit=bin_lun) ((short_Image(i,j),j=1,Header%Ncol),i=1,Header%Nrow)
            end if
            Image=short_image
            where(Image < 0)  Image=Image+65536
        else !32 bits image
            if(header%colrow_order == "Column_Order") then  !order by columns
            read(unit=bin_lun) Image  !Directly in Fortran
            else      !order by rows
            read(unit=bin_lun) ((Image(i,j),j=1,Header%Ncol),i=1,Header%Nrow)
            end if
        end if
        !the logical unit is still opened for reading more frames
    End Subroutine Read_Binary_2D_Image

    Module Subroutine Read_Laue_Image(Image_File,Nrow,Ncol,Datam,ICd,header,extension,h_string,directory)

        ! Arguments
        character(len=*),                      intent(in)    :: Image_File
        integer,                               intent(inout) :: Nrow, Ncol
        integer(kind=DATAKIND),dimension(:,:), intent(out)   :: Datam
        type(Image_Conditions), optional,      intent(out)   :: ICd
        type(Header_Type),      optional,      intent(out)   :: header
        character(len=5),       optional,      intent(out)   :: extension
        character(len=*),       optional,      intent(out)   :: h_string
        character(len=*),       optional,      intent(in)    :: directory

        ! Local variables
        integer                :: i,j,k,log_uni,ier,int32b,Max_Length=3000
        integer(kind=2)        :: int16b
        character(len=5)       :: ext
        character(len=158)     :: mess
        character(len=512)     :: full_name
        character(len=4100)    :: kcd_header
        real                   :: tmp_real
        logical                :: ok
        Type(Image_Conditions) :: ICondt
        Type(Header_Type)      :: head

        call clear_error()
        log_uni = 101 ! Nebil: I have added this assignment because there is no more get_logunit in CrysFML2008
        i=index(Image_File, ".", back=.true.)
        ok=.true.
        if(i == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = 1
            err_cfml%msg = "Image file without extension!"
            return
        end if
        ext=Image_File(i+1:)
        if(present(extension)) extension=ext
        full_name=Image_File
        if(present(directory)) full_name=trim(directory)//OPS_SEP//trim(Image_File)

        select case(trim(ext))

            !case("tif","TIF","tiff","TIFF")
            !    call read_tiff_image(Full_Name,Datam,Nrow,Ncol,ok,ICondt)
            !    !call write_icd(ICondt)
            !    if(present(h_string)) then
            !        h_string=ICondt%comment
            !    end if
            !    if(present(ICd)) ICd=ICondt
            !    if(TIFF_READ_Error .and. Ncol <= 4000) then !Error message for VIVALDI
            !        write(unit=*,fmt="(a)") " => "//trim(TIFF_READ_Error_Message)
            !        ok=.false.
            !        return
            !    end if
            !case("reo","REO","img","IMG")
            !    call read_reo_image(Full_Name,Nrow,Ncol,Datam,ok)
            !case("raw","RAW")
            !    !Assumes 16 unsigned integers stored without any offset. The image matrix is stored by rows.
            !    !The raw data are stored in big-endian mode (this may be chosen in the CFL file in forthcoming versions)
            !    call get_logunit(log_uni)
            !    open(unit=log_uni, file=Full_Name, status="old",action="read", position="rewind", convert= 'BIG_ENDIAN',&
            !            access="stream", form="unformatted")
            !    do j=1,Ncol
            !        do i=1,Nrow
            !        read(unit=log_uni) int16b
            !        int32b=int16b
            !        if(int32b < 0) int32b=int32b+65536
            !        Datam(i,j) = int32b
            !        end do
            !    end do
            !    ok=.true.
            !case("mccd","MCCD","mCCD")
            !    !Assumes 16 unsigned integers stored without an offsetm of 4096 bytes. The image matrix is stored by rows.
            !    !The raw data are stored in big-endian mode (this may be chosen in the CFL file in forthcoming versions)
            !    call read_tiff_image(Full_Name,Datam,Nrow,Ncol,ok,ICondt)
            !    if(present(ICd)) ICd=ICondt
            !    if(TIFF_READ_Error .and. Ncol <= 4000) then !Error message for VIVALDI
            !        write(unit=*,fmt="(a)") " => "//trim(TIFF_READ_Error_Message)
            !        ok=.false.
            !        return
            !    end if
            !    call flip_2D(Datam,.true.,.false.)
            !    ok=.true.
            !case("kcd","KCD","Kcd")
            !    !Test for reading kappa ccd images
            !    call get_logunit(log_uni)
            !    open(unit=log_uni, file=Full_Name, status="old",action="read", position="rewind", &
            !            access="stream", form="unformatted")
            !    read(unit=log_uni) kcd_header
            !    do i=1,Nrow
            !        do j=1,Ncol
            !        read(unit=log_uni) int16b
            !        int32b=int16b
            !        if(int32b < 0) int32b=int32b+65536
            !        Datam(i,j) = int32b
            !        end do
            !    end do
            !    ok=.true.
            !case("bn","BN")
            !    call get_logunit(log_uni)
            !    open(unit=log_uni, file=Full_Name, status="old",action="read", position="rewind", &
            !            access="stream", form="unformatted")
            !    do i=1,Nrow
            !        do j=1,Ncol
            !        read(unit=log_uni) tmp_real
            !        Datam(i,j)=nint(tmp_real)
            !        end do
            !    end do
            !    ok=.true.
            !case("bin","BIN")
            !    call read_simple_binary_image(Full_Name,Nrow,Ncol,Datam,ok)
            case("hbin","HBIN")
                if(present(h_string)) then
                    call Read_HBin_Header_2D_Image(Full_Name,Max_Length,head,ok,mess,log_uni,h_string)
                else
                    call Read_HBin_Header_2D_Image(Full_Name,Max_Length,head,ok,mess,log_uni)
                end if
                if(head%Nrow /= Nrow .or. head%Ncol /= Ncol) then  !Checking compatibility with the instrument (Size passed in the arguments Nrow,Ncol)
                    close(unit=log_uni)
                    err_cfml%flag = .true.
                    err_cfml%ierr = 1
                    write(unit=err_cfml%msg,fmt="(2(a,2i8))") "Incompatible size of images, Instrument size: ",Nrow,Ncol,&
                        ". Image size (according to header), Rows & Columns: ",head%Nrow,head%Ncol
                    return
                end if
                if (ok) then
                    call Read_Binary_2D_Image(log_uni,head,Datam,1)
                    if(present(header)) header=head
                    close(unit=log_uni)
                else
                    err_cfml%flag = .true.
                    err_cfml%ierr = 1
                    err_cfml%msg = trim(mess)
                    return
                end if
        End select

    End Subroutine Read_Laue_Image

End Submodule Laue_Image