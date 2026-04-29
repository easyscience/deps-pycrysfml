!!----
!!----
!!----
SubModule (CFML_Laue) Laue_HBin

    implicit none

    character(len=1), parameter:: end_line=char(10)

    Contains

    Module Subroutine Read_HBin_Header_2D_Image(Image_File,Max_Length,header,ok,mess,bin_lun,h_string)
        !> This subroutine reads and construct completely the object header from
        !> the information contained in the file "Image_File". The user should provide
        !> the Max_Length parameter that is the maximum number of bytes expected for
        !> the full header. The subroutine allocates all components of the header and
        !> read the value of some components for the first frame. There are items that
        !> are not necessary to be in the file (scalar, vector, matrix items). If they
        !> are not found the subroutine puts automatically to zero the corresponding
        !> number and tries to find the next field.
        !>
        !> Created: December 2011   (JRC)
        !> Updated: March 2019   (JRC)

        ! Arguments
        Character(len=*),         intent(in) :: Image_File
        integer,                  intent(in) :: Max_Length
        type(Header_Type),        intent(out):: header
        logical,                  intent(out):: ok
        character(len=*),         intent(out):: mess
        integer,                  intent(out):: bin_lun
        character(len=*),optional,intent(out):: h_string

        ! Local variables
        integer                              :: ier
        integer                              :: i,j,k,len_head
        character(len=max(1000,Max_Length))  :: header_string, pheader_string
        character(len=132)                   :: aux_string
        character(len=len(Image_File))       :: filename

        ok=.true.
        mess= " "

        ! [AF] june 2020 tests
        !write(*,fmt='(a)')       "Image_File:",trim(Image_File)
        !write(*,fmt='(a,i5)')    "char(0)   :",index(Image_File,char(0))
        !write(*,fmt='(a,i5,i5)') "Len Image_File,filename:",len(Image_File),len(filename)

        ! Open the file and check that the file is valid
        i=index(Image_File,".",back=.true.)
        if( i /= 0) then
            filename=trim(Image_File)
            if(index(Image_File(i:),".hbin") == 0) then
            ok=.false.
            mess="Read_Header_2D_Image -> Error: Image file with extension different from .hbin!"
            return
            end if
        else
            filename=trim(image_file)//".hbin"
        end if
        open(newunit=bin_lun,file=trim(filename), Status ="Old", Form="Unformatted",    &
            Access="stream",Action="Read",Position = "rewind",iostat=ier)
        if (ier/=0) then
            ok=.false.
            mess="Read_Header_2D_Image -> Error opening the file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        !Read the length of the header
        read(unit=bin_lun) aux_string  !read the 132 first bytes
        i=index(aux_string,"Header_Length: ") !Skip the image offset
        if( i == 0) then
            ok=.false.
            mess="Read_Header_2D_Image -> Unknown size of the header in file: "//trim(filename)
            close(unit=bin_lun)
            return
        else
            j=index(aux_string(i:),end_line)+i-1
            read(unit=aux_string(i+14:j-1),fmt=*) len_head
            if(len_head > Max_Length) then
            ok=.false.
            mess="Actual size greater than Max_Length in file: "//trim(filename)
            close(unit=bin_lun)
            return
            end if
            rewind(unit=bin_lun)
            read(unit=bin_lun) header_string(1:len_head)  !The full header is now in header_string
            if(present(h_string)) h_string=header_string(1:len_head)
            pheader_string=header_string(1:len_head)  !Original full header string
            !limit now the header_string to the part starting after the header length definition
            header_string=header_string(j+1:)
        end if

        !Initialize the header
        call Init_Header(header)

        i=index(header_string,"Title: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            header%Title = header_string(i+7:j-1)
            !header_string=header_string(j+1:)
        else
            header%Title = " "
        end if

        i=index(header_string,"User_LC_Date_time: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            header%User_LC_Date_time = header_string(i+19:j-1)
            !header_string=header_string(j+1:)
        else
            header%User_LC_Date_time = " "
        end if

        i=index(header_string,"N_Frames: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+9:j-1),fmt=*) header%Nframes
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No number of frames item 'N_Frames:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Nbits: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+6:j-1),fmt=*) header%Nbits
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No number of bits item 'Nbits:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Nrow-Ncol: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+10:j-1),fmt=*) header%Nrow,header%Ncol
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Nrow-Ncol:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Instrument_Name: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            Header%Instrument_Name=header_string(i+17:j-1)
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Instrument_Name:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Detector_Type: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            Header%Detector_type=header_string(i+15:j-1)
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Detector_Type:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"ColRow_Order: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            Header%colrow_order=header_string(i+14:j-1)
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'ColRow_Order:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Flags: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+7:j-1),fmt=*) header%flags
        end if

        i=index(header_string,"Pixel_size_h(mm): ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+17:j-1),fmt=*) header%Pix_Size_h
            header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Pixel_size_h(mm):' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Pixel_size_v(mm): ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+17:j-1),fmt=*) header%Pix_Size_v
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Pixel_size_v(mm):' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Sample_Detector_Dist(mm): ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+25:j-1),fmt=*) header%Sample_Detector_Dist
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Sample_Detector_Dist(mm):' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Lambda range(angstroms): ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+24:j-1),fmt=*) header%Lambda_min,header%Lambda_max
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Lambda range(angstroms):' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        i=index(header_string,"Scan_Type: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            Header%Scan_type=header_string(i+11:j-1)
            !header_string=header_string(j+1:)
        else
            ok=.false.
            mess="No item 'Scan_type:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
        end if

        if(Header%Scan_type(1:3) /= "ACQ") then
            i=index(header_string,"Scan Values: ")
            if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+12:j-1),fmt=*) header%Init_Val,header%Step_Val,header%Final_Val
            !header_string=header_string(j+1:)
            else
            ok=.false.
            mess="No item 'Scan Values:' in file: "//trim(filename)
            close(unit=bin_lun)
            return
            end if
        end if

        i=index(header_string,"N_comment_Items: ")
        if(i /= 0) then
            !header_string=header_string(i:)
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+17:j-1),fmt=*) header%N_comment_Items
            header_string=header_string(j+1:)
        else
            header%N_comment_Items=0
        end if

        if(Header%N_comment_Items /= 0) then
            if(allocated(header%comment)) deallocate(header%comment)
            allocate(header%comment(Header%N_comment_Items))
            do k=1,Header%N_comment_Items
            i=index(header_string,":")
            j=index(header_string,end_line)
            header%comment(k)=header_string(i+1:j-1)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"N_scalar_Items: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+15:j-1),fmt=*) header%N_Scalar_Items
            header_string=header_string(j+1:)
        else
            header%N_Scalar_Items=0
        end if

        if(Header%N_Scalar_Items /= 0) then
            if(allocated(header%Scalar_name)) deallocate(header%Scalar_name)
            allocate(header%Scalar_name(Header%N_Scalar_Items))
            if(allocated(header%Scalar_value)) deallocate(header%Scalar_name)
            allocate(header%Scalar_value(Header%N_Scalar_Items))
            do k=1,Header%N_Scalar_Items
            i=index(header_string,"(")
            j=index(header_string,")")
            header%Scalar_name(k)=header_string(i+1:j-1)
            i=index(header_string,":")
            j=index(header_string,end_line)
            read(unit=header_string(i+1:j-1),fmt=*) header%Scalar_value(k)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"N_vector_Items: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+15:j-1),fmt=*) header%N_vector_Items
            header_string=header_string(j+1:)
        else
            header%N_vector_Items=0
        end if

        if(Header%N_vector_Items /= 0) then
            if(allocated(header%Vector_name)) deallocate(header%Vector_name)
            allocate(header%Vector_name(Header%N_vector_Items))
            if(allocated(header%Vector_value)) deallocate(header%Vector_name)
            allocate(header%Vector_value(3,Header%N_vector_Items))
            do k=1,Header%N_vector_Items
            i=index(header_string,"(")
            j=index(header_string,")")
            header%Vector_name(k)=header_string(i+1:j-1)
            i=index(header_string,":")
            j=index(header_string,end_line)
            read(unit=header_string(i+1:j-1),fmt=*) header%Vector_value(:,k)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"N_matrix_Items: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+15:j-1),fmt=*) header%N_matrix_Items
            header_string=header_string(j+1:)
        else
            header%N_matrix_Items=0
        end if

        if(Header%N_matrix_Items /= 0) then
            if(allocated(header%Matrix_name)) deallocate(header%Matrix_name)
            allocate(header%Matrix_name(Header%N_Matrix_Items))
            if(allocated(header%Matrix_value)) deallocate(header%Matrix_name)
            allocate(header%Matrix_value(3,3,Header%N_Matrix_Items))
            do k=1,Header%N_matrix_Items
            i=index(header_string,"(")
            j=index(header_string,")")
            header%Matrix_name(k)=header_string(i+1:j-1)
            i=index(header_string,":")
            j=index(header_string,end_line)
            read(unit=header_string(i+1:j-1),fmt=*) header%Matrix_value(:,:,k)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"N_environment: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+14:j-1),fmt=*) Header%N_environment
            header_string=header_string(j+1:)
        else
            Header%N_environment=0
        end if

        if(Header%N_environment /= 0) then

            if(allocated(header%Envnt_Name)) deallocate(header%Envnt_Name)
            allocate(header%Envnt_Name(Header%N_environment))
            if(allocated(header%Envnt_Unit)) deallocate(header%Envnt_Unit)
            allocate(header%Envnt_Unit(Header%N_environment))
            if(allocated(header%Envnt_value)) deallocate(header%Envnt_value)
            allocate(header%Envnt_value(Header%N_environment,Header%Nframes))

            do k=1,Header%N_environment
            i=index(header_string,"(")
            j=index(header_string,")")
            header%Envnt_Name(k)=header_string(i+1:j-1)
            i=index(header_string,":")
            j=index(header_string,"~")
            read(unit=header_string(i+1:j-1),fmt=*) header%Envnt_value(k,1)
            i=j
            j=index(header_string,end_line)
            header%Envnt_Unit(k)=header_string(i+1:j-1)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"N_motors: ")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            read(unit=header_string(i+9:j-1),fmt=*) Header%N_motors
            header_string=header_string(j+1:)
        else
            Header%N_motors=0
        end if

        if(Header%N_motors /= 0) then
            if(allocated(header%Motor_Name)) deallocate(header%Motor_Name)
            allocate(header%Motor_Name(Header%N_Motors))
            if(allocated(header%Motor_Unit)) deallocate(header%Motor_Unit)
            allocate(header%Motor_Unit(Header%N_Motors))
            if(allocated(header%Motor_value)) deallocate(header%Motor_value)
            allocate(header%Motor_value(Header%N_Motors,Header%Nframes))

            do k=1,Header%N_motors
            i=index(header_string,"(")
            j=index(header_string,")")
            header%Motor_Name(k)=header_string(i+1:j-1)
            i=index(header_string,":")
            j=index(header_string,"~")
            read(unit=header_string(i+1:j-1),fmt=*) header%Motor_value(k,1)
            i=j
            j=index(header_string,end_line)
            header%Motor_Unit(k)=header_string(i+1:j-1)
            header_string=header_string(j+1:)
            end do
            header_string = pheader_string  !reset header string to full one
        end if

        i=index(header_string,"Time")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            if(allocated(header%time)) deallocate(header%time)
            allocate(header%time(Header%Nframes))
            i=index(header_string,":")
            read(unit=header_string(i+1:j-1),fmt=*) header%time(1)
            !header_string=header_string(j+1:)
        end if

        i=index(header_string,"Monitor")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            if(allocated(header%Monitor)) deallocate(header%Monitor)
            allocate(header%Monitor(Header%Nframes))
            i=index(header_string,":")
            read(unit=header_string(i+1:j-1),fmt=*) header%monitor(1)
            !header_string=header_string(j+1:)
        end if

        i=index(header_string,"Counts")
        if(i /= 0) then
            j=index(header_string(i:),end_line)+i-1
            if(allocated(header%counts)) deallocate(header%counts)
            allocate(header%counts(Header%Nframes))
            i=index(header_string,":")
            read(unit=header_string(i+1:j-1),fmt=*) header%counts(1)
            !header_string=header_string(j+1:)
        end if

        !Read here the sigma_factor and normalization values (if they are not given we assign to monitor and time)
        i=index(pheader_string,"Sigma_Factor: ")
        if(i /= 0) then
            j=index(pheader_string(i:),end_line)+i-1
            read(unit=pheader_string(i+13:j-1),fmt=*) header%sigma_factor
        else
            header%sigma_factor=1.0
        end if

        i=index(pheader_string,"Normalization_Tim:")
        if(i /= 0) then
            j=index(pheader_string(i:),end_line)+i-1
            read(unit=pheader_string(i+18:j-1),fmt=*) header%Normalization_Time
        else
            header%Normalization_Time=header%Time(1)
        end if

        i=index(pheader_string,"Normalization_Mon:")
        if(i /= 0) then
            j=index(pheader_string(i:),end_line)+i-1
            read(unit=pheader_string(i+18:j-1),fmt=*) header%Normalization_Monitor
        else
            header%Normalization_Monitor=header%Monitor(1)
        end if
        !
        !keep the file opened for reading the raw data of the image
        !
    End Subroutine Read_HBin_Header_2D_Image

End Submodule Laue_HBin