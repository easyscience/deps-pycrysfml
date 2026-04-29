SubModule (CFML_DiffPatt) DiffP_ReadPatt_GSAS

 implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_GSAS
   !!--++
   !!--++    Read a pattern for GSAS
   !!--++
   !!--++ 01/05/2019
   !!
   Module Subroutine Read_Pattern_Gsas(Filename, Pat, mode)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: Filename      ! Path+Filename
      class(DiffPat_Type),        intent(out) :: Pat
      character(len=*), optional, intent(in)  :: Mode

      !---- Local Variables ----!
      logical                                      :: previous, bank_missed
      logical, save                                :: keep_open=.false.
      character (len=80)                           :: line
      character (len=8 )                           :: bintyp,datyp
      integer                                      :: items,i, nbank,i_dat
      integer                                      :: ibank,nchan,nrec, ier !, jobtyp
      integer,          dimension(:), allocatable  :: iww
      integer,          dimension(40)              :: pointi, pointf
      real(kind=cp),    dimension(4)               :: bcoef
      real(kind=cp)                                :: divi
      real(kind=cp)                                :: cnorm,step
      logical                                      :: ok, info
      logical                                      :: tof !used only for some type of formats


      !> Init
      call clear_error()

      !> File exists?
      inquire(file=trim(filename),exist=info)
      if ( .not. info) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: The file "//trim(filename)//" doesn't exist"
         return
      end if

      !> Open File
      open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
      if (ier /= 0 ) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: Problems opening the file: "//trim(filename)
         return
      end if

      ok=.false.
      if (present(mode)) then
         divi=1.0_cp
         tof=.true.
         i=len_trim(mode)
         if (i == 3) then
            nbank=1
         else
            read(unit=mode(4:),fmt=*,iostat=ier) nbank       !tofn
            if (ier /= 0) nbank=1
         end if
      else
         nbank=1
         divi=100.0_cp
         tof=.false.
      end if

      if (.not. keep_open) then
         bank_missed=.true.

         do i=1,7
            read(unit=i_dat,fmt="(a)") line
            if (i == 1) pat%title=trim(line)
            if (line(1:4) == "BANK") then
               bank_missed=.false.
               exit
            end if
         end do

         if (bank_missed) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: BANK not found!"
            close(unit=i_dat)
            return
         end if

      else
         read(unit=i_dat,fmt="(a)") line
      end if

      look_bank: do
         items=0
         previous=.false.
         do i=5,80   !This is the line with BANK
            if (line(i:i) /= " ") then
               if (.not. previous) then
                  items=items+1
                  pointi(items)=i
                  previous=.true.
               end if

            else
               if (items > 0 .and. previous) pointf(items)=i-1
               previous=.false.
            end if
         end do
         IF (items > 0) read(unit=line(pointi(1):pointf(1)),fmt=*) ibank

         if ( ibank /= nbank) then  !Verify that we have the proper bank
            do
               read(unit=i_dat,fmt="(a)",iostat=ier) line  !continue reading the file up to finding
               if (ier /= 0) then
                  Err_CFML%IErr=1
                  write(unit=Err_CFML%Msg,fmt="(a,i2,a)") "Read_Pattern_GSAS@DIFFPATT: BANK number: ",nbank," not found!"
                  close(unit=i_dat)
                  return
               end if
               if (line(1:4) == "BANK") then               !the good bank
                  cycle look_bank
               end if
            end do
         end if

         IF (items > 1) read(unit=line(pointi(2):pointf(2)),fmt=*) nchan
         IF (items > 2) read(unit=line(pointi(3):pointf(3)),fmt=*) nrec
         IF (items > 3) read(unit=line(pointi(4):pointf(4)),fmt="(a)") bintyp
         IF (items > 4) read(unit=line(pointi(5):pointf(5)),fmt=*) bcoef(1)
         IF (items > 5) read(unit=line(pointi(6):pointf(6)),fmt=*) bcoef(2)
         IF (items > 6) read(unit=line(pointi(7):pointf(7)),fmt=*) bcoef(3)
         IF (items > 7) read(unit=line(pointi(8):pointf(8)),fmt=*) bcoef(4)
         datyp="STD"
         IF (items > 8) read(unit=line(pointi(9):pointf(9)),fmt="(a)") datyp

         pat%npts=nchan
         if (pat%npts <= 0) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: Check your instr parameter!"
            close(unit=i_dat)
            return
         end if

         !> Allocating
         call Allocate_Pattern(pat)

         if(allocated(iww) ) deallocate(iww)
         allocate(iww(pat%npts))

         if (datyp == "STD") then
            !pat%ct_step  = .true.
            if (bintyp == "CONST") then
               pat%xmin=bcoef(1)/divi !divide by 100 for CW
               step=bcoef(2)/divi  !divide by 100 for CW
               pat%xmax=pat%xmin+(pat%npts-1)*step
            else
               Err_CFML%IErr=1
               Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: Only BINTYP=CONST is allowed for ESD data"
               close(unit=i_dat)
               return
            end if

            if (tof) then
               read(unit=i_dat,fmt="(10f8.0)", iostat=ier) (pat%y(i),i=1,pat%npts)
               if (ier /= 0) then
                  backspace (unit=i_dat)
               end if
               iww(1:pat%npts)=1
            else
               read(unit=i_dat,fmt="(10(i2,f6.0))", iostat=ier) (iww(i),pat%y(i),i=1,pat%npts)
               if (ier /= 0) then
                  backspace (unit=i_dat)
               end if
            end if

            do i=1,pat%npts
               !if (pat%y(i) <= 0.00001) pat%y(i) = 1.0
               if (iww(i) == 0) iww(i) = 1
               pat%sigma(i) = abs(pat%y(i))/real(iww(i))
               pat%x(i)=pat%xmin+(i-1)*step
            end do
            cnorm=1.0

         else if(datyp == "ESD") then
            if (bintyp == "CONST") then
               !pat%ct_step  = .true.
               pat%xmin=bcoef(1)/divi !divide by 100 for CW
               step=bcoef(2)/divi  !divide by 100 for CW
               pat%xmax=pat%xmin+(pat%npts-1)*step

               read(unit=i_dat,fmt="(10f8.0)",iostat=ier) (pat%y(i),pat%sigma(i),i=1,pat%npts)
               if (ier /= 0) then
                  backspace (unit=i_dat)
               end if
               cnorm=0.0
               do i=1,pat%npts
                  pat%x(i)=pat%xmin+(i-1)*step
                  pat%sigma(i)=pat%sigma(i)*pat%sigma(i)
                  cnorm=cnorm+pat%sigma(i)/max(pat%y(i),0.001_cp)
               end do
               cnorm=cnorm/real(pat%npts)
            else
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: Only BINTYP=CONST is allowed for ESD data"
              close(unit=i_dat)
              return
            end if

         else if(datyp == "ALT") then
            if (bintyp == "RALF") then
               !pat%ct_step  = .false.
               read(unit=i_dat,fmt="(4(f8.0,f7.0,f5.0))",iostat=ier)(pat%x(i),pat%y(i),pat%sigma(i),i=1,pat%npts)
               if (ier /= 0) then
                  backspace (unit=i_dat)
               end if
               pat%x=pat%x/32.0
               cnorm=0.0
               do i=1,pat%npts-1
                  divi=pat%x(i+1)-pat%x(i)
                  pat%y(i)=1000.0_cp*pat%y(i)/divi
                  pat%sigma(i)=1000.0_cp*pat%sigma(i)/divi
                  pat%sigma(i)=pat%sigma(i)*pat%sigma(i)
                  cnorm=cnorm+pat%sigma(i)/max(pat%y(i),0.001_cp)
               end do
               cnorm=cnorm/real(pat%npts)
               pat%npts=pat%npts-1
               pat%xmin=bcoef(1)/32.0_cp
               !pat%step=bcoef(2)/32.0
               pat%xmax=pat%x(pat%npts)

            else if(bintyp == "CONST") then
               !pat%ct_step  = .true.
               read(unit=i_dat,fmt="(4(f8.0,f7.0,f5.0))", iostat=ier)(pat%x(i),pat%y(i),pat%sigma(i),i=1,pat%npts)
               if (ier /= 0) then
                  backspace (unit=i_dat)
               end if
               pat%x=pat%x/32.0_cp
               cnorm=0.0_cp
               do i=1,pat%npts
                  pat%sigma(i)=pat%sigma(i)*pat%sigma(i)
                  cnorm=cnorm+pat%sigma(i)/max(pat%y(i),0.001_cp)
               end do
               cnorm=cnorm/real(pat%npts)
               pat%xmin=bcoef(1)
               !pat%step=bcoef(2)
               pat%xmax=pat%x(pat%npts)
            else
               Err_CFML%IErr=1
               Err_CFML%Msg="Read_Pattern_GSAS@DIFFPATT: Only BINTYP=RALF or CONST is allowed for ALT data"
               close(unit=i_dat)
            end if
         end if
         exit !we have finished reading the good bank
      end do look_bank

      pat%ymin=minval(pat%y(1:pat%npts))
      pat%ymax=maxval(pat%y(1:pat%npts))

      close(unit=i_dat)
   End Subroutine Read_Pattern_Gsas

End SubModule DiffP_ReadPatt_GSAS
