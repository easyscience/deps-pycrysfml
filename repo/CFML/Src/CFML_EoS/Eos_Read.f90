!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Read
   implicit none
   Contains

   !!----
   !!---- FUNCTION SET_XDATATYPES
   !!----
   !!----  returns array result xdatatypes(i)=1 if datatype i is present in dataset gdat
   !!----  if used=.true. then only = 1 if at least one datum of the type is used
   !!----
   !!---- Date: 18/02/2021
   !!
   Module Function Set_XdataTypes(Gdat, Used) Result(XdataTypes)
      !---- Arguments ----!
      type (EoS_Data_List_Type), intent(in) :: Gdat  ! the data list
      logical,                   intent(in) :: Used  ! .true. to request
      integer, dimension(0:N_DATA_TYPES)    :: XdataTypes

      !---- Local Variables ----!
      integer :: i

      !> Init
      xdatatypes=0

      if (used)then
         do i=1,gdat%n
            if (gdat%eosd(i)%iuse == 1)xdatatypes(gdat%eosd(i)%xtype) = 1
         end do

      else        ! all data, used or not
         do i=1,gdat%n
            xdatatypes(gdat%eosd(i)%xtype) = 1
         end do
      end if

   End Function Set_XdataTypes

   !!----
   !!---- READ_EOS_DATAFILE
   !!----    General routine to read data for Eos
   !!----
   !!---- 06/12/2018
   !!
   Module Subroutine Read_EoS_DataFile(FName, Dat)
      !---- Arguments ----!
      character(len=*),          intent(in)  :: fname   ! File name
      type (eos_data_list_type), intent(out) :: dat     ! data structure

      !---- Local Variables ----!
      character(len=255), dimension(:), allocatable :: flines
      character(len=255)                            :: line
      character(len=5)                              :: car
      character(len=1)                              :: Ts
      character(len=30), dimension(ncol_data_max)   :: dire

      integer                                       :: nldata,ndat, npos
      integer                                       :: i,j,kk,nl,nk,nlines,m,idatatype,nlines_datum
      integer                                       :: iv, inum
      integer, dimension(ncol_data_max)             :: ivet,iorden

      real(kind=cp), dimension(ncol_data_max)       :: vet,vetsd
      real(kind=cp), dimension(ncol_data_max)       :: rvet
      logical                                       :: esd_as_num

      !---- Data Files ----!
      character(len=512)                            :: filedat=' '
      integer                                       :: NCDat         ! Total columns for data
      integer, dimension(ncol_data_max)             :: IC_Dat        ! Which values are input - local copy

      !> Eos init
      call clear_error()

      !> Init
      dire=' '
      filedat=trim(fname)

      !> Number of lines
      nlines=number_lines(trim(filedat))
      if (nlines <= 0) then
         call set_error(1,"Impossible to read the file ")
         return
      end if

      !> Read lines
      if (allocated(flines)) deallocate(flines)
      allocate(flines(nlines))
      flines=' '
      call reading_lines(trim(filedat),nlines,flines)

      !> Replace tabs in all lines by blank character: moved here April 2024.
      !> Previously only applied to data lines
      do i=1,nlines
          call Get_Separator_pos(flines(i),char(9),ivet,iv)
          do j=1,iv
             flines(i)(ivet(j):ivet(j))=' '
          end do
      end do

      !> Title
      i=1
      j=nlines
      dat%title=''
      call Read_Key_Str(flines, i, j, 'Title', line,'#')
      if (len_trim(line) > 0) dat%title=trim(line)

      !> System
      i=1
      j=nlines
      dat%system=''
      call Read_Key_Str(flines, i, j, 'System', line,'#')
      if (len_trim(line) > 0) dat%system=trim(line)

      !> TScale
      Ts='K'
      i=1
      j=nlines
      call Read_Key_Str(flines, i, j, 'TScale', line,'#')
      if (len_trim(line) > 0) Ts=U_case(trim(adjustl(line)))

      !> PScale
      i=1
      j=nlines
      call Read_Key_Str(flines, i, j, 'PScale', line,'#')
      if (len_trim(line) > 0)then
          line=adjustl(U_case(line))
          j=len_trim(line)
          if(j-i > 15)j=15
          dat%Pscale_name=line(1:j)
      endif

      !> VScale
      i=1
      j=nlines
      call Read_Key_Str(flines, i, j, 'VScale', line,'#')
      if (len_trim(line) > 0)then
          line=adjustl(U_case(line))
          j=len_trim(line)
          if(j-i > 15)j=15
          dat%Vscale_name=line(1:j)
      endif

      !> LScale
      i=1
      j=nlines
      call Read_Key_Str(flines, i, j, 'LScale', line,'#')
      if (len_trim(line) > 0)then
          line=adjustl(U_case(line))
          j=len_trim(line)
          if(j-i > 15)j=15
          dat%Lscale_name=line(1:j)
      endif

      !> DataType: sets idatatype for temporary use
      i=1
      j=nlines
      idatatype=0   ! default
      call Read_Key_Str(flines, i, j, 'DataType', line,'#')
      if (len_trim(line) > 0) then
         line=U_case(trim(adjustl(line)))
         if (index(line,'MODUL') > 0) then         ! allows modulus and moduli !!
            if (index(line,'ADIA') > 0) then
               idatatype=2
            else
               idatatype=1
            end if
         end if
      end if

      !> Format
      i=1
      j=nlines
      call Read_Key_Str(flines, i, j, 'Format', line,'#')

      !> for compatability with eosfit v5, remove any commas
      !> from format directive before processing
      call Get_Separator_pos(line,',',ivet,iv)
      do m=1,iv
         line(ivet(m):ivet(m))=' '
      end do

      call get_words(line,dire,iv)
      if (iv <= 1) then
         call set_error(1,"No keywords found on format line in data file!")
         return
      end if

      nldata=i+1      ! Line where begin the data

      !> Is a number the first value on format line?
      !if 'yes' then we need to read dire(2:iv) and ncdat=iv-1
      !if 'no' then we need to read dire(1:iv) and ncdat=iv

      inum=0
      call get_num(dire(1),vet,ivet,m)
      if (m == 1) then
         inum=1
         ncdat=iv-1
      else
         ncdat=iv
      end if
      ic_dat=0
      ic_dat(1)=1     ! Set all data read in to active
      ic_dat(22)=1    ! Set all data read in to Group 1
      iorden=0

      do i=1,ncdat          ! this must loop from 1 to ncdat always so that iorden is filled correctly
         car=U_case(adjustl(dire(i+inum)))

         !> allow 'press, pressure, vol, volume, temp...' for full compatability with v5.2 old files
         if (index(car,'PRESS') /= 0)  car='P'
         if (index(car,'VOL')   /= 0)  car='V'
         if (index(car,'TEM')   /= 0)  car='T'
         if (index(car,'LIN')   /= 0)  car='A'      ! Linear - set it as 'a' in v7
         if (index(car,'SIGL')  /= 0)  car='SIGA'

         select case (trim(car))
            case ('T')
               ic_dat(4)=1
               iorden(i)=4
            case ('SIGT')
               ic_dat(5)=1
               iorden(i)=5
            case ('P')
               ic_dat(6)=1
               iorden(i)=6
            case ('SIGP')
               ic_dat(7)=1
               iorden(i)=7
            case ('V')
               ic_dat(8)=1
               iorden(i)=8
            case ('SIGV')
               ic_dat(9)=1
               iorden(i)=9
            case ('A')
               ic_dat(10)=1
               iorden(i)=10
            case ('SIGA')
               ic_dat(11)=1
               iorden(i)=11
            case ('B')
               ic_dat(12)=1
               iorden(i)=12
            case ('SIGB')
               ic_dat(13)=1
               iorden(i)=13
            case ('C')
               ic_dat(14)=1
               iorden(i)=14
            case ('SIGC')
               ic_dat(15)=1
               iorden(i)=15
            case ('ALPHA')
               ic_dat(16)=1
               iorden(i)=16
            case ('SIGAL')
               ic_dat(17)=1
               iorden(i)=17
            case ('BETA')
               ic_dat(18)=1
               iorden(i)=18
            case ('SIGBE')
               ic_dat(19)=1
               iorden(i)=19
            case ('GAMMA')
               ic_dat(20)=1
               iorden(i)=20
            case ('SIGGA')
               ic_dat(21)=1
               iorden(i)=21
         end select
      end do

      call get_num(dire(1),vet,ivet,iv)
      if (iv <=0) then
         nl=1
      else
         nl=ivet(1)
      end if

      !> Estimating Number of points
      ndat=0
      do j=nldata,nlines
         line=adjustl(flines(j))
         if (len_trim(line) <= 0) cycle
         if (line(1:1) == '#') cycle
         ndat=ndat+1
      end do
      ndat=ndat/nl
      if (ndat <=0) then
         call set_error(1,'Number of data points estimated in the data file was zero!')
         return
      end if

      !> Allocating data
      call allocate_eos_data_list(ndat,dat)

      !> Set a flag for expecting esd's as separate numbers
      esd_as_num=.false.        ! if esd's then format line implies in brackets
      if (sum(ic_dat(5:21:2)) > 0) esd_as_num=.true.

      !> reading the data starts here
      ndat=0
      nk=0
      rvet=0.0
      nlines_datum=0

      do i=nldata,nlines                ! loop over reading lines: nlines per datum
         write(unit=car,fmt='(i5)') i   ! store the line number in car for error messages
         car=adjustl(car)
         line=adjustl(flines(i))
         if (len_trim(line) <= 0 .or. line(1:1) =='#') cycle

         !> After "#" character is a comment
         npos=index(line,'#',back=.true.)
         if (npos > 0) line=line(:npos-1)

         !---- Data Points ----!
         !> Replace ',' by blank character
         call Get_Separator_pos(line,',',ivet,iv)
         do j=1,iv
            line(ivet(j):ivet(j))=' '
         end do

         !> Check for brackets indicating esd's consistent with format
         if (esd_as_num) then
            if (index(line,'(') > 0) then
               call set_error(1,'Format says esds as numbers but esd in bracket found on  line '//trim(car)//' of data file!')
               dat%n=ndat
               return
            end if
         end if

         !> Read the numbers on this line
         nlines_datum=nlines_datum+1
         call get_numstd(line,vet,vetsd,iv)
         if (iv == 0) then
            call set_error(1,'No values found on line '//trim(car)//' of data file!')
            dat%n=ndat
            return

         else if (iv > ncdat) then        ! This catches too many data when 1 line/datum
            call set_error(1,'Error reading data at line '//trim(car)//': Too many data items found')
            dat%n=ndat
            return
         end if

         do j=1,iv
            kk=nk+j
            if (iorden(kk) < 1) then ! This catches too many data when >1 line/datum
               call set_error(1,'Error reading data at line '//trim(car)//' or previous line: Too many data items found')
               dat%n=ndat
               return

            else if(iorden(kk) > ncol_data_max) then
               call set_error(1,'Error reading data at line '//trim(car)//':  Did you put sig in format line, but have sig in ()?')
               dat%n=ndat
               return
            end if
            rvet(iorden(kk))=vet(j)             ! if esd's listed as separate items, they are in vet

            select case (iorden(kk))            ! if esd's listed with () they are in vetsd
               case (4) ! T
                  if (vetsd(j) > 0.0)rvet(5)=vetsd(j)

               case (6) ! P
                  if (vetsd(j) > 0.0) rvet(7)=vetsd(j)

               case (8) ! V
                  if (vetsd(j) > 0.0) rvet(9)=vetsd(j)

               case (10) ! a
                  if (vetsd(j) > 0.0) rvet(11)=vetsd(j)

               case (12) ! b
                  if (vetsd(j) > 0.0) rvet(13)=vetsd(j)

               case (14) ! c
                  if (vetsd(j) > 0.0) rvet(15)=vetsd(j)

               case (16) ! alpha
                  if (vetsd(j) > 0.0) rvet(17)=vetsd(j)

               case (18) ! beta
                  if (vetsd(j) > 0.0) rvet(19)=vetsd(j)

               case (20)
                  if (vetsd(j) > 0.0) rvet(21)=vetsd(j)
            end select
         end do
         nk=nk+iv

         !> Test for the correct number of data found
         if (nlines_datum < nl) cycle         ! not read enough lines for this datum yet
         if (nk < ncdat) then
            call set_error(1,'Error reading data at line '//trim(car)//': Not enough data items found')
            dat%n=ndat
            return

         else if (nk > ncdat) then
            call set_error(1,'Error reading data at line '//trim(car)//': Too many data items found')
            dat%n=ndat
            return
         end if
         nlines_datum=0     ! re-init counter for neaxt datum

         !> Writting values on Type
         ndat=ndat+1
         dat%eosd(ndat)%iuse=1          ! Active data
         dat%eosd(ndat)%igrp(1)=1       ! Group 1
         dat%eosd(ndat)%xtype=idatatype ! Data type

         !> Convert to Kelvin
         select case (Ts)
            case ('C')
               dat%eosd(ndat)%t=rvet(4) + 273.15

            case ('F')
               dat%eosd(ndat)%t=(rvet(4) + 459.67)/1.8

            case default
               dat%eosd(ndat)%t=rvet(4)
         end select

         dat%eosd(ndat)%p=rvet(6)
         dat%eosd(ndat)%v=rvet(8)
         dat%eosd(ndat)%cell=[rvet(10),rvet(12),rvet(14)]
         dat%eosd(ndat)%ang =[rvet(16),rvet(18),rvet(20)]
         dat%eosd(ndat)%sigt=rvet(5)
         dat%eosd(ndat)%sigp=rvet(7)
         dat%eosd(ndat)%sigv=rvet(9)
         dat%eosd(ndat)%sigc=[rvet(11),rvet(13),rvet(15)]
         dat%eosd(ndat)%siga=[rvet(17),rvet(19),rvet(21)]

         !> New 02/08/2013 to set flags for esd's when they were provided in ()
         do j=1,21,2
            if (rvet(j) > 0.0)ic_dat(j)=1
         end do

         nk=0
         rvet=0.0
      end do

      dat%n=ndat
      dat%ic_dat=ic_dat         ! flags for original input

      !> Default values in function of system
      call Def_Crystal_System(dat)

      ! Do not test for error here because we can still set the volume

      !> Volume calculation if have cell parameters
      call Set_Volume_from_Cell(dat)

   End Subroutine Read_EoS_DataFile

   !!----
   !!---- READ_EOS_FILE
   !!----    General routine a single Eos from a file
   !!----
   !!---- Created 12/10/2015 to allow for files with more than eos
   !!---- and return the first eos in the file.
   !!----
   !!---- 31/05/2019
   !!
   Module Subroutine Read_Eos_File(Fname,Eos)
      !---- Arguments ----!
      character(len=*),intent(in)  :: fname  ! File name
      type (EoS_Type), intent(out) :: Eos    ! EoS object

      !---- Local Variables ----!
      type (EoS_List_Type)  :: Eoslist    ! EoS list object


      call Read_Multiple_Eos_File(Fname,Eoslist)

      if (eoslist%n < 1) then
         call set_error(1,"No EoS found in EoS file ")
      else
         EoS=Eoslist%eos(1)
      end if
   End Subroutine Read_Eos_File

   !!--++
   !!--++ READ_EOS_IN
   !!--++
   !!--++ PRIVATE
   !!--++ General routine to read Eos from a file
   !!--++
   !!--++ 12/10/2015
   !!
   Module Subroutine Read_Eos_In(Flines,Eos)
      !---- Arguments ----!
      character(len=*),dimension(:),intent(in)   :: flines
      type(Eos_type),               intent(out)  :: eos

      !---- Local Variables ----!
      integer                   :: nl, imax,ierr,idoc,nlines,i,c,j, jc,kc
      character(len=255)        :: text,wtext
      character(len=10)         :: forma
      real                      :: val

      !> initialisation
      call init_eos_type(eos)

      nl=0
      imax=0
      ierr=0
      idoc=0                      ! local counter
      nlines=size(flines)

      do
         nl=nl+1
         if (nl > nlines) exit
         text=adjustl(flines(nl))
         if (len_trim(text) <=0) cycle   ! blank line
         if (text(1:1) == '!') cycle     ! comment line
         if (ierr /=0) exit

         c=index(text,'=')+1             !  1 place after the =
         text(1:c-1)=U_case(text(1:c-1)) ! set keyword in caps

         if (index(text,'TITLE') /= 0) then
            eos%title=trim(text(c:))

         else if(index(text,'SAVEDATE') /= 0)then
            eos%savedate=trim(text(c:))

         else if(index(text,'COMMENT') /= 0)then
            idoc=idoc+1
            if(idoc <= size(eos%doc))eos%doc(idoc)=trim(text(c:))

         else if(index(text,'SYSTEM') /= 0)then
            eos%system=trim(adjustl(text(c:)))

         else if(index(text,'MODEL') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%imodel
            if (ierr /=0) call set_error(1,"Error reading the EoS Model number")

         else if(index(text,'ORDER') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%iorder
            if (ierr /=0) call set_error(1,"Error reading the EoS Order number")

         else if(index(text,'THERMAL') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%itherm
            if (ierr /=0) call set_error(1,"Error reading the EoS Thermal model")

         else if(index(text,'CROSS') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%icross
            if (ierr /=0) call set_error(1,"Error reading the EoS Cross-terms model")

         else if(index(text,'TRANS') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%itran
            if (ierr /=0) call set_error(1,"Error reading the EoS Transition model")

         else if(index(text,'SHEAR') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%ishear
            if (ierr /=0) call set_error(1,"Error reading the EoS Shear model")

         else if(index(text,'OSC2') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%iosc(1)
            if (ierr /=0) call set_error(1,"Error reading the Oscillator2 model")

         else if(index(text,'OSC3') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%iosc(2)
            if (ierr /=0) call set_error(1,"Error reading the Oscillator3 model")

         else if(index(text,'PSCALE') /= 0)then
            read(text(c:),'(a)',iostat=ierr)eos%pscale_name
            if (ierr /=0) call set_error(1,"Error reading the Pressure Scale info")

         else if(index(text,'VSCALE') /= 0)then
            read(text(c:),'(a)',iostat=ierr)eos%vscale_name
            if (ierr /=0) call set_error(1,"Error reading the Volume Scale info")

         else if(index(text,'TYPE') /= 0)then
            if(index(U_case(text),'LINEAR') /= 0) eos%linear=.true.

         else if(index(text,'DIRECTION') /= 0)then
            read(text(c:),'(a)',iostat=ierr) eos%LinearDir
            if (ierr /=0) call set_error(1,"Error reading the direction info")

         else if(index(text,'PREF') /= 0)then
            read(text(c:),'(f10.0)',iostat=ierr)eos%pref
            if (ierr /=0) call set_error(1,"Error reading the EoS Pressure reference")

         else if(index(text,'TREF') /= 0)then
            read(text(c:),'(f10.0)',iostat=ierr)eos%tref
            if (ierr /=0) call set_error(1,"Error reading the EoS Temperature reference")

         else if(index(text,'STOICH') /= 0)then
            read(text(c:),'(f10.0)',iostat=ierr)eos%stoich
            if (ierr /=0) call set_error(1,"Error reading the Stochiometry")

         else if(index(text,'DENSITY0') /= 0)then
            read(text(c:),'(f10.0)',iostat=ierr)eos%density0
            if (ierr /=0) call set_error(1,"Error reading the reference density")
            if(eos%density0 < 0.00005)eos%density0=0.0_cp                   ! test against min value allowed by format

         else if(index(text,'ANGLES') /= 0)then
            read(text(c:),'(i5)',iostat=ierr)eos%iangle
            if (ierr /=0) call set_error(1,"Error reading the Angle Model number")

         else if(index(text,'PARAM') /= 0)then
            if (index(U_case(text(c:)),'INF')> 0)then
               read(text(c:),'(i2)',iostat=ierr)i
               val=huge(0._cp)
            else
               read(text(c:),'(i2,f12.6)',iostat=ierr)i,val
               if (i > 50 .and. i < 60)then  !read scale factor name
                  jc=index(U_case(text),',')+1
                  kc=index(U_case(text),')')-1
                  if (jc > 1 .and. kc > jc)eos%comment(i)=trim(adjustl(text(jc:kc)))
               end if
            end if
            if (ierr /=0) call set_error(1,"Error reading the EoS Parameters")

            if (i > 0 .and. i <= N_EOSPAR) then
               eos%params(i)=val
               imax=max(imax,i)   ! use this for vcv reading: allows reading of old files when n_eospar is increased
            end if

         else if(index(text,'VARIANCE') /= 0)then
            ierr=0
            forma="(   e12.5)"
            write(unit=forma(2:4),fmt="(i3)") imax
            do i=1,imax
               ! The variable format <> is a VAX extension that is taken into account by Intel Fortran
               ! however Gfortran does not support it. It is better to use a general variable for writing
               ! dynamically whatever kind of format.

               !read(unit=flines(nl+i),fmt='(<imax>e12.5)',iostat=ierr)eos%vcv(i,1:imax)
               read(unit=flines(nl+i),fmt=forma,iostat=ierr) eos%vcv(i,1:imax)
               if (ierr /= 0)exit
            end do
            if (ierr /= 0)then
               call set_error(1,"Error reading the EoS Variance information")
               exit
            end if

         else if(index(text,'CAPACITY') /= 0)then
            !>New section to read heat capcity data
            ierr=0
            do i=nl+1,nlines
               if (len_trim(flines(i)) == 0)exit
            end do

            imax=i-nl-1
            if (allocated(eos%cv_table))deallocate(eos%cv_table)
            allocate(eos%cv_table(imax,3))

            if (imax > 1) eos%cv_external=.true.

            do i=1,imax
               read(unit=flines(nl+i),fmt='(2f10.0)',iostat=ierr)eos%cv_table(i,1:2)
               if (ierr /= 0)then
                  eos%cv_external=.false.
                  call set_error(1,"Error reading the Heat Capacity information from eos file")
                  exit

               else
                  if (i == 1)then
                     eos%cv_table(i,3)=eos%cv_table(i,2)

                  else
                     eos%cv_table(i,3)=eos%cv_table(i-1,3)+(eos%cv_table(i,1)-eos%cv_table(i-1,1))*(eos%cv_table(i,2)+eos%cv_table(i-1,2))/2.0
                  end if
               end if
            end do
            nl=nl+i

         end if
      end do

      !> Error during reading
      if (ierr /=0) return

      if (eos%iangle > 0 .and. eos%imodel+eos%itherm+eos%itran+eos%icross+eos%ishear /=0) then
         call set_error(1,"EoS file contains both angle and eos models: not allowed")
         return
      end if

      !> Do stuff to allow for old files made prior to Nov 2016 not having icross:
      if (eos%icross == 0 .and. abs(eos%params(8)) > 0.000001_cp) eos%icross=1
          !>0.000001 is the smallest non-zero number in the eos file format
          !>Old files cannot be icross=2, only =1

      !> Trap move of cross terms from params(5:6) to params(8:9) in Nov 2019
      !> old files will return icross > 0 but params(5) /= 0, while params(8:9) = 0
      !> new files wil have params(8) and/or (9) non zero
      if (eos%icross > 0 .and. abs(eos%params(8)) < 0.000001_cp .and. eos%imodel /=6)then
         eos%params(8:9) = eos%params(5:6)
         eos%params(5:6) = 0._cp
         eos%vcv(8:9,1:imax)=eos%vcv(5:6,1:imax)
         eos%vcv(1:imax,8:9)=eos%vcv(1:imax,5:6)
         eos%vcv(5:6,1:imax)=0._cp
         eos%vcv(1:imax,5:6)=0._cp

         wtext="EoS file in old format for cross terms: check parameter values carefully"
         call set_error(-1,trim(wtext))
      end if

      !> Trap move of Z for APL from params(4) to (5): do it after icross so no conflict with cross terms
      if (eos%imodel == 6 .and. abs(eos%params(5)) < 0.000001_cp)then
         eos%params(5) = eos%params(4)
         eos%params(4) = 0._cp     ! Kpp0 will be reset by implied values
         eos%vcv(5,1:imax)=eos%vcv(4,1:imax)
         eos%vcv(1:imax,5)=eos%vcv(1:imax,4)
         eos%vcv(4,1:imax)=0._cp
         eos%vcv(1:imax,4)=0._cp

         Wtext=trim(Wtext)//"  EoS file in old format for APL EoS: check parameter values carefully"
         call set_error(-1,trim(wtext))
      end if

      !> Move angle polynomial values from params(1:30) and clear params(1:30)
      if (eos%iangle > 0)then
         do i=1,3
            j=10*i-9        ! j=1,11,21
            eos%angpoly(i,0,1)=eos%params(j)
            eos%angpoly(i,1,1:3)=eos%params(j+1:j+3)      !P terms
            eos%angpoly(i,2,1:3)=eos%params(j+4:j+6)      !T terms
            eos%angpoly(i,3,1:3)=eos%params(j+7:j+9)      !PT terms
         end do
         eos%params(1:30)=0._cp
      end if

      !>Trap Natom=0 in HP thermal pressure model
      if(eos%itherm == 6 .and. eos%params(13) == 0)then
         eos%params(13)=1.0_cp
      endif

      !>Warn if extra oscillators
      if (sum(eos%iosc) > 0) then
         Wtext=trim(Wtext)//"  Extra oscillators in eos file. These are not supported in this version"
         call set_error(-1,trim(wtext))
      endif

      !> Now finish setting the other eos components
      call set_eos_names(eos)
      call set_thermal_names(eos)
      call Set_Transition_Names(eos)
      call Set_Shear_Names(eos)
      call Set_Cross_Names(eos)
      call Set_Osc_Names(eos)
      call Set_EoS_Use(eos)
      call set_eos_factors(eos)           ! sets the eos factors without resetting param values

      eos%params=eos%params/eos%factor    ! rescale the values
      do i=1,n_eospar                     ! rescale the vcv matrix
         do j=1,n_eospar
            eos%vcv(i,j)=eos%vcv(i,j)/eos%factor(i)/eos%factor(j)
         end do
         eos%esd(i)=sqrt(eos%vcv(i,i))   ! set the esd's from the vcv
      end do

      call Set_EoS_Implied_Values(eos)           ! set implied values

      if (err_CFML%IErr < 0)then
         err_CFML%Msg=trim(err_CFML%Msg)//' '//trim(wtext)
      endif

      !> we have to also set the refinement flags to match vcv
      do i=1,n_eospar
         if (abs(eos%vcv(i,i)) > tiny(0.0)) eos%iref(i)=1
      end do

   End Subroutine Read_Eos_In

   !!----
   !!---- READ_MULTIPLE_EOS_FILE
   !!----    General routine to read Eos from a file
   !!----
   !!---- Created 12/10/2015 to read files with more than eos
   !!----
   !!---- 31/05/2019
   !!
   Module Subroutine Read_Multiple_Eos_File(Fname,Eoslist)
      !---- Arguments ----!
      character(len=*),     intent(in)  :: fname      ! File name
      type (EoS_List_Type), intent(out) :: Eoslist    ! EoS list object

      !---- Variables ----!
      type (EoS_Type)                               :: Eos    ! EoS  object
      character(len=255), dimension(:), allocatable :: flines
      character(len=512)                            :: filedat

      integer                                       :: nlines,neos,i
      integer,dimension(10)                         :: istart


      !> Eos init
      call clear_error()

      !> Init
      filedat=' '
      filedat=trim(fname)
      istart=0

      !> Number of lines
      nlines=number_lines (trim(filedat))
      if (nlines <= 0) then
         call set_error(1,"Impossible to read the EoS file ")
         return
      end if

      !> Read lines
      if (allocated(flines)) deallocate(flines)
      allocate(flines(nlines))
      flines=' '
      call reading_lines(trim(filedat),nlines,flines)

      !> Find how many eos in file, and split up flines
      neos=0
      do i=1,nlines
         if (index(U_case(flines(i)),'EOSFIT PARAMETER FILE') > 0) then      ! Use this because it is generated by cfml_eos and not the header
            neos=neos+1
            istart(neos)=i          ! line number of TITLE
         end if
      end do
      istart(neos+1)=nlines+1             ! last line number+1

      !> Set default pointers for one eos if this line not detected in file:
      if (neos == 0) then
         neos=1
         istart(1)=1
         istart(2)=nlines+1
      end if

      call Allocate_EoS_List(neos, eoslist)

      do i=1,neos
         call read_eos_in(flines(istart(i):istart(i+1)-1),eos)
         eoslist%eos(i)=eos
      end do
   End Subroutine Read_Multiple_Eos_File

End SubModule EoS_Read
