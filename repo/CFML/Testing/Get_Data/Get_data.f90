!!-------------------------------------------------------------
!!---- FullProf software
!!
!! @license   Copyright 2019, Juan Rodriguez Carvajal, Institut Laue-Langevin All rights reserved (see LICENSE)
!! @authors   Juan Rodriguez Carvajal (see AUTHORS)
!!
!!-------------------------------------------------------------
 !!----
 !!---- GET_DATA PROGRAM
 !!----
 !!----
 !!---- Program Get_Data_nxs
 !!----
 !!---- Authors: Juan Rodriguez-Carvajal (ILL)
 !!----          Nebil Ayape Katcho      (ILL)
 !!----          Javier Gonzalez-Platas  (ULL)
 !!----
 !!---- Date: 24/02/2023
 !!
Program Get_Data_nxs
!---- Use Modules ----!
  Use CFML_Diffpatt,   only: DiffPat_E_Type,Write_Pattern_FreeFormat,write_pattern_XYSig, &
                             Write_Pattern_INSTRM5
  Use CFML_Strings,    only: pack_string
  Use GetData_Globals
  use D2B_read_mod,    only: cfl_D2B_type
  use D2B_int_mod

  !---- Variables ----!
  implicit none

  logical, dimension(:), allocatable :: compressed
  logical, dimension(:), allocatable :: list
  logical                            :: existe, select_num, calibration,file_inst,&
                                         first, delete, wbuf, cfl_read,ok

  character(len=1024)                :: CmdLine, cm_line, line         ! Command line
  character(len=120)                 :: aux,aux_prev
  character(len=6)                   :: fnumor, machine

  integer, dimension(:), allocatable :: numor_add
  real,    dimension(:), allocatable :: temper
  integer                            :: NLong                 ! Length of the CmdLine
  integer                            :: i,j,k,n,ier,nr
  integer                            :: num,numdim,num1,num2
  integer                            :: NTemp, ibuf

  real                               :: t,tmin,tmax,t1,t2,ts

  type(DiffPat_E_Type)               :: Pat
  type(Calibration_Detector_Type)    :: Cal
  type(cfl_D2B_type)                 :: cfl
  !!!!!!!!!!!!!!!!!!!!!!!!!

  !>----------------------<
  !>---- Init Program ----<
  !>----------------------<

  call Set_Compress_Program()
  call Set_CFML_DEBUG(.true.)

  !> Command Line
  call Get_Command(Command=Cmdline,Length=nlong)
  call Cut_String(cmdline,nlong)   ! Delete the name of the program
  write(*,"(a)") "  => Input arguments: "//trim(cmdline)
  if (nlong <= 0) then
     call manual()
     call Stop_Program()
  end if

  call cpu_time(t1)
  n=index(cmdline,'>')
  if (n > 0) cmdline=cmdline(:n-1)

  file_inst=.false.
  cfl_read=.false.
  first=.true.
  machine=" "
  fileins=" "

  !> Option -fil?
  n=index(cmdline,'-fil')
  if (n > 0) then

     call Get_Command_Argument(2,fileins)
     fileins=trim(adjustl(fileins))

     if (len_trim(fileins) <= 0) then
        call error_message(' Need the name of the file for Instructions!!')
        call Stop_Program()
     end if

     inquire(file=trim(fileins),exist=existe)
     if (.not. existe) then
        call error_message(' File with instructions to run the program not found!')
        call Stop_Program()
     end if

     open(unit=i_tmp,file=trim(fileins),iostat=ier,action="read",position="rewind")
     if (ier /= 0) then
        call error_message(' Problems when try to open the file with instructions to run the program!')
        call Stop_Program()
     end if
     file_inst=.true.

  end if

  open(unit=i_log,file="get_data.log",action="write",status="replace")
  write(unit=*,fmt="(/,a,/)") " => PROGRAM  Get_Data_nxs running ... "
  write(unit=i_log,fmt="(/,a)") " --------------------------------------------- "
  write(unit=i_log,fmt="(a)")   "            PROGRAM  Get_Data_nxs              "
  write(unit=i_log,fmt="(a)")   "     Data retrival for ILL diffractometers     "
  write(unit=i_log,fmt="(a)")   "     (Version 1.6, JRC-ILL, February 2023)     "
  write(unit=i_log,fmt="(a,/)") " --------------------------------------------- "
  write(unit=i_log,fmt="(a)")   " => Provided arguments in the command line: "
  write(unit=i_log,fmt="(a)")   "   "//trim(cmdline)

  nr=0
  cm_line=" "
  if(file_inst) cmdline= " "
  ok=.false.
  do_ext: do         !general loop

        if(index(fileins,".cfl") /= 0) then
           cfl_read=.true.
           file_inst=.false.; wbuf=.false.
           do
              read(unit=i_tmp,fmt="(a)",iostat=ier) line
              if(ier /= 0) exit
              line=adjustl(L_case(line))
              if(line(1:6) == "instrm") then
                 machine=adjustl(trim(line(7:)))
                 ok=.true.
                 exit
              end if
           end do
        end if

        if(ok .and. trim(machine) == "d2b") then !Process the CFL file for D2B
           close (unit=i_tmp)
           call Integrate_D2B_data(fileins,machine,cfl,i_log)
           if(Err_CFML%Flag) then
             call error_message(Err_CFML%Msg)
             call Stop_Program()
           else
             exit do_ext
           end if
        end if

        if(file_inst) then
           read(unit=i_tmp,fmt='(a)',iostat=ier) cm_line
           nr=nr+1
           if (ier /= 0) then
              nr=nr-1
              if(first) then
                call error_message(' Problems reading the instructions to run the program!')
                close (unit=i_tmp)
                call Stop_Program()
              else
                cmdline= " "
              end if
           else
              cm_line=adjustl(cm_line)
              if(cm_line(1:1) == "!" .or. cm_line(1:1) == "#") then
                nr=nr-1
                cmdline= " "
              else
                i=index(cm_line,"&")
                if( i /= 0) then
                   cmdline=adjustl(trim(cmdline)//" "//trim(cm_line(1:i-1)))
                   cycle
                else
                   cmdline=adjustl(trim(cmdline)//" "//trim(cm_line))
                end if
              end if
           end if

           if(len_trim(cmdline) == 0) then
              write(unit=*,fmt="(a,i2)") " => Number of processed lines: ",nr
              write(unit=*,fmt="(a)")    " => No more lines in "//trim(fileins)//" to process"
              exit !no more lines in the file
           end if

        else if(cfl_read) then

           !Transform the instructions of the CFL file into commands to be processed
           call CFL_to_Commands(cfl,cmdline)
           write(*,"(a)") trim(cmdline)

        end if

        cmdline=trim(adjustl(cmdline))
        write(unit=i_log,fmt="(a)") " => Final command line: "
        write(unit=i_log,fmt="(a)") "   "//trim(cmdline)
       
        !write(unit=*,fmt="(a)") " => CmdLine: "//trim(cmdline)
        call cpu_time(ts)   !Start processing CmdLine and reading numors
        delete=.true.

        !> Processing of CmdLine
        call Process_CmdLine(Cmdline,ier)
        if (ier /= 0) then
           write(unit=*,fmt="(a)") " => ERROR: The input cmdline was: "//trim(Cmdline)
           write(unit=*,fmt="(a)") "    If you are using an input file with extension .cfl and are providing" 
           write(unit=*,fmt="(a)") "    commands of the form '-cmd ...', change the extension to .inp " 
           write(unit=*,fmt="(a)") "    Execute the get_data program without arguments to see the manual!"
           call Stop_Program()
        end if
        Cmdline=" " !deleting the line
        if(trim(pathdir) == "."//ops_sep) delete=.false.

        !> Numors Limit
        num1=minval(b_num(1:nb_num,:))
        num2=maxval(b_num(1:nb_num,:))

        !> Open files for Information
        if (info .and. first) then
           open(unit=i_inf,file="info_data_"//trim(inst)//".inf",status="replace",action="write")
           write(unit=i_log,fmt="(a)") " => Information file: "//"info_data_"//trim(inst)//".inf"
        end if

        first=.false.

        if (info) then
           if (yc) then
              write(unit=i_inf,fmt="(4(a,i6.6),a)")   "  INFORMATION ABOUT NUMORS ",num1," TO ",num2," OF CYCLE",icycle,&
                                               " AND YEAR ",iyear," FROM "//trim(u_case(inst))
              write(unit=i_inf,fmt="(a)")           "  ==================================================================="
           else
              write(unit=i_inf,fmt="(2(a,i6.6),a)")   "  INFORMATION ABOUT NUMORS ",num1," TO ",num2," FROM "//trim(u_case(inst))
              write(unit=i_inf,fmt="(a)")           "  =============================================================="
           end if
           write(unit=i_inf,fmt="(a)") " "
           if (powdat) then
              write(unit=i_inf,fmt="(a)") &
              "   Numor             Title                          Header"// &
              "                  Time(sec.)  Monitor    Tset    Treg  Tsample  Wavelength"
              write(unit=*,fmt="(a)") &
              "   Numor             Title                          Header"// &
              "                  Time(sec.)  Monitor    Tset    Treg  Tsample  Wavelength"
           end if
        end if

        !> Allocating Numors
        numdim=(num2-num1+1)/nstep

        if (powdat) then
           call Allocate_Numors(numdim,0,0,0,PNum)
        end if

        if (ERR_CFML%Flag) then
           if (info) call error_message(trim(ERR_CFML%Msg),i_inf)
           call error_message(trim(ERR_CFML%Msg))
           close(unit=i_inf)
           call Stop_Program()
        end if

        !> Reading Numors

        !> Flag for compressed numors
        if (allocated(compressed)) deallocate(compressed)
        allocate(compressed(numdim))
        compressed=.false.

        num=0
        do i=num1, num2, nstep
           select_num=.false.
           do j=1,nb_num
              if (i >= b_num(j,1) .and. i <= b_num(j,2)) then
                 select_num=.true.
                 exit
              end if
           end do
           if (.not. select_num) cycle

           if (skipnum) then
              !> skip multiples of step=|nstep| when nstep<0
              if (num > 0 .and. mod(num,nskip) == 0) cycle
           end if
           num=num+1

           !> Numor
           write(unit=fnumor,fmt='(i6.6)') i
           if(nxs) then
             line=trim(pathdir)//fnumor//".nxs"
           else
             line=trim(pathdir)//fnumor
           end if

           !> Exist?
           inquire(file=trim(line),exist=existe)
           if (.not. existe) then
              !> Maybe the numor is compressed
              inquire(file=trim(line)//'.Z',exist=existe)
              if (.not. existe) then
                 call error_message("The Numor "//trim(line)//" doesn't exist")
                 num=num-1
                 cycle
              end if

              compressed(num)=.true.

              !> Coping the file to the actual directory
              if (ops == 1) then       ! For Windows
                 call execute_command_line(trim(ProgCompress)//"  "//trim(line)//".Z  > nul")
              else
                 call execute_command_line(trim(ProgCompress)//"  "//trim(line)//".Z >"//fnumor)
              end if
              line=fnumor
           end if

           !> Reading
           if(nxs) then
           call read_nexus(trim(line),nxsf)
           if(err_nexus) then
              call error_message(err_nexus_mess)
              num=num-1
              cycle
           end if
           if(war_nexus) then
              call error_message(war_nexus_mess)
           end if
           !Convert nxsf to numor
              if (powdat) then
                 PNum(num) = nxs_to_powder_numor(nxsf)
              end if
           else
              if (powdat) then
                 call Read_Numor(trim(line),u_case(trim(inst)),PNum(num),info)
              end if
           end if

           !> Information only
           if (info) then
            if (powdat) then
              if (ERR_CFML%Flag) then
                 write(unit=i_inf,fmt="(i8,2(tr2,a))") PNum(num)%numor,PNum(num)%title, PNum(num)%header
                 write(unit=*,fmt="(i8,2(tr2,a))") PNum(num)%numor,PNum(num)%title, PNum(num)%header
              else
                 write(unit=i_inf,fmt="(i8,2(tr2,a),f8.2,f12.1,3f8.2,f12.5)") PNum(num)%numor,PNum(num)%title, &
                    PNum(num)%header, PNum(num)%time, PNum(num)%monitor, PNum(num)%conditions(1:3), PNum(num)%wave

                 write(unit=*,fmt="(i8,2(tr2,a),f8.2,f12.1,3f8.2,f12.5)") PNum(num)%numor,PNum(num)%title, &
                    PNum(num)%header, PNum(num)%time, PNum(num)%monitor, PNum(num)%conditions(1:3), PNum(num)%wave
              end if
            end if
           end if

           if (ERR_CFML%Flag) then  !Moved after writing information message
              call error_message(trim(ERR_CFML%Msg))
              num=num-1
              cycle
           end if

           !> Eliminating Files in the case of compressed numors provided that they are not
           !  in the current directory: pathdir="./" or ".\"
           if (compressed(num) .and. delete) then
              if (ops == 1) then
                 call execute_command_line("del /f /q "//fnumor//".* > nul")
              else
                 call execute_command_line("rm -f "//fnumor)
              end if
           end if

        end do

        if (info) then
           close(unit=i_inf)
           write(unit=*,fmt="(a)") " => The information about read Numors are in file: "//"info_data_"//trim(inst)//".inf"
           call Stop_Program()
        end if

        !>
        !> Adding list according to considerations of dT/n or not
        !>
        if(allocated(numor_add)) deallocate(numor_add)
        if(allocated(list)) deallocate(list)
        if(allocated(temper)) deallocate(temper)
        allocate(numor_add(num), list(num),temper(num))
        numor_add=0; temper=0.0
        list=.false.
        ntemp=0

        write(unit=i_log,fmt="(a,i6,a)") " => Allocated arrays for: ",num," numors"

        if (.not. add) then
           ntemp=num
           do i=1,num
              numor_add(i)=i
           end do

        else
           if (nsum > 0) then

              ntemp=num/nsum  !number of files to be created by adding nsum numors at a time
              n=1
              do i=1,num
                numor_add(i) = n
                if(mod(i,nsum) == 0)  n=n+1
              end do
              write(unit=i_log,fmt="(2(a,i6))") " => Number of files to be created by adding: ",nsum," numors at a time:",ntemp

           else if (dt > 0.0) then

              ! Adding numors by dT range
              if (powdat) then
                 tmin=minval(Pnum(1:num)%conditions(3))
                 tmax=maxval(Pnum(1:num)%conditions(3))
              end if

              write(unit=i_log,fmt="(/,a,f8.4,a,/)") " => Adding numors by dT range: ",dt," K"

              ntemp=1
              t=tmin
              tmax=tmax+2*dt
              if (abs(tmax-tmin) > dt) then
                 do while ( t+dt < tmax )
                    n=0
                    do i=1,num
                       if (powdat) then
                          if (abs(Pnum(i)%conditions(3)-t) < dt ) then
                             numor_add(i)=ntemp
                             temper(i)=Pnum(i)%conditions(3)
                             n=n+1
                          end if
                       end if
                    end do
                    t=t+2*dt
                    if (n /=0) ntemp=ntemp+1
                 end do
                 ntemp=ntemp-1
              else
                 numor_add=1
              end if

              k=0
              do i=num1,num2,nstep
                 k=k+1
                 write(unit=i_log, fmt='(a,i6.6,a,i5,a,f8.3)') "      Numor: ",i,"  Interval: ",&
                             numor_add(k), " Temperature(K): ",temper(k)
              end do

              if (any(numor_add == 0)) then
                 call error_message("Some Numors weren't selected to be added in a particular range")
                 write(unit=i_log, fmt='(/,a)') &
                 " => WARNING! Some Numors weren't selected to be added in a particular range (check the following list!)"
                 n=0
                 do i=num1,num2,nstep
                    n=n+1
                    write(unit=*, fmt='(a,i6.6,a,i5)')     "    Numor: ",i,"  Interval: ",numor_add(n)
                    write(unit=i_log, fmt='(a,i6.6,a,i5)') "    Numor: ",i,"  Interval: ",numor_add(n)
                 end do
                 call Stop_Program()
              end if

           else
              ! Add all numors at the same time
              ntemp=1
              numor_add=1
              write(unit=i_log, fmt='(a)') " => All Numors have been added in a single file"
           end if
        end if

        call cpu_time(t2)
        write(unit=*,fmt="(a,f8.2,a)") " => CPU-time for copying-reading data: ",t2-ts," seconds"
        write(unit=i_log,fmt="(/,a,f8.2,a,/)") " => CPU-time for copying-reading data: ",t2-ts," seconds"

        !> Ntemp contain the number of new files to create
        wbuf=.false.
        if(ntemp > 5) then !Create a buffer file if more than 5 files are to be written
          wbuf=.true.
          open(newunit=ibuf,file=trim(codfil)//".buf",status="replace",action="write")
          write(unit=i_log,fmt="(/,a,/)") " => The buffer file: "//trim(codfil)//".buf  has been created"
        end if
        aux_prev=" "
        do n=1,ntemp
           !>------------------<
           !>---- FileName ----<
           !>------------------<
           !> Name of the files including Temperature
           aux=" "
           if (labelT) then
              do i=1,num
                 if (numor_add(i) /= n) cycle
                 if (powdat) then
                    write(unit=aux,fmt="(a,i6.6,a,f6.1,a1)") trim(codfil)//"_",Pnum(i)%numor,"_",Pnum(i)%conditions(3),"K"
                 end if
                 k=index(aux,".",back=.true.)
                 aux(k:k)="p"
                 aux=pack_string(aux)
                 if(trim(aux) == trim(aux_prev)) then
                    aux=trim(aux)//"p"
                 end if
                 aux_prev=aux
                 exit
              end do

           !> all numors were added to only one
           else if (add) then
              write(unit=aux,fmt="(a,i4.4)") trim(codfil)//"_",n

           !> general case one file for each numor
           else
              do i=1,num
                 if (numor_add(i) /= n) cycle
                 if (powdat) then
                    write(unit=aux,fmt="(a,i6.6)") trim(codfil)//"_",Pnum(i)%numor
                 end if
                 exit
              end do
           end if

           !> Including detector if it is the case
           if (idet > 0) then
              write(unit=aux,fmt="(a,i3.3)") trim(aux)//"_Det_",idet
           end if

           ! Conversion
           if (Outconv) then
              select case (ifmt)
                 case (0,2)
                    aux=trim(aux)//'.dat'
                 case (1)
                    aux=trim(aux)//'.xys'
              end select
           else
              aux=trim(aux)//'.xys'
           end if

           !>--------------------------<
           !>---- Adding Procedure ----<
           !>--------------------------<
           list=.false.
           do i=1,num
              if (numor_add(i) == n) list(i)=.true.
           end do

           calibration=.false.
           machine=u_case(trim(inst))
           select case (trim(machine))

              case ('D1B','D20')
                 if (len_trim(calpath) > 0) then
                    if(.not. ascii_cal) then
                       call read_calibration(trim(CalPath),"mantid_workspace_1/workspace/values",machine,Cal)
                       calibration=.true.
                    else
                       call Read_Calibration_File(CalPath,trim(machine),Cal)
                       if (ERR_CFML%Flag) then
                          call error_message(trim(ERR_CFML%Msg))
                       else
                          write(unit=*,fmt="(a)") " => Calibration file: "//trim(CalPath)
                          write(unit=i_log,fmt="(/,a)") " => Calibration file: "//trim(CalPath)
                          calibration=.true.
                       end if
                    end if
                 end if
                 if(calibration) then
                 if (rnorm > 0.01) then
                    if(angcor) then
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal,angcor=angcor,thres=threshold)
                    else
                       if(perm) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal,perm=perm,thres=threshold)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal,thres=threshold)
                       end if
                    end if
                 else
                    if(angcor) then
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal,angcor=angcor,thres=threshold)
                    else
                       if(perm) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal,perm=perm,thres=threshold)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal,thres=threshold)
                       end if
                    end if
                 end if
                 else
                 if (rnorm > 0.01) then
                    call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,thres=threshold)
                 else
                    call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,thres=threshold)
                 end if
                 end if

              case ('D1A')
                 if (len_trim(calpath) > 0) then
                    call Read_Calibration_File(CalPath,'D1A',Cal)
                    if (ERR_CFML%Flag) then
                       call error_message(trim(ERR_CFML%Msg))
                    else
                       calibration=.true.
                    end if
                 end if

                 if (.not. calibration) then
                    if (rnorm > 0.01) then
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm)
                    else
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat)
                    end if
                 else
                    if (rnorm > 0.01) then
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm, Cal=Cal)
                    else
                       call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, Cal=Cal)
                    end if
                 end if

              case ('D2B')

              !D2B is treated separately

              case ('D4')
                 if (len_trim(calpath) > 0) then
                    if(.not. ascii_cal) then
                       call Read_Calibration(trim(CalPath),"mantid_workspace_1/workspace/values",machine,Cal)
                       calibration=.true.
                    else
                       call Read_Calibration_File(CalPath,'D4',Cal)
                       if (ERR_CFML%Flag) then
                          call error_message(trim(ERR_CFML%Msg))
                       else
                          calibration=.true.
                       end if
                    end if
                 end if

                 if (.not. calibration) then
                    if (rnorm > 0.01) then
                       if (idet > 0) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm, Detect=idet)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm)
                       end if
                    else
                       if (idet > 0) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, Detect=idet)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat)
                       end if
                    end if

                 else

                    if (rnorm > 0.01) then
                       if (idet > 0) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm, Detect=idet, Cal=Cal)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=rnorm,Cal=Cal)
                       end if
                    else
                       if (idet > 0) then
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, Detect=idet,Cal=Cal)
                       else
                          call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal)
                       end if
                    end if
                 end if

              case default
                 call info_message('Not yet implemented')
                 exit
           end select

           !>-----------------------<
           !>---- Output Format ----<
           !>-----------------------<
           if (ERR_CFML%Flag) then
              call error_message(trim(ERR_CFML%Msg))
           else
              write(unit=*,fmt="(a)") " => Writing file: "//trim(aux)
              write(unit=i_log,fmt="(a)") " => Writing file: "//trim(aux)
              if(wbuf) write(unit=ibuf,fmt="(a)") trim(aux)

              !Applying the zero shift before writing
              Pat%x=Pat%x+zero_shift
              !write(*,*) " => Zero_Shift: ",zero_shift
              if(.not. xmin_read) xang_min= Pat%xmin
              if(.not. xmax_read) xang_max= Pat%xmax

              select case (ifmt)
                 case (0)
                    call Write_Pattern_FreeFormat(trim(aux),Pat,excl_cel,xang_min,xang_max)
                 case (1)
                    call write_pattern_XYSig(trim(aux),Pat,excl_cel,xang_min,xang_max)
                 case (2)
                    call Write_Pattern_INSTRM5(trim(aux),Pat,excl_cel,xang_min,xang_max)
              end select
           end if

        end do ! Fin de cada fichero

        if(.not. file_inst) exit

  end do  do_ext  !process next instruction in file

  if(wbuf) close(unit=ibuf)
  call cpu_time(t2)
  write(unit=*,fmt="(/,a)")          " => Program Get_Data finished normally, look at the file: get_data.log for details"
  write(unit=*,fmt="(a,f8.2,a)")     " => Total CPU-time: ",t2-t1," seconds"
  write(unit=i_log,fmt="(/,a)")      " => Program Get_Data finished normally"
  write(unit=i_log,fmt="(a,f8.2,a)") " => Total CPU-time: ",t2-t1," seconds"
  close (unit=i_log)
  call Stop_Program()

  Contains

   Subroutine Stop_Program()
   character(len=1) :: key
   write(unit=*,fmt="(/,a)") " => Press <enter> to finish "
   read(unit=*,fmt="(a)") key
   stop
   End Subroutine Stop_Program

End Program get_data_nxs
