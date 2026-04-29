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
 !!---- Module: Main_Parameters
 !!----
 !!---- Define the main variables for this program
 !!----
 !!---- Subroutines:
 !!----      Process_CmdLine
 !!----      Manual
 !!----      Set_Compress_Program
 !!----      Visualization_D2B
 !!----      Image2D_D2B
 !!----      Adding_Numors_D2B_DiffPattern
 !!----      Construct_Zhkl
 !!----
 !!---- 19/03/2011
 !!
 Module GetData_Global_Parameters
    !---- Use Modules ----!
    Use CFML_GlobalDeps,      only: ops,ops_sep, Directory_Exists
    Use CFML_Messages,        only: error_message,info_message
    Use CFML_Maths,           only: second_derivative,Spline_Interpol ,locate
    Use CFML_ILL_Instrm_Data, only: Calibration_Detector_Type,POWDER_Numor_type,SXTAL_Numor_type, &
                                    Set_Instrm_Directory,Allocate_Numors,ERR_ILLData,ERR_ILLData_Mess, &
                                    Read_Numor,PowderNumors_To_DiffPattern,Read_Calibration_File,  &
                                    Set_Current_Orient,init_err_illdata,SXTAL_Orient_type, Current_Orient,&
                                    Initialize_Numor
    Use CFML_DiffPatt,        only: DiffPat_E_Type,Allocate_Pattern
    Use CFML_Strings,         only: get_num, cut_string, l_case,u_case, Get_Separator_Pos
    Use CFML_SXTAL_Geom,      only: Get_z1_from_pixel, Set_PSD, cell_fr_UB, PSD
    Use Nexus_Mod,            only: read_nexus, nexus_type,err_nexus, war_nexus,err_nexus_mess, war_nexus_mess, &
                                    nxs_to_powder_numor, read_calibration

    !---- Variables ----!
    implicit none
    public

    !> Logicals
    logical                                           :: nxs                   ! True is nexus files are to be reaad
    logical                                           :: Verbose               ! Verbose flag
    logical                                           :: Powdat                ! Flag for Powder or SC
    logical                                           :: Skipnum               ! Skipping Numors
    logical                                           :: Yc                    ! Year-Cycle flag
    logical                                           :: Info                  ! Information flag
    logical                                           :: Add                   ! Add Temperature
    logical                                           :: LabelT                ! Add Temperature at Files name
    logical                                           :: Outconv               ! Output Data File conversion
    logical                                           :: UB_read=.false.       ! The UB matrix is read from file if .true.
    logical                                           :: wav_read=.false.      ! The UB matrix is read from file if .true.
    logical                                           :: angcor=.false.        ! The calibration positions of the cells are used or not in a banana detector
    logical                                           :: perm=.false.          ! The permutation for the alphas of the cells are used or not in a banana detector
    logical                                           :: xmin_read=.false.     ! The minimum angle for output has been read if true.
    logical                                           :: xmax_read=.false.     ! The maximum angle for output has been read if true.

    !> Character
    character(len=1024)                               :: PathDir               ! Path for the Numor
    character(len=1024)                               :: FileIns               ! Instruction file
    character(len=1024)                               :: CalPath               ! Calibration File (+Path)
    character(len=40)                                 :: CodFil                ! CodeFile
    character(len=4)                                  :: Inst                  ! Instrument Name
    character(len=80)                                 :: ProgCompress          ! Compress/Uncompress Program
    character(len=80)                                 :: ub_file
    !> Integer
    integer, parameter                                :: i_tmp=1
    integer, parameter                                :: i_inf=2
    integer, parameter                                :: i_log=3
    integer, parameter                                :: max_cell=6400

    integer                                           :: nb_num
    integer, dimension(5,2)                           :: b_num
    integer                                           :: nstep,nskip ! Initial, Final, Step, Skipping Numors
    integer                                           :: iyear, icycle         ! Year (4 digits) Cycle (1 digit)
    integer                                           :: ifmt                  ! 0 Instrm=0 (.dat), 1 XYs File (.xys),2 instrm=5
    integer                                           :: idet                  ! Number of Detector to print
    integer                                           :: nsum=0                ! Number of sumors to be summed

    !> Real
    real                                              :: dt                    ! Delta Temperature
    real                                              :: rnorm                 ! Monitor (or time) value
    real                                              :: valnorm               ! -rnorm (used for time normalization, D20)
    real                                              :: per=100.0             ! Percentage of central cells contributing
    real                                              :: zero_shift=0.0        ! Global Zero Shift
    real                                              :: Read_wav=0.0
    real                                              :: xang_min=0.0, xang_max=0.0
    real, dimension(3,3)                              :: Read_UB=0.0
    logical, dimension(128)                           :: excl_det=.false.      ! excl_det(i)=.true.  if detector i is to be excluded
    logical, dimension(max_cell)                      :: excl_cel=.false.      ! excl_cel(i)=.true.  if cell i is to be excluded
    integer                                           :: nexcl = 0             ! Number of detectors to be excluded
    integer                                           :: nexcel = 0            ! Number of cells to be excluded (Banana detector, D1B-D20)
    !> Type
    type(POWDER_Numor_type), dimension(:),allocatable :: PNum                  ! Numors for Powder Instrument
    type(SXTAL_Numor_type),  dimension(:),allocatable :: SNum                  ! Numors for SC Instrument
    type(nexus_type) :: nxsf

    Contains

    !!----
    !!---- Subroutine Set_Compress_Program(Fileprog)
    !!----
    !!---- Set Default Program for Compress/Uncompress files
    !!----
    !!---- 19/03/2011
    !!
    Subroutine Set_Compress_Program(FileProg)
        !---- Arguments ----!
        character(len=*), optional, intent(in) :: FileProg

        !---- Local Variables ----!

        if (present(FileProg)) then
           ProgCompress=trim(FileProg)
        else
           if (ops == 1) then
              ProgCompress='7z e -y'          ! Opcion -o directorio_salida
           else
              ProgCompress='gunzip -c'       ! > directorio_salida
           end if
        end if

        return
    End Subroutine Set_Compress_Program

 End Module GetData_Global_Parameters

 Module GetData_General_Procedures
    !---- Use Modules ----!
    Use GetData_Global_Parameters

  Contains

    !!----
    !!---- Subroutine Manual()
    !!----
    !!---- Interactive instructions for using this Program
    !!----
    !!---- 17/03/2011
    !!
    Subroutine Manual()

      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  GET_DATA PROGRAM"
      write(unit=*,fmt="(a)") "  ================"
      write(unit=*,fmt="(a)") "  The program GET_DATA can be used for retrieving information on the ILL DataBase or for"
      write(unit=*,fmt="(a)") "  creating corrected files from powder diffraction instruments (D1A, D1B, D2B,...)"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  The invocation of the program without arguments produces this information."
      write(unit=*,fmt="(a)") "  The minimal information for running the program is to provide as arguments the name of"
      write(unit=*,fmt="(a)") "  the instrument and the numors."
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  List of possible arguments"
      write(unit=*,fmt="(a)") "  If '-fil myfile' is provided as argument, the arguments containing the instructions"
      write(unit=*,fmt="(a)") "  are written in myfile. In such a case the list of arguments can be extended over"
      write(unit=*,fmt="(a)") "  several lines if each line ends with an ampersand '&', which is considered as a"
      write(unit=*,fmt="(a)") "  continuation character. The arguments then finish with the first line without &."
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  Mandatory arguments"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  -ins   instrument_name     : Instrument name (D1B, D1A,...)"
      write(unit=*,fmt="(a)") "  -num   num1  [num2 [step]] : Numors from num1 to num2 every step numors. "
      write(unit=*,fmt="(a)") "                               If step < 0 the multiples of |nstep| are skipped"
      write(unit=*,fmt="(a)") "         n1 n2 ... [n9 n10]  : Up to 5 blocks of range in Numors (2 blocks is the minimum)"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  -dir   pathdir             : The program search the data in directory 'pathdir' "
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  (Optionals):"
      write(unit=*,fmt="(a)") "  -nxs                       : Tells the program to use NeXus files of the form nnnnnn.nxs"
      write(unit=*,fmt="(a)") "  -add                       : Adds all Numors"
      write(unit=*,fmt="(a)") "  -add   [N n ]              : Adds blocks of n Numors between num1 and num2"
      write(unit=*,fmt="(a)") "  -add   [T dT]              : Adds Numors with temperatures within a range of +/- dT "
      write(unit=*,fmt="(a)") "  -cal   filename            : File name of calibration data for the instrument (including path)"
      write(unit=*,fmt="(a)") "  -zer   zero_shift          : Zero-shift to be applied before writing the data"
      write(unit=*,fmt="(a)") "  -xmin  value               : Minimum 2theta angle for output"
      write(unit=*,fmt="(a)") "  -xmax  value               : Maximum 2theta angle for output"
      write(unit=*,fmt="(a)") "  -ang                       : Angular positions of calibration file are used for a banana detector"
      write(unit=*,fmt="(a)") "  -prm                       : Permutation of alphas according to angular positions is used for a banana detector"
      write(unit=*,fmt="(a)") "  -cod   Codefiles           : Prepends the content of 'codefiles' to the name of the files "
      write(unit=*,fmt="(a)") "                             : If not given, the instrument name is used for naming the files"
      write(unit=*,fmt="(a)") "  -det   n                   : Create a file with the scan measured by detector n"
      write(unit=*,fmt="(a)") "  -fil   file_instructions   : File containing the directives for run get_data program"
      write(unit=*,fmt="(a)") "  -fmt   0|1|2               : 0 Convert the Numors to INSTR=0 "
      write(unit=*,fmt="(a)") "                             : 1 Convert the Numors to XYSigma Format (this is the default value)"
      write(unit=*,fmt="(a)") "                             : 2 Convert the Numors to INSTR=5 Format (appropriate for D1B/D20)"
      write(unit=*,fmt="(a)") "  -inf                       : The program reads only the headers and produces a file called "
      write(unit=*,fmt="(a)") "                             : 'info_data_inst.inf', listing relevant info on the given numors"
      write(unit=*,fmt="(a)") "  -lab   T                   : Appends the Temperature(in 0.01K) to the name of the files "
      write(unit=*,fmt="(a)") "  -nor   value               : Normalization value for Monitor normalization"
      write(unit=*,fmt="(a)") "  -tor   value               : Normalization value for Time normalization"
      write(unit=*,fmt="(a)") "  -dex   n_1  n_2 ...nexcl   : Identification numbers of detectors to be excluded "
      write(unit=*,fmt="(a)") "  -cex   n_1  n_2 ...nexcel  : Identification numbers of cells to be excluded (banana detector)"
      write(unit=*,fmt="(a)") "  -per   value               : Percentage of vertical cells to add for integration (D2B)"

      write(unit=*,fmt="(a)") "                             : (Default Value = 100, then all the 128 cells contribute)"
      write(unit=*,fmt="(a)") "                             : Example: -per 31.25 means 128*0.3125=40 central cells contribute"
      write(unit=*,fmt="(a)") "  -yc    year cycle          : Year (4 digits) and cycle(1-4) integer numbers (needs ILL DataBase)"
      write(unit=*,fmt="(a)") "  -mat  file_name            : The UB matrix (SXtals) is read from file file_name instead of using"
      write(unit=*,fmt="(a)") "                             : that of the NUMOR. If file_name is not given, 'ubfrom.raf' is assumed"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  Examples of use of this program:"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  get_data  -ins d1b -dir c:\bd_ill\data\d1b  -num 12341 12387 -cod ymn2 -add -fmt 0"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  get_data  -ins d2b -dir \\Serdon\illdata\111\d2b  -num 85544 85553 -cod test -add -fmt 1"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  get_data  -fil run_d20"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  The content of the file 'run_d20' in the last example is formed by the four lines below:"
      write(unit=*,fmt="(a)") ' -ins D20 -dir "C:\Disk-D\Data\xxx\yyyy\D20" -num 897965  897972 -add &    '
      write(unit=*,fmt="(a)") ' -fmt 1 -cod "Feoxal"  -tor 60.0 -zer 2.53       &                         '
      write(unit=*,fmt="(a)") ' -cal "C:\Disk-D\Data\xxx\yyyy\D20\calib_d20.cal"      &                   '
      write(unit=*,fmt="(a)") ' -cex 867 868 869 870 871 872   2543 2544 2545 2546 2547 2548 2549 2550 2551  2552 '
      write(unit=*,fmt="(a)") "  "

    End Subroutine Manual

    !!----
    !!---- Subroutine Process_CmdLine(Cmd, IError)
    !!----    character(len=*), intent(in)  :: Cmd     ! Command Line
    !!----    integer,          intent(out) :: IError  ! Error Code
    !!----
    !!---- 18/03/2011
    !!
    Subroutine Process_CmdLine(Cmd, IError)
        !---- Arguments ----!
        character(len=*),     intent(in) :: Cmd
        integer,              intent(out):: IError

        !---- Local Variables ----!
        logical                 :: ierr,existe

        character(len=1024)     :: line
        character(len=10)       :: dire
        character(len=1)        :: car

        integer, dimension(50)  :: ipos
        integer, dimension(100) :: ivet
        integer                 :: i,j,k,na,iv,n,nlong, ier

        real,    dimension(100) :: vet


        !> Init
        IError=0
        nxs=.false.
        verbose=.false.
        powdat=.true.
        skipnum=.false.
        yc=.false.
        info=.false.
        add=.false.
        labelT=.false.
        outconv=.false.

        inst=' '
        pathdir=' '
        calpath=' '
        codfil=' '

        nb_num=0
        b_num=0
        nstep=1
        nskip=0
        iyear=0
        icycle=0
        ifmt=1    !By default the format is x y sigma
        idet=0

        dt=0.0
        rnorm=-1.0

        !> Arguments
        call Get_Separator_Pos (cmd,'-', ipos, na)
        if (na == 0) then
           call error_message("Not valid arguments on the command line")
           IError=1
           return
        end if

        !> Loop over arguments
        ierr=.false.
        do i=1,na
           if (i < na) then
              line=adjustl(cmd(ipos(i):ipos(i+1)-1))
           else
              line=adjustl(cmd(ipos(i):))
           end if
           !write(*,"(a)") " => "//trim(line)

           ! Only check if the argument is -number
           call Get_Num(line,vet,ivet,iv)
           if (iv > 0) cycle

           ! Select directive: -dire options
           call Cut_String(line, nlong, dire)
           dire=adjustl(l_case(dire))

           !write(*,"(a)") " => Directive: "//trim(dire)
           select case (trim(dire))
              case ('-inf')                           ! Read only the information a create a file
                 info=.true.

              case ('-ins')                           ! Instrument Name
                 inst=trim(line)
                 if (len_trim(inst) <=0) then
                    ierr=.true.
                    exit
                 end if
                 if (len_trim(inst) <=0) then
                    call error_message("The instrument name is empty!")
                    ierr=.true.
                    exit
                 end if
                 select case (trim(u_case(inst)))
                    case('D1A','D1B','D2B','D4','D20')
                       powdat=.true.
                    case('D9','D10','D19')
                       powdat=.false.
                 end select

              case ('-num')                           ! Numor Selection
                 call Get_Num(line,vet,ivet,iv)
                 if (iv <=0) then
                    ierr=.true.
                    exit
                 end if
                 select case (iv)
                    case (1)
                       nb_num=1
                       b_num(nb_num,1)=ivet(1)
                       b_num(nb_num,2)=ivet(1)
                       nstep=1

                    case (2)
                       nb_num=1
                       b_num(nb_num,1)=min(ivet(1),ivet(2))
                       b_num(nb_num,2)=max(ivet(1),ivet(2))
                       nstep=1

                       ! Special case
                       line=cmd(ipos(i+1):)
                       n=index(line,' ')
                       call Get_Num(line(:n),vet,ivet,iv)
                       if (iv == 1) then
                          nskip=abs(ivet(1))
                          skipnum=.true.
                       end if

                    case (3)
                       nb_num=1
                       b_num(nb_num,1)=min(ivet(1),ivet(2))
                       b_num(nb_num,2)=max(ivet(1),ivet(2))
                       nstep=ivet(3)

                    case (4:10)
                       if (mod(iv,2) /= 0) then
                          call error_message("The number of blocks for Numors are wrong!")
                          ierr=.true.
                          exit
                       end if

                       nb_num=0
                       do j=1,iv,2
                          nb_num=nb_num+1
                          b_num(nb_num,1)=min(ivet(j),ivet(j+1))
                          b_num(nb_num,2)=max(ivet(j),ivet(j+1))
                       end do

                 end select
                 do j=1,nb_num
                    if (b_num(j,1) < 1 .or. b_num(j,2) < 1) then
                       call error_message("Numors numbers are not valid!")
                       ierr=.true.
                       exit
                    end if
                 end do
                 if (ierr) exit

              case ('-dir')                           ! Directory to look for Numors

                 pathdir=trim(adjustl(line))
                 n=len_trim(pathdir)
                 if(pathdir(1:1) == '"') then
                   pathdir(1:1) = " "
                   if(pathdir(n:n) == '"') then
                       pathdir(n:n) = " "
                   end if
                 end if

                 pathdir=trim(adjustl(pathdir))
                 n=len_trim(pathdir)
                 if (pathdir(n:n) /= ops_sep) pathdir=trim(pathdir)//ops_sep

                 if (.not. Directory_Exists(trim(pathdir))) then
                    call error_message("  The Directory "//trim(pathdir)//"  doesn't exist!")
                    ierr=.true.
                    exit
                 end if
                 call Set_Instrm_Directory(pathdir)

              !---- Options ----!
              case ('-mat')
                 ub_file=trim(line)
                 if (len_trim(ub_file) <= 0) then
                    ub_file="ubfrom.raf"
                 end if
                 inquire(file=trim(ub_file),exist=existe)
                 if (.not. existe) then
                    call error_message("  The UB-matrix file does not exist!, Numor UB-matrix will be used ... ")
                 else
                    open(unit=i_tmp,file=trim(ub_file),status="old",action="read",position="rewind")
                    do j=1,3
                      read(unit=i_tmp,fmt=*,iostat=ier) read_ub(j,:)
                      if(ier /= 0) exit
                    end do
                    if(ier /= 0) then
                      call error_message("  Error reading the UB-matrix!, Numor UB-matrix will be used ... ")
                      ub_read=.false.
                    else
                      read(unit=i_tmp,fmt=*,iostat=ier) Read_Wav
                      if(ier == 0) Wav_Read=.true.
                      Ub_Read=.true.
                    end if
                    close(unit=i_tmp)
                 end if

              case ('-nxs')
                 nxs=.true.

              case ('-ver')
                 verbose=.true.

              case ('-add')                           ! Add Numors
                 if (len_trim(line) > 0) then
                    call Cut_String(line,nlong,car)
                    select case(car)
                       case ('t','T')
                          call Get_Num(line,vet,ivet,iv)
                          if (iv /=1) then
                             call error_message("The correct format for 'add' is: -add T dT ")
                             ierr=.true.
                             exit
                          end if
                          dt=vet(1)
                          if (dt <= 0.01) dt = 0.01

                       case ('n','N')
                          call Get_Num(line,vet,ivet,iv)
                          if (iv /=1) then
                             call error_message("The correct format for 'add' is: -add N n ")
                             ierr=.true.
                             exit
                          end if
                          nsum=ivet(1)
                          if (nsum <= 0) nsum = 1

                       case default
                          call error_message("The correct format for 'add' is: -add T dT  or -add N n")
                          ierr=.true.
                          exit
                    end select
                 end if
                 add=.true.

              case ('-cal')                           ! Calibration file
                 j=len_trim(line)
                 if(line(1:1) == '"') line(1:1)=" "
                 if(line(j:j) == '"') line(j:j)=" "
                 line=adjustl(line)
                 calpath=trim(line)
                 inquire(file=trim(calpath),exist=existe)
                 if (.not. existe) then
                    call error_message("  The calibration file is not found in "//trim(calpath))
                    ierr=.true.
                    exit
                 end if

              case ('-zer')                           ! Calibration file
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    ierr=.true.
                    exit
                 end if
                 zero_shift=vet(1)

              case ('-ang')                           ! Angular position in Calibration file are used
                 angcor=.true.

              case ('-prm')                           ! Permuation of alphas according to Calibration file is used
                 perm=.true.

              case ('-fmt')                           ! Write the Numor(s) in Pattern data file
                 outconv=.true.
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /= 1) then
                    call error_message('Code not valid for Output Format!')
                    ierr=.true.
                    exit
                 end if
                 ifmt=ivet(1)

              case ('-yc')                            ! Year & Cycle
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=2) then
                    ierr=.true.
                    exit
                 end if
                 iyear=ivet(1)
                 icycle=ivet(2)
                 yc=.true.
                 if (icycle < 1 .or. icycle > 6) yc=.false.
                 if (iyear < 1973) yc=.false.

              case ('-dex')                            ! Detectors to exclude
                 call Get_Num(line,vet,ivet,iv)
                 if (iv < 2) then
                    ierr=.true.
                    exit
                 end if
                 nexcl=iv
                 do k=1,nexcl
                   excl_det(ivet(k))=.true.
                 end do

              case ('-cex')         ! Cells to exclude for a Banana detector
                 call Get_Num(line,vet,ivet,iv)
                 if (iv < 1) then
                    ierr=.true.
                    exit
                 end if
                 nexcel=iv
                 do k=1,nexcel
                   excl_cel(ivet(k))=.true.
                 end do


              case ('-cod')                           ! Define Codefile
                 codfil=trim(line)
                 if (len_trim(codfil) <=0) then
                    call error_message("The Codefile is empty!")
                    ierr=.true.
                    exit
                 end if
                 k=len_trim(codfil)      !Checking the presence of quotes
                 if(codfil(1:1)=='"')  codfil(1:1)=" "
                 if(codfil(k:k)=='"')  codfil(k:k)=" "
                 codfil=adjustl(codfil)

              case ('-nor')                           ! Normalization monitor
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("A value for Monitor normalization should be given!")
                    ierr=.true.
                    exit
                 end if
                 rnorm=vet(1)
                 valnorm=rnorm

              case ('-tor')                           ! Normalization time
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("A value for Monitor normalization should be given!")
                    ierr=.true.
                    exit
                 end if
                 rnorm=vet(1)
                 valnorm=-rnorm

              case ('-xmin')                           ! Minimum angle to output in powder diffraction pattern
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("A value for minimum two-theta should be given!")
                    ierr=.true.
                    exit
                 end if
                 xang_min=vet(1)
                 xmin_read=.true.
                 write(unit=*,fmt="(a,f8.3,a)") " => Minimum Twotheta output: ",xang_min," degrees"

              case ('-xmax')                           ! Minimum angle to output in powder diffraction pattern
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("A value for minimum two-theta should be given!")
                    ierr=.true.
                    exit
                 end if
                 xang_max=vet(1)
                 xmax_read=.true.
                 write(unit=*,fmt="(a,f8.3,a)") " => Maximum Twotheta output: ",xang_max," degrees"


              case ('-per')                           ! Percentage of contributing vertical cells for D2B
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("The instruction -per needs a value for percentage of contributing vertical cells for D2B!")
                    ierr=.true.
                    exit
                 end if
                 per=vet(1)

              case ('-lab')                            ! Label in Files
                 line=adjustl(line)
                 select case (line(1:1))
                    case ('t','T')
                       labelT=.true.

                    case default
                       call error_message("At the moment the only option is -label T")
                       ierr=.true.
                       exit
                 end select

              case ('-det')                            ! Scan for a particular detector
                 call Get_Num(line,vet,ivet,iv)
                 if (iv /=1) then
                    call error_message("Need the number of detector!")
                    ierr=.true.
                    exit
                 end if
                 idet=ivet(1)

              case default
                 if (i < na) then
                    call error_message(" Command line: "//trim(cmd(ipos(i):ipos(i+1)-1)))
                 else
                    call error_message(" Command line: "//trim(cmd(ipos(i):)))
                 end if
                 ierr=.true.
                 exit
           end select
        end do

        !-----------------
        !> Check arguments
        !-----------------
        if (ierr) Then
           ierror=1
           return
        end if

        if (len_trim(inst) <=0) then
           call error_message("Instrument name was not provided!")
           ierror=1
           return
        end if

        if (nb_num <=0) then
           call error_message("No numors were written!")
           ierror=1
           return
        end if

        if (len_trim(pathdir) <=0) then
           if (ops == 1) then
              call error_message("Directory Data for Numors for Windows System have to be written!")
              ierror=1
              return
           else
              ! Linux/MacOS
           end if
           !if (yc) then
           !   call Get_Absolute_Data_Path(num1,inst,pathdir,iyear,icycle)
           !else
           !   call Get_Absolute_Data_Path(num1,inst,pathdir)
           !end if
           !if (len_trim(pathdir) == 0) then
           !   call error_message("Directory Data for numors has not been found!")
           !   ierror=1
           !   return
           !end if
           !n=len_trim(pathdir)
           !if (pathdir(n:n) /= ops_sep) then
           !   n=index(pathdir,ops_sep,back=.true.)
           !   pathdir=pathdir(:n)
           !end if
        end if

        if (len_trim(codfil) <=0) codfil=trim(inst)

        return
    End Subroutine Process_CmdLine


    function nxs_to_sxtal_numor(nexus) Result(Snum)

        ! Arguments
        type(nexus_type), intent(in) :: nexus
        type(SXTAL_Numor_type)       :: snum

        ! Local variables
        integer :: i,n,ia,ic

        call Initialize_Numor(Snum,nexus%nbang,nexus%nbdata,nexus%nf)
        Snum%title  = trim(nexus%experiment_id)
        Snum%header = trim(nexus%user)//trim(nexus%local_contact)//trim(nexus%end_time)
        Snum%instrm = trim(nexus%instrument_name)
        Snum%numor  = nexus%run_number
        Snum%manip  = nexus%manip
        Snum%scantype = nexus%scan_type
        Snum%wave = nexus%wave
        Snum%conditions(1) = nexus%setp_temperature
        Snum%conditions(2) = nexus%reg_temperature
        Snum%conditions(3) = nexus%temperature
        Snum%conditions(5) = nexus%magnetic_field
        Snum%scans(1) = nexus%scan_start
        Snum%scans(2) = nexus%scan_step
        Snum%scans(3) = nexus%scan_width
        if (nexus%is_timef)        Snum%tmc_ang(1,:) = nexus%timef(:)
        if (nexus%is_monitor)      Snum%tmc_ang(2,:) = nexus%monitor(:)
        if (nexus%is_total_counts) Snum%tmc_ang(3,:) = nexus%total_counts(:)
        if (nexus%is_phi) Snum%angles(1) = 0.5*(nexus%angles(1,1) + nexus%angles(1,nexus%nf))
        if (nexus%is_chi) Snum%angles(2) = 0.5*(nexus%angles(2,1) + nexus%angles(2,nexus%nf))
        if (nexus%is_omega) then
            Snum%angles(3) = 0.5*(nexus%angles(3,1) + nexus%angles(3,nexus%nf))
        else if (nexus%is_canne) then
            Snum%angles(3) = 0.5*(nexus%angles(6,1) + nexus%angles(6,nexus%nf))
        end if
        if (nexus%is_tth) then
            Snum%angles(4) = 0.5*(nexus%angles(8,1) + nexus%angles(8,nexus%nf))
        else if (nexus%is_gamma) then
            Snum%angles(4) = 0.5*(nexus%angles(4,1) + nexus%angles(4,nexus%nf))
        end if
        if (nexus%is_phi) Snum%angles(5) = 0.5*(nexus%angles(5,1) + nexus%angles(5,nexus%nf))
        do n = 1 , nexus%nf
            i = 0
            do ic = 1 , nexus%nx
                do ia = 1 , nexus%nz
                    i = i + 1
                    Snum%counts(i,n)= nexus%counts(ia,ic,n) ! (nz,nx,nf)
                end do
            end do
        end do
        if (nexus%nbang == 1) then
            if (nexus%scan_type == 'phi') then
                Snum%tmc_ang(4,:) = nexus%angles(1,:)
            else if (nexus%scan_type == 'chi') then
                Snum%tmc_ang(4,:) = nexus%angles(2,:)
            else if (nexus%scan_type == 'omega') then
                Snum%tmc_ang(4,:) = nexus%angles(3,:)
            else if (nexus%scan_type == 'gamma') then
                Snum%tmc_ang(4,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'psi') then
                Snum%tmc_ang(4,:) = nexus%angles(5,:)
            else if (nexus%scan_type == 'canne') then
                Snum%tmc_ang(4,:) = nexus%angles(6,:)
            else if (nexus%scan_type == 'nu') then
                Snum%tmc_ang(4,:) = nexus%angles(7,:)
            else if (nexus%scan_type == '2theta') then
                Snum%tmc_ang(4,:) = nexus%angles(8,:)
            end if
        else if (nexus%nbang == 2) then
            if (nexus%scan_type == 'omega') then
                Snum%tmc_ang(4,:) = nexus%angles(3,:)
                Snum%tmc_ang(5,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'canne') then
                Snum%tmc_ang(4,:) = nexus%angles(6,:)
                Snum%tmc_ang(5,:) = nexus%angles(4,:)
            end if
        end if
        if (nexus%geometry == 'NB') Snum%icalc = 1

    end function nxs_to_sxtal_numor

 End Module GetData_General_Procedures



 !!----
 !!---- Program Get_Data
 !!----
 !!---- Authors: Juan Rodriguez-Carvajal (ILL)
 !!----          Javier Gonzalez-Platas  (ULL)
 !!----
 !!---- Date: 24/03/2011
 !!
 Program Get_Data
  !---- Use Modules ----!
  Use GetData_General_Procedures
  Use CFML_Diffpatt, only: DiffPat_E_Type,Write_Pattern_FreeFormat,write_pattern_XYSig, &
                           Write_Pattern_INSTRM5
  Use CFML_Strings,  only: pack_string
  !---- Variables ----!
  implicit none

  logical, dimension(:), allocatable :: compressed
  logical, dimension(:), allocatable :: list
  logical                            :: existe, select_num, calibration,file_inst,&
                                        first, delete, wbuf

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
  integer, dimension(128,128)        :: cal_d2b
  !!!!!!!!!!!!!!!!!!!!!!!!!

  !>----------------------<
  !>---- Init Program ----<
  !>----------------------<

  call Set_Compress_Program()

  !> Command Line
  call Get_Command(Command=Cmdline,Length=nlong)
  call Cut_String(cmdline,nlong)   ! Delete the name of the program, commented for Lahey
  write(*,"(a)") "  "
  if (nlong <= 0) then
     call manual()
     call Stop_Program()
  end if

  call cpu_time(t1)
  n=index(cmdline,'>')
  if (n > 0) cmdline=cmdline(:n-1)

  file_inst=.false.
  first=.true.

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
  write(unit=*,fmt="(/,a,/)") " => PROGRAM  Get_Dat running ... "
  write(unit=i_log,fmt="(/,a)") " --------------------------------------------- "
  write(unit=i_log,fmt="(a)")   "               PROGRAM  Get_Data               "
  write(unit=i_log,fmt="(a)")   "     Data retrival for ILL diffractometers     "
  write(unit=i_log,fmt="(a)")   "     (Version 1.6, JRC-ILL, February 2023)     "
  write(unit=i_log,fmt="(a,/)") " --------------------------------------------- "
  write(unit=i_log,fmt="(a)") " => Provided arguments in the command line: "
  write(unit=i_log,fmt="(a)") "   "//trim(cmdline)

  nr=0
  cm_line=" "
  if(file_inst) cmdline= " "

  do         !general loop

    if(file_inst) then
       !read(unit=i_tmp,fmt='(a)',iostat=ier) cmdline
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
            write(unit=*,fmt="(a)") " => No more lines in "//trim(fileins)//" to process"
            exit !no more lines in the file
       end if
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
       call manual()
       write(unit=*,fmt="(a)") " => The input cmdline was: "//trim(Cmdline)
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
       else
          write(unit=i_inf,fmt="(a)") &
               "   Numor          Title                                  Header                 Hmin    Kmin    Lmin      "// &
               "Hmax    Kmax    Lmax      Tset    Treg  Tsample   Wavelength"
          write(unit=*,fmt="(a)") &
               "   Numor          Title                                  Header                 Hmin    Kmin    Lmin      "// &
               "Hmax    Kmax    Lmax      Tset    Treg  Tsample   Wavelength"
       end if
    end if

    !> Allocating Numors
    numdim=(num2-num1+1)/nstep

    if (powdat) then
       call Allocate_Numors(numdim,0,0,0,PNum)
    else
       call Allocate_Numors(numdim,0,0,0,SNum)
    end if
    if (ERR_ILLData) then
       if (info) call error_message(trim(ERR_ILLData_Mess),i_inf)
       call error_message(trim(ERR_ILLData_Mess))
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
          else
             SNum(num) = nxs_to_sxtal_numor(nxsf)
          end if
       else
          if (powdat) then
             call Read_Numor(trim(line),u_case(trim(inst)),PNum(num),info)
          else
             call Read_Numor(trim(line),u_case(trim(inst)),SNum(num),info)
          end if
       end if

       !> Information only
       if (info) then
          if (powdat) then
            if (ERR_ILLData) then
             write(unit=i_inf,fmt="(i8,2(tr2,a))") PNum(num)%numor,PNum(num)%title, PNum(num)%header
             write(unit=*,fmt="(i8,2(tr2,a))") PNum(num)%numor,PNum(num)%title, PNum(num)%header
            else
             write(unit=i_inf,fmt="(i8,2(tr2,a),f8.2,f12.1,3f8.2,f12.5)") PNum(num)%numor,PNum(num)%title, &
                  PNum(num)%header, PNum(num)%time, PNum(num)%monitor, PNum(num)%conditions(1:3), PNum(num)%wave

             write(unit=*,fmt="(i8,2(tr2,a),f8.2,f12.1,3f8.2,f12.5)") PNum(num)%numor,PNum(num)%title, &
                  PNum(num)%header, PNum(num)%time, PNum(num)%monitor, PNum(num)%conditions(1:3), PNum(num)%wave
            end if
          else
             write(unit=i_inf,fmt="(i8,2(tr2,a),3(3f8.2,2x),f12.5)") SNum(num)%numor,SNum(num)%title,       &
                  SNum(num)%header, SNum(num)%Hmin, SNum(num)%Hmax, SNum(num)%conditions(1:3),SNum(num)%wave

             write(unit=*,fmt="(i8,2(tr2,a),3(3f8.2,2x),f12.5)") SNum(num)%numor,SNum(num)%title,       &
                  SNum(num)%header, SNum(num)%Hmin, SNum(num)%Hmax, SNum(num)%conditions(1:3), SNum(num)%wave
          end if
       end if

       if (ERR_ILLData) then  !Moved after writing information message
          call error_message(trim(ERR_ILLData_Mess))
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
          else
             tmin=minval(Snum(1:num)%conditions(3))
             tmax=maxval(Snum(1:num)%conditions(3))
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
                   else
                      if (abs(Snum(i)%conditions(3)-t) < dt ) then
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
             else
                 write(unit=aux,fmt="(a,i6.6,a,f6.1,a1)") trim(codfil)//"_",Pnum(i)%numor,"_",Snum(i)%conditions(3),"K"
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
             else
                write(unit=aux,fmt="(a,i6.6)") trim(codfil)//"_",Snum(i)%numor
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
                if(nxs) then
                   call read_calibration(trim(CalPath),"mantid_workspace_1/workspace/values",machine,Cal)
                else
                   call Read_Calibration_File(CalPath,trim(machine),Cal)
                   if (err_illdata) then
                      call error_message(trim(err_illdata_mess))
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
                    call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal,angcor=angcor)
                  else
                    if(perm) then
                      call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal,perm=perm)
                    else
                      call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm,Cal=Cal)
                    end if
                  end if
               else
                  if(angcor) then
                    call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal,angcor=angcor)
                  else
                    if(perm) then
                      call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal,perm=perm)
                    else
                      call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat,Cal=Cal)
                    end if
                  end if
               end if
             else
               if (rnorm > 0.01) then
                  call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat, VNorm=valnorm)
               else
                  call PowderNumors_To_DiffPattern(PNumors=PNum,N=Num,ActList=List,Pat=Pat)
               end if
             end if

          case ('D1A')
             if (len_trim(calpath) > 0) then
                call Read_Calibration_File(CalPath,'D1A',Cal)
                if (err_illdata) then
                   call error_message(trim(err_illdata_mess))
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
             if (len_trim(calpath) > 0) then
                if(nxs) then
                   call Read_Calibration(trim(CalPath),"mantid_workspace_1/workspace/values",machine,Cal)
                else
                   call Read_Calibration_File(CalPath,'D2B',Cal)
                   if (err_illdata) then
                      call error_message(trim(err_illdata_mess))
                   else
                      calibration=.true.
                   end if
                end if
                cal_d2b=0
                do j=1,128
                  do i=1,128
                    if(Cal%Active(i,j)) then
                      cal_d2b(i,j)=nint(1000.0*Cal%Effic(i,j))
                    end if
                  end do
                end do
                open (unit=4, file='calib_d2b.bin', access="stream",status="replace", action="write")
                write(unit=4) 128,128
                write(unit=4) cal_d2b
                close(unit=4)
             end if
             if (.not. calibration) then
                if (rnorm > 0.01) then
                   !call adding_Numors_D2B_DiffPattern(PNum,Num,List,excl_det,Pat,VNorm=rnorm, Perc=per)
                   call Image2D_D2B(PNum,Num,List,excl_det,Pat,VNorm=rnorm, Perc=per)
                else
                   !call adding_Numors_D2B_DiffPattern(PNum,Num,List,excl_det,Pat,Perc=per)
                   call Image2D_D2B(PNum,Num,List,excl_det,Pat,Perc=per)
                end if
             else
                if (rnorm > 0.01) then
                   !call adding_Numors_D2B_DiffPattern(PNum,Num,List,excl_det,Pat,VNorm=rnorm, Perc=per,Cal=Cal)
                   call Image2D_D2B(PNum,Num,List,excl_det,Pat,VNorm=rnorm, Perc=per,Cal=Cal)
                else
                   !call adding_Numors_D2B_DiffPattern(PNum,Num,List,excl_det,Pat,Perc=per,Cal=Cal)
                   call Image2D_D2B(PNum,Num,List,excl_det,Pat,Perc=per,Cal=Cal)
                end if
             end if

          case ('D4')
             if (len_trim(calpath) > 0) then
                if(nxs) then
                   call Read_Calibration(trim(CalPath),"mantid_workspace_1/workspace/values",machine,Cal)
                else
                   call Read_Calibration_File(CalPath,'D4',Cal)
                   if (err_illdata) then
                      call error_message(trim(err_illdata_mess))
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


          case('D9','D19','D10')
              write(unit=*,fmt="(a)") " => Now we have to construct I(qh,qk,ql)"
              !It is supposed that the whole set of numors have been measured with
              !the same orientation matrix

              if(ub_read) then
                 if(wav_read) then
                    Call Set_Current_Orient(Read_wav,read_ub)
                 else
                    Call Set_Current_Orient(SNum(1)%wave,read_ub)
                 end if
              else
                 Call Set_Current_Orient(SNum(1)%wave,SNum(1)%ub)
              end if
              if(trim(machine) == 'D9' .or. trim(machine) == 'D10') then
                 call Set_PSD(488.0,2.0,2.0,32,32,2)
              else
                 call Set_PSD(760.0,2.5,1.56,640,256,3)
              end if
              call Construct_Zhkln(SNum,Num)
              exit

          case default
             call info_message('Not yet implemented')
             exit
       end select

       !>-----------------------<
       !>---- Output Format ----<
       !>-----------------------<
       if (err_illdata) then
          call error_message(trim(err_illdata_mess))
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
  end do   !process next instruction in file

  if(wbuf) close(unit=ibuf)
  call cpu_time(t2)
  write(unit=*,fmt="(/,a)")          " => Program Get_Data finished normally, look at the file: get_data.log for details"
  write(unit=*,fmt="(a,f8.2,a)")     " => Total CPU-time: ",t2-t1," seconds"
  write(unit=i_log,fmt="(/,a)")      " => Program Get_Data finished normally"
  write(unit=i_log,fmt="(a,f8.2,a)") " => Total CPU-time: ",t2-t1," seconds"
  call Stop_Program()
  close (unit=i_log)

  Contains

    Subroutine Stop_Program()
      character(len=1) :: key
      write(unit=*,fmt="(/,a)") " => Press <enter> to finish "
      read(unit=*,fmt="(a)") key
      stop
    End Subroutine Stop_Program


    Subroutine Construct_Zhkl(SNumor,Numors,ndiv)
      type(SXTAL_Numor_type),  dimension(:), intent(in) :: SNumor
      integer,                               intent(in) :: Numors
      integer,                               intent(in) :: ndiv
      !--- Local variables ---!
      character(len=80)  :: filen
      integer, dimension(:,:,:), allocatable :: Zhkl, nsup
      integer, dimension(3)   :: nd,ind
      real,    dimension(6)   :: rcel,dcel
      real,    dimension(3)   :: hmin,hmax,h,z1,hr
      real,    dimension(3,3) :: ub,iub
      integer :: ic,ia,i,j,k,counts,m
      real    :: res

      iub=Current_Orient%ubinv
       ub=Current_Orient%ub
      call cell_fr_UB(ub,dcel=dcel,rcel=rcel)
      write(*,"(a,3f10.4,3f8.2)") " => Cell parameters: ",dcel
      !Determine the best box for representing the reciprocal lattice
      hmin=[20.0,20.0,20.0] ; hmax=-hmin
      do i=1,Numors
        do j=1,3
         if(SNumor(i)%hmin(j) < hmin(j) ) hmin(j)=SNumor(i)%hmin(j)
         if(SNumor(i)%hmax(j) > hmax(j) ) hmax(j)=SNumor(i)%hmax(j)
        end do
      end do
      hmin=hmin-[0.1,0.1,0.1]
      hmax=hmax+[0.1,0.1,0.1]
      hr=hmax-hmin
      hr=[hr(1)*rcel(1),hr(2)*rcel(2),hr(3)*rcel(3)] ! In reciprocal Angstroms
      res=minval(abs(hr))/real(ndiv)
      nd=nint(hr/res)
      write(*,*) "    divisions: ", nd
      write(*,*) "   resolution: ", res
      write(*,*) "        hmin : ",hmin
      write(*,*) "        hmax : ",hmax
      write(*,*) "          hr : ",hr

      if(allocated(Zhkl)) deallocate (Zhkl)
      allocate (Zhkl(nd(1),nd(2),nd(3)))
      if(allocated(nsup)) deallocate (nsup)
      allocate (nsup(nd(1),nd(2),nd(3)))
      Zhkl=0 ; nsup=0
      do i=1,Numors
        do j=1,SNumor(i)%nframes
          k=0
          do ic=1,psd%ncat
             do_int: do ia=1,psd%nano
               k=k+1
               counts=SNumor(i)%counts(k,j)
               z1= Get_z1_from_pixel(ia,ic,j,SNumor(i))
               h=matmul(Current_Orient%ubinv,z1)-hmin
               ind=nint([h(1)*rcel(1),h(2)*rcel(2),h(3)*rcel(3)]/res)
             !  write(*,"(a,3f9.4)") "  h=",h
               do m=1,3
                if(ind(m) > nd(m) .or. ind(m) < 1) cycle do_int
               end do
               nsup(ind(1),ind(2),ind(3))=nsup(ind(1),ind(2),ind(3))+1
               Zhkl(ind(1),ind(2),ind(3))=Zhkl(ind(1),ind(2),ind(3))+counts
             !  write(*,"(a,3i4,a,i6)") " ind=",ind, "   Counts"
             end do  do_int
          end do
        end do
      end do
      where (nsup /= 0) Zhkl=Zhkl/nsup
      do i=1,nd(3)
        write(unit=filen,fmt="(a,i3.3,a)") "zhkl_",i,".bin"
        open(unit=3, file=trim(filen), access="stream",status="replace", action="write")
        write(unit=3) nd(1),nd(2)
        write(unit=3) zhkl(:,:,i)
        close(unit=3)
      end do
      return

    End Subroutine Construct_Zhkl

    Subroutine Construct_Zhkln(Num,Numors)
      type(SXTAL_Numor_type),  dimension(:), intent(in) :: Num
      integer,                               intent(in) :: Numors
      !--- Local variables ---!
      logical                 :: out_of_range
      real, dimension(:,:,:), allocatable :: Zhkl,nsup
      integer, dimension(3)   :: nd,ind
      real,    dimension(6)   :: rcel,dcel
      real,    dimension(3)   :: hmin,hmax,h,z1,hr,hs,hlow,hup
      real,    dimension(3,3) :: ub,iub
      integer,dimension(3,8)  :: box_lim
      integer :: ic,ia,i,j,k,counts,m
      real    :: npix_zhkl
      character(len=80) :: tit_zhkl="Z_HKL Binary File",filen

      iub=Current_Orient%ubinv
       ub=Current_Orient%ub
       call cell_fr_UB(ub,dcel=dcel,rcel=rcel)
       write(*,"(a,3f10.4,3f8.2)") " => Cell parameters: ",dcel

      hmin=Num(1)%hmin ; hmax=Num(1)%hmax

      do m=1,Numors
        !Extreme pixels of first and last frames
        box_lim(:,1)=[1,1,1]
        box_lim(:,2)=[1,psd%ncat,1]
        box_lim(:,3)=[psd%nano,1,1]
        box_lim(:,4)=[psd%nano,psd%ncat,1]
        box_lim(:,5)=[1,1,Num(m)%nframes]
        box_lim(:,6)=[1,psd%ncat,Num(m)%nframes]
        box_lim(:,7)=[psd%nano,1,Num(m)%nframes]
        box_lim(:,8)=[psd%nano,psd%ncat,Num(m)%nframes]
        do i=1,8
          ia=box_lim(1,i); ic= box_lim(2,i); j = box_lim(3,i)
          z1= Get_z1_from_pixel(ia,ic,j,Num(m))
          h=matmul(Current_Orient%ubinv,z1)
          do j=1,3
            if(h(j) < hmin(j) ) hmin(j)=h(j)
            if(h(j) > hmax(j) ) hmax(j)=h(j)
          end do
        end do
      end do

      do
        write(unit=*,fmt="(/,2(a,3f8.3),a)") " => HKL range, hmin=(",hmin,")   hmax=(",hmax,")"
        out_of_range=.false.
        write(unit=*,fmt="(a)",advance="no") " => Enter (h,k,l)min: "
        read(unit=*,fmt=*,iostat=ier) hlow
        write(unit=*,fmt="(a)",advance="no") " => Enter (h,k,l)max: "
        read(unit=*,fmt=*,iostat=ier) hup
        do i=1,3
           if(hlow(i) < hmin(i) .or. hlow(i) > hmax(i)) then
             out_of_range=.true.
             exit
           end if
           if(hup(i) < hmin(i) .or. hup(i) > hmax(i)) then
             out_of_range=.true.
             exit
           end if
        end do
        if(out_of_range) then
           write(unit=*,fmt="(a)") " => Warning! given hmin,hmax out of range!"
           cycle
        end if
        do i=1,3
          if(hlow(i) >= hup(i)) then
            out_of_range=.true.
            exit
          end if
        end do
        if(out_of_range) then
           write(unit=*,fmt="(a)") " => Warning! All hmin components have to be lower than hmax components!"
           cycle
        end if
        exit
      end do

      do
        write(unit=*,fmt="(a)",advance="no") " => Enter the number of divisions for h,k and l: "
        read(unit=*,fmt=*) nd
        npix_zhkl=nd(1)*nd(2)*nd(3)
        if(npix_zhkl*4 > 5.0e8) then
          write(unit=*,fmt="(a)") " => Warning! More than 0.5 GigaBytes!, reduce the number of divisions ..."
          cycle
        end if
        exit
      end do

      hr=hup-hlow
      hs=hr/real(nd)
      write(*,*) "    Number of divisions : ",nd
      write(*,*) "        h-steps(r.l.u.) : ",hs
      write(*,*) "      h-steps(1/angstr) : ",hs(1:3)*rcel(1:3)

      if(allocated(Zhkl)) deallocate (Zhkl)
      allocate (Zhkl(nd(1),nd(2),nd(3)))
      if(allocated(nsup)) deallocate (nsup)
      allocate (nsup(nd(1),nd(2),nd(3)))
      Zhkl=0  ; nsup=0

      do i=1,Numors
       do j=1,Num(i)%nframes
         k=0
         do ic=1,psd%ncat
            do_int: do ia=1,psd%nano
              k=k+1
              counts=Num(i)%counts(k,j)
              z1= Get_z1_from_pixel(ia,ic,j,Num(i))
              h=matmul(Current_Orient%ubinv,z1)- hlow
              ind=nint(h/hs)
              do m=1,3
               if(ind(m) > nd(m) .or. ind(m) < 1) cycle do_int
              end do
              nsup(ind(1),ind(2),ind(3))=nsup(ind(1),ind(2),ind(3))+1.0
              Zhkl(ind(1),ind(2),ind(3))=Zhkl(ind(1),ind(2),ind(3))+counts
            end do  do_int
         end do
       end do
      end do

      where (nsup /= 0) Zhkl=Zhkl/nsup
      nsup=Zhkl
      !Interpolate the matrix replacing all zeros with average values of neighbours
      do k=2,nd(3)-1
        do j=2,nd(2)-1
          do i=2,nd(1)-1
             if(nsup(i,j,k) < 0.001)  then
              Zhkl(i,j,k) =((nsup(i-1,j,k)+nsup(i+1,j,k)) +  &
                            (nsup(i,j-1,k)+nsup(i,j+1,k)) +  &
                            (nsup(i,j,k-1)+nsup(i,j,k+1)))/6.0
             end if
          end do
        end do
      end do
      write(unit=filen,fmt="(a,i6.6,a,i6.6,a)") "Z_hkl_",Num(1)%Numor,"_",Num(numors)%Numor,".bin"
      open(unit=3, file=trim(filen), access="stream",status="replace", action="write")
      write(unit=3) tit_zhkl
      write(unit=3) rcel(1:3)*100.0,rcel(4:5)
      write(unit=3) nd
      write(unit=3) zhkl
      close(unit=3)

      return
    End Subroutine Construct_Zhkln


    Subroutine Image2D_D2B(PNumors,N,ActList,Excl,Pat,VNorm,Perc,Cal)
        !---- Arguments ----!
        type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
        integer,                                   intent(in)  :: N          ! Number of Numors
        logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
        logical,dimension(:),                      intent(in)  :: Excl       ! Exclusion of detectors
        type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
        real, optional,                            intent(in)  :: VNorm      ! Normalization value
        real, optional,                            intent(in)  :: Perc       ! Percentage of active Cells
        type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information

        !---- Local Variables ----!
        logical                              :: correction=.false.
        integer, parameter                   :: ndet=128
        integer, parameter                   :: ncell=128
        integer                              :: i,j,k,np,nc,nt,num,nf,nf_max,k1,k2, ith, nfp, &
                                                nstep
        real                                 :: xmin,xmax,step,zh,dh,rporc, time_ini,         &
                                                time_fin,ttheta,cosg, sumdif, cnorm, fac,     &
                                                delta, dx, dg, dgdx
        real, parameter                      :: h=300.0          ! 300mm of height
        real, parameter                      :: radius=1296.0    ! Distance from Sample to Detector
        integer, dimension(:),      allocatable :: nd
        real,    dimension(:,:),    allocatable :: zz,vz
        real,    dimension(:,:,:,:),allocatable :: z
        real,    dimension(:,:,:),  allocatable :: x
        real,    dimension(:),      allocatable :: gam
        real,    dimension(ncell)               :: cnu
        real,    dimension(ncell,ndet)          :: effic
        logical, dimension(ncell,ndet)          :: active=.true.


        call cpu_time(time_ini)

        !> Init
        call init_err_illdata()

        if (N <=0) then
           err_illdata=.true.
           err_illdata_mess=' Number of Numors in the List is zero!'
           return
        end if

        num=count(ActList .eqv. .true.)
        if (num <=0) then
           err_illdata=.true.
           err_illdata_mess=' Number of active Numors in the List was zero!'
          return
        end if

        cnorm=1.0
        effic=1.0
        if (present(Cal))  then
           correction=.true.
           effic=Cal%Effic
           active=Cal%active
        end if
        if (present(VNorm)) cnorm=VNorm

        !> Checking some considerations

        num=0
        do i=1,N
           if (.not. ActList(i)) cycle
           num=num+1
           !> Load Normalization value
           if (num == 1 .and. .not. present(vnorm)) cnorm=PNumors(i)%tmc_ang(2,1)  ! Monitor normalization value
           sumdif=0.0
           do nf=2,PNumors(i)%nframes
              sumdif=sumdif + abs(PNumors(i)%tmc_ang(4,nf) -PNumors(i)%tmc_ang(4,nf-1))
           end do
           if ( sumdif > 1.25) then !1.25 is the separation in degrees of each detector tube in SuperD2B
              err_illdata=.true.
              err_illdata_mess=' The number of frames and step for each frame are incompatible with this routine!'
              return
           end if
        end do
        nf_max=maxval(PNumors(1:N)%nframes,mask=ActList)
        if (allocated(z)) deallocate(z)
        allocate(z(nf_max,ncell,ndet,Num)) ! Intensities (frames,cell,detector,active_numors)
        if (allocated(x)) deallocate(x)
        allocate(x(nf_max,ndet,Num)) ! Angles(frames,detector,active_numors)

        z=0.0
        num=0
        do i=1,N
          if (.not. ActList(i)) cycle
          num=num+1
          do nf=1,PNumors(i)%nframes
             fac=cnorm/PNumors(i)%tmc_ang(2,nf)
             nc=0
             !do nt=1,ndet-1,2
             !   do k=1,ncell
             !     z(nf,:,nt,num)=PNumors(i)%counts(nc+1:nc+ncell,nf)*fac/effic(k,nt)
             !   end do
             !   nc=nc+ncell+ncell
             !   do k=ncell,1,-1
             !      z(nf,k,nt+1,num)=PNumors(i)%counts(nc-k+1,nf)*fac/effic(k,nt)
             !   end do
             !end do
             do nt=1,ndet
                do k=1,ncell
                  z(nf,:,nt,num)=PNumors(i)%counts(nc+1:nc+ncell,nf)*fac/effic(k,nt)
                  nc=nc+ncell
                end do
             end do
          end do
        end do
        ! gamm Values
        if(correction) then
            num=0
            do i=1,N
              if (.not. ActList(i)) cycle
              num=num+1
              x(:,ndet,num)=PNumors(i)%tmc_ang(4,:)
              do nf=1,Pnumors(i)%nframes
                 x(nf,:,num)=x(nf,ndet,num)+Cal%PosX(:)
              end do
            end do
        else
           num=0
           do i=1,N
             if (.not. ActList(i)) cycle
             num=num+1
             x(:,ndet,num)=PNumors(i)%tmc_ang(4,:)
             do nt=ndet-1,1,-1
                x(:,nt,num)=x(:,nt+1,num)-1.25
             end do
           end do
        end if

        xmin=0.05; xmax=maxval(x)
        np=ndet*nf_max
        nstep= nint(1000.0*xmax/real(np-1))
        step=real(nstep)*0.001

        if (allocated(zz)) deallocate(zz)
        allocate(zz(ncell,np)) ! (z,gamm)
        if (allocated(vz)) deallocate(vz)
        allocate(vz(ncell,np)) ! (z,gamm)
        zz=0.0; vz=0.0
        if (allocated(gam)) deallocate(gam)
        allocate(gam(np)) ! (gamm)
        if (allocated(nd)) deallocate(nd)
        allocate(nd(np)) ! (gamm)
        do i=1,np
          gam(i)=real(50+(i-1)*nstep)*0.001
        end do
        nd=0
        num=0

        do i=1,N
           if( .not. ActList(i)) cycle
           num=num+1
           do nt=1,ndet
              if(Excl(nt)) cycle
              do j=1,Ncell
                 if(.not. Active(j,nt)) cycle
                 do nf=1,PNumors(i)%nframes
                    k=locate(gam,x(nf,nt,num))
                    if(k < 1 .or. k > np) cycle
                    nfp=nf-1
                    nfp=max(1,nfp)
                    dg=gam(k)-x(nfp,nt,num)       !Linear interpolation of intensities
                    dx=x(nf,nt,num)-x(nfp,nt,num) !to the required angle
                    if(abs(dx) <= 1.0e-1) then
                        zz(j,k)=zz(j,k) + z(nf,j,nt,num)
                    else
                        dgdx=dg/dx
                        delta = dgdx*((z(nf,j,nt,num)-z(nfp,j,nt,num)))
                        zz(j,k)=zz(j,k) + delta + z(nfp,j,nt,num)
                    end if
                    nd(k) = nd(k)+1
                 end do
              end do
           end do
        end do
        sumdif=count(Active .eqv. .true.)  !total number of detectors x cells contributing to
                                           !the diffraction pattern
        do k=1,np
          fac= sumdif/real(max(1,nd(k)))    !Ncell*Ndet=128x128=16384
          zz(:,k)= fac*zz(:,k)   !Intensity
          vz(:,k)= fac*zz(:,k)   !Variance
        end do

        open(unit=3, file='d2b.bin', access="stream",status="replace", action="write")
        write(*,*) " Ncell",ncell,"  Number of columns",np
        write(unit=3) ncell,np
        write(unit=3) nint(zz)
        close(unit=3)

        !Calculating a 1D powder diffraction pattern
        call Allocate_Pattern(Pat, np)

        !> First active Numor on the List
        i=1
        Pat%Monitor=PNumors(i)%tmc_ang(2,1)  ! Monitor normalization value
        Pat%KindRad='neutron'
        Pat%ScatVar='2theta'
        Pat%instr='D2B'
        Pat%npts=np-1
        Pat%xmin=xmin
        Pat%xmax=xmax
        Pat%step=step

        Pat%title=trim(PNumors(i)%title)
        Pat%TSet=PNumors(i)%conditions(1)
        Pat%TSample=PNumors(i)%conditions(3)
        Pat%wave(1)=PNumors(i)%wave

        Pat%x=gam
        Pat%y=0.0
        Pat%sigma=0.0
        Pat%nd=0

        rporc=100.0
        if(present(Perc)) rporc=perc
        dh=h/real(ncell-1)
        nc=nint(ncell*rporc*0.005)       ! rporc *0.01 /2.0
        nc=max(1,nc)
        nc=min(64,nc)

        do k=1,ncell
          zh=-0.5*h+real(k-1)*dh          ! Z-coordinate of the cell to the equatorial plane
          cnu(k)=cos(atan2(zh,radius))    ! cosinus of Latitude angle
        end do

        k1=65-nc
        k2=64+nc
        write(unit=*,fmt="(a,2i4)") " => Vertical Integration between cells: ",k1,k2

        do i=1,Pat%npts
           cosg=cosd(gam(i))
           do k= k1,k2
             ttheta = acosd(cnu(k)*cosg) !This is the 2theta of the cell
             ith=nint((ttheta-xmin)/step) + 1
             if(ith < 1 .or. ith > Pat%npts) cycle
             Pat%y(ith)=Pat%y(ith)+zz(k,i)
             Pat%sigma(ith)=Pat%Sigma(ith)+vz(k,i)
             Pat%nd(ith)=Pat%nd(ith)+1
           end do
        end do

        do i=1,Pat%npts
          fac= 1.0/real(max(1,Pat%nd(i)))
          Pat%y(i)=Pat%y(i) * fac
          Pat%sigma(i)=Pat%sigma(i) * fac * fac
        end do

        call cpu_time(time_fin)
        write(unit=*,fmt="(a,f8.2,a)") " => CPU-time for calculating and writing image and diff-pat: ",time_fin-time_ini," seconds"

        return
    End Subroutine Image2D_D2B
    !!----
    !!---- Subroutine XXXX ( )
    !!----
    !!----
    !!----
    !!
    Subroutine Visualization_D2B(Num)
        !---- Arguments ----!
        type(POWDER_Numor_type), intent(in) :: Num

        !---- Local Variables ----!
        integer, parameter                  :: ndet=128
        integer, parameter                  :: ncell=128
        integer                             :: k,n,nc,nt
        real, dimension(:,:,:), allocatable :: z
        real, dimension(:,:),   allocatable :: zz


        if (allocated(z)) deallocate(z)
        allocate(z(ncell,ndet,Num%nframes)) ! (y,x,frames)
        z=0.0

        do n=1,num%nframes
           nc=0
           do nt=1,ndet,2
              z(:,nt,n)=num%counts(nc+1:nc+ncell,n)
              nc=nc+ncell+ncell
              do k=ncell,1,-1
                 z(k,nt+1,n)=num%counts(nc-k+1,n)
              end do
           end do
        end do

        if (allocated(zz)) deallocate(zz)
        allocate(zz(ncell,ndet*Num%nframes)) ! (y,x,frames)
        zz=0.0

        do n=1,num%nframes
           do nt=1,ndet
              k=n+(nt-1)*num%nframes
              zz(:,k)=z(:,nt,n)
           end do
        end do

        open(unit=1,file='d2b.dat')
        do k=ncell,1,-1
           write(unit=1,fmt='(3200f5.1)') zz(k,:)
        end do

        close(unit=1)

        return
    End Subroutine Visualization_D2B

    !!----
    !!---- Subroutine Adding_Numors_D2B_DiffPattern(PNumors,N,ActList,Excl,Pat,VNorm,Perc,Cal )
    !!----    type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
    !!----    integer,                                   intent(in)  :: N          ! Number of Numors
    !!----    logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
    !!----    logical,dimension(:),                      intent(in)  :: Excl       ! Excluded detector list
    !!----    type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
    !!----    real, optional,                            intent(in)  :: VNorm      ! Normalization value
    !!----    real, optional,                            intent(in)  :: Porc       ! Porcentage of active Cells
    !!----    type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
    !!----
    !!----    Porc=50.0 Means that take 64 cell (32 up / 32 down from equatorial plane)
    !!----
    !!---- Date: 26/03/2011
    !!
    Subroutine Adding_Numors_D2B_DiffPattern(PNumors,N,ActList,Excl,Pat,VNorm,Perc,Cal )
        !---- Arguments ----!
        type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
        integer,                                   intent(in)  :: N          ! Number of Numors
        logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
        logical,dimension(:),                      intent(in)  :: Excl       ! Excluded detector list
        type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
        real, optional,                            intent(in)  :: VNorm      ! Normalization value
        real, optional,                            intent(in)  :: Perc       ! Percentage of active Cells
        type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information

        !---- Local Variables ----!
        logical                             :: correction=.false.

        integer, parameter                  :: ndet=128
        integer, parameter                  :: ncell=128
        integer, dimension(:), allocatable  :: ind
        integer, dimension(:,:), allocatable:: ncc

        integer                             :: i,j,num,nf,nt,np,nf_max
        integer                             :: k,nk,nkk,nc,nc_fin

        real, parameter                     :: h=300.0          ! 300mm of height
        real, parameter                     :: radio=1500.0     ! Distance from Sample to Detector
        real, dimension(:,:),   allocatable :: x,zz
        real, dimension(:,:,:), allocatable :: z, xx,yy,vv,d2yy
        real                                :: cnorm,sumdif,zc,xmin,xmax,step,x1,x2,rporc
        real                                :: dh,zh,cos_2Theta,cos_nu,nu,gamm,fac
        real                                :: time_ini, time2


        call cpu_time(time_ini)

        !> Init
        call init_err_illdata()


        if (N <=0) then
           err_illdata=.true.
           err_illdata_mess=' Number of Numors in the List was zero!'
           return
        end if

        num=count(actlist .eqv. .true.)
        if (num <=0) then
           err_illdata=.true.
           err_illdata_mess=' Number of active Numors in the List was zero!'
          return
        end if

        !call cpu_time(time1)
        !print*,'Tiempo de Init:',time1-time_ini
        !call cpu_time(time1)

        !> Checking some considerations
        if (allocated(ind)) deallocate(ind)
        allocate(ind(num))
        ind=0

        num=0
        do i=1,N
           if (.not. actlist(i)) cycle

           num=num+1
           ind(num)=i

           sumdif=0.0
           do nf=2,PNumors(i)%nframes
              sumdif=sumdif + abs(PNumors(i)%tmc_ang(4,nf) -PNumors(i)%tmc_ang(4,nf-1))
           end do
           if ( sumdif > 1.25) then !1.25 is the separation in degrees of each detector tube in SuperD2B
              err_illdata=.true.
              err_illdata_mess=' The number of frames and step for each frame are incompatible with this routine!'
              return
           end if
        end do

        cnorm=1.0
        rporc=100.0
        dh=h/real(ncell-1)

        !> Optionals arguments
        if (present(Cal))   correction=.true.
        if (present(vnorm)) cnorm=vnorm
        if (present(perc))  rporc=perc

        !> Allocating General Vectors and Arrays
        nf_max=maxval(PNumors(:)%nframes,mask=actlist)
        if (allocated(xx))  deallocate(xx)
        if (allocated(yy))  deallocate(yy)
        if (allocated(zz))  deallocate(zz)
        if (allocated(vv))  deallocate(vv)
        if (allocated(ncc)) deallocate(ncc)

        allocate( xx(ndet*nf_max,ncell,num))
        allocate( yy(ndet*nf_max,ncell,num))
        allocate( vv(ndet*nf_max,ncell,num))
        allocate( zz(ndet*nf_max,ncell))
        allocate(ncc(ncell,     num))

        xx=0.0
        yy=0.0
        zz=0.0
        vv=0.0
        ncc=0

        !call cpu_time(time2)
        !print*,'Tiempo de Check-Allocate memory:',time2-time1
        !call cpu_time(time2)

        num=0
        !> Loading Numors information in main variables
        do i=1,N
           if (.not. ActList(i)) cycle
           num=num+1

           !> Load Normalization value
           if (num==1 .and. .not. present(vnorm)) cnorm=PNumors(i)%tmc_ang(2,1)  ! Monitor normalization value

           !> Allocating
           if (allocated(x)) deallocate(x)
           if (allocated(z)) deallocate(z)

           allocate(x(ndet,PNumors(i)%nframes))          ! gamms angles
           allocate(z(ncell,ndet,Pnumors(i)%nframes))    ! Intensities
           x=0.0
           z=0.0

           if (.not. correction) then
              !> Total Points -> Calculated taking into account excluded detectors
              !ncc(:,num)=ndet*Pnumors(i)%nframes

              ! gamm Values
              !      v---frames
              x(ndet,:)=PNumors(i)%tmc_ang(4,:)
              do nt=ndet-1,1,-1
                 x(nt,:)=x(nt+1,:)-1.25
              end do

              ! Intensities
              nc=0
              do nt=1,ndet-1,2   !z(cells,det,frames) counts at each cell, detector and frame
                 z(:,nt,:)=Pnumors(i)%counts(nc+1:nc+ncell,:)
                 nc=nc+ncell+ncell
                 do k=ncell,1,-1
                    z(k,nt+1,:)=PNumors(i)%counts(nc-k+1,:)
                 end do
              end do

              !> Passing for Interpolation procedure only the active points
              do k=1,ncell
                 nc=0
                 do nt=1,ndet
                    if(Excl(nt)) cycle
                    do nf=1,Pnumors(i)%nframes
                       nc=nc+1
                       xx(nc,k,num)=x(nt,nf)
                       yy(nc,k,num)=z(k,nt,nf)
                       vv(nc,k,num)=1.0
                    end do
                 end do
                 ncc(k,num)= nc
              end do

           else
              !> Total Points -> Calculated taking into account excluded detectors
              !do k=1,ncell
              !   ncc(k,num)=count(Cal%Active(k,:))*Pnumors(i)%nframes
              !end do

              x(ndet,:)=PNumors(i)%tmc_ang(4,:)
              do nf=1,Pnumors(i)%nframes
                 ! gamms
                 x(:,nf)=x(ndet,nf)+Cal%PosX(:)

                 ! Intensities
                 fac=cnorm/PNumors(i)%tmc_ang(2,nf)
                 nc=0
                 do nt=1,ndet-1,2
                    z(:,nt,nf)=Pnumors(i)%counts(nc+1:nc+ncell,nf) *fac/Cal%effic(:,nt)
                    nc=nc+ncell+ncell
                    do k=ncell,1,-1
                       z(k,nt+1,nf)=PNumors(i)%counts(nc-k+1,nf)*fac/Cal%effic(k,nt)
                    end do
                 end do
              end do

              !> Passing for Interpolation procedure only the active points
              do k=1,ncell
                 nc=0
                 do nt=1,ndet
                    if (.not. Cal%Active(k,nt) .or. Excl(nt) ) cycle
                    do nf=1,Pnumors(i)%nframes
                       nc=nc+1
                       xx(nc,k,num)=x(nt,nf)
                       yy(nc,k,num)=z(k,nt,nf)
                       vv(nc,k,num)=cnorm /(PNumors(i)%tmc_ang(2,nf)*Cal%effic(k,nt))
                    end do
                 end do
                 ncc(k,num)=nc
              end do
           end if
        end do

        !call cpu_time(time1)
        !print*,'Tiempo de Lectura de Numors:',time1-time2
        !print*,'Tiempo medio de Lectura de Numors:',(time1-time2)/real(num)
        !call cpu_time(time1)

        !> Second Derivative
        if (allocated(d2yy)) deallocate(d2yy)
        allocate(d2yy(ndet*nf_max,ncell,num))
        d2yy=0.0
        xmin=360.0
        xmax=-360.0
        do k=1,ncell
           do i=1,num
              nc=ncc(k,i)
              if (nc == 0) cycle
              xmin=min(xmin,minval(xx(1:nc,k,i)))
              xmax=max(xmax,maxval(xx(1:nc,k,i)))
              d2yy(1:nc,k,i)= second_derivative(xx(1:nc,k,i),yy(1:nc,k,i),nc)
           end do
        end do

        !call cpu_time(time2)
        !print*,'Tiempo de Calculo Segunda derivada:',time2-time1
        !call cpu_time(time2)

        !> Minimum,Maximum and Number of Total points for Pat
        xmin=max(0.0,xmin)
        np=ndet*nf_max
        step=(xmax-xmin)/real(np-1)


        call Allocate_Pattern(Pat, np)

        Pat%Monitor=cnorm
        Pat%KindRad='neutron'
        Pat%ScatVar='2theta'
        Pat%instr='D2B'
        Pat%xmin=xmin
        Pat%xmax=xmax
        Pat%step=step

        !> First active Numor on the List
        i=ind(1)
        Pat%title=trim(PNumors(i)%title)
        Pat%TSet=PNumors(i)%conditions(1)
        Pat%TSample=PNumors(i)%conditions(3)
        Pat%wave(1)=PNumors(i)%wave

        Pat%x=0.0
        Pat%y=0.0
        Pat%sigma=0.0
        Pat%nd=0

        nc=nint(ncell*rporc*0.005)       ! rporc *0.01 /2.0
        nc=max(1,nc)
        nc=min(64,nc)
        nc_fin=65+(nc-1)

        !call cpu_time(time1)
        !print*,'Comenzamos Bucle de Puntos:',time1-time2
        !call cpu_time(time1)

        do i=1,np
           Pat%x(i)=xmin+(i-1)*step          ! Required 2Theta
           cos_2theta=cosd(Pat%x(i))

           do k=65, nc_fin
              if (k == 65) then
                 zh=dh*0.5
              else
                 zh=dh*0.5 + (k-65)*dh       ! distance of the cell to the equatorial plane
              end if
              nu=atan2(zh,radio)             ! Latitude angle
              cos_nu=cos(nu)
              gamm=acosd(cos_2theta/cos_nu) ! This is the gamm of the cell at the required 2theta

              do j=1,num                     ! Add numors

                 !First half of the cells

                 nc=ncc(k,j)
                 if (nc == 0) cycle
                 x1=minval(xx(1:nc,k,j))
                 x2=maxval(xx(1:nc,k,j))
                 if (gamm < x1 .or. gamm > x2) cycle
                 zc=Spline_Interpol(gamm,xx(1:nc,k,j),yy(1:nc,k,j),d2yy(1:nc,k,j),nc)
                 Pat%y(i)=Pat%y(i)+zc
                 if (.not. calibration) then
                    Pat%sigma(i)=Pat%sigma(i)+zc
                 else
                    if (abs(gamm-x2) <= 0.001) then
                       nk=nc
                    else
                       nk=locate(xx(1:nc,k,j),gamm)
                    end if
                    Pat%sigma(i)=Pat%sigma(i)+zc*vv(nk,k,j)
                 end if

                 Pat%nd(i)=Pat%nd(i)+1

                 !Second half of the cells

                 nk=128-k+1
                 nc=ncc(nk,j)
                 if (nc == 0) cycle
                 x1=minval(xx(1:nc,nk,j))
                 x2=maxval(xx(1:nc,nk,j))
                 if (gamm < x1 .or. gamm > x2) cycle
                 zc=Spline_Interpol(gamm,xx(1:nc,nk,j),yy(1:nc,nk,j),d2yy(1:nc,nk,j),nc)
                 Pat%y(i)=Pat%y(i)+zc
                 if (.not. calibration) then
                    Pat%sigma(i)=Pat%sigma(i)+zc
                 else
                    if (abs(gamm-x2) <= 0.001) then
                       nkk=nc
                    else
                       nkk=locate(xx(1:nc,nk,j),gamm)
                    end if
                    Pat%sigma(i)=Pat%sigma(i)+zc*vv(nkk,nk,j)
                 end if
                 Pat%nd(i)=Pat%nd(i)+1
              end do
           end do

           if (Pat%nd(i) > 0) then
              Pat%y(i)=real(ncell)*Pat%y(i)/real(Pat%nd(i))
              Pat%sigma(i)=real(ncell)*real(ncell)*abs(Pat%sigma(i))/real(Pat%nd(i)*Pat%nd(i))
           else
              Pat%y(i)=0.0
              Pat%sigma(i)=1.0
           end if
        end do

        !call cpu_time(time2)
        !print*,'Tiempo de calculo del Perfile:',time2-time1
        !print*,'Tiempo medio por Punto:',(time2-time1)/real(np)
        !call cpu_time(time2)

        Pat%npts=np
        Pat%ymin=minval(Pat%y)
        Pat%ymax=maxval(Pat%y)

        call cpu_time(time2)
        print*,'Total time for D2B-Numors treatment:',time2-time_ini

        return
    End Subroutine Adding_Numors_D2B_DiffPattern

  End Program get_data
