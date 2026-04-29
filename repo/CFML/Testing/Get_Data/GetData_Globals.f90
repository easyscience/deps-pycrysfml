 !!---- Module: GetData_Globals
 !!----
 !!---- Define the main variables for GetData_nxs
 !!----
 !!---- 24/02/2023
 !!
 Module GetData_Globals
    !---- Use Modules ----!
    Use CFML_GlobalDeps,      only: ops,ops_sep, Directory_Exists, Err_CFML, clear_error, &
                                    Set_CFML_DEBUG, CFML_debug
    Use CFML_Messages,        only: error_message,info_message
    Use CFML_Maths,           only: second_derivative, Spline_Interpol, locate
    Use CFML_ILL_Instrm_Data, only: Calibration_Detector_Type, POWDER_Numor_type, &
                                    Set_Instrm_Directory, Allocate_Numors, &
                                    Read_Numor, PowderNumors_To_DiffPattern, Read_Calibration_File,  &
                                    Set_Current_Orient, Current_Orient, Initialize_Numor
    Use CFML_DiffPatt,        only: DiffPat_E_Type,Allocate_Pattern
    Use CFML_Strings,         only: get_num, cut_string, l_case,u_case, Get_Separator_Pos
    Use Nexus_Mod,            only: read_nexus, nexus_type,err_nexus, war_nexus,err_nexus_mess, war_nexus_mess, &
                                    nxs_to_powder_numor, read_calibration
    use D2B_read_mod,         only: cfl_D2B_type

    !---- Variables ----!
    implicit none
    public

    !> Logicals
    logical                                           :: nxs                   ! True is nexus files are to be reaad
    logical                                           :: Verbose               ! Verbose flag
    logical                                           :: Powdat=.true.         ! Flag for Powder or SC
    logical                                           :: Skipnum               ! Skipping Numors
    logical                                           :: Yc                    ! Year-Cycle flag
    logical                                           :: Info                  ! Information flag
    logical                                           :: Add                   ! Add Temperature
    logical                                           :: LabelT                ! Add Temperature at Files name
    logical                                           :: Outconv               ! Output Data File conversion
    logical                                           :: wav_read=.false.      ! The UB matrix is read from file if .true.
    logical                                           :: angcor=.false.        ! The calibration positions of the cells are used or not in a banana detector
    logical                                           :: perm=.false.          ! The permutation for the alphas of the cells are used or not in a banana detector
    logical                                           :: xmin_read=.false.     ! The minimum angle for output has been read if true.
    logical                                           :: xmax_read=.false.     ! The maximum angle for output has been read if true.
    logical                                           :: ascii_cal=.false.     ! If true, the calibration file is in ASCII format

    !> Character
    character(len=1024)                               :: PathDir               ! Path for the Numor
    character(len=1024)                               :: FileIns               ! Instruction file
    character(len=1024)                               :: CalPath               ! Calibration File (+Path)
    character(len=40)                                 :: CodFil                ! CodeFile
    character(len=4)                                  :: Inst                  ! Instrument Name
    character(len=80)                                 :: ProgCompress          ! Compress/Uncompress Program
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
    integer                                           :: nsum=0                ! Number of numors to be summed

    !> Real
    real                                              :: dt                    ! Delta Temperature
    real                                              :: rnorm                 ! Monitor (or time) value
    real                                              :: valnorm               ! -rnorm (used for time normalization, D20)
    real                                              :: per=100.0             ! Percentage of central cells contributing
    real                                              :: zero_shift=0.0        ! Global Zero Shift
    real                                              :: Read_wav=0.0
    real                                              :: xang_min=0.0, xang_max=0.0
    real                                              :: Threshold             ! Factor for comparison of individual detectors with average
    real, dimension(3,3)                              :: Read_UB=0.0
    logical, dimension(128)                           :: excl_det=.false.      ! excl_det(i)=.true.  if detector i is to be excluded
    logical, dimension(max_cell)                      :: excl_cel=.false.      ! excl_cel(i)=.true.  if cell i is to be excluded
    integer                                           :: nexcl = 0             ! Number of detectors to be excluded
    integer                                           :: nexcel = 0            ! Number of cells to be excluded (Banana detector, D1B-D20)
    !> Type
    type(POWDER_Numor_type), dimension(:),allocatable :: PNum                  ! Numors for Powder Instrument
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
      write(unit=*,fmt="(a)") "  -threshold                 : Factor for comparison of individual counts with the average: count-average > Threshold * average "
      write(unit=*,fmt="(a)") "  -cod   Codefiles           : Prepends the content of 'codefiles' to the name of the files "
      write(unit=*,fmt="(a)") "                             : If not given, the instrument name is used for naming the files"
      write(unit=*,fmt="(a)") "  -det   n                   : Create a file with the scan measured by detector n"
      write(unit=*,fmt="(a)") "  -fil   file_instructions   : File containing the directives to run the get_data program"
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
      write(unit=*,fmt="(a)") "  -yc    year cycle          : Year (4 digits) and cycle(1-4) integer numbers (needs ILL DataBase)"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  Examples of use of this program:"
      write(unit=*,fmt="(a)") "  ================================"
      write(unit=*,fmt="(a)") "  get_data  -ins d1b -dir c:\bd_ill\data\d1b  -num 12341 12387 -cod ymn2 -add -fmt 0"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  get_data  -fil run_d20.inp"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  get_data  -fil run_d2b.cfl"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  The content of the file 'run_d20.inp' in the last example is formed by the four lines below:"
      write(unit=*,fmt="(a)") ' -ins D20 -dir "C:\Disk-D\Data\xxx\yyyy\D20" -num 897965  897972 -add &    '
      write(unit=*,fmt="(a)") ' -fmt 1 -cod "Feoxal"  -tor 60.0 -zer 2.53       &                         '
      write(unit=*,fmt="(a)") ' -cal "C:\Disk-D\Data\xxx\yyyy\D20\calib_d20.cal"      &                   '
      write(unit=*,fmt="(a)") ' -cex 867 868 869 870 871 872   2543 2544 2545 2546 2547 2548 2549 2550 2551  2552 '
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  WARNING: A file with instructions as above cannot have the extension .cfl"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  For D2B a CFL file with command keywords is compulsory:"
      write(unit=*,fmt="(a)") "  ======================================================="
      write(unit=*,fmt="(a)") "  Example of invoking the program: get_data -fil run_d2b.cfl "
      write(unit=*,fmt="(a)") "  The instrument file is needed for D2B and XtremeD "
      write(unit=*,fmt="(a)") "  D2B data can be treated only in Nexus mode, in the file run_d2b.cfl the instructions are:"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "INSTRM               d2b"
      write(unit=*,fmt="(a)") "INSTRUMENT_FILE      c:\My_geom_directory\d2b.geom"
      write(unit=*,fmt="(a)") "EXCL_DETS          31  56  122    ! Three detectors are excluded: those numbered 31, 56 and 122 "
      write(unit=*,fmt="(a)") "EXCL_CELLS  867 868 869 870       ! Cells that have to be excluded (only for D1B and D20)  "
      write(unit=*,fmt="(a)") "NSIGMA               3.00         ! Pixels of the same 2theta that deviates more than 3 x sigma(average) are excluded "
      write(unit=*,fmt="(a)") "VERBOSE                           ! This produceds more output in the screen"
      write(unit=*,fmt="(a)") "COMBINE              Nac_calib    ! Name of the final *.xys file with the powder pattern"
      write(unit=*,fmt="(a)") "NZ_INT               80           ! The 80 central pixels are considered for integration"
      write(unit=*,fmt="(a)") "NZ1-NZ2            41 87          ! Alternative to the previous command giving explicitly the interval to integrate"
      write(unit=*,fmt="(a)") "INTEG_1D                          ! The integration is performed using Vertical-Sraight sums, otherwise it is performed using 2Theta-arcs"
      write(unit=*,fmt="(a)") "MONITOR          50000.00         ! Monitor for normalization"
      write(unit=*,fmt="(a)") "SCALE_FAC            1.00"
      write(unit=*,fmt="(a)") "TTH_MIN              5.00"
      write(unit=*,fmt="(a)") "TTH_MAX            158.00"
      write(unit=*,fmt="(a)") " "
      write(unit=*,fmt="(a)") "EF_CUTOFF            0.4          ! Efficiencies outside the interval [1-0.4, 1+0.4] make the cells non-active"
      write(unit=*,fmt="(a)") "CALIBRATION_GEN     mantid        ! Mantid uses only Nexus files, one can put lamp and the calibration file is in ASCII"
      write(unit=*,fmt="(a)") " or "
      write(unit=*,fmt="(a)") "CALIBRATION_GEN     calib_1d      ! Uses a calibration file generated by D2B_calib (after running FullProf for determining shifts)"
      write(unit=*,fmt="(a)") " or "
      write(unit=*,fmt="(a)") "CALIBRATION_GEN      lamp          ! Uses a calibration file generated by LAMP"
      write(unit=*,fmt="(a)") "CALIBRATION_PATH    /mantid_workspace_1/workspace/values     !(only for mantid case)"
      write(unit=*,fmt="(a)") "CALIBRATION_FILE    c:\My_calibration_files\vanatest.nxs"
      write(unit=*,fmt="(a)") " or "
      write(unit=*,fmt="(a)") "CALIBRATION_FILE    c:\My_calibration_files\tubes_up.calib    !This is an ascii file to be used with calib_1d"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "SCAN_PATH          c:\My_Data\D2B\data\"
      write(unit=*,fmt="(a)") "NUMORS  564723  564732"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  Instead of providing the path and the numors one can directly use the number of scans and the corresponding names"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "SCANS  10  "
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564723.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564724.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564725.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564726.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564727.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564728.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564729.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564730.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564731.nxs"
      write(unit=*,fmt="(a)") "    c:\My_Data\d2b\data\564732.nxs"
      write(unit=*,fmt="(a)") "  "
      write(unit=*,fmt="(a)") "  "

    End Subroutine Manual

    !!----
    !!---- Subroutine CFL_to_Commands(cfl,cmdline)
    !!----   type(cfl_D2B_type),   intent(in) :: cfl
    !!----   character(len=*),     intent(out):: Cmd
    !!----
    !!---- 25/02/2023
    !!
    Subroutine CFL_to_Commands(cfl,cmd)
        !---- Arguments ----!
        type(cfl_D2B_type),   intent(in) :: cfl
        character(len=*),     intent(out):: Cmd
        !
        ! Local variables
        !
        integer :: i
        character(len=80) :: str
    !type, public :: cfl_D2B_type                                                                    -ins   instrument_name     : Instrument name (D1B, D1A,...)"
        !                                                                                             -num   num1  [num2 [step]] : Numors from num1 to num2 every step numors.
        !integer            :: nscans                                                                                             If step < 0 the multiples of |nstep| are skipped"
        !integer            :: nz_int                                                                       n1 n2 ... [n9 n10]  : Up to 5 blocks of range in Numors (2 blocks is the minimum)"
        !real               :: scale_fac                                                             "
        !real               :: tth_min                                                               -dir   pathdir             : The program search the data in directory 'pathdir' "
        !real               :: tth_max                                                               "
        !real               :: nsigma                                                                (Optionals):"
        !real               :: Norm_Monitor                                                          -nxs                       : Tells the program to use NeXus files of the form nnnnnn.nxs"
        !real               :: ef_cutoff                                                             -add                       : Adds all Numors"
        !logical            :: is_tth_min = .false.                                                  -add   [N n ]              : Adds blocks of n Numors between num1 and num2"
        !logical            :: is_tth_max = .false.                                                  -add   [T dT]              : Adds Numors with temperatures within a range of +/- dT "
        !logical            :: calibration,combine,raw                                               -cal   filename            : File name of calibration data for the instrument (including path)"
        !logical            :: suma                                                                  -zer   zero_shift          : Zero-shift to be applied before writing the data"
        !logical            :: is_ef_cutoff                                                          -xmin  value               : Minimum 2theta angle for output"
        !logical            :: single                                                                -xmax  value               : Maximum 2theta angle for output"
        !logical            :: align                                                                 -ang                       : Angular positions of calibration file are used for a banana detector"
        !logical            :: verbose                                                               -prm                       : Permutation of alphas according to angular positions is used for a banana
        !logical            :: monitor ! if true, a given Norm_Monitor                               -cod   Codefiles           : Prepends the content of 'codefiles' to the name of the files "
        !character(len=6)   :: calib_gen                                                                                        : If not given, the instrument name is used for naming the files"
        !character(len=12)  :: instrument_name                                                       -det   n                   : Create a file with the scan measured by detector n"
        !character(len=20)  :: suffix                                                                -fil   file_instructions   : File containing the directives for run get_data program"
        !character(len=512) :: scan_path,calib_path,calib_file,combine_name                          -fmt   0|1|2               : 0 Convert the Numors to INSTR=0 "
        !integer, dimension(:,:),          allocatable :: scan_list                                                             : 1 Convert the Numors to XYSigma Format (this is the default value)"
        !character(len=512), dimension(:), allocatable :: scans                                                                 : 2 Convert the Numors to INSTR=5 Format (appropriate for D1B/D20)"
        !character(len=:),                 allocatable :: label_sum                                  -inf                       : The program reads only the headers and produces a file called "
        !                                                                                                                        : 'info_data_inst.inf', listing relevant info on the given numors"
    !end type cfl_D2B_type                                                                           -lab   T                   : Appends the Temperature(in 0.01K) to the name of the files "
      !                                                                                              -nor   value               : Normalization value for Monitor normalization"
      !                                                                                              -tor   value               : Normalization value for Time normalization"
      !                                                                                              -dex   n_1  n_2 ...nexcl   : Identification numbers of detectors to be excluded "
      !                                                                                               -cex   n_1  n_2 ...nexcel  : Identification numbers of cells to be excluded (banana detector)"
        write(str,"(a,2i8)") "-num ",cfl%num1,cfl%num2
        cmd="-ins "//trim(cfl%instrument_name)//"  "//trim(str)//"  "//"-dir "//trim(cfl%scan_path)
        cmd=trim(cmd)//"  -nxs  -fmt 1"
        i=len_trim(cfl%combine_name)
        if(i /= 0) cmd=trim(cmd)//" -add "//" -cod "//trim(cfl%combine_name)
        if(cfl%calibration) then
            cmd=trim(cmd)//" -cal "//trim(cfl%calib_file)
        end if
        if(cfl%monitor) then
            write(str,"(a,f12.2)") " -nor ",cfl%norm_monitor
            cmd=trim(cmd)//trim(str)
        end if
    End Subroutine CFL_to_Commands
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
        integer, dimension(1000) :: ivet
        integer                 :: i,j,k,na,iv,n,nlong

        real,    dimension(1000) :: vet


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

               case("-asc")    !Type of calibration file
                 ascii_cal=.true.

               case("-threshold")
                  call Get_Num(line,vet,ivet,iv)
                  if(iv > 0) then
                     threshold=vet(1)
                  else
                     threshold=3.0
                  end if

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
        end if

        if (len_trim(codfil) <=0) codfil=trim(inst)

        return
    End Subroutine Process_CmdLine

 End Module GetData_Globals
