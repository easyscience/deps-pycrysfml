!!----
!!---- Module: Racer_Files_Mod
!!----   Info: Collections of subroutines and global variables used for handling IO in Racer.
!!----
!!---- History
!!----    Update: January - 2010
!!----
!!--..    Introduction of a new file format for the configuration file. The old one was maintained for
!!--..    backward compatibility (Eric Pellegrini)
!!--..
!!--..    Some Code Cleaning:
!!--..        - Unused Variables Removed
!!--..        - Only The Necessary Variables/Functions/Procedures Of The Different Modules Are Declared
!!----
!!----    Update: December - 2006
!!----
!!----    Code converted from f77 to f90 using To_F90 by Alan Miller. Refactoring
!!----    done by J. Rodriguez-Carvajal in December 2006
!!----
!!---- Dependencies
!!----
!!---- Variables
!!----    Racer_File_Error
!!----    Racer_File_Error_Mess
!!----    Snum
!!----
!!---- Procedures
!!----    Functions:
!!----       Openf2
!!----
!!----    Subroutines:
!!----       Format_Parameter_Line
!!----       Racer_Files
!!----       Race_Read_Scan
!!----       Race_Setup
!!----       Read_New_Style_Cfg_File
!!----       Read_Old_Style_Cfg_File
!!----
!!
Module Racer_CFML_Files

    Use CFML_GlobalDeps,       Only: Err_CFML, clear_error, Ops_Sep

    Use CFML_Messages,         Only: Error_Message

    !Use CFML_IO_Formats,       Only: File_To_FileList, File_List_Type

    Use CFML_Strings ,         Only: Get_Filename, Get_num, Get_Words, L_case, U_case, File_Type, Reading_File

    Use CFML_Ill_Instrm_Data,  Only: Sxtal_Numor_Type, Read_Numor, &
                                     Set_Instrm_Directory, Initialize_Data_Directory, Get_Absolute_Data_Path, &
                                     Instrm_directory,Initialize_Numor

    Use Racer_CFML_Mod,        Only: Alphas, Config, Default_Configuration, I_Ans, I_Col, I_Flg, I_Fly, I_Lib   , &
                                     I_Msk, I_Raf, I_Twn, I_Ubm, I_Usr, Mask, Max_Pts, Numors, Racer_Config_Type, &
                                     Racer_CFML_Mod_Error, Racer_CFML_Mod_Error_Mess, Read_Detector_Efficiency_Matrix, &
                                     Vraw, Vval, Zgamma, Zmon, Zomega, Ztim,n_hor, n_ver, Dx, Dy

    Use nexus_mod

    Implicit None

    Private

    ! The public subroutines and functions.
    Public :: Openf2, Racer_Files, Race_Read_Scan, Race_Setup, Read_New_Style_Cfg_File, Read_Old_Style_Cfg_File

    Logical, Public             :: Racer_File_Error = .False.

    Character (Len=512), Public :: Racer_File_Error_Mess = "", Cfg_Filename = "", Cfg_Basename = ""

    Type(Sxtal_Numor_Type), Public  :: Snum

    Type(Racer_Config_Type), Public :: Default_Config

    Integer, Dimension(:,:,:),Allocatable, public :: Map !Array use to mask the pixels max(n_ver,n_hor,Max_Pts)
    Integer, Dimension(:,:,:),Allocatable, public :: Npb !max(2,n_ver,n_hor)
    Real,    Dimension(:,:),  Allocatable, public :: Bgp, Bgerr !max(n_ver,n_hor)

 Contains
!
!- Reads the parameter file
!----------------------------------------------------------------------
    Subroutine Race_Setup()
!----------------------------------------------------------------------
        Character (Len=132)    :: Recordl
        Integer                :: I, J, Ier = 0, Ninp, Len_Cmdline
        Integer, Dimension(20) :: Iitems
        Real,    Dimension(20) :: Aitems
        Character(Len = 512)   :: Cmdline

        !---- Local Variables ----!
        Character (Len=132)  :: Line
        Type(File_Type)      :: File_Contents

        Call Default_Configuration(Default_Config)
        Config = Default_Config

        Call Get_Command_Argument(1, Cmdline, Len_Cmdline)

        If (Len_Cmdline == 0) Call Error_Message('No Configuration File', Routine = 'Race_Setup', Fatal = .True.)

        Cfg_Filename = Cmdline

        !Call Get_Basename(Cfg_Filename,Ops_Sep, Cfg_Basename)

        Cfg_Basename=Get_Filename(Cfg_Filename)
        i=index(Cfg_Filename,".usr")
        if(i == 0) Cfg_Filename=trim(Cfg_Filename)//".usr"

        i=index(Cfg_Basename,".")
        if(i /= 0) Cfg_Basename= Cfg_Basename(1:i-1)

        !Call File_To_FileList(Cfg_Filename, File_Contents)
        File_contents=Reading_File(Cfg_Filename)

        If (Err_CFML%ierr /= 0) Call Error_Message(Err_CFML%Msg, Routine = 'Race_Setup', Fatal = .True.)

        Line = U_case(Adjustl(File_Contents%Line(1)%str))

        ! If the configuration file starts with racer then it is parsed as a new style configuration file.
        If (Trim(Line) == 'RACER') Then
            Call Read_New_Style_Cfg_File(File_Contents, Config)

        ! Otherwise it is parsed as an old style Configuration File.
        Else
            Call Read_Old_Style_Cfg_File(File_Contents, Config)

        End If

        ! Allocate arrays depending on the number of pixels of the detector
        n_ver=Config%np_ver
        n_hor=Config%np_hor
        Dx=Config%Dx
        Dy=Config%Dy
        if(allocated(Mask))    deallocate(Mask)
        if(allocated(Bgp))     deallocate(Bgp)
        if(allocated(Bgerr))   deallocate(Bgerr)
        if(allocated(Alphas))  deallocate(Alphas)
        Allocate(Mask(n_ver,n_hor), Bgp(n_ver,n_hor), Bgerr(n_ver,n_hor),Alphas(n_ver,n_hor) )
        Mask=0; Bgp=0.0; Bgerr=0.0;  Alphas=1.0

        if(allocated(Vraw))    deallocate(Vraw)
        if(allocated(Vval))    deallocate(Vval)
        if(allocated(Map))     deallocate(Map)
        if(allocated(Npb))     deallocate(Npb)

        Allocate(Vraw(n_ver,n_hor,Max_Pts), Vval(n_ver,n_hor,Max_Pts), Map(n_ver,n_hor,Max_Pts))
        Allocate(Npb(2,n_ver,n_hor))
        Vraw=0.0; Vval=0.0; Map=0; Npb=0

        call welcome()
        If (Racer_File_Error) Call Error_Message(Trim(Racer_File_Error_Mess), Fatal = .True.)
        Call Racer_Files(Cfg_Basename)

        Call Read_Detector_Efficiency_Matrix('alphas.dat')
        If (Racer_CFML_Mod_Error) Then

            Call Error_Message(Trim(Racer_CFML_Mod_Error_Mess), Routine = 'Read_Detector_Efficiency_Matrix')
            Racer_CFML_Mod_Error = .False.
            Racer_CFML_Mod_Error_Mess = ""

        End If

        Write( *,"(/,A)") ' General Parameters:'
        Write(I_Ans,"(/,A)") ' General Parameters:'
        If (Config%Scale == 0) Then
            Write(Recordl,"(A,I8,A)") ' The counts will be scaled to ',Config%Monitor, &
                                   ' Counts on the Monitor.'
        Else
            Write(Recordl,"(A,I8,A)") ' The counts will be scaled to ',Config%Monitor, &
                                   '/1000 Sec.'
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        If (Config%TReg == 0) Then
            Write(Recordl,"(A)") ' The sample temperature will be recorded on the Output File.'
        Else
            Write(Recordl,"(A)") ' The regulation temperature will be recorded on the Output File.'
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        Write( *,"(/,A)") ' Integration Parameters:'
        Write(I_Ans,"(/,A)") ' Integration Parameters:'
        Write(Recordl,"(A,F5.2,A)") ' Contour the peak at ', Config%Cont,' of Maximum to find Centroid.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(A,F6.0,A)") ' The Integration Volume for Weak Reflections is ',Config%ActVol, ' Pixels,'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(A,F3.0,A)") '  Multiplied by ',Config%VolFac,' for Strong Reflections.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(A,F5.2,A)") ' Reflections with Sigbr less than ',Config%SigBR,' are treated as Weak.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(A,F5.2,A)") ' Reflections with Fill Fractions less than ',Config%Fill,' are treated as Weak.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(A,F5.2,A)") ' Reflections whose Calc. and Obs. Centres differ by ',Config%DiffLim,' Deg. are Flagged.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        Write( *,"(/,A)") ' Control and Output parameters:'
        Write(I_Ans,"(/,A)") ' Control and Output parameters:'

        If (Config%IAddLib == 1) Then
            Write(Recordl,"(A)") ' Treat only the Strong Reflections, and Fill the Library.'
        Else
            Write(Recordl,"(A)") ' Freeze the Library and treat all Reflections'
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        If (Config%IRefine > 0) Then
            Write(Recordl,"(A,I4,A)") ' Print a nudged UB=Matrix after each ',Config%IRefine,' Reflections.'
        Else If (Config%IRefine < 0) Then
            Write(Recordl,"(A)") ' Print a nudged UB-Matrix after all Reflections are treated.'
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        If (Config%IPrint == 1) Then
            Write(Recordl,"(A)")' Print Integration Details and Modelling Contours.'
        Else If (Config%IPrint == 2) Then
            Write(Recordl,"(A)") ' Print Integration and Modelling Contours.'
        Else If (Config%IPrint == 3) Then
            Write(Recordl,'('' Print Integration Details and Profile.'')')
        Else
            Write(Recordl,'('' Print Other??'')')
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        If (Config%ICard == 1) Then
            Write(Recordl, Fmt='(A)') ' Write floating H K L (2 decimal figures) to '//Trim(Cfg_Basename)//'.col file.'
        Else If (Config%ICard == 2) Then
            Write(Recordl, Fmt='(A)') ' Write floating H K L (4 decimal figures) to '//Trim(Cfg_Basename)//'.col file.'
        Else
            Write(Recordl, Fmt='(A)') ' Write integer H K L to '//Trim(Cfg_Basename)//'.col file.'
        End If

        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        Write( *,"(/,A)") ' Masking Parameters:'
        Write(I_Ans,"(/,A)") ' Masking Parameters:'
        Write(Recordl,"(2(2(A,I2),A))") ' Consider just Pixels ',Config%Imin,' to ',Config%Imax,' Horizontally,',&
                           ' Pixels ',Config%Jmin,' to ',Config%Jmax,' Vertically,'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl
        Write(Recordl,"(2(A,I2),A)") '  and Steps ',Config%Kmin,' to ',Config%Kmax,'.'
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        Do
            Read(Unit=I_Usr,Fmt="(A)",Iostat=Ier) Line
            Call Get_num(Line,Aitems,Iitems,Ninp)
            If (Ninp /= 0 .Or. Ier/=0 )  Exit
        End Do

        If (Config%N_Neigh > 0) Then
            Write(Recordl,"(A,3(F5.2,1X,F5.2,1X,F5.2,3X))") ' Nearest Neighbours: ',Config%Neigh(:,1:3)
            Write( *, '(A)') Recordl
            Write(I_Ans, '(A)') Recordl
            Write(Recordl,"(21X,3(F5.2,1X,F5.2,1X,F5.2,3X))") Config%Neigh(:,4:6)
            Write( *, '(A)') Recordl
            Write(I_Ans, '(A)') Recordl
        End If

        ! The Next Line Will Contain The Sample-Detector Distance, And Maybe The
        ! Offsets, And Maybe The Wavelength.
        If (Config%N_Neigh > 0) Then
            Call Get_num(Line,Aitems,Iitems,Ninp)
        End If

        Write( *,"(/,A)") ' Offsets and Wavelength:'
        Write(I_Ans,"(/,A)") ' Offsets and Wavelength:'
        Write(Recordl,"(A,F5.1,3(A,F6.3))") ' Sample-Detector Dist.: ',Config%DRad, &
                                            '.  Offsets: Detx: ',Config%YOffset,&
                                            ' Detz: ',Config%XOffset,' Omega: ',Config%OmegaOffset
        Write( *, '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        If (Config%Wavelen > 0.0) Then
            Write(Recordl,"(A,F7.4)") ' Treat with Wavelength : ',Config%Wavelen
            Write( *, '(A)') Recordl
            Write(I_Ans, '(A)') Recordl
        End If

        If (Config%UB_Set) Then
            Write( *,"(/,A,3F10.6)") ' Processing UB-Matrix:',(Config%UB(1,J),J=1,3)
            Write(I_Ans,"(/,A,3F10.6)") ' Processing UB-Matrix:',(Config%UB(1,J),J=1,3)
            Write( *,"(22X,3F10.6)")  ((Config%UB(I,J),J=1,3),I=2,3)
            Write(I_Ans,"(22X,3F10.6)")  ((Config%UB(I,J),J=1,3),I=2,3)

        Else
            Write ( *,"(/,A)") ' Treatment performed with measurement UB-Matrix.'
            Write(I_Ans,"(/,A)") ' Treatment performed with measurement UB-Matrix.'
        End If

        If (Config%Twin_UB_Set) Then
            Write( *,"(/,A,3F10.6)") ' UB-Matrix of Twin   :',(Config%Twin_UB(1,J),J=1,3)
            Write(I_Ans,"(/,A,3F10.6)") ' UB-Matrix of Twin   :',(Config%Twin_UB(1,J),J=1,3)
            Write( *,"(22X,3F10.6)")  ((Config%Twin_UB(I,J),J=1,3),I=2,3)
            Write(I_Ans,"(22X,3F10.6)")  ((Config%Twin_UB(I,J),J=1,3),I=2,3)

            ! Open The Output File For The Twinned Reflections
            If (.Not.Openf2 (I_Twn, Cfg_Basename, '.twn', 'Append')) Then
                If (.Not.Openf2 (I_Twn, Cfg_Basename, '.twn', 'New')) Stop
            End If

        End If

        If (Len_Trim(Config%Data_Directory) == 0) Then
            If (Config%IYearCycle /= -1) Then
                Write(*,"(/,A,A8,A,I3.3,/)") ' Treating Data from ',Trim(Config%Instrument),' Cycle ',Config%IYearcycle
                Write(I_Ans,"(/,A,A8,A,I3.3,/)") ' Treating Data from ',Trim(Config%Instrument),' Cycle ',Config%IYearcycle
            Else
                Write(*,"(/,A,A8,A,/)") ' Treating Data from ',Trim(Config%Instrument),' Unknown Cycle'
                Write(I_Ans,"(/,A,A8,A,/)") ' Treating Data from ',Trim(Config%Instrument),' Unknown Cycle'
            End If
        Else
            Write(*,"(/,A,A/)") ' Treating Data from Directory ',Trim(Config%Data_Directory)
            Write(I_Ans,"(/,A,A,/)") ' Treating Data from Directory ',Trim(Config%Data_Directory)
        End If

        ! Print Out Other Messages.
        If (Config%DeadTime == 0.0) Then
            Recordl=" No Dead-Time Correction Applied"

        Else if (Trim(Config%Instrument) /= 'd9' .And. Trim(Config%Instrument) /= 'd10') Then
            Recordl=" No Dead-Time Correction implemented for "//Trim(Config%Instrument)

        Else
            Write(Recordl,"(A,F11.8,A)") ' Dead-Time is ',Config%DeadTime,' Sec.'
        End If

        Write( *,    '(A)') Recordl
        Write(I_Ans, '(A)') Recordl

        Return

    End Subroutine Race_Setup

    Subroutine welcome(lun)
    	integer,optional :: lun
    	integer :: ioutput
    	ioutput=6
    	if(present(lun)) ioutput=lun
        !!----   Integration of Single-Crystal Reflections Using Area Multidetectors
        !!----   by Clive Wilkinson, Hanna W. Khamis, Robert F. D. Stansfield and Garry J. Mcintyre
        !!----   Journal of Applied Crystallography 21, 471-478 (1988)
        Write(ioutput,"(/,A)") '------------------------------------------------------------------------------------'
        Write(ioutput,"(A)")   '                                 PROGRAM: Racer                                     '
        Write(ioutput,"(A)")   '                                                                                    '
        Write(ioutput,"(A)")   '  Reference:                                                                        '
        Write(ioutput,"(A)")   '  Integration of Single-Crystal Reflections Using Area Multidetectors               '
        Write(ioutput,"(A)")   '  by Clive Wilkinson, Hanna W. Khamis, Robert F. D. Stansfield and Garry J. Mcintyre'
        Write(ioutput,"(A)")   '  Journal of Applied Crystallography 21, 471-478 (1988)                             '
        Write(ioutput,"(A)")   '                                                                                    '
        Write(ioutput,"(A)")   '  This version of the program results from the refactoring of the 2006 version      '
        Write(ioutput,"(A)")   '         Code converted to F95 and refactored by J.Rodriguez-Carvajal               '
        Write(ioutput,"(A)")   '      Modified by J.Rodriguez-Carvajal and Eric Pellegrini (up to october 2011)     '
        Write(ioutput,"(A)")   '        This new version is able to read the ILL NEXUS files from d9 and d10        '
        Write(ioutput,"(A)")   '         Version ILL-February-2023 (Nebil A. Katcho and J.Rodriguez-Carvajal)       '
        Write(ioutput,"(A)")   '------------------------------------------------------------------------------------'

        Write(ioutput,"(/,2(a,i3))")    ' Detector dimensions.  Number of pixels: ',n_ver," x ",n_hor
        Write(ioutput,"(2(a, f6.3),a)") '                             Pixel size: ',Dy," x ",Dx," (mm^2)"
        Write(ioutput,"(a)") " "
    End Subroutine welcome

    Subroutine Racer_Files(Namen)
        !---- Arguments ----!
        Character(Len = *),  Intent(In) :: Namen

        !---- Local Variables ----!
        Integer :: N,Ios,J,Isum,I
        Character(len=10) :: forma
        forma="(1X, 32I1)"
        write(forma(5:7),"(i3)") n_hor

        If (.Not. Openf2 (I_Ans, Namen, '.ans', 'Unknown')) Stop
        call welcome(I_Ans)

        If (.Not. Openf2 (I_Col, Namen, '.col', 'Append')) Then
            If (.Not. Openf2 (I_Col, Namen, '.col', 'New')) Stop
        End If

        If (.Not. Openf2 (I_Raf, Namen, '.raf', 'Append')) Then
            If (.Not. Openf2 (I_Raf, Namen, '.raf', 'New')) Stop
        End If

        If (.Not. Openf2 (I_Lib, Namen, '.lib', 'Old')) Then
             If (.Not. Openf2 (I_Lib, Namen, '.lib', 'New')) Stop
        End If

        If (.Not. Openf2 (I_Flg, Namen, '.flg', 'Unknown')) Stop

        If (.Not. Openf2 (I_Ubm, Namen, '.ubm', 'Unknown')) Stop

        If (.Not. Openf2 (I_Fly, Namen, '.fly', 'Unknown')) Stop

!-       Mask File - If It'S There Read It
        If (Openf2 (I_Msk, Namen, '.msk', 'Old')) Then
            N=1
            Ios=0
            Do While (N <= n_ver .And. Ios == 0)
                Read(I_Msk,forma,Iostat=Ios) (Mask(N,J),J=1,n_hor)
                N=N+1
            End Do
            Rewind (I_Msk)
        Else
            If (.Not. Openf2 (I_Msk, Namen, '.msk', 'Unknown')) Stop
        End If

        Isum=Sum(Mask)

        If (Isum == 0) Then
            Do J=1,n_hor
                Do I=1,n_ver
                    Mask(I,J)=1
                End Do
            End Do
        End If

        Do I=n_ver,1,-1
            Write(I_Msk,forma) (Mask(I,J),J=1,n_hor)
        End Do

        Close(Unit=I_Msk)

        Return

    End Subroutine Racer_Files

    ! Read The Scan Data.
    Subroutine Race_Read_Scan(Numor)
        !---- Arguments ----!
        Integer, Intent(In) :: Numor

        !---- Local Variables ----!
        character(len=512)     :: Path
        Real, Dimension(n_ver) :: Atot
        Real, Dimension(n_hor) :: Ctot
        Integer                :: Np, I, Ia, Ic
        Real                   :: Xtim, Total,corr !Tmscal, already included in the reading of Numor
        Type(nexus_type)       :: nexus

        if(len_trim(Config%Data_Directory) > 0) then
          write(unit=Path,fmt="(a,i6.6)") trim(Config%Data_Directory),numor
        else
          call Get_Absolute_Data_Path (Numor, trim(Config%Instrument), Path)
        end if

        If(config%Racer_nexus) Then
           Path=trim(Path)//".nxs"
           call read_nexus(trim(Path),nexus)
           if(err_nexus) then
               Racer_File_Error = .True.
               Write(Racer_File_Error_Mess,"(A,1X,I6.6,1X,A)") ' !!! '//Trim(err_nexus_mess)//' Numor', Numor, 'Skipped !!!'
               err_nexus=.false.
               err_nexus_mess=" "
               return
           end if
           !Assign values to Snum
           call Initialize_Numor(Snum,7, NFrames=nexus%nf)
           If (nexus%Nf > Max_Pts) Then
               Write(Racer_File_Error_Mess,"(2(A,1X,I5),A,1X,I6.6,1X,A)") ' !!! Too Many Points In Numor :', nexus%Nf, &
                                                                         'Points (Max =' , Max_Pts, '). Numor', Numor, 'Skipped !!!'
               Racer_File_Error = .True.
               Return
           End If

           Do Np=1,nexus%Nf

               if(nexus%is_timef) then
                 Ztim(Np)=nexus%Timef(Np)
                 Xtim=Ztim(Np)
               else
                 Ztim(Np)=1
                 Xtim=1
               end if

               if(nexus%is_total_counts) then
                  Total=nexus%total_counts(Np)
               else
                  Total=sum(nexus%counts(:,:,Np))
               end if

               if(nexus%is_monitor) then
                  Zmon(Np)=nexus%monitor(Np)
               else
                  Zmon(np)=0.0
               end if

               if(nexus%is_omega) then
                 Zomega(Np)=nexus%angles(3,Np)
               else
                 Zomega(Np)=0.0
               end if

               if(nexus%is_gamma) then
                  Zgamma(Np)=nexus%angles(4,Np)
               else
                  Zgamma(Np)=0.0
               end if

               Snum%Tmc_Ang(1,Np)=nexus%timef(Np)
               Snum%Tmc_Ang(2,Np)=nexus%monitor(Np)
               Snum%Tmc_Ang(3,Np)=nexus%total_counts(Np)
               Snum%Tmc_Ang(4,Np)=nexus%angles(3,Np)
               Snum%Tmc_Ang(5,Np)=nexus%angles(4,Np)
               Snum%Conditions(1)=nexus%setp_temperature
               Snum%Conditions(2)=nexus%reg_temperature
               Snum%Conditions(3)=nexus%temperature
               Snum%Conditions(4)=0.0
               Snum%Conditions(5)=nexus%magnetic_field

               Vraw(:,:,Np)=nexus%Counts(:,:,Np)  ! (nz,nx,nf)  !Correction of alphas (Not applied)

               call Correct_Dead_time()

           End Do

           Snum%Scans(2)=nexus%scan_step
           Snum%wave=nexus%wave
           Snum%hmin=nexus%reflection
           Snum%ub=nexus%ub
           Snum%icalc=1
           if(nexus%geometry == "NB")  Snum%icalc=2
           do i=1,5
              Snum%angles(i)=0.5*(nexus%angles(i,1) + nexus%angles(i,nexus%nf))       !Phi,Chi,Omega,2Theta(Gamma), Psi
           end do
           !write(*,"(a,5f8.3)") "Phi,Chi,Omega,2Theta(Gamma), Psi : ",Snum%angles(1:5)
           Snum%cpl_fact=abs(Zgamma(Config%Kmax)-Zgamma(1))/abs(Zomega(Config%Kmax)-Zomega(1))

           if(nexus%scan_type == "omega") then
             Snum%Manip=2
            else if(nexus%scan_type == "phi") then
             Snum%Manip=4
             Snum%cpl_fact=(nexus%angles(4,Config%Kmax)-nexus%angles(4,1))/(nexus%angles(1,Config%Kmax)-nexus%angles(1,1))
           else if(nexus%scan_type == "canne") then
             Snum%Manip=2
           end if

        Else

           Call Read_Numor(trim(Path),trim(Config%Instrument), Snum)
           If (Err_CFML%IErr /= 0) Then
               Racer_File_Error = .True.
               Write(Racer_File_Error_Mess,"(A,1X,I6.6,1X,A)") ' !!! '//Trim(Err_CFML%Msg)//' Numor', Numor, 'Skipped !!!'
               Err_CFML%Ierr = 0
               Err_CFML%Flag = .False.
               Err_CFML%Msg = ""
               Return
           End If

           If (Snum%Nframes > Max_Pts) Then
               Write(Racer_File_Error_Mess,"(2(A,1X,I5),A,1X,I6.6,1X,A)") ' !!! Too Many Points In Numor :', Snum%Nframes, &
                                                                         'Points (Max =' , Max_Pts, '). Numor', Numor, 'Skipped !!!'
               Racer_File_Error = .True.
               Return
           End If

           Do Np=1,Snum%Nframes
               Ztim(Np)=Snum%Tmc_Ang(1,Np) !Xtim
               Xtim=Ztim(Np)
               Total=Snum%Tmc_Ang(3,Np)
               Zmon(Np)=Snum%Tmc_Ang(2,Np) !Xmon

               If (Snum%Nbang > 0) Then                 !   1    2     3      4    5
                   Zomega(Np)=Snum%Tmc_Ang(4,Np)!*0.001 ! Xtim  Xmon  Ctot  Ang1  Ang2 (Angles Are Provided In 1/Thousands Of Degree)
               Else                          ! 1   2    3      4            5  (Rval(4:8))
                   Zomega(Np)=Snum%Angles(3)  !Phi,Chi,Omega,2Theta(Gamma), Psi
               End If

               If (Snum%Nbang > 1) Then
                   Zgamma(Np)=Snum%Tmc_Ang(5,Np) !*0.001  !Corrected On Reading Numors
               Else
                   Zgamma(Np)=Snum%Angles(4) !Rval(7)
               End If
               !write(*,"(i6,2f14.4)") Np,Zgamma(Np),Zomega(Np)
               !
               !-------------------------------------------------------------------
               !       Put The Count Data In The Correct Sequence, And Apply The
               !       Efficiency Correction.
               !-------------------------------------------------------------------
               !
               ! Before 1999 The data were written horizontal line by horizontal line; From
               ! 1999 onwards they were written vertical line by vertical line for both D9
               ! and D10, in both cases starting from corner at high Gamma, Low Nu.
               ! (to confirm June 1999)
               !
               I=0
               If (Config%IYearcycle > 701 .And. Config%IYearcycle < 991) Then

                   Do Ia=1,n_ver
                       Do Ic=1,n_hor
                           I=I+1
                           Vraw(Ia,Ic,Np)=Snum%Counts(I,Np) * Alphas(Ia,Ic)
                       End Do
                   End Do

               Else

                   Do Ic=1,n_hor
                       Do Ia=1,n_ver
                           I=I+1
                           Vraw(Ia,Ic,Np)=Snum%Counts(I,Np) * Alphas(Ia,Ic)
                       End Do
                   End Do

               End If

               call Correct_Dead_time()

           End Do
        End If

      Contains

        Subroutine Correct_Dead_time()
           !---------------------------------------------------------------------
           !         Correct Raw Data For Dead-Time
           !---------------------------------------------------------------------
           !
           !         Before 1999 the time was recorded in hundredths of seconds; from
           !         1999 onwards it was recorded as thousandths of seconds for both D9
           !         and D10  (To Confirm April 2000).  For D9 the dead-time is calculated
           !         using the larger of the total counts on the 'Anode' and 'Cathode'
           !         intersecting at the pixel being corrected; for D10 it is calculated
           !         using the Total Detector Count.
           !
           If (Config%DeadTime > 0.0) Then
               !Tmscal=0.001
               !If (Config%IYearcycle > 701 .And. Config%IYearcycle < 991) Tmscal=0.01
               !corr=Config%DeadTime/(Xtim*Tmscal)
               corr=Config%DeadTime/Xtim
               !write(*,*) Xtim, Total, corr, Total*corr
               If (Trim(Config%Instrument) == 'd9') Then
                   ! For D9 First Find The Totals Along The 'Anodes' And 'Cathodes',
                   Atot=0.0
                   Ctot=0.0

                   Do Ia = 1,n_ver
                       Do Ic = 1,n_hor
                           Atot(Ia) = Atot(Ia) + Vraw(Ia,Ic,Np)
                           Ctot(Ic) = Ctot(Ic) + Vraw(Ia,Ic,Np)
                       End Do
                   End Do

                   ! Then correct the individual counts.

                   Do Ia = 1,n_ver
                       Do Ic = 1,n_hor
                           Vraw(Ia,Ic,Np) = Vraw(Ia,Ic,Np)/(1.0 -  Max(Atot(Ia),Ctot(Ic))*corr)
                       End Do
                   End Do

               Else If (Trim(Config%Instrument) == 'd10') Then
                   ! For D10 Go Straight To The Correction.
                   Do Ia = 1,n_ver
                       Do Ic = 1,n_hor
                           Vraw(Ia,Ic,Np) = Vraw(Ia,Ic,Np)/(1.0 - Total*corr)
                       End Do
                   End Do

               End If

           End If
        End Subroutine Correct_Dead_time

    End Subroutine Race_Read_Scan

    Logical Function Openf2 (Lun, Namen, Ext, Flag)
        !---- Arguments ----!
        Integer,          Intent(In)       :: Lun
        Character(Len=*), Intent(In)       :: Namen, Ext, Flag

        !---- Local Variables ----!
        Character(Len=Len(Namen)+Len(Ext)) :: Filen
        Integer                            :: Ios

        Filen=Trim(Namen)//Trim(Ext)

        If (Flag == 'Append') Then
            Open(Unit = Lun, File = Trim(Filen), Status = 'Old', Iostat = Ios, Position = Flag)

        Else
            Open(Unit = Lun, File = Trim(Filen), Status = Flag, Iostat = Ios)

        End If

        If (Ios == 0) Then
            Write(*,*) " -> "//Trim(Filen),' Opened as Mode: ',Flag
            Openf2 = .True.

        Else
            Write(*,*) " -> Cannot Open ",Trim(Filen),' as Mode: ',Flag
            Openf2 = .False.

        End If

        Return

    End Function Openf2

    Subroutine Format_Parameter_Line(Line_In, Line_Out, Skip_Line)
        !---- Arguments ----!
        Character (Len=132), Intent(In) :: Line_In
        Character (Len=132), Intent(Out):: Line_Out
        Logical            , Intent(Out):: Skip_Line

        !---- Local Variables ----!
        Integer :: Ind

        Skip_Line = .False.

        ! Lines Are Left Adjusted.
        Line_Out = Adjustl(Line_In)

        ! An Empty Line Is Skipped.
        If (Len_Trim(Line_Out) == 0) Then
            Skip_Line = .True.
            Return
        End If

        ! A line that starts with '!' Or '#' is skipped.
        If (Line_Out(1:1) == "!" .Or. Line_Out(1:1) == "#") Then
            Skip_Line = .True.
            Return
        End If

        ! Check For An End Of Line Comment (Starts With '!' Or '#').
        Ind = Index(Line_Out, "!")
        If (Ind /= 0) Then
            Line_Out = Trim(Line_Out(1:Ind-1))
        Else
            Ind = Index(Line_In, "#")
            If (Ind /= 0) Line_Out = Trim(Line_In(1:Ind-1))
        End If

        Return

    End Subroutine Format_Parameter_Line

    Subroutine Read_New_Style_Cfg_File(File_Contents, Config)
        !---- Arguments ----!
        Type(File_Type)   ,      Intent(In)    :: File_Contents
        Type(Racer_Config_Type), Intent(Inout) :: Config

        !---- Local Variables ----!
        Logical                           :: Skip_Line
        Integer                           :: Comp, I, Ier, J, Numor1, Numor2, Nwords, &
                                             IYear, ICycle, Ier1, Temp
        Character (Len=132)               :: F_Line
        Character (Len=80), Dimension(20) :: Words
        Character (Len=80)                :: auxstr

        Ier  = 0
        Ier1 = 0

        IYear  = -1
        ICycle = -1
        Config%Racer_nexus = .false.

        Do I = 1, File_Contents%Nlines

            Call Format_Parameter_Line(File_Contents%Line(I)%str, F_Line, Skip_Line)

            If (Skip_Line) Cycle

            call Get_Words(F_Line, Words, Nwords)
            ! The keywords is upperized.
            Words(1)=U_case(Words(1))

            Select Case (Words(1)(1:3))

                Case ("ADD")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%IAddLib
                    If (Ier /= 0) Config%IAddLib = Default_Config%IAddLib

                Case ("NXS","NEX")

                    Config%Racer_nexus = .true. !The numors continue to be provided as integers "nnnnnn" and the files will be read as "nnnnnn.nxs"

                Case ("CAR")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%ICard
                    If (Ier /= 0) Config%ICard = Default_Config%ICard

                Case ("CON")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%Cont
                    If (Ier /= 0) Config%Cont = Default_Config%Cont

                Case ("CYC")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) ICycle
                    If (Ier /= 0) ICycle = -1

                ! The path for the working directory where numors are stored.
                Case ("DAT")
                    Read(Unit = Words(2), Fmt = "(a)", Iostat = Ier) Config%Data_Directory  !It was *, but "/" doesn't work with free format for strings
                    If (Ier /= 0) Config%Data_Directory = Default_Config%Data_Directory
                    J=len_trim(Config%Data_Directory)
                    if(Config%Data_Directory(J:J) /= ops_sep) Config%Data_Directory=trim(Config%Data_Directory)//ops_sep

                Case ("DEA")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%DeadTime
                    If (Ier /= 0) Config%DeadTime = Default_Config%DeadTime

                Case ("DET")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%DRad
                    If (Ier /= 0) Config%DRad = Default_Config%DRad

                Case ("NPI")
                    auxstr=trim(Words(2))//" "//trim(Words(3))//" "//trim(Words(4))//" "//trim(Words(5))
                    Read(Unit = auxstr, Fmt = *, Iostat = Ier) Config%np_ver, Config%np_hor, Config%Dy, Config%Dx
                    If (Ier /= 0) then
                      Config%np_ver = Default_Config%np_ver
                      Config%np_hor = Default_Config%np_hor
                      Config%Dy     = Default_Config%Dy
                      Config%Dx     = Default_Config%Dx
                    End If

                Case ("DIF")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%DiffLim
                    If (Ier /= 0) Config%DiffLim = Default_Config%DiffLim

                ! Volume of "Core" Ellipsoid (In "Actual" Volume Units)
                ! at which the library ellipsoids are stored
                ! (It is also used as the volume over which weak peaks
                ! are integrated (Intensity then Scaled!);
                Case ("ELL")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%ActVol
                    If (Ier /= 0) Config%ActVol = Default_Config%ActVol

                ! The Fill Fraction.
                Case ("FIL")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%Fill
                    If (Ier /= 0) Config%Fill = Default_Config%Fill

                ! The horizontal offset.
                Case ("HOR")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%XOffset
                    If (Ier /= 0) Config%XOffset = Default_Config%XOffset
                    Config%XOffset     = -Config%XOffset

                ! The I, J And K Min And Maximum Values.
                Case ("IJK")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%IMin
                    If (Ier /= 0) Ier1 = 1
                    Read(Unit = Words(3), Fmt = *, Iostat = Ier) Config%IMax
                    If (Ier /= 0) Ier1 = 1
                    Read(Unit = Words(4), Fmt = *, Iostat = Ier) Config%JMin
                    If (Ier /= 0) Ier1 = 1
                    Read(Unit = Words(5), Fmt = *, Iostat = Ier) Config%JMax
                    If (Ier /= 0) Ier1 = 1
                    Read(Unit = Words(6), Fmt = *, Iostat = Ier) Config%KMin
                    If (Ier /= 0) Ier1 = 1
                    Read(Unit = Words(7), Fmt = *, Iostat = Ier) Config%KMax
                    If (Ier /= 0) Ier1 = 1

                    If (Ier1 /= 0) Then

                        Config%IMin = Default_Config%IMin
                        Config%IMax = Default_Config%IMax
                        Config%JMin = Default_Config%JMin
                        Config%JMax = Default_Config%JMax
                        Config%KMin = Default_Config%KMin
                        Config%KMax = Default_Config%KMax

                    Else

                        Config%IMin  = Max(0, Config%IMin)
                        Config%JMin  = Max(0, Config%JMin)
                        Config%KMin  = Max(1, Config%KMin)

                    End If

                ! The Instrument Name. Must be specified in case where the Working Directory has not been set.
                Case ("INS")
                    Read(Unit = Words(2), Fmt = "(a)", Iostat = Ier) Config%Instrument
                    If (Ier /= 0) Config%Instrument = Default_Config%Instrument

                    Config%Instrument=l_case(Config%Instrument)
                    !write(*,"(a)") " => "//trim(Config%Instrument)//" <-- Instrument name"

                ! The Scaling Monitor Count.
                Case ("MON")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%Monitor
                    If (Ier /= 0) Config%Monitor = Default_Config%Monitor

                ! The Vectors To Nearest Neighbours. The Values Must Be Entered In The Next Line.
                Case ("NEA")

                    Call Format_Parameter_Line(File_Contents%Line(I+1)%str, F_Line, Skip_Line)

                    If (Skip_Line) Then
                        Ier = 1

                    Else
                        Call Get_Words(F_Line, Words, Nwords)

                        Select Case(Nwords)

                            Case(12:14)
                                Read(Unit = F_Line, Fmt = *, Iostat = Ier) Config%Neigh(:,1:4)
                                Config%N_Neigh = 4

                            Case(15:17)
                                Read(Unit = F_Line, Fmt = *, Iostat = Ier) Config%Neigh(:,1:5)
                                Config%N_Neigh = 5

                            Case(18:)
                                Read(Unit = F_Line, Fmt = *, Iostat = Ier) Config%Neigh(:,:)
                                Config%N_Neigh = 6

                            Case Default
                                Ier = 1

                        End Select

                        If (Ier /= 0) Then

                            Config%N_Neigh = Default_Config%N_Neigh
                            Config%Neigh  = Default_Config%Neigh

                        End If

                    End If

                Case ("NUM")

                    Comp = 0
                    Do
                        Call Format_Parameter_Line(File_Contents%Line(I + Comp + 1)%str, F_Line, Skip_Line)
                        If (Skip_Line) Exit
                        Call Get_Words(F_Line, Words, Nwords)
                        If (Nwords > 1) Then

                            Read(Unit = F_Line, Fmt = *, Iostat = Ier) Numor1, Numor2

                        Else
                            Read(Unit = F_Line, Fmt = *, Iostat = Ier) Numor1

                        End If

                        If (Ier /= 0) Exit

                        Comp = Comp + 1

                    End Do

                    If (Comp == 0) Then
                        If (Allocated(Config%Numors_Intervals)) Deallocate(Config%Numors_Intervals)

                    Else
                        If (Allocated(Config%Numors_Intervals)) Deallocate(Config%Numors_Intervals)
                        Allocate(Config%Numors_Intervals(Comp,2))

                        Do J = 1, Comp

                            Call Format_Parameter_Line(File_Contents%Line(I + J)%str, F_Line, Skip_Line)

                            Call Get_Words(F_Line, Words, Nwords)

                            If (Nwords > 1) Then

                                Read(Unit = F_Line, Fmt = *, Iostat = Ier) Numor1, Numor2

                                If (Numor2 < Numor1) Then

                                    Temp   = Numor1
                                    Numor1 = Numor2
                                    Numor2 = Temp

                                End If

                            Else

                                Read(Unit = F_Line, Fmt = *, Iostat = Ier) Numor1

                                Numor2 = Numor1

                            End If

                            Config%Numors_Intervals(J,:) = [ Numor1, Numor2 ]

                        End Do

                    End If

                ! The omega offset.
                Case ("OME")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%OmegaOffset
                    If (Ier /= 0) Config%OmegaOffset = Default_Config%OmegaOffset

                Case ("PRI")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%IPrint
                    If (Ier /= 0) Config%IPrint = Default_Config%IPrint

                Case ("REF")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%IRefine
                    If (Ier /= 0) Config%IRefine = Default_Config%IRefine

                ! The Regulation Temperature.
                Case ("REG")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%TReg
                    If (Ier /= 0) Config%TReg = Default_Config%TReg

                Case ("SCA")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%Scale
                    If (Ier /= 0) Config%Scale = Default_Config%Scale

                Case ("SIG")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%SigBR
                    If (Ier /= 0) Config%SigBR = Default_Config%SigBR

                ! The Twin Ub Matrix.
                Case ("TWI")
                    Do J = 1, 3
                        Call Format_Parameter_Line(File_Contents%Line(I+J)%str, F_Line,  Skip_Line)
                        Read(Unit = F_Line, Fmt = *, Iostat = Ier) Config%Twin_UB(J,:)
                        If (Ier /= 0 .OR. Skip_Line) Ier1 = 1
                    End Do

                    If (Ier1 /= 0) Then
                        Config%Twin_UB_Set = Default_Config%Twin_UB_Set
                        Config%Twin_UB     = Default_Config%Twin_UB
                    Else
                        Config%Twin_UB_Set = 1
                    End If

                ! The UB Matrix.
                Case ("UB_")
                    Do J = 1, 3
                        Call Format_Parameter_Line(File_Contents%Line(I+J)%str, F_Line,  Skip_Line)
                        Read(Unit = F_Line, Fmt = *, Iostat = Ier) Config%UB(J,:)
                        If (Ier /= 0 .OR. Skip_Line) Ier1 = 1
                    End Do

                    If (Ier1 /= 0) Then
                        Config%UB_Set = Default_Config%UB_Set
                        Config%UB     = Default_Config%UB
                    Else
                        Config%UB_Set = 1
                    End If

                ! The vertical offset.
                Case ("VER")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%YOffset
                    If (Ier /= 0) Config%YOffset = Default_Config%YOffset

                ! The Volume Multiplier For Ellipsoid Volume Which Defines The Limit Of Strong Peaks.
                Case ("VOL")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%VolFac
                    If (Ier /= 0) Config%VolFac = Default_Config%VolFac

                ! The wavelength.
                Case ("WAV")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) Config%Wavelen
                    If (Ier /= 0) Then
                        Config%Wavelen     = Default_Config%Wavelen
                        Config%Wavelen_Set = Default_Config%Wavelen_Set
                    Else
                        Config%Wavelen_Set = 1
                    End If

                Case ("YEA")
                    Read(Unit = Words(2), Fmt = *, Iostat = Ier) IYear
                    If (Ier /= 0) IYear = -1

            End Select

        End Do

        If (IYear >= 0 .AND. ICycle > 0) Config%IYearCycle = 10*IYear + ICycle

        ! Checks that the configuration is OK.
        Call Check_Configuration(Config)

        Return

    End Subroutine Read_New_Style_Cfg_File

    ! Subroutine Read_Old_Style_Cfg_File(File_Contents, Config)
    ! Reads a racer configuration file written with the original style
    ! Arguments:
    !     Type(File_Type)   , Intent(In)    :: File_Contents -> the contents of the configuration file
    !                                                                stored in a structure.
    !     Type(Racer_Config_Type), Intent(Inout) :: Config        -> a structure that will store the configuration
    !                                                                read in the file.
    Subroutine Read_Old_Style_Cfg_File(File_Contents, Config)
        !---- Arguments ----!
        Type(File_Type),         Intent(In)    :: File_Contents
        Type(Racer_Config_Type), Intent(Inout) :: Config

        !---- Local Variables ----!
        Integer                           :: comp, comp1, I, Ier, J, Numor1, Numor2, Nwords, N_Intervals, Temp
        Character (Len=132)               :: Line
        Character (Len=80), Dimension(20) :: Words
        Type(File_Type)                   :: Parameters

        comp = 0
        ! Computes the number of non-empty or non-comment lines.
        Do I = 1, File_Contents%Nlines
            Line = Adjustl(File_Contents%Line(I)%str)
            If(Line(1:5) == "!NPIX") Then
              Read(Unit = line(6:), Fmt = *, Iostat = Ier) Config%np_ver, Config%np_hor, Config%Dy, Config%Dx
              If (Ier /= 0) then
                Config%np_ver = Default_Config%np_ver
                Config%np_hor = Default_Config%np_hor
                Config%Dy     = Default_Config%Dy
                Config%Dx     = Default_Config%Dx
              End If
            End If
            If (Len_Trim(Line) == 0 .Or. Line(1:1) == "!") Cycle
            comp = comp + 1
        End Do

        ! This structure will store all the non-empty and non-comment lines.
        Parameters%NLines = comp
        Allocate(Parameters%Line(comp))

        ! Parses the configuration file again and fills the |Parameters| structure with the non-empty
        ! and non-comment lines.
        comp = 0
        Do I = 1, File_Contents%Nlines
            Line = Adjustl(File_Contents%Line(I)%str)
            If (Len_Trim(Line) == 0 .Or. Line(1:1) == "!") Cycle
            J = Index(Line, "!")
            If( J /= 0 ) Line = Trim(Line(1:J-1))
            comp = comp + 1
            Parameters%Line(comp)%str = Trim(Line)
        End Do

        comp = 1

        ! The first line to parse should contains the value in the following order:
        !     - Contour level          --> real
        !     - Monitor count          --> integer
        !     - Dead time              --> real
        !     - Scale                  --> integer
        !     - Regulation temperature --> integer
        Line = Parameters%Line(comp)%str
        Read(Line, *, IOSTAT=Ier) Config%Cont, Config%Monitor, Config%DeadTime, Config%Scale, Config%TReg

        ! An error was found when reading that line assign the corresponding config var to their default value
        ! and continue.
        If (Ier > 0) Then
            Config%Cont     = Default_Config%Cont
            Config%Monitor  = Default_Config%Monitor
            Config%DeadTime = Default_Config%DeadTime
            Config%Scale    = Default_Config%Scale
            Config%TReg     = Default_Config%TReg
        End If

        ! The second line to parse should contains the value in the following order:
        !     - Activity volume       --> real
        !     - Volume factor         --> real
        !     - Sigma to noise ratio  --> real
        !     - Fill fraction         --> real
        !     - Differentiation limit --> real
        comp = comp + 1
        Line = Parameters%Line(comp)%str
        Read(Line, *, Iostat = Ier) Config%ActVol, Config%VolFac, Config%SigBR, Config%Fill, Config%DiffLim

        ! An error was found when reading that line, assigns the corresponding config vars to their default value
        ! and continue.
        If (Ier > 0) Then
            Config%ActVol  = Default_Config%ActVol
            Config%VolFac  = Default_Config%VolFac
            Config%SigBR   = Default_Config%SigBR
            Config%Fill    = Default_Config%Fill
            Config%DiffLim = Default_Config%DiffLim
        End If


        ! The third line to parse should contains the value in the following order:
        !     - IaddLib       --> Integer (0/1)
        !     - IRefine       --> Integer
        !     - IPrint        --> Integer (1/2/3)
        !     - ICard         --> Integer
        comp = comp + 1
        Line = Parameters%Line(comp)%str
        Read(Line, *, Iostat = Ier) Config%IAddLib, Config%IRefine, Config%IPrint, Config%ICard

        ! An error was found when reading that line, assigns the corresponding config vars to their default value
        ! and continue.
        If (Ier > 0) Then
            Config%IAddLib = Default_Config%IAddLib
            Config%IRefine = Default_Config%IRefine
            Config%IPrint  = Default_Config%IPrint
            Config%ICard   = Default_Config%ICard
        End If

        ! The fourth line to parse should contains the value in the following order:
        !     - Imin --> Integer
        !     - Imax --> Integer
        !     - Jmin --> Integer
        !     - Jmax --> Integer
        !     - Kmin --> Integer
        !     - Kmax --> Integer
        comp = comp + 1
        Line = Parameters%Line(comp)%str
        Read(Line, *, Iostat = Ier) Config%Imin, Config%Imax, Config%Jmin, Config%Jmax, Config%Kmin, Config%Kmax

        ! An error was found when reading that line, assigns the corresponding config vars to their default value
        ! and continue.
        If (Ier > 0) Then
            Config%IMin = Default_Config%IMin
            Config%IMax = Default_Config%IMax
            Config%JMin = Default_Config%JMin
            Config%JMax = Default_Config%JMax
            Config%KMin = Default_Config%KMin
            Config%KMax = Default_Config%KMax

        Else
            Config%IMin  = Max(0,Config%IMin)
            Config%JMin  = Max(0,Config%JMin)
            Config%KMin  = Max(1,Config%KMin)
        End If

        ! The fifth line may contain the value in the following order:
        !     - Neigh1 --> 3 Floats
        !     - Neigh2 --> 3 Floats
        !     - Neigh3 --> 3 Floats
        !     - Neigh4 --> 3 Floats
        !     - (Neigh5 --> 3 Floats)
        !     - (Neigh6 --> 3 Floats)
        comp = comp + 1
        Line = Parameters%Line(comp)%str
        Call Get_Words(Line, Words, Nwords)
        Select Case(Nwords)
            Case(12:14)
                Read(Line, *, Iostat = Ier) Config%Neigh(:,1:4)
                Config%N_Neigh = 4

            Case(15:17)
                Read(Line, *, Iostat = Ier) Config%Neigh(:,1:5)
                Config%N_Neigh = 5

            Case(18:)
                Read(Line, *, Iostat = Ier) Config%Neigh(:,:)
                Config%N_Neigh = 6

            Case Default
                Ier = 1
        End Select

        If (Ier > 0) Then
            Config%N_Neigh = Default_Config%N_Neigh
            Config%Neigh  = Default_Config%Neigh

        Else
            comp = comp + 1

        End If

        ! The sixth line will contain the value in the following order:
        !     - Drad    --> Float
        !     - XOffset --> Float
        !     - YOffset --> Float
        !     - OmegaOffset --> Float
        !     - Wavelen --> Float
        Line = Parameters%Line(comp)%str
        Call Get_Words(Line, Words, Nwords)
        Select Case(Nwords)
          Case(1)
             Read(Line, *, Iostat = Ier) Config%DRad
             if(ier > 0) Config%DRad = Default_Config%DRad
             Config%YOffset      = Default_Config%YOffset
             Config%XOffset      = Default_Config%XOffset
             Config%OmegaOffset  = Default_Config%OmegaOffset
             Config%Wavelen      = Default_Config%Wavelen
             Config%Wavelen_Set  = Default_Config%Wavelen_Set
          Case(2)
             Read(Line, *, Iostat = Ier) Config%DRad, Config%YOffset
             if(ier > 0) then
               Config%DRad = Default_Config%DRad
               Config%YOffset      = Default_Config%YOffset
             end if
             Config%XOffset      = Default_Config%XOffset
             Config%OmegaOffset  = Default_Config%OmegaOffset
             Config%Wavelen      = Default_Config%Wavelen
             Config%Wavelen_Set  = Default_Config%Wavelen_Set
          Case(3)
             Read(Line, *, Iostat = Ier) Config%DRad, Config%YOffset, Config%XOffset
             if(ier > 0) then
               Config%DRad = Default_Config%DRad
               Config%YOffset      = Default_Config%YOffset
               Config%XOffset      = Default_Config%XOffset
             end if
             Config%OmegaOffset  = Default_Config%OmegaOffset
             Config%Wavelen      = Default_Config%Wavelen
             Config%Wavelen_Set  = Default_Config%Wavelen_Set
          Case(4)
             Read(Line, *, Iostat = Ier) Config%DRad, Config%YOffset, Config%XOffset,Config%OmegaOffset
             if(ier > 0) then
               Config%DRad         = Default_Config%DRad
               Config%YOffset      = Default_Config%YOffset
               Config%XOffset      = Default_Config%XOffset
               Config%OmegaOffset  = Default_Config%OmegaOffset
             end if
             Config%Wavelen      = Default_Config%Wavelen
             Config%Wavelen_Set  = Default_Config%Wavelen_Set
          Case(5:)
             Read(Line, *, Iostat = Ier) Config%DRad, Config%YOffset, Config%XOffset,Config%OmegaOffset,Config%Wavelen
             if(ier > 0) then
               Config%DRad         = Default_Config%DRad
               Config%YOffset      = Default_Config%YOffset
               Config%XOffset      = Default_Config%XOffset
               Config%OmegaOffset  = Default_Config%OmegaOffset
               Config%Wavelen      = Default_Config%Wavelen
               Config%Wavelen_Set  = Default_Config%Wavelen_Set
             else
                Config%Wavelen_Set  = 1
             end if
       End Select

       Config%XOffset      = -Config%XOffset

        ! The next three lines may contain the value in the following order:
        !     -UB(1,:)     --> 3 Floats
        !     -UB(2,:)     --> 3 Floats
        !     -UB(3,:)     --> 3 Floats
        Do I = 1, 3
            comp = comp + 1
            Line = Parameters%Line(comp)%str
            Call Get_Words(Line, Words, Nwords)

            If (Nwords >= 3) Then
                Read(Line, *, Iostat = Ier) Config%UB(I,:)

            Else
                comp = comp - I
                Ier = 1
                Exit
            End If
        End Do

        If (Ier > 0) Then
            Config%UB     = Default_Config%UB
            Config%UB_Set = Default_Config%UB_Set

        Else
            Config%UB_Set = 1
            ! The next three lines may contain the value in the following order:
            !     -Twin_UB(1,:)     --> 3 Floats
            !     -Twin_UB(2,:)     --> 3 Floats
            !     -Twin_UB(3,:)     --> 3 Floats
            Do I = 1, 3
                comp = comp + 1
                Line = Parameters%Line(comp)%str
                Call Get_Words(Line, Words, Nwords)
                If (Nwords >= 3) Then
                    Read(Line, *, Iostat = Ier) Config%Twin_UB(I,:)
                Else
                    Comp = Comp - I
                    Ier = 1
                    Exit
                End If
            End Do

            If (Ier > 0) Then
                Config%Twin_UB     = Default_Config%Twin_UB
                Config%Twin_UB_Set = Default_Config%Twin_UB_Set
            Else
                Config%Twin_UB_Set = 1
            End If

        End If

        ! The next line will contain the value in the following order:
        !     - Instrument --> String
        !     - data directory --> String - optional
        !     - yearcycle --> Integer
        comp = comp + 1
        Line = Parameters%Line(comp)%str

        Read(Line, *, Iostat = Ier) Config%Instrument, Config%IYearCycle

        If (Ier > 0) Then
            Call Get_Words(Line, Words, Nwords)
            if (Nwords >= 2) then
              Config%Instrument=adjustl(Words(1))
              Config%Data_Directory=adjustl(Words(2))
              I=len_trim(Config%Data_Directory)
              if (Config%Data_Directory(I:I) /= ops_sep) Config%Data_Directory=trim(Config%Data_Directory)//ops_sep
              if (Nwords >= 3) Read(Unit=Words(3),Fmt=*,Iostat=Ier) Config%IYearCycle
              if (Nwords > 3)  Config%racer_nexus=.true.
            else
               Config%Instrument = Default_Config%Instrument
               Config%IYearCycle = Default_Config%IYearCycle
            end if
        End If

        Config%Instrument=L_case(Config%Instrument)

        ! The next lines contain the numors intervals. Two integers for an intervals and a single integer for a
        ! unique numor to consider.
        N_Intervals = 0
        Do I = comp + 1, Parameters%NLines
            Numor1 = -1
            Numor2 = -1
            Line = Parameters%Line(I)%str

            Read(Line, *, Iostat = Ier) Numor1, Numor2

            ! If an error occured consider that the line contains the path for numors directory and for alphas
            ! filename.
            If (Ier > 0) Then
                Read(Line, *, Iostat = Ier) Config%Data_Directory
                If (Ier > 0) Config%Data_Directory = Default_Config%Data_Directory
                    J=len_trim(Config%Data_Directory)
                    if(Config%Data_Directory(J:J) /= ops_sep) Config%Data_Directory=trim(Config%Data_Directory)//ops_sep
                Exit
            End If

            If (Numor1 <= 0 .And. Numor2 <= 0) Cycle

            N_Intervals = N_Intervals + 1

        End Do

        If (N_Intervals == 0) Then
            If (Allocated(Config%Numors_Intervals)) Deallocate(Config%Numors_Intervals)

        Else

            If (Allocated(Config%Numors_Intervals)) Deallocate(Config%Numors_Intervals)
            Allocate(Config%Numors_Intervals(N_Intervals,2))

            comp1 = 0
            Do I = comp + 1, comp + N_Intervals
                Numor1 = -1
                Numor2 = -1
                Line = Parameters%Line(I)%str

                Read(Line, *, Iostat = Ier) Numor1, Numor2

                If (Numor1 <= 0 .And. Numor2 <= 0) Cycle

                If (Numor2 < 0) Then
                    Numor2 = Numor1

                Else

                    If (Numor2 < Numor1) Then
                        Temp = Numor1
                        Numor1 = Numor2
                        Numor2 = Temp
                    End If

                End If

                comp1 = comp1 + 1
                Config%Numors_Intervals(comp1,:) = (/ Numor1, Numor2/)

            End Do

        End If

        ! Checks that the configuration is OK.
        Call Check_Configuration(Config)

        Return

    End Subroutine Read_Old_Style_Cfg_File

    ! Subroutine Check_Configuration()
    ! Checks the configuration.
    Subroutine Check_Configuration(Cfg)
        ! ___ Local variables ---!
        Type(Racer_Config_Type), Intent(Inout) :: Cfg

        !---- Local variables ---!
        Integer              :: iop, IYear, ICycle, I, N_Numors, J, Comp
        Character(len = 512) :: Path, Actual_Path

        ! An instrument must have been set.
        If (Len_Trim(Cfg%Instrument) == 0) Then
            Racer_File_Error = .True.
            Racer_File_Error_Mess = "No instrument set."
            Return
        End If

        If (.Not. Allocated(Cfg%Numors_Intervals)) Then

            Racer_File_Error      = .True.
            Racer_File_Error_Mess = "No numors intervals given. No numors to process."
            Return

        Else

            N_Numors = 0
            Do I = 1, Size(Cfg%Numors_Intervals, 1)

                N_Numors = N_Numors + Cfg%Numors_Intervals(I,2) - Cfg%Numors_Intervals(I,1) + 1

            End Do

            If (N_Numors == 0) Then

                Racer_File_Error      = .True.
                Racer_File_Error_Mess = "No numors intervals given. No numors to process."
                Return

            End If

            If (Allocated(Numors)) Deallocate(Numors)
            Allocate(Numors(N_Numors))

            Comp = 1
            Do I = 1, Size(Cfg%Numors_Intervals, 1)

                Do J = Cfg%Numors_Intervals(I,1), Cfg%Numors_Intervals(I,2)

                    Numors(Comp) = J
                    Comp = Comp + 1

                End Do

            End Do

        End If

        ! Further testings ans settings.
        If (Len_Trim(Cfg%Data_Directory) == 0) Then

            Call Initialize_Data_Directory()
            If (Cfg%IYearCycle > 0) Then
                IYear = Cfg%IYearCycle/10
                ICycle = Cfg%IYearCycle - 10*IYear
                Do I = 1, Size(Numors, 1)
                    Call Get_Absolute_Data_Path(Numors(I), Cfg%Instrument, Path, IYear, ICycle, &
                                                Actual_Path = Actual_Path)
                    ! Case where the numors was found in their uncompress form.
                    If (Actual_Path == Path) Exit
                End Do
            Else
                Do I = 1, Size(Numors,1)
                    Call Get_Absolute_Data_Path(Numors(I), Cfg%Instrument, Path, Actual_Path = Actual_Path)
                    ! Case where the numors was found in their uncompress form.
                    If (Actual_Path == Path) Exit
                End Do
            End If

            iop = index(Path, ops_sep, back=.true.)
            Cfg%Data_Directory = Path(1:iop)

        End If

        Call Set_Instrm_Directory(Working_Dir = Cfg%Data_Directory)

        If (Err_CFML%Ierr /= 0) Then
            Racer_File_Error      = .True.
            Racer_File_Error_Mess = Err_CFML%Msg
            Err_CFML%Ierr     = 0
            Err_CFML%Flag     = .False.
            Err_CFML%Msg      = ""
        End If

    End Subroutine Check_Configuration

End Module Racer_CFML_Files
